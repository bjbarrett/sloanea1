data {
int K;              // num behaviors
int N;              // num observations in dataset
int J;              // num individuals
int tech[N];        // techique chosen
real pay_i[N,K];        // observed personal yields of techs (1/0)
int bout[N];        // processing bout per individual
int id[N];          // individual id
int N_effects;      // number of learning parameters to estimate
int sex_index[N];   //index variable for sex; 1 is female 2 is male
int age_index[N];   //index variable for age; 1 is female 2 is male
int group_index[N];   //index variable for age; 1 is female 2 is male

}
parameters {
matrix[2,N_effects] A;                  //age means
matrix[2,N_effects] S;                  //sex  means
vector<lower=0>[N_effects] sigma_i;       // standard deviations of varying effects
matrix[N_effects,J] zed_i;                // individual z-scores for cholesky decomp
cholesky_factor_corr[N_effects] L_Rho_i;  // correlation matrix
vector<lower=0>[N_effects] sigma_g;       // standard deviations of varying effects
matrix[N_effects,2] zed_g;                // individual z-scores for cholesky decomp
cholesky_factor_corr[N_effects] L_Rho_g;  // correlation matrix
}

transformed parameters{
  
    matrix[J,N_effects] I;              //define varying effects for individuals
    matrix[2,N_effects] G;              //define varying effects for groups
    I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
    G = (diag_pre_multiply(sigma_g,L_Rho_g) * zed_g)'; //cholesky decomp majick
}

model {
  vector[K] AC;       // attraction scores
  real logPrA;        // individual learning logPr
  real phi;           // stickiness parameter to recent experience
  real lambda;        // sensitivity to attraction scores

//priors
to_vector(A[1,]) ~ normal(1,0.6);
to_vector(S[1,]) ~ normal(1,0.6);
to_vector(A[2,]) ~ normal(0,1);
to_vector(S[2,]) ~ normal(0,1);
sigma_i ~ exponential(1);
to_vector(zed_i) ~ normal(0,1);
L_Rho_i ~ lkj_corr_cholesky(3);
sigma_g ~ exponential(1);
to_vector(zed_g) ~ normal(0,1);
L_Rho_g ~ lkj_corr_cholesky(3);

  for ( i in 1:N ) {
  //update attractions
    for ( j in 1:K ) {
      if ( bout[i] > 1 ) {
        AC[j]= (1-phi)*AC[j] + phi*pay_i[i-1,j];
      } else {
        AC[j]= 0;
      }
    }//j
            lambda = exp( I[id[i],1] + G[group_index[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;
            phi= inv_logit(  I[id[i],2] + G[group_index[i],2] +  A[2,age_index[i]] + S[2,sex_index[i]]);
            logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
            target += ( logPrA );

    }//i
}

generated quantities {
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    vector[K] lin_mod;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    matrix[N_effects,N_effects] Rho_i;
    matrix[N_effects,N_effects] Rho_g;
    matrix[N,K] PrPreds;     


    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);
    Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);

for ( i in 1:N ) {
  //update attractions
    for ( j in 1:K ) {
      if ( bout[i] > 1 ) {
        AC[j]= (1-phi)*AC[j] + phi*pay_i[i-1,j];
      } else {
        AC[j]= 0;
      }
    }//j
            lambda = exp( I[id[i],1] + G[group_index[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;
            phi= inv_logit(  I[id[i],2] + G[group_index[i],2] +  A[2,age_index[i]] + S[2,sex_index[i]]);
            logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
            log_lik[i] = logPrA ;

            for(j in 1:K){
            PrPreds[i,j] = exp( lambda*AC[j] - log_sum_exp( lambda*AC) );
            }
                }//i
}
