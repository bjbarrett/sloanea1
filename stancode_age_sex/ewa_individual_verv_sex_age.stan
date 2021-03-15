data {
int n_behav;              // num behaviors
int n_obs;              // num observations in dataset
int n_id;              // num individuals
int n_group;              // num groups
real logage[n_obs];              // num groups
int tech[n_obs];        // techique chosen
real y[n_obs,n_behav];        // observed personal yields of techs (1/0)
int bout[n_obs];        // processing bout per individual
int id[n_obs];          // individual id
int n_effects;      // number of learning parameters to estimate
int sex_index[n_obs];   //index variable for sex; 1 is female 2 is male
int group_index[n_obs];   //index variable for age; 1 is female 2 is male
}

parameters {
matrix[2,n_effects] S;                  //sex  means
matrix[2,n_effects] bA;                //age params per sex
vector<lower=0>[n_effects] sigma_i;       // standard deviations of varying effects
matrix[n_effects , n_id] zed_i;                // individual z-scores for cholesky decomp
cholesky_factor_corr[n_effects] L_Rho_i;  // correlation matrix
vector<lower=0>[n_effects] sigma_g;       // standard deviations of varying effects
matrix[n_effects,n_group] zed_g;                // individual z-scores for cholesky decomp
cholesky_factor_corr[n_effects] L_Rho_g;  // correlation matrix
}

transformed parameters{
    matrix[n_id,n_effects] I;              //define varying effects for individuals
    matrix[n_group,n_effects] G;              //define varying effects for groups
    I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
    G = (diag_pre_multiply(sigma_g,L_Rho_g) * zed_g)'; //cholesky decomp majick
}

model {
  vector[n_behav] AC;       // attraction scores
  real logPrA;        // individual learning logPr
  real phi;           // stickiness parameter to recent experience
  real lambda;        // sensitivity to attraction scores

//priors
to_vector(S[1,]) ~ normal(1,0.6);
to_vector(S[2,]) ~ normal(0,1);
sigma_i ~ exponential(1);
to_vector(zed_i) ~ normal(0,1);
L_Rho_i ~ lkj_corr_cholesky(3);
sigma_g ~ exponential(1);
to_vector(zed_g) ~ normal(0,1);
L_Rho_g ~ lkj_corr_cholesky(3);

  for ( i in 1:n_obs ) {
  //update attractions
    for ( j in 1:n_behav ) {
      if ( bout[i] > 1 ) {
        AC[j]= (1-phi)*AC[j] + phi*y[i-1,j];
      } else {
        AC[j]= 0;
      }
    }
            lambda = exp( I[id[i],1] + G[group_index[i],1] + S[1,sex_index[i]] + bA[1,sex_index[i]]*logage[i] ) ;
            phi= inv_logit(  I[id[i],2] + G[group_index[i],2]  + S[2,sex_index[i]] + bA[2,sex_index[i]]*logage[i] );
            logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
            target += ( logPrA );

    }
}

generated quantities {
    vector[n_obs] log_lik;
    vector[n_behav] AC;       // attraction scores
    real logPrA;        // individual learning temp
    vector[n_behav] lin_mod;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    matrix[n_effects,n_effects] Rho_i;
    matrix[n_effects,n_effects] Rho_g;
    matrix[n_obs,n_behav] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);
    Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);

for ( i in 1:n_obs ) {
  //update attractions
    for ( j in 1:n_behav ) {
      if ( bout[i] > 1 ) {
        AC[j]= (1-phi)*AC[j] + phi*y[i-1,j];
      } else {
        AC[j]= 0;
      }
    }//j
            lambda = exp( I[id[i],1] + G[group_index[i],1] + S[1,sex_index[i]] + bA[1,sex_index[i]]*logage[i] ) ;
            phi= inv_logit(  I[id[i],2] + G[group_index[i],2]  + S[2,sex_index[i]] + bA[2,sex_index[i]]*logage[i] );
            logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
            log_lik[i] = logPrA ;

            for(j in 1:n_behav){
            PrPreds[i,j] = exp( lambda*AC[j] - log_sum_exp( lambda*AC) );
            }
                }//i
}
