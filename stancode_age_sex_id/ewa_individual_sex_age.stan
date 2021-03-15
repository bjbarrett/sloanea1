data {
int n_behav;              // num behaviors
int n_obs;              // num observations in dataset
int n_id;                // num individuals
int n_group;              // num groups
real logage[n_obs];              // log age of forager
int tech[n_obs];          // techique observed
real y[n_obs,n_behav];        // observed personal yields of techs (1/0)
int bout[n_obs];        // processing bout per individual
int id[n_obs];          // individual id
int n_effects;      // number of learning parameters to estimate
int sex_index[n_obs];   //index variable for sex; 1 is female 2 is male
int group_index[n_obs];   //index variable for age; 1 is female 2 is male
}

parameters {
matrix[2,n_effects] S;                  //sex  means
matrix[n_id,n_effects] bA;                //age params per sex
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
  real phi[n_id];           // stickiness parameter to recent experience
  real lambda[n_id];        // sensitivity to attraction scores

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
        AC[j]= (1-phi[id[i]])*AC[j] + phi[id[i]]*y[i-1,j];
      } else {
        AC[j]= 0;
      }
    }
            lambda[id[i]] = exp( I[id[i],1] + G[group_index[i],1] + S[sex_index[i] , 1] + I[id[i] , 3]*logage[i] ) ;
            phi[id[i]]= inv_logit(  I[id[i],2] + G[group_index[i],2]  + S[sex_index[i] , 2] + bA[id[i] , 4]*logage[i] );
            logPrA = lambda[id[i]]*AC[tech[i]] - log_sum_exp( lambda[id[i]]*AC );
            target += ( logPrA );

    }
}

generated quantities {
    vector[n_obs] log_lik;
    vector[n_behav] AC;       // attraction scores
    real logPrA;        // individual learning temp
    vector[n_behav] lin_mod;
    real lambda[n_id];           // stickiness parameter
    real phi[n_id];           // stickiness parameter
    matrix[n_effects,n_effects] Rho_i;
    matrix[n_effects,n_effects] Rho_g;
    matrix[n_obs,n_behav] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);
    Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);

for ( i in 1:n_obs ) {
  //update attractions
    for ( j in 1:n_behav ) {
      if ( bout[i] > 1 ) {
        AC[j]= (1-phi[id[i]])*AC[j] + phi[id[i]]*y[i-1,j];
      } else {
        AC[j]= 0;
      }
    }//j
            lambda[id[i]] = exp( I[id[i],1] + G[group_index[i],1] + S[sex_index[i] , 1] + I[id[i] , 3]*logage[i] ) ;
            phi[id[i]]= inv_logit(  I[id[i],2] + G[group_index[i],2]  + S[sex_index[i] , 2] + bA[id[i] , 4]*logage[i] );
            logPrA = lambda[id[i]]*AC[tech[i]] - log_sum_exp( lambda[id[i]]*AC );
            log_lik[i] = logPrA ;

            for(j in 1:n_behav){
            PrPreds[i,j] = exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) );
            }
                }//i
}
