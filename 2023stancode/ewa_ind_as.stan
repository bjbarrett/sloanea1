data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int L;              // num groups
    int N_effects;      // number of learning parameters to estimate
    array[N] int tech;        // techique chosen
    array[N,K] real pay_i ;    // observed personal yields of techs (1/0)
    array[N] int bout;        // processing bout per individual
    array[N] int id;          // individual id
    array[N] real age;   // age index
    array[N] int sex_index;   // sex index
    array[K] real ac_init;     // initial attraction scores
}

parameters {
  vector<lower=0>[N_effects*2] sigma_i;       // standard deviations of varying effects
  matrix[N_effects*2,J] zed_i;                // individual z-scores for cholesky decomp
  cholesky_factor_corr[N_effects*2] L_Rho_i;  // correlation matrix
  vector[2] Sl;                    
  vector[2] Sp;
  vector[N_effects] mu;                // individual z-scores for cholesky decomp

}

transformed parameters{
  matrix[J,N_effects*2] I;              //define varying effects for individuals
  I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;           // asocial learning Pr
    real phi;           // stickiness parameter to recent experience
    real lambda;        // sensitivity to attraction scores

   //priors
    mu[1] ~  normal(1,0.6);
    mu[2] ~  normal(0,1);
    to_vector(Sl) ~  normal(mu[1],1);
    to_vector(Sp) ~  normal(mu[2],1);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,0.8);
    L_Rho_i ~ lkj_corr_cholesky(4);

    //likelihood loop
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*pay_i[i-1,j];
            } else {
                AC[j] = ac_init[j];
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values;
            lambda = exp( I[id[i],1] + Sl[sex_index[i]] + I[id[i],3]*age[i] );
            phi= inv_logit( I[id[i],2] + Sp[sex_index[i]] + I[id[i],4]*age[i] );
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
        target += logPrA;
     }//i  

}//end of model

generated quantities{
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    array[J] real lambda_i;           // stickiness parameter
    array[J] real phi_i;           // stickiness parameter
    matrix[N_effects*2,N_effects*2] Rho_i;
    matrix[N,K] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);

    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi_i[id[i]])*AC[j] + phi_i[id[i]]*pay_i[i-1,j];
            } else {
                AC[j] = ac_init[j];
            }
        }//j

        if ( bout[i]==1 ) {
            lambda_i[id[i]] = exp( I[id[i],1] + Sl[sex_index[i]] + I[id[i],3]*age[i] );
            phi_i[id[i]] = inv_logit( I[id[i],2] + Sp[sex_index[i]] + I[id[i],4]*age[i] );
        }

        logPrA =  lambda_i[id[i]]*AC[tech[i]] - log_sum_exp(  lambda_i[id[i]]*AC );
        log_lik[i] = (logPrA);
        for(j in 1:K){
          PrPreds[i,j] = exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) );
          }
     }//i  
}//end of model
