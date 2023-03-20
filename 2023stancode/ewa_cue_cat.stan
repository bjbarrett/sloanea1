data{
  int N;                 // Number of observations
  int N_id;              // Number of individuals
  int K;              // num behaviors
  array[N] int id;             // Individual ID
  array[N] int bout;          // bout Number 
  array[N] int ChoiceSelf;     // Choice of Ego
  array[N_id,K] real AcInit;
  int N_effects;      // number of learning parameters to estimate
  matrix[N,K] nmat_obs;          // Matrix with frequency of K options among models
  matrix[N,K] qmat_obs;          // Matrix with frequency of K options among models

}

parameters{
  real logit_gam;
  real gauss_beta;
  real logit_phi;
  real log_L;
  matrix[N_effects,N_id] z_ID;               //Matrix of uncorrelated z - values
  vector<lower=0>[N_effects] sigma_ID;       //SD of parameters among individuals
  cholesky_factor_corr[N_effects] L_Rho_ID;    // This is the Cholesky factor: if you multiply this matrix and it's transpose you get correlation matrix
}

transformed parameters{
  matrix[N_id,N_effects] v_ID; // Matrix of varying effects for each individual
  v_ID = ( diag_pre_multiply( sigma_ID , L_Rho_ID ) * z_ID )';
}

model{
  matrix[N_id,6] A; // Attraction matrix
  // define the priors
  logit_phi ~  normal(0,1);
  log_L ~  normal(-1,1);
  logit_gam ~ normal(0,1);
  gauss_beta ~ normal(0,1);
  to_vector(z_ID) ~ normal(0,1);
  sigma_ID ~ exponential(1);
  L_Rho_ID ~ lkj_corr_cholesky(4);

  // initialize attraction scores
   for ( i in 1:N_id ) A[ i , 1:K ] = to_vector(AcInit[ i , 1:K ])';
  // loop over all choices (20k rows in dataset)
  for ( i in 1:N ) {
    vector[K] pay;
    vector[K] pA;
    vector[K] pS;
    vector[K] p;

    real gam;
    real bet;
    real L;
    real phi;

    // Asocial choice probability
    L =  exp(log_L + v_ID[id[i],1]);
    pA = softmax( L*A[id[i],1:6]' );
    //Social choice probability
    if ( sum(nmat_obs[i,:])==0 ) {
      p = pA;
    } else {
      // log-lin cue bias
      bet = gauss_beta + v_ID[id[i],4]   ;
      for ( j in 1:6 ) pS[j] = exp(bet*qmat_obs[i,j]);
      pS = pS / sum(pS);
      
      gam = inv_logit( logit_gam + v_ID[id[i],3]);
      // Combine everything to get choice probabilities for each optio
      p = (1-gam)*pA + gam*pS ;
    }
     //Multinomial Likelihood of observed choice
     ChoiceSelf[i] ~ categorical( p );
    // Second, update attractions conditional on observed choice
    pay[1:K] = rep_vector(0,6);
    pay[ ChoiceSelf[i] ] = 1;
    phi =  inv_logit(logit_phi + v_ID[id[i],2]);
    A[ id[i] , 1:6 ] = ( (1-phi)*to_vector(A[ id[i] , 1:6 ]) + phi*pay)';
  }//i
}

generated quantities{
    vector[N] log_lik;
    matrix[N_id,6] A; // Attraction matrix
    vector[K] pay;
    vector[K] pA;
    vector[K] pS;
    vector[K] p;
    real gam;
    real bet;
    real L;
    real phi;
    vector[N] pp;
    matrix[N_effects,N_effects] Rho_ID;

   Rho_ID = multiply_lower_tri_self_transpose(L_Rho_ID);

   for ( i in 1:N_id ) A[ i , 1:K ] = to_vector(AcInit[ i , 1:K ])';
          
  // loop over all observations 
  for ( i in 1:N ) {
 // Asocial choice probability
    L =  exp(log_L + v_ID[id[i],1]);
    pA = softmax( L*A[id[i],1:6]' );
    //Social choice probability
    if ( sum(nmat_obs[i,:])==0 ) {
      p = pA;
    } else {
      bet = gauss_beta + v_ID[id[i],4]   ;
      for ( j in 1:6 ) pS[j] = exp(bet*qmat_obs[i,j]);
      pS = pS / sum(pS);
      gam = inv_logit( logit_gam + v_ID[id[i],3]);
      // Combine everything to get choice probabilities for each optio
      p = (1-gam)*pA + gam*pS ;
    }
     //Multinomial Likelihood of observed choice
    log_lik[i] = categorical_lpmf(ChoiceSelf[i] | p);
    pp[i] = categorical_rng(p);
    // Second, update attractions conditional on observed choice
    pay[1:K] = rep_vector(0,6);
    pay[ ChoiceSelf[i] ] = 1;
    phi =  inv_logit(logit_phi + v_ID[id[i],2]);
    A[ id[i] , 1:6 ] = ( (1-phi)*to_vector(A[ id[i] , 1:6 ]) + phi*pay)';
  }//i
}
