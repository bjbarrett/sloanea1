
data {
    int K;              // num options(gnomes)
    int N;              // num observations
    int J;              // num individuals
    int tech[N];        // tech chosen
    real y[N,K];        // observed personal yields of techs 1-K
    real s[N,K];        // observed social variables of techs 1-K
    int bout[N];        // bout
    int id[N];          // player id
    int N_effects;      // number of learning parameters to estimate
}
parameters {
    real<lower=0> lambda;                   // mutlinomial error parameter
    vector[N_effects] mu;                   // average effects
    matrix[N_effects,J] zed;                // individual z-scores
    cholesky_factor_corr[N_effects] L_Rho;  // correlation matrix
    vector<lower=0>[N_effects] sigma;       // standard deviations
}
transformed parameters{
    matrix[J,N_effects] a_id;
    // put scales and correlations back in
    a_id = (diag_pre_multiply(sigma,L_Rho) * zed)';
}
model {
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] s_temp;
    real phi;          // stickiness parameter
    real gamma;          // social weight
    real fconf;         // conformity exponent

    //priors
    lambda ~ exponential(1);
    mu ~ normal(0,1);
    sigma ~ exponential(1); 
    to_vector(zed) ~ normal(0,1);
    L_Rho ~ lkj_corr_cholesky(3);

    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                phi = inv_logit( mu[1] + a_id[id[i],1] );
                AC[j] = (1-phi)*AC[j] + phi*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        if ( bout[i] > 1 ) {
            if ( sum(s[i]) > 0 ) {
                fconf = exp( mu[2] + a_id[id[i],2] );
                for ( j in 1:K ) {
                    s_temp[j] = pow(s[i,j],fconf);
                }
                PrS =  s_temp[tech[i]] / sum(s_temp) ;

                gamma = inv_logit( mu[3] + a_id[id[i],3] );
                target +=( log( (1-gamma)*exp(logPrA) + gamma*PrS ) );
            } else {
                target +=( logPrA );
            }
        } else {
            target +=( logPrA );
        }
    }//i
}//end of model

generated quantities{
	  real dev;		//deviance
      vector[N] log_lik;
   	  vector[K] AC;       // attraction scores
   	  real logPrA;        // individual learning temp
	  real PrS;        // social learning temp
   	  vector[K] s_temp;
   	  real phi;          // stickiness parameter
   	  real gamma;          // social weight
   	  real fconf;         // conformity exponent
      vector[J] phi_i;          // stickiness parameter ve
      vector[J] gamma_i;          // social weight veff
      vector[J] fconf_i;         // conformity exponent ve
      matrix[N_effects,N_effects] Rho;
      vector[N_effects] Sigma;

    Sigma = sigma;
    Rho = L_Rho * L_Rho';

    dev = 0;
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                phi = inv_logit( mu[1] + a_id[id[i],1] );
                AC[j] = (1-phi)*AC[j] + phi*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

          if ( bout[i] > 1 ) {
            if ( sum(s[i]) > 0 ) {
                fconf = exp( mu[2] + a_id[id[i],2] );
                for ( j in 1:K ) {
                    s_temp[j] = pow(s[i,j],fconf);
                }
                PrS =  s_temp[tech[i]] / sum(s_temp) ;

                gamma = inv_logit( mu[3] + a_id[id[i],3] );
                dev = dev + -2*log( (1-gamma)*exp(logPrA) + gamma*PrS ) ;
                log_lik[i] = log( (1-gamma)*exp(logPrA) + gamma*PrS ) ;
            } else {
               dev = dev + -2*logPrA ;
               log_lik[i] = logPrA ;
            }
        } else {
            dev = dev + -2*logPrA ;
            log_lik[i] = logPrA ;
        }
    }//i

    for (j in 1:J){
        gamma_i[j] =  inv_logit( mu[3] + a_id[j,3] );
        phi_i[j] =  inv_logit( mu[1] + a_id[j,1] );
        fconf_i[j] = exp( mu[2] + a_id[j,2] );
        }

}
