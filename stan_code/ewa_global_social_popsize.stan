data {
    int K;              // num options(gnomes)
    int N;              // num observations
    int J;              // num individuals
    int tech[N];        // technique chosen
    real y[N,K];        // observed personal yields of techs 1-K
    real c[N,K];        // observed cohort social variables of techs 1-K
    real a[N,K];        // observed age-bias social variables of techs 1-K
    real k[N,K];        // observed kin-bias social variables of techs 1-K
    real s[N,K];        // observed number of ttimes observing behaviors
    int bout[N];        // bout or fruit
    int id[N];          // player id
    int N_effects;      // number of learning parameters to estimate
    real popsize[N];           // 
}
parameters {
    real<lower=0> lambda;                   // mutlinomial error parameter
    vector[N_effects] mu;                   // average effects
    matrix[N_effects,J] zed;                // individual z-scores
    cholesky_factor_corr[N_effects] L_Rho;  // correlation matrix
    vector<lower=0>[N_effects] sigma;       // standard deviations
    vector[N_effects] b_p;

}
transformed parameters{
    matrix[J,N_effects] a_id;
    // put scales and correlations back in
    a_id = (diag_pre_multiply(sigma,L_Rho) * zed)';
}
model {
    vector[K] AC;        // attraction scores
    real logPrA;         // individual learning temp
    real PrS;            // social learning temp
    vector[K] s_temp;        
    vector[K] lin_mod;
    real phi;            // stickiness parameter
    real gamma;          // social weight
    real beta_coho;           // payoff exponent
    real beta_age;           // payoff exponent
    real beta_kin;           // payoff exponent
    real fconf;          // conform exponent

    //priors
    lambda ~ exponential(1);
    mu ~ normal(0,1);
    sigma ~ exponential(1);
    to_vector(zed) ~ normal(0,1);
    L_Rho ~ lkj_corr_cholesky(3);
    b_p ~ normal(0,1);

    for ( i in 1:N ) {
        //update attraction scores
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            phi = inv_logit( mu[1] + a_id[id[i],1] + b_p[1]*popsize[i] );  //attraction to new experience
            gamma = inv_logit( mu[2] + a_id[id[i],2] + b_p[2]*popsize[i] ); //weight given to social info
            fconf = exp( mu[3] + a_id[id[i],3] + b_p[3]*popsize[i]);                         // strength of frequency dependence
            beta_coho = ( mu[4] + a_id[id[i],4] + b_p[4]*popsize[i]);                             //contribution of cohort bias
            beta_age = ( mu[5] + a_id[id[i],5] + b_p[5]*popsize[i]);                             //contribution of age bias
            beta_kin = ( mu[6] + a_id[id[i],6] + b_p[6]*popsize[i]);                             //contribution of kin bias

        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );           // calculate individual learning prob

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {                                       //only use social info when it is observed

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( beta_coho*c[i,j] +  beta_age*a[i,j] + beta_kin*k[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fconf);
                for ( j in 1:K ) lin_mod[j] = lin_mod[j] * s_temp[j];

                PrS = lin_mod[tech[i]]/sum(lin_mod);
                
                target += ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) );

            } else {
                target += ( logPrA );
            }
        } else {
            target += ( logPrA );
         }
     }//i  

}//end of model

generated quantities{
    real dev;            //deviance to calc DIC
    vector[N] log_lik;   //log-likelihood to calc WAIC with compare fcn 

    vector[K] AC;        // attraction scores
    real logPrA;         // individual learning temp
    real PrS;            // social learning temp
    vector[K] s_temp;        
    vector[K] lin_mod;
    real phi;            // stickiness parameter
    real gamma;          // social weight
    real beta_coho;           // payoff exponent
    real beta_age;           // payoff exponent
    real beta_kin;           // payoff exponent
    real fconf;          // conform exponent
    matrix[N_effects,N_effects] Rho;
    vector[N_effects] Sigma;

    Sigma = sigma;
    Rho = L_Rho * L_Rho';

    dev=0;
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            phi = inv_logit( mu[1] + a_id[id[i],1] + b_p[1]*popsize[i] );  //attraction to new experience
            gamma = inv_logit( mu[2] + a_id[id[i],2] + b_p[2]*popsize[i] ); //weight given to social info
            fconf = exp( mu[3] + a_id[id[i],3] + b_p[3]*popsize[i]);                         // strength of frequency dependence
            beta_coho = ( mu[4] + a_id[id[i],4] + b_p[4]*popsize[i]);                             //contribution of cohort bias
            beta_age = ( mu[5] + a_id[id[i],5] + b_p[5]*popsize[i]);                             //contribution of age bias
            beta_kin = ( mu[6] + a_id[id[i],6] + b_p[6]*popsize[i]);                             //contribution of kin bias
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( beta_coho*c[i,j] +  beta_age*a[i,j] + beta_kin*k[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fconf);
                for ( j in 1:K ) lin_mod[j] = lin_mod[j] * s_temp[j];
                PrS = lin_mod[tech[i]]/sum(lin_mod);
                
                dev = dev + -2*( log( (1-gamma)*exp(logPrA) + gamma*PrS ) );
                log_lik[i] = ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) ) ;  

            } else {
                 dev = dev + -2*( logPrA );
                 log_lik[i] = (logPrA);
            }
        } else {
                 dev = dev + -2*( logPrA );
                 log_lik[i] = (logPrA);           }
     }//i  
}//end of model
