
data {
    int K;              // num options(gnomes)
    int N;              // num observations
    int J;              // num individuals
    int tech[N];        // tech chosen
    real pay_i[N,K];        // observed personal yields of techs 1-K
    real f[N,K];       // observed female social variables of techs 1-K
    real k[N,K];       // observed kin-bias social variables of techs 1-K
    real p[N,K];       // observed payoff social variables of techs 1-K
    real r[N,K];       // observed rank-bias social variables of techs 1-K
    real s[N,K];        // observed number of ttimes observing behaviors
    real x[N,K];        // sex-bias
    int bout[N];        // bout or fruit
    int id[N];          // player id
    int N_effects;      // number of learning parameters to estimate
    int sex_index[N];   //index variable for sex; 1 is female 2 is male
    int age_index[N];   //index variable for age; 1 juv 2 adult
    int group_index[N];   //index variable for group; 1 kubu 2 is noha
}
parameters {
    matrix[N_effects,2] A;                  //age means
    matrix[N_effects,2] S;                  //sex  means
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
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] lin_mod;
    vector[K] s_temp;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real fc;             //frequency dependence
    real bf;             // female-bias
    real bk;             // kin-bias
    real bp;             // payoff-bias
    real br;             // rank-bias
    real bx;             // sex-bias

    //priors
    to_vector(A[1,]) ~ normal(1,0.6);
    to_vector(S[1,]) ~ normal(1,0.6);
    to_vector(A[2,]) ~ normal(0,1);
    to_vector(S[2,]) ~ normal(0,1);
    to_vector(A[3,]) ~ normal(0,1);
    to_vector(S[3,]) ~ normal(0,1);
    to_vector(A[4,]) ~ normal(0,0.5);
    to_vector(S[4,]) ~ normal(0,0.5);
    to_vector(A[5,]) ~ normal(0,0.8);
    to_vector(S[5,]) ~ normal(0,0.8);
    to_vector(A[6,]) ~ normal(0,0.8);
    to_vector(S[6,]) ~ normal(0,0.8);
    to_vector(A[7,]) ~ normal(0,0.8);
    to_vector(S[7,]) ~ normal(0,0.8);
    to_vector(A[8,]) ~ normal(0,0.8);
    to_vector(S[8,]) ~ normal(0,0.8);
    to_vector(A[9,]) ~ normal(0,0.8);
    to_vector(S[9,]) ~ normal(0,0.8);  
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
                AC[j] = (1-phi)*AC[j] + phi*pay_i[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( I[id[i],1] + G[group_index[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;
            phi= inv_logit(  I[id[i],2] + G[group_index[i],2] +  A[2,age_index[i]] + S[2,sex_index[i]]);
            gamma = inv_logit(I[id[i],3] + G[group_index[i],3] + A[3,age_index[i]] + S[3,sex_index[i]] );
            fc = exp(I[id[i],4] + G[group_index[i],4] + A[4,age_index[i]] + S[4,sex_index[i]]);
            bf =  I[id[i],5] + G[group_index[i],5] + A[5,age_index[i]] + S[5,sex_index[i]] ; // female
            bk =  I[id[i],6] + G[group_index[i],6] + A[6,age_index[i]] + S[6,sex_index[i]] ; // kin
            bp =  I[id[i],7] + G[group_index[i],7] + A[7,age_index[i]] + S[7,sex_index[i]] ; // payoff
            br =  I[id[i],8] + G[group_index[i],8] + A[8,age_index[i]] + S[8,sex_index[i]] ; // rank
            bx =  I[id[i],9] + G[group_index[i],9] + A[9,age_index[i]] + S[9,sex_index[i]] ; // same sex
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( bf*f[i,j] + bk*k[i,j] + bp*p[i,j] + br*r[i,j] + bx*x[i,j] );
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
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
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] lin_mod;
    vector[K] s_temp;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real fc;             //frequency dependence
    real bf;             // female-bias
    real bk;             // kin-bias
    real bp;             // payoff-bias
    real br;             // rank-bias
    real bx;             // sex-bias
    matrix[N_effects,N_effects] Rho_i;
    matrix[N_effects,N_effects] Rho_g;
    matrix[N,K] PrPreds;     

    Rho_i = L_Rho_i * L_Rho_i';
    Rho_g = L_Rho_g * L_Rho_g';

 for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*pay_i[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( I[id[i],1] + G[group_index[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;
            phi= inv_logit(  I[id[i],2] + G[group_index[i],2] +  A[2,age_index[i]] + S[2,sex_index[i]]);
            gamma = inv_logit(I[id[i],3] + G[group_index[i],3] + A[3,age_index[i]] + S[3,sex_index[i]] );
            fc = exp(I[id[i],4] + G[group_index[i],4] + A[4,age_index[i]] + S[4,sex_index[i]]);
            bf =  I[id[i],5] + G[group_index[i],5] + A[5,age_index[i]] + S[5,sex_index[i]] ; // female
            bk =  I[id[i],6] + G[group_index[i],6] + A[6,age_index[i]] + S[6,sex_index[i]] ; // kin
            bp =  I[id[i],7] + G[group_index[i],7] + A[7,age_index[i]] + S[7,sex_index[i]] ; // payoff
            br =  I[id[i],8] + G[group_index[i],8] + A[8,age_index[i]] + S[8,sex_index[i]] ; // rank
            bx =  I[id[i],9] + G[group_index[i],9] + A[9,age_index[i]] + S[9,sex_index[i]] ; // same sex
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( bf*f[i,j] + bk*k[i,j] + bp*p[i,j] + br*r[i,j] + bx*x[i,j] );
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                for ( j in 1:K ) lin_mod[j] = lin_mod[j] * s_temp[j];
                PrS = lin_mod[tech[i]]/sum(lin_mod);

                log_lik[i] =  log( (1-gamma)*exp(logPrA) + gamma*PrS )  ; 
                for(j in 1:K){
                PrPreds[i,j] = (1-gamma)*exp( lambda*AC[j] - log_sum_exp( lambda*AC) ) + gamma*(lin_mod[j]/sum(lin_mod)) ;
                }
            } else {
                 log_lik[i] = (logPrA);
                 for(j in 1:K){
                    PrPreds[i,j] = exp( lambda*AC[j] - log_sum_exp( lambda*AC) );
                 }
            }
        } else {
                 log_lik[i] = (logPrA);
                 for(j in 1:K){
                    PrPreds[i,j] = exp( lambda*AC[j] - log_sum_exp( lambda*AC) );
                }
            }
     }//i  

}//end of model
