
data {
    int n_behav;              // num behaviors
    int n_obs;              // num observations in dataset
    int n_id;              // num individuals
    int n_group;              // num groups
    int tech[n_obs];        // techique chosen
    real y[n_obs , n_behav];        // observed personal yields of techs (1/0)
    real q[n_obs , n_behav];       // observed cue social variables of techs 1-K
    real s[n_obs , n_behav];        // observed number of ttimes observing behaviors
    int bout[n_obs];        // processing bout per individual
    int id[n_obs];          // individual id
    real age[n_obs];          // individual id
    int n_effects;      // number of learning parameters to estimate
    int sex_index[n_obs];   //index variable for sex, 1 female, 2 male
    int group_index[n_obs];   //index variable for group
}

parameters {
    matrix[2 , n_effects] S;                    //sex  means
    //matrix[2 , n_effects] bA;                //age params per sex
    vector<lower=0>[n_effects*2] sigma_i;       // standard deviations of varying effects
    matrix[n_effects*2 , n_id] zed_i;                // individual z-scores for cholesky decomp
    cholesky_factor_corr[n_effects*2] L_Rho_i;  // correlation matrix
    vector<lower=0>[n_effects] sigma_g;       // standard deviations of varying effects
    matrix[n_effects , n_group] zed_g;                // group z-scores for cholesky decomp
    cholesky_factor_corr[n_effects] L_Rho_g;  // correlation matrix

}
transformed parameters{
    matrix[n_id , n_effects*2] I;              //define varying effects for individuals
    matrix[n_group , n_effects] G;              //define varying effects for groups
    I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
    G = (diag_pre_multiply(sigma_g,L_Rho_g) * zed_g)'; //cholesky decomp majick
}
model {
    vector[n_behav] AC;       // attraction scores
    real logPrA;        // asocial learning logprob
    real PrS;           // social learning temp
    vector[n_behav] lin_mod;  // loglinear model cues
    real phi[n_id];           // stickiness parameter to recent experience
    real lambda[n_id];        // sensitivity to attraction scores
    real gamma[n_id];         // social weight
    real beta[n_id];     // conform exponent

    //priors
    to_vector(S[,1]) ~ normal(1,0.6);
    to_vector(S[,2]) ~ normal(0,1);
    to_vector(S[,3]) ~ normal(0,1);
    to_vector(S[,4]) ~ normal(0,1);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,1);
    L_Rho_i ~ lkj_corr_cholesky(3);
    sigma_g ~ exponential(1);
    to_vector(zed_g) ~ normal(0,1);
    L_Rho_g ~ lkj_corr_cholesky(3);

    for ( i in 1:n_obs ) {
        //update attractions + reinforcement learning
        for ( j in 1:n_behav ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi[id[i]])*AC[j] + phi[id[i]]*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j
        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda[id[i]] = exp( I[id[i],1] + G[group_index[i],1] + S[sex_index[i] , 1] + I[id[i] , 5]*age[i] ) ;
            phi[id[i]]= inv_logit(  I[id[i],2] + G[group_index[i],2]  + S[sex_index[i] , 2] + I[id[i] , 6]*age[i] );
            gamma[id[i]] = inv_logit(  I[id[i],3] + G[group_index[i],3]  + S[sex_index[i] , 3] + I[id[i] , 7]*age[i] );
            beta[id[i]] =  I[id[i] , 4] + G[group_index[i] , 4]  + S[sex_index[i] , 4] +  I[id[i] , 8]*age[i]  ;
        }
        logPrA = lambda[id[i]]*AC[tech[i]] - log_sum_exp( lambda[id[i]]*AC );
        
        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info
                for ( j in 2:n_behav ) {
                    lin_mod[j] = exp( beta[id[i]]*q[i,j]);                 // compute non-frequency cue as log-linear model
                }
                lin_mod[1] = 1; // aliased outcome
                PrS = lin_mod[tech[i]]/sum(lin_mod);
                target += ( log( (1-gamma[id[i]])*exp(logPrA) + gamma[id[i]]*PrS ) );
            } else {
                target += ( logPrA );
            }
        } else {
            target += ( logPrA );
         }
     }//i  

}//end of model

generated quantities{
    vector[n_obs] log_lik;
    vector[n_behav] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[n_behav] lin_mod;
    real lambda[n_id];           // stickiness parameter
    real phi[n_id];           // stickiness parameter
    real gamma[n_id];         // social weight
    real beta[n_id];     // cue-bias
    matrix[n_effects*2,n_effects*2] Rho_i;
    matrix[n_effects,n_effects] Rho_g;
    matrix[n_obs , n_behav] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);
    Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);


    for ( i in 1:n_obs ) {
        //update attractions
        for ( j in 1:n_behav ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi[id[i]])*AC[j] + phi[id[i]]*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda[id[i]] = exp( I[id[i],1] + G[group_index[i],1] + S[sex_index[i] , 1] + I[id[i] , 5]*age[i] ) ;
            phi[id[i]]= inv_logit(  I[id[i],2] + G[group_index[i],2]  + S[sex_index[i] , 2] + I[id[i] , 6]*age[i] );
            gamma[id[i]] = inv_logit(  I[id[i],3] + G[group_index[i],3]  + S[sex_index[i] , 3] + I[id[i] , 7]*age[i] );
            beta[id[i]] =  I[id[i] , 4] + G[group_index[i] , 4]  + S[sex_index[i] , 4] +  I[id[i] , 8]*age[i]  ;
        }

        logPrA = lambda[id[i]]*AC[tech[i]] - log_sum_exp( lambda[id[i]]*AC );

        //only socially learn if there is social info
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {

                // compute non-frequency cues as log-linear model
                for ( j in 2:n_behav ) {
                    lin_mod[j] = exp( beta[id[i]]*q[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome
                // compute frequency cue
                PrS = lin_mod[tech[i]]/sum(lin_mod);

                log_lik[i] =  log( (1-gamma[id[i]])*exp(logPrA) + gamma[id[i]]*PrS )  ; 
                
                for(j in 1:n_behav){
                PrPreds[i,j] = (1-gamma[id[i]])*exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) ) + gamma[id[i]]*(lin_mod[j]/sum(lin_mod)) ;
                }
                
            } else {
                 log_lik[i] = (logPrA);
                 
                 for(j in 1:n_behav){
                    PrPreds[i,j] = exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) );
                 }
            }
        } else {
                 log_lik[i] = (logPrA);
                 
                 for(j in 1:n_behav){
                    PrPreds[i,j] = exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) );
                }
            }
     }//i  

}//end of model
