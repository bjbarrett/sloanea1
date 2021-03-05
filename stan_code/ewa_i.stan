data {
int K;              // num options
int N;              // num observations
int J;              // num individuals
int tech[N];        // tech chosen
real y[N,K];        // observed personal yields of techs
int bout[N];      // bout
int id[N];          // player id
}
parameters {
real<lower=0> lambda;   // mutlinomial error parameter
real alpha;             // stickiness/memory parameter
real a_indiv[J];         //varying effects for individual leanring
real<lower=0> sigma_indiv;
}
model {
vector[K] AC;       // attraction scores
real logPrA;        // individual learning temp
real temp;		// stickiness parameter

//priors
lambda ~ exponential(1);
sigma_indiv ~ exponential(2);
alpha ~ normal(0,1);
a_indiv ~ normal(0,sigma_indiv);
for ( i in 1:N ) {
//update attractions
	for ( j in 1:K ) {
		if ( bout[i] > 1 ) {
			temp <- inv_logit( alpha + a_indiv[id[i]] );
			AC[j] <- (1-temp)*AC[j] + temp*y[i-1,j];
		} else {
			AC[j] <- 0;
		}
	}//j
	logPrA <- lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
	increment_log_prob( logPrA );

	}//i
}

generated quantities {
real dev; // deviance
vector[K] AC;       // attraction scores
real logPrA;        // individual learning temp
real temp;		// stickiness parameter
vector[N] log_lik;
vector[J] phi_i;          // stickiness parameter ve
real phi;



dev <- 0;
for ( i in 1:N ) {
//update attractions
	for ( j in 1:K ) {
		if ( bout[i] > 1 ) {
			temp <- inv_logit( alpha + a_indiv[id[i]] );
			AC[j] <- (1-temp)*AC[j] + temp*y[i-1,j];
		} else {
			AC[j] <- 0;
		}
	}//j
	logPrA <- lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
	dev <- dev + -2*logPrA;
	log_lik[i] <- logPrA ;

	}//i
	phi <- inv_logit(alpha);
	for (j in 1:J){
		phi_i[j] <- inv_logit( alpha + a_indiv[J] );
		}
}