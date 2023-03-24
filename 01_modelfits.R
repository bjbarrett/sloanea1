
#################################
##########slope models###########
#################################
library(rstan)
library(rethinking)
library(cmdstanr)
options(mc.cores=60) 

df <- read.csv("ST_28_aabbccrav_11Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi

#d <- df[df$forg_bout>=100,]
d <- df

d$max_bouts <- 0
for(r in 1:nrow(d)){
  d$max_bouts[r] <- max(d$forg_bout[d$mono_index==d$mono_index[r]])
}
d <- d[d$max_bouts>=100,]
d <- d[d$max_bouts<=200,]
nrow(d)

d$logage <- log(d$age)
d$logage_s <- (d$logage -mean(d$logage))/sd(d$logage)
d$logagecont <- log(d$agecont)
d$logagecont_s <- (d$logagecont -mean(d$logagecont))/sd(d$logagecont)
d$technique_index <- d$TECH_i
d$technique <- d$TECH
d$sex_index <- d$male + 1 #males 2, female 1
d$mono_index <- as.integer(as.factor(d$mono))
max(d$mono_index)
lambda_init <- median((exp(rnorm(100000,1,1))))
ac_init <- rep(.01,6)
ac_init[5] <- 3
ac_init <- exp(0)*ac_init
ac_init
exp(ac_init)/sum(exp(ac_init))
sum(exp(ac_init)/sum(exp(ac_init)))

#this initializes all individuals to be biased toward scrubbing
ac_mtx <- matrix(0, nrow = length( unique(d$mono_index) ), ncol = 6)
ac_mtx[,5] <- rep(3, length( unique(d$mono_index) ))

### individual learning models
# datalist_i <- list(
#   N = nrow(d),                                  #length of dataset
#   J = length( unique(d$mono_index) ),       #number of individuals
#   L = length( unique(d$grouptoday_i) ),       #number of individuals
#   K = max(d$technique_index),                   #number of processing techniques
#   tech = d$technique_index,                     #technique index
#   pay_i = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ),  #individual payoff at timestep (1 if succeed, 0 is fail)
#   bout = d$forg_bout,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   N_effects=2 ,                               #number of parameters to estimates
#   sex_index=d$sex_index,
#   age=d$agecont_s,
#   group_index=d$grouptoday_i,
#   ac_init = ac_init
# )

########CMDSTANR model fits#########
# file <- file.path("2023stancode/ewa_ind_s.stan")
# mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
# fit_i <- mod$sample(
#   data = datalist_i,
#   seed = 23,
#   chains = 3,
#   parallel_chains = 3,
#   refresh = 100,
#   iter_sampling = 500,
#   iter_warmup = 500,
#   threads=20,
#   adapt_delta = 0.9,
# )
# 
# stanfit <- rstan::read_stan_csv(fit_i$output_files())
# save(stanfit , file="ind_s.rds")
# 
# file <- file.path("2023stancode/ewa_ind_as.stan")
# mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
# fit_i <- mod$sample(
#   data = datalist_i,
#   seed = 3,
#   chains = 3,
#   parallel_chains = 3,
#   refresh = 10,
#   iter_sampling = 1000,
#   iter_warmup = 1000,
#   threads=20,
#   adapt_delta = 0.95,
# )
# 
# fit_i$summary(variables="sigma_i")
# fit_i$summary(variables="L")
# ead_stan_csv(fit_i$output_files())
# save(stanfit , file="ind_as.rds")
# 
# datalist_coho <- list(
#   N = nrow(d) ,                                  #length of dataset
#   J = length( unique(d$mono_index) ) ,       #number of individuals
#   K = max(d$technique_index) ,                   #number of processing techniques
#   L = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   pay_i = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   age = d$agecont_s ,
#   N_effects=4,                               #number of parameters to estimates
#   ac_init = ac_init
# )

####### individual learning models


datalist <- list(
  N = nrow(d) ,                                  #length of dataset
  N_id = length( unique(d$mono_index) ) ,       #number of individuals
  K = max(d$technique_index) ,                   #number of processing techniques
  ChoiceSelf = d$technique_index,                     #technique index
  bout = d$forg_bout ,                          #processing bout unique to individual J
  id = d$mono_index ,                      #individual ID
  N_effects=2,                               #number of parameters to estimates,
  AcInit = ac_mtx,
  sex_index = d$sex_index,
  age = d$logagecont_s
)

file <- file.path("2023stancode/ewa_ind_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_i <- mod$sample(
  data = datalist,
  seed = 3,
  chains = 3,
  parallel_chains = 3,
  refresh = 10,
  iter_sampling = 500,
  iter_warmup = 500,
  threads=20,
  adapt_delta = 0.8,
)
fit_i$save_object(file = "fit_i_cmd.rds")

stanfit <- rstan::read_stan_csv(fit_i$output_files())
save(stanfit , file="ind_cat.rds")


file <- file.path("2023stancode/ewa_ind_sex_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_i_sex <- mod$sample(
  data = datalist,
  seed = 3,
  chains = 3,
  parallel_chains = 3,
  refresh = 25,
  iter_sampling = 500,
  iter_warmup = 500,
  threads=20,
  adapt_delta = 0.8,
)

fit_i_sex$save_object(file = "fit_i_sex_cmd.rds")

file <- file.path("2023stancode/ewa_ind_age_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_i_age$save_object(file = "fit_i_age_cmd.rds")
######## frequency dependent learning
datalist <- list(
  N = nrow(d) ,                                  #length of dataset
  N_id = length( unique(d$mono_index) ) ,       #number of individuals
  K = max(d$technique_index) ,                   #number of processing techniques
  ChoiceSelf = d$technique_index,                     #technique index
  nmat_obs = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout ,                          #processing bout unique to individual J
  id = d$mono_index ,                      #individual ID
  N_effects=4 ,                               #number of parameters to estimates
  AcInit = ac_mtx,
  sex_index = d$sex_index,
  age = d$logagecont_s
)

file <- file.path("2023stancode/ewa_freq_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_f <- mod$sample(
  data = datalist,
  seed = 3,
  chains = 3,
  parallel_chains = 3,
  refresh = 10,
  iter_sampling = 500,
  iter_warmup = 500,
  threads=20,
  adapt_delta = 0.95,
)
fit_f$save_object(file = "fit_f_cmd.rds")

####cohort biased learning
datalist <- list(
  N = nrow(d) ,                                  #length of dataset
  N_id = length( unique(d$mono_index) ) ,       #number of individuals
  K = max(d$technique_index) ,                   #number of processing techniques
  ChoiceSelf = d$technique_index,                     #technique index
  nmat_obs = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  qmat_obs = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
  bout = d$forg_bout ,                          #processing bout unique to individual J
  id = d$mono_index ,                      #individual ID
  N_effects=4 ,                               #number of parameters to estimates
  AcInit = ac_mtx,
  sex_index = d$sex_index,
  age = d$logagecont_s
)

file <- file.path("2023stancode/ewa_cue_sex_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_coho_sex <- mod$sample(
  data = datalist,
  seed = 697,
  chains = 3,
  parallel_chains = 3,
  refresh = 10,
  iter_sampling = 500,
  iter_warmup = 500,
  threads=20,
  adapt_delta = 0.95,
)
fit_coho_sex$save_object(file = "fit_coho_sex_cmd.rds")

file <- file.path("2023stancode/ewa_cue_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_coho <- mod$sample(
  data = datalist,
  seed = 697,
  chains = 3,
  parallel_chains = 3,
  refresh = 10,
  iter_sampling = 500,
  iter_warmup = 500,
  threads=20,
  adapt_delta = 0.99,
)
fit_coho$save_object(file = "fit_coho_cmd.rds")

####age-biased learning
datalist <- list(
  N = nrow(d) ,                                  #length of dataset
  N_id = length( unique(d$mono_index) ) ,       #number of individuals
  K = max(d$technique_index) ,                   #number of processing techniques
  ChoiceSelf = d$technique_index,                     #technique index
  nmat_obs = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  qmat_obs = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
  bout = d$forg_bout ,                          #processing bout unique to individual J
  id = d$mono_index ,                      #individual ID
  N_effects=4 ,                               #number of parameters to estimates
  AcInit = ac_mtx,
  sex_index = d$sex_index,
  age = d$logagecont_s
)

file <- file.path("2023stancode/ewa_cue_cat.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )

fit_age <- mod$sample(
  data = datalist,
  seed = 60,
  chains = 3,
  parallel_chains = 3,
  refresh = 25,
  iter_sampling = 500,
  iter_warmup = 500,
  threads=20,
  adapt_delta = 0.95,
)
fit_age$save_object(file = "fit_age_cmd.rds")

####kin-bias
###sex-biasd
# 
# ###############################################################################
# ####lets try age curves for each individual####################################
# ###############################################################################
# 
# datalist_i <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   age = d$agecont_s ,
#   n_effects=2                               #number of parameters to estimates
# )
# 
# 
# parlist_i_sa <- c("phi" , "lambda" , "G" , "S" , "I", "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" ,"log_lik" , "PrPreds" ) # i deleted 
# fit_i_sa = stan( file = 'stancode_age_sex_id/ewa_individual_sex_age.stan', data = datalist_i ,
#                  iter = 1000, warmup=500, chains=4, cores=4, 
#                  control=list(adapt_delta=0.95) , pars=parlist_i_sa, refresh=50 , seed=666)
# precis(fit_i_sa ,  pars='I' , depth=3)
# ####freqdep####
# 
# datalist_coho <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   age = d$agecont_s ,
#   n_effects=4                               #number of parameters to estimates
# )
# 
# datalist_coho$q <- datalist_coho$q / max(datalist_coho$q)
# 
# ##frequencey dep##
# 
# parlist_freq_sa <- c("phi" , "lambda" , "gamma" , "fc" , "G" , "S" , "I" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_freq_sa = stan( file = 'stancode_age_sex_id/ewa_freq_sex_age.stan', data = datalist_coho , 
#                     iter = 1000, warmup=500, chains=4, cores=4, 
#                     control=list(adapt_delta=0.99) , pars=parlist_freq_sa, refresh=10 , seed=667)
# 
# ######cohort######
# parlist_coho_sa <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "I" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_coho_sa = stan( file = 'stancode_age_sex_id/ewa_cue_sex_age.stan', data = datalist_coho , 
#                     iter = 1000, warmup=500, chains=4, cores=4, 
#                     control=list(adapt_delta=0.99) , pars=parlist_coho_sa, refresh=10 , seed=668 , init=0)
# 
# #save(fit_i_sa , d , file="fit_i_sa_st_160320201222.rdata")
# 
# ###age_bias###########
# datalist_age <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   age = d$agecont_s ,
#   n_effects=4                               #number of parameters to estimates
# )
# 
# datalist_age$q <- datalist_age$q / max(datalist_age$q)
# 
# parlist_age_sa <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "I" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_age_sa = stan( file = 'stancode_age_sex_id/ewa_cue_sex_age.stan', data = datalist_age , 
#                    iter = 1000, warmup=500, chains=4, cores=4, seed=669 , init=0 , control=list(adapt_delta=0.95))
# 
# ###kin#######
# datalist_kin <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   age = d$agecont_s ,
#   n_effects=4                              #number of parameters to estimates
# )
# 
# datalist_kin$q <- datalist_kin$q / max(datalist_kin$q)
# 
# parlist_kin_sa <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "I" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) 
# fit_kin_sa = stan( file = 'stancode_age_sex_id/ewa_cue_sex_age.stan', data = datalist_kin , 
#                    iter = 1000, warmup=500, chains=4, cores=4, seed=232, control=list(adapt_delta=0.95) , init=0)
# 
# 
# ###global#######
# datalist_global <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   k = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   a = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
#   c = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   age = d$agecont_s ,
#   n_effects=7                              #number of parameters to estimates
# )
# 
# datalist_global$c <- datalist_global$c / max(datalist_global$c)
# datalist_global$k <- datalist_global$k / max(datalist_global$k)
# datalist_global$a <- datalist_global$a / max(datalist_global$a)
# 
# parlist_global_sa <- c("phi" , "lambda" , "gamma" , "beta_age" ,"beta_coho","beta_kin" , "G" , "S" , "I" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) 
# fit_global_sa = stan( file = 'stancode_age_sex_id/ewa_global_sex_age.stan', data = datalist_global , 
#                    iter = 1000, warmup=500, chains=4, cores=4, seed=1234, control=list(adapt_delta=0.98) , init=0)
# 
# 
# ####################################################################
# 
# 
# ##########individual learning alone############
# datalist_i <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   logage = d$logage_s ,
#   n_effects=2                               #number of parameters to estimates
# )
# 
# 
# parlist_i_s <- c("phi" , "lambda" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_i_s = stan( file = 'stancode_sex/ewa_individual_sex.stan', data = datalist_i , 
#                iter = 60, warmup=30, chains=4, cores=4, 
#                control=list(adapt_delta=0.95) , pars=parlist_i_s, refresh=10 , seed=666)
# 
# parlist_i_sa <- c("phi" , "lambda" , "G" , "S" , "bA" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" ,"log_lik" , "PrPreds" ) # i deleted 
# fit_i_sa = stan( file = 'stancode_age_sex/ewa_individual_sex_age.stan', data = datalist_i ,
#                  iter = 60, warmup=30, chains=2, cores=2, 
#                  control=list(adapt_delta=0.95) , pars=parlist_i_sa, refresh=10 , seed=666)
# 
# ####social learning
# 
# #cohort-bias and freq (we just need s)
# datalist_coho <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   logage = d$logage_s ,
#   n_effects=4                               #number of parameters to estimates
# )
# 
# 
# ##frequencey dep
# parlist_freq_s <- c("phi" , "lambda" , "gamma" , "fc" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_freq_s = stan( file = 'stancode_sex/ewa_freq_sex.stan', data = datalist_coho , 
#                    iter = 60, warmup=30, chains=4, cores=4, 
#                    control=list(adapt_delta=0.99) , pars=parlist_freq_s, refresh=10 , seed=666)
# 
# parlist_freq_sa <- c("phi" , "lambda" , "gamma" , "fc" , "bA" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_freq_sa = stan( file = 'stancode_age_sex/ewa_freq_sex_age.stan', data = datalist_coho , 
#                    iter = 60, warmup=30, chains=4, cores=4, 
#                    control=list(adapt_delta=0.99) , pars=parlist_freq_sa, refresh=10 , seed=666)
# 
# 
# datalist_coho$q <- datalist_coho$q / max(datalist_coho$q)
# 
# parlist_coho_s <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_coho_s = stan( file = 'stancode_sex/ewa_cue_sex.stan', data = datalist_coho , 
#                 iter = 60, warmup=30, chains=4, cores=4, 
#                 control=list(adapt_delta=0.99) , pars=parlist_coho_s, refresh=10 , seed=666)
# 
# parlist_coho_sa <- c("phi" , "lambda" , "gamma" , "beta" , "bA" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_coho_sa = stan( file = 'stancode_age_sex/ewa_cue_sex_age.stan', data = datalist_coho , 
#                    iter = 60, warmup=30, chains=4, cores=4, 
#                    control=list(adapt_delta=0.99) , pars=parlist_coho_sa, refresh=10 , seed=666)
# 
# 
# ###age_bias
# datalist_age <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   logage = d$logage_s ,
#   n_effects=4                               #number of parameters to estimates
# )
# 
# datalist_age$q <- datalist_age$q / max(datalist_age$q)
# 
# parlist_age_s <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_age_s = stan( file = 'stancode_sex/ewa_cue_sex.stan', data = datalist_age , 
#                    iter = 60, warmup=30, chains=4, cores=4, 
#                    control=list(adapt_delta=0.99) , pars=parlist_age_s, refresh=10 , seed=666)
# 
# parlist_age_sa <- c("phi" , "lambda" , "gamma" , "beta" , "bA" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_coho_sa = stan( file = 'stancode_age_sex/ewa_cue_sex_age.stan', data = datalist_age , 
#                     iter = 60, warmup=30, chains=4, cores=4, 
#                     control=list(adapt_delta=0.99) , pars=parlist_age_sa, refresh=10 , seed=666)
# 
# ###kin
# datalist_kin <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   logage = d$logage_s ,
#   n_effects=4                               #number of parameters to estimates
# )
# 
# datalist_kin$q <- datalist_kin$q / max(datalist_kin$q)
# 
# parlist_kin_s <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_age_s = stan( file = 'stancode_sex/ewa_cue_sex.stan', data = datalist_kin , 
#                   iter = 60, warmup=30, chains=4, cores=4, 
#                   control=list(adapt_delta=0.99) , pars=parlist_kin_s, refresh=10 , seed=666)
# 
# parlist_kin_sa <- c("phi" , "lambda" , "gamma" , "beta" , "bA" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_kin_sa = stan( file = 'stancode_age_sex/ewa_cue_sex_age.stan', data = datalist_kin , 
#                     iter = 60, warmup=30, chains=4, cores=4, 
#                     control=list(adapt_delta=0.99) , pars=parlist_kin_sa, refresh=10 , seed=666)
# ###sex
# datalist_sex <- list(
#   n_obs = nrow(d) ,                                  #length of dataset
#   n_id = length( unique(d$mono_index) ) ,       #number of individuals
#   n_behav = max(d$technique_index) ,                   #number of processing techniques
#   n_group = max(d$grouptoday_i) ,
#   tech = d$technique_index,                     #technique index
#   y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$x1 , d$x2 , d$x3 , d$x4 ,d$x5 ,d$x6  ) ,
#   s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
#   bout = d$forg_bout ,                          #processing bout unique to individual J
#   id = d$mono_index ,                      #individual ID
#   sex_index=d$sex_index ,
#   group_index=d$grouptoday_i ,
#   logage = d$logage_s ,
#   n_effects=4                               #number of parameters to estimates
# )
# 
# datalist_sex$q <- datalist_sex$q / max(datalist_sex$q)
# 
# parlist_sex_s <- c("phi" , "lambda" , "gamma" , "beta" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_age_s = stan( file = 'stancode_sex/ewa_cue_sex.stan', data = datalist_sex , 
#                   iter = 60, warmup=30, chains=4, cores=4, 
#                   control=list(adapt_delta=0.99) , pars=parlist_sex_s, refresh=10 , seed=666)
# 
# parlist_sex_sa <- c("phi" , "lambda" , "gamma" , "beta" , "bA" , "G" , "S" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" ) # I deleted
# fit_sex_sa = stan( file = 'stancode_age_sex/ewa_cue_sex_age.stan', data = datalist_sex , 
#                    iter = 60, warmup=30, chains=4, cores=4, 
#                    control=list(adapt_delta=0.99) , pars=parlist_sex_sa, refresh=10 , seed=666)
# 
# 
