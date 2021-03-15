# ssh -l brendan_barrett ecocn03
# ssh -l brendan_barrett ecocn04
# R
library(rethinking)
library(rstan)
#setwd("~/Dropbox/sloanea/")
#d <- read.csv("~/Dropbox/sloanea/ST_28_aabbrav_9Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi
df <- read.csv("ST_28_aabbccrav_11Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi
d <- df

# d$s_all <- d$s1 + d$s2 + d$s3 + d$s4 + d$s5 + d$s6

# for(r in 1:nrow(d)){
# 	d$first_obs[r] <- min(d$forg_bout[d$mono_index==d$mono_index[r] & d$s_all > 0])
# }

# d$post_obs <- ifelse(d$forg_bout < d$first_obs , 0 , 1) #assigns a 01if after social info observed for EMA in model social component

# d_long <- d
# d<- d[1:2000,]

# tmux a -t 0

#monkeys must have at least 10 observations to be analyzed
# d$max_bouts <- 0
# for(r in 1:nrow(d)){
# 	d$max_bouts[r] <- max(d$forg_bout[d$mono_index==d$mono_index[r]])
# }

# d <- d[d$max_bouts>=20,]

# d$obs <- max(d$forgbout[d$mono])
# d$mono[ which.max(d$forg_bout < 10) ] )

# as.data.frame(table(d$mono))
d$mono_index <- (as.integer(as.factor(d$mono)))
#d <- d[1:4000,]
d$pop_month_s <- (d$pop_month - mean(d$pop_month)) / sd(d$pop_month)

# datalist_i <- list(
# N = nrow(d),
# J = length( unique(d$mono_index) ),
# K=max(d$TECH_i),
# tech = d$TECH_i,
# y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
# bout = d$forg_bout,#bout is forg index here
# id = d$mono_index,
# N_effects=1,
# year=d$year,
# age=d$agecont_s

# )
d$logage <- log(d$age)
d$logage_s <- (d$logage -mean(d$logage))/sd(d$logage)
d$logagecont <- log(d$agecont)
d$logagecont_s <- (d$logagecont -mean(d$logagecont))/sd(d$logagecont)

datalist_s <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects=3,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$logagecont_s,
  postobs=d$post_obs
  
)


datalist_coho <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  q = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects=3,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$agecont_s,
  postobs=d$post_obs
  
)

datalist_coho$q <- datalist_coho$q / max(datalist_coho$q)

datalist_kin <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  q = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects=3,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$agecont_s,
  postobs=d$post_obs
  
)
datalist_kin$q <- datalist_kin$q / max(datalist_kin$q)

datalist_age <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  q = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects=3,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$logagecont_s,
  postobs=d$post_obs
  
)
datalist_age$a <- datalist_age$a / max(datalist_age$a)

datalist_global <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  c = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
  a = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
  k = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects=6,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$logagecont_s,
  postobs=d$post_obs,
  popsize=d$pop_month_s
)

datalist_global$a <- datalist_global$a / max(datalist_global$a)
datalist_global$c <- datalist_global$c / max(datalist_global$c)
datalist_global$k <- datalist_global$k / max(datalist_global$k)

datalist_global_slopes <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  c = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
  a = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
  k = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects = 12,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$agecont_s,
  postobs=d$post_obs
  
)

datalist_global_slopes$a <- datalist_global_slopes$a / max(datalist_global_slopes$a)
datalist_global_slopes$c <- datalist_global_slopes$c / max(datalist_global_slopes$c)
datalist_global_slopes$k <- datalist_global_slopes$k / max(datalist_global_slopes$k)

datalist_coho_slopes <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  q = cbind(d$c1 , d$c2 , d$c3 , d$c4 ,d$c5 ,d$c6  ) ,
  # a = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
  # k = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects = 6,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$logagecont_s,
  postobs=d$post_obs
  
)

datalist_coho_slopes$q <- datalist_coho_slopes$q / max(datalist_coho_slopes$q)

datalist_age_slopes <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  q = cbind(d$a1 , d$a2 , d$a3 , d$a4 ,d$a5 ,d$a6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects = 6,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$logagecont_s,
  postobs=d$post_obs
  
)

datalist_age_slopes$q <- datalist_age_slopes$q / max(datalist_age_slopes$q)

datalist_kin_slopes <- list(
  N = nrow(d),
  J = length( unique(d$mono_index) ),
  K=max(d$TECH_i),
  tech = d$TECH_i,
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
  q = cbind(d$k1 , d$k2 , d$k3 , d$k4 ,d$k5 ,d$k6  ) ,
  s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
  bout = d$forg_bout,#bout is forg index here
  id = d$mono_index,
  N_effects = 6,
  postmig=d$postmig,
  postfizz = d$postfizz, 
  age=d$logagecont_s,
  postobs=d$post_obs
  
)

datalist_kin_slopes$q <- datalist_kin_slopes$q / max(datalist_kin_slopes$q)

###list of pars to include
parlistF=c("a_id" , "mu", "lambda", "sigma" , "Rho", "phi_i" , "gamma_i" , "fconf_i" , "dev" , "log_lik","Rho")
parlistQ=c("a_id" , "mu", "lambda" , "dev" , "log_lik" , "sigma" , "Rho")
parlistQage=c("a_id" , "mu", "lambda" , "dev" , "log_lik" , "sigma" , "Rho" )
parlistQpop=c("a_id" , "mu", "lambda" , "dev" , "log_lik" , "sigma" , "Rho" , "b_p")
#no age or sex predictors
fit_s = stan( file = 'ewa_freq_social.stan', data = datalist_s ,iter = 1000, warmup=500, chains=4, cores= 4, pars=parlistF, control=list(adapt_delta=0.98), init="0" , refresh = 1)
fit_coho = stan( file = 'ewa_cue_social.stan', data = datalist_coho ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_age = stan( file = 'ewa_cue_social.stan', data = datalist_age ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_kin = stan( file = 'ewa_cue_social.stan', data = datalist_kin ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_global = stan( file = 'ewa_global_social.stan', data = datalist_global ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))

###varying slopes for age
fit_global_age_slopes = stan( file = 'ewa_global_social_age_slopes.stan', data = datalist_global_slopes ,iter = 1000, warmup=600, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99) , init="0" , refresh = 1)
fit_coho_age_slopes = stan( file = 'ewa_cue_social_age_slopes.stan', data = datalist_coho_slopes ,iter = 1000, warmup=600, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99) , init="0" , refresh = 1)
fit_age_age_slopes = stan( file = 'ewa_cue_social_age_slopes.stan', data = datalist_age_slopes ,iter = 1000, warmup=600, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99) , init="0" , refresh = 1)
fit_kin_age_slopes = stan( file = 'ewa_cue_social_age_slopes.stan', data = datalist_kin_slopes ,iter = 1000, warmup=600, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99) , init="0" , refresh = 1)
fit_freq_age_slopes = stan( file = 'ewa_freq_social_age_slopes.stan', data = datalist_kin_slopes ,iter = 1000, warmup=600, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99) , init="0" , refresh = 1)

#####popsize
parlistQpop=c("a_id" , "mu", "lambda" , "dev" , "log_lik" , "sigma" , "Rho" , "b_p")
fit_global_pop = stan( file = 'ewa_global_social_popsize.stan', data = datalist_global ,iter = 1200, warmup=600, chains=3, cores= 3, pars=parlistQpop , refresh = 50, control=list(adapt_delta=0.99))

fit_i = stan(file = 'ewa_i.stan', data = datalist_i ,iter = 2000, warmup=1000, chains=2,  pars=parlistI , control=list(adapt_delta=0.99))
write.csv(extract(fit_s) , "post_s_ST.csv")

traceplot(fit_global_age_slopes2 , pars='mu') , nrow=4 , ncol= 5)
plot(fit_global_age_slopes , pars='mu') 
compare(fit_global_age_slopes , fit_coho_age_slopes , fit_age_age_slopes , fit_kin_age_slopes , fit_freq_age_slopes , fit_global_age_male_slopes)


save(d, fit_global_age_slopes , fit_coho_age_slopes , fit_age_age_slopes , fit_kin_age_slopes , fit_freq_age_slopes , file="fits_aabbcc_28days_1Feb2019.RData")

save(d,fit_global_age_male_slopes, file="fits_aabb_28days_13Octmale.RData")



######lets try reparameterized models based off of vervet paper

#################################
##########slope models###########
#################################
library(rethinking)
library(rstan)
#setwd("~/Dropbox/sloanea/")
#d <- read.csv("~/Dropbox/sloanea/ST_28_aabbrav_9Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi
df <- read.csv("ST_28_aabbccrav_11Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi
d <- df


#monkeys must have at least max_num_bouts observations to be analyzed
max_num_bouts <- 10
d$max_bouts <- 0
for(r in 1:nrow(d)){
  d$max_bouts[r] <- max(d$forg_bout[d$mono_index==d$mono_index[r]])
}

d <- d[d$max_bouts>9,]
d$logage <- log(d$age)
d$logage_s <- (d$logage -mean(d$logage))/sd(d$logage)
d$logagecont <- log(d$agecont)
d$logagecont_s <- (d$logagecont -mean(d$logagecont))/sd(d$logagecont)
d$technique_index <- d$TECH_i
d$technique <- d$TECH
d$sex_index <- d$male + 1 #males 2, female 1
d$mono_index <- as.integer(as.factor(d$mono))

##########individual learning alone############
datalist_i <- list(
  n_obs = nrow(d) ,                                  #length of dataset
  n_id = length( unique(d$mono_index) ) ,       #number of individuals
  n_behav = max(d$technique_index) ,                   #number of processing techniques
  n_group = max(d$grouptoday_i) ,
  tech = d$technique_index,                     #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ,d$y4 , d$y5 , d$y6 ) ,              #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout ,                          #processing bout unique to individual J
  id = d$mono_index ,                      #individual ID
  sex_index=d$sex_index ,
  group_index=d$grouptoday_i ,
  logage = d$logage_s ,
  n_effects=2                               #number of parameters to estimates
)

parlist <- c("G" , "S" , "I", "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" )
fit_i = stan( file = 'ewa_individual_verv_sex.stan', data = datalist_i ,iter = 600, warmup=300, chains=4, cores=4, control=list(adapt_delta=0.95) , pars=parlist, refresh=10 , seed=666)
parlist_2 <- c("G" , "S" , "I", "bA" , "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" )
fit_i2 = stan( file = 'stancode_age_sex/ewa_individual_verv_sex_age.stan', data = datalist_i ,iter = 600, warmup=300, chains=4, cores=4, control=list(adapt_delta=0.95) , pars=parlist_2, refresh=10 , seed=666)

precis(fit_i , depth=3 , pars='sigma_i') 
precis(fit_i , depth=3 , pars='S') 
plot(precis(fit_i , depth=3 , pars='I') )
post <- extract.samples(fit_i)
str(post)
plot(precis(post$I[,,1]) )
apply(post$I[,,1] , 2 , precis)
post$I[,,1]
plambda_male <- list
precis(post$I[,,1] , depth=3)
##############age-bias############

##single strategy social learning
datalist_s <- list(
  n_obs = nrow(d),                                  #length of dataset
  n_id = length( unique(d$mono_index) ),       #number of individuals
  n_behav = max(d$technique_index),                   #number of processing techniques
  n_group = max(d$grouptoday_i),
  tech = d$technique_index,           #technique index
  y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6  ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$freq1 , d$freq2 , d$freq3 , d$freq4 , d$freq5 , d$freq6 ), #observed counts of all K techniques to individual J (frequency-dependence)
  q = cbind(d$pay1 , d$pay2 , d$pay3 ),##############EXAMINE
  bout = d$forg_bout,#bout is forg index  unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  n_effects=4                                                                        #number of parameters to estimates
)

datalist_s$q <- datalist_s$q / max(datalist_s$q)
datalist_s$s <- datalist_s$s/ max(datalist_s$s)


##global model
datalist_g <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$ID_actor_index) ),  #number of individuals
  K = max(d$technique_index),         #number of processing techniques
  tech = d$technique_index,           #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind( d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
  f = cbind( d$fem1 , d$fem2 , d$fem3 ),
  k = cbind( d$kin1 , d$kin2 , d$kin3 ),
  p = cbind( d$pay1 , d$pay2 , d$pay3 ),
  r = cbind( d$rank1 , d$rank2 , d$rank3 ),
  x = cbind( d$sex1 , d$sex2 , d$sex3 ),
  bout = d$forg_bout, #bout is forg index  unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  N_effects=9                                                                        #number of parameters to estimates
)

datalist_g$s <- datalist_g$s/ max(datalist_g$s)
datalist_g$f <- datalist_g$f / max(datalist_g$f)
datalist_g$k <- datalist_g$k/ max(datalist_g$k)
datalist_g$p <- datalist_g$p/ max(datalist_g$p)
datalist_g$r <- datalist_g$r/ max(datalist_g$r)
datalist_g$x <- datalist_g$x/ max(datalist_g$x)

###########################################model fits###########################################
parlist <- c("A" ,"G" , "S" , "I", "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" )

fit_i = stan( file = 'ewa_individual.stan', data = datalist_i ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.95) , pars=parlist, refresh=100)
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.99) , pars=parlist, refresh=100 , init=0)
fit_freq = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.9999 ,  max_treedepth = 15) , pars=c("A" ,"S" , "I", "sigma", "Rho" , "log_lik" ), refresh=100 , init=0)
