R
library(rethinking)
library(rstan)
#d <- read.csv("~/Dropbox/sloanea/ST_28_aabbrav_9Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi

df <- read.csv("ST_28_aabbrav_9Oct2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi
d <- df

# d$s_all <- d$s1 + d$s2 + d$s3 + d$s4 + d$s5 + d$s6

# for(r in 1:nrow(d)){
# 	d$first_obs[r] <- min(d$forg_bout[d$mono_index==d$mono_index[r] & d$s_all > 0])
# }

# d$post_obs <- ifelse(d$forg_bout < d$first_obs , 0 , 1) #assigns a 01if after social info observed for EMA in model social component

# d_long <- d
# d<- d[1:2000,]

#monkeys must have at least 10 observations to be analyzed
d$max_bouts <- 0
for(r in 1:nrow(d)){
	d$max_bouts[r] <- max(d$forg_bout[d$mono_index==d$mono_index[r]])
}

d <- d[d$max_bouts>=10,]
d2 <- d[d$max_bouts>=20,]

# d$obs <- max(d$forgbout[d$mono])
# d$mono[ which.max(d$forg_bout < 10) ] )

# as.data.frame(table(d$mono))
d$mono_index <- (as.integer(as.factor(d$mono)))
#d <- d[1:4000,]


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
age=d$agecont_s,
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
age=d$agecont_s,
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
age=d$agecont_s,
postobs=d$post_obs

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


#scale payoffs/cues by dividing by max value
# d1 <- datalist_s

# d1$yobs <- d1$yobs / max(d1$yobs)
# d1$cohos <- d1$cohos / max(d1$cohos)
# d1$ks <- d1$ks / max(d1$ks)
# d1$ps <- d1$ps / max(d1$ps)
# d1$press <- d1$press / max(d1$press)

# datalist_smig <- list(
# N = nrow(dm),
# J = length( unique(dm$mono_i) ),
# K=max(dm$TECH_i),
# tech = dm$TECH_i,
# y = cbind( dm$y1 , dm$y2 , dm$y3 , dm$y4 , dm$y5 , dm$y6 ),
# s = cbind(dm$s1 , dm$s2 , dm$s3 , dm$s4 ,dm$s5 ,dm$s6  ) ,
# bout = dm$forg_bout,#bout is forg index here
# id = as.integer(as.factor(dm$mono_i)),
# N_effects=3,
# postmig=dm$postmig
# )

# datalist_sfizz <- list(
# N = nrow(d),
# J = length( unique(d$mono_i) ),
# K=max(d$TECH_i),
# tech = d$TECH_i,
# y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
# s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
# bout = d$forg_bout,#bout is forg index here
# id = as.integer(as.factor(d$mono_i)),
# N_effects=3,
# postmig=d$post_fizz
# )

# datalist_ssize <- list(
# N = nrow(d),
# J = length( unique(d$mono_i) ),
# K=max(d$TECH_i),
# tech = d$TECH_i,
# y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
# s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
# bout = d$forg_bout,#bout is forg index here
# id = as.integer(as.factor(d$mono_i)),
# N_effects=3,
# popsize= d$pop_month
# )



# parlistI=c( "lambda","phi_i", "dev" , "log_lik" , "alpha")
# parlistF=c("a_id" , "mu", "lambda", "dev" , "log_lik")
parlistF=c("a_id" , "mu", "lambda", "sigma" , "Rho", "phi_i" , "gamma_i" , "fconf_i" , "dev" , "log_lik")
parlistQ=c("a_id" , "mu", "lambda" , "dev" , "log_lik" , "sigma" , "Rho")
parlistQage=c("a_id" , "mu", "lambda" , "dev" , "log_lik" , "sigma" , "L_Rho" )

fit_s = stan( file = 'ewa_freq_social.stan', data = datalist_s ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistF, control=list(adapt_delta=0.98))
fit_coho = stan( file = 'ewa_cue_social.stan', data = datalist_coho ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_age = stan( file = 'ewa_cue_social.stan', data = datalist_age ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_kin = stan( file = 'ewa_cue_social.stan', data = datalist_kin ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_global = stan( file = 'ewa_global_social.stan', data = datalist_global ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQ, control=list(adapt_delta=0.98))
fit_global_age = stan( file = 'ewa_global_social_age.stan', data = datalist_global ,iter = 1000, warmup=500, chains=2, cores= 2, pars=parlistQage, control=list(adapt_delta=0.98))

fit_s = stan( file = 'ewa_freq_social.stan', data = datalist_s ,iter = 1000, warmup=500, chains=3, cores= 3, pars=parlistF, control=list(adapt_delta=0.99))
fit_coho = stan( file = 'ewa_cue_social.stan', data = datalist_coho ,iter = 1000, warmup=500, chains=3, cores= 3, pars=parlistQ, control=list(adapt_delta=0.99))
fit_age = stan( file = 'ewa_cue_social.stan', data = datalist_age ,iter = 1000, warmup=500, chains=3, cores= 3, pars=parlistQ, control=list(adapt_delta=0.99))
fit_kin = stan( file = 'ewa_cue_social.stan', data = datalist_kin ,iter = 1000, warmup=500, chains=3, cores= 3, pars=parlistQ, control=list(adapt_delta=0.99))
fit_global = stan( file = 'ewa_global_social.stan', data = datalist_global ,iter = 1000, warmup=500, chains=3, cores= 3, pars=parlistQ, control=list(adapt_delta=0.99))
#fit_global_age = stan( file = 'ewa_global_social_age.stan', data = datalist_global ,iter = 1000, warmup=500, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99))
###varying slopes for age
fit_global_age_slopes = stan( file = 'ewa_global_social_age_slopes.stan', data = datalist_global_slopes ,iter = 1200, warmup=700, chains=3, cores= 3, pars=parlistQage, control=list(adapt_delta=0.99))

fit_i = stan(file = 'ewa_i.stan', data = datalist_i ,iter = 2000, warmup=1000, chains=2,  pars=parlistI , control=list(adapt_delta=0.99))
write.csv(extract(fit_s) , "post_s_ST.csv")

pdf(file.pdf,width=18,height=11,paper='special') 
traceplot(fit_s , pars=parlistF , nrow=4 , ncol= 5)
dev.off()

post<- extract(fit_s)
dens(logistic(post$mu.1) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to new experience" , col="white")##phi
abline(v=median(logistic(post$mu.1)) ) 
shade( density(logistic(post$mu.1 )) , lim= as.vector(HPDI(logistic(post$mu.1), prob=0.9999)) , col = col.alpha("forestgreen", 0.25))

dens(logistic(post$mu.2) , main=expression(paste(gamma))  ,  xlim=c(0,1) , xlab= "weight of social information" , col="white") #gamma
abline(v=median(exp(post$mu.2)) ) 
shade( density(exp(post$mu.2 )) , lim= as.vector(HPDI(logistic(post$mu.2), prob=0.9999)) , col = col.alpha("slateblue", 0.25))

dens(exp(post$mu.3)  , main=expression(paste( "\u0192"[c])),  xlim=c(0,3) , xlab="strength of frequency dependence" , col="white")##fconf
abline(v=median(exp(post$mu.3)) ) #fconf
abline(v=1 , lty=2 ) #fconf
shade( density(exp(post$mu.3 )) , lim= as.vector(HPDI(exp(post$mu.3), prob=0.9999)) , col = col.alpha("red", 0.25))

dens(post$lambda , show.HPDI=0.99999 , main=expression(paste(lambda))  ,  xlim=c(0,5) , xlab= "lambda") #gamma
abline(v=mean(post$lambda ) ) 

