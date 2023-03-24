library(posterior)
library(bayesplot)
fit_i<- readRDS("~/sloanea1/fit_i_cmd.rds")
fit_age <- readRDS("~/sloanea1/fit_age_cmd.rds")
fit_i_age<- readRDS("~/sloanea1/fit_i_age_cmd.rds")
fit_i_sex<- readRDS("~/sloanea1/fit_i_sex_cmd.rds")
fit_f<- readRDS("~/sloanea1/fit_f_cmd.rds")
fit_coho_sex <- readRDS("~/sloanea1/fit_coho_sex_cmd.rds")
fit_coho <- readRDS("~/sloanea1/fit_coho_cmd.rds")

#fit_i
fit <- fit_i
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi")) , transformations = list(log_L=exp ,logit_phi = logistic))
fit$summary(c("log_L","logit_phi"))
fit$summary("v_ID")
fit$summary("Rho_ID")
fit$draws("pp")

draws_df <- fit$draws(variables=c("log_L", "logit_phi", "v_ID" , "sigma_ID" , "Rho_ID" ,"pp" , "log_lik")  , format = "df")
str(draws_df)
##plot thyme
LambdaDensPlot(draws_df , col="violet")
PhiDensPlot(draws_df , col="blue")
    

# stanfit <- rstan::read_stan_csv(fit$output_files())
# post_i <- extract(stanfit)
# precis(stanfit , pars=c("Rho_ID") , depth=3)

#fit_i_sex
fit <- fit_i_sex
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi")))
fit$summary(c("log_L","logit_phi"))
fit$summary("v_ID")
fit$summary("Rho_ID")

#fit_i_age
fit <- fit_i_age
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi","b_age")))
fit$summary(c("log_L","logit_phi","b_age"))
fit$summary("v_ID")
draws_df <- fit$draws(variables=c("b_age" ,"log_L", "logit_phi", "v_ID" , "sigma_ID" , "Rho_ID" ,"pp" , "log_lik")  , format = "df")
LambdaDensPlot(draws_df , color="violet")
PhiDensPlot(draws_df , color="blue")

##fit f
fit <- fit_f
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi" , "logit_gam" , "log_f")))
fit$summary(c("log_L" , "logit_phi" , "logit_gam" , "log_f"))
fit$summary("v_ID")
fit$summary("Rho_ID")
draws_df <- fit$draws(variables=c("log_L", "logit_phi", "logit_gam" ,"log_f" , "v_ID" , "sigma_ID" , "Rho_ID" ,"pp" , "log_lik")  , format = "df")
LambdaDensPlot(draws_df)
PhiDensPlot(draws_df )
GammaDensPlot(draws_df)
FrDensPlot(draws_df)


##fit coho
fit <- fit_coho
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta")))
fit$summary(c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta" ,"sigma_ID"))
fit$summary("v_ID")
fit$summary("Rho_ID")
draws_df <- fit$draws(variables=c("log_L", "logit_phi", "logit_gam" ,"gauss_beta" , "v_ID" , "sigma_ID" , "Rho_ID" ,"pp" , "log_lik")  , format = "df")
LambdaDensPlot(draws_df)
PhiDensPlot(draws_df )
GammaDensPlot(draws_df)
BetaDensPlot(draws_df)

###fit_coho_sex
fit <- fit_coho_sex
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta")))
fit$summary(c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta"))
fit$summary("v_ID")
fit$summary("Rho_ID")

stanfit_coho_sex <- rstan::read_stan_csv(fit$output_files())
post_coho_sex <- extract(stanfit_coho_sex)
WAIC(stanfit_coho_sex)
plot(precis(stanfit_coho_sex , pars=c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta") , depth=2))
x <- precis(stanfit_coho_sex , pars="v_ID",depth=3)
(x[seq(from=4 , to=nrow(x) , by=4),])
(x[seq(from=3 , to=nrow(x) , by=4),])

library(reshape2)
melt(m)
precis(post_coho_sex$v_ID[,,3])
##fit age
fit <- fit_age
loo(fit$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta")))
fit$summary(c("log_L" , "logit_phi" , "logit_gam" , "gauss_beta" ,"sigma_ID"))
fit$summary("Rho_ID")

fit$summary("v_ID")

# library('rethinking')
# load("~/Documents/sloanea1/17032021_il_coho_kin_sa_sloanea.rdata")
# load("~/Documents/sloanea1/17032021_freq_age_sa_sloanea.rdata")
# 
# ls()
# 
# #examine IL model
# post <- extract(fit_i_sa)
# model <- fit_i_sa
# precis(model , depth=3 , pars='mu' )
# 
# precis(model , depth=3 , pars='bA' )
# precis(model , depth=3 , pars='sigma_i' )
# plot(precis(model , depth=3 , pars='phi' ))
# plot(precis(model , depth=3 , pars='phi_i' ))
# 
# plot(precis(model , depth=3 , pars='lambda' ))
# post <- extract(model)
# str(post)
# 
# mean(exp(post$S[,1,1])) # female lambda
# mean(exp(post$S[,2,1])) # male lambda
# apply(post$lambda, 2 , mean) #mean lambda for all individuals
# mean(apply(post$lambda, 2 , mean)) #mean lambda of all individuals , matches well to lambda gq
# dens(exp(post$S[,1,1]) , col="red")
# dens(exp(post$S[,2,1]) , col="blue" , add=TRUE)
# 
# mean(exp(post$S[,2,1])) # male lambda)
# mean(logistic(post$S[,1,2])) #female phi
# mean(logistic(post$S[,2,2])) #male phi
# apply(post$phi, 2 , mean) #mean phi for all individuals
# mean(apply(post$phi, 2 , mean)) #mean phi of all individuals , matches well to phi gq
# dens(logistic(post$S[,1,2]) , col="red")
# dens(logistic(post$S[,2,2]) , col="blue" , add=TRUE)
# precis(logistic(post$I[,,2]))
# apply(logistic(post$I[,,2] ), 2 , mean)#mean phi of all individuals , matches well to phi gq
# 
# col_pal_sex <- c("red" , "blue")
# #lets try a plot of phi and lambda over age
# lambda_obs_t <- rep(0, nrow(d))
# phi_obs_t <- rep(0, nrow(d))
# for(i in 1:nrow(d)){
#   lambda_obs_t[i] <- mean(exp( post$I[,d$mono_index[i],1] + post$G[,d$grouptoday_i[i],1] + post$S[,d$sex_index[i] , 1] + post$I[,d$mono_index[i] , 3]*d$logage_s[i] ))
#   phi_obs_t[i] <- mean(logistic( post$I[,d$mono_index[i],2] + post$G[,d$grouptoday_i[i],2] + post$S[,d$sex_index[i] , 2] + post$I[,d$mono_index[i] , 4]*d$logage_s[i] ))
# }
# plot( d$logage_s , lambda_obs_t )
# plot( d$logage_s , phi_obs_t )
# 
# #weird estimates of phi at each timestep
# plot(d$mono_i~d$logage_s , ylab="weight given to new experience (\u03D5)" , xlab="log(age) (standardized)" , pch=19 , col="white" , ylim=c(0,1), xlim=c(min(d$logagecont_s),max(d$logage_s)) , cex=1.5 , cex.lab=1.5)
# for(i in 1:max(d$mono_index)){	
#   obs <- which(d$mono_index==i)
#   lines( d$logage_s[obs] , phi_obs_t[obs] )
# }
# 
# ##estimates along sequence
# plot(d$mono_i~d$logage_s , ylab="weight given to new experience (\u03D5)" , xlab="log(age) (standardized)" , pch=19 , col="white" , ylim=c(0,1), xlim=c(min(d$logage_s),max(d$logage_s)) , cex=1.5 , cex.lab=1.5)
# post_temp <- apply(post$S[,,2] , 1 , mean) + apply(post$G[,,2] , 1 , mean)
# for(i in 1:max(d$mono_index)){
#   age_seq <- seq(from=min(d$logage_s[d$mono_index==i]) , to=max(d$logage_s[d$mono_index==i]) , length=25 )
#     pee_med <- sapply(age_seq , function(z)
#     mean(logistic( post$I[,i,2] + post_temp + post$I[,i, 4]*z ))
#     )
#   lines(age_seq , pee_med , lw=2 , col=col.alpha("red", 0.25))
# }
# 
# age_seq <- seq(from=min(d$logage_s) , to=max(d$logage_s) , length=25 )
# 
# pee_ci<- sapply(age_seq , function(z)
#   HPDI(logistic( apply(post$I[,,2] , 1 , mean) + apply(post$S[,,2] , 1 , mean) + apply(post$G[,,2] , 1 , mean) + post$I[,i, 4]*z ))
#  )
# 
# pee_mean <- sapply(age_seq , function(z)
#   mean(logistic( apply(post$I[,,2] , 1 , mean) + apply(post$S[,,2] , 1 , mean) + apply(post$G[,,2] , 1 , mean) + post$I[,i, 4]*z ))
# )
# lines(age_seq , pee_mean , lw=2)
# #shade(age_seq , pee_ci)
# 
# PlotAgeParams <- function(model , linkfcn , param_index1 , param_index2 , yaxlab){
#   post <- extract(model)
#   plot(d$mono_i~d$logage_s , ylab=yaxlab , xlab="log(age) (standardized)" , pch=19 , col="white" , ylim=c(0,20), xlim=c(min(d$logage_s),max(d$logage_s)) , cex=1.5 , cex.lab=1.5)
#   post_temp <- apply(post$S[,,param_index1] , 1 , mean) + apply(post$G[,,param_index1] , 1 , mean)
#   for(i in 1:max(d$mono_index)){
#     age_seq <- seq(from=min(d$logage_s[d$mono_index==i]) , to=max(d$logage_s[d$mono_index==i]) , length=25 )
#     pee_med <- sapply(age_seq , function(z)
#       median(linkfcn( post$I[,i,param_index1] + post_temp + post$I[,i, param_index2]*z ))
#     )
#     lines(age_seq , pee_med , lw=2 , col=col.alpha("red", 0.25))
#   }
#   age_seq2 <- seq(from=min(d$logage_s) , to=max(d$logage_s) , length=25 )
#   # pee_ci<- sapply(age_seq , function(z)
#   #   HPDI(logistic( apply(post$I[,,2] , 1 , mean) + apply(post$S[,,2] , 1 , mean) + apply(post$G[,,2] , 1 , mean) + post$I[,i, 4]*z ))
#   # )
#   
#   pee_mean <- sapply(age_seq2 , function(z)
#     median(linkfcn( apply(post$I[,,param_index1] , 1 , mean) + apply(post$S[,,param_index1] , 1 , mean) + apply(post$G[,,param_index1] , 1 , mean) + post$I[,i, param_index2]*z ))
#   )
#   lines(age_seq2 , pee_mean , lw=2)
# }
# 
# PlotAgeParams(model=fit_i_sa, linkfcn = logistic , param_index1 = 2 , param_index2 = 4 , yaxlab = "weight given to new experience (\u03D5)" )
# PlotAgeParams(model=fit_i_sa, linkfcn = exp , param_index1 = 1 , param_index2 = 3 , yaxlab = "sensitivity to differences in payoffs")
# 
# ###freq
# PlotAgeParams(model=fit_age_sa, linkfcn = logistic , param_index1 = 3 , param_index2 = 7 , yaxlab = "weight given to social information" )
# PlotAgeParams(model=fit_kin_sa, linkfcn = logistic , param_index1 = 3 , param_index2 = 7 , yaxlab = "weight given to social information" )
# PlotAgeParams(model=fit_freq_sa, linkfcn = logistic , param_index1 = 3 , param_index2 = 7 , yaxlab = "weight given to social information" )
# PlotAgeParams(model=fit_coho_sa, linkfcn = logistic , param_index1 = 3 , param_index2 = 7 , yaxlab = "weight given to social information" )
# PlotAgeParams(model=fit_freq_sa, linkfcn = exp , param_index1 = 4 , param_index2 = 8 , yaxlab = "strenght freq dep" )
# PlotAgeParams(model=fit_coho_sa, linkfcn = identity  , param_index1 = 4 , param_index2 = 8 , yaxlab = "strenght cohort bias" )
# PlotAgeParams(model=fit_age_sa, linkfcn = identity  , param_index1 = 4 , param_index2 = 8 , yaxlab = "strenght cohort bias" )
# PlotAgeParams(model=fit_kin_sa, linkfcn = identity  , param_index1 = 4 , param_index2 = 8 , yaxlab = "strenght cohort bias" )
# 
# post <- extract(fit_freq_sa)
# apply(post$phi , 2 , mean)
# apply(post$gamma , 2 , mean)
# apply(post$fc , 2 , mean)
# plot(precis(fit_coho_sa , pars="beta" , depth=3))
# plot(precis(fit_age_sa , pars="beta" , depth=3))
# 
# post <- extract(fit_freq_sa)
# str(post)
# prs <- post$PrPreds
# str(prs)
# xx <- apply(prs[,,1] , 2 , mean)
# str(xx)
# d$Pr1 <- apply(prs[,,1] , 2 , mean)
# d$Pr2 <- apply(prs[,,2] , 2 , mean)
# d$Pr3 <- apply(prs[,,3] , 2 , mean)
# d$Pr4 <- apply(prs[,,4] , 2 , mean)
# d$Pr5 <- apply(prs[,,5] , 2 , mean)
# d$Pr6 <- apply(prs[,,6] , 2 , mean)
# plot(Pr1 ~ dates.julian , data=d[d$grouptoday=="fl",] , col="red" , ylim=c(0,1))
# points(Pr2 ~ dates.julian , data=d[d$grouptoday=="fl",] , col="orange")
# points(Pr3 ~ dates.julian , data=d[d$grouptoday=="fl",] , col="yellow")
# points(Pr4 ~ dates.julian , data=d[d$grouptoday=="fl",] , col="green")
# points(Pr5 ~ dates.julian , data=d[d$grouptoday=="fl",] , col="blue")
# points(Pr6 ~ dates.julian , data=d[d$grouptoday=="fl",] , col="slateblue")
# 
# 
# precis(stanfit , depth=2, pars='sigma_i')
# precis(stanfit , depth=2, pars=c('Sl' , 'Sp' , 'Sg' , 'Sf'))
# 
# precis(stanfit , depth=3, pars=c('I'))
