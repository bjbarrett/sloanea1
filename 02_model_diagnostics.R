library(posterior)
library(bayesplot)

#fit_i
fit <- fit_i
loo(fit_i$draws("log_lik"))
mcmc_hist(fit$draws("sigma_ID"))
mcmc_hist(fit$draws(c("log_L" , "logit_phi")) , transformations = list(log_L=exp ,logit_phi = logistic))
fit$summary(c("logit_phi"))
fit$summary(c("log_L"))
fit$summary("sigma_ID")
fit$summary("v_ID")
fit$summary("pp")
fit$draws("pp")
trace(fit, pars="sigma_ID")

draws_df <- fit$draws(variables=c("logit_phi" , "log_L" , "v_ID" , "sigma_ID" , "Rho_ID" ,"pp" , "log_lik")  , format = "df")
str(draws_df)
xx <- posterior::as_draws_list(draws_df)
stanfit <- rstan::read_stan_csv(fit$output_files())
post_i <- extract(stanfit)
precis(stanfit , pars=c("Rho_ID") , depth=3)
str(post_i)
str(stanfit)
hist(post_i$pp[,11])

#fit_i_sex

loo(fit_i_sex$draws("log_lik"))
mcmc_hist(fit_i_sex$draws("sigma_ID"))
mcmc_hist(fit_i_sex$draws(c("log_L" , "logit_phi")) , transformations = list(log_L=exp ,logit_phi = logistic))
fit_i_sex$summary(c("logit_phi"))
fit_i_sex$summary(c("log_L"))
fit$summary("sigma_ID")
fit$summary("v_ID")
fit$summary("pp")
fit$draws("pp")
trace(fit, pars="sigma_ID")

##fit f
loo(fit_f$draws("log_lik"))
fit_f$summary(c("logit_phi"))
fit_f$summary(c("logit_gam"))
fit_f$summary(c("log_L"))
fit_f$summary(c("log_f"))
fit_f$summary("sigma_ID")
fit_f$summary("v_ID")

pred_f <- posterior_predict(fit_f)
##fit coho
loo(fit_coho$draws("log_lik"))
fit_coho$summary(c("logit_phi"))
fit_coho$summary(c("logit_gam"))
fit_coho$summary(c("log_L"))
fit_coho$summary(c("gauss_beta"))
fit_coho$summary("sigma_ID")
fit_coho$summary("v_ID")

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
