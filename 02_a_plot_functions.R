invlogit <- function(x){
  1 / (1 + exp(-x))
}#setting up a function that will help us generate data

LambdaDensPlot <- function( x , xlim=c(0,22) , color="yellow3"){
  col_numz <- which(colnames(x)=="v_ID[1,1]")
  temp <- x[,col_numz: (col_numz + max(d$mono_index) - 1)]
  temp2 <- exp(temp + x$log_L)
  #plot(precis(temp2, depth=3 ))
  dens( exp(x$log_L) , xlim=c(0,22) , show.HPDI=.89 , adj=0.9 , xlab="lambda")
  abline(v=median(exp(x$log_L)) , lw=2 , col=color) #line at true value
  seq_l <- seq(from=0 , to=0.9*max(density(exp(x$log_L))$y) , length=max(d$mono_index)) #vertical range to plot hpdi segs
  f_med <- apply(temp2 , 2 ,  median) #actual medians
  f_hpdi <- apply(temp2 , 2 , HPDI, prob=0.89) #actual medians
  for(i in 1:max(d$mono_index)){
    segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
              x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("black" , alpha=.7) , lty=1 , lw=.5)
  }
  points(f_med[order(f_med)] , seq_l , cex=0.25 ,  col=color, pch=5)
}

PhiDensPlot <- function( x , xlim=c(0,1) , color="purple"){
  col_numz <- which(colnames(x)=="v_ID[1,2]")
  temp <- x[,col_numz: (col_numz + max(d$mono_index) - 1)]
  temp2 <- invlogit(temp + x$logit_phi)
  dp <- dens(invlogit(x$logit_phi) , xlim=c(0,1) , show.HPDI=.89 , adj=0.9 , xlab="phi")
  abline(v=median(invlogit(x$logit_phi)) , lw=2 , col=color) #line at true value
  seq_l <- seq(from=0 , to=0.9*max(density(inv_logit(x$logit_phi))$y) , length=max(d$mono_index)) #vertical range to plot hpdi segs
  f_med <- apply(temp2 , 2 ,  median) #actual medians
  f_hpdi <- apply(temp2 , 2 , HPDI, prob=0.89) #actual medians
  for(i in 1:max(d$mono_index)){
    segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
              x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("black" , alpha=.7) , lty=1 , lw=.5)
  }
  points(f_med[order(f_med)] , seq_l , cex=0.25 ,  col=color, pch=5)
}

GammaDensPlot <- function( x , xlim=c(0,1) , color="green"){
  col_numz <- which(colnames(x)=="v_ID[1,3]")
  temp <- x[,col_numz: (col_numz + max(d$mono_index) - 1)]
  temp2 <- invlogit(temp + x$logit_gam)
  dp <- dens(invlogit(x$logit_gam) , xlim=c(0,1) , show.HPDI=.89 , adj=0.9 , xlab="phi")
  abline(v=median(invlogit(x$logit_gam)) , lw=2 , col=color) #line at true value
  seq_l <- seq(from=0 , to=0.9*max(density(inv_logit(x$logit_gam))$y) , length=max(d$mono_index)) #vertical range to plot hpdi segs
  f_med <- apply(temp2 , 2 ,  median) #actual medians
  f_hpdi <- apply(temp2 , 2 , HPDI, prob=0.89) #actual medians
  for(i in 1:max(d$mono_index)){
    segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
              x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("black" , alpha=.7) , lty=1 , lw=.5)
  }
  points(f_med[order(f_med)] , seq_l , cex=0.25 ,  col=color, pch=5)
}

FrDensPlot <- function( x , xlim=c(0,8) , color="red4"){
  col_numz <- which(colnames(x)=="v_ID[1,4]")
  temp <- x[,col_numz: (col_numz + max(d$mono_index) - 1)]
  temp2 <- exp(temp + x$log_f)
  #plot(precis(temp2, depth=3 ))
  dens(exp(x$log_f) , xlim=c(0,6), show.HPDI=.89 , adj=0.9 , xlab="lambda")
  abline(v=median(exp(x$log_f)) , lw=2 , col=color) #line at true value
  seq_l <- seq(from=0 , to=0.9*max(density(exp(x$log_f))$y) , length=max(d$mono_index)) #vertical range to plot hpdi segs
  f_med <- apply(temp2 , 2 ,  median) #actual medians
  f_hpdi <- apply(temp2 , 2 , HPDI, prob=0.89) #actual medians
  for(i in 1:max(d$mono_index)){
    segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
              x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("black" , alpha=.7) , lty=1 , lw=.5)
  }
  points(f_med[order(f_med)] , seq_l , cex=0.25 ,  col=color, pch=5)
}


BetaDensPlot <- function( x , xlim=c(0,20) , color="darkgreen"){
  col_numz <- which(colnames(x)=="v_ID[1,4]")
  temp <- x[,col_numz: (col_numz + max(d$mono_index) - 1)]
  temp2 <- temp + x$gauss_beta
  dp <- dens(x$gauss_beta , xlim=c(0,20) , show.HPDI=.89 , adj=0.9 , xlab="phi")
  abline(v=median(x$gauss_beta) , lw=2 , col=color) #line at true value
  seq_l <- seq(from=0 , to=0.9*max(density(x$gauss_beta)$y) , length=max(d$mono_index)) #vertical range to plot hpdi segs
  f_med <- apply(temp2 , 2 ,  median) #actual medians
  f_hpdi <- apply(temp2 , 2 , HPDI, prob=0.89) #actual medians
  for(i in 1:max(d$mono_index)){
    segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
              x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("black" , alpha=.7) , lty=1 , lw=.5)
  }
  points(f_med[order(f_med)] , seq_l , cex=0.25 ,  col=color, pch=5)
}