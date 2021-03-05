R
library(rethinking)
library(rstan)
library(chron)
library(lubridate)
library(beepr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("~/Dropbox/sloanea")

d <- read.csv("~/Dropbox/sloanea/ST2003to2012_Oct6_2018.csv", na.strings = "" , stringsAsFactors=FALSE) #NA stuff helps with naomi
demo <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/Demography Data/Demography-dates.csv" , na.strings = "" , stringsAsFactors=FALSE)
##get demo dates functional with lubridate
demo$dob <- mdy(as.factor(demo$dob))
d$date <- dmy(as.factor(d$date))  #excel might fuck this up if you open csv
d$grouptoday <- gsub("M:BE-JN-TI-TR,aa" , "aa"  , d$grouptoday)
d$grouptoday <- ifelse(d$mono=="TR" , d$grouptoday=="aa" , d$grouptoday )
d$grouptoday_i <- as.integer(as.factor(d$grouptoday))
d <- d[!is.na(d$mono),]
dna <- d[is.na(d$mono),]

as.numeric(years(demo$dob[5:8]-demo$dob[1:4]))/(100*60*60*24*365)
##define age by day function
# agedays <- function(x,y){
# 	as.numeric(days(x-y))/(60*60*24)
# }
# agesim <- function(x,y){
# 	1-as.numeric(days(x-y)) / (60*60*24)
# }

ageyears <- function(x,y){
	(interval(x,y)/days(1))*(1/365)
}

agecoho <- function(x,y){
	1/ (1 + abs( (interval(x,y)/days(1))*(1/365) ))
}

#(interval(demo$dob[5],demo$dob[1])/days(1))*(1/365)
1/(1 -1+(1/365))
##extract unique monkeys from dataset
d$fav <- as.character(d$fav)
d$rav <- as.character(d$rav)
d$AA <- as.character(d$AA)
d$BB <- as.character(d$BB)

d$fav <- ifelse(d$fav== "YADT" , "YA,DT", d$fav)#in data creation now, can remove if dataset is recreated
d$rav <- ifelse(d$rav== "MB CI" , "MB,CI", d$rav)#in data creation now, can remove if dataset is recreated
focals <- as.integer(as.factor(as.vector(sort(unique(d$mono)))))
foc <- data.frame("mono" = as.vector(sort(unique(d$mono))), "mono_index" = as.integer(as.factor(as.vector(sort(unique(d$mono))))))
foc[,1] <- as.character(foc[,1])
#rav <- unique(unlist(strsplit(d$rav, ","), use.names = FALSE)) #https://stackoverflow.com/questions/3879522/finding-unique-values-from-a-list
#fav <- unique(unlist(strsplit(d$fav, ","), use.names = FALSE))
# all_monos <- unique(unlist(list(rav,focals))) #makes a list that gets un;listed with unique vaues extracted
# sort(unique(all_monos))
# sort(unique(rav))
focals
# d$mono[all_monos %in% focals==TRUE]

# all_monos %in% d$mono
# d$mono %in% all_monos

d$mono_index <- as.integer(as.factor(d$mono))
length(unique(d$mono_index))
#d[d$rav=='UK',]
 
o_freq <- array(0,dim=c(nrow(d),length(unique(d$mono_index)),6 ))
o_age <- o_coho <- o_kin <- array(NA,dim=c(nrow(d),length(unique(d$mono_index)),6 ))

#double check code
for( nobs in 1:nrow(d) ){
    for (i in 1:nrow(foc)){
        if ( grepl(foc[i,1],d[nobs,"rav"])==TRUE || grepl(foc[i,1],d[nobs,"AA"])==TRUE || grepl(foc[i,1],d[nobs,"BB"])==TRUE){
        	o_freq[nobs,i,] <- 0 #assigns a 0 to all options if individual i is observing foraging bout nobs
        	o_freq[nobs,i,d$TECH_i[nobs]] <- 1 #assigns a 1 observed option for individual i is observing foraging bout nobs
        	# o_age[nobs,i,] <- 0 #assigns age of forager to social cue
        	o_age[nobs,i,d$TECH_i[nobs]] <- ageyears(demo$dob[demo$mono==d$mono[nobs]] , d$date[nobs]) #assigns a 1 observed option for individual i is observing foraging bout nobs
            # o_coho[nobs,i,] <- 0 #assigns age of forager to social cue
        	o_coho[nobs,i,d$TECH_i[nobs]] <- agecoho(demo$dob[demo$mono==d$mono[nobs]] , demo$dob[demo$mono==foc[i,1]]) #assigns a 1 observed option for individual i is observing foraging bout nobs
	        if ( identical( demo$mom[demo$mono==d$mono[nobs]] , demo$mom[demo$mono==foc[i,1]] ) | identical(  demo$mom[demo$mono==foc[i,1]] , d$mono[nobs] ) ){
	        	o_kin[nobs,i,] <- 0 
	        	o_kin[nobs,i,d$TECH_i[nobs]] <- 1 
	        	#commented out zeros just takes mean of all observed values at each option, will plug in zeros at end, 
	        	#this helps so a rare behavior made by an individual with a certain cue does not loose value becaue 0 go into average
        	}
        }	
    }
}

beep(3)

# o_age[,173,2] ##RO
# o_age[,173,6] ##RO
# o_age[,65,2] ##EO
# o_age[,190,2] ##Sr
# o_age[,197,2] ##TD

# identical (demo$mom[demo$mono=="LT"] , demo$mom[demo$mono=="LN"])
dr <- d
#give each fruit a unique id in temporal order
d$fruit_id <- 0
for (i in 1:nrow(d) ){ d$fruit_id[i] <- i} 

#create foraging bout for each time an individual updates personal info
d$forg_bout <- rep(0,nrow(d))
ff <- rep(0,length(unique(d$mono)))
for (r in 1:nrow(d)) {
    for(i in 1:length(unique(d$mono))) {
        if( d[r,"mono_index"]==i){
            ff[i] <-ff[i] + 1
            d$forg_bout[r] <- ff[i]
        }
    }
}
beep(5)

#order data frame
df <- d
d <- d[order(d$fruit_id),]

d$date.chron <- chron(dates=as.character(d$date),format=c(dates="y-m-d") )
d$dates.julian <- 1 + as.integer(d$date.chron-min(d$date.chron))
win_width <- 28 #social info memory window in days


###frequency dependent learning
d$s1 <- d$s2 <- d$s3 <- d$s4 <- d$s5 <- d$s6 <- 66 #set up colums for frequency dependene where social info is sum between timesteps
###age-biased learning
d$a1 <- d$a2 <- d$a3 <- d$a4 <- d$a5 <- d$a6 <- 66 #set up colums for frequency dependene where social info is sum between timesteps
#cohort bias
d$c1 <- d$c2 <- d$c3 <- d$c4 <- d$c5 <- d$c6 <- 66
###kin biases
d$k1 <- d$k2 <- d$k3 <- d$k4 <- d$k5 <- d$k6 <- 66 

for (nobs in 1:nrow(d)){
	 zz<-min(d$fruit_id[d$dates.julian >= d$dates.julian[nobs]-win_width])
	 d$s1[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] ) 
     d$s2[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] ) 
	 d$s3[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] ) 
	 d$s4[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] ) 
	 d$s5[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] ) 
	 d$s6[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] )
	d$a1[nobs] <- mean( o_age[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE)
	d$a2[nobs] <- mean( o_age[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) 
	d$a3[nobs] <- mean( o_age[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE) 
    d$a4[nobs] <- mean( o_age[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) 
    d$a5[nobs] <- mean( o_age[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) 
    d$a6[nobs] <- mean( o_age[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE)
    d$c1[nobs] <- mean( o_coho[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE)
	d$c2[nobs] <- mean( o_coho[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) 
	d$c3[nobs] <- mean( o_coho[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE) 
    d$c4[nobs] <- mean( o_coho[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) 
    d$c5[nobs] <- mean( o_coho[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) 
    d$c6[nobs] <- mean( o_coho[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE)
    d$k1[nobs] <- mean( o_kin[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE)
	d$k2[nobs] <- mean( o_kin[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) 
	d$k3[nobs] <- mean( o_kin[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE) 
    d$k4[nobs] <- mean( o_kin[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) 
    d$k5[nobs] <- mean( o_kin[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) 
    d$k6[nobs] <- mean( o_kin[ zz:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE)    
	}



# for (nobs in 1:nrow(d)){
# 	d$s1[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 sum( o_freq[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] ) ,
# 	 sum( o_freq[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] ) 
# 	)

# 	d$s2[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 sum( o_freq[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] ) ,
# 	 sum( o_freq[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] ) 
# 	)

# 	d$s3[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 sum( o_freq[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] ) ,
# 	 sum( o_freq[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] ) 
# 	)

# 	d$s4[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 sum( o_freq[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] ) ,
# 	 sum( o_freq[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] ) 
# 	)

# 	d$s5[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 sum( o_freq[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] ) ,
# 	 sum( o_freq[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] ) 
# 	)

# 	d$s6[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 sum( o_freq[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] ) ,
# 	 sum( o_freq[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] ) 
# 	)

# }


# ###age-biased learning
# d$a1 <- d$a2 <- d$a3 <- d$a4 <- d$a5 <- d$a6 <- 66 #set up colums for frequency dependene where social info is sum between timesteps

# for (nobs in 2:nrow(d)){
# 	 zz <- min( d$fruit_id[d$dates.julian >= (d$dates.julian[nobs]-win_width)] )
# 	 d$s1[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] ) ,
#      d$s2[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] ) ,
# 	 d$s3[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] ) ,
# 	 d$s4[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] ) ,
# 	 d$s5[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] ) ,
# 	 d$s6[nobs] <- sum( o_freq[ zz : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] ) 
# 	}


# for (nobs in 1:nrow(d)){
# 	d$a1[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_age[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE) ,
# 	 mean( o_age[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE) 
# 	)

# 	d$a2[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_age[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) ,
# 	 mean( o_age[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) 
# 	)

# 	d$a3[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_age[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE ) ,
# 	 mean( o_age[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE) 
# 	)

# 	d$a4[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_age[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) ,
# 	 mean( o_age[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) 
# 	)

# 	d$a5[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_age[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) ,
# 	 mean( o_age[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) 
# 	)

# 	d$a6[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_age[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE) ,
# 	 mean( o_age[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE) 
# 	)

# }
# beep(1)

# d$c1 <- d$c2 <- d$c3 <- d$c4 <- d$c5 <- d$c6 <- 66 #set up colums for frequency dependene where social info is sum between timesteps


# for (nobs in 1:nrow(d)){
# 	d$c1[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_coho[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE) ,
# 	 mean( o_coho[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE) 
# 	)

# 	d$c2[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_coho[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) ,
# 	 mean( o_coho[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) 
# 	)

# 	d$c3[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_coho[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE ) ,
# 	 mean( o_coho[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE) 
# 	)

# 	d$c4[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_coho[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) ,
# 	 mean( o_coho[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) 
# 	)

# 	d$c5[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_coho[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) ,
# 	 mean( o_coho[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) 
# 	)

# 	d$c6[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_coho[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE) ,
# 	 mean( o_coho[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE) 
# 	)

# }
# beep(1)


# d$k1 <- d$k2 <- d$k3 <- d$k4 <- d$k5 <- d$k6 <- 66 #set up colums for frequency dependene where social info is sum between timesteps

# for (nobs in 1:nrow(d)){
# 	d$k1[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_kin[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE) ,
# 	 mean( o_kin[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 1 ] , na.rm = TRUE) 
# 	)

# 	d$k2[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_kin[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) ,
# 	 mean( o_kin[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 2 ] , na.rm = TRUE) 
# 	)

# 	d$k3[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_kin[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE ) ,
# 	 mean( o_kin[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 3 ] , na.rm = TRUE) 
# 	)

# 	d$k4[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_kin[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) ,
# 	 mean( o_kin[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 4 ] , na.rm = TRUE) 
# 	)

# 	d$k5[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_kin[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) ,
# 	 mean( o_kin[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 5 ] , na.rm = TRUE) 
# 	)

# 	d$k6[nobs] <- ifelse( d$forg_bout[nobs] > 1 ,
# 	 mean( o_kin[ (d$fruit_id[ nobs - 1 ]) : (d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE) ,
# 	 mean( o_kin[ 1:(d$fruit_id[nobs] - 1) , d$mono_index[nobs] , 6 ] , na.rm = TRUE) 
# 	)

# }
# beep(1)


d$a6[is.nan(d$a6)] <- 0 #gets rid on nans
d$a5[is.nan(d$a5)] <- 0
d$a4[is.nan(d$a4)] <- 0
d$a3[is.nan(d$a3)] <- 0
d$a2[is.nan(d$a2)] <- 0
d$a1[is.nan(d$a1)] <- 0
d$k6[is.nan(d$k6)] <- 0 #gets rid on nans
d$k5[is.nan(d$k5)] <- 0
d$k4[is.nan(d$k4)] <- 0
d$k3[is.nan(d$k3)] <- 0
d$k2[is.nan(d$k2)] <- 0
d$k1[is.nan(d$k1)] <- 0
d$c6[is.nan(d$c6)] <- 0 #gets rid on nans
d$c5[is.nan(d$c5)] <- 0
d$c4[is.nan(d$c4)] <- 0
d$c3[is.nan(d$c3)] <- 0
d$c2[is.nan(d$c2)] <- 0
d$c1[is.nan(d$c1)] <- 0

dd <- drop.levels(d)


# as.vector(unique(strsplit(fav, ",", fixed = TRUE)))
d$year <- as.integer(as.factor(year(d$date)))
d$yeartrue <- year(d$date)
d$age <- d$yeartrue-d$yob
d$agecont <- 
d$y2012 <- ifelse(year(d$date)==2012, 1, 0)
d$y2011 <- ifelse(year(d$date)==2011, 1, 0)
d$y2010 <- ifelse(year(d$date)==2010, 1, 0)
d$y2009 <- ifelse(year(d$date)==2009, 1, 0)
d$y2008 <- ifelse(year(d$date)==2008, 1, 0)
d$y2007 <- ifelse(year(d$date)==2007, 1, 0)
d$y2006 <- ifelse(year(d$date)==2006, 1, 0)
d$y2005 <- ifelse(year(d$date)==2005, 1, 0)
d$y2004 <- ifelse(year(d$date)==2004, 1, 0)
d$y2003 <- ifelse(year(d$date)==2003, 1, 0)

d$y1 <- ifelse(d$TECH_i==1 , 1, 0)
d$y2 <- ifelse(d$TECH_i==2 , 1, 0)
d$y3 <- ifelse(d$TECH_i==3 , 1, 0)
d$y4 <- ifelse(d$TECH_i==4 , 1, 0)
d$y5 <- ifelse(d$TECH_i==5 , 1, 0)
d$y6 <- ifelse(d$TECH_i==6 , 1, 0)


mono_index <- data.frame(as.vector(unique(d$mono)), as.vector(unique(d$mono_index)))
#setNames(mono_index, c("mono", "mono_i"))

str(d)

d$pop_month <- 0
for (i in 1:nrow(d)){
	d$pop_month[i] <-length(unique(d$mono[d$year==d$year[i] & d$grouptoday==d$grouptoday[i]]))
}
d$pop_month_s <- d$pop_month - mean(d$pop_month)/sd(d$pop_month)
d$age_s <- d$age - mean(d$age)/sd(d$age)
d$dob <- as.Date(d$dob,format='%m/%d/%y')
year(d$dob) <- ifelse( year(d$dob)==2067 , 1967, year(d$dob))

d$agecont <-ageyears( d$dob , d$date)
d$agecont_s <- (d$agecont - mean(d$agecont))/sd(d$agecont)

###this orders in monkey order
d <- d[order(d$mono_index,d$timedate),]

d<- write.csv(d, "ST_28_aabbrav_9Oct2018.csv")
###migration dataset
dm<- subset(d,male==1)
migmales <- unique(dm$mono[dm$postmig==1])
dm <- dm[!is.na(dm$mono),]
dm$mono_i <- as.integer(as.factor(dm$mono_i))
dm <- dm[dm$mono %in% migmales,]

datalist_i <- list(
N = nrow(d),
J = length( unique(d$mono_i) ),
K=max(d$TECH_i),
tech = d$TECH_i,
y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
bout = d$forg_bout,#bout is forg index here
id = as.integer(as.factor(d$mono_i)),
N_effects=1,
year=d$year,
age=d$agecont_s

)

datalist_s <- list(
N = nrow(d),
J = length( unique(d$mono_i) ),
K=max(d$TECH_i),
tech = d$TECH_i,
y = cbind( d$y1 , d$y2 , d$y3 , d$y4 , d$y5 , d$y6 ),
s = cbind(d$s1 , d$s2 , d$s3 , d$s4 ,d$s5 ,d$s6  ) ,
bout = d$forg_bout,#bout is forg index here
id = as.integer(as.factor(d$mono_i)),
N_effects=4,
postmig=d$postmig,
postfizz = d$postfizz, 
age=d$agecont_s

)

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



parlistI=c( "lambda","phi_i", "dev" , "log_lik" , "alpha")
parlistF=c("a_id" , "mu", "lambda","phi_i" , "gamma_i" , "fconf_i" , "chi_i" , "dev" , "log_lik")
parlistF2=c("a_id" , "mu", "lambda","phi_i" , "gamma_i" , "fconf_i" , "chi_i" , "dev" , "log_lik","beta")

traceplot(fit_i)

fit_i = stan(file = 'ewa_i.stan', data = datalist_i ,iter = 2000, warmup=1000, chains=2,  pars=parlistI , control=list(adapt_delta=0.99))
fit_s = stan( file = 'ewa_social_freq_ema.stan', data = datalist_s ,iter = 200, warmup=100, chains=2, control=list(adapt_delta=0.99))
write.csv(extract(fit_s) , "post_s_ST.csv")

# fit_s_mig = stan( file = 'PN_freq_social_mig.stan', data = datalist_smig ,iter = 2000, warmup=1000, chains=2,  pars=parlistF2 , control=list(adapt_delta=0.99))
# write.csv(extract(fit_s_mig) , "post_s_mig_ST.csv")

# fit_s_fizz = stan( file = 'PN_freq_social_mig.stan', data = datalist_sfizz ,iter = 2000, warmup=1000, chains=2,  pars=parlistF2 , control=list(adapt_delta=0.99))
# write.csv(extract(fit_s_fizz) , "post_s_fizz_ST.csv")

# fit_s_popsize = stan( file = 'PN_freq_social_popsize.stan', data = datalist_ssize ,iter = 2000, warmup=1000, chains=2,  pars=parlistF2 , control=list(adapt_delta=0.99))
# write.csv(extract(fit_s_popsize) , "post_s_popsize_ST.csv")

# #Graphs of posterior
# postmig <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/post_s_mig_ST.csv")
# postfizz <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/post_s_fizz_ST.csv")
# posts <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/post_s_ST.csv")
# postpop <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/post_s_popsize_ST.csv")

# ##main effects only

dens(logistic(post$mu.1) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to new experience" , col="white")##phi
abline(v=median(logistic(post$mu.1)) ) 
shade( density(logistic(post$mu.1 )) , lim= as.vector(HPDI(logistic(post$mu.1), prob=0.9999)) , col = col.alpha("forestgreen", 0.25))

dens(exp(post$mu.2)  , main=expression(paste( "\u0192"[c])),  xlim=c(0,3) , xlab="strength of frequency dependence" , col="white")##fconf
abline(v=median(exp(post$mu.2)) ) #fconf
abline(v=1 , lty=2 ) #fconf
shade( density(exp(post$mu.2 )) , lim= as.vector(HPDI(exp(post$mu.2), prob=0.9999)) , col = col.alpha("red", 0.25))

dens(logistic(post$mu.3) , main=expression(paste(gamma))  ,  xlim=c(0,1) , xlab= "weight of social information" , col="white") #gamma
abline(v=median(logistic(post$mu.3)) ) 
shade( density(logistic(post$mu.3 )) , lim= as.vector(HPDI(logistic(post$mu.3), prob=0.9999)) , col = col.alpha("slateblue", 0.25))

dens(post$lambda , show.HPDI=0.99999 , main=expression(paste(lambda))  ,  xlim=c(0,5) , xlab= "lambda") #gamma
abline(v=mean(post$lambda ) ) 

vfphi <- post[,match("phi_i.1",names(post)):match("phi_i.228",names(post))]
vfgam <- post[,match("gamma_i.1",names(post)):match("gamma_i.228",names(post))]
vffconf <- post[,match("fconf_i.1",names(post)):match("fconf_i.228",names(post))]

varefphi <-  vareffconf <- varefgam <- rep(0,length(unique(d$mono)))
for (i in 1:length(unique(d$mono))) {
varefphi[i] <-	median(vfphi[,i])
vareffconf[i] <-median(vffconf[,i])
varefgam[i] <-	mean(vfgam[,i])
}

##phi
plot(varefphi~mono_index[,1], ylab="Weight given to new experience (\u03D5)" , xlab="Individual ID" , ylim=c(0,1) , xaxt='n' , cex=0.5 )
abline(h=median(logistic(post$mu.1)), col="darkgreen" , lw=3)
abline(h=HPDI(logistic(post$mu.1))[1], col="darkgreen" , lty=2)
abline(h=HPDI(logistic(post$mu.1))[2], col="darkgreen" , lty=2)
points(varefphi~mono_index[,1],pch=19, cex=0.5) 
axis(1, at=mono_index[,1] , labels=mono_index[,1], las=2 , cex.axis=0.3 , lwd.ticks=1 , hadj=0.3)

for(i in 1:228){	
	pts <- sample(vfphi[,i],100)
	for (j in 1:100){ 	
		points( pts[j] ~ i  , col=col.alpha( "darkgreen" , alpha = 0.1 ) , pch=19 , cex=0.5)
	}
}

##gamma
plot(varefgam~mono_index[,1], ylab="Weight of social information (\u03B3)" , xlab="Individual ID" , ylim=c(0,1), xaxt='n' , cex=0.5 )
abline(h=median(logistic(post$mu.3)), col="slateblue" , lw=3)
abline(h=HPDI(logistic(post$mu.3))[1], col="slateblue" , lty=2)
abline(h=HPDI(logistic(post$mu.3))[2], col="slateblue" , lty=2)
points(varefgam~mono_index[,1],pch=19, cex=0.5) 
axis(1, at=mono_index[,1] , labels=mono_index[,1], las=2 , cex.axis=0.3 , lwd.ticks=1 , hadj=0.3)

for(i in 1:228){	
	pts <- sample(vfgam[,i],100)
	for (j in 1:100){ 	
		points( pts[j] ~ i  , col=col.alpha( "slateblue" , alpha = 0.1 ) , pch=19 , cex=0.5)
	}
}


#fconf
plot(vareffconf~mono_index[,1], ylab="Strength of frequency dependence" , xlab="Individual ID" , ylim=c(0,6), xaxt='n' , cex=0.5)
abline(h=median(exp(post$mu.2)), col="red" , lw=3)
abline(h=HPDI(exp(post$mu.2))[1], col="red" , lty=2)
abline(h=HPDI(exp(post$mu.2))[2], col="red" , lty=2)
points(vareffconf~mono_index[,1],pch=19 , cex=0.5) 
axis(1, at=mono_index[,1] , labels=mono_index[,1], las=2 , cex.axis=0.3 , lwd.ticks=1 , hadj=0.3)

for(i in 1:228){	
	pts <- sample(vffconf[,i],100)
	for (j in 1:100){ 	
		points( pts[j] ~ i  , col=col.alpha( "red" , alpha = 0.1 ) , pch=19 , cex=0.5)
	}
}

#######POST MIGRATION

par(mfrow=c(1, 3), oma = c(-1, -1, -1, -1))
post <- postmig
dens( logistic(post$mu.1) ,  main=expression(paste(phi)) , xlim=c(0,1) , ylim=c(0,25), xlab="weight given to new experience" , col = col.alpha("white", 0.000001) , cex.lab=1.6 , cex.main=1.8 )##phi
dens(logistic(post$mu.1 + post$beta.1) , add=TRUE , col = col.alpha("white", 0.000001) , xlim=c(0,1) , xlab="weight given to new experience" )##phi
abline(v=mean(logistic(post$mu.1 )) , col="slateblue" ) 
abline(v=mean(logistic(post$mu.1 + post$b_year.1 )) , col="orange" ) 
shade( density(logistic(post$mu.1 )) , lim= as.vector(HPDI(logistic(post$mu.1), prob=0.9999)) , col = col.alpha("slateblue", 0.25))
shade( density(logistic(post$mu.1 + post$beta.1 )) , lim= as.vector(HPDI(logistic(post$mu.1+ post$beta.1), prob=0.9999)) , col = col.alpha("orange", 0.25))
legend("topright" ,legend=c("Pre-Migration", "Post-Migration"), pch=19, col=c(col.alpha("slateblue", 0.5) , col.alpha("orange", 0.5)) , cex=1.5 , , bty = "n") # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend

#plot(density(logistic(post$mu.1)) , main="phi" , xlim=c(0,1) ,ylim=c(0,15), xlab="weight given to new experience" , col="slateblue" , fill="slateblue")##phi

#abline(v=median(logistic(post$mu.1 +p)) ) 
dens(exp(post$mu.2 ) ,main=expression(paste( "\u0192"[c])) ,  xlim=c(0,5) , ylim=c(0,1.5) , xlab="strength of frequency dependence" , col = col.alpha("white", 0.000001) , cex.lab=1.6, cex.main=1.8 )##fconf
dens(exp(post$mu.2 + post$beta.2) , add=TRUE , col = col.alpha("white", 0.000001) )##fconf
abline(v=median(exp(post$mu.2 )) , col="slateblue" ) 
abline(v=median(exp(post$mu.2 + post$beta.2 )) , col="orange" ) 
shade( density( exp(post$mu.2 )) , lim= as.vector(HPDI(exp(post$mu.2), prob=0.999)) , col = col.alpha("slateblue", 0.25))
shade( density(exp(post$mu.2+ post$beta.2 )) , lim= as.vector(HPDI(exp(post$mu.2+ post$beta.2), prob=0.999)) , col = col.alpha("orange", 0.25))
legend("topright" ,legend=c("Pre-Migration", "Post-Migration"), pch=19, col=c(col.alpha("slateblue", 0.5) , col.alpha("orange", 0.5)) , cex=1.5 , , bty = "n") # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend

dens(logistic(post$mu.3 ) , main=expression(paste(gamma)) , xlim=c(0,1) ,ylim=c(0,25), xlab="weight of social information" , col = col.alpha("white", 0.000001) , cex.lab=1.6, cex.main=1.8 )##phi
dens(logistic(post$mu.3   + post$beta.3) , add=TRUE , col = col.alpha("white", 0.000001) , main="phi" , xlim=c(0,1) )##phi
abline(v=mean(logistic(post$mu.3 )) , col="slateblue" ) 
abline(v=mean(logistic(post$mu.3 + post$beta.3 )) , col="orange" ) 
shade( density(logistic(post$mu.3 )) , lim= as.vector(HPDI(logistic(post$mu.3), prob=0.999)) , col = col.alpha("slateblue", 0.25))
shade( density(logistic(post$mu.3+ post$beta.3 )) , lim= as.vector(HPDI(logistic(post$mu.3+ post$beta.3), prob=0.999)) , col = col.alpha("orange", 0.25))
legend("topright" ,legend=c("Pre-Migration", "Post-Migration"), pch=19, col=c(col.alpha("slateblue", 0.5) , col.alpha("orange", 0.5)) , cex=1.5 , , bty = "n") # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend

lty=c(1,1), #

##post fission
post <- postfizz
#old.par <- par(mfrow=c(1, 3), oma = c(-2, -2, -2, -2))
#par(mfrow=c(1, 1))
dens( logistic(post$mu.1) ,  main=expression(paste(phi)) , xlim=c(0,1) , ylim=c(0,35), xlab="weight given to new experience" , col = col.alpha("white", 0.000001), cex.lab=1.6 , cex.main=1.8)##phi
dens(logistic(post$mu.1 + post$beta.1) , add=TRUE , col = col.alpha("white", 0.000001) , xlim=c(0,1) , xlab="weight given to new experience" )##phi
abline(v=mean(logistic(post$mu.1 )) , col="slateblue" ) 
abline(v=mean(logistic(post$mu.1 + post$beta.1 )) , col="orange" ) 
shade( density(logistic(post$mu.1 )) , lim= as.vector(HPDI(logistic(post$mu.1), prob=0.9999)) , col = col.alpha("slateblue", 0.25))
shade( density(logistic(post$mu.1 + post$beta.1 )) , lim= as.vector(HPDI(logistic(post$mu.1+ post$beta.1), prob=0.9999)) , col = col.alpha("orange", 0.25))
legend("topright" ,legend=c("Pre-Fission", "Post-Fission"), pch=19, col=c(col.alpha("slateblue", 0.5) , col.alpha("orange", 0.5)) , cex=1.5 , , bty = "n") # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend

#plot(density(logistic(post$mu.1)) , main="phi" , xlim=c(0,1) ,ylim=c(0,15), xlab="weight given to new experience" , col="slateblue" , fill="slateblue")##phi

#abline(v=median(logistic(post$mu.1 +p)) ) 
dens(exp(post$mu.2 ) ,main=expression(paste( "\u0192"[c])) ,  xlim=c(0,5) , ylim=c(0,3) , xlab="strength of frequency dependence" , col = col.alpha("white", 0.000001), cex.lab=1.6 , cex.main=1.8 )##fconf
dens(exp(post$mu.2 + post$beta.2) , add=TRUE , col = col.alpha("white", 0.000001) )##fconf
abline(v=median(exp(post$mu.2 )) , col="slateblue" ) 
abline(v=median(exp(post$mu.2 + post$beta.2 )) , col="orange" ) 
shade( density( exp(post$mu.2 )) , lim= as.vector(HPDI(exp(post$mu.2), prob=0.999)) , col = col.alpha("slateblue", 0.25))
shade( density(exp(post$mu.2+ post$beta.2 )) , lim= as.vector(HPDI(exp(post$mu.2+ post$beta.2), prob=0.999)) , col = col.alpha("orange", 0.25))
legend("topright" ,legend=c("Pre-Fission", "Post-Fission"), pch=19, col=c(col.alpha("slateblue", 0.5) , col.alpha("orange", 0.5)) , cex=1.5 , , bty = "n") # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend

dens(logistic(post$mu.3 ) , main=expression(paste(gamma)) , xlim=c(0,1) ,ylim=c(0,25), xlab="weight of social information" , col = col.alpha("white", 0.000001) , cex.lab=1.6 , cex.main=1.8)##phi
dens(logistic(post$mu.3   + post$beta.3) , add=TRUE , col = col.alpha("white", 0.000001) , main="phi" , xlim=c(0,1) )##phi
abline(v=mean(logistic(post$mu.3 )) , col="slateblue" ) 
abline(v=mean(logistic(post$mu.3 + post$beta.3 )) , col="orange" ) 
shade( density(logistic(post$mu.3 )) , lim= as.vector(HPDI(logistic(post$mu.3), prob=0.999)) , col = col.alpha("slateblue", 0.25))
shade( density(logistic(post$mu.3+ post$beta.3 )) , lim= as.vector(HPDI(logistic(post$mu.3+ post$beta.3), prob=0.999)) , col = col.alpha("orange", 0.25))
legend("topright" ,legend=c("Pre-Fission", "Post-Fission"), pch=19, col=c(col.alpha("slateblue", 0.5) , col.alpha("orange", 0.5)) , cex=1.5 , , bty = "n") # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend


#abline(v=median(exp(post$mu.2)) ) #fconf
#abline(v=1 , lty=2 ) #fconf

dens(1 + exp(post$mu.3) , show.HPDI=0.85 ,main="f_pay" ,  xlim=c(0,5), xlab="strength of payoff bias")##fpay
#abline(v=1 + median(exp(post$mu.3))  ) 

dens(logistic(post$mu.4) , show.HPDI=0.85 , main="delta" ,  xlim=c(0,1)  , xlab="weight of frequency dependence")##delta
#abline(v=median(logistic(post$mu.4))) 

dens(logistic(post$mu.5) , show.HPDI=0.85 , main="gamma" ,  xlim=c(0,1) , xlab= "weight of social information") #gamma
#abline(v=median(logistic(post$mu.5)) ) 

###popsize
post <- postpop
#varef for individuals
#plot varying effects for phi
vfphi <- post[,match("a_id.1.1",names(post)):match("a_id.228.1",names(post))]
vfgam <- post[,match("a_id.1.3",names(post)):match("a_id.228.3",names(post))]
vffconf <- post[,match("a_id.1.2",names(post)):match("a_id.23.2",names(post))]

varefphi <-  vareffconf  <- varefgam <- rep(0,23)

for (i in 1:228){
varefphi[i] <-	mean(logistic(vfphi[,i] + post$mu.1 + post$beta.1*mono_index$age[i]))
varefgam[i] <-	mean(logistic(vfgam[,i] + post$mu.3  + post$beta.3*mono_index$age[i]))
vareffconf[i] <-	median(exp(vffconf[,i] + post$mu.2  + post$beta.2*mono_index$age[i]))

}


##########phi and age
plot(d$mono_i~d$pop_month, ylab="Weight given to new experience (\u0278)" , xlab="group size" , pch=19 , col="white" , ylim=c(0,.7) , xlim=c(min(d$pop_month) , max(d$pop_month)) ,cex=1.5 , cex.lab=1.5 )
#abline(h=logistic(median(post$mu.5)), col="violet" , lw=2)
#abline(h=logistic(median(post$mu[,3])), col="red")
#text(mono_index$age, varefphi, mono_index$mono, cex=1, pos=3, col="black")
#axis(1, at = seq(from=min(d$age) , to=max(d$age), by = 2))
#axis(2, at = seq(from=0 , to=0.7, by = 0.1))

popsize.seq <- seq(from=min(d$pop_month) , to=max(d$pop_month) , length=25 )
pee.med <- sapply(popsize.seq , function(z)
median(logistic(post$mu.1 + post$beta.1*z) ))
lines(popsize.seq , pee.med , lw=2)

#pee.ci<- sapply(popsize.seq , function(z)
HPDI(logistic(post$mu.1 + post$beta.1*z) ))
#lines(popsize.seq , pee.ci[1,] , lw=2 , lty=2)
#lines(popsize.seq , pee.ci[2,] , lw=2 , lty=2)

sampz <- sample.int(nrow(post) , size=100)
for (i in 1:100){
pee <- sapply(popsize.seq , function(z)
	 logistic(post$mu.1[sampz[i]] + post$beta.1[sampz[i]]*z) )
lines(popsize.seq , pee , lw=2 , col=col.alpha("slateblue", 0.1))
}
pee.med <- sapply(popsize.seq , function(z)
median(logistic(post$mu.1 + post$beta.1*z) ))
lines(popsize.seq , pee.med , lw=2)

#gamma

plot(d$mono_i~d$pop_month, ylab="Weight of Social Information (\u03B3)" , xlab="group size" , pch=19 , col="white" , xlim=c(min(d$pop_month) , max(d$pop_month)), ylim=c(0,1) , cex=1.5 , cex.lab=1.5)
#abline(h=logistic(median(post$mu.5)), col="slateblue" , lw=2)
#abline(h=logistic(median(post$mu[,3])), col="red")
#points(varefgam~mono_index$age, pch=19 , col="slateblue" , cex=1.5 )
#text(mono_index$age, varefgam, mono_index$mono, cex=1, pos=3, col="black")

popsize.seq <- seq(from=min(d$pop_month) , to=max(d$pop_month) , length=25 )
pee.med <- sapply(popsize.seq , function(z)
	 mean(logistic(post$mu.3 + post$beta.3*z) ))
lines(popsize.seq , pee.med , lw=2)

pee.ci<- sapply(popsize.seq , function(z)
	 HPDI(logistic(post$mu.3 + post$beta.3*z) ))

#lines(popsize.seq , pee.ci[1,] , lw=2 , lty=2)
#lines(popsize.seq , pee.ci[2,] , lw=2 , lty=2)

sampz <- sample.int(nrow(post) , size=100)
for (i in 1:100){
pee <- sapply(popsize.seq , function(z)
	 logistic(post$mu.3[sampz[i]] + post$beta.3[sampz[i]]*z) )
lines(popsize.seq , pee , lw=2 , col=col.alpha("darkgreen", 0.1))
}

lines(popsize.seq , pee.med , lw=2)

#fconf
plot(d$mono_i~d$pop_month, ylab="Strength of Frequency Dependence (f-conf)" , xlab="group size" , pch=19 , col="white" , xlim=c(min(d$pop_month) , max(d$pop_month)), ylim=c(0,4) , cex=1.5 , cex.lab=1.5)
#abline(h=logistic(median(post$mu.5)), col="slateblue" , lw=2)
#abline(h=logistic(median(post$mu[,3])), col="red")
#points(vareffconf~mono_index$age, pch=19 , col="orange" , cex=1.5 )
#text(mono_index$age, vareffconf, mono_index$mono, cex=1, pos=3, col="black")
popsize.seq <- seq(from=min(d$pop_month) , to=max(d$pop_month) , length=25 )
pee.med <- sapply(popsize.seq , function(z)
	 mean(exp(post$mu.2 + post$beta.2*z) ))
lines(popsize.seq , pee.med , lw=2)

pee.ci<- sapply(popsize.seq , function(z)
	 HPDI(exp(post$mu.2 + post$beta.2*z) ))
#lines(popsize.seq , pee.ci[1,] , lw=2 , lty=2)
#lines(popsize.seq , pee.ci[2,] , lw=2 , lty=2)

sampz <- sample.int(nrow(post) , size=100)
for (i in 1:100){
pee <- sapply(popsize.seq , function(z)
	 exp(post$mu.2[sampz[i]] + post$beta.2[sampz[i]]*z) )
lines(popsize.seq , pee , lw=2 , col=col.alpha("red", 0.1))
}

lines(popsize.seq , pee.med , lw=2)



###########group size

census <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/Demography Data/MonoGroupCensusLongform.csv")
census <- subset(census, year < 2013 )
census <- subset(census, year > 2002)
census <- census[,1:34]
trimws(census$mono)
cc <- reshape(census , varying=list(names(census)[4:34]), direction='long')
names(cc)[names(cc) == 'X1'] <- 'grouptoday'
names(cc)[names(cc) == 'time'] <- 'day'
cc$date <- as.Date(paste(cc$year , cc$month , cc$day , sep="-")  )

demo$natal <- trimws(tolower(demo$natal))
demo$resident.group <- tolower(demo$resident.group)

d<-merge(d,demo)
str(cc)

ccc<- subset(cc, select=c(mono,date,grouptoday) )
selectedRows <- (ccc$date %in% d$date )
ccc <- ccc[selectedRows,]
d$mono <- as.character(d$mono)
ccc$mono <- as.character(ccc$mono)
d$date <- as.Date(d$date)
unique(d$mono[d$grouptoday==cc & d$month==m & year==y])
cc$groupsize <-0
for (i in 1:nrow(cc)){
	cc$groupsize[i] <- length(unique(cc$mono[cc$grouptoday==cc$grouptoday[i] & cc$month==cc$month[h] & cc$year==cc$year[i]]))
}
