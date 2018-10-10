R
library(chron)
library(rethinking)
library(lubridate)
library(stringr) 
library(gtools)
library(stringi) 
library(reshape)

#library(chron)
d12 <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2012.R.ST.csv")
d11 <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2011.R.ST.csv")
d10 <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2010.R.ST.csv")
d09<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2009.R.ST.csv")
d08<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2008.R.ST.csv")
d07<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2007.R.ST.csv")
d06<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2006.R.ST.csv")
d05<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2005.R.ST.csv")
d04<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2004.R.ST.csv")
d03<- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/R Importable Data/2003.R.ST.csv")

demo <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/Demography Data/Demography.csv")

d12$date <- as.Date(d12$date,format='%d/%m/%y')
d11$date <- as.Date(d11$date,format='%d/%m/%y')
d10$date <- as.Date(d10$date,format='%d/%m/%y')
d09$date <- as.Date(d09$date,format='%d/%m/%y')
d08$date <- as.Date(d08$date,format='%d/%m/%y')
d07$date <- as.Date(d07$date,format='%d/%m/%y')
d06$date <- as.Date(d06$date,format='%d/%m/%y')
d05$date <- as.Date(d05$date,format='%d/%m/%y')
d04$date <- as.Date(d04$date,format='%d/%m/%y')
d03$date <- as.Date(d03$date,format='%d/%m/%y')

year(d03$date) <- 2003
year(d04$date) <- 2004
year(d05$date) <- 2005
year(d06$date) <- 2006
year(d07$date) <- 2007
year(d08$date) <- 2008
year(d09$date) <- 2009
year(d10$date) <- 2010
year(d11$date) <- 2011
year(d12$date) <- 2012

d <- smartbind(d12,d11,d10,d09,d08,d07,d06,d05,d04,d03)
#d <- smartbind(d12,d03)

#d$time <- ifelse(d$time.bi==NA , d$time.s , d$time.bi)
d$time.s <- as.character(d$time.s)
d$time.bi <- as.character(d$time.bi)

str_trim(d$time.s)
str_trim(d$time.bi)
d$time.bi <- gsub(" ", "", d$time.bi, fixed = TRUE) #get rid of all whitespace
d$time.s <- gsub(" ", "", d$time.s, fixed = TRUE)
d$time.bi <- gsub("AM", "", d$time.bi, fixed = TRUE) #get rid of all AM
d$time.s <- gsub("AM", "", d$time.s, fixed = TRUE)
d$time.bi <- gsub("PM", "", d$time.bi, fixed = TRUE) #get rid of all PM
d$time.s <- gsub("PM", "", d$time.s, fixed = TRUE)

##boo excel-- clean up random spaces and time errors to convert all to military time
for(i in 1:nrow(d)){
	ifelse(substr(d$time.s[i] , 1 , 2)=="1:" , str_sub(d$time.s[i],1,2) <- "13:" , str_sub(d$time.s[i],1,2) <-  substr(d$time.s[i] , 1 , 2) ) 
	ifelse(substr(d$time.s[i] , 1 , 2)=="2:" , str_sub(d$time.s[i],1,2) <- "14:" , str_sub(d$time.s[i],1,2) <-  substr(d$time.s[i] , 1 , 2) ) 
	ifelse(substr(d$time.s[i] , 1 , 2)=="3:" , str_sub(d$time.s[i],1,2) <- "15:" , str_sub(d$time.s[i],1,2) <-  substr(d$time.s[i] , 1 , 2) ) 
	ifelse(substr(d$time.s[i] , 1 , 2)=="4:" , str_sub(d$time.s[i],1,2) <- "16:" , str_sub(d$time.s[i],1,2) <-  substr(d$time.s[i] , 1 , 2) )
	ifelse(substr(d$time.s[i] , 1 , 2)=="5:" , str_sub(d$time.s[i],1,2) <- "17:" , str_sub(d$time.s[i],1,2) <-  substr(d$time.s[i] , 1 , 2) )####Check this to make sure nid$time.shts are not included
	ifelse(substr(d$time.s[i] , 1 , 3)=="01:" , str_sub(d$time.s[i],1,3) <- "13:" , str_sub(d$time.s[i],1,3) <-  substr(d$time.s[i] , 1 , 3) ) 
	ifelse(substr(d$time.s[i] , 1 , 3)=="02:" , str_sub(d$time.s[i],1,3) <- "14:" , str_sub(d$time.s[i],1,3) <-  substr(d$time.s[i] , 1 , 3) ) 
	ifelse(substr(d$time.s[i] , 1 , 3)=="03:" , str_sub(d$time.s[i],1,3) <- "15:" , str_sub(d$time.s[i],1,3) <-  substr(d$time.s[i] , 1 , 3) ) 
	ifelse(substr(d$time.s[i] , 1 , 3)=="04:" , str_sub(d$time.s[i],1,3) <- "16:" , str_sub(d$time.s[i],1,3) <-  substr(d$time.s[i] , 1 , 3) )
	ifelse(substr(d$time.s[i] , 1 , 3)=="05:" , str_sub(d$time.s[i],1,3) <- "17:" , str_sub(d$time.s[i],1,3) <-  substr(d$time.s[i] , 1 , 3) )####Check this to make sure nid$time.shts are not included
 	ifelse(substr(d$time.bi[i] , 1 , 2)=="1:" , str_sub(d$time.bi[i],1,2) <- "13:" , str_sub(d$time.bi[i],1,2) <-  substr(d$time.bi[i] , 1 , 2) ) 
	ifelse(substr(d$time.bi[i] , 1 , 2)=="2:" , str_sub(d$time.bi[i],1,2) <- "14:" , str_sub(d$time.bi[i],1,2) <-  substr(d$time.bi[i] , 1 , 2) ) 
	ifelse(substr(d$time.bi[i] , 1 , 2)=="3:" , str_sub(d$time.bi[i],1,2) <- "15:" , str_sub(d$time.bi[i],1,2) <-  substr(d$time.bi[i] , 1 , 2) ) 
	ifelse(substr(d$time.bi[i] , 1 , 2)=="4:" , str_sub(d$time.bi[i],1,2) <- "16:" , str_sub(d$time.bi[i],1,2) <-  substr(d$time.bi[i] , 1 , 2) )
	ifelse(substr(d$time.bi[i] , 1 , 2)=="5:" , str_sub(d$time.bi[i],1,2) <- "17:" , str_sub(d$time.bi[i],1,2) <-  substr(d$time.bi[i] , 1 , 2) )####Check this to make sure nid$time.bihts are not included
	ifelse(substr(d$time.bi[i] , 1 , 3)=="01:" , str_sub(d$time.bi[i],1,3) <- "13:" , str_sub(d$time.bi[i],1,3) <-  substr(d$time.bi[i] , 1 , 3) ) 
	ifelse(substr(d$time.bi[i] , 1 , 3)=="02:" , str_sub(d$time.bi[i],1,3) <- "14:" , str_sub(d$time.bi[i],1,3) <-  substr(d$time.bi[i] , 1 , 3) ) 
	ifelse(substr(d$time.bi[i] , 1 , 3)=="03:" , str_sub(d$time.bi[i],1,3) <- "15:" , str_sub(d$time.bi[i],1,3) <-  substr(d$time.bi[i] , 1 , 3) ) 
	ifelse(substr(d$time.bi[i] , 1 , 3)=="04:" , str_sub(d$time.bi[i],1,3) <- "16:" , str_sub(d$time.bi[i],1,3) <-  substr(d$time.bi[i] , 1 , 3) )
	ifelse(substr(d$time.bi[i] , 1 , 3)=="05:" , str_sub(d$time.bi[i],1,3) <- "17:" , str_sub(d$time.bi[i],1,3) <-  substr(d$time.bi[i] , 1 , 3) )####Check this to make sure nid$time.bihts are not included
}

sort(unique(d$time.s2))

d$time.s2 <- chron(times=d$time.s)
d$time.bi2 <- chron(times=d$time.bi)
d$time <- ifelse(is.na(d$time.bi2)==TRUE , d$time.s2  , d$time.bi2  )

d$timedate <- chron(dates=d$date, times=d$time ,format= c(dates="y-m-d", times="h:m:s")) #make sure all files have a time seen

###deal with issues in ravxx that make it a pair in the ass- its better than one in the true

##make proximity data useful
d$AA <- toupper(as.character(d$aa))
d$AA <- gsub(" ", "", d$AA, fixed = TRUE)

# d$AA <- ifelse(d$AA=="ON? OR LO?" , "ON,LO" , d$AA)
# d$AA <- ifelse(d$AA=="PC? OR NE?" ,  "PC,NE" , d$AA)
# d$AA <- ifelse(d$AA=="KK?" , "KK" , d$AA)
# d$AA <- ifelse(d$AA=="NO INFO" , "" , d$AA)
# d$AA <- ifelse(d$AA=="POSSIBLY CS" , "CS" , d$AA)
# d$AA <- ifelse(d$AA=="LB (MORE NERVOUS STARE THAN INTEREST IN PROCESSING)" , "LB" , d$AA)
# d$AA <- ifelse(d$AA=="?" , "" , d$AA)
# d$AA <- ifelse(d$AA=="QJ (BECAUSE OF APPROACH NOT PROCESSING RELATED)" , "QJ" , d$AA)
# d$AA <- ifelse(d$AA=="MZ,OCCASIONALLY" , "MZ" , d$AA)
# d$AA <- ifelse(d$AA=="KY,FACING,HIM" , "KY" , d$AA)
# d$AA <- ifelse(d$AA=="VL,(FOOD,INTEREST)" , "VL" , d$AA)
# d$AA <- ifelse(d$AA=="PD(RCIPD)" , "PD" , d$AA)
# d$AA <- ifelse(d$AA=="HC(RCIHC)" , "HC" , d$AA)
# d$AA <- ifelse(d$AA=="CD(RESCD)" , "CD" , d$AA)
# d$AA <- ifelse(d$AA=="VIDEO" , "" , d$AA)
# d$AA <- ifelse(d$AA==" N" , "" , d$AA)
d$AA <- ifelse(d$AA=="JP(DORSAL)" , "JP" , d$AA)
d$AA <- ifelse(d$AA=="ML?" , "ML" , d$AA)
d$AA <- ifelse(d$AA=="KY(DORSAL)" , "KY" , d$AA)
d$AA <- ifelse(d$AA=="MZ.OW" , "MZ,OW" , d$AA)
d$AA <- ifelse(d$AA=="MZ.OW" , "MZ,OW" , d$AA)
d$AA <- gsub("?", "", d$AA, fixed = TRUE)
d$AA <- ifelse(d$AA=="DA,(NOT,FORAGING,ST)" , "DA" , d$AA)
d$AA <- ifelse(d$AA=="XDCKM" , "KM" , d$AA)
d$AA <- ifelse(d$AA=="THCONTACT" , "TH" , d$AA)
d$AA <- ifelse(d$AA=="MZ(D),MN" , "MZ,MN" , d$AA)
d$AA <- ifelse(d$AA=="MZ(DORSAL)" , "MZ" , d$AA)
d$AA <- ifelse(d$AA=="MM,MZKY" , "MM,MZ,KY" , d$AA)
d$AA <- ifelse(d$AA=="DR(DORSAL)" , "DR" , d$AA)
d$AA <- ifelse(d$AA=="DR(CONTACT)" , "DR" , d$AA)
d$AA <- ifelse(d$AA=="DT(DORSAL)" , "DT" , d$AA)
d$AA <- ifelse(d$AA=="EI,DORSAL" , "EI" , d$AA)
d$AA <- ifelse(d$AA=="EI(D)" , "EI" , d$AA)
d$AA <- ifelse(d$AA== "GT(CONTACT)PD(D)" , "GT,PD" , d$AA)
d$AA <- ifelse(d$AA== "GT(CONTACT)PD(D)" , "GT,PD" , d$AA)
d$AA <- ifelse(d$AA== "GT(CONTACT)PD(D)BB" , "GT,PD,BB" , d$AA)
d$AA <- ifelse(d$AA== "GT(NURSING)" , "GT" , d$AA)
d$AA <- ifelse(d$AA== "HIT,R" , "" , d$AA)
d$AA <- ifelse(d$AA=="JU(D)" , "JU" , d$AA)
d$AA <- ifelse(d$AA=="(HE,IN,1,RIGHT,BEFORE,THIS)" , "HE" , d$AA)
d$AA <- ifelse(d$AA=="PD(D)" , "PD" , d$AA)
d$AA <- ifelse(d$AA=="HE,(P)" , "HE" , d$AA)
d$AA <- ifelse(d$AA=="HE,(P)" , "HE" , d$AA)
d$AA <- ifelse(d$AA=="THCONTACT" , "TH" , d$AA)

d$AA  <- ifelse( grepl('^[A-Za-z]+$', d$AA)==TRUE & stri_length(d$AA)==6, gsub("(\\D\\D)(\\D\\D)(\\D\\D)", "\\1,\\2,\\3", d$AA) , d$AA) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4
d$AA  <- ifelse( grepl('^[A-Za-z]+$', d$AA)==TRUE & stri_length(d$AA)==4, gsub("(\\D\\D)(\\D\\D)", "\\1,\\2", d$AA) , d$AA) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4
d$AA <- ifelse(d$AA==" LH,ERMP" , "LH,ER,MP" , d$AA)
d$AA <- ifelse(d$AA=="DR(CONTACT)" , "DR" , d$AA)
d$AA <- gsub("(P)", "", d$AA, fixed = TRUE)
d$AA <- gsub("(D)", "", d$AA, fixed = TRUE)
sort(unique(d$AA))

d$BB <- toupper(as.character(d$bb))
d$BB <- gsub("?", "", d$BB, fixed = TRUE)
d$BB <- gsub(" ", "", d$BB, fixed = TRUE)
d$BB <- gsub(".", ",", d$BB, fixed = TRUE)

d$BB  <- ifelse( grepl('^[A-Za-z]+$', d$BB)==TRUE & stri_length(d$BB)==10, gsub("(\\D\\D)(\\D\\D)(\\D\\D)(\\D\\D)(\\D\\D)", "\\1,\\2,\\3,\\4,\\5", d$BB) , d$BB) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4
d$BB  <- ifelse( grepl('^[A-Za-z]+$', d$BB)==TRUE & stri_length(d$BB)==8, gsub("(\\D\\D)(\\D\\D)(\\D\\D)(\\D\\D)", "\\1,\\2,\\3,\\4", d$BB) , d$BB) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4
d$BB  <- ifelse( grepl('^[A-Za-z]+$', d$BB)==TRUE & stri_length(d$BB)==6, gsub("(\\D\\D)(\\D\\D)(\\D\\D)", "\\1,\\2,\\3", d$BB) , d$BB) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4
d$BB  <- ifelse( grepl('^[A-Za-z]+$', d$BB)==TRUE & stri_length(d$BB)==4, gsub("(\\D\\D)(\\D\\D)", "\\1,\\2", d$BB) , d$BB) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4
d$BB <- ifelse(d$BB=="," , "" , d$BB)
d$BB <- ifelse(d$BB=="BK,WW,WG,WB,OJ,PLUSACOUPLEOTHERSINAPPROX.5" , "BK,WW,WG,WB,OJ" , d$BB)
d$BB <- ifelse(d$BB=="BOJJ,JU,BH,EE" , "BO,JJ,JU,BH,EE" , d$BB)
d$BB <- ifelse(d$BB=="CB,,RF" , "CB,RF" , d$BB)
d$BB <- ifelse(d$BB=="CB/OT" , "CB,OT" , d$BB)
d$BB <- ifelse(d$BB=="CL,RF,(ONLY,RF,PROCESSING)"  , "CL,RF"  , d$BB)
d$BB <- ifelse(d$BB=="CN,"  , "CN"  , d$BB)
d$BB <- ifelse(d$BB=="EIME(P)"  , "EI,ME"  , d$BB)
d$BB <- ifelse(d$BB=="EO,MN,DD,DT,VDCK"  , "EO,MN,DD,DT,VD,CK"  , d$BB)
d$BB <- ifelse(d$BB=="FVUFYKM"  , "VU,FY,KM"  , d$BB)
d$BB <- ifelse(d$BB=="DI,"  , "DI"  , d$BB)
d$BB <- ifelse(d$BB=="GF,OD,-,GROOMING,,NEITHER,PROCESSING"  , "GF,OD"  , d$BB)
d$BB <- ifelse(d$BB=="HE,(2,LENGTHS)"  , "HE"  , d$BB)
d$BB <- ifelse(d$BB=="HE,"  , "HE"  , d$BB)
d$BB <- ifelse(d$BB=="HE,,LT"  , "HE,LT"  , d$BB)
d$BB <- ifelse(d$BB=="HE(P)"  , "HE"  , d$BB)
d$BB <- ifelse(d$BB=="JAME,TU(ONLY,WHILE,INGESTING)"  , "JA,ME,TU"  , d$BB)
d$BB <- ifelse(d$BB=="KYVDVUCAVVDUEO"  , "KY,VD,VU,CA,VV,DU,EO"  , d$BB)
d$BB <- ifelse(d$BB=="LT,,JJ"  , "JJ,LT"  , d$BB)
d$BB <- ifelse(d$BB=="ME,"  , "ME"  , d$BB)
d$BB <- ifelse(d$BB=="MF,"  , "MF"  , d$BB)
d$BB <- ifelse(d$BB=="ML,"  , "ML"  , d$BB)
d$BB <- ifelse(d$BB=="MN(P),OW"  , "MN,OW"  , d$BB)
d$BB <- ifelse(d$BB=="MO,DK,WM,)BUT,NOT,DOING,SLOANEA)"   , "MO,DK,WM"   , d$BB)
d$BB <- ifelse(d$BB=="OP(P)"   , "OP"   , d$BB)
d$BB <- ifelse(d$BB=="QJ(P)"   , "QJ"   , d$BB)
d$BB <- ifelse(d$BB=="RHSIKNAHCDDR"   , "RH,SI,KN,AH,CD,DR"   , d$BB)
d$BB <- ifelse(d$BB=="RY,CONGOS"   , "RY"   , d$BB)
d$BB <- ifelse(d$BB=="VD,(ONLY,DURING,INGEST)"   , "VD"   , d$BB)
d$BB <- ifelse(d$BB=="VV(P)VDDU(P)"   , "VV,VD,DU"   , d$BB)
d$BB <- ifelse(d$BB=="CN,(AND,DN,BUT,NOT,DOING,ST)"   , "CN,DN"   , d$BB)
d$BB <- ifelse(d$BB=="BK,WW,WG,WB,OJ,PLUSACOUPLEOTHERSINAPPROX,5"   , "BK,WW,WG,WB,OJ"   , d$BB)
d$BB <- gsub("(P)", "", d$BB, fixed = TRUE)

sort(unique(d$BB))

###rav
d$rav_orig <- d$rav
d$rav <- as.character(d$ravxx)
d$rav <- toupper(d$rav)
d$rav  <- ifelse( grepl('^[A-Za-z]+$', d$rav)==TRUE & stri_length(d$rav)==4, gsub("(\\D\\D)(\\D\\D)", "\\1,\\2", d$rav) , d$rav) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4

#gsub("KK?" , "dogs" , d$rav)
d$rav <- ifelse(d$rav=="KK?" , "KK" , d$rav)
d$rav <- ifelse(d$rav=="CD(RESCD)" , "CD" , d$rav)
d$rav <- ifelse(d$rav=="ON? OR LO?" , "ON,LO" , d$rav)
d$rav <- ifelse(d$rav=="PC? OR NE?"  , "PC,NE" , d$rav)
d$rav <- ifelse(d$rav=="NO INFO"  , "" , d$rav)
d$rav <- ifelse(d$rav=="LH ER MP"  , "LH,ER,MP" , d$rav)
d$rav <- ifelse(d$rav=="POSSIBLY CS"  , "CS" , d$rav)
d$rav <- ifelse(d$rav=="LB (MORE NERVOUS STARE THAN INTEREST IN PROCESSING)"  , "LB" , d$rav)
d$rav <- ifelse(d$rav=="QJ (BECAUSE OF APPROACH NOT PROCESSING RELATED)" , "QJ" , d$rav)
d$rav <- ifelse(d$rav=="MZ,OCCASIONALLY"  , "MZ" , d$rav)
d$rav <- ifelse(d$rav=="KY,FACING,HIM" , "KY" , d$rav)
d$rav <- ifelse(d$rav=="VL,(FOOD,INTEREST)" , "VL" , d$rav)
d$rav <- ifelse(d$rav=="DYSS" , "DY,SS" , d$rav)
d$rav <- ifelse(d$rav=="SYMW" , "SY,MW" , d$rav)
d$rav <- ifelse(d$rav=="DWWM" , "DW,WM" , d$rav)
d$rav <- ifelse(d$rav=="PD(RCIPD)" , "PD" , d$rav)
d$rav <- ifelse(d$rav=="HC(RCIHC)" , "HC" , d$rav)
d$rav <- ifelse(d$rav=="RTUG" , "RT,UG" , d$rav)
d$rav <- ifelse(d$rav=="STSY" , "ST,SY" , d$rav)
d$rav <- ifelse(d$rav=="MTRT" , "MT,RT" , d$rav)
d$rav <- ifelse(d$rav=="MLJU" , "ML,JU" , d$rav)
d$rav <- ifelse(d$rav=="MLJA" , "ML,JA" , d$rav)
d$rav <- ifelse(d$rav=="MRVD" , "MR,VD" , d$rav)
d$rav <- ifelse(d$rav=="TODU" , "TO,DU" , d$rav)
d$rav <- ifelse(d$rav=="MZOW" , "MZ,OW" , d$rav)
d$rav <- ifelse(d$rav=="DAOW" , "DA,OW" , d$rav)
d$rav <- ifelse(d$rav=="FYMZ" , "FY,MZ" , d$rav)
d$rav <- ifelse(d$rav=="UK (OR IS IT GC?)" , "UK" , d$rav)
d$rav <- ifelse(d$rav=="UG?" , "UG" , d$rav)
d$rav <- ifelse(d$rav=="TW?" , "TW" , d$rav)
d$rav <- ifelse(d$rav=="SY?" , "SY" , d$rav)
d$rav <- ifelse(d$rav=="RO AND GT ONLY WATCHED INGESTING NOT PROCESSING" , "" , d$rav)
d$rav <- ifelse(d$rav=="OWKY? " , "OW,KY" , d$rav)
d$rav <- ifelse(d$rav=="ON VIDEO" , "" , d$rav)
d$rav <- ifelse(d$rav=="N (EI LOOKS AT FRUITS)" , "" , d$rav)
d$rav <- ifelse(d$rav=="VIDEO" , "" , d$rav)
sort(unique(d$rav))
d$rav <- gsub(" ", "", d$rav, fixed = TRUE)
d$rav  <- ifelse( grepl('^[A-Za-z]+$', d$rav)==TRUE & stri_length(d$rav)==4, gsub("(\\D\\D)(\\D\\D)", "\\1,\\2", d$rav) , d$rav) #PUTS COMMAS BETWEEN VALUES OF STINGS WITH ONLY LETTERS AND LENGTH 4



####FAV STUFF                                        
d$fav <- as.character(d$favxx)
d$fav <- toupper(d$fav)
d$fav <- ifelse(d$fav=="KK?" , "KK" , d$fav)
d$fav <- ifelse(d$fav=="NO INFO"   , "" , d$fav)
d$fav <- ifelse(d$fav=="PY, ER"   , "PY,ER" , d$fav)
d$fav <- ifelse(d$fav=="XM (DY OR UK)"   , "DY,UK" , d$fav)
d$fav <- ifelse(d$fav=="HA (NO FRUIT)"    , "HA" , d$fav)
d$fav <- ifelse(d$fav=="12:02:22 ACICIDM ST" , "" , d$fav)
d$fav <- ifelse(d$fav=="XX (GLANCES AT/AVOIDS)" , "" , d$fav)
d$fav <- ifelse(d$fav=="CS (GLANCES)" , "CS" , d$fav)
d$fav <- ifelse(d$fav=="AVS,FAV" , "" , d$fav)
d$fav <- ifelse(d$fav=="FACING,MZ,BUT,NOT,LOOKING" , "MZ" , d$fav)
d$fav <- ifelse(d$fav=="PW,(ALARMS,AT,PW,NOT,CONNECTED,TO,PROCESSING)" , "PW" , d$fav)
d$fav <- ifelse(d$fav== "AL,(NOT,TO,DO,WITH,PROCESSING)" , "AL" , d$fav)
d$fav <- ifelse(d$fav== "CA,(TRAVELLING)" , "" , d$fav)
d$fav <- ifelse(d$fav== "PPNOTPROC" , "" , d$fav)
d$fav <- ifelse(d$fav== "GM,SR,LOTSOFMALESRUNNINGINXCC" , "GM,SR" , d$fav)
d$fav <- ifelse(d$fav== "FY,KM" , "FY,KM" , d$fav)
d$fav <- ifelse(d$fav== "YA?DU?" , "YA,DU" , d$fav)
d$fav <- ifelse(d$fav== "LOOKSAROUNDTREE" , "" , d$fav)
d$fav <- ifelse(d$fav== ">10MHXBMO" , "MH,MO" , d$fav)
d$fav <- ifelse(d$fav== ">10PPMO" , "PP,MO" , d$fav)
d$fav <- ifelse(d$fav== ">10WMGMDK" , "WM,GM,DK" , d$fav)
d$fav <- ifelse(d$fav== ">10GTROHC" , "GT,RO,HC" , d$fav)
d$fav <- ifelse(d$fav== "<10GTRO" , "GT,RO" , d$fav)
d$fav <- ifelse(d$fav== "TOVD" , "TO,VD" , d$fav)
d$fav <- ifelse(d$fav== "YAMR" , "YA,MR" , d$fav)
d$fav <- ifelse(d$fav== "MFSD" , "MF,SD" , d$fav)
d$fav <- ifelse(d$fav== "SDMFGF" , "MF,SD,GF" , d$fav)
d$fav <- ifelse(d$fav== "SDST" , "SD,ST" , d$fav)
d$fav <- ifelse(d$fav== "DT?" , "DT" , d$fav)
d$fav <- ifelse(d$fav== "CE?" , "CE" , d$fav)
d$fav <- ifelse(d$fav== "MMFYKM" , "MM,FY,KM" , d$fav)
d$fav <- ifelse(d$fav== "FYKM" , "FY,KM" , d$fav)
d$fav <- ifelse(d$fav== "CMAL" , "CM,AL" , d$fav)
d$fav <- ifelse(d$fav== "TOMM" , "TO,MM", d$fav)
d$fav <- ifelse(d$fav== "FY/KM" , "FY,KM", d$fav)
d$fav <- ifelse(d$fav== "DSRT" , "DS,RT", d$fav)
d$fav <- ifelse(d$fav== "YADT" , "YA,DT", d$fav)

d$fav <- ifelse(d$fav=="VIDEO" , "" , d$fav)
d$fav <- ifelse(d$fav=="N" , "" , d$fav)
####replace roll with Scrub

d$tech.parse <- gsub("LS" , "NN" , d$tech.parse)
d$tech.parse <- gsub("OP" , "OO" , d$tech.parse)

d$H <- ifelse ( grepl("H",d$tech.parse)==TRUE,1,0)  
d$LS <- ifelse ( grepl("NN",d$tech.parse)==TRUE,1,0)
d$OP <- ifelse ( grepl("OO",d$tech.parse)==TRUE,1,0)
d$P <- ifelse ( grepl("P",d$tech.parse)==TRUE ,1,0)
d$P <- ifelse ( grepl("PS",d$tech.parse)==TRUE ,1,d$P)
d$RO <- ifelse ( grepl("RO",d$tech.parse)==TRUE,1,0)  
d$S <- ifelse ( grepl("S",d$tech.parse)==TRUE  , 1 , 0)
d$S <- ifelse ( grepl("L",d$tech.parse)==TRUE , 1 , d$S)
d$W <- ifelse ( grepl("W",d$tech.parse)==TRUE ,1,0)  
d$PS <- ifelse ( grepl("PS",d$tech.parse)==TRUE,1,0)  


d$tech.parse <- gsub("NN" , "LS" , d$tech.parse)
d$tech.parse <- gsub("OO" , "OP" , d$tech.parse)


d$TECH <- NA
#d$TECH <-ifelse(d$H==1 & d$LS==1  d$OP==1 & d$P==0 & d$RO==0 & d$S==0 & d$W==0 , "XX" , d$TECH )
##HIT
d$TECH <-ifelse(d$H==1 & d$LS==1 & d$RO==0 & d$W==0 , "H" , d$TECH ) # if h and ls , but no w or ro then H
d$TECH <-ifelse(d$H==1 & d$S==1 & d$RO==0 & d$W==0 , "H" , d$TECH ) # if h and s , but no w or ro then H
d$TECH <-ifelse(d$H==1 & d$OP==1 & d$RO==0 & d$W==0 , "H" , d$TECH ) # if h and op , but no w or ro then H
#"LAUNDRY STYLE"
d$TECH <-ifelse(d$H==0 & d$LS==1 & d$OP==0 & d$RO==0 & d$W==0 , "LS" , d$TECH ) #if ls but no op,w,h then ls 
#OPPOSITE PISTON
d$TECH <-ifelse(d$H==0 & d$LS==0 & d$OP==1 & d$RO==0 & d$W==0 , "OP" , d$TECH ) #if op but no ls,w,h then op 
##POUND
d$TECH <-ifelse(d$H==0 & d$LS==0 & d$OP==0 & d$P==1 & d$RO==0 & d$S==0 & d$W==0 | d$PS==1 , "P" , d$TECH )
##PALM ROLL
d$TECH <-ifelse(d$RO==1 & d$W==0, "RO" , d$TECH )
##SCRUB
d$TECH <-ifelse(d$H==0 & d$LS==0 & d$OP==0 & d$RO==0 & d$S==1 & d$W==0 , "S" , d$TECH )
##WIPE
d$TECH <-ifelse(d$H==1 & d$LS==0 & d$OP==0 & d$RO==0 & d$S==0 | d$W==1 , "W" , d$TECH ) ##if only hit code as wipe, or if only wipe code as wipe
#OP/LS hybrid
d$TECH <-ifelse(d$H==0 & d$LS==1 & d$OP==1 & d$RO==0 & d$W==0 , "OP/LS" , d$TECH )

#d$tech.parse[is.na(d$TECH)]
#subset(d , is.na(d$TECH))

##in future use this to correct potential problems with coding

d_all <- d
dna <- d_all[is.na(d_all$TECH),]

d <- d[!is.na(d$TECH),]
#########extract techniues to recode by hand
for (i in 1:nrow(d)){
d$timedate[i] <- ifelse(is.na(d$timedate[i])==TRUE , d$timedate[i-1] + 0.00002 , d$timedate[i] )
}

d <- d[order(d$timedate),] #order by timedate http://www.statmethods.net/management/sorting.html

census <- read.csv("~/Dropbox/Lomas Barbudal/Sloanea Data/Demography Data/MonoGroupCensusLongform.csv")
census <- subset(census, year < 2013 )
census <- subset(census, year > 1999)
census <- census[,1:34]
trimws(census$mono)
cc <- reshape(census , varying=list(names(census)[4:34]), direction='long')
names(cc)[names(cc) == 'X1'] <- 'grouptoday'
names(cc)[names(cc) == 'time'] <- 'day'
cc$date <- as.Date(paste(cc$year , cc$month , cc$day , sep="-")  )

demo$natal <- trimws(tolower(demo$natal))
demo$resident.group <- tolower(demo$resident.group)

##grouptoday issues picj up here
d_store <- d
d<-merge(d,demo)
str(cc)

ccc<- subset(cc, select=c(mono,date,grouptoday) )
selectedRows <- (ccc$date %in% d$date )
ccc <- ccc[selectedRows,]
d$mono <- as.character(d$mono)
ccc$mono <- as.character(ccc$mono)
ccc$grouptoday <- as.character(ccc$grouptoday)

d$date <- as.Date(d$date)
ds <- merge(d,ccc, by=c("mono" , "date") , all=TRUE)
ds <- ds[!is.na(ds$TECH),]
d<-ds
names(d)[names(d) == 'grouptoday.y'] <- 'grouptoday'

####checkgrouptoday
d <- d[order(d$timedate),]
d <- subset(d , select=c(date,timedate , mono , TECH , grouptoday , natal , resident.group, mom , obs , tech.parse , switch , rav, fav , AA , BB , name, male, yob, dob ))


###WHEN NEW DATA GETS ADDED USE THIS TO CHECK FOR INCOSISTENCIES#####
dz <- subset(d, grouptoday==NA)

dz <- subset(d, grouptoday==" ")
table(dz$mono,dz$date)
###############################
d_store <- d
unique(d$grouptoday)

d$grouptoday <- gsub("M:BF-BH-BO-HE-JA-NP-TU,fl" , "fl" , d$grouptoday)
d$grouptoday <- gsub("A:NP" , "fl" , d$grouptoday)
d$grouptoday <- gsub("sp,M:DK-DS-GM-GT-HA-HC-SR-YA nm" , "lb" , d$grouptoday)
d$grouptoday <- gsub("sp,M:GM-SK" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-HA-HC-SR" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-GM-HA-HC-SK-SR" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-GM-GT-HA-HC-SK-SR" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-GM-HA-SK-SR" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-HA-HC-SK-SR" , "lb" , d$grouptoday)
d$grouptoday <- gsub("sp,M:DK-DS-GM-GT-HA-HC-SR-YA" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:GM-SK" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:GM-SK,M:GM-SK-TP" , "lb" , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-GM-HA-NP-SK-SR" , "lb" , d$grouptoday)
d$grouptoday <- gsub("lb,lb-TP" , "lb" , d$grouptoday)
d$grouptoday <- gsub("ff,rf" , "rf" , d$grouptoday)
d$grouptoday <- gsub("ff,rf" , "rf" , d$grouptoday)
d$grouptoday <- gsub("cu,ff" , "rf"  , d$grouptoday)
d$grouptoday <- gsub("M:DK-DS-HA-HC-SK-SR" , "lb"  , d$grouptoday)
d$grouptoday <- gsub("rr,M:AR-DH-PO-PP-RU-RY-SN-TD-UG-UU-WI-WM-WW" , "rr"  , d$grouptoday)
d$grouptoday <- gsub("rr,mk" , "mk"  , d$grouptoday)
d$grouptoday <- gsub("M:DJ-DK-DS-HA-HC-SK-SR" , "lb"  , d$grouptoday)
d$grouptoday <- gsub("M:BJ-NM-TP" , "mk"  , d$grouptoday)
d$grouptoday <- gsub("M:EZ-NH" , "mk"  , d$grouptoday)
d$grouptoday <- gsub("M:HC-NM,M:DK-DS-HA-HC-SK-SR" , "lb"  , d$grouptoday)
d$grouptoday <- gsub("M:HA-SK" , "lb"  , d$grouptoday)
d$grouptoday <- gsub("A:NM,M:HC-NM" , "lb"  , d$grouptoday)
d$grouptoday <- gsub("M:HC-NM,lb" , "lb"  , d$grouptoday)
d$grouptoday <- gsub("M:BE-JN-TI-TR" , "aa"  , d$grouptoday)

d$male[d$male == '0?'] <- '0'
d<- droplevels(d)

d$male <- as.integer(d$male)
d$male <- d$male - 1
 
ddd <- d
d <- d[!is.na(d$grouptoday),]

d_alltechs <- d

subset(d, grouptoday==" " ) ####Check for years not in this range later

d <- d[d$TECH!="P",]
d <- d[d$TECH!="OP/LS",]

d$TECH_i <- as.integer(as.factor(d$TECH))
d$mono_i <- as.integer(as.factor(d$mono))
d$grouptoday_i <- as.integer(as.factor(d$grouptoday))


dstore <- d
d$nonnatal <- ifelse(d$natal!=d$grouptoday & d$male==1 , 1 , 0)
sort(unique(d$mono[d$nonnatal==1]))
'%!in%' <- function(x,y)!('%in%'(x,y))
d$postmig<- ifelse(d$nonnatal==1 , 1 , 0)
d$post_rf_fizz <- ifelse(d$grouptoday %in% c("ff","rf") & d$natal=="ff" & d$yob<2007 , 1 , 0)
d$post_mk_fizz <- ifelse(d$grouptoday %in% c("rr","mk","cu") & d$natal=="rr" & d$yob<2004, 1 , 0)
d$post_fl_fizz <- ifelse(d$grouptoday %in% c("aa","fl") & d$natal=="aa" & d$yob<2003, 1 , 0)


d$post_fizz <-  d$post_rf_fizz + d$post_mk_fizz  + d$post_fl_fizz

write.csv(d , "ST2003to2012_Oct3_2018.csv")
# unique(d$mono[d$nonnatal==1])
# unique(d$mono[d$postmig==1])

# col_index <- c("violet" ,"red" , "orange" , "gold" , "green" , "blue" , "slateblue" , "black" )
# tech_index <- c(1:8)
# tech_list <- sort(unique(d$TECH))

# plot(mono_i ~ fruit_index, data= subset(d , mono %in% c("SK" , "HA" , "GM" , "SR" , "HC" , "GT" , "DK" , "DS") ) , xlim=c(12700,14770) , ylim=c(0,200) , pch=19 , col=col_index[d$TECH_i])
# plot(mono_i ~ forg_bout, data= subset(d , mono %in% c("SK" , "HA" , "GM" , "SR" , "HC" , "GT" , "DK" , "DS") ) , ylim=c(0,200) , pch=19 , col=col_index[d$TECH_i])
# plot(mono_i ~ forg_bout, data= subset(d , mono %in% c("SK" , "HA" , "GM" , "SR" , "HC" , "GT" , "DK" , "DS") ) , ylim=c(0,250) , pch=19 , col=col_index[d$TECH_i])
# plot(mono_i ~ fruit_index, data= subset(d , grouptoday %in% "ff" ) , ylim=c(0,250) , pch=19 , col=col_index[d$TECH_i])
# plot(mono_i ~ fruit_index, data= subset(d , grouptoday %in% "ff" ) , ylim=c(0,250) , pch=19 , col=col_index[d$TECH_i] , cex=0.5)
# plot(mono_i ~ fruit_index, data= subset(d , grouptoday %in% "rf" ) , ylim=c(0,250) , pch=1 , col="col_index[d$TECH_i]" , cex=0.5)

# plot(mono_i ~ forg_bout, data= subset(d , mono %in% "SK") , ylim=c(0,300) , pch=19 , col="white")
# for (i in 1:nrow(subset(d , mono %in% "SK"))){
# points(mono_i[i] ~ forg_bout[i], data= subset(d , mono %in% "SK") , ylim=c(0,300) , pch=19 , col=col_index[subset(d , mono %in% "SK")[i,] ] , cex=0.5) }
# d$TECH_i$subset(d , mono %in% "SK")[i,]




