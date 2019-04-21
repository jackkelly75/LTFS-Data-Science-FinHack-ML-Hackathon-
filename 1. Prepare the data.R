########################################
## prepare the data for analysis      ##
## https://github.com/jackkelly75     ##
########################################

rm(list=ls(all=TRUE))
gc(reset=T)

############
#load required packages
############
require(data.table)
require(bit64)
require(caret)
require(gbm)
library(xgboost)

##########
#set directory to that containing files
##########
setwd("C:/Users/Jack/Desktop/comp")
#setwd("/media/jack/SP PHD U3/comp")

##########
#load training and testing data
##########
# fread is much faster than read.csv
loan <- as.data.frame(fread("train.csv", header=TRUE, sep=","))   #fread from data.table - Similar to read.table but faster and more convenient.
Xtest <- as.data.frame(fread("test.csv", header=TRUE, sep=","))


###################################
## sort train                    ##
###################################

################
#convert data to be usable
################
###loan$Date.of.Birth
##don't have time to be extra carful, just approach on a by year basis
x <- substr(loan$Date.of.Birth, 7, 8 )
#newest age is 00.
for(i in 1:length(x)){
	if(x[i] == "00"){
		x[i] <- "2000"
	} else {
	x[i] <- paste("19",x[i], sep = "")
	}
}
x <- 2019 - as.numeric(x)
loan$Date.of.Birth <- x
colnames(loan)[9] <- "Age"

#Employment.Type
####three levels - "salaried", "self employed", "".
#salariad = 0
#self employed = 1
#"" = "NA"
loan$Employment.Type <- gsub("Salaried", 0, loan$Employment.Type)
loan$Employment.Type <- gsub("Self employed", 1, loan$Employment.Type)
loan$Employment.Type <- gsub("^$|^ $", NA, loan$Employment.Type)

##DisbursalDate - data of purchase # check in test data
unique(substr(loan$DisbursalDate, 7, 8 )) #all are from 2018
unique(substr(loan$DisbursalDate, 4, 5))  #all are from aug, sep or oct
#just get length from data of disbursal to end of nov (as nov is used in test). (october has 31, september has 30 days)
x <- loan$DisbursalDate
x <- substr(x, 1, 5)
x <-gsub("-10", "-30"  , x)
x <- gsub("-09", "-61"  , x)
x <- gsub("-08", "-91"  , x)
x <- (31 - as.numeric(substr(x, 1, 2))) + (as.numeric(substr(x, 4, 5 ))) #will be a day or two out as assuming 31 days in month at this point
loan[,11] <- x

#PERFORM_CNS.SCORE.DESCRIPTION - shows that a 0 on PERFORM_CNS.SCORE means no data availble
#remove this colomn as the PERFORM_CNS.SCORE
#in PERFORM_CNS.SCORE turn 0 to NA
loan$PERFORM_CNS.SCORE.DESCRIPTION <- NULL
loan$PERFORM_CNS.SCORE[loan$PERFORM_CNS.SCORE == 0] <- NA

#AVERAGE.ACCT.AGE
x <- loan$AVERAGE.ACCT.AGE
mon <- gsub(".* ","",x)
mon <- as.numeric(gsub("mon","",mon))
year <- sub(" .*", "", x)
year <- sub("yrs", "", year)
year <- as.numeric(year) * 12
loan$AVERAGE.ACCT.AGE <- year + mon

#CREDIT.HISTORY.LENGTH
x <- loan$CREDIT.HISTORY.LENGTH
mon <- gsub(".* ","",x)
mon <- as.numeric(gsub("mon","",mon))
year <- sub(" .*", "", x)
year <- sub("yrs", "", year)
year <- as.numeric(year) * 12
loan$CREDIT.HISTORY.LENGTH <- year + mon


################################################
#sort test
################################################

################
#convert data to be usable
################
###Xtest$Date.of.Birth
##don't have time to be extra carful, just approach on a by year basis
x <- substr(Xtest$Date.of.Birth, 7, 8 )
#newest age is 00.
for(i in 1:length(x)){
	if(x[i] == "00"){
		x[i] <- "2000"
	} else {
	x[i] <- paste("19",x[i], sep = "")
	}
}
x <- 2019 - as.numeric(x)
Xtest$Date.of.Birth <- x
colnames(Xtest)[9] <- "Age"

#Employment.Type
####three levels - "salaried", "self employed", "".
#salariad = 0
#self employed = 1
#"" = "NA"
Xtest$Employment.Type <- gsub("Salaried", 0, Xtest$Employment.Type)
Xtest$Employment.Type <- gsub("Self employed", 1, Xtest$Employment.Type)
Xtest$Employment.Type <- gsub("^$|^ $", NA, Xtest$Employment.Type)

##DisbursalDate - data of purchase # check in test data
unique(substr(Xtest$DisbursalDate, 7, 8 )) #all are from 2018
unique(substr(Xtest$DisbursalDate, 4, 5))  #all are from aug, sep or oct
#just get length from data of disbursal to end of nov (as nov is used in test). (october has 31, september has 30 days)
x <- Xtest$DisbursalDate
x <- substr(x, 1, 5)
x <-gsub("-10", "-30"  , x)
x <- gsub("-09", "-61"  , x)
x <- gsub("-08", "-91"  , x)
x <- (31 - as.numeric(substr(x, 1, 2))) + (as.numeric(substr(x, 4, 5 ))) #will be a day or two out as assuming 31 days in month at this point
Xtest[,11] <- x

#PERFORM_CNS.SCORE.DESCRIPTION - shows that a 0 on PERFORM_CNS.SCORE means no data availble
#remove this colomn as the PERFORM_CNS.SCORE
#in PERFORM_CNS.SCORE turn 0 to NA
Xtest$PERFORM_CNS.SCORE.DESCRIPTION <- NULL
Xtest$PERFORM_CNS.SCORE[Xtest$PERFORM_CNS.SCORE == 0] <- NA

#AVERAGE.ACCT.AGE
x <- Xtest$AVERAGE.ACCT.AGE
mon <- gsub(".* ","",x)
mon <- as.numeric(gsub("mon","",mon))
year <- sub(" .*", "", x)
year <- sub("yrs", "", year)
year <- as.numeric(year) * 12
Xtest$AVERAGE.ACCT.AGE <- year + mon

#CREDIT.HISTORY.LENGTH
x <- Xtest$CREDIT.HISTORY.LENGTH
mon <- gsub(".* ","",x)
mon <- as.numeric(gsub("mon","",mon))
year <- sub(" .*", "", x)
year <- sub("yrs", "", year)
year <- as.numeric(year) * 12
Xtest$CREDIT.HISTORY.LENGTH <- year + mon



#############
#convert it to the numeric version
#############
Xtest <- apply(Xtest, 2, function(x) as.numeric(x))
loan <- apply(loan, 2, function(x) as.numeric(x))



######################
## Impute Function  ##
######################

#### This function imputes each feature with some constant values
Impute <- function(data, value){
  num <- apply(data, 2, function(x) sum(is.na(x)))
  data <- as.matrix(data)
  data[which(is.na(data))] <- rep(value, num)
  data <- as.data.frame(data)
  return(data)
}

########################
#Impute data
#########################
#### missing data imputation
med <- apply(loan, 2, median, na.rm=TRUE)
loan <- Impute(loan, med)
Xtest <- Impute(Xtest, med[1:dim(Xtest)[2]])




###################################
#remove any variables with low variance
###################################

#nearZeroVar(loan, names= T)

#"MobileNo_Avl_Flag"
#"Driving_flag"
#"Passport_flag"
#"SEC.NO.OF.ACCTS"
#"SEC.ACTIVE.ACCTS"
#"SEC.OVERDUE.ACCTS"    
#"SEC.CURRENT.BALANCE"
#"SEC.SANCTIONED.AMOUNT"
#"SEC.DISBURSED.AMOUNT"
#"SEC.INSTAL.AMT"
#"AVERAGE.ACCT.AGE"
#"CREDIT.HISTORY.LENGTH"


#nearZeroVar(loan, names= T, freqCut = 2000)

#"MobileNo_Avl_Flag"
#"SEC.CURRENT.BALANCE"
#"SEC.SANCTIONED.AMOUNT"
#"SEC.DISBURSED.AMOUNT"
#"SEC.INSTAL.AMT"


#############################
#remove what i condider redundant information
#############################
#very very low variance
Xtest$MobileNo_Avl_Flag <- NULL
loan$MobileNo_Avl_Flag <- NULL
Xtest$SEC.CURRENT.BALANCE <- NULL
loan$SEC.CURRENT.BALANCE <- NULL
Xtest$SEC.SANCTIONED.AMOUNT <- NULL
loan$SEC.SANCTIONED.AMOUNT <- NULL
Xtest$SEC.INSTAL.AMT <- NULL
loan$SEC.INSTAL.AMT <- NULL


#not so low but low variance

Xtest$Driving_flag <- NULL
loan$Driving_flag <- NULL
Xtest$Passport_flag <- NULL
loan$Passport_flag <- NULL

Xtest$SEC.NO.OF.ACCTS <- NULL
loan$SEC.NO.OF.ACCTS <- NULL
Xtest$SEC.ACTIVE.ACCTS <- NULL
loan$SEC.ACTIVE.ACCTS <- NULL
Xtest$SEC.OVERDUE.ACCTS <- NULL
loan$SEC.OVERDUE.ACCTS <- NULL
Xtest$SEC.DISBURSED.AMOUNT <- NULL
loan$SEC.DISBURSED.AMOUNT <- NULL
Xtest$AVERAGE.ACCT.AGE <- NULL
loan$AVERAGE.ACCT.AGE <- NULL
Xtest$CREDIT.HISTORY.LENGTH <- NULL
loan$CREDIT.HISTORY.LENGTH <- NULL

#might be important, however is so many options that they can't be one hot encoded by my laptop
loan$supplier_id <- NULL
Xtest$supplier_id <- NULL
loan$Employee_code_ID <- NULL ##employee Id may be a factor, but there are a large amount of them
Xtest$Employee_code_ID <- NULL


##########################
## redundant by what they are
#########################
loan$UniqueID <- NULL
Xtest$UniqueID <- NULL
loan$PERFORM_CNS.SCORE.DESCRIPTION <- NULL
Xtest$PERFORM_CNS.SCORE.DESCRIPTION <- NULL



############################
## one hot encoding catagorical variables
############################

######################
## manufactureing ID
######################
##only use the IDs that are simliar between datasets
x <- intersect(unique(loan$manufacturer_id), unique(Xtest$manufacturer_id))

for(unique_value in x){
	loan[paste("manufacturer_id", unique_value, sep = ".")] <- ifelse(loan$manufacturer_id == unique_value, 1, 0)
}
for(unique_value in x){
	Xtest[paste("manufacturer_id", unique_value, sep = ".")] <- ifelse(Xtest$manufacturer_id == unique_value, 1, 0)
}
loan$manufacturer_id <- NULL
Xtest$manufacturer_id <- NULL


######################
## same with branch_id
######################
##only use the IDs that are simliar between datasets
x <- intersect(unique(loan$branch_id), unique(Xtest$branch_id))

for(unique_value in unique(loan$branch_id)){
	loan[paste("branch_id", unique_value, sep = ".")] <- ifelse(loan$branch_id == unique_value, 1, 0)
}
for(unique_value in unique(Xtest$branch_id)){
	Xtest[paste("branch_id", unique_value, sep = ".")] <- ifelse(Xtest$branch_id == unique_value, 1, 0)
}
loan$branch_id <- NULL
Xtest$branch_id <- NULL




##############################
##save data                 ##
##############################

save(loan, file = "loan.Rdata")
save(Xtest, file = "Xtest.Rdata")
