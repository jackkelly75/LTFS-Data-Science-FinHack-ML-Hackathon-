##third trial
#80/20 training/test
#selecting parameters by using interation tests


require(data.table)
require(bit64)
require(caret)
require(gbm)
require(xgboost)

##########
#set directory to that containing files
##########
setwd("C:/Users/Jack/Desktop/comp")
#setwd("/media/jack/SP PHD U3/comp")

load("loan.Rdata")
load("Xtest.Rdata")



#no need to normalise data as using XGBoost of tree method

##############
#Split dataset into testing and training subsets
#############

train_data <- loan[ , !names(loan) %in% c("loan_default")] #takes out the prediction coloumn which would train the model on a value
train_labels <- loan$loan_default

dtrain <- xgb.DMatrix(data = as.matrix(train_data), label= train_labels)



############################################
## inital testing
#############################################
cv.nround = 1000
cv.nfold = 5

mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  		objective = "binary:logistic", 
	  		eval_metric = "auc", 
	  		max_depth = 5,
	  		early_stopping_round = 50,  
	  		print_every_n = 10, 
	  		eta = 0.1,
      		gamma = 0, 
      		subsample = 0.8,
      		colsample_bytree = 0.8, 
      		min_child_weight = 1,
      		max_delta_step = 0,
      		seed = 12,
      		nthread = 4,
      		scale_pos_weight = 1)

#[421]	train-auc:0.738662+0.001067	test-auc:0.670558+0.002563

##########################################################
#### use iteration test ot find depth and child_weight  ##
##########################################################

cv.nround = 5000
cv.nfold = 5

min.value =data.frame(matrix(ncol=5,nrow=2))
colnames(min.value) <- c("depth =1", "3", "5", "7", "9")
rownames(min.value) <- c("min_child_weight = 8", "10")

min.iter =data.frame(matrix(ncol=5,nrow=2))
colnames(min.iter) <- c("depth =1", "3", "5", "7", "9")
rownames(min.iter) <- c("min_child_weight = 8", "10")

max_depth_seq <- seq(1, 10, 2)
min_child_weight_seq <- c(8, 10)

set.seed(12)
iterations  = 0
for (i in max_depth_seq){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in min_child_weight_seq){
		iterations_1 = iterations_1 + 1
		print(paste("depth = ", i, "  child  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  		objective = "binary:logistic", 
	  		eval_metric = "auc", 
	  		max_depth = i,
	  		early_stopping_round = 10,  
	  		print_every_n = 10, 
	  		eta = 0.1,
      		gamma = 0, 
      		subsample = 0.8,
      		colsample_bytree = 0.8, 
      		min_child_weight = p,
      		max_delta_step = 0,
      		seed = 12,
      		nthread = 5,
      		scale_pos_weight = 1)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
      	y <- which.max(mdcv$evaluation_log$test_auc_mean)
      	min.iter[iterations_1,iterations] <- y
	}
}

min.value
min.iter
#run with 

#                      depth =1        3         5         7         9
#min_child_weight = 1 0.6599102 0.670747 0.6701934 0.6693704 0.6662656
#min_child_weight = 3 0.6599188 0.669739 0.670838 0.6689008 0.6669428
#min_child_weight = 6 0.6597698 0.6703628 0.6707378 0.6694276 0.6682592
#min_child_weight = 8 0.6597570 0.6704492 0.6699362 0.6693802 0.6680896
#min_child_weight= 10 0.6573152 0.6709150 0.6705980 0.6701516 0.6681354

#depth =3 and min child weight = 10 is highest.

#                     depth =1   3   5   7  9
#min_child_weight = 1     1166 663 250 140 99
#min_child_weight = 3     1166 493 312 150 133
#min_child_weight = 6     1120 633 288 186 112
#min_child_weight = 8     1120 575 197 182 104
#min_child_weight= 10     684 614 296 206 112




##########################################################
#### further find exact depth and child_weights        ##
##########################################################
#We found ideal depth of 3 and child_weight of 10
#Lets go one step deeper and look for optimum values. We’ll search for values 1 above and below the optimum values because we took an interval of two.

cv.nround = 5000
cv.nfold = 5

min.value =data.frame(matrix(ncol=3,nrow=4))
colnames(min.value) <- c("depth =2", "3", "4")
rownames(min.value) <- c("min_child_weight =9", "10", "11", "12")

min.iter =data.frame(matrix(ncol=3,nrow=4))
colnames(min.iter) <- c("depth =2", "3", "4")
rownames(min.iter) <- c("min_child_weight = 9", "10", "11", "12")

max_depth_seq <- c(2, 3, 4)
min_child_weight_seq <- c(9, 10, 11, 12)

set.seed(12)
iterations  = 0
for (i in max_depth_seq){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in min_child_weight_seq){
		iterations_1 = iterations_1 + 1
		print(paste("depth = ", i, "  child  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  		objective = "binary:logistic", 
	  		eval_metric = "auc", 
	  		max_depth = i,
	  		early_stopping_round = 10,  
	  		print_every_n = 10, 
	  		eta = 0.1,
      		gamma = 0, 
      		subsample = 0.8,
      		colsample_bytree = 0.8, 
      		min_child_weight = p,
      		max_delta_step = 0,
      		seed = 12,
      		nthread = 4,
      		scale_pos_weight = 1)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
      	y <- which.max(mdcv$evaluation_log$test_auc_mean)
      	min.iter[iterations_1,iterations] <- y
	}
}

#                     depth =2         3         4
#min_child_weight =9 0.6683504 0.6697702 0.6708216
#10                  0.6679862 0.6701124 0.6713524
#11                  0.6693838 0.6708064 0.6713942
#12                  0.6692698 0.6710146 0.6713052

#still not optimsed i think
#create one with depth of 4 and 11 jsut to try







##########################
#2nd trial
##########################################################
#### use iteration test ot find depth and child_weight  ##
##########################################################

cv.nround = 5000
cv.nfold = 5

min.value =data.frame(matrix(ncol=5,nrow=6))
colnames(min.value) <- c("depth =3", "6", "8", "10", "15")
rownames(min.value) <- c("min_child_weight = 11", "13", "15", "18", "20", "25")

min.iter =data.frame(matrix(ncol=5,nrow=6))
colnames(min.iter) <- c("depth =3", "6", "8", "10", "15")
rownames(min.iter) <- c("min_child_weight = 11", "13", "15", "18", "20", "25")

max_depth_seq <- c(3, 6, 8, 10, 15)
min_child_weight_seq <- c(11, 13, 15, 17, 19, 25)

set.seed(12)
iterations  = 0
for (i in max_depth_seq){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in min_child_weight_seq){
		iterations_1 = iterations_1 + 1
		print(paste("depth = ", i, "  child  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  		objective = "binary:logistic", 
	  		eval_metric = "auc", 
	  		max_depth = i,
	  		early_stopping_round = 10,  
	  		print_every_n = 10, 
	  		eta = 0.1,
      		gamma = 0, 
      		subsample = 0.8,
      		colsample_bytree = 0.8, 
      		min_child_weight = p,
      		max_delta_step = 0,
      		seed = 12,
      		nthread = 5,
      		scale_pos_weight = 1)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
      	y <- which.max(mdcv$evaluation_log$test_auc_mean)
      	min.iter[iterations_1,iterations] <- y
	}
}


#change the tbale to what they are actually as i set the talbes up wrong

#                       depth =3         6         8        10        15
#min_child_weight = 11 0.6710502 0.6705076 0.6694430 0.6679236 0.6616564
#13                    0.6710300 0.6712726 0.6695182 0.6679162 0.6622646
#15                    0.6705608 0.6712966 0.6703392 0.6679904 0.6633202
#17                    0.6713120 0.6707848 0.6695712 0.6681988 0.6636772
#19                    0.6708978 0.6709428 0.6699800 0.6686318 0.6638036
#25                    0.6705090 0.6707012 0.6699238 0.6687686 0.6649998

#highest is 0.6713120 
#depth of 3 and min_child_weight of 17

#                      depth =3   6   8  10 15
#min_child_weight = 11      611 224 136 107 86
#13                         658 212 149 125 77
#15                         595 242 172 121 75
#17                         690 262 161 106 77
#19                         594 230 167 120 74
#25                         533 216 173 118 72


##########################################################
#### further find exact depth and child_weights        ##
##########################################################
#We found ideal depth of 3 and child_weight of 17
#Lets go one step deeper and look for optimum values. We’ll search for values 1 above and below the optimum values because we took an interval of two.


cv.nround = 5000
cv.nfold = 5

min.value =data.frame(matrix(ncol= 5,nrow=3))
colnames(min.value) <- c("depth = 1", "2", "3", "4", "5")
rownames(min.value) <- c("min_child_weight = 16", "17", "18")

min.iter =data.frame(matrix(ncol= 5,nrow=3))
colnames(min.iter) <- c("depth = 1", "2", "3", "4", "5")
rownames(min.iter) <- c("min_child_weight = 16", "17", "18")

max_depth_seq <- c(1, 2, 3, 4, 5)
min_child_weight_seq <- c(16, 17, 18)

set.seed(12)
iterations  = 0
for (i in max_depth_seq){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in min_child_weight_seq){
		iterations_1 = iterations_1 + 1
		print(paste("depth = ", i, "  child  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  		objective = "binary:logistic", 
	  		eval_metric = "auc", 
	  		max_depth = i,
	  		early_stopping_round = 10,  
	  		print_every_n = 10, 
	  		eta = 0.1,
      		gamma = 0, 
      		subsample = 0.8,
      		colsample_bytree = 0.8, 
      		min_child_weight = p,
      		max_delta_step = 0,
      		seed = 12,
      		nthread = 4,
      		scale_pos_weight = 1)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
      	y <- which.max(mdcv$evaluation_log$test_auc_mean)
      	min.iter[iterations_1,iterations] <- y
	}
}


#                      depth = 1         2         3         4         5
#min_child_weight = 16 0.6600554 0.6687950 0.6706516 0.6713970 0.6715556
#17                    0.6594074 0.6697978 0.6711632 0.6716492 0.6714238
#18                    0.6594936 0.6693012 0.6709890 0.6715008 0.6717308

#depth of 5 and min child of 18
#retry with:
#depth = 4, 5, 6,7 
#min child weight = 17, 18, 19, 20

                      depth = 4         5         6         7
min_child_weight = 17 0.6714676 0.6706174 0.6708618 0.6705446
18                    0.6710310 0.6708098 0.6719864 0.6704080
19                    0.6709586 0.6710240 0.6719852 0.6704868
20                    0.6710916 0.6711266 0.6710506 0.6711190


#best values are:
###depth = 6
###min_child_weight = 18


##########################################################
#### Tune gamma                                         ##
##########################################################
#3. Tune gamma

cv.nround = 5000
cv.nfold = 5

set.seed(12)
depth = 6
min_child_weight = 18
colsample = 0.8
subsample = 0.8
alpha = 0
lambda = 1
eta = 0.1


min.value <-rep(NA, 6)
names(min.value) <- c("0", "0.01", "0.02", "0.03", "0.04", "0.05")

gamma <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05)

set.seed(12)
iterations  = 0
for (i in gamma){
	iterations <- iterations + 1
	print(paste("gamma = ", i))
	mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  	objective = "binary:logistic", 
	  	eval_metric = "auc", 
	  	max_depth = depth,
	  	early_stopping_round = 10,  
	  	print_every_n = 10, 
	  	eta = eta,
      	gamma = i, 
      	subsample = subsample,
      	colsample_bytree = colsample, 
      	min_child_weight = min_child_weight,
      	max_delta_step = 0,
      	seed = 12,
      	nthread = 4,
      	scale_pos_weight = 1,
      	alpha = alpha,
      	lambda = lambda)
     x <- max(mdcv$evaluation_log$test_auc_mean)
     min.value[iterations] <- x
}



#tune gamma with the paramters found above
#0.0 to 0.05 in 0.1 intervals
#        0      0.01      0.02      0.03      0.04      0.05 
#0.6707920 0.6700762 0.6715024 0.6714200 0.6705108 0.6709746


#gamma = 0.02

##########################################################
#### Tune subsample and colsample_bytree                ##
##########################################################
#4. 
#The next step would be try different subsample and colsample_bytree values. Lets do this in 2 stages as well and take values 0.6,0.7,0.8,0.9 for both to start with.


cv.nround = 5000
cv.nfold = 5

set.seed(12)
depth = 6
min_child_weight = 18
alpha = 0
lambda = 1
eta = 0.1
gamma = 0.02


min.value =data.frame(matrix(ncol= 5,nrow=5))
colnames(min.value) <- c("colsample = 0.6", "0.7", "0.8", "0.9", "1.0")
rownames(min.value) <- c("subsample = 0.6", "0.7", "0.8", "0.9", "1.0")

colsample <- c(0.6,0.7,0.8,0.9, 1)
subsample <- c(0.6,0.7,0.8,0.9, 1)



set.seed(12)
iterations  = 0
for (i in colsample){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in subsample){
		iterations_1 = iterations_1 + 1
		print(paste("colsample = ", i, "  subsample  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  	objective = "binary:logistic", 
	  	eval_metric = "auc", 
	  	max_depth = depth,
	  	early_stopping_round = 10,  
	  	print_every_n = 10, 
	  	eta = eta,
      	gamma = gamma, 
      	subsample = p,
      	colsample_bytree = i, 
      	min_child_weight = min_child_weight,
      	max_delta_step = 0,
      	seed = 12,
      	nthread = 6,
      	scale_pos_weight = 1,
      	alpha = alpha,
      	lambda = lambda)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
	}
}

#                colsample = 0.6       0.7       0.8       0.9       1.0
#subsample = 0.6       0.6703012 0.6704794 0.6706972 0.6696446 0.6696794
#0.7                   0.6695158 0.6705866 0.6706204 0.6702778 0.6701764
#0.8                   0.6713876 0.6712038 0.6707062 0.6716058 0.6711730
#0.9                   0.6709354 0.6714518 0.6709608 0.6714990 0.6711544
#1.0                   0.6706918 0.6707116 0.6709712 0.6711216 0.6698840

#colsample = 0.9
#subsample = 0.8

##########################################################
#### Further tune subsample and colsample_bytree        ##
##########################################################
#after found optimum value for both subsample and colsample_bytree. Now we should try values in 0.05 interval around these.



cv.nround = 5000
cv.nfold = 5

set.seed(12)
depth = 6
min_child_weight = 18
alpha = 0
lambda = 1
eta = 0.1
gamma = 0.02


min.value =data.frame(matrix(ncol= 3,nrow=3))
colnames(min.value) <- c("colsample = 0.85", "0.9", "0.95")
rownames(min.value) <- c("subsample = 0.75", "0.8", "0.85")

colsample <- c(0.85, 0.90, 0.95)
subsample <- c(0.75, 0.8, 0.85)



set.seed(12)
iterations  = 0
for (i in colsample){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in subsample){
		iterations_1 = iterations_1 + 1
		print(paste("colsample = ", i, "  subsample  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  	objective = "binary:logistic", 
	  	eval_metric = "auc", 
	  	max_depth = depth,
	  	early_stopping_round = 10,  
	  	print_every_n = 10, 
	  	eta = eta,
      	gamma = gamma, 
      	subsample = p,
      	colsample_bytree = i, 
      	min_child_weight = min_child_weight,
      	max_delta_step = 0,
      	seed = 12,
      	nthread = 6,
      	scale_pos_weight = 1,
      	alpha = alpha,
      	lambda = lambda)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
	}
}

#                 colsample = 0.85       0.9      0.95
#subsample = 0.75        0.6708992 0.6714052 0.6708064
#0.8                     0.6707944 0.6711354 0.6716004
#0.85                    0.6714492 0.6711112 0.6711654

#max auc is colsample 0.95 and subsample 0.8


##########################################################
#### Tuning Regularization Parameters                   ##
##########################################################
#5 
#Next step is to apply regularization to reduce overfitting. Though many people don’t use this parameters much as gamma provides a substantial way of controlling complexity. But we should always try it. I’ll tune ‘alpha’ value here and leave it upto you to try different values of ‘lambda’.
#alpha - 0, 0.00001, 0.01, 1 (default 0)
#lambda - 0, 0.00001, 0.01, 1, 1.01, 1.1 (default 1)


cv.nround = 5000
cv.nfold = 5

set.seed(12)
depth = 6
min_child_weight = 18
eta = 0.1
gamma = 0.02
colsample = 0.95
subsample = 0.8

min.value =data.frame(matrix(ncol= 4,nrow=6))
colnames(min.value) <- c("alpha =0", "0.00001", "0.01", "1")
rownames(min.value) <- c("lambda = 0", "0.00001", "0.01", "1", "1.01", "1.1")


alpha = c(0, 0.00001, 0.01, 1)
lambda = c(0, 0.00001, 0.01, 1, 1.01, 1.1)

set.seed(12)
iterations  = 0
for (i in alpha){
	iterations <- iterations + 1
	iterations_1 = 0
	for(p in lambda){
		iterations_1 = iterations_1 + 1
		print(paste("alpha = ", i, "  lambda  = ",p , sep = ""))
		mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  	objective = "binary:logistic", 
	  	eval_metric = "auc", 
	  	max_depth = depth,
	  	early_stopping_round = 10,  
	  	print_every_n = 10, 
	  	eta = eta,
      	gamma = gamma, 
      	subsample = subsample,
      	colsample_bytree = colsample, 
      	min_child_weight = min_child_weight,
      	max_delta_step = 0,
      	seed = 12,
      	nthread = 6,
      	scale_pos_weight = 1,
      	alpha = i,
      	lambda = p)
      	x <- max(mdcv$evaluation_log$test_auc_mean)
      	min.value[iterations_1,iterations] <- x
	}
}

#            alpha =0   0.00001      0.01         1
#lambda = 0 0.6710818 0.6705370 0.6711768 0.6709658
#0.00001    0.6714498 0.6710936 0.6709252 0.6710572
#0.01       0.6710714 0.6708120 0.6709352 0.6707434
#1          0.6712920 0.6715328 0.6706362 0.6711750
#1.01       0.6716594 0.6699576 0.6703462 0.6710844
#1.1        0.6707992 0.6718102 0.6710896 0.6716468


#retry with
alpha = c(0.00001)
lambda = c(1.08, 1.1, 1.15)

min.value =data.frame(matrix(ncol= 1,nrow=3))
colnames(min.value) <- c("alpha = 0.00001")
rownames(min.value) <- c("lambda = 1.08", "1.1", "1.15")


#              alpha = 0.00001
#lambda = 1.08       0.6710780
#1.1                 0.6714938
#1.15                0.6708264


#alpha = 0.00001
#lambda = 1.1

##########################################################
#### Reducing Learning Rate                             ##
##########################################################
#Step 6: Reducing Learning Rate
#Lastly, we should lower the learning rate and add more trees. Lets use the cv function of XGBoost to do the job again.
#eta is learning rate
#eta - 0.1, 0.01, 0.001



cv.nround = 5000
cv.nfold = 5

set.seed(12)
depth = 6
min_child_weight = 18
gamma = 0.02
colsample = 0.95
subsample = 0.8
alpha = 0.00001
lambda = 1.1


min.value <-rep(NA, 3)
names(min.value) <- c("0.1", "0.01", "0.001")

eta <- c(0.1, 0.01, 0.001)

set.seed(12)
iterations  = 0
for (i in eta){
	iterations <- iterations + 1
	print(paste("eta = ", i))
	mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  	objective = "binary:logistic", 
	  	eval_metric = "auc", 
	  	max_depth = depth,
	  	early_stopping_round = 10,  
	  	print_every_n = 10, 
	  	eta = i,
      	gamma = gamma, 
      	subsample = subsample,
      	colsample_bytree = colsample, 
      	min_child_weight = min_child_weight,
      	max_delta_step = 0,
      	seed = 12,
      	nthread = 4,
      	scale_pos_weight = 1,
      	alpha = alpha,
      	lambda = lambda)
     x <- max(mdcv$evaluation_log$test_auc_mean)
     min.value[iterations] <- x
}


#      0.1      0.01     0.001 
# 0.6707482 0.6731906 0.6368974 

#eta = 0.01


##############################
#get nrounds
#############################

cv.nround = 5000
cv.nfold = 5

set.seed(12)
depth = 6
min_child_weight = 18
gamma = 0.02
colsample = 0.95
subsample = 0.8
alpha = 0.00001
lambda = 1.1
eta = 0.01

mdcv <- xgb.cv(data = dtrain, nfold = cv.nfold, nround = cv.nround,
	  	objective = "binary:logistic", 
	  	eval_metric = "auc", 
	  	max_depth = depth,
	  	early_stopping_round = 10,  
	  	print_every_n = 10, 
	  	eta = eta,
      	gamma = gamma, 
      	subsample = subsample,
      	colsample_bytree = colsample, 
      	min_child_weight = min_child_weight,
      	max_delta_step = 0,
      	seed = 12,
      	nthread = 6,
      	scale_pos_weight = 1,
      	alpha = alpha,
      	lambda = lambda)


#2319 rouneds




##########################
#xgboost (default)
##########################
#train model using xgboost and parameters found

set.seed(12)
depth = 6
min_child_weight = 18
gamma = 0.02
colsample = 0.95
subsample = 0.8
alpha = 0.00001
lambda = 1.1
eta = 0.01
nround = 2319



model <- xgb.train(data = dtrain, nround = nround,
	  		objective = "binary:logistic", 
	  		print_every_n = 10, 
	  		eta = eta,
      		depth = depth,
      		gamma = gamma, 
      		subsample = subsample,
      		colsample_bytree = colsample, 
      		min_child_weight = min_child_weight,
      		max_delta_step = 0,
      		seed = 12, 
      		verbose = 2,
      		nthread = 4,
      		scale_pos_weight = 1)




# generate predictions for our held-out testing data
pred <- predict(model, dtest)


residuals = test_labels - (as.numeric(pred > 0.5))
RMSE = sqrt(mean(residuals ^2 ))
print(paste0("RMSE = ", round(RMSE,4)))

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))

# get information on how important each feature is
importance_matrix <- xgb.importance(names(train_data), model = model)
# and plot it!
xgb.plot.importance(importance_matrix)

xgbpred <- ifelse (pred > 0.5,1,0)
confusionMatrix (as.factor(xgbpred), as.factor(test_labels))
#gives a accuracy


##########################
#perform on test dataset
##########################

Xtest <- Xtest[,model$feature_names]
pred <- predict(model, as.matrix(Xtest))

xgbpred <- ifelse (pred > 0.5,1,0)

sub <- read.csv('sample_submission.csv', header=T)
sub$loan_default <- xgbpred

saveFileName <- paste('./submission_',
                          '[XGBoost]_',
                          '[binary,logistic]',
                          '[depth  ', depth, ', child ', min_child_weight, ']',
                          '[colsample ', colsample , 'subsample ', subsample, ']',
                          '[gamma ', gamma, ']',
                          '[alpha ', alpha, ' lambda ', lambda, ']',
                          '[eta ', eta, ']',
                          '[5 fold CV]',
                          '[run ', nround, " times]", 
                          '.csv', sep='')


write.csv(sub, saveFileName, row.names=F, quote=FALSE)

