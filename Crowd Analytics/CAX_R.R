library(dplyr)
library(glmnet)

library(ISLR)
library(leaps)


path = "C:/Users/murugesan.r/Desktop/CAX_Dataset"
setwd(path)
print(getwd())


CAX_train <- read.csv("CAX_Startup_Train.csv", as.is = T)
CAX_test <- read.csv("CAX_Startup_Test.csv", as.is = T)

CAX_train$Category <- "Train"
CAX_test$Category <- "Test"

summary(CAX_train)
str(CAX_train)


#Merging the data sets
CAX <- rbind(CAX_train, CAX_test)


col_len <- length(CAX)

#Seeing the distribution of 1's and 0'

table(CAX_train$Depen)

# Let's perfom the modelling with all the data as is in the data
# The data seems pretty Clean

acc = numeric(150)



for ( cols in 3: length(CAX_train)){
	
	
	if(!is.numeric(CAX_train[,cols]))
	{
		CAX_train[,cols] <- as.factor( CAX_train[,cols] )
	}

}


for ( cols in 3: length(CAX_test)){
	
	if(!is.numeric(CAX_test[,cols]))
	{
		CAX_test[,cols] <- as.factor( CAX_test[,cols] )
	}

}

	
	# Extra_features

	
for ( cols in 3: length(CAX_train)){
	
	if(is.numeric(CAX_train[,cols]))
	{
		CAX_train[length(CAX_train)+1] <- CAX_train[,cols]^2 
		

	}

}


for ( cols in 3: length(CAX_test)){
	
	if(is.numeric(CAX_test[,cols]))
	{
		CAX_test[length(CAX_test)+1] <- CAX_test[,cols]^2 
		
	}

}

# For loop was used to find the best seed value and the best sample size for training set

	CAX_t <- CAX_train
	CAX_t$CAX_ID <- NULL
	CAX_t$Category <- NULL

	
set.seed(70)
number = 177

if( i %in% c(44,109)){
next
}

train = sample(1:nrow(CAX_train),number)

table(CAX_train[train,]$Depen)
table(CAX_train[-train,]$Depen)


	CAX_fit <- glm(Dependent ~ ., data = CAX_t[train,], family = "binomial")
	
	table(round(predict(CAX_fit, CAX_t[-train,], type = "response")),CAX_t[-train,]$Depen)
	
	acc = table(round(predict(CAX_fit, CAX_t[-train,], type = "response"))== CAX_t[-train,]$Depen)[["TRUE"]]/ nrow(CAX_t[-train,])
	
	
	
	
	CAX_tt <- CAX_test
	CAX_tt$CAX_ID <- NULL
	CAX_tt$Dependent <- 0
	CAX_tt$Category <- NULL
	
	
	 x  =  model.matrix(Dependent ~ . , CAX_t ) [,-1]
	 x1 =  model.matrix(Dependent ~ . , CAX_tt) [,-1]
	 
	 y = as.numeric(CAX_t$Dependent)
	 
	 ridge.fit =  glmnet(x, y, alpha = 0)
	 cv.ridge.fit = cv.glmnet(x,y, alpha = 0)
	 
	 table(round(predict(cv.ridge.fit, x))==y)
     
	 preds<- round(predict(cv.ridge.fit, x1))
	
	 submission = data.frame(CAX_ID = CAX_test$CAX_ID, Dependent = preds)
	 names(submission)[names(submission) == "X1"] <- "Dependent"

	

	df.ridge.fit = data.frame(cbind(predict(cv.ridge.fit, x),y,predict(cv.ridge.fit, x)-y))
  

	
	preds<- round(predict(CAX_fit, CAX_tt, type = "response"))
	
	submission = data.frame(CAX_ID = CAX_test$CAX_ID, Dependent = preds)
	
	
	# First submission 61% accuracy
	
	# 4th Submission 68% accuracy with lasso. Target nearing
	# Let us use random forest and c forest to take the notch higher
	
	# Target 70% 
	# Try Random Forest, cforest, svm, Ridge lasso
	
	
	
	###############
	
	#library(leaps)
	
	
	
	set.seed(70)
	number = 177


	train = sample(1:nrow(CAX_train),number)

		test.mat <- model.matrix(Dependent ~., data = CAX_tt)
	
	k = 20
	accuracy <- matrix(NA,k, 50)
	train_accuracy <- matrix(NA,k, 50)
	
	folds = sample(rep(1:k, length = 234))
	
	
	
	# For k cross validation 
	for (k in 1:20){
	
    
	regit.full = regsubsets(Dependent ~., data = CAX_t[folds!=k,], method = "forward", nvmax = 50)
	
 
	for (i in 1:50)
	{
		coefi = coef(regit.full, id = i)
		train2.mat <- model.matrix(Dependent ~., data = CAX_t[folds == k,])

		pred = round(train2.mat[,names(coefi)] %*% coefi)
		
		trainall.mat <- model.matrix(Dependent ~., data = CAX_t)

		pred2 = round(trainall.mat[,names(coefi)] %*% coefi)
		
		
		
		
		
		if(any(names(table(pred == CAX_t[folds==k,]$Dependent)) %in% "TRUE"))
		{
			accuracy[k,i] = table(pred == CAX_t[folds==k,]$Dependent)[["TRUE"]]/ length(pred)
			
			train_accuracy[k,i] = table(pred2 == CAX_t$Dependent)[["TRUE"]]/ length(pred2)
			
		} else {
		accuracy[k,i] = 0
		train_accuracy[k,i] = 0
		
		}
		
		train_accuracy 
	}
	}
	
	
	plot(apply(train_accuracy,2,mean), type = "b", col = "blue", ylim = c(0.4,.8))
    matlines(apply(accuracy,2,mean), col = "red")
	
	
	
	
	print(max(accuracy))
	plot(apply(accuracy,2,mean) ,type = "b")
	print(which.max(apply(accuracy,2,mean)))
	
	
	#########  Final modeling based on the above accuracy 
	
	#trying with i = 3, 10 , 24
	
		i = 24
		k = 20
		
		
		#Selecting 24 features 
		
		ac1 <- rep(NA,20)
		ac2 <- rep(NA,20)
		
		#for(i in 1:20)
		#{
		
		i= 20
	    
		regit.full = regsubsets(Dependent ~., data = CAX_t[folds!=i,], method = "forward", nvmax = 50)
	
		coefi = coef(regit.full, id = 24)
		train2.mat <- model.matrix(Dependent ~., data = CAX_t)

		train3.mat <- model.matrix(Dependent ~., data = CAX_t[folds==i,])

		pred = round(train2.mat[,names(coefi)] %*% coefi)
		pred2 = round(train3.mat[,names(coefi)] %*% coefi)
		
		
		print(i)
		ac1[i]=table(pred == CAX_t$Dependent)[["TRUE"]]/234
		pred_output = round(test.mat[,names(coefi)] %*% coefi)
		
		ac2[i]=(table(pred2 == CAX_t[folds == i,]$Dependent)[["TRUE"]]/length(pred2))
		
		
		
		#}
	
		
	
	
	
	submission = data.frame(CAX_ID = CAX_test$CAX_ID, Dependent = pred_output)
	
	# 5th Submission 72.9% accuracy with lasso. Target nearing
	# Let us use random forest and c forest to take the notch higher
	# Used subset selection method to get the best output
	
	# Target 78% 
	# Try Random Forest, cforest, svm
	
	
	
	# Writing the csv file
	
	sub_path <- "C:/Users/murugesan.r/Desktop/CAX_Dataset/submission"
	
	write.csv(submission, paste0(sub_path,"/CAX_sub5.csv"), row.names = FALSE)
	
	