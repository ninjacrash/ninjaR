
library("randomForest")
library("Hmisc")
library("caTools")

source("generate_data.R")

divide_data <- function(data, seed = 1, train_percent = 0.7){
  # Function to partition data into train and test set
  set.seed(seed = seed)
  train_smp_size <- round(nrow(data) * train_percent)
  train_ind <- sample(seq_len(nrow(data)), size = train_smp_size)
  
  train <- data[train_ind,]
  test <- data[-train_ind,]
  
  train<-train[complete.cases(train),]
  test<-test[complete.cases(test),]
  list('train' = train, 'test' = test)
}

get_prediction_matrix.RF <- function(data, seed = 1){
  # set.seed(123)
  partitioned_data <- divide_data(data[, -5])
  train <- partitioned_data$train
  test <- partitioned_data$test
  
  ##Predicting Checked_at_shelter based on other variables (excluding id, date_time,shelter_checked_at)
  # set.seed(100)
  RF<-randomForest(train[,-c(1,7,8,9)],as.factor(train[[9]]),sampsize=c(30),do.trace = TRUE, importance = TRUE,ntree=500,forest=TRUE)
  print(RF)
  RF$importance
  
  #AUC for training data
  trainTarget<-as.factor(train[[9]])
  train_pred<-predict(RF,train[,-c(1,7,8,9)])
  train_pred_number<-as.integer(train_pred)
  train.c.stat<-colAUC(train_pred_number,trainTarget,plotROC = TRUE)
  
  pred.cat<-data.frame(RF.prediction.category=predict(RF,test[,-c(1,7,8,9)]))
  pred.prob<-data.frame(RF.prediction=predict(RF,test[,-c(1,7,8,9)],type="prob"))
  user_id<-test[,c("user_id")]
  pred<-cbind(user_id,pred.cat,pred.prob) 
  pred
}

