divide_data <- function(data, seed = 1, train_percent = 0.7){
  # Function to partition data into train and test set
  set.seed(seed = seed)
  train_smp_size <- round(nrow(data) * train_percent)
  train_ind <- sample(seq_len(nrow(data)), size = train_smp_size)
  
  train <- data[train_ind,]
  test <- data[-train_ind,]
  
  list('train' = train, 'test' = test)
}

get_prediction_matrix.RF <- function(data, seed = 1){
  # set.seed(123)
  # Remove these cols after investigating data
  ignore.cols <- c(2, 6, 7, 8, 9, 10)
  dependent.variable <- 6
  
  partitioned_data <- divide_data(data[, -ignore.cols])
  train <- partitioned_data$train
  test <- partitioned_data$test
  # @deep Refactor here start
  # hold.me.out <- list('train' = train[, c(1, 4)], 'test' = test[, c(1, 4)])
  train <- train[complete.cases(train),]
  test <- test[complete.cases(test),]
  id.variable <- 5
  # train.user_id <- train[, 1]
  # test.user_id <- test[, 1]
  # hold.me.out$train <- hold.me.out$train[hold.me.out$train[, 1] %in% train.user_id, 2]
  # hold.me.out$test <- hold.me.out$test[hold.me.out$test[, 1] %in% test.user_id, 2]
  # train <- cbind(train[, c(1:4)], hold.me.out$train, train[, 5:6])  
  # test <- cbind(test[, c(1:4)], hold.me.out$test, test[, 5:6]) 
  # @deep Refactor here end
  
  ##Predicting Checked_at_shelter based on other variables (excluding id, date_time,shelter_checked_at)
  # set.seed(100)
  RF<-randomForest(train[,-c(dependent.variable, id.variable)],
                   as.factor(train[[dependent.variable]]),
                   sampsize=c(300), 
                   do.trace = TRUE, 
                   importance = TRUE, 
                   ntree=500, 
                   forest=TRUE)
  print(RF)
  RF$importance
  model.file = './data/prediction_model.RData'
  save(RF, file = model.file)
  print(paste('Model saved as', model.file, collapse = " "))
  #AUC for training data
  trainTarget<-as.factor(train[[dependent.variable]])
  train_pred<-predict(RF,train[,-c(dependent.variable, id.variable)])
  train_pred_number<-as.integer(train_pred)
  train.c.stat<-colAUC(train_pred_number,trainTarget,plotROC = TRUE)
  pred.cat<-data.frame(RF.prediction.category=predict(RF,test[,-c(dependent.variable, id.variable)]))
  pred.prob<-data.frame(RF.prediction=predict(RF,test[,-c(dependent.variable, id.variable)],type="prob"))
  user_id<-test[,id.variable]
  pred<-cbind(user_id,pred.cat,pred.prob) 
  pred
}

get_prediction_matrix.LM <- function(data, seed = 1){
  # set.seed(123)
  ignore.cols <- c(2, 6, 7, 8, 9, 10)
  dependent.variable <- 6

  partitioned_data <- divide_data(data[, -ignore.cols])
  train <- partitioned_data$train
  test <- partitioned_data$test
  # @deep Refactor here start
  # hold.me.out <- list('train' = train[, c(1, 4)], 'test' = test[, c(1, 4)])
  train <- train[complete.cases(train),]
  test <- test[complete.cases(test),]
  id.variable <- 5
  # hold.me.out$train <- hold.me.out$train[hold.me.out$train[, 1] %in% train.user_id, 2]
  # hold.me.out$test <- hold.me.out$test[hold.me.out$test[, 1] %in% test.user_id, 2]
  # train <- cbind(train[, c(1:4)], hold.me.out$train, train[, 6:9])  
  # test <- cbind(test[, c(1:4)], hold.me.out$test, test[, 6:9]) 
  # @deep Refactor here end
  
  ##Predicting Checked_at_shelter based on other variables (excluding id, date_time,shelter_checked_at)
  # set.seed(100)
  train[,dependent.variable] <- as.factor(train[,dependent.variable])
  vglm.formula <- paste(c(names(train[dependent.variable]), paste(names(train)[-c(dependent.variable, id.variable)], collapse = " + ")), collapse = ' ~ ')
  LM <- vglm(formula = vglm.formula, x = train[,-c(id.variable, dependent.variable)], y = train[, dependent.variable], family = binomialff)
  print(LM)
  LM
  LM_prediction <- 1 - predict(LM, test, type="response")
  LM_prediction <- (LM_prediction-min(LM_prediction))/(max(LM_prediction)-min(LM_prediction))
  
  true_value <- train[, dependent.variable]
  output <- data.frame(true_value,LR_prediction)
  # RF<-randomForest(train[,-c(1,5,7,8,9)],as.factor(train[[9]]),sampsize=c(30),do.trace = TRUE, importance = TRUE,ntree=500,forest=TRUE)
  # print()
  # RF$importance
  # 
  # #AUC for training data
  # trainTarget<-as.factor(train[[9]])
  # train_pred<-predict(RF,train[,-c(1,5,7,8,9)])
  # train_pred_number<-as.integer(train_pred)
  # train.c.stat<-colAUC(train_pred_number,trainTarget,plotROC = TRUE)
  # 
  # pred.cat<-data.frame(RF.prediction.category=predict(RF,test[,-c(1,5,7,8,9)]))
  # pred.prob<-data.frame(RF.prediction=predict(RF,test[,-c(1,5,7,8,9)],type="prob"))
  # user_id<-test[,c("user_id")]
  # pred<-cbind(user_id,pred.cat,pred.prob) 
  # pred
}



