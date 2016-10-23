
library("randomForest")
library("Hmisc")
library("caTools")

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

source("generate_data.R")
data <- generate_data(size = 100)
names(data)
str(data)

set.seed(123)
train_smp_size<-70
train_ind <- sample(seq_len(nrow(data)), size = train_smp_size)

train <- data[train_ind,]
test <- data[-train_ind,]

train<-train[complete.cases(train),]
test<-test[complete.cases(test),]

##Predicting Checked_at_shelter based on other variables (excluding id, date_time,shelter_checked_at)
set.seed(100)
RF<-randomForest(train[,-c(1,6,7,8,9)],as.factor(train[[9]]),sampsize=c(30),do.trace = TRUE, importance = TRUE,ntree=500,forest=TRUE)
print(RF)
RF$importance

#### trying glm
#train[train$checked_at_shelter=="Yes",c("nice_depvar")]<-1
#train[train$checked_at_shelter=="No",c("nice_depvar")]<-0
#LR <- glm.fit(train[,-c(1,6,7,8,9,10)],as.vector(train[,c("nice_depvar")]),family=binomial(link='logit'))
#LR <- glm(nice_depvar~reason_to_contact+gender+education+what_person_wants,family=binomial(link='logit'),data=train)

names(train[,-c(1,6,7,8,9,10)])


#AUC for training data
trainTarget<-as.factor(train[[9]])
train_pred<-predict(RF,train[,-c(1,6,7,8,9)])
train_pred_number<-as.integer(train_pred)
train.c.stat<-colAUC(train_pred_number,trainTarget,plotROC = TRUE)

trainTarget<-as.factor(train[[9]])
train_pred<-predict(LR,train[,-c(1,6,7,8,9,10)])
train_pred_number<-as.integer(train_pred)
train.c.stat<-colAUC(train_pred_number,trainTarget,plotROC = TRUE)

pred.cat<-data.frame(RF.prediction.category=predict(RF,test[,-c(1,6,7,8,9)]))
pred.prob<-data.frame(RF.prediction=predict(RF,test[,-c(1,6,7,8,9)],type="prob"))
user_id<-test[,c("user_id")]
pred<-cbind(user_id,pred.cat,pred.prob)

### New User Prediction
new.user<-data.frame(reason_to_contact="have nowhere to sleep",gender="F",education="high school",what_person_wants="bed")
do.factors<-rbind(train[,-c(1,6,7,8,9)],new.user)
last.row<-nrow(do.factors)
new.user.right<-do.factors[last.row,]

nu.pred.cat<-data.frame(RF.prediction.category=predict(RF,new.user.right))
nu.pred.prob<-data.frame(RF.prediction=predict(RF,new.user.right,type="prob"))
nu.user_id<-"newuser_newid"
nu.pred<-cbind(nu.user_id,nu.pred.cat,nu.pred.prob)
