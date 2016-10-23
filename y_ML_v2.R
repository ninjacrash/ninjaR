library("randomForest")
library("Hmisc")
library("caTools")
library("httr")
library("jsonlite")

API_URL <- "http://pg.globalhack.ninja/track_events?"
result <- GET(url=API_URL,add_headers(`Content-Type`='application/json',Prefer="count=none"))
table_to_mine <- fromJSON(content(result,"text"))

factors <- c("education","gender","reason","dependents")
suppressWarnings(table_to_mine[factors] <- lapply(table_to_mine[factors],as.factor))

table_to_mine<-table_to_mine[,c("user_id1","event_id","education","gender","reason","dependents","event_type")]

table_to_mine[table_to_mine$event_type=="entered_shelter",c("checked_at_shelter")]<-"YES"
table_to_mine[table_to_mine$event_type=="request",c("checked_at_shelter")]<-"NO"

############################
data<-table_to_mine
train_rows<-sample.split(data$checked_at_shelter, SplitRatio=0.7)
train = data[train_rows,]
test  = data[!train_rows,]

train<-train[complete.cases(train),]
test<-test[complete.cases(test),]

##Predicting Checked_at_shelter based on other variables (excluding id, date_time,shelter_checked_at)
set.seed(100)
RF<-randomForest(train[,c(3,4,5,6)],as.factor(train[[8]]),sampsize=c(1000),do.trace = TRUE, importance = TRUE,ntree=500,forest=TRUE)
print(RF)
RF$importance

names(train)
#AUC for training data
trainTarget<-as.factor(train[[8]])
train_pred<-predict(RF,train[,c(3,4,5,6)])
train_pred_number<-as.integer(train_pred)
train.c.stat<-colAUC(train_pred_number,trainTarget,plotROC = TRUE)

pred.cat<-data.frame(RF.prediction.category=predict(RF,test[,c(3,4,5,6)]))
pred.prob<-data.frame(RF.prediction=predict(RF,test[,c(3,4,5,6)],type="prob"))
pred<-cbind(test[,c("user_id1","event_id")],pred.cat,pred.prob)

###Checks
table(train$education)
table(train$gender)
table(train$reason)
table(train$dependents)

###################################
### New User Prediction
new.user<-data.frame(education="high_school",gender="f",reason="lost_job",dependents="2")
do.factors<-rbind(train[,c(3,4,5,6)],new.user)
last.row<-nrow(do.factors)
new.user.right<-do.factors[last.row,]

nu.pred.cat<-data.frame(RF.prediction.category=predict(RF,new.user.right))
nu.pred.prob<-data.frame(RF.prediction=predict(RF,new.user.right,type="prob"))
nu.user_id<-"newuser_newid"
nu.pred<-cbind(nu.user_id,nu.pred.cat,nu.pred.prob)
