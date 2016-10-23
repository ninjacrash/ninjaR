sink("prediction_log.txt")
library(jsonlite)
library("randomForest")

load("RF_model.RData")
load("training_data.RData")
train <- t.to.keep

args <- commandArgs(trailingOnly = TRUE)
#test.person <- "{\"education\":\"high_school\",\"gender\":\"f\",\"reason\":\"lost_job\",\"dependents\":\"2\"}"
test.person <- args[[1]]

new.user<-data.frame(fromJSON(test.person))
do.factors<-rbind(t.to.keep,new.user)
last.row<-nrow(do.factors)
new.user.right<-do.factors[last.row,]

nu.pred.cat<-data.frame(RF.prediction.category=predict(RF,new.user.right))
nu.pred.prob<-data.frame(RF.prediction=predict(RF,new.user.right,type="prob"))
nu.user_id<-"newuser_newid"
nu.pred<-cbind(nu.user_id,nu.pred.cat,nu.pred.prob)
sink()
print(toJSON(nu.pred))
