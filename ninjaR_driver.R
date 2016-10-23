source("ninjaR_utils.R")
packagedownloader('r-requirements.txt')
repo_sourcer()

reproduction_seed <- set.seed(1)
API_URL <- "http://pg.globalhack.ninja/track_events?"
result <- GET(url=API_URL,add_headers(`Content-Type`='application/json',Prefer="count=none"))
shelter_table <- fromJSON(content(result,"text"))
factors <- c("education","gender","reason","dependents")
suppressWarnings(shelter_table[factors] <- lapply(shelter_table[factors],as.factor))
shelter_table[shelter_table$event_type=="entered_shelter",c("checked_at_shelter")]<-"YES"
shelter_table[shelter_table$event_type=="request",c("checked_at_shelter")]<-"NO"
preds.RF <- get_prediction_matrix.RF(shelter_table, seed = reproduction_seed)
# preds.GLM <- get_prediction_matrix.GLM(data, seed = reproduction_seed)

new.user<-data.frame(reason_to_contact="have nowhere to sleep",gender="F",education="high school",what_person_wants="bed")
do.factors<-rbind(train[,-c(1,6,7,8,9)],new.user)
last.row<-nrow(do.factors)
new.user.right<-do.factors[last.row,]

nu.pred.cat<-data.frame(RF.prediction.category=predict(RF,new.user.right))
nu.pred.prob<-data.frame(RF.prediction=predict(RF,new.user.right,type="prob"))
nu.user_id<-"newuser_newid"
nu.pred<-cbind(nu.user_id,nu.pred.cat,nu.pred.prob)
