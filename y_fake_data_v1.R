

#
rtc.values<-c("could not pay rent","have nowhere to sleep","lost my job","potential family violence")
my.control.rtc<-as.vector(rmultinom(1, size = 100, prob = c(0.2,0.6,0.1,0.1)))
rtc<-unlist(mapply(rep, times=my.control.rtc, rtc.values))
table(rtc)

#
gender.values<-c("M","F")
my.control.gender<-as.vector(rmultinom(1, size = 100, prob = c(0.6,0.4)))
gender<-unlist(mapply(rep, times=my.control.gender, gender.values))
table(gender)

#
ed.values<-c("some high school","high school","college")
my.control.ed<-as.vector(rmultinom(1, size = 100, prob = c(0.4,0.4,0.2)))
educ<-unlist(mapply(rep, times=my.control.ed, ed.values))
table(educ)

#
wpw.values<-c("$300","$400","bed","MISS","safety")
my.control.wpw<-as.vector(rmultinom(1, size = 100, prob = c(0.1,0.2,0.5,0.1,0.1)))
wpw<-unlist(mapply(rep, times=my.control.wpw, wpw.values))
table(wpw)

yakov.data<-data.frame(reason_to_contact=rtc,gender=gender,education=educ,what_person_wants=wpw)

