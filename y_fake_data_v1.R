ed.values<-c("some high school","high school","college")
my.control.ed<-as.vector(rmultinom(1, size = 100, prob = c(0.1,0.2,0.8)))
educ2<-unlist(mapply(rep, times=my.control.ed, ed.values))
table(educ2)
