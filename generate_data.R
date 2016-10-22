generate_data <- function(size, seed = 1, time_diff_hours = 5, outlier_prob = seq(0.05, 0.1, by = 0.01)){
  # Function generates a data frame of given size
  
  ##########################################################
  # Hard coded variables, can be used as command line params
  ##########################################################
  set.seed(seed)
  DATA_SIZE <- size
  TIME_DIFF <- time_diff_hours
  OUTLIER_PROBABILITIES <- outlier_prob
  
  ##########################################################
  # Generating features for dataset
  ##########################################################
  
  # Generating user_id
  u.id<-stringi::stri_rand_strings(DATA_SIZE, 6)
  for (i in 1:30) {
    u.id[i]<-paste(sample(0:9,7,replace=TRUE), collapse="" )
  }
  
  # Generating reason_to_contact
  rtc.values<-c("could not pay rent","have nowhere to sleep","lost my job","potential family violence")
  my.control.rtc<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.2,0.6,0.1,0.1)))
  rtc<-unlist(mapply(rep, times=my.control.rtc, rtc.values))
  
  # Generating gender
  gender.values<-c("M","F")
  my.control.gender<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.6,0.4)))
  gender<-unlist(mapply(rep, times=my.control.gender, gender.values))
  
  # Generating education
  ed.values<-c("some high school","high school","college")
  my.control.ed<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.4,0.4,0.2)))
  educ<-unlist(mapply(rep, times=my.control.ed, ed.values))
  
  # Generating what_person_wants
  wpw.values<-c("$300","$400","bed",NA,"safety")
  my.control.wpw<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.1,0.2,0.5,0.1,0.1)))
  wpw<-unlist(mapply(rep, times=my.control.wpw, wpw.values))
  
  # Creating missing values
  holes <- as.logical(rbinom(n = DATA_SIZE, size = 1, prob = sample(OUTLIER_PROBABILITIES, 1)))
  wpw[holes] <- NA
  
  # Generating shelter_checked_at
  shelter_checked_at <- as.factor(sample(c("Shelter1", "Shelter2", "Shelter3"), 
                                         DATA_SIZE,
                                         prob = c(0.25, 0.5, 0.25), 
                                         replace = TRUE))
  
  # Creating missing values
  holes <- as.logical(rbinom(n = DATA_SIZE, size = 1, prob = sample(OUTLIER_PROBABILITIES, 1)))
  shelter_checked_at[holes] <- NA
  
  # Generating time bounds of data collection
  # To be used in next section
  time.now <- Sys.time()
  time.start.temp <- as.POSIXlt(time.now)
  time.start.temp$hour <- time.start.temp$hour - TIME_DIFF
  time.start <- as.POSIXct(time.start.temp)
  
  # Generating date_checked
  date_checked <- sample(seq(from = time.start, to = time.now, by = "mins"), DATA_SIZE)
  date_checked <- format(date_checked, "%Y/%m/%d %H:%M:%S")
  date_checked <- as.character(format(date_checked, format = "%Y/%m/%d"))
  
  # Creating missing values
  holes <- as.logical(rbinom(n = DATA_SIZE, size = 1, prob = sample(OUTLIER_PROBABILITIES, 1)))
  date_checked[holes] <- NA
  
  # Generating intervention
  intervention <- as.factor(sample(c("gave money", "did not give money", "gave bed", "job training", "family therapy"), 
                                   DATA_SIZE,
                                   prob = c(0.15, 0.27, 0.39, 0.11, 0.08),
                                   replace = TRUE))
  
  # Generating checked_at_shelter
  checked_at_shelter <- as.factor(sample(c("No", "Yes"), 
                                         DATA_SIZE,
                                         prob = c(0.5, 0.5),
                                         replace = TRUE))
  
  ##########################################################
  # Dataset creation
  ##########################################################
  
  data<-data.frame(user_id = u.id, 
                   reason_to_contact = rtc, 
                   gender = gender,
                   education = educ,
                   what_person_wants = wpw, 
                   intervention = intervention, 
                   shelter_checked_at = shelter_checked_at, 
                   date_checked = date_checked, 
                   checked_at_shelter = checked_at_shelter)
  
  
  #########################################################
  
  ######################################################### 
  data
}