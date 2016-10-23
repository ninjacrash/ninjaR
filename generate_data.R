generate_data <- function(size, seed = 1, time.start = as.POSIXct('2014-01-01 09:00.00 CDT', tz="America/Chicago"), outlier_prob = seq(0.05, 0.1, by = 0.01)){
  # Function generates a data frame of given size
  
  ##########################################################
  # Hard coded variables, can be used as command line params
  ##########################################################
  set.seed(seed)
  DATA_SIZE <- size
  TIME_START <- time.start
  OUTLIER_PROBABILITIES <- outlier_prob
  SAMPLE_MESSAGES <- as.character(read.csv("./data/stories.csv",header=F)$V1)
  UIDS <- as.character(read.csv("./data/UIDs.csv",header=F)$V1)
 ##########################################################
  # Generating features for dataset
  ##########################################################
  
  # Generating user_id
  user_id <- sample(x = UIDS, size = DATA_SIZE, replace = TRUE)
  
  # Generating reason_to_contact
  # rtc.values<-c("could not pay rent","have nowhere to sleep","lost my job","potential family violence")
  # my.control.rtc<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.2,0.6,0.1,0.1)))
  # rtc<-unlist(mapply(rep, times=my.control.rtc, rtc.values))
  
  # Generating gender
  # gender.values<-c("M","F")
  # my.control.gender<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.6,0.4)))
  # gender<-unlist(mapply(rep, times=my.control.gender, gender.values))
  
  # Generating education
  # ed.values<-c("some high school","high school","college")
  # my.control.ed<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(0.4,0.4,0.2)))
  # educ<-unlist(mapply(rep, times=my.control.ed, ed.values))
  
  # Generating what_person_wants
  wpw.values<-c(SAMPLE_MESSAGES)
  my.control.wpw<-as.vector(rmultinom(1, size = DATA_SIZE, prob = c(rep(0.08, 12), 0.04)))
  wpw<- sample(unlist(mapply(rep, times=my.control.wpw, wpw.values)))
  
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
  
  # Generating date_checked
  date_checked <- sample(seq(from = time.start, to = time.now, by = "mins"), DATA_SIZE, replace = TRUE)
  date_checked <- strftime(as.POSIXlt(date_checked), format = "%m/%d/%Y %H:%M:%S %Z", tz = 'America/Chicago') 

  
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
  
  # Creating missing values
  holes <- which(checked_at_shelter == 'No')
  date_checked[holes] <- NA
  
  ##########################################################
  # Dataset creation
  ##########################################################
  
  data<-data.frame(user_id = user_id,  
                   # gender = gender,
                   # education = educ,
                   message = wpw, 
                   intervention = intervention, 
                   shelter_name = shelter_checked_at, 
                   event_dt = date_checked, 
                   checked_at_shelter = checked_at_shelter)
  
  
  #########################################################
  
  ######################################################### 
  data
}
