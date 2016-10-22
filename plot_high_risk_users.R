library(ggplot2)
library(ggmap)
library(httr)
library(jsonlite)

userids <- list()

get_loc <- function(userids) {
  combined_df <- data.frame()
  for (userid in userids){
    API_URL <- paste0("http://ec2-54-159-3-36.compute-1.amazonaws.com/events?user_id=eq.",userid,"&&select=user_id,extra-%3E%3Elatitude,extra-%3E%3Elongitude")
    result <- GET(url=API_URL,add_headers(`Content-Type`='application/json',Prefer="count=none"))
    response <- fromJSON(content(result,"text"))
    combined_df <- rbind(combined_df,response)
  }
  combined_df <- unique(combined_df)
  combined_df$latitude <- as.numeric(combined_df$latitude)
  combined_df$longitude <- as.numeric(combined_df$longitude)
  # getting the map
  map <- get_map(location = c(longitude = mean(combined_df$longitude), latitude = mean(combined_df$latitude)), zoom = 4,
                        maptype = "satellite", scale = 2)
  
  # plotting the map with some points on it
  ggmap(map) +
    geom_point(data = combined_df, aes(x = longitude, y = latitude, fill = "red", alpha = 0.8), size = 5, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)
  return()
}