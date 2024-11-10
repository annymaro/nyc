suppressPackageStartupMessages(library(tidyverse))
library(nycflights13)
library(lm.beta)

dep_delay_lower = quantile(flights$dep_delay,0.003, na.rm=T)
dep_delay_upper = quantile(flights$dep_delay,0.997, na.rm=T)
flight_outliers<-which(flights$dep_delay > dep_delay_upper | flights$dep_delay < dep_delay_lower)
flights_noutliers<- data.frame(flights[-flight_outliers,])

Q1 <- (nrow(flights) - length(which(flights$dep_delay > dep_delay_upper | flights$dep_delay < dep_delay_lower)))/nrow(flights) * 100


#Run cor.test for the relationship between departure delay and distance.
depat_delay <- round(flights_noutliers$dep_delay,2)
flight_dist <- round(flights_noutliers$distance,2)
Q2 <- cor.test(depat_delay,flight_dist)



#Create a regression predicting departure delay from distance. The summary of the model should be assigned to Q3
Q3 <-
  summary(lm(dep_delay~distance, flights_noutliers))



Q4 <- lm.beta(lm(dep_delay~distance, flights_noutliers))


#Create another regression, this time adding carrier to the regression from Q3.The summary of the model should be assigned to Q5.
Q5<-
  summary(lm(dep_delay~distance+carrier, flights_noutliers))


