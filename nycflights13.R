
suppressPackageStartupMessages(library(tidyverse))


library(nycflights13)

Q1 <- as.data.frame(filter(flights, carrier %in% c('AA', 'EV', 'FL')) %>%
  group_by(carrier) %>%
  summarise(mean_dist = round(mean(distance, na.rm = TRUE),2)) %>% 
  arrange(carrier) %>% 
  head()) 


Q2 <- flights %>% 
  count(month) %>% 
  
  arrange(desc(n)) %>% 
  head(1)


Q3 <- flights %>%
  group_by(origin, dest) %>%
  summarise(min_dist = round(min(distance),2)) %>% 
  arrange(min_dist) %>% 
  
  head(5)


#What five days of the year had the highest mean distance when leaving from JFK?  Sort in descending order.

Q4<-as.data.frame(flights%>%
  filter(origin=="JFK") %>% 
  group_by(month,day) %>% 
  summarise(mean_distance=round(mean(distance),2))%>% 
  arrange(desc(mean_distance)) %>%
  head(5))


#Calculate the maximum arrival delay for flights to Boston and Atlanta, separately.
Q5<-flights%>%
  filter(dest=="BOS" | dest=="ATL") %>% 
  group_by(dest)%>%
  summarise(max_arr_delay=(max(arr_delay, na.rm = TRUE)))



