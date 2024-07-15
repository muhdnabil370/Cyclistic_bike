
#load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#import the cleaned data
clean_trip_final <-read.csv("./clean_trip_final.csv")
str(clean_trip_final)
names(clean_trip_final)

#order the data
clean_trip_final$month <- ordered(clean_trip_final$month,
                                  levels = c("Jan_23","Feb_23","Mar_23","Apr_23",
                                             "May_23","Jun_23","Jul_23","Aug_23",
                                             "Sep_23","Oct_23","Nov_23","Dec_23"))

clean_trip_final$week_day <- ordered(clean_trip_final$week_day,
                                     levels = c("Sunday","Monday","Tuesday",
                                                "Wednesday","Thursday","Friday",
                                                "Saturday"))

#Analysis:- min, max, median, average
view(describe(clean_trip_final$ride_length,fast = TRUE))

#Total no.of customer
view(table(clean_trip_final$customer_type))

#Total rides for each customer type in minutes
view(setNames(aggregate(ride_length ~ customer_type, clean_trip_final, sum),
              c("customer_type", "total_ride_len(mins)")))

#Differences between member and casual rides in terms of length of ride
view(clean_trip_final %>% 
       group_by(customer_type) %>% 
       summarise(min_length_ride = min(ride_length), max_length_ride = max(ride_length),
                 median_length_ride = median(ride_length), mean_length_ride = mean(ride_length)))

#Average ride_length for user by day_of_week and Number of total rides by day_of_week
view(clean_trip_final %>% 
       group_by(week_day) %>% 
       summarise(average_ride_length = mean(ride_length), number_of_ride = n()))

#Average ride_length by month
view(clean_trip_final %>% 
       group_by(month) %>% 
       summarise(average_ride_length = mean(ride_length), number_of_ride = n()))

#Average ride length comparision by each week according to each customer type
view(aggregate(clean_trip_final$ride_length ~ clean_trip_final$customer_type + 
                 clean_trip_final$week_day, FUN = mean))

view(aggregate(ride_length ~ customer_type + week_day, clean_trip_final, mean)) # same fucntion same output just diffent approach

#Average ride length comparision by each month according to each customer type
view(aggregate(clean_trip_final$ride_length ~ clean_trip_final$customer_type +
                 clean_trip_final$month, FUN = mean))

#Analyze rider lenght data by customer type and weekday
view(clean_trip_final %>% 
       group_by(customer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#Analyze rider lenght data by customer type and month
view(clean_trip_final %>% 
       group_by(customer_type, month) %>% 
       summarise(number_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#save the data for data visualization
write.csv(clean_trip_final,file = "./clean_trip_final_tableau.csv",row.names = FALSE)
























