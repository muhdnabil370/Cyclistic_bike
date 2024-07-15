
#install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("data.table")
install.packages("readr")
install.packages("psych")
install.packages("hrbrthemes")
install.packages("ggplot2")

#load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#import data
df1 <- read_csv("./202301-divvy-tripdata/202301-divvy-tripdata.csv")
df2 <- read_csv("./202302-divvy-tripdata/202302-divvy-tripdata.csv")
df3 <- read_csv("./202303-divvy-tripdata/202303-divvy-tripdata.csv")
df4 <- read_csv("./202304-divvy-tripdata/202304-divvy-tripdata.csv")
df5 <- read_csv("./202305-divvy-tripdata/202305-divvy-tripdata.csv")
df6 <- read_csv("./202306-divvy-tripdata/202306-divvy-tripdata.csv")
df7 <- read_csv("./202307-divvy-tripdata/202307-divvy-tripdata.csv")
df8 <- read_csv("./202308-divvy-tripdata/202308-divvy-tripdata.csv")
df9 <- read_csv("./202309-divvy-tripdata/202309-divvy-tripdata.csv")
df10 <- read_csv("./202310-divvy-tripdata/202310-divvy-tripdata.csv")
df11 <- read_csv("./202311-divvy-tripdata/202311-divvy-tripdata.csv")
df12 <- read_csv("./202312-divvy-tripdata/202312-divvy-tripdata.csv")

#Data Validation
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)
colnames(df7)
colnames(df8)
colnames(df9)
colnames(df10)
colnames(df11)
colnames(df12)

#total number of rows
sum(nrow(df1) + nrow(df2) + nrow(df3) + nrow(df4)
    + nrow(df5) + nrow(df6) + nrow(df7) + nrow(df8)
    + nrow(df9) + nrow(df10) + nrow(df11) + nrow(df12))

#Combine data of 12 month into one 
trip_final <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

#save the combined file
write.csv(trip_final,file = "./trip_final.csv",row.names = FALSE)

#setting global variable size to inf
options(future.global.maxSize = Inf)

#final data validation
str(trip_final)
view(head(trip_final))
view(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)

#Data Cleaning

#Count row with "na" values
colSums(is.na(trip_final))

#Remove Missing
clean_trip_final <- trip_final[complete.cases(trip_final),]
#Remove duplicate
clean_trip_final <- distinct(clean_trip_final)
#Remove data with greater start_at than end_at
clean_trip_final <- clean_trip_final %>% 
  filter(started_at < ended_at)
#Remove  na
clean_trip_final <- drop_na(clean_trip_final)
clean_trip_final <- remove_empty(clean_trip_final,which = c("cols"))
clean_trip_final <- remove_missing(clean_trip_final)

#checked cleaned data
colSums(is.na(clean_trip_final))
view(filter(clean_trip_final, clean_trip_final$started_at > clean_trip_final$ended_at))

#Renaming columns for better context
clean_trip_final <- rename(clean_trip_final, customer_type = member_casual, bike_type = rideable_type)

#Separate date in date, day, month, year for better analysis
clean_trip_final$date <- as.Date(clean_trip_final$started_at)
clean_trip_final$week_day <- format(as.Date(clean_trip_final$date), "%A")
clean_trip_final$month <- format(as.Date(clean_trip_final$date), "%b_%y")
clean_trip_final$year <- format(clean_trip_final$date, "%Y")

#separate column for time
clean_trip_final$time <- as.POSIXct(clean_trip_final$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_trip_final$time <- format(clean_trip_final$time, format = "%H:%M")

#Add ride length column
clean_trip_final$ride_length <- difftime(clean_trip_final$ended_at, clean_trip_final$started_at, units = "mins")

#select the data we are going to use
clean_trip_final <- clean_trip_final %>% 
  select(bike_type, customer_type, month, year, time, started_at, week_day, ride_length)

#remove stolen bike
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length>1440,]
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length<5,]

#save the cleaned data
write.csv(clean_trip_final,file = "./clean_trip_final.csv",row.names = FALSE)

#data validation and cleaning R


