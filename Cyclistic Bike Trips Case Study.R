#load the libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(dplyr)
library(hms)

##
##---------------PREPARE AND PROCESS---------------
##

#Import 12 Months of Data
apr23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-04-divvy-tripdata.csv")
may23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-05-divvy-tripdata.csv")
jun23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-06-divvy-tripdata.csv")
jul23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-07-divvy-tripdata.csv")
aug23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-08-divvy-tripdata.csv")
sep23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-09-divvy-tripdata.csv")
oct23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-10-divvy-tripdata.csv")
nov23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-11-divvy-tripdata.csv")
dec23_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2023-12-divvy-tripdata.csv")
jan24_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2024-01-divvy-tripdata.csv")
feb24_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2024-02-divvy-tripdata.csv")
mar24_df <- read.csv("C:/Users/jacks/OneDrive/Desktop/Google Course Files/Cyclistic Case Study/Data Files/2024-03-divvy-tripdata.csv")

##merge all of 12 data files into one year file
bikes_trips <- rbind(apr23_df, may23_df, jun23_df, jul23_df, aug23_df, sep23_df, oct23_df, nov23_df, dec23_df, jan24_df, feb24_df, mar24_df)

##removing original 12 files
remove(apr23_df, may23_df, jun23_df, jul23_df, aug23_df, sep23_df, oct23_df, nov23_df, dec23_df, jan24_df, feb24_df, mar24_df)

#create new data frame to create columns 
cyclistic_df <- bikes_trips

#remove original date frame for space 
rm(bikes_trips)

#remove unnecessary columns
cyclistic_df <- cyclistic_df %>% select(-c(ride_id, start_station_id, end_station_id))

#replace old bike names with up-to-date names
cyclistic_df$rideable_type <- gsub("docked_bike", "classic_bike", cyclistic_df$rideable_type)

#create columns to separate date and time
cyclistic_df$start_date <- as.Date(cyclistic_df$started_at) #column for start date of trip
cyclistic_df$start_time <- format(as.POSIXct(cyclistic_df$started_at), "%H:%M:%S") #column for start time of trip
cyclistic_df$end_date <- as.Date(cyclistic_df$ended_at) #column for end date of trip
cyclistic_df$end_time <- format(as.POSIXct(cyclistic_df$ended_at), "%H:%M:%S") #column for end time of trip

#create columns for hour
cyclistic_df$start_hour <- lubridate::hour(cyclistic_df$started_at)

#create column to calculate duration of trip
cyclistic_df$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")

#create columns for day of week and month of start date
cyclistic_df$day_of_week <- weekdays(as.Date(cyclistic_df$start_date)) #column for day of week
cyclistic_df$month <- months(as.Date(cyclistic_df$start_date)) #column for month

#create column for season of start date
cyclistic_df <- cyclistic_df %>% mutate(season = 
                                          case_when(month == "April" ~ "Spring",
                                                    month == "May" ~ "Spring",
                                                    month == "June" ~ "Summer",
                                                    month == "July" ~ "Summer",
                                                    month == "August" ~ "Summer",
                                                    month == "September" ~ "Fall",
                                                    month == "October" ~ "Fall",
                                                    month == "November" ~ "Fall",
                                                    month == "December" ~ "Winter",
                                                    month == "January" ~ "Winter",
                                                    month == "February" ~ "Winter",
                                                    month == "March" ~ "Spring")
)

#remove trips with a duration shorter than one minute or longer than one day
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <= 1),] #remove negatives and rides shorter than one minute 
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length >= 1440),] #remove rides longer than one day (or 1440 minutes)

#remove null and blank values
cyclistic_df <- na.omit(cyclistic_df) #removes rows with NA values
cyclistic_df <- cyclistic_df[!cyclistic_df$start_station_name == "", ] #removes blanks from start_station_name
cyclistic_df <- cyclistic_df[!cyclistic_df$end_station_name == "", ] #removes blanks from end_station_name
cyclistic_df <- cyclistic_df[!cyclistic_df$start_lat == "", ] #removes blanks from start_lat
cyclistic_df <- cyclistic_df[!cyclistic_df$start_lng == "", ] #removes blanks from start_lng
cyclistic_df <- cyclistic_df[!cyclistic_df$end_lat == "", ] #removes blanks from end_lat
cyclistic_df <- cyclistic_df[!cyclistic_df$end_lng == "", ] #removes blanks from end_lng

#View final table
View(cyclistic_df)

#Total number of Rides
nrow(cyclistic_df)


##
##---------------ANALYSIS---------------
##


#
#-----SET UP FUNCTIONS-----
#

#create mode function for data frame
my_mode <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}


#
#-----MEMBER AND BIKE TYPE ANALYSIS-----
#

#group and count rides by member type
cyclistic_df %>% group_by(member_casual) %>% count(member_casual)

#group and count rides by bike type
cyclistic_df %>% group_by(rideable_type) %>% count(rideable_type)

#group member type of bike
cyclistic_df %>% group_by(member_casual, rideable_type) %>% count(rideable_type)


#
#-----HOUR CALCULATIONS-----
#

#count rides by hour
cyclistic_df %>% group_by(start_hour) %>% count(start_hour) %>% print(n = 24)

#mode hour
modeStartHour <- my_mode(cyclistic_df$start_hour)
print(modeStartHour)

#group member and count rides by hour
cyclistic_df %>% group_by(member_casual, start_hour) %>% count(start_hour) %>% print(n = 48)

#mode hour per group
modeStartHourByGroup <- cyclistic_df %>% group_by(member_casual) %>% summarise(mode = my_mode(start_hour))
print(modeStartHourByGroup)


#
#-----DAY OF WEEK CALCULATIONS-----
#

#count rides by day of week
cyclistic_df %>% count(day_of_week)

#mode day of week
modeDay <- my_mode(cyclistic_df$day_of_week)
print(modeDay)

#group member and count rides by day
cyclistic_df %>% group_by(member_casual) %>% count(day_of_week) 

#mode day per group
modeDayByGroup <- cyclistic_df %>% group_by(member_casual) %>% summarise(mode = my_mode(day_of_week))
print(modeDayByGroup)


#
#-----MONTH CALCULATIONS-----
#

#count rides by month
cyclistic_df %>% count(month)

#mode month
modeMonth <- my_mode(cyclistic_df$month)
print(modeMonth)

#group member and counts rides by month
cyclistic_df %>% group_by(member_casual) %>% count(month) %>% print(n = 24)

#mode month per group
modeMonthByGroup <- cyclistic_df %>% group_by(member_casual) %>% summarise(mode = my_mode(month))
print(modeMonthByGroup)


#
#-----SEASON CALCULATIONS-----
#

#-SPRING-

#total spring rides
cyclistic_df %>% filter(season == "Spring") %>% count(season)

#spring rides per group
cyclistic_df %>% group_by(member_casual) %>% filter(season == "Spring") %>% count(season)


#-SUMMER-

#total summer rides
cyclistic_df %>% filter(season == "Summer") %>% count(season)

#summer rides per group
cyclistic_df %>% group_by(member_casual) %>% filter(season == "Summer") %>% count(season)


#-FALL-

#total fall rides
cyclistic_df %>% filter(season == "Fall") %>% count(season)

#fall rides per group
cyclistic_df %>% group_by(member_casual) %>% filter(season == "Fall") %>% count(season)


#-WINTER-

#total winter rides
cyclistic_df %>% filter(season == "Winter") %>% count(season)

#winter rides per group
cyclistic_df %>% group_by(member_casual) %>% filter(season == "Winter") %>% count(season)


#-ALL SEASONS-

#total rides by season
cyclistic_df %>% group_by(season) %>% count(season)

#mode season
modeSeason <- my_mode(cyclistic_df$season)
print(modeSeason)

#season rides by group
cyclistic_df %>% group_by(season, member_casual) %>% count(season)

#mode season by group
modeSeasonByGroup <- cyclistic_df %>% group_by(member_casual) %>% summarise(mode = my_mode(season))
print(modeSeasonByGroup)


#
#-----RIDE LENGTH  CALCULATIONS-----
#


#--GENERAL RIDE LENGTH--

#average ride length
avgRide <- mean(cyclistic_df$ride_length) 
print(avgRide)

#max and min ride length
maxRide <- max(cyclistic_df$ride_length)
print(maxRide)
minRide <- min(cyclistic_df$ride_length)
print(minRide)


#--MEMBER AND BIKE TYPE-- 

#average ride length per member group  
avgRideByMem <- cyclistic_df %>% group_by(member_casual) %>% summarise(avg = mean(ride_length))
print(avgRideByMem)

#average ride length per bike group
avgRideByBike <- cyclistic_df %>% group_by(rideable_type) %>% summarise(avg = mean(ride_length))
print(avgRideByBike)

#average ride length per member and bike type
avgRideBikeMem <- cyclistic_df %>% group_by(member_casual, rideable_type) %>% summarise(avg = mean(ride_length))
print(avgRideBikeMem)


#--HOUR--

#average ride length per hour
avgRideByHour <- cyclistic_df %>% group_by(start_hour) %>% summarise(avg = mean(ride_length))
print(avgRideByHour, n = 24)

#average ride length per member and hour
avgRideHourMem <- cyclistic_df %>% group_by(member_casual, start_hour) %>% summarise(avg = mean(ride_length))
print(avgRideHourMem, n = 48)


#--DAY OF WEEK--

#average ride length per day
avgRideByDay <- cyclistic_df %>% group_by(day_of_week) %>% summarise(avg = mean(ride_length))
print(avgRideByDay)

#average ride length per member and day
avgRideDayMem <- cyclistic_df %>% group_by(member_casual, day_of_week) %>% summarise(avg = mean(ride_length))
print(avgRideDayMem)


#--MONTH--

#average ride length per month
avgRideByMonth <- cyclistic_df %>% group_by(month) %>% summarise(avg = mean(ride_length))
print(avgRideByMonth)

#average ride length per member and month
avgRideMonthMem <- cyclistic_df %>% group_by(member_casual, month) %>% summarise(avg = mean(ride_length))
print(avgRideMonthMem, n = 24)


#--SEASON--

#-SPRING-

#average ride length in spring
avgRideSpring <- cyclistic_df %>% group_by(season) %>% filter(season == "Spring") %>% summarise(avg = mean(ride_length))
print(avgRideSpring)

#average ride length per member and spring
avgRideSpringMem <- cyclistic_df %>% group_by(member_casual, season) %>% filter(season == "Spring") %>% summarise(avg = mean(ride_length))
print(avgRideSpringMem)


#-SUMMER-

#average ride length in summer
avgRideSummer <- cyclistic_df %>% group_by(season) %>% filter(season == "Summer") %>% summarise(avg = mean(ride_length))
print(avgRideSpring)

#average ride length per member and summer
avgRideSummerMem <- cyclistic_df %>% group_by(member_casual, season) %>% filter(season == "Summer") %>% summarise(avg = mean(ride_length))
print(avgRideSummerMem)


#-FALL-

#average ride length in fall
avgRideFall <- cyclistic_df %>% group_by(season) %>% filter(season == "Fall") %>% summarise(avg = mean(ride_length))
print(avgRideFall)

#average ride length per member and fall
avgRideFallMem <- cyclistic_df %>% group_by(member_casual, season) %>% filter(season == "Fall") %>% summarise(avg = mean(ride_length))
print(avgRideFallMem)


#-WINTER-

#average ride length in winter
avgRideWinter <- cyclistic_df %>% group_by(season) %>% filter(season == "Winter") %>% summarise(avg = mean(ride_length))
print(avgRideWinter)

#average ride length per member and winter
avgRideWinterMem <- cyclistic_df %>% group_by(member_casual, season) %>% filter(season == "Winter") %>% summarise(avg = mean(ride_length))
print(avgRideWinterMem)


#-ALL SEASONS-

#average ride length per season
avgRideBySeason <- cyclistic_df %>% group_by(season) %>% summarise(avg = mean(ride_length))
print(avgRideBySeason)

#average ride length per member and season
avgRideSeasonMem <- cyclistic_df %>% group_by(member_casual, season) %>% summarise(avg = mean(ride_length))
print(avgRideSeasonMem)


##
##---------------SHARE/VISUALIZE---------------
##

#create copy of data frame for Tableau usage
cyclistic_tableau <- cyclistic_df

#clean data frame for tableau
cyclistic_tableau <- cyclistic_tableau %>% select(-c(start_station_name, end_station_name, started_at, ended_at))

#download as a csv file
fwrite(cyclistic_tableau, "cyclistic_final_data.csv")
