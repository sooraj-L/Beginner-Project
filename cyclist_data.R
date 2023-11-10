library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

install.packages("geosphere")
library(geosphere)

#Read the trip data from 2022 july to 2023 june

trip_data_2022_07 <- read.csv("202207-divvy-tripdata.csv")
trip_data_2022_08 <- read.csv("202208-divvy-tripdata.csv")
trip_data_2022_09 <- read.csv("202209-divvy-publictripdata.csv")
trip_data_2022_10 <- read.csv("202210-divvy-tripdata.csv")
trip_data_2022_11 <- read.csv("202211-divvy-tripdata.csv")
trip_data_2022_12 <- read.csv("202212-divvy-tripdata.csv")
trip_data_2023_01 <- read.csv("202301-divvy-tripdata.csv")
trip_data_2023_02 <- read.csv("202302-divvy-tripdata.csv")
trip_data_2023_03 <- read.csv("202303-divvy-tripdata.csv")
trip_data_2023_04 <- read.csv("202304-divvy-tripdata.csv")
trip_data_2023_05 <- read.csv("202305-divvy-tripdata.csv")
trip_data_2023_06 <- read.csv("202306-divvy-tripdata.csv")

# Data Check

colnames(trip_data_2022_07)
colnames(trip_data_2022_08)
colnames(trip_data_2022_09)
colnames(trip_data_2022_10)
colnames(trip_data_2022_11)
colnames(trip_data_2022_12)
colnames(trip_data_2023_01)
colnames(trip_data_2023_02)
colnames(trip_data_2023_03)
colnames(trip_data_2023_04)
colnames(trip_data_2023_05)
colnames(trip_data_2023_06)

#converting col names into lower case
names(trip_data_2023_02) <- tolower(names(trip_data_2023_02))
names(trip_data_2023_05) <- tolower(names(trip_data_2023_05))

#checking the col names
colnames(trip_data_2023_02)
colnames(trip_data_2023_05)


#Data type check
str(trip_data_2022_07)
str(trip_data_2022_08)
str(trip_data_2022_09)
str(trip_data_2022_10)
str(trip_data_2022_11)
str(trip_data_2022_12)
str(trip_data_2023_01)
str(trip_data_2023_02)
str(trip_data_2023_03)
str(trip_data_2023_04)
str(trip_data_2023_05)
str(trip_data_2023_06)

#Combine all data sets

all_trips <- bind_rows(trip_data_2022_07,trip_data_2022_08,trip_data_2022_09,
                       trip_data_2022_10,trip_data_2022_11,trip_data_2022_12,
                       trip_data_2023_01,trip_data_2023_02,trip_data_2023_03,
                       trip_data_2023_04,trip_data_2023_05,trip_data_2023_06)

head(all_trips)

# Process
# Data cleaning before conducting analysis

colnames(all_trips)

#How many rows are in dataframe
nrow(all_trips)

#Dimensions of dataframe
dim(all_trips)

#See list of columns and dataframe
str(all_trips)

# Statistical summary of data.
summary(all_trips)

# There are 5795 NA's in end_lat and end_lng

#Add columns that list out the date,month,year and day.

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_Week <- format(as.Date(all_trips$date),"%A")
head(all_trips)

#deleting column day_of_week
colnames(all_trips)
all_trips <- all_trips[,-15]
head(all_trips)

#Changing to lower case
names(all_trips) <- tolower(names(all_trips))

#Add column ride_length so that I can compare ride length for each ride
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
head(all_trips)                          


#Converting data type of ride_length from character to numeric

all_trips$ride_length <- as.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length)

str(all_trips$ride_length)


##Remove bad data

#Removing ride length which is less than zero and is greater than 1440 min.
#Because ride length shouldn't be either negative or more than one day.

all_trips_v2 <- all_trips[!(all_trips$ride_length <=0 | 
                            all_trips$ride_length >1440),]
dim(all_trips_v2)
summary(all_trips_v2)
View(all_trips_v2)

#Remove NA from all_trips_v2
all_trips_v2 <- drop_na(all_trips_v2)

dim(all_trips_v2)

summary(all_trips_v2)

#Remove duplicated ID's
all_trips_v3 <- all_trips_v2[!duplicated(all_trips_v2$ride_id),]

dim(all_trips_v3)

View(all_trips_v3)

#Assigning the correct order to each day of the week
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, 
                                    levels = c('Monday','Tuesday','Wednesday',
                                               'Thursday','Friday','Saturday',
                                               'Sunday'))
                                          
                                                        
View(all_trips_v3)


#Analyze

# Firstly lets find out number of rides by type of rider
all_trips_v3 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_ride = n(),.groups = 'drop') %>%
  arrange(day_of_week)

#Assign the correct order to each month of the year
all_trips_v3$month <- ordered(all_trips_v3$month,
                              levels=c('07','08','09','10','11','12',
                                       '01','02','03','04','05','06'))

print(all_trips_v3 %>%
  group_by(member_casual,month) %>%
  summarise(number_of_rides = n(),.groups = 'drop') %>%
  arrange(month),n=24)

#finding out distance for each ride
all_trips_v4 <- all_trips_v3 %>%rowwise%>%
  mutate(ride_distance = distm(x=c(start_lng,start_lat),
                               y=c(end_lng,end_lat)))

all_trips_v3$ride_distance <- distGeo(matrix(c(all_trips_v3$start_lng,
                                               all_trips_v3$start_lat),ncol=2),
                                      matrix(c(all_trips_v3$end_lng,all_trips_v3$end_lat),
                                             ncol=2))

View(all_trips_v4)
View(all_trips_v3)

#Summary findings

# it seems like membership riders have take most number of rides compared to
# casual riders. Casual riders use bikes on weekends more often while the 
# membership riders ride most on weekdays.

# August witness the highest number of rides for membership riders and
# July has the highest number of riders for casual riders.

#Summer is the peak season for both casual and membership riders.

#Now let's find out whether ride_length is different for each ride_type.

aggregate(all_trips_v4$ride_length~ all_trips_v4$member_casual + 
            all_trips_v4$day_of_week,FUN = mean)


#Calculating monthly wise average_ride_length for member_casual

print(all_trips_v4 %>%
  group_by(member_casual,month)%>%
  summarise(avg_ride_length = mean(ride_length),.groups = 'drop') %>%
  arrange(month),n=24)


all_trips_v4 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(avg_ride_length = mean(ride_distance),.groups = 'drop') %>%
  arrange(day_of_week)

all_trips_v4 %>%
  group_by(member_casual,month) %>%
  summarise(avg_ride_length = mean(ride_distance),.groups = 'drop') %>%
  arrange(month)


#lets find out how many riders used same station for start point and end station

all_trips_v4 %>%
  group_by(member_casual)%>%
  summarise(number_of_rides = n(),.groups = 'drop')

all_trips_v4 %>%
  group_by(member_casual)%>%
  filter(ride_distance < 1)%>%
  summarise(number_of_rides = n(),.groups = 'drop')

summary(all_trips_v4)

#it is seen that 5% of casual riders returned their bikes at the starting station
#While only 2% of membership riders returned their bikes at their start point station


#Sharing the analysis

all_trips_v4 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides = n(),.groups = 'drop')%>%
  ggplot(aes(x=day_of_week,y=number_of_rides,fill=member_casual)) +
  geom_bar(position = 'dodge',stat = 'identity')

all_trips_v4 %>%
  group_by(member_casual,month)%>%
  summarise(number_of_rides = n(),.groups = 'drop')%>%
  ggplot(aes(x=month,y=number_of_rides,fill=member_casual))+
  geom_bar(position = 'dodge',stat = 'identity')

all_trips_v4 %>%
  group_by(member_casual,day_of_week)%>%
  summarise(average_ride_length = mean(ride_length),.groups = 'drop')%>%
  ggplot(aes(x=day_of_week,y=average_ride_length,fill=member_casual))+
  geom_bar(position = 'dodge',stat='identity')

all_trips_v4 %>%
  group_by(member_casual,month)%>%
  summarise(average_ride_length=mean(ride_length),.groups = 'drop')%>%
  ggplot(aes(x=month,y=average_ride_length,fill=member_casual))+
  geom_bar(position = 'dodge',stat='identity')

#Now lets detect outliers

colnames(all_trips_v4)
str(all_trips_v4)

boxplot(all_trips_v4[,c('start_lat','start_lng','end_lat','end_lng',
                        'ride_length','ride_distance')])

#ride_distance has detected an outlier
#Lets remove outlier

all_trips_v4 %>%
  group_by(member_casual)%>%
  filter(ride_distance < 10000)%>%
  ggplot(aes(x=ride_distance,fill=member_casual)) +
  geom_histogram()


all_trips_v4 %>%
  group_by(member_casual,day_of_week)%>%
  summarise(average_ride_distance=mean(ride_distance),.groups = 'drop')%>%
  ggplot(aes(x=day_of_week,y=average_ride_distance,fill=member_casual)) +
  geom_bar(position = 'dodge',stat = 'identity')

all_trips_v4 %>%
  group_by(member_casual,month)%>%
  summarise(average_ride_distance=mean(ride_distance),.groups = 'drop')%>%
  ggplot(aes(x=month,y=average_ride_distance,fill=member_casual)) +
  geom_bar(position = 'dodge',stat = 'identity')

all_trips_v4 %>%
  group_by(member_casual,day_of_week)%>%
  filter(ride_distance <1)%>%
  ggplot(aes(x=day_of_week,fill=member_casual))+
  geom_bar()


## Analysis

# members_casual vs day_of_week (average number of rides)

# From the fist plot which is number_of_rides of member_casual during days. It can be seen
# from the plot that member riders have most number of rides and casual rides are less compared
# to annual rides. This is because casual riders mostly ride on weekends and annual riders are tend
# to do both weekdays and weekends

#member_casual vs month (average number of rides)

# In this plot we can see that, member riders have highest number of rides compared to 
# casual riders. Casual riders mostly use their bikes during summer months. While, annual riders 
# mostly use on all months but less during winter. Casual riders peak season is summer.

# member_casual  vs day_of_week

#From this plot it can be seen that both the member and casual have equal average ride length
# however, the average ride length of member riders are bit higher than the casual riders.


#Conclusion
# It leads to me a conclusion that member riders use the bikes for longer commute and they
# return their bikes very late. While, Casual riders use it for leisure and fun and mostly use
# on weekends and they return their bikes at their same starting station


