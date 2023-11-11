# Beginner-Project (How does bike share navigate speed)
This project was a beginner project for an online certification course Google Data Analytics Professional Certificate. The project is based on cyclist data that has two different riders which is casual and annual riders. The real question arise is there any benefit from making casual riders to take annual membership. How social media can influence casual riders to switch to annual riders. The data I have is divvy's trip data which has data of 12 months. I choose data from July 2022 till June 2023. 

Speaking about the data it has 15 columns which is ride_id, rideable_type, started_at, ended_at, start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual. First we find the ride_length for each rider ti identify how long each riders are communiting. Then, create a column for day_of_week and find the day in which riders take ride.
After calling all the files we combine the files using bind_rows function that combine all the dataset together. Then we start cleaning the data for further processing. To get the overall summary of the data we use summary() function and we can see all the arithmatic calculation such as mean, mode, 1st Quartile, 2nd Quartile, median etc. From the summary it showed that there are 5795 NA's in the dataset.
As a part of cleaning process I had to seperate date into different columns as year, month, day and day_of_week. To do this we can use the following code which is shown below..

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_Week <- format(as.Date(all_trips$date),"%A")

Next is we create a column ride_length so that we can compare ride length for each ride. The code shown as below..
difftime(all_trips$ended_at,all_trips$started_at)

Now let's get bad data removed. First we have to filter out the ride_length which is less than zero and greater than 1440 min. Because ride length shouldn't be negetive or more than one day.
To do that we can use the following code.
all_trips[!(all_trips$ride_length <=0 | 
                            all_trips$ride_length >1440),]

Now lets drop all the NA's using drop NA and remove all the duplicated id's using duplicated function.

**The cleaning part is done now its time for analyzing.**
First lets find out number of rides by type of rider for that we have to use group_by function and we are grouping member_casual and day_of_week column. If we do this we will get a dataset that shows casual and member that have total number of rides they took on each days.

Lets calculate the distance for each ride. To do that we a special function called distGeo. distGeo is a function which calculate the distance from one point to other. Here in this case we use start_lng,start_lat, end_lng,end_lat to calculate the distance.

**Summary findings**
It seems like membership riders have take most number of rides compared to casual riders. Casual riders use bikes on weekends more often while the membership riders ride most on weekdays.
Audgust witness the highest number of rides for membership riders and july has the highest number of riders for casual riders.
summer is the peak season for both casual and membership riders 
