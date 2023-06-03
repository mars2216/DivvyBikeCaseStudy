# DivvyBikeCaseStudy
In this case study, I performed many real-world tasks of a data analyst. I worked for a company, and met different team members. In order to answer the key business questions.

---
title: "Official Case Study 1"
author: "MP"
date: "2023-05-29"
output:
  html_document: default
  pdf_document: default
---
### PREPARE
  Dataset: https://divvy-tripdata.s3.amazonaws.com/index.html

  For this analysis, I will use the Q1 2019 to Q4 2019 data.
```{r}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
```
```{r}
q1_2019 <- read_csv("C:/Users/Marshal/Downloads/Official Bike Trip Data/.CSV/Divvy_Trips_2019_Q1/Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("C:/Users/Marshal/Downloads/Official Bike Trip Data/.CSV/Divvy_Trips_2019_Q2/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("C:/Users/Marshal/Downloads/Official Bike Trip Data/.CSV/Divvy_Trips_2019_Q3/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("C:/Users/Marshal/Downloads/Official Bike Trip Data/.CSV/Divvy_Trips_2019_Q4/Divvy_Trips_2019_Q4.csv")
```

### PROCESS

```{r}
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
```
```{r}
q2_2019fixed <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,tripduration = "01 - Rental Details Duration In Seconds Uncapped"
                   ,birthyear = "05 - Member Details Member Birthday Year"
                   ,gender = "Member Gender"
                   ,member_casual = "User Type")

q4_2019fixed <- rename(q4_2019
                  ,ride_id = trip_id 
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)

q3_2019fixed <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype)

q1_2019fixed <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype)
```
We need to check the data types of each column to ensure that all data is correctly formatted
```{r}
str(q1_2019fixed)
str(q2_2019fixed)
str(q3_2019fixed)
str(q4_2019fixed)
```
 We need to convert the "gender" column to character type so that we can merge the tables.
```{r}
q4_2019fixed <- mutate(q4_2019fixed, gender = as.character(gender)) 
q3_2019fixed <- mutate(q3_2019fixed, gender = as.character(gender)) 
q2_2019fixed <- mutate(q2_2019fixed, gender = as.character(gender))
q1_2019fixed <- mutate(q1_2019fixed, gender = as.character(gender))
```
Now, i will merge the data frames into one data frame.
```{r}
trips <- bind_rows(q1_2019fixed, q2_2019fixed, q3_2019fixed, q4_2019fixed)
```
The next step is to clean the data. This step is crucial to ensure accurate data and enable future calculations. We will add columns to list the date, month, day, and year of each trip. This will allow us to aggregate trip data by month, day, or year.
```{r}
trips$date <- as.Date(trips$started_at) 
trips$month <- format(as.Date(trips$date), "%m")
trips$day <- format(as.Date(trips$date), "%d")
trips$year <- format(as.Date(trips$date), "%Y")
trips$day_of_week <- format(as.Date(trips$date), "%A")
colnames(trips)
```
Now, we will add a column for the ride length of each trip by calculating the time difference between the start time and end time of the trip.
```{r}
trips$ride_length = difftime(trips$ended_at, trips$started_at)
```
There are some "bad" data points that need to be removed when the ride length is negative, which occurs when bikes are temporarily removed for quality checks. We will create a new dataframe without these trips with negative ride lengths.
```{r}
trip_data_clean <- trips[!(trips$ride_length <= 0),]
glimpse(trip_data_clean)
```

##Analyze 

Now, we will perform a data analysis to find patterns between Customers and Subscribers. Before we begin the analysis, let's examine some basic descriptive statistics about the data.
```{r}
mean(trip_data_clean$ride_length)
median(trip_data_clean$ride_length) 
max(trip_data_clean$ride_length) 
min(trip_data_clean$ride_length)
``` 

First, we will compare the descriptive statistics of ride lengths for Customers and Subscribers. 
```{r}
aggregate(trip_data_clean$ride_length ~ trip_data_clean$member_casual, FUN = mean)
aggregate(trip_data_clean$ride_length ~ trip_data_clean$member_casual, FUN = median)
aggregate(trip_data_clean$ride_length ~ trip_data_clean$member_casual, FUN = max)
aggregate(trip_data_clean$ride_length ~ trip_data_clean$member_casual, FUN = min)
```
Before we proceed, let's sort the day_of_week column in the correct order. 

```{r}
trip_data_clean$day_of_week <- ordered(trip_data_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

Next, we will examine the average ride time per day and the total number of rides for Customers and Subscribers.
```{r}
plot <- trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>%  #groups by member_casual
  summarise(number_of_rides = n() #calculates the number of rides and average duration 
  ,average_ride_length = mean(ride_length),.groups="drop") %>% # calculates the average duration
  arrange(member_casual, day_of_week) #sort
```
```{r}
ggplot(plot,aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```
From the above chart, it can be observed that Subscribers are the group with the highest number of rides on each day of the week.

```{r}
ggplot(plot,aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Members and Casual riders Vs. Day of the week")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```
From the above chart, we can observe that the Customer group rides for longer durations during the week, with the highest number of rides on weekends, while Subscribers maintain a consistent ride duration throughout the week, with the highest number of rides on weekends.

```{r}
trip_data_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders by Month") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```
From the above chart, we can see that the Subscriber group has higher ride counts throughout the year.

## Act
For the final step in the data analysis process, we will provide three recommendations to increase the number of Subscribers annually. But first, let's present three key insights.

### Key Findings:
 1. Customers have the highest ride counts on weekends. On the other hand, Subscribers have the highest ride counts on weekdays.
 2. On average, Subscribers have shorter ride durations compared to Customers.
 3. There is no significant difference between Customers and Subscribers in terms of the number of rides they take each month. Both Customers and Subscribers have the highest ride counts during the summer months and the lowest ride counts at the end of winter and the beginning of spring.

### Recommendations:
 1. Target Customers for leisure bike rentals on weekends.
 2. Launch a major summer campaign to attract more bike rentals, as there is a higher potential for both Customers and Subscribers during this season.

