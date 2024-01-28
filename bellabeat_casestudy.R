#installing and loading necessary packages

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("here")


library(tidyverse)
library(skimr)
library(janitor)
library(here)


#importing data

daily_activity <- read_csv("dailyActivity_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

colnames(daily_activity)
colnames(hourly_calories)
colnames(daily_sleep)
colnames(hourly_steps)

str(daily_activity)
str(hourly_calories)
str(daily_sleep)
str(hourly_steps)


#DATA CLEANING


#renaming data columns

daily_activity <- rename(daily_activity,
                         id = "Id",
                         activity_date = "ActivityDate",
                         total_steps = "TotalSteps",
                         total_distance = "TotalDistance",
                         tracker_distance = "TrackerDistance",
                         logged_activity_distance = "LoggedActivitiesDistance",
                         very_active_distance = "VeryActiveDistance",
                         moderately_active_distance = "ModeratelyActiveDistance",
                         light_active_distance = "LightActiveDistance",
                         sedentary_active_distance = "SedentaryActiveDistance",
                         very_active_minutes = "VeryActiveMinutes",
                         fairly_active_minutes =  "FairlyActiveMinutes",
                         lightly_active_minutes  = "LightlyActiveMinutes",
                         sedentary_minutes = "SedentaryMinutes",
                         calories = "Calories" )

hourly_calories <- rename(hourly_calories,
                          id = "Id",
                          activity_hour =  "ActivityHour",
                          calories = "Calories" )

daily_sleep <- rename(daily_sleep,
                      id = "Id",
                      sleep_day = "SleepDay",
                      total_sleep_records = "TotalSleepRecords",
                      sleep_minute = "TotalMinutesAsleep",
                      bed_time = "TotalTimeInBed")

hourly_steps <- rename(hourly_steps,
                       id = "Id",
                       activity_hour = "ActivityHour",
                       total_steps_hr = "StepTotal")


#splitting mm:dd:yyyy hh:mm:ss to date and time

hourly_calories$activity_hour <- as.POSIXct(hourly_calories$activity_hour, "%m/%d/%Y %I:%M:%S %p", tz = "")
hourly_calories$date <- format(hourly_calories$activity_hour, format = "%m/%d/%y")
hourly_calories$time <- format(hourly_calories$activity_hour, format = "%H:%M:%S")

daily_sleep$sleep_day <- as.POSIXct(daily_sleep$sleep_day, "%m/%d/%Y %I:%M:%S %p" , tz = "")
daily_sleep$date <- format(daily_sleep$sleep_day, format = "%m/%d/%Y")
daily_sleep$time <- format(daily_sleep$sleep_day, format = "%H:%M:%S")

hourly_steps$activity_hour <- as.POSIXct(hourly_steps$activity_hour, "%m/%d/%Y %I:%M:%S %p" , tz = "")
hourly_steps$date <- format(hourly_steps$activity_hour, format = "%m/%d/%Y")
hourly_steps$time <- format(hourly_steps$activity_hour, format = "%H:%M:%S")


#removing unwanted columns from dataframes

daily_sleep <- select(daily_sleep, -c(sleep_day))
hourly_calories <- select(hourly_calories, -c(activity_hour))
hourly_steps <- select(hourly_steps, -c(activity_hour))


#counting NA values and removing them

daily_activity_null <- sum(is.na(daily_activity))
hourly_calories_null <- sum(is.na(hourly_calories))
daily_sleep_null <- sum(is.na(daily_sleep))
hourly_steps_null <- sum(is.na(hourly_steps)) # no null values were found  


# ensuring that rows of all 4 dataframes are unique

daily_activity <- distinct(daily_activity)
daily_sleep <- distinct(daily_sleep)
hourly_calories <- distinct(hourly_calories)
hourly_steps <- distinct(hourly_steps)

#ensuring that each dataframe  has same id's

idcountactivity <- nrow(distinct(daily_activity,id))
print(idcountactivity)
idcountsleep <- nrow(distinct(daily_sleep,id))
print(idcountsleep)
idcountcalories <- nrow(distinct(hourly_calories,id))
print(idcountcalories)
idcountsteps <- nrow(distinct(hourly_steps,id))
print(idcountsteps)


length(intersect(daily_activity$id, daily_sleep$id))
length(intersect(daily_activity$id, hourly_calories$id))
length(intersect(daily_activity$id, hourly_steps$id))


# Data Analysis

#finding mean of each activity in daily_activity dataframe

activity <- daily_activity %>% 
  summarise(very_min_avg  = mean(very_active_minutes),
            fair_min_avg  = mean(fairly_active_minutes),
            light_min_avg = mean(lightly_active_minutes),
            sedentary_min_avg  = mean(sedentary_minutes))

activity_long <- activity %>%            #converting the dataframe to long format
  pivot_longer(cols = c(very_min_avg, fair_min_avg, light_min_avg, sedentary_min_avg), names_to = "activity_type", values_to = "mean_minutes")

hourly_cal <- hourly_calories %>%  #finding out average calories burned in each hour
  group_by(time) %>% 
  summarise(cal_avg = mean(calories))


hourly_step <- hourly_steps %>%        #finding out the steps took in each hour
  group_by(time) %>% 
  summarise(step_avg = mean(total_steps_hr))


#Data Visualization

#Viz for type of activity

ggplot(data = activity_long)+
  geom_col(mapping = aes(x = activity_type, y = mean_minutes, fill = activity_type))+
  labs(title = "Type of Activity Vs Average Minutes Spent", x  = "Type of Activity", y = " Average Minutes", fill = "Type of Activity")+
  theme(axis.text.x = element_text(angle =90))+
  scale_x_discrete(labels = c("fair_min_avg" = "Fairly Active", "light_min_avg"  = "Lightly Active", "sedentary_min_avg" = "Sedentary Active",  "very_min_avg" = "Very Active"))+
  scale_fill_discrete(labels = c("Fairly Active", "Lighlty Active", "Sedentary Active", "Very Active"))



# Viz for time spent during sleeping

ggplot(data = daily_sleep)+
  geom_point(mapping = aes(x = bed_time, y = sleep_minute))+
  geom_smooth(mapping = aes(x = bed_time, y = sleep_minute))+
  labs(title = "Sleep Plot", x  = "Minutes In Bed" , y = "Minutes Slept")


#Viz for average calories burned in each hour

ggplot(data = hourly_cal)+
  geom_col(mapping = aes(x = time, y = cal_avg,fill = cal_avg))+
  labs(title = "Average Calories Burned" , x = "Time" , y  = "Average Calorie burned")+
  theme(axis.text.x = element_text(angle = 90))


#Viz for average steps took in each hour

ggplot(data = hourly_step) +
  geom_col(mapping  = aes(x = time, y = step_avg, fill = step_avg))+
  labs(title = "Average Steps Taken Vs Time", x = "Time", y = "Average Steps Taken")+
  theme(axis.text.x = element_text(angle = 90))
