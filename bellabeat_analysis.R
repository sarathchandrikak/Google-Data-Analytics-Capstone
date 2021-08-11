# Reading files

dailyactivity <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
dailycalories <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
dailyintensities <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
dailysteps <- read.csv("Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
heartrate_second <- read.csv("Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
hourlycalories <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourlyintensities <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourlySteps <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
minutecalories_narrow <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
minutecalories_wide <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteCaloriesWide_merged.csv")
minuteintensities_narrow <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
minuteintensities_wide <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged.csv")
minutemets_narrow <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
minutesleep <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
minutessteps_narrow <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")
minutesteps_wide <- read.csv("Fitabase Data 4.12.16-5.12.16/minuteStepsWide_merged.csv")
sleepday <- read.csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightlog <- read.csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# packages

library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(corrplot)
library(ggcorrplot)

# Data Pre-processing 

str(dailyactivity)
summary(dailyactivity)
# Splitting into separate date and time columns

str(heartrate_second)
summary(heartrate_second)

heartrate_second<- 
  heartrate_second %>%
  separate(Time, c("Date", "Time"), " ")

View(heartrate_second)

str(sleepday)
summary(sleepday)
sleepday <- 
  sleepday %>%
  separate(SleepDay, c("Date", "Time"), " ")

View(sleepday)

str(weightlog)
summary(weightlog)
weightlog <- 
  weightlog %>%
  separate(Date, c("Date", "Time"), " ")

View(weightlog)


# Calculating average heartbeat across the day for each person

heartbeat_daily <- 
  tibble(heartrate_second %>%
           group_by(Date, Id) %>%
           summarise(MeanHeartBeat=(mean(Value))))

View(heartbeat_daily)

# Checking the dimensions of all data files 
dim(dailyactivity)
dim(heartbeat_daily)
dim(sleepday)
dim(weightlog)

# Finding unique persons in each data frame 

length(unique(weightlog$Id))
length(unique(heartbeat_daily$Id))
length(unique(sleepday$Id))
length(unique(dailyactivity$Id))

# 33 is the highest unique values from the dataframe. That is whole of the data is collected from 33 participants.

# Finding duplicates in each data frame

nrow(dailyactivity[duplicated(dailyactivity),])
nrow(heartbeat_daily[duplicated(heartbeat_daily),])
nrow(sleepday[duplicated(sleepday),])
nrow(weightlog[duplicated(weightlog),])

# Removing duplicates from sleepday dataframe

sleepdata <- dplyr::distinct(sleepday)

# Summary of Statistics

dailyactivity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         VeryActiveMinutes) %>%
  summary()

# Since Minimum steps are 0, we can assume the person not being active
# Active Minutes are around 210

summary(heartbeat_daily)
# Mean heartbeat is nearby to original value of 72

sleepday %>%
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
summary()

weightlog %>%
  select(WeightKg, BMI) %>%
summary()

# Finding NA and null values in dataframes

which(is.na(dailyactivity))
which(is.na(heartbeat_daily))
which(is.na(sleepday))
which(is.na(weightlog))

# Most of the values in fat column of weight log are Null, so removing that column 

weightlog <- weightlog[-c(6)]
View(weightlog)
# Creating a data frame with common users from all the data frames

combined_df <- merge(dailyactivity, sleepday, by.x=c("Id", "ActivityDate"), by.y=c("Id", "Date"))
combined_df = merge(combined_df, weightlog, by.x=c("Id", "ActivityDate"), by.y=c("Id", "Date"))
combined_df = merge(combined_df, heartbeat_daily, by.x=c("Id", "ActivityDate"), by.y=c("Id", "Date"))

View(combined_df)
dim(combined_df)
length(unique(combined_df$Id))
length(unique(combined_df$ActivityDate))

# 3 persons recorded all the values at least on one single day. 

# Dividing time into morning, afternoon, evening, night for heart data

heartrate_second$time<- dmy_hms(heartrate_second$Time)
which(is.na(heartrate_second))
dim(heartrate_second)
heartrate_second <- na.omit(heartrate_second)
breaks <- hour(hm("6:00", "12:00", "16:00", "19:00", "23:59"))

# labels for the breaks

labels <- c("Morning", "Afternoon", "Evening", "Night")
heartrate_second$Time_of_day <- cut(x=hour(heartrate_second$time), breaks = breaks, labels = labels, include.lowest=TRUE)


# Plotting Data on graphs

# Sleep Data - Visualization between Total Min Asleep and Total Time in Bed

ggplot(data=sleepday, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  labs(title="Total Minutes Asleep vs Total Time in Bed") 

# From the graph it is clear that both the points are almost around the straight line except for few.
# We can consider this people mostly fall asleep right after they get into bed. By this Bellabeat can
# include a feature that indicates a reminder to sleep for users in and around their daily sleep time. 

# Mean Heart Beat Histogram

ggplot(data=heartbeat_daily, aes(x=MeanHeartBeat)) +
  geom_histogram() + 
  labs(title = "Mean Heart Beat Histogram")

# Heart Rate Graph during different times of the day

heartbeat_grouping <- 
  tibble(heartrate_second %>%
           group_by(Time_of_day) %>%
           summarise(MeanValue=(mean(Value))))

heartbeat_grouping <- heartbeat_grouping %>% drop_na()

ggplot(data = heartbeat_grouping, aes(x=Time_of_day, y=MeanValue)) + 
  geom_bar(stat = "identity") + 
  labs(title="Time of Day vs Mean Heart Value")

# On an average heartbeat of people during evening is high and night it is very low.

# Daily Activity Total steps vs Calories

ggplot(data=dailyactivity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  labs(title="Total Steps VS Total Calories")

# Total distance vs Total steps

ggplot(data=dailyactivity, aes(x=TotalSteps, y=TotalDistance)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(title = "Total Steps VS Distance")

# Total distance vs Tracker Distance

ggplot(data=dailyactivity, aes(x=TrackerDistance, y=TotalDistance)) +
  geom_point() + 
  geom_smooth(aes(x=TrackerDistance, y=TotalDistance)) +
  labs(title = "Tracker Distance VS Total Distance")

# Very Active Minutes vs Calories
ggplot(data=dailyactivity, aes(x=VeryActiveMinutes , y=Calories)) +
  geom_point() + 
  geom_smooth(aes(x=VeryActiveMinutes , y=Calories)) +
  labs(title = "Very Active Minutes VS Calories")

# Fairly Active Minutes vs Calories

ggplot(data=dailyactivity, aes(x=FairlyActiveMinutes, y=Calories)) +
  geom_point() + 
  geom_smooth(aes(x=FairlyActiveMinutes , y=Calories)) +
  labs(title = "Fairly Active Minutes VS Calories")

# Lightly Active Minutes vs Calories
ggplot(data=dailyactivity, aes(x=LightlyActiveMinutes, y=Calories)) +
  geom_point() + 
  geom_smooth(aes(x=LightlyActiveMinutes, y=Calories)) +
  labs(title = "Lightly Active Minutes vs Calories")
export.type <- "jpeg"
export.filename <- "Lightly Active Minutes vs Calories.jpeg"
export.height <- 123
export.width<- 123

export.func <- paste0(export.type,"(",export.filename,")")

eval(parse(text="export.func"))

# Correlation Matrix of daily activity data

selected_columns <- select(dailyactivity, TotalSteps, TotalDistance, VeryActiveMinutes, SedentaryMinutes, Calories)
corr = cor(selected_columns)
corrplot(corr, method = 'number')

# Graphs on combined_data of 3 records

ggplot(data=combined_df, aes(x=VeryActiveMinutes, y=BMI)) +
  geom_point() + 
  geom_smooth(orientation = "x") +
  labs(title = "Very Active Minutes vs BMI")

ggplot(data=combined_df, aes(x=FairlyActiveMinutes, y=BMI)) +
  geom_point() + 
  geom_smooth(orientation = "x") +
  labs(title = "Fairly Active Minutes vs Total Minutes Asleep")

ggplot(data=combined_df, aes(x=LightlyActiveMinutes, y=BMI)) +
  geom_point() + 
  geom_smooth(orientation = "x") +
  labs(title = "Very Active Minutes vs Total Minutes Asleep")

# Saving Modified Files

write_csv(x = heartbeat_grouping, "heartbeat_timeofday.csv")
write_csv(x = heartbeat_daily, "heartbeat_daywise.csv")
write_csv(x = combined_df, "combined_data.csv")


combined_df %>%
  select(FairlyActiveMinutes,
         VeryActiveMinutes,
         LightlyActiveMinutes,
         TotalMinutesAsleep,
         BMI) %>%
  summary()

ggplot(data=combined_df, aes(x=FairlyActiveMinutes, y=BMI)) +
  geom_bar(stat="identity")

library(tidyr)
library(ggplot2)

summary(heartrate_second)

  
  
