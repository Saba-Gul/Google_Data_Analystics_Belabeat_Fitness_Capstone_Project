# Loading packages
library("tidyverse")
library("dplyr") 
library("lubridate") 
library("dplyr") 
library("ggplot2")

# Import the datasets:
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")

glimpse(daily_activity)
glimpse(daily_sleep)

head(daily_activity)
colnames(daily_activity)

head(daily_sleep)
colnames(daily_sleep)

# Check the number of participants in each dataset
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)

#Check the number of days covered per participant in each dataset
n_distinct(daily_activity$ActivityDate)
n_distinct(daily_sleep$SleepDay)

# Rename the columns to 'Date'
clean_sleep <- daily_sleep %>%
  rename(Date=SleepDay)
clean_activity <- daily_activity %>%
  rename(Date=ActivityDate)

# Convert the date values to type date
clean_sleep$Date <- as.Date(clean_sleep$Date, format = "%m/%d/%Y")
clean_activity$Date <- as.Date(clean_activity$Date, format = "%m/%d/%Y")

# View data after transformation
head(clean_sleep)
head(clean_activity)

activity_summary <- clean_activity %>%
  select(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes) %>%
  summary()
activity_summary

# Load the 'scales' library to convert decimals to percentages.
library(scales)

# Summarize the average time spent on each kind of activity across all participants.
average_activities <- clean_activity %>%
  summarize(
    average_sedentary = mean(SedentaryMinutes),
    average_lightly_active = mean(LightlyActiveMinutes),
    average_fairly_active = mean(FairlyActiveMinutes),
    average_very_active = mean(VeryActiveMinutes),
    total_minutes = sum(average_sedentary, average_lightly_active, average_fairly_active, average_very_active)
  ) %>% 
  # Calculate the percentage of time spent in each activity level.
  mutate(
    sedentary_percent = average_sedentary / total_minutes,
    lightly_active_percent = average_lightly_active / total_minutes,
    fairly_active_percent = average_fairly_active / total_minutes,
    very_active_percent = average_very_active / total_minutes
  )

# Create vectors for activity levels and their corresponding percentiles.
activity_levels <- c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")
activity_percentiles <- label_percent()(c(average_activities$sedentary_percent, average_activities$lightly_active_percent, average_activities$fairly_active_percent, average_activities$very_active_percent))

# Create a data frame 'activity_data' to store activity levels and their percentiles.
activity_data <- data.frame(activity_levels, activity_percentiles)

# Display the 'activity_data' data frame.
activity_data

# Create a polar bar plot to visualize the percentages of time spent in each activity level.
ggplot(data = activity_data, aes(x = "", y = activity_percentiles, fill = activity_levels)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Percentages of Time for each Activity Level", fill = "Activity Level") +
  theme_void()  # Use a blank theme for a cleaner visualization.

# Create a summary data frame 'activity_per_user' by grouping data by participant ID.
activity_per_user <- clean_activity %>%
  group_by(Id) %>%
  summarize(
    avg_sedentary = mean(SedentaryMinutes), 
    avg_very_active = mean(VeryActiveMinutes)
  )

# Create a histogram of the average daily sedentary minutes per participant.
hist(
  activity_per_user$avg_sedentary,                  # Data to be plotted
  main = "Average Daily Sedentary Minutes per Participant",  # Title of the histogram
  xlab = "Sedentary Time (Minutes)",                       # X-axis label
  labels = TRUE                                    # Display axis labels
)
# Create another scatter plot using ggplot, mapping SedentaryMinutes to the x-axis and Calories to the y-axis.
ggplot(data = clean_activity, mapping = aes(x = SedentaryMinutes, y = Calories)) +
  geom_point() +  # Add points to the plot
  geom_smooth() +  # Add a smoothed line to the plot
  labs(title = "Sedentary Time vs Calories Burned")  # Add a title to the plot

# Create a summary data frame 'average_reported_minutes' by grouping data by participant ID.
average_reported_minutes <- clean_activity %>%
  group_by(Id) %>%
  summarize(
    average_sedentary = mean(SedentaryMinutes),
    average_lightly_active = mean(LightlyActiveMinutes),
    average_fairly_active = mean(FairlyActiveMinutes),
    average_very_active = mean(VeryActiveMinutes),
    recorded_minutes = sum(average_sedentary, average_lightly_active, average_fairly_active, average_very_active)
  )

# Create a histogram of the total recorded minutes per participant.
hist(
  average_reported_minutes$recorded_minutes,     # Data to be plotted
  main = "Total Recorded Minutes per Participant",  # Title of the histogram
  xlab = "Recorded Minutes",                      # X-axis label
  labels = TRUE                                   # Display axis labels
)
# Create a scatter plot using ggplot, mapping Date to the x-axis and SedentaryMinutes to the y-axis for the first set of points.
ggplot(data = clean_activity) +
  geom_point(mapping = aes(x = Date, y = SedentaryMinutes, colour = "SedentaryMinutes")) + 
  # Add a second set of points to the same plot, mapping Date to the x-axis and VeryActiveMinutes to the y-axis.
  geom_point(mapping = aes(x = Date, y = VeryActiveMinutes, colour = "VeryActiveMinutes")) +
  # Create multiple plots (facets) based on participant ID using facet_wrap.
  facet_wrap(~Id) +
  labs(title = "Sedentary and Very Active Minutes per Participation", y = "Minutes")  # Add a title and y-axis label to the plot

ggplot(data=clean_sleep, mapping=aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) +
  geom_point() +
  geom_smooth()

extra_time_in_bed=clean_sleep$TotalTimeInBed-clean_sleep$TotalMinutesAsleep
ggplot(data=clean_sleep, mapping=aes(x=TotalMinutesAsleep, y=extra_time_in_bed)) +
  geom_point() +
  geom_smooth() +
  labs(title="Extra Time in Bed vs Time Asleep", x="Minutes Asleep", y="Extra Minutes in Bed")

# Calculate summary statistics for the variable 'TotalMinutesAsleep' in the 'clean_sleep' data frame.
time_asleep <- clean_sleep %>% 
  select(TotalMinutesAsleep) %>% 
  summary()

# Create a summary data frame 'sleep_per_user' by grouping data by participant ID and calculating the average minutes asleep.
sleep_per_user <- clean_sleep %>%
  group_by(Id) %>%
  summarize(avg_sleep = mean(TotalMinutesAsleep))

# Create a histogram of the average minutes asleep per participant.
hist(
  sleep_per_user$avg_sleep,               # Data to be plotted
  main = "Average Minutes Asleep per Participant",  # Title of the histogram
  xlab = "Minutes Asleep",                # X-axis label
  labels = TRUE                           # Display axis labels
)

# Display the summary statistics for 'TotalMinutesAsleep'.
time_asleep

# Merge the 'clean_activity' and 'clean_sleep' data frames by the common columns 'Id' and 'Date', including all observations from both data frames.
combined_data <- merge(clean_activity, clean_sleep, by = c("Id", "Date"), all = TRUE)

# Display the first few rows of the merged data frame to inspect the result.
head(combined_data)

ggplot(combined_data) +
  geom_point(mapping=aes(x=SedentaryMinutes, y=TotalMinutesAsleep)) +
  geom_smooth(mapping=aes(x=SedentaryMinutes, y=TotalMinutesAsleep))
ggplot(combined_data) +
  geom_point(mapping=aes(x=VeryActiveMinutes, y=TotalMinutesAsleep)) +
  geom_smooth(mapping=aes(x=VeryActiveMinutes, y=TotalMinutesAsleep))
