# Install and load the 'tidyverse' package, which includes several data manipulation and visualization libraries.
install.packages("tidyverse")
library(tidyverse)

# Read a CSV file named "Averarage_Total_Steps.csv" into a data frame named 'daily_activity_df'.
daily_activity_df <- read_csv("Averarage_Total_Steps.csv")

# Display the first few rows of the 'daily_activity_df' data frame.
head(daily_activity_df)

# Display a concise summary of the 'daily_activity_df' data frame.
glimpse(daily_activity_df)

# Display the column names of the 'daily_activity_df' data frame.
colnames(daily_activity_df)

# Create a new data frame 'daily_activity_lifestyle_df' by adding a column 'lifestyle' based on conditions using the 'mutate' function.
daily_activity_lifestyle_df <- daily_activity_df %>% 
  mutate(lifestyle = 
           case_when(`Average_TotalSteps` < 5000 ~ "Sedentary",
                     `Average_TotalSteps` >= 5000 & `Average_TotalSteps` < 7500 ~ "Light Active",
                     `Average_TotalSteps` >= 7500 & `Average_TotalSteps` < 10000 ~ "Moderately Active",
                     `Average_TotalSteps` >= 10000 ~ "Very Active"))

# Display the first few rows of the 'daily_activity_lifestyle_df' data frame.
head(daily_activity_lifestyle_df)

# Create a new data frame 'lifestyle_count' to count the occurrences of each lifestyle category.
lifestyle_count <- daily_activity_lifestyle_df %>% 
  count(lifestyle) 

# Create another data frame 'lifestyle_count_percent' to calculate the percentage of users in each lifestyle category.
# scales::label_percent(): This part of the code calls the label_percent function from the scales package.
lifestyle_count_percent <- lifestyle_count %>% 
  mutate(percent = scales::label_percent()(n / sum(n)))

# Display the first few rows of the 'lifestyle_count_percent' data frame.
head(lifestyle_count_percent)

# Plot the bar chart 

ggplot(lifestyle_count_percent, aes(x=lifestyle, y= percent)) + 
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ,stat = "identity")+
  coord_flip()


# Prepare labels for a pie chart with lifestyle categories and their corresponding percentages.
lbls <- paste(lifestyle_count_percent$lifestyle, "\n", lifestyle_count_percent$percent, sep="")

# Create a pie chart using 'pie' function with lifestyle count percentages.
pie(lifestyle_count_percent$n, labels = lbls, clockwise = TRUE, main = "Representation of users lifestyle")

# Read a CSV file named "dailyIntensities_merged_with_Cal.csv" into a data frame named 'daily_intensity_df'.
daily_intensity_df <- read_csv("dailyIntensities_merged_with_Cal.csv")

# Display the first few rows of the 'daily_intensity_df' data frame.
head(daily_intensity_df)

# Create a new data frame 'daily_active_minutes_df' by adding a column 'total_activity' as the sum of active minutes.
daily_active_minutes_df <- daily_intensity_df %>% 
  mutate(total_activity = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes)

# Display the first few rows of the 'daily_active_minutes_df' data frame.
head(daily_active_minutes_df)

# Load the 'ggplot2' library for creating visualizations and create a scatter plot with a smoothed line.
ggplot(data = daily_active_minutes_df, mapping = aes(x = total_activity, y = Calories)) +

  geom_jitter() +                   # Add jittered points for better visibility.
  geom_smooth() +                   # Add a smoothed line.
  labs(title = "Total Activity Vs Calories Burned" )  # Add title to the plot.
