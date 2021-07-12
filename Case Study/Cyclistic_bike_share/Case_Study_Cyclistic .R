# Install required packages

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/edwin/Documents/Case_Study/Case_Study_Cyclistic/Data_Set/2020_csv") 
#sets your working directory to simplify calls to data

# Upload Divvy datasets (csv files) here
jun_2020 <- read_csv("202006-divvy-tripdata.csv")
jul_2020 <- read_csv("202007-divvy-tripdata.csv")
aug_2020 <- read_csv("202008-divvy-tripdata.csv")
sep_2020 <- read_csv("202009-divvy-tripdata.csv")
oct_2020 <- read_csv("202010-divvy-tripdata.csv")
nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020 <- read_csv("202012-divvy-tripdata.csv")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")

# Compare column name of each table to make sure it matches 
# so that they can be join together
colnames(jun_2020)
colnames(jul_2020)
colnames(aug_2020)
colnames(sep_2020)
colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(apr_2021)
colnames(may_2021)

# # Export quarterly dataframe for future use
# write.csv(q2_2020, "/Users/edwin/Documents/Case_Study/Case_Study_Cyclistic/Data_Set/2020_csv/Divvy_Trips_2020_Q2.csv", row.names = FALSE)
# write.csv(q3_2020, "/Users/edwin/Documents/Case_Study/Case_Study_Cyclistic/Data_Set/2020_csv/Divvy_Trips_2020_Q3.csv", row.names = FALSE)
# write.csv(q4_2020, "/Users/edwin/Documents/Case_Study/Case_Study_Cyclistic/Data_Set/2020_csv/Divvy_Trips_2020_Q4.csv", row.names = FALSE)

# Stacking the quarter dataframe of 2020 together
all_trips <- rbind(jun_2020, jul_2020, aug_2020, sep_2020, 
                   oct_2020, nov_2020, dec_2020, jan_2021, 
                   feb_2021, mar_2021, apr_2021, may_2021)



# Inspecting the new dataframe created
str(all_trips)
glimpse(all_trips)
summary(all_trips)
head(all_trips)

# Rename member_casual column to member_casual
all_trips <- rename(all_trips, user_type = member_casual)

# Explore the dataset
unique(all_trips$user_type)
unique(all_trips$rideable_type)
unique(all_trips$start_station_name)
unique(all_trips$end_station_name)

# Add columns that lists the date, month, day and year to make sure aggregation on different level
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%b")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a column for trip duration in hrs, rounded to 2 decimal points
all_trips$ride_length = round(difftime(all_trips$ended_at, all_trips$started_at)/3600, 2)

# Inspect the all_trips table
summary(all_trips)
glimpse(all_trips)

# Change the ride_length to numeric
all_trips$ride_length <- as.numeric(all_trips$ride_length)

# Remove Bad Data 
# Remove rows with routine bike by head quarter and negative ride_length
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" |
                                   all_trips$ride_length <= 0),]

# Investigating the data frame, NA rows is found
summary(all_trips_v2)

# Investigate the na rows
all_trips_v2[(is.na(all_trips_v2$started_at)),]

# remove na rows
all_trips_v2 <- drop_na(all_trips_v2)
summary(all_trips_v2)
glimpse(all_trips_v2)

#--------------------------------------------------------------------------------
# ride_length into frequency
all_trips_v3 = all_trips_v2 %>%
  mutate(duration_interval = case_when(
    ride_length <= 1 ~ "1) 0hrs ~ 1hrs",
    ride_length >1 & ride_length <= 2.5 ~ "2) 1hrs ~ 2.5hrs",
    ride_length >2.5 & ride_length <= 5 ~ "3) 2.5hrs ~ 5hrs",
    ride_length >5 & ride_length <= 10 ~ "4) 5hrs ~ 10hrs",
    ride_length >10 & ride_length <= 100 ~ "5) 10hrs ~ 100hrs",
    ride_length >100 & ride_length <= 1000 ~ "6) 100hrs ~ 1,000hrs",
    ride_length > 1000 ~ "7) > 10,00hrs")) 

# Add a column to see if start station == end station
all_trips_v3 <- all_trips_v3 %>% 
  mutate(start_equal_end_station = case_when(
    start_station_name == end_station_name ~ "Yes",
    start_station_name != end_station_name ~ "No"
  ))
# exported as csv for visualization in tableau

write.csv(all_trips_v3, "/Users/edwin/Documents/Case_Study/Case_Study_Cyclistic/Data_Set/2020_csv/Divvy_Trips_2020_06_2021_05.csv", row.names = FALSE)

# Aggregate the data for further analysis and visualization 
# Perform descriptive analysis
mean(all_trips_v3$ride_length)
median(all_trips_v3$ride_length)
max(all_trips_v3$ride_length)
min(all_trips_v3$ride_length)

# Perform descriptive analysis for casual vs memeber
aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type, FUN = min)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type, FUN = sum)

aggregate(all_trips_v3$ride_id ~ all_trips_v3$user_type, FUN = length)



# Average ride length by each day of the week for casual vs member
aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type + all_trips_v3$day_of_week, FUN = mean)

# Rearrange the week of days which are out of order
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v3$ride_length ~ all_trips_v3$user_type + all_trips_v3$day_of_week, FUN = mean)
aggregate(all_trips_v3$ride_id ~ all_trips_v3$user_type + all_trips_v3$day_of_week, FUN = length) #Number of casual vs member by weekday

#............................................................................................

# Breakdown numbers of ride and average duration by months for casual vs member
casual_member <- all_trips_v3 %>% 
  group_by(user_type) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length),
            total_duration = sum(ride_length))

casual_member <- casual_member %>% 
  mutate(percent_total_ride = round(prop.table(number_of_rides)*100),
         percent_total_duration = round(prop.table(total_duration)*100))%>% 
  select(user_type, number_of_rides, percent_total_ride, total_duration, percent_total_duration, average_duration)

slices <- casual_member$percent_total_duration 
lbls <- casual_member$user_type
lbls <- paste(lbls, slices, sep=" ") 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Total Ride Duration")

slices <- casual_member$percent_total_ride 
lbls <- casual_member$user_type
lbls <- paste(lbls, slices, sep=" ") 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Total Number of Rides")
# The total duration of all rides by the casual user is higher than member.
# The total number of rides by the casual user is lower than member.
# Thus, casual user tarvel with bike less frequent but much longer than member user.

#............................................................................................
# type of bike use
all_trips_v3 %>% group_by(user_type, rideable_type) %>% 
  summarise(total_ride_number = n(),
            total_ride_duration = sum(ride_length)) %>% 
  mutate(percent_total_ride_number = prop.table(total_ride_number),
         percent_total_ride_duration = prop.table(total_ride_duration))
# docked bike may be the more prefer type or may be there are more docked bike
#............................................................................................

# Breakdown numbers of ride and average duration by weekday for casual vs member
by_weekday_hour <- all_trips_v3 %>% 
  mutate(hour = as.factor(hour(started_at))) %>% 
  group_by(day_of_week, hour, user_type) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),				#calculates the number of rides and average duration 
            average_duration = mean(ride_length),
            total_duration = sum(ride_length)) 

# sum(by_weekday_hour$number_of_rides) # data validation
by_weekday_hour %>% 
  ggplot(aes(x = day_of_week, 
             y = number_of_rides, 
             fill = user_type)) + 
  geom_col() + 
  labs(title = "Number of ride on each weekday",
       caption = "Data from  Jul 2020 to May 2021") +
  facet_wrap(~ user_type) +
  theme(axis.text.x = element_text(angle = 45))

by_weekday_hour %>% 
  ggplot(aes(x = day_of_week, 
             y = total_duration, 
             fill = user_type)) + 
  geom_col() +
  labs(title = "Total trip duration on each weekday",
       caption = "Data from  Jul 2020 to May 2021") +
  facet_wrap(~ user_type) +
  theme(axis.text.x = element_text(angle = 45))
# From this two visual, it is clear that the casual user uses the bike at a slightly higher rate on Friday and a significantly higer number of ride on the weekend.
# The total duration of bike usage is also much higher in the weekend. 
# The bike usage of member very consistent through out the weekday.


by_weekday_hour %>% 
  ggplot(aes(x = hour, 
             y = number_of_rides, 
             fill = user_type)) + 
  geom_col() +
  labs(title = "Breakdwon nubmer of ride by day of Week and time",
       caption = "Data from  Jul 2020 to May 2021") +
  facet_grid(day_of_week~ user_type)
# From the visual, 
# member user shows two peak on the bike usage on weekday, one around 7am and one around 5pm. So member are more likely to be office worker.
# casual user shows one peak on the bike usage on weekday at 5pm. So casual user on weekday could be office worker who choose to return home with bike and taking plublic transport in the morning.

all_trips_v3 %>% 
  group_by(user_type, day_of_week) %>% 
  summarise(total_duration = sum(ride_length),
            total_ride_number = n()) %>% 
  mutate(percent_total_duration = prop.table(total_duration)*100,
         percent_total_ride_number = prop.table(total_ride_number)*100)
# Almost half of total duration traveled by the casual were on weekend.
# Taking this into account, the company could launch a weekend only annual member, weekend or three day pass(including Friday)

#............................................................................................

# Investigate any pattern in the time of the year
by_month_day <- all_trips_v3 %>%
  mutate(month_year = paste(month, year))

by_month_day$month_year <- ordered(by_month_day$month_year, levels=c("Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021"))

by_month_day <- by_month_day %>%
  group_by(user_type, month_year, day) %>% 
  summarise(number_of_rides = n(),						
            average_duration = mean(ride_length),
            total_duration = sum(ride_length)) %>% 	
  arrange(user_type, month_year)
# sum(by_month_day$number_of_rides) # data validation

# Visualise the results 
by_month_day %>% 
  ggplot(aes(x = month_year, 
             y = number_of_rides, 
             fill = user_type)) + 
  geom_col() +
  labs(title = "Average trip duration on each weekday",
       caption = "Data from  Jul 2020 to May 2021") +
  theme(axis.text.x = element_text(angle = 45))

by_month_day %>% 
  ggplot(aes(x = month_year, 
             y = total_duration, 
             fill = user_type)) + 
  geom_col() +
  labs(title = "Average trip duration on each weekday",
       caption = "Data from  Jul 2020 to May 2021") +
  theme(axis.text.x = element_text(angle = 45))
# From the two visuals you can concluded, the usage of share bike peak at July.
# The usage of bike is correlated to the weather of the season. The bike usage is at its lowest due the cold weather giving rider many inconvenient.
# The number of ride sharply increases at March as the spring start and warmer day are coming.
# The number of ride peaked at the start of summer when the temperature and just right for outdoor activity.
# It then slowly decrease after July.

#............................................................................................

# Breakdown of number of ride by duration interval and user type
by_duration_interval <- all_trips_v3 %>%
  group_by(user_type, duration_interval) %>% 
  summarise(number_of_ride = n(),
            total_duration = sum(ride_length)) %>% 
  arrange(user_type) 

by_duration_interval <- by_duration_interval %>% 
  mutate(percent_number_of_ride = round(number_of_ride*100/sum(number_of_ride),6),
         percent_total_duration = round(total_duration*100/sum(total_duration),2))

# Visualise the result
by_duration_interval %>% 
  ggplot(aes(x = user_type, 
             y = percent_number_of_ride, 
             fill = duration_interval)) + 
  geom_col() +
  labs(title = "Percentage breakdwon of number of ride by the ride duration interval",
       caption = "Data from  Jul 2020 to May 2021") 
# 99% of member user's travel duration is less than or equal 1hr.
# 85% of casual user's travel duration is less than or equal 1hr.
#............................................................................................

# Investigating the popular trips  
start_end <- all_trips_v3 %>% 
  group_by(user_type, start_station_name, end_station_name, start_equal_end_station) %>% 
  summarise(number_of_ride = n(),
            average_duration = mean(ride_length),
            total_duration = sum(ride_length)
  ) %>% 
  arrange(desc(number_of_ride), user_type)
# Popular trip, trip with the highest number, seem to start and end at the same station for causal user
# Investigate further to test the hypothesis that the majority of the start and end station is the same for casual user 
test <- start_end %>% arrange(user_type, desc(start_equal_end_station))

# summaries the numbers in percentage for better comparison 
summarise_start_end <- all_trips_v3 %>% 
  group_by(user_type, start_equal_end_station) %>% 
  summarise(total_duration = sum(ride_length),
            number_of_ride = n()) %>% 
  mutate(percent_total_duration = round(prop.table(total_duration)*100,2),
         percent_number_of_ride = round(prop.table(number_of_ride)*100,2)) %>% 
  select(user_type, start_equal_end_station, total_duration, percent_total_duration, number_of_ride, percent_number_of_ride)
summarise_start_end
# sum(summarise_start_end$number_of_ride)
# 

# ....................................................................................................................

# Build one data frame to compare the number of time each station is use, both start and end.
# Popular_start_station for casual user
s_x <- all_trips_v3 %>% 
  group_by(start_station_name) %>% 
  filter(user_type == "casual") %>% 
  summarise(start_station_count_casual = n() ) %>%
  arrange(-start_station_count_casual) # desc

# Popular_start_station for member user
s_y <- all_trips_v3 %>% 
  group_by(start_station_name) %>% 
  filter(user_type == "member") %>% 
  summarise(start_station_count_member = n() ) %>%
  arrange(-start_station_count_member) # desc

# full join to obtain usage of all start_station
start_station <- full_join(s_x,s_y, by="start_station_name")
start_station <- rename(start_station, station_name = start_station_name)# rename for joining table 

# Popular_end_station for casual user
e_x <- all_trips_v3 %>% 
  group_by(end_station_name) %>% 
  filter(user_type == "casual") %>% 
  summarise(end_station_count_casual = n() ) %>% 
  arrange(-end_station_count_casual) # desc]

# Popular_end_station for member user
e_y <- all_trips_v3 %>% 
  group_by(end_station_name) %>% 
  filter(user_type == "member") %>% 
  summarise(end_station_count_member = n() ) %>% 
  arrange(-end_station_count_member) # desc

# full join to obtain usage of all end_station
end_station <- full_join(e_x,e_y, by="end_station_name")
end_station <- rename(end_station, station_name = end_station_name) # rename for joining table 

# full join to obtain usage of all station
start_end_station_count <- full_join(start_station, end_station, by="station_name")
start_end_station_count <- select(start_end_station_count, station_name, #Rearrange the column order
                                  start_station_count_casual, end_station_count_casual, 
                                  start_station_count_member, end_station_count_member)

# Investigating the new data frame and validation the data
summary(start_end_station_count)
# check <- 
sum(start_end_station_count$start_station_count_casual, na.rm= TRUE) + 
sum(start_end_station_count$start_station_count_member, na.rm= TRUE) # data validation # na.rm



# The top popular stations of casual user and member is very different.
# The top popular start and end station of casual user is highly similar.
# The top popular start and end station of member user is highly similar.
casual_top_20 <- head(start_end_station_count,20)
round(sum(casual_top_20$start_station_count_casual)/sum(start_end_station_count$start_station_count_casual,na.rm= TRUE)*100,2)
round(sum(casual_top_20$start_station_count_member)/sum(start_end_station_count$start_station_count_member,na.rm= TRUE)*100,2)
# ~20% of ride start from the top 20 popular station. 
# Marketing plan could focus on casual user in these area.

# ....................................................................................................................

#Investigate how different are the popular start and end stations for casual and member user
casual_start_rank <- arrange(start_end_station_count,-start_station_count_casual)
member_start_rank <- arrange(start_end_station_count,-start_station_count_member)
casual_end_rank <- arrange(start_end_station_count,-end_station_count_casual)
member_end_rank <- arrange(start_end_station_count,-end_station_count_member)

# Comparing the start and end station usage of casual user
# casual user: Top 20 popular start and end stations(rank by number of trip count)
x = 20 # number of popular stations

# create data frame with top x number of start station name 
start_station <- casual_start_rank[,1] #station name
top_start_station <- start_station[1:x,] # top x number of station 
top_start_station <- gsub(" ", "_", top_start_station$station_name)
print(top_start_station)

# create data frame with top x number of end station name 
end_station <- casual_end_rank[,1]
top_end_station <- end_station[1:x,]
top_end_station <- gsub(" ", "_", top_end_station$station_name)
print(top_end_station)

# create a vector to see the difference between popular start and end stations
similar <- c() # empty vector
for (i in 1:x) {
  if(top_start_station[i] %in% top_end_station){ # if element [i] of start_station_name is in end stations list
    similar <- c(similar, "TRUE") # add "TRUE" to the vector
  }else(similar <- c(similar, "FALSE")) # else add "FALSE" to the vector
} 
print(similar)
# From the results it can be conclude that the popular locations casual user like to visit is within a certain area.
# ....................................................................................................................

# Repeat the same with member
# member user: Top 20 popular start and end stations(rank by number of trip count)
x = 20

start_station <- member_start_rank[,1]
top_start_station <- start_station[1:x,] 
top_start_station <- gsub(" ", "_", top_start_station$station_name)
print(top_start_station)


end_station <- member_end_rank[,1]
top_end_station <- end_station[1:x,]
top_end_station <- gsub(" ", "_", top_end_station$station_name)
print(top_end_station)

similar <- c()
for (i in 1:x) {
  if(top_start_station[i] %in% top_end_station){
    similar <- c(similar, "TRUE")
  }else(similar <- c(similar, "FALSE"))
}
print(similar)
# From the results it can be conclude that the popular locations casual user like to visit is within a certain area.
# ...........................................................................................

x = 20

start_station <- start_end[,2]
start_station
top_start_station <- start_station[1:x,] 
top_start_station
top_start_station <- gsub(" ", "_", top_start_station$start_station_name)
print(top_start_station)


end_station <- casual_end_rank[,1]
top_end_station <- end_station[1:x,]
top_end_station <- gsub(" ", "_", top_end_station$station_name)
print(top_end_station)

similar <- c()
for (i in 1:x) {
  if(top_start_station[i] %in% top_end_station){
    similar <- c(similar, "TRUE")
  }else(similar <- c(similar, "FALSE"))
}
print(similar)
# From the results it can be conclude that the popular locations casual user like to visit is within a certain area.
# ...........................................................................................

# Do member and casual user likely to travel in the same area.
x = 20

# casual
start_station <- casual_start_rank[,1]
top_start_station <- start_station[1:x,] 
top_start_station <- gsub(" ", "_", top_start_station$station_name)
print(top_start_station)

member
end_station <- member_start_rank[,1]
top_end_station <- end_station[1:x,]
top_end_station <- gsub(" ", "_", top_end_station$station_name)
print(top_end_station)

similar <- c()

for (i in 1:x) {
  if(top_start_station[i] %in% top_end_station){
    similar <- c(similar, "TRUE")
  }else(similar <- c(similar, "FALSE"))
}
# similar <- c(similar, "aaa")
similar <- data_frame(similar = similar)
similar %>% group_by(similar) %>% summarise(count = n())
# The popular area of member and casual user like to visit overlap partly.
