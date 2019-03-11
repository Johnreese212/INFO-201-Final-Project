#Load libraries
library(jsonlite)
library(httr)
library(dplyr)
library(stringr)

#Donald Trump's Twitter data from 2017
trump_tweets <- read_json("data/2017_trump_twitter.json", simplifyVector = TRUE)
trump_tweets <- trump_tweets %>% 
  select(created_at,
         text,
         retweet_count,
         favorite_count, 
         is_retweet)

#Donald Trump's approval rating
trump_approval <- read.csv("data/approval_polllist.csv", stringsAsFactors = FALSE)
trump_approval <- trump_approval %>% 
  select(poll_id,
         startdate, 
         enddate, 
         pollster,
         grade,
         samplesize,
         approve,
         disapprove,
         url)

#Preliminary Analysis

summary_twitter <- trump_tweets %>% 
  summarize(
    "Total Tweets" = nrow(trump_tweets),
    "Tweets from Trump" = nrow(trump_tweets) - sum(is_retweet),
    "Average Retweet Count" = round(mean(retweet_count), digits = 0),
    "Average Favorite Count" = mean(favorite_count)
  )

#General Averages
# Returns the most common value from data set -- adapted from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
find_mode <- function(data) {
  unique <- unique(data)
  unique[which.max(tabulate(match(data, unique)))]
}

average_tweets <-mean(trump_tweets$retweet_count)
pollsters <- as.data.frame(unique(trump_approval$pollster))
summary_approval <- trump_approval %>% 
  summarize(
    "Total Pollsters" = nrow(pollsters),
    "Most Common Grade" = find_mode(grade),
    "Average Approval" = round(mean(approve), digits = 1),
    "Max Approval" = round(max(approve), digits = 1),
    "Min Approval" = round(min(approve), digits = 1),
    "Standard Deviation of Approval" = round(sd(approve), digits = 1),
    "Average Disapproval" = round(mean(disapprove), digits = 1),
    "Max Dissapproval" = round(max(disapprove), digits = 1),
    "Min Dissapproval" = round(min(disapprove), digits = 1),
    "Standard Deviation of Dissapproval" = round(sd(disapprove), digits = 1)
  )

#Answering question #3

average_12_31<- trump_approval[1343:1345,]
average_12_31<- mean(average_12_31$approve)


#Answering Our Question #4

# A data frame representing the number of daily tweets during 2017 by President Trump
trump_tweet_frequency <- trump_tweets %>% 
  mutate(
    Date = as.Date(trimws(paste(substr(created_at,4, 10), "2017")), "%b %d %Y")
    , num = 1
    ) %>% 
  group_by(Date) %>% 
  summarize(Frequency = sum(num)) 

# A data frame representing Trump's average daily approval ratings during 2017.
# The date in the new data frame is the same as the 'enddate' in the trump_approval data frame.
trump_daily_approval <- trump_approval %>% 
  mutate(
    Date = as.Date(enddate, "%m/%d/%y")
  ) %>% 
  group_by(Date) %>% 
  summarize(approve = mean(approve),disapprove = mean(disapprove))

# A joined data frame with info from both of the above data frames
daily_approval_and_frequency <- inner_join(trump_tweet_frequency, trump_daily_approval, by = "Date")


approval_and_frequency_correlation <- cor(daily_approval_and_frequency$approve, daily_approval_and_frequency$Frequency)







