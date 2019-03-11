#Load libraries
library(jsonlite)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)
library(plyr)

#Donald Trump's Twitter data from 2017
trump_tweets <- read_json("data/2017_Trump_Twitter.json", simplifyVector = TRUE)
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
print(average_12_31)

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
  summarize(approval_rating = mean(approve))

# A joined data frame with info from both of the above data frames
daily_approval_and_frequency <- inner_join(trump_tweet_frequency, trump_daily_approval, by = "Date")
#View(daily_approval_and_frequency)

approval_and_frequency_correlation <- cor(daily_approval_and_frequency$approval_rating, daily_approval_and_frequency$Frequency)
#View(approval_and_frequency_correlation)


# Trump Twitter Analysis

twitter_data <- as.data.frame(t(as.data.frame(strsplit(trump_tweets$created_at, " "), stringsAsFactors = FALSE)), stringsAsFactors = FALSE) 
twitter_data <- twitter_data %>% 
  mutate(created_at = trump_tweets$created_at) %>% 
  left_join(trump_tweets, by = "created_at") %>% 
  select(V2, V3, V6, V4, created_at, text, retweet_count, favorite_count, is_retweet)
twitter_data <- twitter_data %>% 
  mutate(month = match(twitter_data$V2, month.abb), day = V3, year = V6, time = V4) %>% 
  select(month, day, year, time, created_at, text, retweet_count, favorite_count, is_retweet)

monthly_tweets <- group_by(twitter_data, month) %>% 
  count() %>% 
  as.data.frame(stringAsVariable = FALSE) %>% 
  mutate("Month" = month.name[month],
         "Number of Tweets" = n) %>% 
  select("Month", "Number of Tweets")

monthly_tweet_count <- group_by(twitter_data, month) %>% 
  count()

get_monthly_info <- function(given_month) {
  tweets <- monthly_tweet_count$n[monthly_tweet_count$month == given_month]
  date <- month.name[given_month]
  
  avg_data <- twitter_data %>% 
    filter(month == given_month) %>% 
    summarize(
      num_days = as.numeric(max(day)),
      total_tweets = as.numeric(n()),
      most_retweeted = text[retweet_count == max(retweet_count)][1],
      least_retweeted = text[retweet_count == min(retweet_count)][1],
      most_favorited = text[favorite_count == max(favorite_count)][1],
      least_favorited = text[favorite_count == min(favorite_count)][1]
    )
  
  avg_num_tweets <- round(avg_data$total_tweets / avg_data$num_days)
  paste0("There were ", tweets, " tweets in ", date, " 2017, with an average of ", avg_num_tweets, " tweets per day.")
}

