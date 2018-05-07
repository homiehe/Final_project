library(tuber)
library(dplyr)
library(tidytext)
library(jsonlite)
library(tidyverse)
library(scales)
library(tm)


####################################
# 1 load the data              #####
####################################

# get the id and key for youtube api
ID <- "850273028006-td8fpgupcbjgr3h2jkm8ncimkjauehcs.apps.googleusercontent.com"
secrete <- "he-T79o-yif9EKImit-5h8li"
p <- yt_oauth(ID,secrete,token='')

# 2018 yt comments on missile strike against Syria.

comment_CBSN_news_2018 <- get_all_comments(video_id = c('PC_2e8W5qho'))

comment_fox_news_2018 <- get_all_comments(video_id = c('GEO7BS4CW9s'))

comment_BBC_news_2018 <- get_all_comments(video_id = c('lFzjHpjb3y8'))

# 2014 yt comments on missile strike against Syria

comment_CBSN_news_2014 <- get_all_comments(video_id = c('etwRqC8xAA8'))

comment_fox_news_2014 <- get_all_comments(video_id = c('6SPzfGFOJ84'))

comment_BBC_news_2014 <- get_all_comments(video_id = c('zYX4IxZi54Q'))

comment_rt_news_2014 <- get_all_comments(video_id = c('BLhKHFGBUEg'))

comment_reblop_news_2014 <- get_all_comments(video_id = c('_sorHHNEyK8'))

comment_VICE_news_2014 <- get_all_comments(video_id = c('AUjHb4C7b94'))

comment_CNN_news_2014 <- get_all_comments(video_id = c('FR5HBkA3XjA'))

#Data rocessing

process <- function(i){
  a <- i %>% select(textOriginal,publishedAt,likeCount) %>% as.tibble()
  a <- a %>% 
    separate(publishedAt, into = c("date","time"),sep="T") %>% 
    separate(time,into = c("time","x"),sep=".000Z") %>% 
    unite(date,date,time,sep=" ") %>% 
    select(-x)
  a$date <- strptime(a$date, format="%Y-%m-%d %H:%M:%S")
  a
}

comment_2018_1 <- process(comment_CBSN_news_2018)

comment_2018_2 <- process(comment_fox_news_2018)

comment_2018_3 <- process(comment_BBC_news_2018)

comment_2018 <- rbind(comment_2018_1,comment_2018_2,comment_2018_3)

comment_2014_1 <- process(comment_CBSN_news_2014)

comment_2014_2 <- process(comment_fox_news_2014)

comment_2014_3 <- process(comment_BBC_news_2014)

comment_2014_4 <- process(comment_rt_news_2014)

comment_2014_5 <- process(comment_reblop_news_2014)

comment_2014_6 <- process(comment_VICE_news_2014)

comment_2014_7 <- process(comment_CNN_news_2014)

comment_2014 <- rbind(comment_2014_1,comment_2014_2,comment_2014_3,comment_2014_4,comment_2014_5,comment_2014_6,comment_2014_7)

# combine the comment data set with two year

comment_2014 <- comment_2014 %>% mutate(year = 2014)

comment_2018 <- comment_2018 %>% mutate(year = 2018)

comment <- rbind(comment_2014,comment_2018)

####################################
# 2 clean the data             #####
####################################

# Remove URLs and anything other than English letters or space, unify the using of word

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
removeSpace <- function(x) gsub("[\n]", " ", x)
changeWord <- function(a,b,x) gsub(a, b, x)

comment$textOriginal <- comment$textOriginal %>% 
  removeURL() %>% removeNumPunct() %>% removeSpace() %>% tolower() 

comment$textOriginal <- changeWord(a = "trumps" , b= "trump" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "russian" , b= "russia" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "russias" , b= "russia" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "american", b = "america" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "americas", b = "america" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "usa", b = "america" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "fucked", b = "fuck" , x=comment$textOriginal)
comment$textOriginal <- changeWord(a = "fucking", b = "fuck" , x=comment$textOriginal)
comment$likeCount<-as.numeric(comment$likeCount)

# divide the data by like count           

comment <- comment %>%
  mutate(popularity = 
           ifelse(comment$likeCount > 200, "very popular", 
                  ifelse(comment$likeCount > 50, "popular",
                         ifelse(comment$likeCount > 1, "good","normal"))))

# divide the data by date

comment1 <- comment

comment1$date <- as.Date(comment1$date)

a1 <- comment1 %>% filter(year == 2014) %>% filter(date <= "2014-9-26" & date >= "2014-9-23") %>% mutate(time="1")
a2 <- comment1 %>% filter(year == 2014) %>% filter(date <= "2014-9-30" & date >= "2014-9-27") %>% mutate(time="2")
a3 <- comment1 %>% filter(year == 2014) %>% filter(date <= "2014-10-4" & date >= "2014-10-1") %>% mutate(time="3")
a4 <- comment1 %>% filter(year == 2014) %>% filter(date <= "2014-10-8" & date >= "2014-10-5") %>% mutate(time="4")
a5 <- comment1 %>% filter(year == 2014) %>% filter(date <= "2014-10-13" & date >= "2014-10-9") %>% mutate(time="5")

comment_14 <- rbind(a1,a2,a3,a4,a5)

b1 <- comment1 %>% filter(year == 2018) %>% filter(date <= "2018-4-14" & date >= "2018-4-11") %>% mutate(time="1")
b2 <- comment1 %>% filter(year == 2018) %>% filter(date <= "2018-4-18" & date >= "2018-4-15") %>% mutate(time="2")
b3 <- comment1 %>% filter(year == 2018) %>% filter(date <= "2018-4-22" & date >= "2018-4-19") %>% mutate(time="3")
b4 <- comment1 %>% filter(year == 2018) %>% filter(date <= "2018-4-26" & date >= "2018-4-23") %>% mutate(time="4")
b5 <- comment1 %>% filter(year == 2018) %>% filter(date <= "2018-5-1" & date >= "2018-4-27") %>% mutate(time="5")

comment_18 <- rbind(b1,b2,b3,b4,b5)

comment_new <- rbind(comment_14,comment_18)

# Get tokenized data
token_new <- comment_new %>% unnest_tokens(word,textOriginal)

token <- comment %>% unnest_tokens(word,textOriginal)

token$date <- as.Date(token$date)

data(stop_words)

# clean the stop words

token <- token %>%
  anti_join(stop_words)

token_new <- token_new %>%
  anti_join(stop_words)


####################################
# 3 save the data              #####
####################################

write.csv(comment,"comment1.csv")
write.csv(token,"token1.csv")
write.csv(comment_new,"comment_new.csv")
write.csv(token_new,"token_new.csv")

comment_new %>% filter(year==2014)
