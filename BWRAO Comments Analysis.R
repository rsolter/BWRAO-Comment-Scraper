# BWRAO Exploration

library(tidyverse)
library(anytime)
library(zoo)
library(stringr)
library(scales)

setwd("Personal Git/BWRAO-Comment-Scraper/")
comments<- read.csv("Data/Full_Comments_no_body.csv",stringsAsFactors = F)


## Processing ----

# Formatting Dates
comments$Article_Date <- as.Date(comments$Article_Date)
comments$Comment_Date <- as.Date(comments$Comment_Date)

# Trimming 
comments$Article_Author <- stringr::str_trim(comments$Article_Author)
comments$Comment_Poster <- stringr::str_trim(comments$Comment_Poster)



###############################
# Visualizations

## Comment count over time
comments_time <- comments %>% group_by(Article_Date) %>% tally()
comments_time$trailing_30_avg <- zoo::rollmeanr(comments_time$n,k=30,fill=NA)

ggplot(comments_time) +
  geom_point(aes(x=Article_Date, y=n), alpha=0.4, color="#636363") +
  #geom_line(aes(x=Article_Date,y=n), alpha=0.4) +
  #geom_point(aes(x=Article_Date, y=trailing_30_avg), color="#636363") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_line(aes(x=Article_Date, y=trailing_30_avg), color="#3182bd", size=1) +
  geom_vline(xintercept = as.Date("2008-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2009-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2010-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2011-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2012-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2013-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2014-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=3) +
  theme_minimal() +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #     panel.background = element_blank(), axis.line = element_line(colour = "black"),
  #     axis.text.x = element_text(angle=90)) +
  #ylab("Comment Count") + 
  xlab ("Article Publish Date") +
  labs(title="Comment Count by Publish Date",caption = "30-day Avg. in blue")


## Article count over time
articles_time <- comments %>% 
  select(Article_Title,Article_Date) %>% 
  unique() %>%
  arrange(Article_Date) %>% 
  mutate(Article_count=row_number())
#comments_time$trailing_30_avg <- zoo::rollmeanr(comments_time$n,k=30,fill=NA)

ggplot(articles_time) +
  geom_line(aes(x=Article_Date, y=Article_count), alpha=0.4, color="#636363") +
  #geom_line(aes(x=Article_Date,y=n), alpha=0.4) +
  #geom_point(aes(x=Article_Date, y=trailing_30_avg), color="#636363") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  #geom_line(aes(x=Article_Date, y=trailing_30_avg), color="#3182bd", size=1) +
  geom_vline(xintercept = as.Date("2008-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2009-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2010-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2011-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2012-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2013-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2014-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=3) +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=3) +
  theme_minimal() +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #     panel.background = element_blank(), axis.line = element_line(colour = "black"),
  #     axis.text.x = element_text(angle=90)) +
  #ylab("Comment Count") + 
  xlab ("Article Publish Date") +
  labs(title="Comment Count by Publish Date",caption = "30-day Avg. in blue")
