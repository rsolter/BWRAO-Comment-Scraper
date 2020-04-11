# BWRAO Exploration

library(tidyverse)
library(anytime)
library(zoo)
#library(sentimentr)

<<<<<<< HEAD
c1 <- read.csv("tmp/comments_1_1537.csv")
c2 <- read.csv("tmp/Comments_1538_1562.csv")
c3 <- read.csv("tmp/Comments_1562_1617.csv")
c4 <- read.csv("tmp/Comments_1618_2338.csv")
c5 <- read.csv("tmp/Comments_3905_5022.csv")
c6 <- read.csv("tmp/Comments_5023_6035.csv")
=======
c1 <- read.csv("Data/comments_1_1537.csv")
c2 <- read.csv("Data/Comments_5023_6035.csv")
>>>>>>> 1ada4d6d0b44c1387fbaf83eae2f00b5d3c3d6d6

names(c5) <- names(c1)
names(c6) <- names(c1)

comments <- rbind(c1,c2,c3,c4,c5,c6)

#write.csv(comments, "comments_1_1537.csv",row.names=F)

# Formatting Dates

comments$Article.Publish.Date <- as.character(comments$Article.Publish.Date)
comments$Article.Publish.Date <- anytime::anytime(comments$Article.Publish.Date)
comments$Article.Publish.Date <- as.Date(comments$Article.Publish.Date)

comments$Article.Publish.Year <- lubridate::year(comments$Article.Publish.Date)
comments$Article.Publish.Month <- lubridate::month(comments$Article.Publish.Date)

comments$Comment.Date <- as.character(comments$Comment.Date)
comments$Comment.Date <- anytime::anytime(comments$Comment.Date)
comments$Comment.Date <- as.Date(comments$Comment.Date)

comments$Comment.Body <- as.character(comments$Comment.Body)

write.csv(comments,file = "Processed_comments.csv",row.names = F)

## Comment count over time


comments_time <- comments %>% group_by(Article.Publish.Date) %>% tally()
comments_time$trailing_30_avg <- zoo::rollmeanr(comments_time$n,k=30,fill=NA)

time_chart<-ggplot(comments_time) +
  geom_point(aes(x=Article.Publish.Date, y=n), alpha=0.4) +
  geom_line(aes(x=Article.Publish.Date,y=n), alpha=0.4) +
  geom_point(aes(x=Article.Publish.Date, y=trailing_30_avg), color="dark red") +
  geom_line(aes(x=Article.Publish.Date, y=trailing_30_avg), color="dark red") +
  theme_minimal() + ylab("Comment Count") +
  ggtitle("Daily Count of Comments",subtitle = "30-day avg. in brown")

# top commentors
top_commentors <- comments %>% group_by(Comment.Poster) %>% tally() %>% arrange(-n) %>% mutate(Perc=n/nrow(comments)*100)
