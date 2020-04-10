# BWRAO Exploration

library(tidyverse)
library(anytime)
library(zoo)
#library(sentimentr)

c1 <- read.csv("comments_1_1537.csv")
c2 <- read.csv("Comments_5023_6035.csv")

names(c2) <- names(c1)


comments <- rbind(c1,c2)

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
