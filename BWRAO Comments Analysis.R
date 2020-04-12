# BWRAO Exploration

library(tidyverse)
library(anytime)
library(zoo)
#library(sentimentr)

setwd("Personal Git/BWRAO-Comment-Scraper/")


links1<-read.table(file="Data/archive_links_1_78.txt")
links2<-read.table(file="Data/archive_links_79_97.txt")
links3<-read.table(file="Data/archive_links_98_end.txt")

links_orig <-rbind(links1,links2,links3) 

#write.csv(links_orig, "Data/links_orig.csv",row.names=F)

comment_files<-list.files(path="Data/",pattern = ".csv")

comment_list <- list()

for(j in 1:length(comment_files)){
  comment_name <- paste("c",j,sep="")
  comment_list[[j]] <- read.csv(paste("Data/",comment_files[j],sep=""))
  names(comment_list[[j]]) <- c("Article_Title","Article_Author","Article_Date","Comment_Title","Comment_Body"
                                ,"Comment_Poster","Comment_Date","Comment_Time","Comment_Recs")
} 

comments <- bind_rows(comment_list)

#write.csv(comments, "Data/comments_orig.csv",row.names=F)

users <- comments$Comment_Poster %>% unique()
write.table(users,"Data/users.txt")

###############################
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
