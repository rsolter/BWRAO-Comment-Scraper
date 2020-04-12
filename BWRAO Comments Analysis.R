# BWRAO Exploration

library(tidyverse)
library(anytime)
library(zoo)
#library(sentimentr)

setwd("Personal Git/BWRAO-Comment-Scraper/")


#links1<-read.table(file="Data/archive_links_1_78.txt")
#links2<-read.table(file="Data/archive_links_79_97.txt")
#links3<-read.table(file="Data/archive_links_98_end.txt")
#links_orig <-rbind(links1,links2,links3) 
#write.csv(links_orig, "Data/links_orig.csv",row.names=F)


comment_files<-list.files(path="Data/",pattern = ".csv")

comment_list <- list()

for(j in 1:length(comment_files)){
  comment_name <- paste("c",j,sep="")
  comment_list[[j]] <- read.csv(paste("Data/",comment_files[j],sep=""))
  names(comment_list[[j]]) <- c("Article_Title","Article_Author","Article_Date","Comment_Title","Comment_Body"
                                ,"Comment_Poster","Comment_Date","Comment_Time","Comment_Recs")
} 

comments1 <- bind_rows(comment_list[1:2])

c1<-comments1[1:100000,]
c2<-comments1[100001:200000,]
c3<-comments1[200001:300000,]
c4<-comments1[300001:400000,]
c5<-comments1[400001:500000,]
c6<-comments1[500001:630292,]

write.csv(c1, "Data/Comments1.csv",row.names=F)
write.csv(c2, "Data/Comments2.csv",row.names=F)
write.csv(c3, "Data/Comments3.csv",row.names=F)
write.csv(c4, "Data/Comments4.csv",row.names=F)
write.csv(c5, "Data/Comments5.csv",row.names=F)
write.csv(c6, "Data/Comments6.csv",row.names=F)

users <- comments1$Comment_Poster %>% unique()
write.table(users,"Data/users.txt")



###############################
# Formatting Dates
comments <- comments1

comments$Article.Publish.Date <- as.character(comments$Article.Publish.Date)
comments$Article.Publish.Date <- anytime::anytime(comments$Article.Publish.Date)
comments$Article.Publish.Date <- as.Date(comments$Article.Publish.Date)

comments$Article.Publish.Year <- lubridate::year(comments$Article.Publish.Date)
comments$Article.Publish.Month <- lubridate::month(comments$Article.Publish.Date)

comments$Comment.Date <- as.character(comments$Comment.Date)
comments$Comment.Date <- anytime::anytime(comments$Comment.Date)
comments$Comment.Date <- as.Date(comments$Comment.Date)

comments$Comment.Body <- as.character(comments$Comment.Body)


###############################
# Visualizations

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

