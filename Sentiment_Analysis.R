suppressMessages(library(ggplot2)) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tm)
suppressMessages(library(wordcloud))
suppressMessages(library(plyr))
suppressMessages(library(lubridate))
suppressMessages(library(syuzhet))


#Import the twitter data set
tweetsdata=read.csv('../input/demonetization-tweets.csv',stringsAsFactors = FALSE)


options(warn=-1)
summary(tweetsdata)


#Addition of New Features

tweetsdata$created_date=as.Date(tweetsdata$created,format='%Y-%m-%d %H:%M:%S')#convert created to date format
tweetsdata$hour = format(as.POSIXct(tweetsdata$created,format="%Y-%m-%d %H:%M:%S"),"%H")#Extract Hour from the date
tweetsdata$isRetweetNum=ifelse(tweetsdata$isRetweet==FALSE,0,1)#Numerical variable to indicate whether a tweet was retweet
tweetsdata$retweetedNum=ifelse(tweetsdata$retweeted==FALSE,0,1)#Total number of times a tweet was tetweeted
tweetsdata$tweet=c(1)#Additional column that will help us in summing up total tweets



#Hour of the Day Trends

options(repr.plot.width=6, repr.plot.height=4)
HourFrame=as.data.frame(table(tweetsdata$hour))
colnames(HourFrame)=c("Hour","TweetCount")
HourFrame$Hour=as.numeric(HourFrame$Hour)
y=ddply(tweetsdata, .(tweetsdata$hour), numcolwise(sum))
HourFrame$retweetedNum=y$isRetweetNum
ggplot(HourFrame,aes(x=Hour))+geom_line(aes(y = TweetCount, colour = "TotalTweets")) + 
  geom_line(aes(y = retweetedNum, colour = "Retweets"))


#Device Breakup

evices=tweetsdata$statusSource
devices <- gsub("","", devices)
devices <- strsplit(devices, ">")
devices <- sapply(devices,function(x) ifelse(length(x) > 1, x[2], x[1]))

devices_source=as.data.frame(table(devices))
colnames(devices_source)=c("Device","TweetCount")
devices_source=devices_source[devices_source$TweetCount>50,]
devices_source=devices_source[order(-devices_source$TweetCount),]

ggplot(devices_source,aes(x=reorder(Device, -TweetCount),y=TweetCount,fill=TweetCount))+geom_bar(stat='identity') +coord_flip()


#Most Popular Users

y=ddply(tweetsdata, .(screenName), numcolwise(sum))
popularUsers=y[,c("screenName","retweetCount","tweet")]
popularUsers=popularUsers[order(-popularUsers$retweetCount),]
popularUsers=head(popularUsers,n=10)
popularUsers

#Most Replies
Replies=tweetsdata[is.na(tweetsdata$replyToSN)==FALSE,]
y=ddply(Replies, .(replyToSN), numcolwise(sum))
Replies=y[,c("replyToSN","tweet")]
Replies=Replies[order(-Replies$tweet),]
Replies=head(Replies,n=20)
colnames(Replies)=c("User","RepliesReceived")
Replies


#Sentiment Analysis

some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweetsdata$text)
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
some_txt<-gsub("@\\w+","",some_txt)
some_txt<-gsub("[[:punct:]]"," ",some_txt)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)


tweetSentiment <- get_nrc_sentiment(some_txt)

barplot(
  sort(colSums(prop.table(tweetSentiment[, 1:8]))), 
  #  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Tweets text", xlab="Percentage"
)