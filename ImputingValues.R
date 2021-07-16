library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)

data<-read.csv("./Activity Monitoring/activity.csv", header=TRUE)

impute<-mice(data,m=5, maxit =40, seed=245435)
imputedata<-complete(impute, 5)

Nas<-sum(is.na(data$steps))
print(Nas)



datagroup3<- imputedata %>% 
  group_by(date) %>% 
  summarise(sum= sum(steps, na.rm=TRUE))

datagroup4<- imputedata %>% 
  group_by(interval) %>% 
  summarise(avg= mean(steps, na.rm=TRUE))

g<-qplot(datagroup3$sum, geom="histogram", binwidth=2500, col=I("black"), fill=I("green"))+
  geom_vline(aes(xintercept=mean(datagroup3$sum), color="mean"), show.legend=TRUE, linetype="dashed")+
  geom_vline(aes(xintercept=median(datagroup3$sum), color="median"), show.legend=TRUE)+
  labs(x="Daily Steps", y="Frequency", title="Histogram of Steps per Day") +
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "black"))
t<-ggplot(datagroup4, aes(x=interval, y=avg))+geom_line()+labs(x="Interval",y="Avg Steps" )


g
t

mean(datagroup$sum, na.rm=TRUE)
median(datagroup$sum, na.rm=TRUE)