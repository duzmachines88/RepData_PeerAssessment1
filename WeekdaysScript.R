library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(lubridate)
library(chron)

data<-read.csv("./Activity Monitoring/activity.csv", header=TRUE)

data$date <- as.Date(strptime(data$date, format="%Y-%m-%d"))


impute<-mice(data,m=5, maxit =40)
imputedata<-complete(impute, 5)




imputedata$weekends<-is.weekend(imputedata$date)


datagroupweekend<- imputedata %>%  
  group_by(interval, weekends) %>% 
  summarise(sum= mean(steps, na.rm=TRUE))%>%
  mutate(weekday = case_when(weekends==FALSE ~ "Weekday", weekends==TRUE ~ "Weekend")) 


g<-ggplot(datagroupweekend, aes(interval, sum))+
  geom_line(aes(colour=factor(as.factor(datagroupweekend$weekday)))) +
  labs(x="Interval", y="Steps", title="Total Steps by Interval", colour="Weekend or Weekday") + 
  facet_grid(weekday~.)
  
  