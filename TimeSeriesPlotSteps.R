library(dplyr)
library(ggplot2)
library(tidyr)

data<-read.csv("./Activity Monitoring/activity.csv", header=TRUE)

datagroup2<- data %>% 
  group_by(interval) %>% 
  summarise(Step_Avg= mean(steps, na.rm=TRUE))



g<-ggplot(datagroup2, aes(x=interval, y=Step_Avg))+geom_line()+labs(x="Interval",y="Avg Steps" )


print(g)

max<-which.max(datagroup2$Step_Avg)

imax<-datagroup2[max,1]

print(imax)