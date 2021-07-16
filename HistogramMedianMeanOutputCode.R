library(dplyr)
library(ggplot2)
library(tidyr)

data<-read.csv("./Activity Monitoring/activity.csv", header=TRUE)

datagroup<- data %>% 
  group_by(date) %>% 
  summarise(sum= sum(steps, na.rm=TRUE))


g<-qplot(datagroup$sum, geom="histogram", binwidth=2500, col=I("black"), fill=I("green"))+
  geom_vline(aes(xintercept=mean(datagroup$sum), color="mean"), show.legend=TRUE, linetype="dashed")+
  geom_vline(aes(xintercept=median(datagroup$sum), color="median"), show.legend=TRUE)+
  labs(x="Daily Steps", y="Frequency", title="Histogram of Steps per Day") +
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "black"))
  
print(g)

print(mean(datagroup$sum, na.rm=TRUE))
print(median(datagroup$sum, na.rm=TRUE))