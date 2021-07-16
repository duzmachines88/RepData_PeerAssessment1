#code to create folder in current working directory
if(!file.exists("./Activity Monitoring")){dir.create("./Activity Monitoring")}

##variable to save the url to data  
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

##downloadfile from URL and store in folder Activity Monitoring
download.file(fileurl,destfile="./Activity Monitoring/AMdata.zip", method = "curl")

##unzip folder command
unzip(zipfile="./Activity Monitoring/AMdata.zip", exdir="./Activity Monitoring")


data<-read.csv("./Activity Monitoring/activity.csv", header=TRUE)
