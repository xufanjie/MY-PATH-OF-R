#Q1
library(dplyr)
library(tidyr)
#read the data in
cleanweather<-read.csv('assignment2/cleanweather.csv')
uncleanweather<-read.csv('assignment2/uncleanweather.csv')
#check the data
glimpse(cleanweather)
glimpse(uncleanweather)
#gather 'x.*' columns
uncleantem1<-gather(uncleanweather,days,values,-X,-year,-month,-measure)
#delet 'X' column
uncleantem2<-uncleantem1[,-1]
#take values in 'measure' as column names and 'values' as values
uncleantem3<-spread(uncleantem2,measure,values)
#load two libraries
library(stringr)
library(lubridate)
#get rid of Xs in day
uncleantem3$days<-str_replace(uncleantem3$days,'X','')
#unite year, month and days
uncleantem4<-unite(uncleantem3,date,year,month,days,sep='-',remove=TRUE)
#convert date column into proper date format
uncleantem4$date<-ymd(uncleantem4$date)
#arrange columns
uncleantem5<-cbind(X=1:nrow(uncleantem4),uncleantem4)
uncleantem6<-select(uncleantem5,X:date,Events,CloudCover,Max.Dew.PointF:WindDirDegrees)
#if we convert PrecipitationIn into numeric, will get a lot NA
#replace T by 0
uncleantem6$PrecipitationIn<-str_replace(uncleantem6$PrecipitationIn,'T','0')
for(i in 4:24){
   uncleantem6[,i]<-as.numeric(uncleantem6[,i])
}
# Find row with Max.Humidity of 1000 (this is the obvious error from data collection) and Change 1000 to 100
filter(uncleantem6,Max.Humidity==1000)
#find 1000 is at the 138th row
uncleantem6[138,'Max.Humidity']<-100
# Replace empty cells in events column with "None"
event<-uncleantem6$Events
for(i in 1:nrow(uncleantem6)){
  if(event[i]==''){
    event[i]='None'
  }
}
uncleantem6$Events<-event
# Change column names
new_colnames<- c("date", "events","cloud_cover","max_dew_point_f", "max_gust_speed_mph", "max_humidity", "max_sea_level_pressure_in", "max_temperature_f", "max_visibility_miles", "max_wind_speed_mph","mean_humidity","mean_sea_level_pressure_in", "mean_temperature_f","mean_visibility_miles","mean_wind_speed_mph","mean_dew_point_f","min_dew_point_f", "min_humidity","min_sea_level_pressure_in","min_temperature_f", "min_visibility_miles","precipitation_in","wind_dir_degrees")
colnames(uncleantem6)<-c('X',new_colnames)
#now the data is cleaned
cleaned_weather<-uncleantem6
#summary
summary(cleaned_weather)
glimse(cleaned_weather)

#Q2
students<-read.csv('assignment2/students.csv')
scoretotal <- function(somedata) {
  #separate grade in to single numbers
  x<- strsplit(as.character(somedata), split = "/")
  #add numbers from each observation together
  y<- sapply(x, function(y) {sum(as.integer(y))})
  return(y)
}
#get the summary table
students %>%
  group_by(Mjob) %>%
  summarize(meanscore=mean(scoretotal(Grades)))