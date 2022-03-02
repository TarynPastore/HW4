install.packages(c("dplyr","lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")
average <- function(x){
  x.no = na.omit(x)
  sum(x.no)/length(x.no)
}
average(weather$AirTemp)

weather$dateF <- mdy_hm(weather$Date)

weather$doy <- yday(weather$dateF)

weather$year <- year(weather$dateF)


ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval_times <- int_length(intervals)

intervals[interval_times != 900]

#HomeWork

#Question 1

dt <- subset(weather, AirTemp>0|
               XLevel<2|
               YLevel<2|
               na.omit(precip), 
             select=precip)
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021 &
                              weather$AirTemp < 0 & weather$XLevel > 2 & weather$YLevel > 2, 
          
                            NA, 
                            weather$Precip)
sum(is.na(weather))

#Question 2

weather$Volt_Below_8.5 <- ifelse(weather$BatVolt >= 8.5, # check if at or below zero
                             1, 
                             0) 

#Question 3

Abnormalities <- function(x) {x.no = na.omit(x) 
x.no[x.no <= mean(x.no)-(sd(x.no)*2) | 
  x.no >= mean(x.no)+(sd(x.no)*2)]
}
Abnormalities(weather$AirTemp)
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
Abnormal_AirTemp <- function(x) {x.no = na.omit(x)
x.no <= -25 &
  x.no >= 38}
Abnormal_AirTemp(weather$AirTemp)
Abnormalities(weather$SolRad)
#Question 4

ggplot(data=weather[weather$doy >= 1 & weather$doy < 91 ,],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="magenta")+
  theme_classic()
