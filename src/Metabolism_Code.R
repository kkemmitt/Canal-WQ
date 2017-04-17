setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")
library(dplyr)
library(tidyr)
library(ggplot2)
library(streamMetabolizer)

dat<-read.csv("DO_prelim_allsites.csv")
dat$Reading.Date<-as.POSIXct(dat$Reading.Date, format = "%m/%d/%Y %H:%M", tz='Etc/GMT+7')
  lubridate::tz(dat$Reading.Date)#checking that time is in right tz
  (dat$posix.time.solar <- streamMetabolizer::calc_solar_time(dat$Reading.Date, longitude=-106.3))
#longtitude is that of UTC, keep consistent
  
  x_min=min(dat$posix.time.solar[dat$Sensor.Name=="Wql_010161_Probe1.DO"])
  x_min=as.POSIXct("2017-02-15 00:00:00 UTC")
  x_max=max(dat$posix.time.solar)
    ggplot(dat, aes(x=posix.time.solar, y=Sensor.Reading, color=Sensor.Name))+
      geom_line()+
      scale_x_datetime(limits=c(x_min, x_max))+
      labs(x="Solar Time", y="Dissolved Oxygen (%)")
    
    
    
  