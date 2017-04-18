setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")
library(streamMetabolizer)
prac<-read.csv("Sensor.Readings.Presssure.csv")
prac$Reading.Date<-as.POSIXct(prac$Reading.Date, format = "%m/%d/%Y %H:%M", tz='Etc/GMT+7')
lubridate::tz(prac$Reading.Date)#checking that time is in right tz
(prac$solar.time <- streamMetabolizer::calc_solar_time(prac$Reading.Date, longitude=-106.3))
names(prac)[names(prac)=="DO"] <- "perc.sat" 
names(prac)[names(prac)=="Temp"] <- "temp"
names(prac)[names(prac)=="press.avg.mbar"] <- "measured.atmP"
prac$perc.sat[prac$perc.sat < 0] <- NA # get rid of any negative perc.sat values

#####for loop to calculate 100 percent oxygen sat concentration####
#too big to run all at once so subset data by site to###
R13 <- prac[ which(prac$Sensor.Name=='Wql_010001_Probe1'),] # is site name used by CAP

attach(R13)
salinity=3 ##guestimate of freshwater salinity
perc.sat=100
o2.sat.100<-c()

for(i in 1:nrow(R13)){
  
  temp[i]<-R13[i,]$temp
  measured.atmP[i]<-R13[i,]$measured.atmP
  o2.sat.100<-O2.saturation(salinity, temp, measured.atmP, perc.sat)
  
  
}

R13$o2.sat.100<-o2.sat.100$mgO2.per.L #insert calculations into dataframe
R13$o2.mg.L<- (R13$perc.sat * R13$o2.sat.100)/100
R13$depth=4 #assume canals are ~4 ft deep 
R13$light=900 #need light data, will use this for now but should replace later with real values

names(R13)[names(R13)=="o2.sat.100"] <- "DO.sat"
names(R13)[names(R13)=="o2.mg.L"] <- "DO.obs"
names(R13)[names(R13)=="posix.time.solar"]<-"solar.time"
names(R13)[names(R13)=="temp"]<-"temp.water"
datar13<-R13[, c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")]
datar13<-arrange(datar13, solar.time)
datar13$hour<-hour(datar13$solar.time)
datar13$hour<-as.numeric(datar13$hour)

datar13$light[datar13$hour == 22] <- 3000
datar13$light[datar13$hour == 23] <- 2700
datar13$light[datar13$hour == 0] <- 1800
datar13$light[datar13$hour == 1] <- 800
datar13$light[datar13$hour == 2] <- 100
datar13$light[datar13$hour == 3] <- 0
datar13$light[datar13$hour == 4] <- 0
datar13$light[datar13$hour == 5] <- 0
datar13$light[datar13$hour == 6] <- 0
datar13$light[datar13$hour == 7] <- 0
datar13$light[datar13$hour == 8] <- 0
datar13$light[datar13$hour == 9] <- 0
datar13$light[datar13$hour == 10] <- 0
datar13$light[datar13$hour == 11] <- 0
datar13$light[datar13$hour == 12] <- 5
datar13$light[datar13$hour == 13] <- 400
datar13$light[datar13$hour == 14] <- 1300
datar13$light[datar13$hour == 15] <- 2000
datar13$light[datar13$hour == 16] <- 2500 
datar13$light[datar13$hour == 17] <- 3000 #11
datar13$light[datar13$hour == 18] <- 3500 #noon our time
datar13$light[datar13$hour == 19] <- 3500
datar13$light[datar13$hour == 20] <- 3000
datar13$light[datar13$hour == 21] <- 2500
########running model#############
mm_name(type='mle')

mle_name <- mm_name(type='mle')
mle_name
mle_specs <- specs(mle_name, init.GPP.daily=30, init.ER.daily=-10, init.K600.daily=3, GPP_daily_mGPP_daily_lower = -Inf)

mle_specs <- specs(mle_name)
mle_specs

dt<-datar13[, c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")] #subset only columns we need
mle_fit <- metab(mle_specs, data=dt) #fit model
