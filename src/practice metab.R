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

datar13$light[22<format(datar13$solar.time, format = "%H")<4]<-0
datar13$light[,datar13[hour (solar.time)>4]]<-0


mm_name(type='mle')

mle_name <- mm_name(type='mle')
mle_name

mle_specs <- specs(mle_name)
mle_specs

mle_fit <- metab(mle_specs, data=datar13)
