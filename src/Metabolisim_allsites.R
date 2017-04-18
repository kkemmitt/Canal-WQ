###Load in sensor and weather data for all sites###
setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")
library(streamMetabolizer)

######################merge bp data with sensor data################
data<-read.csv("SensorReadings2017.csv") #read in sensor data
data$month_day <- paste(data$Month, data$Day, sep = "-")

####load in weather data 
weather<-read.csv("phoenix_weather_jan_april.csv") 
weather$month_day <- paste(weather$Month, weather$Day, sep = "-")

##don't need all the weather data so created df of just pressure and date###
weath<-weather[, c("Date", "press.avg.mbar", "month_day")]

### merge data###
total <- merge(data, weath,by="month_day")
newdat<- total[, c("Sensor.Name", "Parameter", "Sensor.Reading", "Reading.Date", "press.avg.mbar")]
prac<-spread(total, Parameter, Sensor.Reading) #convert from long to wide
write.csv(prac, file = "Sensor.Readings.Presssure.csv")




##########convert %DO to mg##########33




####function to calculate o2 saturation at 100%###
prac<-read.csv("Sensor.Readings.Presssure.csv")
### convert sensor data date to correct format ###
prac$Reading.Date<-as.POSIXct(prac$Reading.Date, format = "%m/%d/%Y %H:%M", tz='Etc/GMT+7')
lubridate::tz(prac$Reading.Date)#checking that time is in right tz
(prac$solar.time <- streamMetabolizer::calc_solar_time(prac$Reading.Date, longitude=-106.3))

#rename columns to match function 
names(prac)[names(prac)=="DO"] <- "perc.sat" 
names(prac)[names(prac)=="Temp"] <- "temp"
names(prac)[names(prac)=="press.avg.mbar"] <- "measured.atmP"
prac$perc.sat[prac$perc.sat < 0] <- NA # get rid of any negative perc.sat values


O2.saturation<-function(salinity, temp, measured.atmP, perc.sat) {
  
  a=4.9e1
  b=-1.335
  c=2.759e-2
  d=-3.235e-4
  e=1.598e-6
  p=5.516e-1
  q=-1.759e-2
  r=2.253e-4
  s=-2.654e-7
  t=5.362e-8
  A=5.257e1
  B=6.69e3
  C=4.681
  TK=temp+273
  Chloride=(salinity-0.03)/1.805
  atmPsealevel=1013
  MolVol=22.414
  MWO2=32
  
  alpha=a+(b*temp)+(c*temp^2)+(d*temp^3)+(e*temp^4)-(Chloride*(p+(q*temp)+(r*temp^2)+(s*temp^3)+(t*temp^4)))
  bunsen=alpha/1000
  vapP=exp(A-(B/TK)-(C*log(TK)))
  
  umoleO2.per.L<-(((measured.atmP-vapP)/atmPsealevel)*(perc.sat/100)*0.2095*bunsen*1e6*(1/MolVol))
  mgO2.per.L<-umoleO2.per.L*(MWO2/1000)
  pO2.torr<-((measured.atmP-vapP)*((perc.sat/100)*0.2095))*0.75
  pO2.mbar<-pO2.torr/0.75
  pO2.kPa<-pO2.mbar/10
  
  output<-data.frame(salinity, temp, measured.atmP, perc.sat, umoleO2.per.L, mgO2.per.L, pO2.torr, pO2.mbar, pO2.kPa)
  print(output)
}


#####for loop to calculate 100 percent oxygen sat concentration####
#too big to run all at once so subset data by site to###
SOCA <- prac[ which(prac$Sensor.Name=='Wql_030002_Probe1'),] #SOCA is site name used by CAP

attach(SOCA)
salinity=3 ##guestimate of freshwater salinity
perc.sat=100
o2.sat.100<-c()

for(i in 1:nrow(SOCA))
{
  temp[i]<-SOCA[i,]$temp
  measured.atmP[i]<-SOCA[i,]$measured.atmP
  o2.sat.100<-O2.saturation(salinity, temp, measured.atmP, perc.sat)
  
  
}

SOCA$o2.sat.100<-o2.sat.100$mgO2.per.L #insert calculations into dataframe
SOCA$o2.mg.L<- (SOCA$perc.sat * SOCA$o2.sat.100)/100
SOCA$depth=4 #assume canals are ~4 ft deep 
SOCA$light=900 #need light data, will use this for now but should replace later with real values

names(SOCA)[names(SOCA)=="o2.sat.100"] <- "DO.sat"
names(SOCA)[names(SOCA)=="o2.mg.L"] <- "DO.obs"
names(SOCA)[names(SOCA)=="posix.time.solar"]<-"solar.time"
names(SOCA)[names(SOCA)=="temp"]<-"temp.water"


O2dataSOCA<-SOCA[, c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")]
O2dataSOCA<-arrange(O2dataSOCA, solar.time)
O2dataSOCA<-O2dataSOCA[-3778,] #delete weird data


##########start model--maximum likelihood###
library(streamMetabolizer)

mm_name(type='mle')

mle_name <- mm_name(type='mle')
mle_name

mle_specs <- specs(mle_name)
mle_specs

mle_fit <- metab(mle_specs, data=test)
met_preds <- predict_metab(mle_fit)# plot of GPP values from model 
plot(O2dataSOCA$DO.obs ~ O2dataSOCA$solar.time)

plot_metab_preds(mle_fit)

plot_DO_preds(mle_fit)

library(ggplot2)
ggplot(prac, aes(x=solar.time, y=perc.sat, color=Sensor.Name))+
  geom_line()
