install.packages("streamMetabolizer", dependencies=TRUE, 
                 repos=c("https://owi.usgs.gov/R","https://cran.rstudio.com"))
update.packages(oldPkgs=c("streamMetabolizer","unitted"),
                dependencies=TRUE, repos=c("https://owi.usgs.gov/R", "https://cran.rstudio.com"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(streamMetabolizer)

#######example
dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28, attach.units=TRUE)
dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
dat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

metab_inputs('mle', 'data')

data.frame(
  colname=names(dat), 
  class=unname(sapply(unitted::v(dat), function(col) paste(class(col), collapse=','))),
  units=unname(unitted::get_units(dat)))

mle_specs <- specs(mle_name)
mle_specs

mle_fit <- metab(mle_specs, data=dat, info=c(site='French Creek, WY', source='Bob Hall'))

plot_metab_preds(mle_fit)

plot_DO_preds(mle_fit)


#______________________________________________________________________________
##################Trying with real data############

##################organize data
#O2_dat=read.csv("SRP_data_1site.csv", header=T)

data<-read.csv("SRP_data_1site.csv") #read data

###convert date to good format for stream metabolizer
data$Date.Time<-as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M", tz='Etc/GMT+7')
lubridate::tz(data$Date.Time)#checking that time is in right tz
(data$posix.time.solar <- streamMetabolizer::calc_solar_time(data$Date.Time, longitude=-106.3))

###rename columns to match function below###
names(data)[names(data)=="DO_percent"] <- "perc.sat" 
names(data)[names(data)=="Temp_C"] <- "temp"
names(data)[names(data)=="pressure_mbar"] <- "measured.atmP"


####function to calculate o2 saturation at 100%###

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


#rename columns so they match stream metabolizer code
#might need to manually do this, got messed up the first time
names(data)[names(data)=="temp"] <- "temp.water"
names(data)[names(data)=="posix.time.solar"] <- "solar.time"

#######MY AMAZING FOR LOOP that takes data from spreadsheet and calculates conc DO at 100 percent saturation####
salinity=3 ##guestimate of freshwater salinity
perc.sat=100
o2.sat.100<-c()
for(i in 1:nrow(data))
{
  temp[i]<-data[i,]$temp
  measured.atmP[i]<-data[i,]$measured.atmP
  o2.sat.100<-O2.saturation(salinity, temp, measured.atmP, perc.sat)
  
  
}

data$o2.sat.100<-o2.sat.100$mgO2.per.L #insert calculations into dataframe

data$o2.mg.L<- (data$perc.sat * data$o2.sat.100)/100
data$depth=4
data$light=900

names(data)[names(data)=="o2.sat.100"] <- "DO.sat"
names(data)[names(data)=="o2.mg.L"] <- "DO.obs"

O2data<-data[, c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")]

##########start model

mm_name(type='mle')

mle_name <- mm_name(type='mle')
mle_name

mle_specs <- specs(mle_name)
mle_specs

mle_fit <- metab(mle_specs, data=O2data)
mle_fit

plot_metab_preds(mle_fit)

plot_DO_preds(mle_fit)
