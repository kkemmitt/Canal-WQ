setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")
data<-read.csv("617_gpscoord.csv", header = T, stringsAsFactors = FALSE)  

library(rworldmap)
newmap <- getMap(resolution = "low")

AZ<-map("state", "ARIZONA")

library(ggmap)
library(ggplot2)
library(measurements)

# change the degree symbol to a space, get rid of directionality
data$Latitude = gsub('o', ' ', data$Latitude)
data$Latitude = gsub('N', '', data$Latitude)

data$Longitude = gsub('o', ' ', data$Longitude)
data$Longitude = gsub('W', '-', data$Longitude)


# convert from decimal minutes to decimal degrees
data$Latitude = measurements::conv_unit(data$Latitude, from = 'deg_dec_min', to = 'dec_deg')
data$Longitude = measurements::conv_unit(data$Longitude, from = 'deg_dec_min', to = 'dec_deg')

data$Latitude<- as.numeric(data$Latitude)
data$Longitude<- as.numeric(data$Longitude)
# add GPS coords for sites missing data using arbitrary google maps coords
data[17, 7]=-112.052678
data[17, 8]=33.543581
data[18, 7]=-112.032298
data[18,8]=33.530309
data[19,7]=-112.130126
data[19,8]=33.570640
data[20,7]=-112.128109
data[20,8]=33.569282
data[21,7]=-111.764411
data[21,8]=33.471314
data[22,7]=-111.764411
data[22,8]=33.471314
data[23,7]=-111.729178
data[23,8]=34.059863
data[24,7]=-112.073941
data[24,8]=33.562445
data[25,7]=-111.960834
data[25,8]=33.489284
data[27,7]=-111.892127
data[27,8]=33.532391
data[28,7]=-111.816026
data[28,8]=33.436950
data[29,7]=-111.796569 #not sure how inaccurate this is
data[29,8]=33.330168
data[30,7]=-111.715816
data[30,8]=33.499609
data[31,7]=-111.816924
data[31,8]=33.436817
data[32,7]=-114.387874
data[32,8]=34.500866

data[26,7]="NA" #not sure where this site is
data[26,8]="NA" #not sure where this site is
# Get a map
arizona <- get_map(location = c(lon = -112.0728, lat = 33.5), zoom = 9.8)

ggmap(arizona) +
  geom_point(data = data, aes(x = Longitude, y = Latitude, shape=ClusterName, na.rm=T), show.legend = T)+
  xlab("Longitude")+
  ylab("Latitude")


plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
points(data$Longitude, data$Latitude, col = "red", cex = .6)
data<-read.csv("617_nutrients_f188850a9e51c192bbe89b680f40c7cc934463af.csv")
data1<-read.csv("617_quarterlyLakeSampling.csv")
attach(data)
View(data)
#plot data ###
plot(data$Tot_N ~ data$Date)
ggplot(aes(Tot_N, data=data), geom=boxplot, color=Site_Acronym)
ggplot(data, aes(x=Date, y=Tot_N))+ geom_point()
ggplot(data, aes(x=Tot_P, y=Tot_N))+ geom_point()
