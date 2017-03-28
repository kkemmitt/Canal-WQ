setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")

algae<-read.csv("617_algae.csv", header=T, na.strings = "NA")
library(plyr)

#subset data that have defined cluster
newalgae <- algae[which(algae$ClusterName==c("verde", "salt", "srp", "cap")),]
stats<-ddply(newalgae, c("SiteNumber", "ClusterName"), summarize,
             mean.ChlA = mean(ChlA, na.rm=T))

#plot of total mean chla for each of the 4 clusters
ggplot(stats, aes(x=ClusterName, y=mean.ChlA)) +
  geom_boxplot()+
  xlab("Cluster Name")+
  ylab("Mean ChlA (ug/L)")+
  scale_x_discrete(labels=c("CAP", "Salt", "SRP", "Verde"))

stats <- ddply(algae, c("SiteNumber", "Month"), summarize, 
              N.mass    = sum(!is.na(ChlA)),
              mean.mass = mean(ChlA, na.rm=TRUE),
              sd.mass   = sd(ChlA, na.rm=TRUE),
              se.mass   = sd(ChlA, na.rm=TRUE) / sqrt(sum(!is.na(ChlA))) )

stats<-ddply(algae, c("SiteNumber", "ClusterName"), summarize,
             mean.ChlA = mean(ChlA, na.rm=T))

library(ggplot2)

ggplot(stats, aes(x=c("cap", "salt", "srp", "verde"), y=mean.ChlA)) +
  geom_boxplot()
ggplot(algae, aes(x=SiteNumber))
