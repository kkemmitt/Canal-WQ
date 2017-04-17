setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")

data<-read.csv("617_sampleNames.csv")
data2<-read.csv("617_algae.csv")
colnames(data2)[2] <- "Site_ID" #rename column so two datasets match

#define each site number with a type
data["Site_Type"]<-c(NA, NA, "canal", "river", "canal", "canal", "canal", "canal", "treated", "canal", "treated", "canal", "treated", "river", "lake", "lake", "canal", "canal", "canal", "river", "lake", "lake", "river", "river", "lake", "lake", "lake" )

fulldat<-merge(data, data2, by="Site_ID") #merge data by Site ID

logchla<-log10(fulldat$ChlA+1)
boxplot(logchla~fulldat$Site_Type)

library(ggplot2)
library(wesanderson)

#this works do not touch
ggplot(fulldat, aes(Site_Type, logchla, fill=Site_Type))+
  geom_boxplot(outlier.color = "red")+
  scale_x_discrete(limits=c("canal", "lake", "river"))+
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest"))+
  xlab("Site Type")+
  ylab("log CHLa (ug/L)")+
  theme(axis.title = element_text(size=12,face="bold"), axis.text = element_text(size=12))

#can touch
ggplot(fulldat, aes(as.factor(Site_Type), ChlA), fill=as.factor(Site_Type))+
  geom_boxplot()+
  scale_x_discrete(limits=c("canal", "lake", "river"))+
  scale_color_manual(values=wes_palette(n=3, name="GrandBudapest"))+
  xlab("Site Type")+
  ylab("log CHLa (ug/L)")+
  theme(axis.title = element_text(size=12,face="bold"), axis.text = element_text(size=10))

  library(plyr)
  summ <- ddply(fulldat, c("ChlA", "Site_Type"), summarize, 
                N.chla    = sum(!is.na(ChlA)),
                mean.chla = mean(ChlA, na.rm=TRUE),
                sd.chla   = sd(ChlA, na.rm=TRUE),
                se.chla   = sd(ChlA, na.rm=TRUE) / sqrt(sum(!is.na(ChlA))) )
  summ
  
  ggplot(summ, aes(x=Site_Type, y=mean.chla)) +
    geom_bar()
  
    geom_errorbar(aes(ymin=mean.chla-se.chla, ymax=mean.chla+se.chla),colour="black", width=.05,size=.5) +
    geom_line(lwd=1)

##----algae breakdown by site-----###
    library(plyr)
    mm <- ddply(fulldat, "Site_ID", summarise, 
                mchla = mean(ChlA, na.rm=T), 
                mtotal=sum(Total, na.rm=T),
                percentchloro=(sum(Chlorophyta, na.rm=T))/mtotal*100,
                percentcyano=(sum(Cyanophyta, na.rm=T))/mtotal*100,
                percentbacillario=(sum(Bacillariophyta, na.rm=T))/mtotal*100,
                percenttotal= sum(percentbacillario, percentcyano, percentchloro)
                  )
    ggplot(mm, aes(x = Site_ID, y = mchla)) + geom_bar(stat="identity")
       