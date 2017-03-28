setwd("C:/Users/Katie Kemmitt/OneDrive/Research/Drinking-Water-Canal-Data/data")

data<-read.csv("617_sampleNames.csv")
data2<-read.csv("617_algae.csv")
colnames(data2)[2] <- "Site_ID" #rename column so two datasets match

#define each site number with a type
data["Site_Type"]<-c(NA, NA, "canal", "river", "canal", "canal", "canal", "canal", "treated", "canal", "treated", "canal", "treated", "river", "lake", "lake", "canal", "canal", "canal", "river", "lake", "lake", "river", "river", "lake", "lake", "lake" )

fulldat<-merge(data, data2, by="Site_ID") #merge data by Site ID

logchla<-log10(fulldat$ChlA+1)
boxplot(logchla~fulldat$Site_Type, x)



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
  scale_color_manual(values=wes_palette(n=3, name="GrandBudapest"))
  xlab("Site Type")+
  ylab("log CHLa (ug/L)")+
  theme(axis.title = element_text(size=12,face="bold"), axis.text = element_text(size=10))

       