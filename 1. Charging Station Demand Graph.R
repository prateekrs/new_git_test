library(ggplot2)
library(gridExtra)

setwd("C:\\Users\\Prateek Raj\\Desktop\\New folder (3)\\Analyzing data")
data<-read.csv("houston.csv")
str(data)
data_filtered<-data[data$KWH.Used>0.25,]
str(data_filtered)
date_time<-as.POSIXlt(strptime(data_filtered$Transaction.Date, "%m/%d/%Y %H:%M"))
data_filteredDC<-data_filtered[data_filtered$DC.L2=="DC",]
data_filteredL2<-data_filtered[data_filtered$DC.L2=="L2",]

#qplot(factor(data_filtered$DC.L2),geom="histogram")+xlab("Type of Charging Station")+ylab("Demand")+ggtitle(" Demand V/s Type of Charging Station")
#c1<-qplot(data_filteredDC$Serial.Number)+geom_bar()+coord_flip()+xlab("Charging Station")+ylab("Demand")+ ggtitle("DC Charging Stations-Demand Graph")
#c2<-qplot(data_filteredAC$Serial.Number)+geom_bar()+coord_flip()+xlab("Charging Station")+ylab("Demand")+ggtitle("Level-2 Charging Station-Demand Graph")

dclevels<-levels(data_filteredDC)
l2levels<-levels(data_filteredL2)

dc = ggplot(aes(x=factor(data_filteredDC$Asset.Name), y=..count..),data=data_filteredDC, lim=)+
geom_histogram( fill="lightblue", colour="darkblue")+
xlab("DC Charging Station")+ylab("#  of  transactions")+ggtitle("Demand at DC Charging Stations")+theme_bw()+
theme(text = element_text(size=12, colour="darkblue"),
axis.text.x = element_text(angle = 30, hjust=1),
axis.title.x = element_text(face="bold", colour="black", size=12,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=12,vjust=0.3),
plot.title = element_text(size=16,face="bold",colour="black",,vjust=1))+scale_x_discrete(limits=rev(dclevels))+
stat_bin(geom="text", aes(label=..count.., vjust=-0.5),size=4)+coord_cartesian(ylim = c(0, 1110)) 


l2 = ggplot(data=data_filteredL2, aes(x=factor(data_filteredL2$Asset.Name), y=..count..))+
geom_histogram(fill="lightblue", colour="darkblue", width=1)+
xlab("L2 Charging Station")+ylab("#  of  transactions")+ggtitle("Demand at L2 Charging Stations")+theme_bw()+
theme(text = element_text(size=12, colour="darkblue"),
axis.text.x = element_text(angle = 30, hjust=1),
axis.title.x = element_text(face="bold", colour="black", size=12,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=12,vjust=0.3),
plot.title = element_text(size=16,face="bold", colour="black",,vjust=1))+scale_x_discrete(limits=rev(l2levels))+
stat_bin(geom="text", aes(label=..count.., vjust=-0.5),size=4)+coord_cartesian(ylim = c(0, 600))+scale_y_continuous(breaks=seq(0, 600, 100))


grid.arrange(dc,l2)


