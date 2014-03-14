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

qplot(factor(data_filtered$DC.L2),geom="histogram")+xlab("Type of Charging Station")+ylab("Demand")+ggtitle(" Demand V/s Type of Charging Station")
#c1<-qplot(data_filteredDC$Serial.Number)+geom_bar()+coord_flip()+xlab("Charging Station")+ylab("Demand")+ ggtitle("DC Charging Stations-Demand Graph")
#c2<-qplot(data_filteredAC$Serial.Number)+geom_bar()+coord_flip()+xlab("Charging Station")+ylab("Demand")+ggtitle("Level-2 Charging Station-Demand Graph")


dc = ggplot(data=data_filteredDC)+
geom_histogram(aes(x=factor(data_filteredDC$Asset.Name), y=..count..), fill="lightblue", colour="darkblue", binwidth=1)+
xlab("DC Charging Station (Asset name)")+ylab("Demand")+ggtitle("Demand at DC Charging Stations")+theme_bw()+
theme(text = element_text(size=14, colour="darkblue"),axis.text.x = element_text(angle = 60, hjust=1),
axis.title.x = element_text(face="bold", colour="black", size=16,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=16),
plot.title = element_text(face="bold",vjust=1.5,colour="black"))

#grid.arrange(dc,l2)
dc