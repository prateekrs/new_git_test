library(ggplot2)
library(gridExtra)
setwd("C:\\Users\\Prateek Raj\\Desktop\\New folder (3)\\Analyzing data")
data<-read.csv("houston.csv")
str(data)
data_filtered<-data[data$KWH.Used>0.25,]
str(data_filtered)
date_time<-as.POSIXlt(strptime(data_filtered$Transaction.Date, "%m/%d/%Y %H:%M"))

ggplot(data=data_filtered, aes(x=data_filtered$DC.L2,y=..count..,fill=data_filtered$DC.L2))+
geom_histogram(width=0.5)+
xlab("Charging Station Type")+ylab("# of transactions")+ggtitle(" Demand V/s Type of Charging Station")+theme_bw()+
theme(text = element_text(size=14, colour="darkblue"),
axis.title.x = element_text(face="bold", colour="black", size=14,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=14,vjust=0.3),
plot.title = element_text(size=22,face="bold",colour="black",,vjust=1))+
scale_x_discrete(breaks=c("DC", "L2",""), labels=c("DC", "L2","L1"))+
scale_fill_discrete(name="Charging Station Type\n", breaks=c("DC", "L2",""), labels=c("  DC", "  L2","  L1"))+ 
stat_bin(geom="text", aes(label=..count.., vjust=-0.3), size=6)