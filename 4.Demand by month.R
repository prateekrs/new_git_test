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

qplot(x=factor(date_time$mon))+stat_bin(geom="text", aes(label=..count.., vjust=-1))+xlab("Month")+ylab("# of transactions")+ ggtitle("Monthly Demand graph")+
geom_histogram(fill="lightblue", colour="darkblue", binwidth=1)+theme_bw()+
theme(text = element_text(size=14, colour="darkblue"),axis.text.x = element_text( hjust=0.5),
axis.title.x = element_text(face="bold", colour="black", size=16,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=16,vjust=0.3),
plot.title = element_text(face="bold",vjust=1.5,colour="black"))+coord_cartesian(ylim = c(0,1050))+
scale_x_discrete(breaks=c("0","1","2","3","4","5","6","7","8","9","10","11"),labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))


#qplot(factor(date_time$mon),geom="histogram")+stat_bin(geom="text", aes(label=..count.., vjust=-1))+xlab("Month")+ylab("Demand")+ ggtitle("Monthly Demand graph")
