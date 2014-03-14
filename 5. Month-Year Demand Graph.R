library(ggplot2)
library(gridExtra)

setwd("C:\\Users\\Prateek Raj\\Desktop\\New folder (3)\\Analyzing data")
data<-read.csv("houston.csv")
str(data)
data_filtered<-data[data$KWH.Used>0.25,]
str(data_filtered)
date_time<-as.POSIXlt(strptime(data_filtered$Transaction.Date, "%m/%d/%Y %H:%M"))
year<-date_time$year+1900
month<-date_time$mon
data_filteredDC<-data_filtered[data_filtered$DC.L2=="DC",]
data_filteredL2<-data_filtered[data_filtered$DC.L2=="L2",]

d<-data.frame(month,year)
d
qplot(data=d,x=factor(month),facets=.~year)+stat_bin(geom="text", aes(label=..count.., vjust=-0.5))+xlab("Month")+ylab("# of transactions")+ ggtitle("Month-Year Demand graph")+
geom_histogram(fill="lightblue", colour="darkblue", binwidth=1)+theme_bw()+
theme(text = element_text(size=14, colour="darkblue"),axis.text.x = element_text(angle=30, hjust=1),
axis.title.x = element_text(face="bold", colour="black", size=16,vjust=-0.2),
axis.title.y = element_text(face="bold", colour="black", size=16,vjust=0.5),
plot.title = element_text(face="bold",vjust=1.5,colour="black"))+
scale_x_discrete(breaks=c("0","1","2","3","4","5","6","7","8","9","10","11"),labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))+
coord_cartesian(ylim = c(0,750))

