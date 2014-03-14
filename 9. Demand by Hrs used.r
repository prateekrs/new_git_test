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

breaks<-seq(-0.000001,12,by=0.5)
f<-cut(data_filtered$Hours.Used,breaks)
qplot(f,data=data_filtered,geom="histogram")+stat_bin(geom="text", aes(label=..count.., vjust=-1))+xlab("KWHrs Used")+ylab("Demand")+ ggtitle("Demand by KWhrs used")

qplot(x=f)+stat_bin(geom="text", aes(label=..count.., vjust=-1))+xlab("Number of Hours used")+ylab("# of transactions")+ ggtitle("Demand by Number of Hours used")+
geom_histogram(fill="lightblue", colour="darkblue", binwidth=1)+theme_bw()+
theme(text = element_text(size=14, colour="darkblue"),axis.text.x = element_text( hjust=0.5),
axis.title.x = element_text(face="bold", colour="black", size=16,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=16,vjust=0.3),
plot.title = element_text(size=20,face="bold",vjust=1.5,colour="black"))+coord_cartesian(ylim = c(0,4200))+
scale_x_discrete(breaks=c("(-1e-06,0.5]","(0.5,1]","(1,1.5]","(1.5,2]","(2,2.5]","(2.5,3]","(3,3.5]",
"(3.5,4]","(4,4.5]","(4.5,5]","(4.5,5]","(5,5.5]","(5.5,6]","(6,6.5]","(6.5,7]","(7,7.5]","(7.5,8]",
"(8,8.5]","(8.5,9]","(9,9.5]","(9.5,10]","(10,10.5]","(10.5,11]","(11,11.5]","(11.5,12]")
,labels=c("0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-3.5",
"3.5-4","4-4.5","4.5-5","4.5-5","5-5.5","5.5-6","6-6.5","6.5-7","7-7.5","7.5-8",
"8-8.5","8.5-9","9-9.5","9.5-10","10-10.5","10.5-11","11-11.5","11.5-12"))

