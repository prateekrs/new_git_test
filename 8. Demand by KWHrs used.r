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

breaks<-seq(-0.00001,40,by=5)
f<-cut(data_filtered$KWH.Used,breaks)
qplot(f,data=data_filtered,geom="histogram")+stat_bin(geom="text", aes(label=..count.., vjust=-1))+xlab("KWHrs Used")+ylab("Demand")+ ggtitle("Demand by KWhrs used")

qplot(x=f)+stat_bin(geom="text", aes(label=..count.., vjust=-1))+xlab("Range of KWH used")+ylab("# of transactions")+ ggtitle("Demand by KWH used")+
geom_histogram(fill="lightblue", colour="darkblue", binwidth=1)+theme_bw()+
theme(text = element_text(size=14, colour="darkblue"),axis.text.x = element_text( hjust=0.5),
axis.title.x = element_text(face="bold", colour="black", size=16,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=16,vjust=0.3),
plot.title = element_text(size=20,face="bold",vjust=1.5,colour="black"))+coord_cartesian(ylim = c(0,4000))+
scale_x_discrete(breaks=c("(-1e-05,5]","(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]","(35,40]"),labels=c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35-40"))
