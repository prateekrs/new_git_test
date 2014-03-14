library(ggplot2)
library(gridExtra)

setwd("C:\\Users\\Prateek Raj\\Desktop\\New folder (3)\\Analyzing data")
data<-read.csv("houston.csv")
str(data)
data_filtered<-data[data$KWH.Used>0.25,]
str(data_filtered)
data_filtered$Transaction.Date<-as.POSIXlt(strptime(data_filtered$Transaction.Date, "%m/%d/%Y %H:%M"))
data_filteredDC<-data_filtered[data_filtered$DC.L2=="DC",]
data_filteredL2<-data_filtered[data_filtered$DC.L2=="L2",]

RFIDlevels<-levels(factor(data_filtered$RFID))

firstdate<-as.POSIXlt(strptime("12/31/2099 23:59", "%m/%d/%Y %H:%M"))
firstdates<-vector("character",length=length(RFIDlevels))


for(i in seq_along(RFIDlevels)) 
{
userdb<-data_filtered[data_filtered$RFID==RFIDlevels[i],]
for(j in seq_along(userdb$Transaction.Date))
{ if(userdb$Transaction.Date[j]<firstdate)
{firstdate=userdb$Transaction.Date[j]}
}
firstdates[i]<-as.character(firstdate)
firstdate<-as.POSIXlt(strptime("12/31/2099 23:59", "%m/%d/%Y %H:%M"))
}

firstdates<-as.POSIXlt(strptime(firstdates, "%Y-%m-%d %H:%M:%s"))
df<-data.frame(RFIDlevels,firstdates)

write.csv(df,"Users.csv")

mo<-strftime(firstdates, "%m")
yr<-strftime(firstdates, "%y")
table1<-data.frame(table(mo,yr))
for(i in seq_along(table1$Freq))
{if(i>=2){
table1$Freq[i]=table1$Freq[i-1]+table1$Freq[i]
}
}

yr<-strftime(data_filtered$Transaction.Date, "%y")
mo<-strftime(data_filtered$Transaction.Date, "%m")
table2<-data.frame(table(mo,yr))

tpc<-vector("numeric",length=length(table1$Freq))

for(i in seq_along(table1$Freq))
{
tpc[i]=table2$Freq[i]/table1$Freq[i]
}


for(i in seq_along(df$tpc))
{
tpc[i]=table2$Freq[i]/table1$Freq[i]
}

df<-data.frame(table1$mo,table1$yr,tpc)
df$NewCol <- do.call(paste, c(df[c("table1.yr", "table1.mo")], sep = ",")) 
#dat$NewCol <- do.call(paste, c(dat[c("C3", "C4")], sep = ""))

qplot(x=df$NewCol,y=tpc,data=df,geom="histogram")


df$NewCol <- do.call(paste, c(df[c("table1.yr", "table1.mo")], sep = ",")) 
ggplot(aes(x=factor(df$NewCol), y=tpc),data=df)+
geom_histogram( fill="lightblue", colour="darkblue")+
xlab("Month-Year")+ylab("#  of  Monthly Transactions per Active User")+ggtitle("Trend of Monthly Transactions per Active User (from Jan 2011- Dec 2013)")+
theme_bw()+theme(text = element_text(size=12, colour="darkblue"),
axis.text.x = element_text(angle = 30, hjust=1),axis.title.x = element_text(face="bold", colour="black", size=12,vjust=-0.5),
axis.title.y = element_text(face="bold", colour="black", size=12,vjust=0.3),
plot.title = element_text(size=16,face="bold",colour="black",,vjust=1))+
scale_x_discrete(breaks=c("11,01","11,02","11,03","11,04","11,05","11,06","11,07","11,08","11,09","11,10","11,11","11,12","12,01","12,02","12,03","12,04","12,05","12,06","12,07","12,08","12,09","12,10","12,11","12,12","13,01","13,02","13,03","13,04","13,05","13,06","13,07","13,08","13,09","13,10","13,11","13,12"),
labels=c("Jan-2011","Feb-2011","Mar-2011","Apr-2011","May-2011","Jun-2011","Jul-2011","Aug-2011","Sep-2011","Oct-2011","Nov-2011","Dec-2011","Jan-2012","Feb-2012","Mar-2012","Apr-2012","May-2012","Jun-2012","Jul-2012","Aug-2012","Sep-2012","Oct-2012","Nov-2012","Dec-2012","Jan-2013","Feb-2013","Mar-2013","Apr-2013","May-2013","Jun-2013","Jul-2013","Aug-2013","Sep-2013","Oct-2013","Nov-2013","Dec-2013"))+
coord_cartesian(ylim = c(0, 6.3))+scale_y_continuous(breaks=seq(0, 8, 1))
 
