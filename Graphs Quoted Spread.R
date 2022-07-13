####Graphs####



library(DescTools)
library(data.table)
library(haven)
library(plm)
library(zoo)
library(ggpubr)


######Prices######




####Quoted Spread STocks#####

QuotedSpread_stock=matrix(nrow = D,ncol=4)
colnames(QuotedSpread_stock)=c("No Tax","Stock Tax","Option Tax","Double Tax")
QuotedSpread_stock[,1]=QuotedSpread[,1]
QuotedSpread_stock[,2]=QuotedSpreadts[,1]
QuotedSpread_stock[,3]=QuotedSpreadto[,1]
QuotedSpread_stock[,4]=QuotedSpreadtt[,1]


Preeventmean_QSStock=mean(QuotedSpread_stock[1:D/2,1])
Preeventmean_QSStockts=mean(QuotedSpread_stock[1:D/2,2])
Preeventmean_QSStockto=mean(QuotedSpread_stock[1:D/2,3])
Preeventmean_QSStocktt=mean(QuotedSpread_stock[1:D/2,4])
Posteventmean_QSStock=mean(QuotedSpread_stock[((D/2)+1):D,1])
Posteventmean_QSStockts=mean(QuotedSpread_stock[((D/2)+1):D,2])
Posteventmean_QSStockto=mean(QuotedSpread_stock[((D/2)+1):D,3])
Posteventmean_QSStocktt=mean(QuotedSpread_stock[((D/2)+1):D,4])

QS_Stock=tibble("No Tax"=rollmean(rollmean(QuotedSpread_stock[,1],10),10)-Preeventmean_QSStock,"Stock Tax"=rollmean(rollmean(QuotedSpread_stock[,2],10),10)-Preeventmean_QSStockts,
                       "Option Tax"=rollmean(rollmean(QuotedSpread_stock[,3],10),10)-Preeventmean_QSStockto,"Double Tax"=rollmean(rollmean(QuotedSpread_stock[,4],10),10)-Preeventmean_QSStocktt)


L=nrow(QS_Stock)
Means_QSS=matrix(nrow=L,ncol=4)
colnames(Means_QSS)=c("Mean No Tax","Mean Stock Tax","Mean Option Tax","Mean Double Tax")
Means_QSS[1:(L/2),1]=Preeventmean_QSStock-Preeventmean_QSStock
Means_QSS[(L/2+1):L,1]=Posteventmean_QSStock-Preeventmean_QSStock
Means_QSS[1:(L/2),2]=Preeventmean_QSStockts-Preeventmean_QSStockts
Means_QSS[(L/2+1):L,2]=Posteventmean_QSStockts-Preeventmean_QSStockts
Means_QSS[1:(L/2),3]=Preeventmean_QSStockto-Preeventmean_QSStockto
Means_QSS[(L/2+1):L,3]=Posteventmean_QSStockto-Preeventmean_QSStockto
Means_QSS[1:(L/2),4]=Preeventmean_QSStocktt-Preeventmean_QSStocktt
Means_QSS[(L/2+1):L,4]=Posteventmean_QSStocktt-Preeventmean_QSStocktt

Date=seq(from = 1, to = L, by = 1)

QS_Stock=QS_Stock %>% add_column("Mean No Tax"=Means_QSS[,1],"Mean Stock Tax"=Means_QSS[,2],"Mean Option Tax"=Means_QSS[,3],"Mean Double Tax"=Means_QSS[,4],"Date"=Date)

Graph_QSS=ggplot(QS_Stock) +
  geom_line(aes(Date,`Stock Tax`),colour="grey 30",size=.8)+geom_line(aes(Date,`Mean Stock Tax`),colour="grey 30",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_QSS[L-1,2]), label = "Stock Tax", hjust = 0, vjust = 1,colour="grey 30")+
  geom_line(aes(Date,`Option Tax`),colour="grey 45",size=.8)+geom_line(aes(Date,`Mean Option Tax`),colour="grey 45",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_QSS[L-1,3]), label = "Option Tax", hjust = 0, vjust = 1,colour="grey 45")+
  geom_line(aes(Date,`Double Tax`),colour="grey 60",size=.8)+geom_line(aes(Date,`Mean Double Tax`),colour="grey 60",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_QSS[L-1,4]), label = "Double Tax", hjust = 0, vjust = 1,colour="grey 60")+
  geom_line(aes(Date,`No Tax`),colour="black",size=.8)+geom_line(aes(Date,`Mean No Tax`),colour="black",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_QSS[L-1,1]), label = "Benchmark", hjust = 0, vjust = 1.,colour="black")+
  geom_vline(xintercept = nrow(QS_Stock)/2,linetype="dotdash",colour="red", size=.8)+ylab("Quoted Spread")+xlab("")

Graph_QSS


####Quoted Spread SOptions#####


#Calls#

QuotedSpread_Call=matrix(nrow = D,ncol=4)
colnames(QuotedSpread_Call)=c("No Tax","Stock Tax","Option Tax","Double Tax")
QuotedSpread_Call[,1]=QuotedSpread[,2]
QuotedSpread_Call[,2]=QuotedSpreadts[,2]
QuotedSpread_Call[,3]=QuotedSpreadto[,2]
QuotedSpread_Call[,4]=QuotedSpreadtt[,2]


Preeventmean_QSCall=mean(QuotedSpread_Call[1:D/2,1])
Preeventmean_QSCallts=mean(QuotedSpread_Call[1:D/2,2])
Preeventmean_QSCallto=mean(QuotedSpread_Call[1:D/2,3])
Preeventmean_QSCalltt=mean(QuotedSpread_Call[1:D/2,4])
Posteventmean_QSCall=mean(QuotedSpread_Call[((D/2)+1):D,1])
Posteventmean_QSCallts=mean(QuotedSpread_Call[((D/2)+1):D,2])
Posteventmean_QSCallto=mean(QuotedSpread_Call[((D/2)+1):D,3])
Posteventmean_QSCalltt=mean(QuotedSpread_Call[((D/2)+1):D,4])

QS_Call=tibble("No Tax"=rollmean(QuotedSpread_Call[,1],10)-Preeventmean_QSCall,"Stock Tax"=rollmean(QuotedSpread_Call[,2],10)-Preeventmean_QSCallts,
                "Option Tax"=rollmean(QuotedSpread_Call[,3],10)-Preeventmean_QSCallto,"Double Tax"=rollmean(QuotedSpread_Call[,4],10)-Preeventmean_QSCalltt)


L=nrow(QS_Call)
Means_QSC=matrix(nrow=L,ncol=4)
colnames(Means_QSC)=c("Mean No Tax","Mean Stock Tax","Mean Option Tax","Mean Double Tax")
Means_QSC[1:(L/2),1]=Preeventmean_QSCall-Preeventmean_QSCall
Means_QSC[(L/2+1):L+1,1]=Posteventmean_QSCall-Preeventmean_QSCall
Means_QSC[1:(L/2),2]=Preeventmean_QSCallts-Preeventmean_QSCallts
Means_QSC[(L/2+1):L+1,2]=Posteventmean_QSCallts-Preeventmean_QSCallts
Means_QSC[1:(L/2),3]=Preeventmean_QSCallto-Preeventmean_QSCallto
Means_QSC[(L/2+1):L+1,3]=Posteventmean_QSCallto-Preeventmean_QSCallto
Means_QSC[1:(L/2),4]=Preeventmean_QSCalltt-Preeventmean_QSCalltt
Means_QSC[(L/2+1):L+1,4]=Posteventmean_QSCalltt-Preeventmean_QSCalltt

Date=seq(from = 1, to = L, by = 1)

QS_Call=QS_Call %>% add_column("Mean No Tax"=Means_QSC[,1],"Mean Stock Tax"=Means_QSC[,2],"Mean Option Tax"=Means_QSC[,3],"Mean Double Tax"=Means_QSC[,4],"Date"=Date)

Graph_QSC=ggplot(QS_Call) +
  geom_line(aes(Date,`Stock Tax`),colour="grey 30",size=.6)+geom_line(aes(Date,`Mean Stock Tax`),colour="grey 30",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSC[L-1,2]), label = "Stock Tax", hjust = 0, vjust = 1,colour="grey 30")+
  geom_line(aes(Date,`Option Tax`),colour="grey 45",size=.6)+geom_line(aes(Date,`Mean Option Tax`),colour="grey 45",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSC[L-1,3]), label = "Option Tax", hjust = 0, vjust = 1,colour="grey 45")+
  geom_line(aes(Date,`Double Tax`),colour="grey 60",size=.6)+geom_line(aes(Date,`Mean Double Tax`),colour="grey 60",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSC[L-1,4]), label = "Double Tax", hjust = 0, vjust = 1,colour="grey 60")+
  geom_line(aes(Date,`No Tax`),colour="black",size=.6)+geom_line(aes(Date,`Mean No Tax`),colour="black",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSC[L-1,1]), label = "No Tax", hjust = 0, vjust = 1.,colour="black")+
  geom_vline(xintercept = nrow(QS_Call)/2,linetype="dotdash",colour="red")+ylab("Quoted Spread")+xlab("")

Graph_QSC



#Puts#

QuotedSpread_Put=matrix(nrow = D,ncol=4)
colnames(QuotedSpread_Put)=c("No Tax","Stock Tax","Option Tax","Double Tax")
QuotedSpread_Put[,1]=QuotedSpread[,3]
QuotedSpread_Put[,2]=QuotedSpreadts[,3]
QuotedSpread_Put[,3]=QuotedSpreadto[,3]
QuotedSpread_Put[,4]=QuotedSpreadtt[,3]


Preeventmean_QSPut=mean(QuotedSpread_Put[1:D/2,1])
Preeventmean_QSPutts=mean(QuotedSpread_Put[1:D/2,2])
Preeventmean_QSPutto=mean(QuotedSpread_Put[1:D/2,3])
Preeventmean_QSPuttt=mean(QuotedSpread_Put[1:D/2,4])
Posteventmean_QSPut=mean(QuotedSpread_Put[((D/2)+1):D,1])
Posteventmean_QSPutts=mean(QuotedSpread_Put[((D/2)+1):D,2])
Posteventmean_QSPutto=mean(QuotedSpread_Put[((D/2)+1):D,3])
Posteventmean_QSPuttt=mean(QuotedSpread_Put[((D/2)+1):D,4])

QS_Put=tibble("No Tax"=rollmean(QuotedSpread_Put[,1],10)-Preeventmean_QSPut,"Stock Tax"=rollmean(QuotedSpread_Put[,2],10)-Preeventmean_QSPutts,
               "Option Tax"=rollmean(QuotedSpread_Put[,3],10)-Preeventmean_QSPutto,"Double Tax"=rollmean(QuotedSpread_Put[,4],10)-Preeventmean_QSPuttt)


L=nrow(QS_Put)
Means_QSP=matrix(nrow=L,ncol=4)
colnames(Means_QSP)=c("Mean No Tax","Mean Stock Tax","Mean Option Tax","Mean Double Tax")
Means_QSP[1:(L/2),1]=Preeventmean_QSPut-Preeventmean_QSPut
Means_QSP[(L/2+1):L+1,1]=Posteventmean_QSPut-Preeventmean_QSPut
Means_QSP[1:(L/2),2]=Preeventmean_QSPutts-Preeventmean_QSPutts
Means_QSP[(L/2+1):L+1,2]=Posteventmean_QSPutts-Preeventmean_QSPutts
Means_QSP[1:(L/2),3]=Preeventmean_QSPutto-Preeventmean_QSPutto
Means_QSP[(L/2+1):L+1,3]=Posteventmean_QSPutto-Preeventmean_QSPutto
Means_QSP[1:(L/2),4]=Preeventmean_QSPuttt-Preeventmean_QSPuttt
Means_QSP[(L/2+1):L+1,4]=Posteventmean_QSPuttt-Preeventmean_QSPuttt

Date=seq(from = 1, to = L, by = 1)

QS_Put=QS_Put %>% add_column("Mean No Tax"=Means_QSP[,1],"Mean Stock Tax"=Means_QSP[,2],"Mean Option Tax"=Means_QSP[,3],"Mean Double Tax"=Means_QSP[,4],"Date"=Date)

Graph_QSP=ggplot(QS_Put) +
  geom_line(aes(Date,`Stock Tax`),colour="grey 30",size=.6)+geom_line(aes(Date,`Mean Stock Tax`),colour="grey 30",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSP[L-1,2]), label = "Stock Tax", hjust = 0, vjust = 1,colour="grey 30")+
  geom_line(aes(Date,`Option Tax`),colour="grey 45",size=.6)+geom_line(aes(Date,`Mean Option Tax`),colour="grey 45",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSP[L-1,3]), label = "Option Tax", hjust = 0, vjust = 1,colour="grey 45")+
  geom_line(aes(Date,`Double Tax`),colour="grey 60",size=.6)+geom_line(aes(Date,`Mean Double Tax`),colour="grey 60",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSP[L-1,4]), label = "Double Tax", hjust = 0, vjust = 1,colour="grey 60")+
  geom_line(aes(Date,`No Tax`),colour="black",size=.6)+geom_line(aes(Date,`Mean No Tax`),colour="black",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_QSP[L-1,1]), label = "No Tax", hjust = 0, vjust = 1.,colour="black")+
  geom_vline(xintercept = nrow(QS_Put)/2,linetype="dotdash",colour="red")+ylab("Quoted Spread")+xlab("")

Graph_QSP


