####Graphs####



library(DescTools)
library(data.table)
library(haven)
library(plm)
library(zoo)
library(ggpubr)


#####Put 'em together baby########


figure=ggarrange(Graph_QSS,Graph_ESS,Graph_RSS,Graph_PIS,ncol = 2, nrow = 2)

figure





####Price Impact Stocks#####

PriceImpact_stock=matrix(nrow = D,ncol=4)
colnames(PriceImpact_stock)=c("No Tax","Stock Tax","Option Tax","Double Tax")
PriceImpact_stock[,1]=PriceImpact[,1]
PriceImpact_stock[,2]=PriceImpactts[,1]
PriceImpact_stock[,3]=PriceImpactto[,1]
PriceImpact_stock[,4]=PriceImpacttt[,1]


Preeventmean_PIStock=mean(PriceImpact_stock[1:D/2,1])
Preeventmean_PIStockts=mean(PriceImpact_stock[1:D/2,2])
Preeventmean_PIStockto=mean(PriceImpact_stock[1:D/2,3])
Preeventmean_PIStocktt=mean(PriceImpact_stock[1:D/2,4])
Posteventmean_PIStock=mean(PriceImpact_stock[((D/2)+1):D,1])
Posteventmean_PIStockts=mean(PriceImpact_stock[((D/2)+1):D,2])
Posteventmean_PIStockto=mean(PriceImpact_stock[((D/2)+1):D,3])
Posteventmean_PIStocktt=mean(PriceImpact_stock[((D/2)+1):D,4])

PI_Stock=tibble("No Tax"=rollmean(rollmean(PriceImpact_stock[,1],10),10)-Preeventmean_PIStock,"Stock Tax"=rollmean(rollmean(PriceImpact_stock[,2],10),10)-Preeventmean_PIStockts,
                "Option Tax"=rollmean(rollmean(PriceImpact_stock[,3],10),10)-Preeventmean_PIStockto,"Double Tax"=rollmean(rollmean(PriceImpact_stock[,4],10),10)-Preeventmean_PIStocktt)


L=nrow(PI_Stock)
Means_PIS=matrix(nrow=L,ncol=4)
colnames(Means_PIS)=c("Mean No Tax","Mean Stock Tax","Mean Option Tax","Mean Double Tax")
Means_PIS[1:(L/2),1]=Preeventmean_PIStock-Preeventmean_PIStock
Means_PIS[(L/2+1):L,1]=Posteventmean_PIStock-Preeventmean_PIStock
Means_PIS[1:(L/2),2]=Preeventmean_PIStockts-Preeventmean_PIStockts
Means_PIS[(L/2+1):L,2]=Posteventmean_PIStockts-Preeventmean_PIStockts
Means_PIS[1:(L/2),3]=Preeventmean_PIStockto-Preeventmean_PIStockto
Means_PIS[(L/2+1):L,3]=Posteventmean_PIStockto-Preeventmean_PIStockto
Means_PIS[1:(L/2),4]=Preeventmean_PIStocktt-Preeventmean_PIStocktt
Means_PIS[(L/2+1):L,4]=Posteventmean_PIStocktt-Preeventmean_PIStocktt

Date=seq(from = 1, to = L, by = 1)

PI_Stock=PI_Stock %>% add_column("Mean No Tax"=Means_PIS[,1],"Mean Stock Tax"=Means_PIS[,2],"Mean Option Tax"=Means_PIS[,3],"Mean Double Tax"=Means_PIS[,4],"Date"=Date)

Graph_PIS=ggplot(PI_Stock) +
  geom_line(aes(Date,`Stock Tax`),colour="grey 30",size=.8)+geom_line(aes(Date,`Mean Stock Tax`),colour="grey 30",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_PIS[L-1,2]), label = "Stock Tax", hjust = 0, vjust = 1,colour="grey 30")+
  geom_line(aes(Date,`Option Tax`),colour="grey 45",size=.8)+geom_line(aes(Date,`Mean Option Tax`),colour="grey 45",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_PIS[L-1,3]), label = "Option Tax", hjust = 0, vjust = 1,colour="grey 45")+
  geom_line(aes(Date,`Double Tax`),colour="grey 60",size=.8)+geom_line(aes(Date,`Mean Double Tax`),colour="grey 60",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_PIS[L-1,4]), label = "Double Tax", hjust = 0, vjust = 1,colour="grey 60")+
  geom_line(aes(Date,`No Tax`),colour="black",size=.8)+geom_line(aes(Date,`Mean No Tax`),colour="black",linetype="dashed",size=.7)+
  annotate(geom = 'text', x=200, y=as.numeric(Means_PIS[L-1,1]), label = "Benchmark", hjust = 0, vjust = 1.,colour="black")+
  geom_vline(xintercept = nrow(PI_Stock)/2,linetype="dotdash",colour="red",size=0.8)+ylab("Price Impact")+xlab("")

Graph_PIS


####Realized Spread SOptions#####


#Calls#

PriceImpact_Call=matrix(nrow = D,ncol=4)
colnames(PriceImpact_Call)=c("No Tax","Stock Tax","Option Tax","Double Tax")
PriceImpact_Call[,1]=PriceImpact[,2]
PriceImpact_Call[,2]=PriceImpactts[,2]
PriceImpact_Call[,3]=PriceImpactto[,2]
PriceImpact_Call[,4]=PriceImpacttt[,2]


Preeventmean_PICall=mean(PriceImpact_Call[1:D/2,1])
Preeventmean_PICallts=mean(PriceImpact_Call[1:D/2,2])
Preeventmean_PICallto=mean(PriceImpact_Call[1:D/2,3])
Preeventmean_PICalltt=mean(PriceImpact_Call[1:D/2,4])
Posteventmean_PICall=mean(PriceImpact_Call[((D/2)+1):D,1])
Posteventmean_PICallts=mean(PriceImpact_Call[((D/2)+1):D,2])
Posteventmean_PICallto=mean(PriceImpact_Call[((D/2)+1):D,3])
Posteventmean_PICalltt=mean(PriceImpact_Call[((D/2)+1):D,4])

PI_Call=tibble("No Tax"=rollmean(PriceImpact_Call[,1],10)-Preeventmean_PICall,"Stock Tax"=rollmean(PriceImpact_Call[,2],10)-Preeventmean_PICallts,
               "Option Tax"=rollmean(PriceImpact_Call[,3],10)-Preeventmean_PICallto,"Double Tax"=rollmean(PriceImpact_Call[,4],10)-Preeventmean_PICalltt)


L=nrow(PI_Call)
Means_PIC=matrix(nrow=L,ncol=4)
colnames(Means_PIC)=c("Mean No Tax","Mean Stock Tax","Mean Option Tax","Mean Double Tax")
Means_PIC[1:(L/2),1]=Preeventmean_PICall-Preeventmean_PICall
Means_PIC[(L/2+1):L+1,1]=Posteventmean_PICall-Preeventmean_PICall
Means_PIC[1:(L/2),2]=Preeventmean_PICallts-Preeventmean_PICallts
Means_PIC[(L/2+1):L+1,2]=Posteventmean_PICallts-Preeventmean_PICallts
Means_PIC[1:(L/2),3]=Preeventmean_PICallto-Preeventmean_PICallto
Means_PIC[(L/2+1):L+1,3]=Posteventmean_PICallto-Preeventmean_PICallto
Means_PIC[1:(L/2),4]=Preeventmean_PICalltt-Preeventmean_PICalltt
Means_PIC[(L/2+1):L+1,4]=Posteventmean_PICalltt-Preeventmean_PICalltt

Date=seq(from = 1, to = L, by = 1)

PI_Call=PI_Call %>% add_column("Mean No Tax"=Means_PIC[,1],"Mean Stock Tax"=Means_PIC[,2],"Mean Option Tax"=Means_PIC[,3],"Mean Double Tax"=Means_PIC[,4],"Date"=Date)

Graph_PIC=ggplot(PI_Call) +
  geom_line(aes(Date,`Stock Tax`),colour="grey 30",size=.6)+geom_line(aes(Date,`Mean Stock Tax`),colour="grey 30",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIC[L-1,2]), label = "Stock Tax", hjust = 0, vjust = 1,colour="grey 30")+
  geom_line(aes(Date,`Option Tax`),colour="grey 45",size=.6)+geom_line(aes(Date,`Mean Option Tax`),colour="grey 45",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIC[L-1,3]), label = "Option Tax", hjust = 0, vjust = 1,colour="grey 45")+
  geom_line(aes(Date,`Double Tax`),colour="grey 60",size=.6)+geom_line(aes(Date,`Mean Double Tax`),colour="grey 60",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIC[L-1,4]), label = "Double Tax", hjust = 0, vjust = 1,colour="grey 60")+
  geom_line(aes(Date,`No Tax`),colour="black",size=.6)+geom_line(aes(Date,`Mean No Tax`),colour="black",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIC[L-1,1]), label = "No Tax", hjust = 0, vjust = 1.,colour="black")+
  geom_vline(xintercept = nrow(PI_Call)/2,linetype="dotdash",colour="red")+ylab("Price Impact")+xlab("")

Graph_PIC



#Puts#

PriceImpact_Put=matrix(nrow = D,ncol=4)
colnames(PriceImpact_Put)=c("No Tax","Stock Tax","Option Tax","Double Tax")
PriceImpact_Put[,1]=PriceImpact[,3]
PriceImpact_Put[,2]=PriceImpactts[,3]
PriceImpact_Put[,3]=PriceImpactto[,3]
PriceImpact_Put[,4]=PriceImpacttt[,3]


Preeventmean_PIPut=mean(PriceImpact_Put[1:D/2,1])
Preeventmean_PIPutts=mean(PriceImpact_Put[1:D/2,2])
Preeventmean_PIPutto=mean(PriceImpact_Put[1:D/2,3])
Preeventmean_PIPuttt=mean(PriceImpact_Put[1:D/2,4])
Posteventmean_PIPut=mean(PriceImpact_Put[((D/2)+1):D,1])
Posteventmean_PIPutts=mean(PriceImpact_Put[((D/2)+1):D,2])
Posteventmean_PIPutto=mean(PriceImpact_Put[((D/2)+1):D,3])
Posteventmean_PIPuttt=mean(PriceImpact_Put[((D/2)+1):D,4])

PI_Put=tibble("No Tax"=rollmean(PriceImpact_Put[,1],10)-Preeventmean_PIPut,"Stock Tax"=rollmean(PriceImpact_Put[,2],10)-Preeventmean_PIPutts,
              "Option Tax"=rollmean(PriceImpact_Put[,3],10)-Preeventmean_PIPutto,"Double Tax"=rollmean(PriceImpact_Put[,4],10)-Preeventmean_PIPuttt)


L=nrow(PI_Put)
Means_PIP=matrix(nrow=L,ncol=4)
colnames(Means_PIP)=c("Mean No Tax","Mean Stock Tax","Mean Option Tax","Mean Double Tax")
Means_PIP[1:(L/2),1]=Preeventmean_PIPut-Preeventmean_PIPut
Means_PIP[(L/2+1):L+1,1]=Posteventmean_PIPut-Preeventmean_PIPut
Means_PIP[1:(L/2),2]=Preeventmean_PIPutts-Preeventmean_PIPutts
Means_PIP[(L/2+1):L+1,2]=Posteventmean_PIPutts-Preeventmean_PIPutts
Means_PIP[1:(L/2),3]=Preeventmean_PIPutto-Preeventmean_PIPutto
Means_PIP[(L/2+1):L+1,3]=Posteventmean_PIPutto-Preeventmean_PIPutto
Means_PIP[1:(L/2),4]=Preeventmean_PIPuttt-Preeventmean_PIPuttt
Means_PIP[(L/2+1):L+1,4]=Posteventmean_PIPuttt-Preeventmean_PIPuttt

Date=seq(from = 1, to = L, by = 1)

PI_Put=PI_Put %>% add_column("Mean No Tax"=Means_PIP[,1],"Mean Stock Tax"=Means_PIP[,2],"Mean Option Tax"=Means_PIP[,3],"Mean Double Tax"=Means_PIP[,4],"Date"=Date)

Graph_PIP=ggplot(PI_Put) +
  geom_line(aes(Date,`Stock Tax`),colour="grey 30",size=.6)+geom_line(aes(Date,`Mean Stock Tax`),colour="grey 30",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIP[L-1,2]), label = "Stock Tax", hjust = 0, vjust = 1,colour="grey 30")+
  geom_line(aes(Date,`Option Tax`),colour="grey 45",size=.6)+geom_line(aes(Date,`Mean Option Tax`),colour="grey 45",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIP[L-1,3]), label = "Option Tax", hjust = 0, vjust = 1,colour="grey 45")+
  geom_line(aes(Date,`Double Tax`),colour="grey 60",size=.6)+geom_line(aes(Date,`Mean Double Tax`),colour="grey 60",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIP[L-1,4]), label = "Double Tax", hjust = 0, vjust = 1,colour="grey 60")+
  geom_line(aes(Date,`No Tax`),colour="black",size=.6)+geom_line(aes(Date,`Mean No Tax`),colour="black",linetype="dashed",size=.6)+
  annotate(geom = 'text', x=220, y=as.numeric(Means_PIP[L-1,1]), label = "No Tax", hjust = 0, vjust = 1.,colour="black")+
  geom_vline(xintercept = nrow(QS_Put)/2,linetype="dotdash",colour="red")+ylab("Price Impact")+xlab("")

Graph_PIP
