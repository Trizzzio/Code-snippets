library(tidyverse)



#Double Simulation using optimization function 


alpha=.1
delta=1
sigma0=1
m0=100
d0=m0-sigma0
u0=m0+sigma0
l=.5
L=.75
t=0.01



D=250 #Number of days
N=30 #Number of trades per day
T=D*N  #Number of total trades
S=10000


mu=.5
eta=.6





MidPrice= vector(length = T)
M=vector(length = D+1)
M[1]=m0
sigma=vector(length=D+1)
sigma[1]=sigma0


#Initialize conditional  probabilitiy list

Probandinfo=vector(mode="list",length=D)

#Initialize Quantity Lists

Quantities=vector(mode="list",length=D)


#Initialize Price list and Information list

Prices=vector(mode="list",length=D)

#Initialize lists for simulation of simulation 

PRICES=vector(mode="list",length=S)
QUANTITIES=vector(mode="list",length=S)

for(k in 1:S){
  
  
  for(j in 1:D){
    
    #Initialize All the Vectors and Matrices for the inner loop
    
    ProbinfoCond=matrix(nrow=N,ncol=3)
    colnames(ProbinfoCond) <- c("ProbinfoBuy", "ProbinfoNT","ProbinfoSell")
    ProbnoinfoCond=matrix(nrow=N,ncol=3)
    colnames(ProbnoinfoCond) <- c("ProbnoinfoBuy", "ProbnoinfoNT","ProbnoinfoSell")
    ProbuCond=matrix(nrow=N,ncol=3) 
    colnames(ProbuCond) <- c("ProbuBuy", "ProbuNT","ProbuSell")
    ProbdCond=matrix(nrow=N,ncol=3)
    colnames(ProbdCond) <- c("ProbdBuy", "ProbdNT","ProbdSell")
    Probinfo=vector(length=N)
    Probinfo[1]=eta
    Probnoinfo=vector(length=N)
    Probnoinfo[1]=1-eta
    Probu=vector(length=N)
    Probu[1]=mu
    Probd=vector(length=N)
    Probd[1]=1-mu
    Qs=vector(length=N)
    Qc=vector(length=N)
    Qp=vector(length=N)
    AskPrices=matrix(nrow=N,ncol=3) 
    colnames(AskPrices) <- c("As", "Ac","Ap")
    BidPrices=matrix(nrow=N,ncol=3) 
    colnames(BidPrices) <- c("Bs", "Bc","Bp")
    
    Info=matrix(nrow=N+1,ncol=4) 
    colnames(Info) <- c("IE", "state","type","Trade")
    Info[1,1]="NA"
    Info[1,2]="NA"
    Info[1,3]="NA"
    Info[1,4]=0
    
    
    
    #Draw Information Event and state of the world 
    
    InfoEvent=runif(1, min = 0, max = 1)
    state=runif(1, min = 0, max = 1)
    
    
    #Payoffs for Calls
    IntegrandCallu = function(Kc) {(M[j]+sigma0)-Kc}
    IntegrandCallm=function(Kc) {M[j]-Kc}
    IntegrandCalld=function(Kc) {M[j]-sigma0-Kc}
    
    upperlimitcallu=min(u0,M[j]+sigma0)
    lowerlimitcallu=min(upperlimitcallu,d0)
    
    upperlimitcallm=min(m0,M[j])
    lowerlimitcallm=min(upperlimitcallm,d0)
    
    upperlimitcalld=min(d0,M[j]-sigma0)
    lowerlimitcalld=min(upperlimitcalld,d0)
    
    PayoffCallu=integrate(IntegrandCallu,lowerlimitcallu,upperlimitcallu)$value
    PayoffCallm=integrate(IntegrandCallm,lowerlimitcallm,upperlimitcallm)$value
    PayoffCalld=integrate(IntegrandCalld,lowerlimitcalld,upperlimitcalld)$value
    
    #Payoffs for Puts
    IntegrandPutd = function(Kc) {Kc-(M[j]-sigma0)}
    IntegrandPutm=function(Kc) {Kc-M[j]}
    IntegrandPutu=function(Kc) {Kc-M[j]+sigma0}
    
    lowerlimitputm=max(M[j],m0)
    upperlimitputm=max(lowerlimitputm,u0)
    
    lowerlimitputd=max(d0,M[j]-sigma0)
    upperlimitputd=max(lowerlimitputd,u0)
    
    lowerlimitputu=max(M[j]+sigma0,u0)
    upperlimitputu=max(lowerlimitputu,u0)
    
    PayoffPutd=integrate(IntegrandPutd,lowerlimitputd,upperlimitputd)$value
    PayoffPutm=integrate(IntegrandPutm,lowerlimitputm,upperlimitputm)$value
    PayoffPutu=integrate(IntegrandPutu,lowerlimitputu,upperlimitputu)$value
    
    
    m=M[j]
    d=M[j]-sigma0
    u=M[j]+sigma0
    
    for(i in 1:N){
      
      #Draw type of traders
      
      
      type=runif(1, min = 0, max = 1)
      typeLT=runif(1, min = 0, max = 1)
      
      #Update Conditional Probabilities
      
      #Probability of Information Event Happening
      
      ProbinfoCond[i,1]=(Probinfo[i]+alpha*Probinfo[i]*(-1+3*Probu[i]))/(1+alpha*Probinfo[i]*(-1+3*Probu[i]))
      ProbinfoCond[i,2]=((-1+alpha)*Probinfo[i])/(-1+alpha*Probinfo[i])
      ProbinfoCond[i,3]=(Probinfo[i]+alpha*Probinfo[i]*(2-3*Probu[i]))/(1+alpha*Probinfo[i]*(2-3*Probu[i]))
      
      ProbnoinfoCond[i,1]=1-ProbinfoCond[i,1]
      ProbnoinfoCond[i,2]=1-ProbinfoCond[i,2]
      ProbnoinfoCond[i,3]=1-ProbinfoCond[i,3]
      
      #Probability of up and down state 
      
      ProbuCond[i,1]=((1+2*alpha)*Probinfo[i]*Probu[i])/(1+alpha*Probinfo[i]*(-1+3*Probu[i]))
      ProbuCond[i,2]=((-1+alpha)*Probinfo[i]*Probu[i])/(-1+alpha*Probinfo[i])
      ProbuCond[i,3]=((-1+alpha)*Probinfo[i]*Probu[i])/(-1+alpha*Probinfo[i]*(-2+3*Probu[i]))
      
      ProbdCond[i,1]=ProbinfoCond[i,1]-ProbuCond[i,1]
      ProbdCond[i,2]=ProbinfoCond[i,2]-ProbuCond[i,2]
      ProbdCond[i,3]=ProbinfoCond[i,3]-ProbuCond[i,3]
      
      #Update Prices
      
      
      AskPrices[i,1]=(M[j]+sigma0)*ProbuCond[i,1]+M[j]*ProbnoinfoCond[i,1]+(M[j]-sigma0)*ProbdCond[i,1]
      AskPrices[i,2]=PayoffCallu*ProbuCond[i,1]+PayoffCallm*ProbnoinfoCond[i,1]+PayoffCalld*ProbdCond[i,1]
      AskPrices[i,3]=PayoffPutm*ProbnoinfoCond[i,3]+PayoffPutd*ProbdCond[i,3]+PayoffPutu*ProbuCond[i,3]
      
      BidPrices[i,1]=(M[j]+sigma0)*ProbuCond[i,3]+M[j]*ProbnoinfoCond[i,3]+(M[j]-sigma0)*ProbdCond[i,3]
      BidPrices[i,2]=PayoffCallu*ProbuCond[i,3]+PayoffCallm*ProbnoinfoCond[i,3]+PayoffCalld*ProbdCond[i,3]
      BidPrices[i,3]=PayoffPutm*ProbnoinfoCond[i,1]+PayoffPutd*ProbdCond[i,1]+PayoffPutu*ProbuCond[i,1]
      
      
      #Optimal Investment for trade t
      
      
      if(j<D/2){
      
      
      fnup=function(Q){   
        
        -(1/3*(-L+(u- AskPrices[i,1])*Q[1]+(PayoffCallu-AskPrices[i,2])*Q[2])-
            1/3*delta*(-L+(u- AskPrices[i,1])*Q[1]+(PayoffCallu-AskPrices[i,2])*Q[2])^2+
            1/3*(-l+(m- AskPrices[i,1])*Q[1]+(PayoffCallm-AskPrices[i,2])*Q[2])-
            1/3*delta*(-l+(m- AskPrices[i,1])*Q[1]+(PayoffCallm-AskPrices[i,2])*Q[2])^2+
            1/3*((d- AskPrices[i,1])*Q[1]+(PayoffCalld-AskPrices[i,2])*Q[2])-
            1/3*delta*((d- AskPrices[i,1])*Q[1]+(PayoffCalld-AskPrices[i,2])*Q[2])^2)
      }
      
      fndown=function(Q){   
        
        -(1/3*((BidPrices[i,1]-u)*Q[1]+(PayoffPutu-AskPrices[i,3])*Q[2])-
            1/3*delta*((BidPrices[i,1]-u)*Q[1]+(PayoffPutu-AskPrices[i,3])*Q[2])^2+
            1/3*(-l+(BidPrices[i,1]-m)*Q[1]+(PayoffPutm-AskPrices[i,3])*Q[2])-
            1/3*delta*(-l+(BidPrices[i,1]-m)*Q[1]+(PayoffPutm-AskPrices[i,3])*Q[2])^2+
            1/3*(-L+(BidPrices[i,1]-d)*Q[1]+(PayoffPutd-AskPrices[i,3])*Q[2])-
            1/3*delta*(-L+(BidPrices[i,1]-d)*Q[1]+(PayoffPutd-AskPrices[i,3])*Q[2])^2)
      }
      
      Qstart=rep(0,2)
      
      Solution1= optim(par=Qstart,fn=fnup)
      Solution2= optim(par=Qstart,fn=fndown)
      QS=Solution1$par[1]
      QC=Solution1$par[2]
      QP=Solution2$par[2]
      QSS=Solution2$par[1]
      
      }
      
      else
        
      {
        fnup=function(Q){   
          
          -(1/3*(-L+(u- AskPrices[i,1]*(1+t))*Q[1]+(PayoffCallu-AskPrices[i,2])*Q[2])-
              1/3*delta*(-L+(u- AskPrices[i,1]*(1+t))*Q[1]+(PayoffCallu-AskPrices[i,2])*Q[2])^2+
              1/3*(-l+(m- AskPrices[i,1]*(1+t))*Q[1]+(PayoffCallm-AskPrices[i,2])*Q[2])-
              1/3*delta*(-l+(m- AskPrices[i,1]*(1+t))*Q[1]+(PayoffCallm-AskPrices[i,2])*Q[2])^2+
              1/3*((d- AskPrices[i,1]*(1+t))*Q[1]+(PayoffCalld-AskPrices[i,2])*Q[2])-
              1/3*delta*((d- AskPrices[i,1]*(1+t))*Q[1]+(PayoffCalld-AskPrices[i,2])*Q[2])^2)
        }
        
        fndown=function(Q){   
          
          -(1/3*(((1-t)*BidPrices[i,1]-u)*Q[1]+(PayoffPutu-AskPrices[i,3])*Q[2])-
              1/3*delta*((1-t)*(BidPrices[i,1]-u)*Q[1]+(PayoffPutu-AskPrices[i,3])*Q[2])^2+
              1/3*(-l+((1-t)*BidPrices[i,1]-m)*Q[1]+(PayoffPutm-AskPrices[i,3])*Q[2])-
              1/3*delta*(-l+((1-t)*BidPrices[i,1]-m)*Q[1]+(PayoffPutm-AskPrices[i,3])*Q[2])^2+
              1/3*(-L+((1-t)*BidPrices[i,1]-d)*Q[1]+(PayoffPutd-AskPrices[i,3])*Q[2])-
              1/3*delta*(-L+((1-t)*BidPrices[i,1]-d)*Q[1]+(PayoffPutd-AskPrices[i,3])*Q[2])^2)
        }
        
        Qstart=rep(0,2)
        
        Solution1= optim(par=Qstart,fn=fnup)
        Solution2= optim(par=Qstart,fn=fndown)
        QS=Solution1$par[1]
        QC=Solution1$par[2]
        QP=Solution2$par[2]
        QSS=Solution2$par[1]
        
        
        
        
        
      }
      
      if(InfoEvent<eta){#Information Event
        
        
        
        if(state<mu){#up state
          
          
          if(type<alpha){#Informed Trader in up state
            
            Qs[i]=max(0,QS)
            Qc[i]=max(0,QC)
            Qp[i]=0
            
            #Update Probabilities
            
            Probinfo[i+1]=ProbinfoCond[i,1]
            Probnoinfo[i+1]=ProbnoinfoCond[i,1]
            
            Probu[i+1]=((1+2*alpha)*Probu[i])/(1+alpha*(-1+3*Probu[i]))
            Probd[i+1]=1-Probu[i+1]
            
            Info[i+1,1]="YES"
            Info[i+1,2]="UP"
            Info[i+1,3]="IT"
            Info[i+1,4]=1
            
            
          }
          
          if(type>alpha){#Liquidity Traders
            
            if(typeLT>2/3){#Liquidity Trader 1
              
              Qs[i]=max(0,QS)
              Qc[i]=max(0,QC)
              Qp[i]=0
              
              #Update Probabilities
              
              Probinfo[i+1]=ProbinfoCond[i,1]
              Probnoinfo[i+1]=ProbnoinfoCond[i,1]
              
              Probu[i+1]=((1+2*alpha)*Probu[i])/(1+alpha*(-1+3*Probu[i]))
              Probd[i+1]=1-Probu[i+1]
              
              Info[i+1,1]="YES"
              Info[i+1,2]="UP"
              Info[i+1,3]="LT1"
              Info[i+1,4]=1
              
            }
            
            if(typeLT<1/3){#Liquidity Trader 3
              
              Qs[i]=-max(0,QS)
              Qp[i]=max(0,QC)
              Qc[i]=0
              
              #Update Probabilities
              
              Probinfo[i+1]=ProbinfoCond[i,3]
              Probnoinfo[i+1]=ProbnoinfoCond[i,3]
              
              Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha*(-2+3*Probu[i]))
              Probd[i+1]=1-Probu[i+1]
              
              Info[i+1,1]="YES"
              Info[i+1,2]="UP"
              Info[i+1,3]="LT3"
              Info[i+1,4]=-1
              
              
            }
            
            if(typeLT>1/3 & typeLT<2/3){#Liquidity Trader 2
              
              Qs[i]=0
              Qp[i]=0
              Qc[i]=0
              
              #Update Probabilities
              
              Probinfo[i+1]=ProbinfoCond[i,2]
              Probnoinfo[i+1]=ProbnoinfoCond[i,2]
              
              Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha)
              Probd[i+1]=1-Probu[i+1]
              
              Info[i+1,1]="YES"
              Info[i+1,2]="UP"
              Info[i+1,3]="LT2"
              Info[i+1,4]=0
              
            }
            
          }
          
        }
        
        if(state>mu){#down state
          
          if(type<alpha){#Informed Trader
            
            Qs[i]=-max(0,QS)
            Qp[i]=max(0,QC)
            Qc[i]=0
            
            #Update Probabilities
            
            Probinfo[i+1]=ProbinfoCond[i,3]
            Probnoinfo[i+1]=ProbnoinfoCond[i,3]
            
            Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha*(-2+3*Probu[i]))
            Probd[i+1]=1-Probu[i+1]
            
            Info[i+1,1]="YES"
            Info[i+1,2]="DOWN"
            Info[i+1,3]="IT"
            Info[i+1,4]=-1
            
            
          }
          
          if(type>alpha){#Liquidity Traders
            
            if(typeLT>2/3){#Liquidity Trader 1
              
              Qs[i]=max(0,QS)
              Qc[i]=max(0,QC)
              Qp[i]=0
              
              #Update Probabilities
              
              Probinfo[i+1]=ProbinfoCond[i,1]
              Probnoinfo[i+1]=ProbnoinfoCond[i,1]
              
              Probu[i+1]=((1+2*alpha)*Probu[i])/(1+alpha*(-1+3*Probu[i]))
              Probd[i+1]=1-Probu[i+1]
              
              Info[i+1,1]="YES"
              Info[i+1,2]="DOWN"
              Info[i+1,3]="LT1"
              Info[i+1,4]=1
              
              
            }
            
            if(typeLT<1/3){#Liquidity Trader 3
              
              Qs[i]=-max(0,QS)
              Qp[i]=max(0,QC)
              Qc[i]=0
              
              #Update Probabilities
              
              Probinfo[i+1]=ProbinfoCond[i,3]
              Probnoinfo[i+1]=ProbnoinfoCond[i,3]
              
              Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha*(-2+3*Probu[i]))
              Probd[i+1]=1-Probu[i+1]
              
              Info[i+1,1]="YES"
              Info[i+1,2]="DOWN"
              Info[i+1,3]="LT3"
              Info[i+1,4]=-1
              
            }
            
            if(typeLT>1/3 & typeLT<2/3){#Liquidity Trader 2
              
              Qs[i]=0
              Qp[i]=0
              Qc[i]=0
              
              #Update Probabilities
              
              Probinfo[i+1]=ProbinfoCond[i,2]
              Probnoinfo[i+1]=ProbnoinfoCond[i,2]
              
              Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha)
              Probd[i+1]=1-Probu[i+1]
              
              Info[i+1,1]="YES"
              Info[i+1,2]="DOWN"
              Info[i+1,3]="LT2"
              Info[i+1,4]=0
              
            }
            
          }
          
        } 
        
      }
      if(InfoEvent>eta){#No Information Event 
        
        if(typeLT>2/3){#Liquidity Trader 1
          
          Qs[i]=max(0,QS)
          Qc[i]=max(0,QC)
          Qp[i]=0
          
          #Update Probabilities
          
          Probinfo[i+1]=ProbinfoCond[i,1]
          Probnoinfo[i+1]=ProbnoinfoCond[i,1]
          
          Probu[i+1]=((1+2*alpha)*Probu[i])/(1+alpha*(-1+3*Probu[i]))
          Probd[i+1]=1-Probu[i+1]
          
          Info[i+1,1]="NO"
          Info[i+1,2]="NA"
          Info[i+1,3]="LT1"
          Info[i+1,4]=1
          
          
        }
        
        if(typeLT<1/3){#Liquidity Trader 3
          
          Qs[i]=-max(0,QS)
          Qp[i]=max(0,QC)
          Qc[i]=0
          
          #Update Probabilities
          
          Probinfo[i+1]=ProbinfoCond[i,3]
          Probnoinfo[i+1]=ProbnoinfoCond[i,3]
          
          Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha*(-2+3*Probu[i]))
          Probd[i+1]=1-Probu[i+1]
          
          Info[i+1,1]="NO"
          Info[i+1,2]="NA"
          Info[i+1,3]="LT3"
          Info[i+1,4]=-1
          
          
        }
        
        if(typeLT>1/3 & typeLT<2/3){#Liquidity Trader 2
          
          Qs[i]=0
          Qp[i]=0
          Qc[i]=0
          
          #Update Probabilities
          
          Probinfo[i+1]=ProbinfoCond[i,2]
          Probnoinfo[i+1]=ProbnoinfoCond[i,2]
          
          Probu[i+1]=((-1+alpha)*Probu[i])/(-1+alpha)
          Probd[i+1]=1-Probu[i+1]
          
          Info[i+1,1]="NO"
          Info[i+1,2]="NA"
          Info[i+1,3]="LT2"
          Info[i+1,4]=0
          
        }
        
        
        
      }
      
    }
    
    
    #Create tibble with probabilities and Infos
    
    time=seq(1:(N+1))
    Prob=cbind(Probu,Probd,Probinfo,Probnoinfo,time,Info) 
    Prob = Prob[-c(1),]
    Prob = as.data.frame(cbind(Prob,ProbnoinfoCond,ProbuCond,ProbdCond)) #%>% as.tibble()
    Probandinfo[[j]]=Prob
    
    Prices[[j]]=cbind(AskPrices,BidPrices)
    
    Quants= cbind(Qs,Qc,Qp) %>% as.tibble()
    
    Quantities[[j]]=Quants
    
    
    MidPrice[(1+N*(j-1)):(N*j)]=(AskPrices[1:10,1]+BidPrices[1:10,1])/2
    M[j+1]=MidPrice[j*N]
    
    sigma[j+1]=max(AskPrices[1:N,1])-min(BidPrices[1:N,1])
    
    
    
    
  }
  
  
  
  
  SingleQuantities = do.call(rbind,Quantities)
  
  SinglePrices = do.call(rbind,Prices) %>% as.tibble()
  
  
  QUANTITIES[[k]]=SingleQuantities
  PRICES[[k]]=SinglePrices
  
}


S=2500

#First step: Turn lists into TxM Matrices for Qs,Qc,Qp,As,Ac,Ap,Bs,Bc,Bp

Stock=matrix(nrow=T,ncol = S)
Call=matrix(nrow=T,ncol = S)
Put=matrix(nrow=T,ncol = S)
As=matrix(nrow=T,ncol = S)
Ac=matrix(nrow=T,ncol = S)
Ap=matrix(nrow=T,ncol = S)
Bs=matrix(nrow=T,ncol = S)
Bc=matrix(nrow=T,ncol = S)
Bp=matrix(nrow=T,ncol = S)


for(i in 1:S){
  
  Stock[,i]=QUANTITIES[[i]][[1]]
  Call[,i]=QUANTITIES[[i]][[2]]
  Put[,i]=QUANTITIES[[i]][[3]]
  
  As[,i]=PRICES[[i]][[1]]
  Ac[,i]=PRICES[[i]][[2]]
  Ap[,i]=PRICES[[i]][[3]]
  
  Bs[,i]=PRICES[[i]][[4]]
  Bc[,i]=PRICES[[i]][[5]]
  Bp[,i]=PRICES[[i]][[6]]
  
}

# AverageQuantities=do.call(rbind,QUANTITIES)
# AveragePrices=do.call(rbind,PRICES)



#Initializing empty mean matrixes of prices and Quantities

MeanQuantities=matrix(ncol=3,nrow=T)
colnames(MeanQuantities) <- c("QuantityStock", "QuantityCall","QuantityPut")


MeanPrices=matrix(ncol=6,nrow=T)
colnames(MeanPrices) <- c("AskStock", "AskCall","AskPut","BidStock", "BidCall","BidPut")

#Loop for creating means 


for(i in 1:T){
  
  MeanQuantities[i,1]=mean(Stock[i,1:S])
  MeanQuantities[i,2]=mean(Call[i,1:S])
  MeanQuantities[i,3]=mean(Put[i,1:S])
  
  
  MeanPrices[i,1]=mean(As[i,1:S])
  MeanPrices[i,2]=mean(Ac[i,1:S])
  MeanPrices[i,3]=mean(Ap[i,1:S])
  
  MeanPrices[i,4]=mean(Bs[i,1:S])
  MeanPrices[i,5]=mean(Bc[i,1:S])
  MeanPrices[i,6]=mean(Bp[i,1:S])
  
}

rm(Stock,Call,Put,As,Ac,Ap,Bs,Bc,Bp)
