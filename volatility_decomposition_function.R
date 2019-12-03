#paper link:https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3409074

#find average weight of each industry using firm level data
find_wei=function(x,y,z){
  y$code=as.character(y$code)
  y$date=as.Date(as.character(y$date),format="%Y-%m-%d")
  x$code=as.character(x$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  z$code=as.character(z$code)
  z$date=as.Date(as.character(z$date),format="%Y-%m-%d")
  z$date[1]=as.Date(as.character("1997-12-31"),format="%Y-%m-%d")
  y$ind=as.character(y$ind)
  A=c() ;B=c() ;C=c() ;D=c() ;E=c() ;F=c() ;G=c(); H=c();I=c();J=c(); K=c();L=c()
  M=c(); N=c();O=c();P=c(); Q=c();R=c(); S=c();sumall=c()
  for (i in 1:length(x[,1])) {
    yy=year(x$date[i])
    mm=month(x$date[i])
    cut=paste(yy,"-",mm,"-","01",sep="")
    cut=as.Date(as.character(cut),format="%Y-%m-%d")
    sz=subset(z,z$date<cut)
    nd=sz$date[length(sz[,1])]
    y2=subset(y,y$date==as.Date(as.character(nd),format="%Y-%m-%d"))
    allcap=sum(y2$capall)
    sumall[i]=allcap
    if (length(subset(y2,y2$ind=="A")[,1])==0){
      A[i]=0
    } else {
      A[i]=sum(subset(y2,y2$ind=="A")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="B")[,1])==0){
      B[i]=0
    } else {
      B[i]=sum(subset(y2,y2$ind=="B")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="C")[,1])==0){
      C[i]=0
    } else {
      C[i]=sum(subset(y2,y2$ind=="C")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="D")[,1])==0){
      D[i]=0
    } else {
      D[i]=sum(subset(y2,y2$ind=="D")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="E")[,1])==0){
      E[i]=0
    } else {
      E[i]=sum(subset(y2,y2$ind=="E")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="F")[,1])==0){
      F[i]=0
    } else {
      F[i]=sum(subset(y2,y2$ind=="F")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="G")[,1])==0){
      G[i]=0
    } else {
      G[i]=sum(subset(y2,y2$ind=="G")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="H")[,1])==0){
      H[i]=0
    } else {
      H[i]=sum(subset(y2,y2$ind=="H")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="I")[,1])==0){
      I[i]=0
    } else {
      I[i]=sum(subset(y2,y2$ind=="I")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="J")[,1])==0){
      J[i]=0
    } else {
      J[i]=sum(subset(y2,y2$ind=="J")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="K")[,1])==0){
      K[i]=0
    } else {
      K[i]=sum(subset(y2,y2$ind=="K")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="L")[,1])==0){
      L[i]=0
    } else {
      L[i]=sum(subset(y2,y2$ind=="L")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="M")[,1])==0){
      M[i]=0
    } else {
      M[i]=sum(subset(y2,y2$ind=="M")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="N")[,1])==0){
      N[i]=0
    } else {
      N[i]=sum(subset(y2,y2$ind=="N")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="O")[,1])==0){
      O[i]=0
    } else {
      O[i]=sum(subset(y2,y2$ind=="O")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="P")[,1])==0){
      P[i]=0
    } else {
      P[i]=sum(subset(y2,y2$ind=="P")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="Q")[,1])==0){
      Q[i]=0
    } else {
      Q[i]=sum(subset(y2,y2$ind=="Q")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="R")[,1])==0){
      R[i]=0
    } else {
      R[i]=sum(subset(y2,y2$ind=="R")$capall)/allcap
    }
    if (length(subset(y2,y2$ind=="S")[,1])==0){
      S[i]=0
    } else {
      S[i]=sum(subset(y2,y2$ind=="S")$capall)/allcap
    }
    if (i %%10==0){
      print(i)
    }
  }
  x=data.frame(x,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,sumall)
  return(x)
}

#volatility decomposition using nine million firm level data
fmain=function(x,y,z,u,v){
  y$code=as.character(y$code)
  y$date=as.Date(as.character(y$date),format="%Y-%m-%d")
  y$industry=as.character(y$industry)
  x$code=as.character(x$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  z$code=as.character(z$code)
  z$date=as.Date(as.character(z$date),format="%Y-%m-%d")
  u$code=as.character(u$code)
  u$date=as.Date(as.character(u$date),format="%Y-%m-%d")
  u$ind=as.character(u$ind)
  v$code=as.character(v$code)
  v$date=as.Date(as.character(v$date),format="%Y-%m-%d")
  v$industry=as.character(v$industry)
  for (i in 1:length(y[,1])) {
    if (is.na(y$IRETURN[i])){
      y$IRETURN[i]=0
    }
  }
  u=flag(u)
  MKT=c()
  IND=c()
  FIRM=c()
  for (i in 1:length(x[,1])) {
    yy=year(x$date[i])
    mm=month(x$date[i])
    data_m=subset(z,year(z$date)==yy&month(z$date)==mm)
    s1=data_m$MRETURN-mean(data_m$MRETURN)
    s2=s1^2
    MKT[i]=sum(s2)
    
    data_i=subset(y,year(y$date)==yy&month(y$date)==mm)
    data_i2=data.frame(data_i,DIND=data_i$IRETURN-data_m$MRETURN)
    data_i3=data.frame(data_i2,VIND=(data_i2$DIND)^2)
    inw=c()
    for (j in 1:length(data_i3[,1])) {
      iname=data_i3$industry[j]
      inw[j]=x[iname][i,1]
    }
    data_i4=data.frame(data_i3,inw=inw)
    s3=data_i4$VIND*data_i4$inw
    IND[i]=sum(s3)
    
    data_f=subset(u,year(u$date)==yy&month(u$date)==mm)
    dif=c()
    for (j in 1:length(data_f[,1])) {
      inin=data_f$ind[j]
      dddd=data_f$date[j]
      cankao=subset(data_i,data_i$date==dddd & data_i$industry==inin)
      dif[j]=data_f$ereturn[j]-cankao$IRETURN
    }
    data_f2=data.frame(data_f,dif=dif)
    data_f3=data.frame(data_f2,vdif=(data_f2$dif)^2)
    
    m_v=subset(v,year(v$date)==yy&month(v$date)==mm)
    vin=c()
    
    for (j in 1:length(m_v[,1])) {
      inin=m_v$ind[j]
      temp2=subset(data_f3,data_f3$ind==inin)
      tem2=na.omit(temp2)
      if (length(temp2[,1])==0){
        vin[j]=0
      } else {
        mcankao=fid(temp2)
        vvv=c()
        for (k in 1:length(mcankao[,1])) {
          iid=mcankao$code[k]
          cankao3=subset(temp2,temp2$code==iid)
          vvv[k]=sum(cankao3$vdif)
        }
        mcankao=data.frame(mcankao,vvv=vvv)
        s5=mcankao$weight*mcankao$vvv
        vin[j]=sum(s5)
      }
    }
    m_v2=data.frame(m_v,vin)
    ffw=c()
    for (j in 1:length(m_v2[,1])) {
      iii=m_v2$industry[j]
      ffw[j]=x[iii][i,1]
      if (is.na(m_v2$vin[j])){
        m_v2$vin[j]=0
      }
    }
    m_v3=data.frame(m_v2,ffw)
    m_v4=data.frame(m_v3,ffff=m_v3$vin*m_v3$ffw)
    s6=sum(m_v4$ffff)
    FIRM[i]=s6
    
    print(i)
    print(s6)
    
  }
  x=data.frame(x,MKT,IND,FIRM)
  return(x)
}

#data vasualization of volatility component 
ggplot(data = data,aes(x=date, y=AMKT))+
  geom_line()+
  xlab("Date")+
  ylab("Market volatility")+
  labs(title = "Volatility decomposition: Market volatility (1998-2018)")+
  theme_light()  +
  facet_grid(type ~ .)

ggplot(data = data,aes(x=date, y=AIND))+
  geom_line()+
  xlab("Date")+
  ylab("Industry volatility")+
  labs(title = "Volatility decomposition: Industry volatility (1998-2018)")+
  theme_light()  +
  facet_grid(type ~ .)

ggplot(data = data,aes(x=date, y=AFIRM))+
  geom_line()+
  xlab("Date")+
  ylab("Firm volatility")+
  labs(title = "Volatility decomposition: Firm volatility (1998-2018)")+
  theme_light()  +
  facet_grid(type ~ .)