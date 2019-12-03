#Paper link: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3409074

#Multivariate Granger-causality test with four variables
f_four=function(x){
  ino=c("A","B","C","D","E","F","G","I","J","K","L","M","N","R","S")
  k=matrix(NA,nrow=15,ncol=15)
  k=data.frame(k)
  row.names(k)=ino
  colnames(k)=ino
  for (i in 1:15) {
    for (j in 1:15) {
      if (i!=j){
        name_x=ino[i]
        name_y=ino[j]
        test=x[c("MKT",name_x,name_y)]
        non_test=x[,!names(x) %in% c("MKT",ino[i],ino[j])]
        mix=sum_vol(non_test,iw3)
        test=data.frame(test,mix)
        a=VAR(test,lag.max=15,ic="AIC")
        lag=a$p
        lag=as.numeric(lag)
        nn=names(test)
        nn=list(nn)
        se=seq(1:lag)
        for (u in 1:length(se)) {
          t=u+1
          nn[[t]]=sapply(nn[[1]], function(x){x=paste(x,se[u],sep="")})
        }
        
        for (u in 1:lag) {
          t=u+1
          for (v in 1:4) {
            ss=test[nn[[1]][v]][,1]
            lname=nn[[t]][v]
            test=data.frame(test,lname=lagdf(ss,u))
            ll=4+(u-1)*4+v
            colnames(test)[ll]=nn[[t]][v]
          }
        }  
        nono=subset(nn[[1]],nn[[1]]!=name_y)
        test=test[,!names(test) %in% nono]
        colnames(test)[1]="sy"
        head(test)
        model=lm(sy~.,data = test)
        cons=c()
        for (u in 1:lag) {
          t=u+1
          cons[u]=nn[[t]][2]
        }
        cons=as.character(cons)
        test=linearHypothesis(model, cons,test="Chisq")
        p=test$`Pr(>Chisq)`[2]
        if (p<=0.05){
          k[i,j]="*"
        } else {
          k[i,j]="NO"
        }  
      }  
    }
  }
  return(k)
}