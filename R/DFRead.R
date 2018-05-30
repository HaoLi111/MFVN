DFRead<-function(df,target,independent = 1,dependent = 2){
  dfx<-df[,independent]
  dfy<-df[,dependent]
  for (i in 1:(length(dfx)-1)){
    if (target == dfx[i]){

      return(dfy[i])
    }else if(dfx[i]<target& target<dfx[i+1]){
      m=(dfy[i+1]-dfy[i])/(dfx[i+1]-dfx[i])
      return(m*(target-dfx[i])+dfy[i])

    }
  }
  if(target ==dfx[length(dfx)]) dfy[length(dfx)]
}

DFRead2<-function(df,target,independent = 1,dependent = seq(from = 2, to = ncol(df), by = 1)){
  dfx<-df[,independent]
  dfy<-df[,dependent]
  for (i in 1:(length(dfx)-1)){
   if (target == dfx[i]){
        return(dfy[i,])
    }else if(dfx[i]<target& target<dfx[i+1]){
      m=(dfy[i+1,]-dfy[i,])/(dfx[i+1]-dfx[i])
       return(m*(target-dfx[i])+dfy[i,])

      }
  }
    if(target ==dfx[length(dfx)]) dfy[length(dfx),]
}