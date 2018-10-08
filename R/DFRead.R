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

DFFlatten<-function(df,independent = 1,dependent = 2: ncol(df)){
  NAMES<-colnames(df)[-independent]
  x=rep(df[,independent],length(dependent))
  y=NULL
  Asp=NULL
  for(i in dependent) y=c(y,df[,dependent[i]])
  Asp = rep(NAMES,each=length(x))
  dtfm=data.frame(x,y,Asp)
  class()
}

DFVis<-function(data){
  require(ggplot2)
  qplot(data=df,x=)
}