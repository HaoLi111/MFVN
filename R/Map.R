#1)toMap
toMap<-function(m){
  nm<-matrix(nrow=ncol(m),ncol=nrow(m))
  for(i in 1:nrow(m)){
    nm[,i]=as.vector(m[(nrow(m)-i+1),])
  }
  return(nm)
}
#2)MapR
MapR<-function(a,b) sqrt(sum((a-b)^2))

#3)
MapBase<-function(x,...) UseMethod('MapBase')
MapBase.numeric<-function(x,y,xscale=1,yscale=1){
  x=0:x
  y=0:y
  x=(x[-1]+ x[-length(x)])/2
  y=(y[-1]+ y[-length(y)])/2
  return(list(BaseX=t(MatBase(y*yscale,x)),
              BaseY=t(Map(MatBase(x*xscale,y)))
              ))
}
MapBase.matrix<-function(m,xscale=1,yscale=1) MapBase(nrow(m),ncol(m),xscale,yscale)
MapBase2<-function(m,xscale=1,yscale=1) MapBase(nrow(m),ncol(m),xscale,yscale)

#4)MapSimp

MapSimp<-function(m,rn=2,cn=2){
  nm<-matrix(nrow=(nrow(m)/rn),ncol=(ncol(m)/cn))
  for(i in 1:(nrow(nm))){
    for(j in 1:(ncol(nm))){
      nm[i,j]<-sum(m[(rn*(i-1)+1):(rn*i),(cn*(j-1)+1):(cn*j)])
    }
  }
  return(nm)
}
MapSimpAvg<-function(m,rn=2,cn=2) return(MapSimp(m,rn,cn)/rn/cn)
#mapSimp(matRand(1:12,1:12),3,4)

#3)MapR_along
MapR_along<-function(m,rPoint = c(0,0),xscale=1,yscale=1,out = 'm'){
  BASE<-MapBase(m,xscale=xscale,yscale=yscale)
  X = BASE$BaseX
  Y = BASE$BaseY
  rr=nrow(m)
  cc=ncol(m)
  for(i in 1:rr){
    for(j in 1:cc){
      m[i,j] = MapR(rPoint,c(X[i,j],Y[i,j]))
    }
  }
  if(out=='m'){
    return(m)
  }else if(out == 'sum'){
    return(sum(as.vector(m)))
  }
}


