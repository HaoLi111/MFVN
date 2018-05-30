#1)Map
Map<-function(m){
  nm<-matrix(nrow=ncol(m),ncol=nrow(m))
  for(i in 1:nrow(m)){
    nm[,i]=as.vector(m[(nrow(m)-i+1),])
  }
  return(nm)
}
#2)MapR
MapR<-function(a,b) sqrt(sum((a-b)^2))

#3)
MapBase<-function(x,y,xscale,yscale){
  x=0:x
  y=0:y
  x=linAvg(x)
  y=linAvg(y)
  return(list(BaseX=MatBase(y*yscale,x),
              t(MatBase(x*xscale,y))
              ))
}

MapBase2<-function(M,xscale,yscale) MapBase(nrow(m),ncol(m),xscale,yscale)

#4)MapSimp

MapSimp<-function(m,rn,cn){
  nm<-matrix(nrow=(nrow(m)/rn),ncol=(ncol(m)/cn))
  for(i in 1:(nrow(nm))){
    for(j in 1:(ncol(nm))){
      nm[i,j]<-sum(m[(rn*(i-1)+1):(rn*i),(cn*(j-1)+1):(cn*j)])
    }
  }
  return(nm)
}
MapSimpAvg<-function(m,rn,cn) return(MapSimp(m,rn,cn)/rn/cn)
#mapSimp(matRand(1:12,1:12),3,4)

#3)MapR_along
MapR_along<-function()

