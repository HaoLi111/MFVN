#Mat
Mat<-function(x,y){
  if(length(x)>1) x = length(x)
  if(length(y)>1) y = length(y)
  return(matrix(nrow=x,ncol=y))
}
#MatRand
MatRand<-function(x,y,min = 0, max = 1) {
  if(length(x)>1) x = length(x)
  if(length(y)>1) y = length(y)
  return(matrix(nrow=x,ncol=y,runif(x*y))*(max-min)+min)
}
#MatBase
MatBase<-function(x,y) {
  xi=NULL
  for (i in (1:(length(y)))){
    xi=cbind(xi,x)
  }
  xi<-as.matrix(xi)
  return(xi)
}

#numeric derivative of dependent variable z~x,y,with respect to variable x,y
#returns a list GradX and GradY
MatGrad<-function(x,y,z){
  GradX=apply(z,2,diff)/diff(x)
  GradY=apply(z,1,diff)/diff(y)
  return(list(GradX=GradX,GradY=GradY))
}

MatGradB<-function(x,y,z){
  x=LinMidP(x)
  y=LinMidP(y)
  return(list(GradBX=x,GradBY=y))
}


#View a 2 var data with 1 of the variables as the span axis
MatSlic<-function(x,y,z,dimcode = 2,
                  type = "b",
                  main = "z"){
  if(dimcode==1){
    matplot(MatBase(y,x),t(z),type=type,main =main,
            xlab = deparse(substitute(y)),
            ylab = deparse(substitute(z)))
  }else{
    matplot(MatBase(x,y),z,type=type,main = main,
            xlab = deparse(substitute(x)),
            ylab = deparse(substitute(z)))
  }
}

#z = matrix(c(1,2,31,4,2,23,4,21,42),3,3)
#z
#MatSlic(1:3,1:3,z)

#MVis
MatVis<-function(x,y,z,f= NULL,
               aspect = c("persp","contour","Slice","Slice.t")){
  if(is.list(x)){
    y<-x[2]
    z<-x[3]
    x<-x[1]
  }
  if(!is.null(f)) z<-Mapply(x,y,f)
  layout((matrix(length())))
}
