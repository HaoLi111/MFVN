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
MatSlic<-function(x,y,z,dimcode = 2,xlab=NA,ylab=NA,
                  type = "b",
                  main = "z"){
  
  if(dimcode==1){
    if(is.na(xlab)) xlab = deparse(substitute(y))
    if(is.na(ylab)) ylab = deparse(substitute(z))
    matplot(MatBase(y,x),t(z),type=type,main =main,
            xlab = xlab,
            ylab = ylab)
  }else{
    if(is.na(xlab)) xlab= deparse(substitute(x))
    if(is.na(ylab)) ylab = deparse(substitute(z))
    matplot(MatBase(x,y),z,type=type,main = main,
            xlab = xlab,
            ylab = ylab)
  }
}

#z = matrix(c(1,2,31,4,2,23,4,21,42),3,3)
#z
#MatSlic(1:3,1:3,z)

#MVis
MatVis<-function(x,...) UseMethod('MatVis')
MatVis.matrix<-function(x,y=NULL,z=NULL,xlab=NULL,ylab=NULL,zlab=NULL,asp = 1,
                        aspect = c('persp','fill.contour','MatSlic','MatSlicT')){
  #layoutting
  if(length(aspect)>1){
    
  }else{
    show(get(aspect)(x))
    }
}
MatVis.list<-function(x,y,z,
               aspect = c("persp","contour","Slice","Slice.t")){
    y<-x[2]
    z<-x[3]
    x<-x[1]
  
}
#MatVis(MatRand(3,3),'fill.contour')
