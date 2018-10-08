#input: x,y: vector (1D), independent variable
#Output:
Mapply<-function(x = get("x", envir = .GlobalEnv),
                 y = get("y",envir = .GlobalEnv),
                 f = get("f",envir = .GlobalEnv)){
  if (is.list(x) | is.data.frame(x)){
    y<-x$y
    x<-x$x
  }
  re<-matrix(nrow=length(x),ncol=length(y))
  if(is.function(f)){
  for(i in 1:length(x)){
    for(j in 1:length(y)) re[i,j]=f(x[i],y[j])
  }
  }else{
    if(is.character(f)) f<-parse(text = f)
    xl<-x
    for(i in seq_along(xl)){
      x<-xl[i]
      re[i,]<-as.vector(eval(f))
      }
  }
  return(re)
}
#x=1:10
#y=1:20
#z=matrix(nrow=length(x),
#         ncol=length(y))
#ff=function(x,y) return(x^2+y)

contourFunction<-function(x,xbase=seq(from = -5,to=5,by=.05),ybase=seq(from = -5,to=5,by=.05),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
contour.function<-function(x,xbase=seq(from = -5,to=5,by=.05),ybase=seq(from = -5,to=5,by=.05),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
contourExpression<-function(x,xbase=seq(from = -5,to=5,by=.05),ybase=seq(from = -5,to=5,by=.05),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}

contour.expression<-function(x,xbase=seq(from = -5,to=5,by=.05),ybase=seq(from = -5,to=5,by=.05),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
contourCharacter<-function(x,xbase=seq(from = -5,to=5,by=.05),ybase=seq(from = -5,to=5,by=.05),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
contour.character<-function(x,xbase=seq(from = -5,to=5,by=.05),ybase=seq(from = -5,to=5,by=.05),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
