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
    re[i,]=f(x[i],y)
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

contour.function<-function(x,xbase=seq(from = -5,to=5,by=.1),ybase=seq(from = -5,to=5,by=.1),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
