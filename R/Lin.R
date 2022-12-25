#Lin
#1
#LinGrad
LinGrad<-function(x,y,inverse = F,list = F,plot = F,point =T){
  if(inverse == T){
    LinGrad(y,x,inverse = F)
  }else{
    Grad = diff(y)/diff(x)
    if(plot|list) IndMP = LinMidP(x)
    if(plot) ifelse(point==T,point(IndMP,Grad,
                                type = 'b'),
                    plot(IndMP,Grad,
                         type = 'b',xlab = deparse(substitute(x)),ylab  = paste(deparse(substitute(y))),'Gradient',sep = '_'))
    if(list){
      list(Midpoint = IndMP,Gradient = Grad)
    }else{
      Grad
    }
  }
}

#2
#LinMidP
LinMidP<-function(x,plot = F){
if(is.data.frame(x)){
  MidP = apply(x,2,LinMidP)
}
return((x[-1]+x[-length(x)])/2)
}

#A vector 'base of variable' for plotting with the original
#numeric of x or a vector that contains the min and max of the domain of x
#with accuracy 'acc'
LinBase<-function(x,acc,contain.first = TRUE){
  xbase=c((1:acc)/acc*(max(x)-min(x))+min(x))
  if (contain.first==T) xbase = c(min(x),xbase)
  return(xbase)
}

#linear read table

LinRead<-function(a,b,aa){
  n = length(a)
  if(n!= length(b)) warning("Length differ")
  #print(a)
  #print(b)
  for(i in 2:length(a)){
    if (aa<a[i]){
      if(a[i]-a[i-1]==0) return(b[i]/2+b[i+1]/2)
      return(b[i-1]+(b[i]-b[i-1])*(aa-a[i-1])/(a[i]-a[i-1]))
    }
  }
  return(b[n])
}

#linRead(a,b,3.5)
#linRead(b,a,3.5)
