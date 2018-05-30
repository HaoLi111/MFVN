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

LinRead<-function(a,b,aa,plot=F){
  w<-length(a)
  if (w==length(b)){
    if (plot ==T) plot(a,b,type="l")
    #x=cbind(a,b)
    for(i in (1:(w-1))){
      if(aa<a[i+1]){
        x1=a[i]
        y1=b[i]
        x2=a[i+1]
        y2=b[i+1]
        m=(y2-y1)/(x2-x1)
        bb=(m*(aa-x1))+y1
        break
      }
    }
    if(plot == T){
      points(aa,bb,type="p",col="red")
    title(main="linRead Visual feedback: Read data success")
    }
  }else{
    message("read data failed: length of the variable")
  }
  #return(plot(a,b,lty="b"))
  return(bb)
  #return(r)
}

#linRead(a,b,3.5)
#linRead(b,a,3.5)
