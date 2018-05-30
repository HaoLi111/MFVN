#Pol

PolEst<-function(xbase,b){
  n=length(b)
  y=b[1]*xbase
  if (n>1){
    for(i in (2:n)){
      y=y+b[i]*xbase^i
    }
  }
  y
}

PolEst2<-function(x,y,n,bb) return(PolEst(PolFit(x,y,n),bb))

PolFit<-function(x,y,n,
                 assign = F){
  a=1
  for(i in 2:length(x)){
    a=c(1,a)
  }#polinomial fit with independent variable x, dependent variable y
  if (n>1){
    for (i in (2:n)){     #assigned with the order if power decreasing.
      x=cbind(x,x^i)
    }
  }
  x=cbind(a,x)
  fit = solve(t(x)%*%x,t(x)%*%y)
  rownames(fit) = as.character(0:n)
  fit
  #if(assign){
  # assign(nameFun,function(x) eval({parse(text = )}))
  #}
}

polVis<-function(a,b,n){  #visualization check of estimated results
  plot(a,b)
  est=polFit(a,b,n)
  bb=linbase(a,20)
  points(bb,polEst(bb,est),type='l')
  title(main="polVis Checking")
  return(est)
}

PolVis2<-function(a,b,n){  #visualization check of estimated results
  #plot(a,b) DO NOT PLOT
  est=PolFit(a,b,n)
  bb=linbase(a,20)
  points(bb,
         PolEst(bb,est),type='l')
  title(main="polVis Checking")
  return(est)
}


