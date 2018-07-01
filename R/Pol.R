#Pol

PolEst<-function(xbase,b){
  n=length(b)
  y=b[1]
  if (n>1){
    for(i in (2:n)){
      y=y+b[i]*xbase^(i-1)
    }
  }
  y
}

PolEst2<-function(x,y,n,bb) return(PolEst(PolFit(x,y,n),bb))

PolFit<-function(x,y,n,
                 assign = F){
#  a=rep(1.0, times = length(x))
  #for(i in 2:length(x)){
  #  a=c(1,a)
 # }#polinomial fit with independent variable x, dependent variable y
  xt=NULL
  if (n>1){
    for (i in (0:n)){     #assigned with the order if power decreasing.
      xt=cbind(xt,x^i)
    }
  }
  x=xt
  fit = solve(t(x)%*%x,t(x)%*%y)
  rownames(fit) = as.character(0:n)
  fit
  #if(assign){
  # assign(nameFun,function(x) eval({parse(text = )}))
  #}
}


PolVis<-function(a,b,n){  #visualization check of estimated results
  plot(a,b)
  est=polFit(a,b,n)
  bb=seq(from=min(a),to=max(a),by=.01)
  points(bb,PolEst(bb,est),type='l')
  title(main="polVis Checking")
  return(est)
}


Pol_write<-function(v,cha='x'){
  re<-''
  for (i in seq_along(v)){
    re<-paste(re,'+(',v[i],')*',cha,'^',i,sep = '')
  }
  return(re)
}
Pol_express<-function(v,cha='x'){
  re<-''
  for (i in seq_along(v)){
    re<-paste(re,'+(',v[i],')*',cha,'^',i,sep = '')
  }
  parse(text = re)
}

