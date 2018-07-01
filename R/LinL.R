LinL<-function(x,y){
  if(length(x) ==length(y)){
    sum(sqrt( (diff(x))^2 +  (diff(y))^2))
  }else{
    message('length(x)!=length(y)')
  }
}


#for(i in 1:(length(x)-1)){
  #n[i] = (y[i+1] - y[i])^2 + (x[i+1] - x[i])^2
 # n[i] = sqrt(n[i])
#}
