Exh2d<-function(func, x = seq(from = -5,to = 5, by=.1),
                    y = seq(from = -5,to = 5, by=.1)){
  message('start exhausive calculation')
  v=NULL
  for(i in x){
    for(j in y){
      if(func(i,j)==T){
        v<-rbind(v,c(i,j))
      }
    }
  }

  message('calculation finished')
  v
}
Exh3d<-function(func,x=seq(from = -3, to = 3,by=.1),
                    y=seq(from = -3, to = 3,by=.1),
                    z=seq(from = -3, to = 3,by=.1)){
  message('start exhausive calculation')
  v = NULL
  for(i in x){
    for(j in y){
      for(k in z){
        if(func(i,j,k)==T) v<-rbind(v,c(i,j,k))
      }
    }
  }
  message('calculation finished')
  v
}
