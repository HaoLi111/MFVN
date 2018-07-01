#ImpPlot3d<-function(func,...) UseMethod('ImpPlot3d')
ImpPlot3d<-function(func,x=seq(from = -3, to = 3,by=.1),
                             y=seq(from = -3, to = 3,by=.1),
                             z=seq(from = -3, to = 3,by=.1),
                             highlight.3d=T,
                             dictate = F){
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
  message('starting up package scatterplot3d')
  #0!= length(grep(paste("^package:",'scatterplot3d', "$", sep=""), search()))
  #check if scatterplot3d is loaded
  if(0== length(grep(paste("^package:",'scatterplot3d', "$", sep=""), search()))){
    require(scatterplot3d)
  }
  message('start plotting')
  scatterplot3d(x=v[,1],y =v[,2],z = v[,3],highlight.3d=highlight.3d)#,
                #col=col,main=(paste('ImpPlot of ',paste(as.character(body(func)),collapse = ' ',sep = ''),collapse = '',sep = '')))
  message('plotting finished')
  if(dictate==T) return(v)
}


