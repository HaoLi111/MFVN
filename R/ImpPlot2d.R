#ImpPlot2d<-function(func,...) UseMethod('ImpPlot2d')
ImpPlot2d<-function(func, x = seq(from = -5,to = 5, by=.1),
                             y = seq(from = -5,to = 5, by=.1),
                             xlab = 'x',ylab = 'y',
                             asp = 1,col = 'red',
                             points = F){
  if(points ==F) plot(c(min(x),max(x)),c(min(y),max(y)),type = 'n',
                      asp = 1,xlab =xlab,ylab = ylab,
                      main = paste('ImpPlot of ',paste(as.character(body(func)),collapse = ' ',sep = ''),sep = ''))
    for(i in x){
      for(j in y){
        if(func(i,j)==T){
          points(i,j,col  = col)
        }
      }
    }
  message('plot completed')
}

