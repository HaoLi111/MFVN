\name{ImpPlot3d}
\alias{ImpPlot3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D Implicit Plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ImpPlot3d(func = function(x,y,z) x^2+y^2+z^2<=sqrt(3^2+3^2+3^2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%\code{\link{help}}, ~~~
}
\examples{
ImpPlot3d(func = function(x,y,z) x^2+y^2+z^2<=sqrt(3^2+3^2+3^2),x=seq(from = -3, to = 3,by=.6),y=seq(from = -3, to = 3,by=.6),z=seq(from = -3, to = 3,by=.6),dictate = T)
## The function is currently defined as
function(func,x=seq(from = -3, to = 3,by=.1),
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



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~implicit }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~plot }% __ONLY ONE__ keyword per line
\keyword{ ~3d }% __ONLY ONE__ keyword per line
