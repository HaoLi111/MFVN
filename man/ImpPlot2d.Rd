\name{ImpPlot2d}
\alias{ImpPlot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
2D implicit Plot
}
\description{
Exhausively point out the data that satisfy some functions of x and y.
}
\usage{
ImpPlot2d(func = function(x,y) abs(x^2 +2*y^2-2)<=1e-2,x = seq(from=0,to=2,by=.01),y = seq(from=0,to=2,by=.01))

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

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hao Li
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{contour.function}}, ~~~
\code{\link{ImpPlot2d}}, ~~~
}
\examples{ImpPlot2d(func = function(x,y) abs(x^2 +2*y^2-2)<=1e-2,x = seq(from=0,to=2,by=.01),y = seq(from=0,to=2,by=.01))
#Currently defined as
#ImpPlot2d<-function(func,...) UseMethod('ImpPlot2d')
function(func, x = seq(from = -5,to = 5, by=.1),
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ implicit }% use one of  RShowDoc("KEYWORDS")
\keyword{ plot }% use one of  RShowDoc("KEYWORDS")
