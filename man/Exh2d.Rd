\name{Exh2d}
\alias{Exh2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
2d Exhausive Validification
}
\description{
3d Exhausive Validification
}
\usage{
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func}{
If a function is used to define the validification it should return a boolean vector with 3 independent variables in order e.g. x,y,z
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
\code{\link{contour.function}}, ~~~
\code{\link{ImpPlot2d}}, ~~~
}
\examples{
Exh2d(func = function(x,y) x^2+y^2<=sqrt(3^2+3^2),x=seq(from = -3, to = 3,by=.6),y=seq(from = -3, to = 3,by=.6))
## The function is currently defined as
function(func, x = seq(from = -5,to = 5, by=.1),
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
