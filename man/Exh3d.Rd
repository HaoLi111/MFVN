\name{Exh3d}
\alias{Exh3d}

\title{
3d Exhausive Validification
}
\description{
3d Exhausive Validification
}
\usage{
Exh3d(func = function(x,y,z) x^2+y^2+z^2<=sqrt(3^2+3^2+3^2),x=seq(from = -3, to = 3,by=.6),y=seq(from = -3, to = 3,by=.6),z=seq(from = -3, to = 3,by=.6))
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Exh3d(func = function(x,y,z) x^2+y^2+z^2<=sqrt(3^2+3^2+3^2),x=seq(from = -3, to = 3,by=.6),y=seq(from = -3, to = 3,by=.6),z=seq(from = -3, to = 3,by=.6))

## The function is currently defined as

function(func,x=seq(from = -3, to = 3,by=.1),
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ exhausive }% use one of  RShowDoc("KEYWORDS")
