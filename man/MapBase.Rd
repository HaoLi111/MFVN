\name{MapBase}
\alias{MapBase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Location Base of a matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MapBase(x, y, xscale, yscale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{xscale}{
%%     ~~Describe \code{xscale} here~~
}
  \item{yscale}{
%%     ~~Describe \code{yscale} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
MapBase(4,4)
## The function is currently defined as
function(x,y,xscale,yscale){
  x=0:x
  y=0:y
  x=(x[-1]+ x[length(x)])/2
  y=(y[-1]+ y[length(y)])/2
  return(list(BaseX=MatBase(y*yscale,x),
              BaseY=t(MatBase(x*xscale,y))
              ))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ base }% use one of  RShowDoc("KEYWORDS")

