\name{contour.function}
\alias{contour.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create implicit plot with x,y base
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contour.function(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x is a function that return value or boolean vector
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
\code{\link{contour}}, ~~~
\code{\link{Exh2d}}, ~~~
\code{\link{ImpPlot2d}},~~~
}
\examples{
contour(function(x,y) x^2 - .5*y^2<=.7)

## The function is currently defined as
contour.function<-function(x,xbase=seq(from = -5,to=5,by=.1),ybase=seq(from = -5,to=5,by=.1)){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~implicit }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~2d }% __ONLY ONE__ keyword per line
