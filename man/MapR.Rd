\name{MapR}
\alias{MapR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance between spatial points
}
\description{
Calculate N dimensional 'distance' from given coordinates.
}
\usage{
MapR(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
coordinate of the first point
}
  \item{b}{
coordinate of the second point
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
MapR(c(1,2),c(3,5))
## The function is currently defined as
function (a, b)
sqrt(sum((a - b)^2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ radius }% use one of  RShowDoc("KEYWORDS")
\keyword{ distance }% __ONLY ONE__ keyword per line
