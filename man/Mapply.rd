\name{Mapply}
\alias{Mapply}
\title{Apply a function for 2 independent variables and 1 dependent variable}
\usage{
Mapply(x=seq(from=-5, to=5, by=.1),y=-20:15,f = 'x^2+y*32-x*y')
}
\description{
Repeat for 2 vectors perpendicular placed
}
\examples{
Mapply(x=seq(from=-5, to=5, by=.1),y=-20:15,f = 'x^2+y*32-x*y')
Mapply(x=seq(from=-5, to=5, by=.1),y=-20:15,f = function(x,y) x^2+y*32-x*y)
}