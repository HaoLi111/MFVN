\name{DFRead}
\alias{DFRead}
\title{Read and continue data from Dataframe}
\usage{
DFRead(df = data.frame(ind = 1:10,dep=(1:10)*2,oth=-4:5),target=4.125,independent = 1,dependent = 2)
}
\description{
Read and continue data from Dataframe.
Default is by connecting data points with line segments and find the intersect.
}
\examples{
DFRead(df = data.frame(ind = 1:10,dep=(1:10)*2,oth=-4:5),target=4.125,independent = 1,dependent = 2)
}