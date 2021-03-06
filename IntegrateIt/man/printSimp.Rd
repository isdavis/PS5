% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printSimp.R
\name{printSimp}
\alias{printSimp}
\alias{printSimp,ANY-method}
\title{Printing result of the Simpson Rule}
\usage{
printSimp(object)
}
\arguments{
\item{object}{An object of class Simpson}
}
\value{
An evaluation of the Simpson rule for the given object of class Simpson
}
\description{
Prints the result of the Simpson rule, given an object of class Simpson
}
\note{
This is a very simple function
}
\examples{

i<-new("Simpson", x=c(1,2,3,4,5), y=c(1,2,3,1,1), ab=c(1,5))
printSimp(i)

}
\seealso{
\code{\link{printSimp}}
}
\author{
Ian Davis
}
