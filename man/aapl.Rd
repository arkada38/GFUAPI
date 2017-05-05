\name{aapl}
\alias{aapl}
\docType{data}
\title{
Historical quotes of AAPL
}
\description{
An xts dataset with historical quotes of AAPL from 2007-03-08 to 2017-03-07
}
\usage{data("aapl")}
\format{
  A data frame with 2517 observations on the following 5 variables.
  \describe{
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
  }
}
\examples{
data(aapl)
## maybe str(aapl) ; plot(aapl) ...
}
\keyword{datasets}
