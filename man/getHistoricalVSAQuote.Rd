\name{getHistoricalVSAQuote}
\alias{getHistoricalVSAQuote}
\title{
Volume and spread calculations
}
\description{
Analyze a dataframe from getHistoricalQuote function and return a dataframe with VSA columns
}
\usage{
getHistoricalVSAQuote(data, period = 15)
}
\arguments{
  \item{data}{
The dataframe from getHistoricalQuote function
}
  \item{period}{
Number of observations for SMA calculation
}
}
\value{
\item{UTC_Date}{POSIXct UTC Date}
\item{Market_Date}{POSIXct Market Date}
\item{Open}{Open prices}
\item{High}{High prices}
\item{Low}{Low prices}
\item{Close}{Close prices}
\item{Volume}{Volume}
\item{C}{The relative close of the bar inside the bar}
\item{B}{Type of bar}
\item{V}{The relative volume of bar}
\item{H}{The relative height of bar}
\item{Ce}{The relative close of bar}
\item{M}{Relative price change of the bar}
}
\examples{
data(aapl)
head(getHistoricalVSAQuote(aapl))
summary(getHistoricalVSAQuote(aapl))
}
