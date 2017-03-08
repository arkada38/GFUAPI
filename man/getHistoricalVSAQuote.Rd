\name{getHistoricalVSAQuote}
\alias{getHistoricalVSAQuote}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volume and spread calculations
}
\description{
Analyze a dataframe from getHistoricalQuote function and return a dataframe with VSA columns
}
\usage{
getHistoricalVSAQuote(data, period = 15, standardize = F)
}
\arguments{
  \item{data}{
The dataframe from getHistoricalQuote function
}
  \item{period}{
Number of observations for SMA calculation
}
  \item{standardize}{
True if you want a standardize data
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
\item{B}{Type of bar}
\item{C}{The relative close of the bar inside the bar}
\item{V}{The relative volume of bar}
\item{H}{The relative height of bar}
\item{Ce}{The relative close of bar}
\item{M}{Relative price change of the bar}
}
\examples{
data(aapl)
head(getHistoricalVSAQuote(aapl))
str(getHistoricalVSAQuote(aapl, standardize = TRUE))
}
