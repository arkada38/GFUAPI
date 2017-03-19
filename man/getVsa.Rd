\name{getVsa}
\alias{getVsa}
\title{
Volume and spread calculations
}
\description{
Analyze a dataframe from getHistoricalQuote function and return a dataframe with VSA columns only
}
\usage{
getVsa(df, period = 15)
}
\arguments{
  \item{df}{
The dataframe from getHistoricalQuote function.
Or just with few columns - "High", "Low", "Close" and "Volume". But the data.frame must be sorted from oldest to newest.
}
  \item{period}{
Vector with number of observations for SMA calculation
}
}
\value{
\item{C}{The relative close of the bar inside the bar}
\item{B}{Type of bar}
\item{V}{The relative volume of bar by period}
\item{H}{The relative height of bar by period}
\item{Ce}{The relative close of bar by period}
\item{M}{Relative price change of the bar by period}
}
\examples{
data(aapl)
head(getVsa(aapl))
summary(getVsa(aapl, period = c(8, 15)))
}
