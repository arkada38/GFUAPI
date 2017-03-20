\name{getAnalysis}
\alias{getAnalysis}
\title{
Volume and spread calculations
}
\description{
Analyze a dataframe from getHistoricalQuote function and return a dataframe with VSA columns only
}
\usage{
getAnalysis(df, period = 15)
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
\item{close.inside.bar}{The relative close of the bar inside the bar}
\item{type.of.bar}{Type of bar}
\item{relative.volume}{The relative volume of bar by period}
\item{relative.height}{The relative height of bar by period}
\item{close.between.extremums}{In which part between the maximum and minimum for the period is the closure}
\item{relative.change.of.closes}{Relative change of closes by period}
\item{relative.open.mean.deviation}{Relative deviations from the mean of open by period}
\item{relative.close.mean.deviation}{Relative deviations from the mean of close by period}
}
\examples{
data(aapl)
head(getAnalysis(aapl))
summary(getAnalysis(aapl, period = c(8, 15)))
}
