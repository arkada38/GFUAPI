\name{getHistoricalPrices}
\alias{getHistoricalPrices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Downloading historical prices to xts object
}
\description{
Downloads a historical prices data to xts object
}
\usage{
getHistoricalPrices(symbol, exchange, interval = 86400, period = "10Y")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol}{
The code of the security. For example, GOOG for Google or EURUSD for the Euro/Dollar currency pair.
}
  \item{exchange}{
The exchange where the security is listed. For example, NASDAQ for GOOG or CURRENCY for EURUSD. Can be empty for stocks.
}
  \item{interval}{
A number in seconds. Its minimum value is 60 seconds.
}
  \item{period}{
The period of time from which data will be returned. Always returns the most recent data. Examples of this parameter are 1d (one day), 1w (one week), 1m (one month), or 1y (one year).
}
}
\value{
\item{Open}{Open prices}
\item{High}{High prices}
\item{Low}{Low prices}
\item{Close}{Close prices}
\item{Volume}{Volume}
}

\examples{
getHistoricalPrices("AAPL", "NASD")

getHistoricalPrices("GBPUSD", "CURRENCY", interval = 900, period = "3d")
}
