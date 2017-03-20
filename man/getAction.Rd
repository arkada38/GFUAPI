\name{getAction}
\alias{getAction}
\title{
Returns a data.frame with profitable actions
}
\description{
Says "buy", "sell" or do "nothing" for data when it profitable
}
\usage{
getAction(df, range.up, range.down)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A given data.frame for testing. Must have a "High", "Low" and "Close" columns
}
  \item{range.up}{
The level for take profit. If contains then action is successfully.
}
  \item{range.down}{
The level for stop loss. If contains then action is not successfully.
}
}
\value{
\item{action}{1 for buy, 0.5 for do nothing and 0 for sell}
\item{action.factor}{Factor with 3 levels: buy, sell, nothing}
\item{buy}{1 for buy and 0 for others}
\item{nothing}{1 for nothing and 0 for others}
\item{sell}{1 for sell and 0 for others}
}
\examples{
data(aapl)
getAction(aapl, range.up = 0.06, range.down = 0.025)
}
