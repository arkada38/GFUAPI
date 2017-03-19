\name{getVsaWithPrevious}
\alias{getVsaWithPrevious}
\title{
Returns vsa data.frame with previous records
}
\description{
Makes a copy of a vsa data.frame with offset
}
\usage{
getVsaWithPrevious(df.vsa.only, n.of.prevs)
}
\arguments{
  \item{df.vsa.only}{
The data.frame from getVsa function
}
  \item{n.of.prevs}{
The quantity of previous records for offset
}
}
\examples{
data(aapl)
vsa <- getVsa(aapl)
getVsaWithPrevious(vsa, 3)
}
