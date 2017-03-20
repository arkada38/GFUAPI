\name{getAnalysisWithPrevious}
\alias{getAnalysisWithPrevious}
\title{
Returns analysis data.frame with previous records
}
\description{
Makes a copy of a analysis data.frame with offset
}
\usage{
getAnalysisWithPrevious(df.analysis.only, n.of.prevs)
}
\arguments{
  \item{df.analysis.only}{
The data.frame from getAnalysis function
}
  \item{n.of.prevs}{
The quantity of previous records for offset
}
}
\examples{
data(aapl)
analysis <- getAnalysis(aapl)
getAnalysisWithPrevious(analysis, 3)
}
