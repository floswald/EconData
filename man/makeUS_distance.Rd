\name{makeUS_distance}
\alias{makeUS_distance}
\title{make Distance between US state centers}
\usage{
  makeUS_distance(root = "~/git/EconData", agg = NULL)
}
\arguments{
  \item{agg}{list of character vectors of state
  abbreviations that should be aggregated into one
  location}
}
\description{
  make Distance between US state centers
}
\examples{
all <- makeUS_distance()
aggregated <- makeUS_distance(agg=list(c("ME","VT"),c("ND","SD","WY")))
}
\seealso{
  Other makers: \code{\link{makeAbbreviations}},
  \code{\link{makeAllData}}, \code{\link{makeCPIUS}},
  \code{\link{makeFHFA}},
  \code{\link{makeLincolnHomeValues}},
  \code{\link{makeMORTGAGE30US}},
  \code{\link{makeMedianIncome}},
  \code{\link{makeUS_coordinates}}
}

