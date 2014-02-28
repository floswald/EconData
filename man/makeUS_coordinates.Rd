\name{makeUS_coordinates}
\alias{makeUS_coordinates}
\title{make Coordinate of US State centers}
\usage{
  makeUS_coordinates(root = "~/git/EconData", agg = NULL)
}
\arguments{
  \item{agg}{list of character vectors of state
  abbreviations that should be aggregated into one
  location}
}
\description{
  make Coordinate of US State centers
}
\examples{
all <- makeUS_coordinates()
aggregated <- makeUS_coordinates(agg=list(c("ME","VT"),c("ND","SD","WY")))
}
\references{
  \url{http://staff.washington.edu/glynn/dist_matrix.pdf}
}
\seealso{
  Other makers: \code{\link{makeAbbreviations}},
  \code{\link{makeAllData}}, \code{\link{makeCPIUS}},
  \code{\link{makeFHFA}},
  \code{\link{makeLincolnHomeValues}},
  \code{\link{makeMedianIncome}},
  \code{\link{makeUS_distance}}
}

