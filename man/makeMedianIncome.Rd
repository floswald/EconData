\name{makeMedianIncome}
\alias{makeMedianIncome}
\title{make dataset of median income by state and year}
\usage{
  makeMedianIncome(root = "~/git/EconData")
}
\arguments{
  \item{root}{path/to/your/package/root}
}
\value{
  NULL.
}
\description{
  produce dataset in current and 2012 dollars of median
  household income by US state.
}
\details{
  saves 2 lists \code{current} and \code{in2012}, each with
  elements zinc, zse, incl, sel. Those elements are
  \enumerate{ \item zinc: a zoo object of median income
  \item zse: a zoo object of it's standard error \item
  incl: data.frame with median income in long format \item
  sel: data.frame with median income in long format }

  source: census bureau
}
\references{
  \url{http://www.census.gov/hhes/www/income/data/historical/household/}
}
\seealso{
  Other makers: \code{\link{makeAbbreviations}},
  \code{\link{makeAllData}}
}

