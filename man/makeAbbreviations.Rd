\name{makeAbbreviations}
\alias{makeAbbreviations}
\title{Correspondance of US State with FIPS code, Census Region and Division}
\usage{
  makeAbbreviations(url = "http://www.epa.gov/enviro/html/codes/state.html",
    root = "~/git/EconData")
}
\arguments{
  \item{url}{http://www.epa.gov/enviro/html/codes/state.html}

  \item{root}{path/to/your/package/root}
}
\value{
  NULL. saves data to packageroot/data
}
\description{
  produces a data.table with state names and FIPS
  abbreviations. Also includes census region and division.
}
\details{
  you will most likely want to merge this table into some
  dataset you come across and which is missing full state
  names, or abbreviations, or FIPS code etc.
}
\seealso{
  Other makers: \code{\link{makeAllData}},
  \code{\link{makeMedianIncome}}
}

