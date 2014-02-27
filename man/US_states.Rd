\docType{data}
\name{US_states}
\alias{US_states}
\title{Correspondance of US State with FIPS code, Census Region and Division}
\format{long data.table}
\source{
  \url{http://www.epa.gov/enviro/html/codes/state.html},
  \url{http://www.census.gov/popest/about/geo/codes.html}
}
\description{
  produces a data.table with state names and FIPS
  abbreviations. Also includes census region and division.

  you will most likely want to merge this table into some
  dataset you come across and which is missing full state
  names, or abbreviations, or FIPS code etc.
}
\author{
  Florian Oswald
}
\keyword{datasets}

