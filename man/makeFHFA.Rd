\name{makeFHFA}
\alias{makeFHFA}
\title{make FHFA expanded house price indices}
\usage{
  makeFHFA(fhfaStates = "http://www.fhfa.gov/webfiles/25831/3q13hpists_expandeddata.txt",
    fhfaCBSA = "http://www.fhfa.gov/webfiles/25833/3q13hpicbsa_expandeddata.txt",
    root = "~/git/EconData")
}
\arguments{
  \item{fhfaStates}{URL to txt state level data}

  \item{fhfaCBSA}{URL to txt 50 larges MSA level data}

  \item{root}{path/to/your/package/root}
}
\value{
  NULL.
}
\description{
  downloads and builds FHFA Expanded-Data Indexes
  (Estimated using Enterprise, FHA, and Real Property
  County Recorder Data Licensed from DataQuick). Data is
  quarterly.
}
\references{
  \url{http://www.fhfa.gov/Default.aspx?Page=87}
}
\seealso{
  Other makers: \code{\link{makeAbbreviations}},
  \code{\link{makeAllData}}, \code{\link{makeCPIUS}},
  \code{\link{makeLincolnHomeValues}},
  \code{\link{makeMedianIncome}},
  \code{\link{makeUS_coordinates}},
  \code{\link{makeUS_distance}}
}

