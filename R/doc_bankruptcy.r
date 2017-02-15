

#' Total US non-business bankruptcy filings by State
#' 
#' Annual dataset of the number of non-business bankruptcy filings for 1960-2016 by US state. 
#' 
#' @format Data frame with one column per state. Data are counts of bankruptcy cases.
#' 
#' @source \url{http://www.abi.org/newsroom/bankruptcy-statistics}, but syndicated via the Federal Reserve System, so more data than contained in the public website.
#' @author Florian Oswald
#' @usage data(ABI_tot)
"ABI_tot"

#' Number of chapter 7 bankruptcy filings by State
#' 
#' Annual dataset of the number of chapter 7 bankruptcy filings for 1991-2016 by US state. 
#' 
#' @format Data frame with one column per state. Data are counts of chapter 7 bankruptcy cases.
#' 
#' @source \url{http://www.abi.org/newsroom/bankruptcy-statistics}, but syndicated via the Federal Reserve System, so more data than contained in the public website.
#' @author Florian Oswald
#' @usage data(ABI_ch7)
"ABI_ch7"

#' Number of chapter 13 bankruptcy filings by State
#' 
#' Annual dataset of the number of chapter 13 bankruptcy filings for 1991-2016 by US state. 
#' 
#' @format Data frame with one column per state. Data are counts of chapter 13 bankruptcy cases.
#' 
#' @source \url{http://www.abi.org/newsroom/bankruptcy-statistics}, but syndicated via the Federal Reserve System, so more data than contained in the public website.
#' @author Florian Oswald
#' @usage data(ABI_ch13)
"ABI_ch13"


#' Chapter 7 bankruptcy Rate by State
#' 
#' Annual dataset of the Chapter 7 bankruptcy Rate for 1991-2016 by US state. The Rate is defined in percent of local population. I.e. a value of 0.27 means that 0.27% of the population of a given state in a given year is in chapter 7 bankruptcy. The rates are obtained by dividing \code{\link{ABI_ch7}} by \code{\link{Fed_pop_count}}.
#' 
#' @format Data frame with one column per state. Data are percent of local population in bankruptcy.
#' 
#' @source \url{http://www.abi.org/newsroom/bankruptcy-statistics}, but syndicated via the Federal Reserve System, so more data than contained in the public website.
#' @author Florian Oswald
#' @usage data(ABI_ch7_rate)
"ABI_ch7_rate"

#' Chapter 13 bankruptcy Rate by State
#' 
#' Annual dataset of the Chapter 13 bankruptcy Rate for 1991-2016 by US state. The Rate is defined in percent of local population. I.e. a value of 0.27 means that 0.27% of the population of a given state in a given year is in chapter 13 bankruptcy.
#' 
#' @format Data frame with one column per state. Data are percent of local population in bankruptcy. The rates are obtained by dividing \code{\link{ABI_ch13}} by \code{\link{Fed_pop_count}}.
#' 
#' @source \url{http://www.abi.org/newsroom/bankruptcy-statistics}, but syndicated via the Federal Reserve System, so more data than contained in the public website.
#' @author Florian Oswald
#' @usage data(ABI_ch13_rate)
"ABI_ch13_rate"

#' US State Population Count
#' 
#' Annual data on population by US state
#' 
#' @format Data frame with one column per state. Data are the number of people residing in a state.
#' 
#' @source Syndicated via the Federal Reserve System.
#' @author Florian Oswald
#' @usage data(Fed_pop_count)
"Fed_pop_count"


#' Rate of Foreclosure Starts
#' 
#' "Foreclosures started" refers to the percentage rate of loans for which a foreclosure has been initiated during the quarter, that is, the number of loans sent to the foreclosure process as a percentage of the total number of mortgages in the pool.
#' 
#' @format Data frame with one column per state. 
#' 
#' @source Mortgage Banker's Association syndicated via the Federal Reserve System.
#' @author Florian Oswald
#' @usage data(fore_rate)
"MBA_fore_rate"