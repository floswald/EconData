

# courtesy of http://stackoverflow.com/questions/9561684/documenting-dataset-with-roxygen2


#' @name US_medinc
#' @title US median household income by state and year
#' @description contains 2 lists \code{current} and \code{in2012},
#' which stands for median income in current dollars or 2012 dollars.
#' each list has elements zinc, zse, incl, sel. 
#'  
#' @details Each of \code{current} and \code{in2012} contains
#' \enumerate{
#' \item zinc: a zoo object of median income
#' \item zse: a zoo object of it's standard error
#' \item incl: data.frame with median income in long format
#' \item sel: data.frame with median income in long format
#' }
#' 
#' @docType data
#' @usage data(US_medinc)
#' @format zoo objects (wide) and data.frames(long)
#' @source \url{http://www.census.gov/hhes/www/income/data/historical/household/}
#' @author Florian Oswald
NULL
