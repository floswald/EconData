


# this file contains functions to build RDatasets
# from raw data


#' make a US State <-> Abbreviation data.table
#'
#' @param url
#' @return NULL. saves data to packageroot/data
makeAbbreviations <- function(url="http://www.epa.gov/enviro/html/codes/state.html"){

	tab <- readHTMLTable(url)
	states <- data.table(tab[[1]])
	setnames(st,c("Abbreviation","FIPS","State"))
	states[,State := as.character(State)]
	states[,Abbreviation:= as.character(Abbreviation)]
	states[,FIPS := as.numeric(as.character(FIPS))]

	save(states,file="~/git/EconData/US_states.RData")

	return(NULL)
}





#' make dataset of median income by state and year
#'
#' produce dataset in current and 2012 dollars of
#' median household income by US state.
#'
#' saves 2 lists \code{current} and \code{in2012}, each 
#' with elements zinc, zse, incl, sel. Those elements are
#' \enumerate{
#' \item zinc: a zoo object of median income
#' \item zse: a zoo object of it's standard error
#' \item incl: data.frame with median income in long format
#' \item sel: data.frame with median income in long format
#' }
#' 
#' source: census bureau
#' @return NULL. 
#' @references \url{http://www.census.gov/hhes/www/income/data/historical/household/}
makeMedianIncome <- function(){

	yrs <- 2012:1984

	# indices
	cols <- list()
	cols$inc <- c(1,seq(from=3,to=60,by=2))
	cols$se  <- c(1,seq(from=4,to=60,by=2))

	# current dollars
	rows <- 8:59

	current <- lapply(cols,function(x) read.xlsx(file="~/git/EconData/inst/extdata/census/H08_2012.xls",sheetIndex=1,rowIndex=rows,colIndex=x,header=FALSE))
	names(current$inc) <- c("State",paste0(yrs))
	names(current$se) <- c("State",paste0(yrs))

	# get long dataset
	current$incl <- melt(current$inc,id.vars="State")
	names(current$incl)[2:3] <- c("Year","medinc")
	current$sel <- melt(current$se,id.vars="State")
	names(current$sel)[2:3] <- c("Year","se")

	# make zoo object
	rownames(current$inc) <- current$inc$State
	rownames(current$se) <- current$se$State
	current$inc <- current$inc[,-1]
	current$se <- current$se[,-1]

	current$inc <- t(current$inc)
	current$se <- t(current$se)

	current$zinc <- as.zoo(ts(current$inc[nrow(current$inc):1,],start=1984))
	current$zse <- as.zoo(ts(current$se[nrow(current$se):1,],start=1984))

	current$inc <- NULL
	current$se <- NULL

	medinc.current <- current


	# 2012 dollars
	rows <- rows + 53

	in2012 <- lapply(cols,function(x) read.xlsx(file="~/git/EconData/inst/extdata/H08_2012.xls",sheetIndex=1,rowIndex=rows,colIndex=x))

	in2012 <- lapply(cols,function(x) read.xlsx(file="~/git/EconData/inst/extdata/census/H08_2012.xls",sheetIndex=1,rowIndex=rows,colIndex=x,header=FALSE))
	names(in2012$inc) <- c("State",paste0(yrs))
	names(in2012$se) <- c("State",paste0(yrs))

	# get long dataset
	in2012$incl <- melt(in2012$inc,id.vars="State")
	names(in2012$incl)[2:3] <- c("Year","medinc")
	in2012$sel <- melt(in2012$se,id.vars="State")
	names(in2012$sel)[2:3] <- c("Year","se")

	# make zoo object
	rownames(in2012$inc) <- in2012$inc$State
	rownames(in2012$se) <- in2012$se$State
	in2012$inc <- in2012$inc[,-1]
	in2012$se <- in2012$se[,-1]

	in2012$inc <- t(in2012$inc)
	in2012$se <- t(in2012$se)

	in2012$zinc <- as.zoo(ts(in2012$inc[nrow(in2012$inc):1,],start=1984))
	in2012$zse <- as.zoo(ts(in2012$se[nrow(in2012$se):1,],start=1984))

	in2012$inc <- NULL
	in2012$se <- NULL
	
	medinc.in2012 <- in2012

	save(medinc.current,medinc.in2012,file="~/git/EconData/data/US_medinc.RData")
	return(NULL)
}

