


# this file contains functions to build RDatasets
# from raw data

#' make all datasets
#'
#' @param root path/to/your/package/root
#' @family makers
makeAllData <- function(root="~/git/EconData"){
	cat('building all datasets\n')
	if (!file.exists(file.path(root,"data"))){
		cat(sprintf("first you have to create a data directory at %s\n",file.path(root,"data")))
		return(FALSE)
	}
	makeAbbreviations(root=root)
	makeMedianIncome(root=root)
	makeFHFA(root=root)
	makeLincolnHomeValues(root=root)
	makeUS_coordinates(root=root)
  makeUS_distance(root=root)
	makeCPIUS(root=root)
	return(TRUE)
}

#' makes Correspondance of US State with FIPS code, Census Region and Division 
#'
#' produces a data.table with state names and FIPS 
#' abbreviations. Also includes census region and division.
#'
#' you will most likely want to merge this table into some 
#' dataset you come across and which is missing full state names,
#' or abbreviations, or FIPS code etc.
#'
#' @param url http://www.epa.gov/enviro/html/codes/state.html
#' @param root path/to/your/package/root
#' @return NULL. saves data to packageroot/data
#' @family makers
makeAbbreviations <- function(url="http://www.epa.gov/enviro/html/codes/state.html",
							  root="~/git/EconData"){

	# get abbreviations
	tab <- readHTMLTable(url)
	states <- data.table(tab[[1]])
	setnames(states,c("Abbreviation","FIPS","STATE"))
	states[,STATE := as.character(STATE)]
	states[,Abbreviation:= as.character(Abbreviation)]
	states[,FIPS := as.numeric(as.character(FIPS))]
	setkey(states,FIPS)

	st <- data.table(read.xlsx(file=file.path(root,"inst/extdata/census/state_geocodes_v2011.xls"),sheetIndex=2))
	st[, c("Reg_ID","Div_ID","FIPS") := lapply(st[,list(Reg_ID,Div_ID,FIPS)], function(x) as.numeric(as.character(x)))]
	st[, c("Region","Division","State") := lapply(st[,list(Region,Division,State)], function(x) as.character(x))]

	st[,State := NULL]

	setkey(st,FIPS)

	US_states <- st[states]
	setnames(US_states,"Abbreviation","state")
	setcolorder(US_states,c("FIPS","STATE","state","Reg_ID","Region","Div_ID","Division"))

	save(US_states,file=file.path(root,"data/US_states.RData"))

	return(US_states)
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
#' @param root path/to/your/package/root
#' @family makers
#' @references \url{http://www.census.gov/hhes/www/income/data/historical/household/}
makeMedianIncome <- function(root="~/git/EconData"){

	yrs <- 2012:1984

	# indices
	cols <- list()
	cols$inc <- c(1,seq(from=3,to=60,by=2))
	cols$se  <- c(1,seq(from=4,to=60,by=2))

	# current dollars
	rows <- 8:59

	current <- lapply(cols,function(x) read.xlsx(file=file.path(root,"inst/extdata/census/H08_2012.xls"),sheetIndex=1,rowIndex=rows,colIndex=x,header=FALSE))
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

	in2012 <- lapply(cols,function(x) read.xlsx(file=file.path(root,"inst/extdata/census/H08_2012.xls"),sheetIndex=1,rowIndex=rows,colIndex=x,header=FALSE))
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

	save(medinc.current,medinc.in2012,file=file.path(root,"data/US_medinc.RData"))
	return(NULL)
}




#' make FHFA expanded house price indices
#' 
#' downloads and builds FHFA 
#' Expanded-Data Indexes (Estimated using 
#' Enterprise, FHA, and Real Property County Recorder 
#' Data Licensed from DataQuick). Data is quarterly.
#'
#' @return NULL. 
#' @param fhfaStates URL to txt state level data
#' @param fhfaCBSA URL to txt 50 larges MSA level data
#' @param root path/to/your/package/root
#' @family makers
#' @references \url{http://www.fhfa.gov/Default.aspx?Page=87}
makeFHFA <- function(fhfaStates="http://www.fhfa.gov/webfiles/25831/3q13hpists_expandeddata.txt",
                     fhfaCBSA="http://www.fhfa.gov/webfiles/25833/3q13hpicbsa_expandeddata.txt",
					 root="~/git/EconData"){

	states <- list()
	msa50  <- list()

	states$qtr <- data.table(read.table(file=fhfaStates,sep="\t",header=TRUE))
	msa50$qtr <- data.table(read.table(file=fhfaCBSA,sep="\t",header=TRUE))

	states$yr <- states$qtr[,list(index_nsa=mean(index_nsa),index_sa=mean(index_sa)),by=list(state,yr)]
	msa50$yr <- msa50$qtr[,list(index_nsa=mean(index_nsa),index_sa=mean(index_sa)),by=list(CBSA,Metropolitan_Area_Name,yr)]

	# create date variable in qtr
	states$qtr[, quarter := as.yearqtr(paste0(yr," Q",qtr))]
	states$qtr[, Date    := as.Date(quarter)]
	states$qtr[, c("yr","qtr") := NULL]
	msa50$qtr[, quarter := as.yearqtr(paste0(yr," Q",qtr))]
	msa50$qtr[, Date    := as.Date(quarter)]
	msa50$qtr[, c("yr","qtr") := NULL]

	# rename
	FHFA.states <- states
	FHFA.msa50 <- msa50

	save(FHFA.msa50,file=file.path(root,"data/FHFA_msa50.RData"))
	save(FHFA.states,file=file.path(root,"data/FHFA_states.RData"))
}
	




#' make Lincoln Institute Home and Land Value Dataset
#' 
#' reads data from the Lincoln Institute xls file.
#'
#' @return NULL. 
#' @param root path/to/your/package/root
#' @family makers
#' @references \url{https://www.lincolninst.edu/subcenters/land-values/}
makeLincolnHomeValues <- function(root="~/git/EconData"){

	d <- fread(input="~/git/EconData/inst/extdata/lincolninst/landdata-states-2013q1.csv",skip=1)
	d[,qtr := as.yearqtr(Date)]
	d[,Date := NULL]
	setnames(d,c("State","Home.Value","Structure.Cost","Land.Value","Land.Share","Home.Price.Index","Land.Price.Index","qtr"))
	warning("amounts in HomeValues are current dollars.")
	HomeValues <- d
	save(HomeValues,file=file.path(root,"data/HomeValues.RData"))
}


#' make several US inflation datasets
#'
#' saves cpi for all urban consumers 
#' and housing cpi  to disk
#' @family makers
#' @references \url{http://research.stlouisfed.org/fred2/series/CPIAUCSL} 
#'             \url{http://research.stlouisfed.org/fred2/series/CPIHOSSL} 
#' download data from FRED with quantmod package
makeCPIUS <- function(root="~/git/EconData"){

	getSymbols('CPIAUCSL',src='FRED')
	getSymbols('CPIHOSSL',src='FRED')

	save(CPIAUCSL,file=file.path(root,"data/CPIAUCSL.RData"))
	save(CPIHOSSL,file=file.path(root,"data/CPIHOSSL.RData"))

}




#' Haversine Distance function
#'
#' use the haversine formula to compute distance
#' between 2 locations
#'
#' source: \url{http://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}


# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

deg2rad <- function(deg) return(deg*pi/180)






#' make Coordinate of US State centers
#'
#' @param agg list of character vectors of state abbreviations that should be aggregated into one location
#' @references \url{http://staff.washington.edu/glynn/dist_matrix.pdf}
#' @family makers
#' @examples
#' all <- makeUS_coordinates()
#' aggregated <- makeUS_coordinates(agg=list(c("ME","VT"),c("ND","SD","WY")))
makeUS_coordinates <- function(root="~/git/EconData",agg=NULL){
	coordStates <- data.table(read.table("http://staff.washington.edu/glynn/state.data",header=FALSE))
	setnames(coordStates,c("state","FIPS","lat","long"))
	coordStates$state <- as.character(coordStates$state)

	if (!is.null(agg)){

		# drop FIPS
		coordStates[, FIPS := NULL]

		l <- list()
		for (i in 1:length(agg)){
			l[[i]] <- copy(coordStates[ state %in% agg[[i]] , list(lat=mean(lat),long=mean(long)) ])
			# mean produces weired results. just pick first location for now.
			#l[[i]] <- copy(coordStates[ state %in% agg[[i]] , list(lat=lat[1],long=long[1]) ])
			l[[i]][, state := paste(agg[[i]],collapse=".")]
			setcolorder(l[[i]],c("state","lat","long"))
		}

		# remove individual states
		# and add to list
		l[[length(agg) + 1]] <- coordStates[ !state %in% unlist(agg) ]

		coordStates_agg <- rbindlist(l)

		save(coordStates_agg,file=file.path(root,"data/coordStates_agg.RData"))
		return(coordStates_agg)
	} else {
	
		save(coordStates,file=file.path(root,"data/coordStates.RData"))
		return(coordStates)
	}


}



#' make Distance between US state centers
#'
#' @param agg list of character vectors of state abbreviations that should be aggregated into one location
#' @family makers
#' @examples
#' all <- makeUS_distance()
#' aggregated <- makeUS_distance(agg=list(c("ME","VT"),c("ND","SD","WY")))
makeUS_distance <- function(root="~/git/EconData",agg=NULL){

	if(!is.null(agg)){
		cSt <- makeUS_coordinates(root=root,agg=agg)
	} else {

		data(coordStates,package="EconData")
		cSt <- coordStates

	}

	# convert degrees to radians
	cSt[ ,lat := deg2rad(lat)]
	cSt[ ,long := deg2rad(long)]

	n <- nrow(cSt)

	m <- matrix(0,n,n)

	for (i in 1:n) {

		for (j in 1:n){

			if (i!=j) {

				m[i,j] <- round(gcd.hf(long1=cSt[i,]$long,
								 lat1=cSt[i,]$lat,
								 long2=cSt[j,]$long,
								 lat2=cSt[j,]$lat))
			}
		}
	}
	rownames(m) <- cSt$state
	colnames(m) <- cSt$state
	if (!is.null(agg)){
		State_distMat_agg <- m
		save(State_distMat_agg,file=file.path(root,"data/State_distMat_agg.RData"))

		State_distTable_agg = data.table(melt(State_distMat_agg))
		setnames(State_distTable_agg,c("from","to","km"))
		save(State_distTable_agg,file=file.path(root,"data/State_distTable_agg.RData"))

		return(list(mat=State_distMat_agg,tab=State_distTable_agg))

	} else {

		State_distMat <- m
		save(State_distMat,file=file.path(root,"data/State_distMat.RData"))

		State_distTable = data.table(melt(State_distMat))
		setnames(State_distTable,c("from","to","km"))
		save(State_distTable,file=file.path(root,"data/State_distTable.RData"))

		return(list(mat=State_distMat,tab=State_distTable))

	}



}




