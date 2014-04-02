


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
	makeMORTGAGE30US(root=root)
	makeInterstateMig(root=root)
	makeOwnershipRates(root=root)
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
	current$Year <- as.character(current$Year)
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

	medinc_current <- current


	# 2012 dollars
	rows <- rows + 53

	in2012 <- lapply(cols,function(x) read.xlsx(file=file.path(root,"inst/extdata/census/H08_2012.xls"),sheetIndex=1,rowIndex=rows,colIndex=x,header=FALSE))
	names(in2012$inc) <- c("State",paste0(yrs))
	names(in2012$se) <- c("State",paste0(yrs))

	# get long dataset
	in2012$incl <- melt(in2012$inc,id.vars="State")
	names(in2012$incl)[2:3] <- c("Year","medinc")
	in2012$Year <- as.character(in2012$Year)
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
	
	medinc_2012 <- in2012

	save(medinc_2012,file=file.path(root,"data/US_medinc_2012.RData"))
	save(medinc_current,file=file.path(root,"data/US_medinc_current.RData"))
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
#' @param root path/to/your/package/root
#' @family makers
#' @references \url{http://www.fhfa.gov/Default.aspx?Page=87}
makeFHFA <- function(root="~/git/EconData"){

	states <- list()
	msa50  <- list()
	USDiv  <- list()
  
	states$qtr <- data.table(read.table(file=file.path(root,"inst/extdata/FHFA/4q13hpists_expandeddata.txt"),sep="\t",header=TRUE))
	msa50$qtr <- data.table(read.table(file=file.path(root,"inst/extdata/FHFA/4q13hpicbsa_expandeddata.txt"),sep="\t",header=TRUE))
	USDiv$qtr <- data.table(read.table(file=file.path(root,"inst/extdata/FHFA/4q13hpicd_expandeddata.txt"),sep="\t",header=TRUE))

	states$yr <- states$qtr[,list(index_nsa=mean(index_nsa),index_sa=mean(index_sa)),by=list(state,yr)]
	msa50$yr <- msa50$qtr[,list(index_nsa=mean(index_nsa),index_sa=mean(index_sa)),by=list(CBSA,Metropolitan_Area_Name,yr)]
	USDiv$yr <- USDiv$qtr[,list(index_nsa=mean(index_nsa)),by=list(cd,yr)]

	# create date variable in qtr
	states$qtr[, quarter := as.yearqtr(paste0(yr," Q",qtr))]
	states$qtr[, Date    := as.Date(quarter)]
	states$qtr[, c("yr","qtr") := NULL]
	msa50$qtr[, quarter := as.yearqtr(paste0(yr," Q",qtr))]
	msa50$qtr[, Date    := as.Date(quarter)]
	msa50$qtr[, c("yr","qtr") := NULL]
	USDiv$qtr[, quarter := as.yearqtr(paste0(yr," Q",qtr))]
	USDiv$qtr[, Date    := as.Date(quarter)]
	USDiv$qtr[, c("yr","qtr") := NULL]

	# rename
	FHFA_states <- states
	FHFA_msa50 <- msa50
	FHFA_Div   <- USDiv

	save(FHFA_msa50,file=file.path(root,"data/FHFA_msa50.RData"))
	save(FHFA_Div,file=file.path(root,"data/FHFA_Div.RData"))
	save(FHFA_states,file=file.path(root,"data/FHFA_states.RData"))
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

	d <- fread(input=file.path(root,"inst/extdata/lincolninst/landdata-states-2013q1.csv"),skip=1)
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


#' make 30 year FRM rates dataset
#'
#' saves the Freddie Mac 30 year FRM index
#' to disk
#' @family makers
#' @references \url{http://research.stlouisfed.org/fred2/series/MORTGAGE30US} 
#' download data from FRED with quantmod package
makeMORTGAGE30US <- function(root="~/git/EconData"){

	getSymbols('MORTGAGE30US',src='FRED')

	save(MORTGAGE30US,file=file.path(root,"data/US_MortgageRates.RData"))

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


#' make Interstate Migration Transition Matrix
#'
#' @param root
makeInterstateMig <- function(root="~/git/EconData"){

	idx <- seq(from=13,to=21,by=2)
	while (tail(idx,1) < 121) {
		newfrom <- tail(idx,1) + 3
		newto <- newfrom + 8
		idx <- c(idx,seq(newfrom,newto,by=2))
	}
	idx <- idx[idx<121]


	d <- read.xlsx(file=file.path(root,"inst/extdata/census/State_to_State_Migrations_Table_2012.xls"),
				   sheetIndex=1,
				   rowIndex=c(12:16,18:22,24:28,30:34,36:40,42,43,49:53,55:59,61:65,67:71,73:76),
				   colIndex=c(1,2,6,10,idx),header=FALSE)

	names(d) <- c("current","pop.current","instate.mig",as.character(d$V1))

	load(file.path(root,"data/US_states.RData"))
	abbr <- US_states[,list(State=tolower(STATE),state)]

	snames <- names(d)
	snames <- sub("\\s+$", '',snames)	# remove white space at end of string
	snames <- data.frame(State=tolower(snames))
	snames <- merge(snames,abbr,"State")

	names(d)[4:ncol(d)] <- as.character(snames$state)
	d[,1] <- names(d)[4:ncol(d)]
	d[,-1] <- apply(d[,-1],2,as.numeric)
	frac <- as.matrix(d[,-1])

	# as percentage of current population
	dd <- d
	dd[,-1] <- t(apply(frac,1,function(X) X / X[1]))

	library(reshape)
	m <- melt(dd[,-c(2,3)],"current")
	w.own <- m
	w.own[w.own$current==w.own$variable,]$value <- dd$instate.mig

	level <- d
	props <- m
	props2 <- w.own
	state.migration <- list(level=level,props=props,props.with.own=props2)

	save(state.migration,file=file.path(root,"data/Migration.RData"))
	return(state.migration)
}


#' make Homeownership rates by state over time
#'
#' @param root
makeOwnershipRates <- function(root="~/git/EconData"){



	rows <- list(9:59,70:120,131:181,192:242,253:303,314:364,375:425,436:486)
	years <- 2012:2005

	tabs <- list()
	for (i in 1:length(years)){
		cat("reading year",years[i],"\n")
		tabs[[i]]          <- read.xlsx(file=file.path(root,"inst/extdata/census/tab3_state05_2012_hmr.xls"),sheetName="A",rowIndex=rows[[i]],colIndex=c(1,seq(2,8,by=2)),header=FALSE)
		tmp                <- read.xlsx(file=file.path(root,"inst/extdata/census/tab3_state05_2012_hmr.xls"),sheetName="A",rowIndex=rows[[i]],colIndex=c(1,seq(3,9,by=2)),header=FALSE)
		tabs[[i]][,1]      <- gsub("\\.+$","",tabs[[i]][,1])
		tmp[,1]            <- gsub("\\.+$","",tmp[,1])
		names(tabs[[i]])   <- c("State",paste(years[i]," Q",1:4,sep=""))
		names(tmp)         <- c("State",paste(years[i]," Q",1:4,sep=""))
		tabs[[i]]          <- melt(tabs[[i]],"State")
		tabs[[i]]$variable <- as.Date(as.yearqtr(tabs[[i]]$variable))
		names(tabs[[i]])   <- c("State","Date","own.rate")
		tmp                <- melt(tmp,"State")
		tabs[[i]]$se       <- tmp$value
	}

	names(tabs) <- paste("y",years,sep="")
	Ownership <- rbindlist(tabs)

	save(Ownership,file=file.path(root,"data/Ownership.RData"))
}




