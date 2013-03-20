##  Data Handeling/conversion functions

##  Converts flow data from cfs or cmd to mm depth per day
ConvFlow <- function(cfs=NULL, cmd=NULL, WA, mmd=NULL, AREAunits = "mi2"){
	feetpermile <- 5280
	mmperfoot 	<- 304.8
	secondsperday 	<- 3600 * 24
	if (!is.null(cfs) & AREAunits == "mi2"){
		mm_d <- cfs * secondsperday * mmperfoot / WA / (feetpermile^2) 
		return(mm_d)
	} else if (! is.null(cmd)){  # Assume are in km2
		mm_d <- cmd /WA/1000
		return(mm_d)
	} else {
		cfs <- mmd * WA * (feetpermile^2) / (secondsperday * mmperfoot)
		return(cfs)
	}
}


##  Will fill in a time series with NA values in the missing days
FillMissing <- function(dateSeries, meas){
	startDate <- dateSeries[1]
	endDate <- dateSeries[length(dateSeries)]
	FullSeries <- seq(startDate,endDate, by=1)
	measFilled <- rep(NA,length(FullSeries))
	for (i in 1:length(FullSeries)){
		if (length(which(dateSeries == FullSeries[i])) == 1) {
			measFilled[i] <- meas[which(dateSeries == FullSeries[i])]
		}
	}
	return(data.frame(Date=FullSeries, x = measFilled))
}





