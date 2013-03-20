##		Watershed Model  - All watersheds
# 		March 2013 version
#		This version will read in actual precip measured at volunteer sites (use when updating the recorded data)  
#  USE at 10:30 am, when co-op data is also available

##  Get watershed run input:
source("Watershed_Input.R")  # watershed-specific info


###################################################################################################################################
load("InputOutput/PrevRun.RData")  ##  Gives model output from earlier (data frame: PrevRun)
load("InputOutput/InputToModel.RData")  
	##  This gives us the input values from yesterday, we will add to them today. 
	##	(vectors: DS, Pmm, TxC, TnC, data frame: PrevSM)

PastDay <- Past24hrData(state=STATE, CoopStns = COOPstationsBEST, CoopBkup = COOPstationsBACKUP, NOAAstn = NOAA_STATIONS, UseCoopPrecip = UseCoop, PrintDataTables = PRINT_TABLES)	
print(PastDay)  

Forecasts <- forcastsNOAA(stn = NOAA_STATIONS, probCut = 30, numbDays = 5, PrintDataTables = PRINT_TABLES)
Pred_P <- apply(Forecasts$Pmm, MARGIN=1, mean)
Pred_Tx <- apply(Forecasts$MxT, MARGIN=1, mean)
Pred_Tn <- apply(Forecasts$MnT, MARGIN=1, mean)
Pred_Prob <- apply(Forecasts$PrbPrecip, MARGIN=1, max)

# Append our OBSERVED input data   
today <- as.Date(Sys.time(), tz="EST")  	# 	Today's date
yesterday <- today-1
DS<-c(DS[1:which(DS==yesterday)],today)   				# 	Date Series
tail(DS)  						## 	print this to make sure we have no duplications...
Pmm <- c(Pmm[1:which(DS==yesterday)], PastDay$P_mm) 			# Precip, mm
TxC <- c(TxC[1:which(DS==yesterday)], PastDay$Tx) 				# Daily Max Temp, C
TnC <- c(TnC[1:which(DS==yesterday)], PastDay$Tn) 				# Daily Min Temp, C

if (opt == "PrevRun"){  # Unless we have problems, we should use this option (remove other option?)
	len <- 1
	len1 <- length(DS)
	Start30DaysAgo <- ifelse(len1 <(30-DAYS_PREDICTION), 1, len1-(29-DAYS_PREDICTION))
	ObsFor <- data.frame(			## These are the observed values and the predicted values to be input into the model
		Date = c(DS[len1], seq(today+1, today+DAYS_PREDICTION, by=1)), 
		Pmm = c(Pmm[len1], Pred_P[1:DAYS_PREDICTION]), 
		TxC = c(TxC[len1], Pred_Tx[1:DAYS_PREDICTION]), 
		TnC = c(TnC[len1], Pred_Tn[1:DAYS_PREDICTION]) 
		)
	SM <- SnowMelt(Date=ObsFor$Date, precip_mm=ObsFor$Pmm, Tmax_C=ObsFor$TxC, Tmin_C=ObsFor$TnC, lat_deg=LAT_DEG, 
			startingSnowDepth_m = PrevSM$SnowDepth_m[which(PrevSM$Date==yesterday)], 
			startingSnowDensity_kg_m3 = ifelse(PrevSM$SnowDepth_m[which(PrevSM$Date==yesterday)] > 0, 			PrevSM$SnowWaterEq_mm[which(PrevSM$Date==yesterday)]/PrevSM$SnowDepth_m[which(PrevSM$Date==yesterday)],			450),
			YestSnowTemp = PrevSM$SnowTemp[which(PrevSM$Date==yesterday)],
			YestAlbedo = PrevSM$Albedo[which(PrevSM$Date==yesterday)],
			SnowMeltYest = PrevSM$SnowMelt_mm[which(PrevSM$Date==yesterday)] /1000,
			YestEnergy = PrevSM$NetEnergy[which(PrevSM$Date==yesterday)],
			Av30dayDeltaT = mean((TxC[Start30DaysAgo:len1]-TnC[Start30DaysAgo:len1]))
			)

}else {
	len <- length(DS)
	ObsFor <- data.frame(			## These are the observed values and the predicted values to be input into the model
		Date = c(DS, seq(today+1, today+DAYS_PREDICTION, by=1)), 
		Pmm = c(Pmm, Pred_P[1:DAYS_PREDICTION]), 
		TxC = c(TxC, Pred_Tx[1:DAYS_PREDICTION]), 
		TnC = c(TnC, Pred_Tn[1:DAYS_PREDICTION]) 
		)
	SM <- SnowMelt(Date=ObsFor$Date, precip_mm=ObsFor$Pmm, Tmax_C=ObsFor$TxC, Tmin_C=ObsFor$TnC, lat_deg=LAT_DEG)
}	
	
rsm <- SM$SnowMelt_mm + SM$Rain_mm  ## new input to stream function


######################################################################################################
##################### 				Streamflow model			############################
TodaysMR	<-	StreamflowFN(
		Depth				= SOIL_DEPTH, 		# Average wshd soil depth (mm) 
		SATper				= POROSITY , 	# Porosity (fraction)	
		AWCper				= AWC_PERCENTAGE, 	# Available Water Capacity (AWC) (fraction) 
		latitudeDegrees		= LAT_DEG, 
		P					= rsm[1:(len+DAYS_PREDICTION)], 	# Rain + Snow melt (mm)
		dateSeries			= ObsFor$Date, 		# Date series (format should be: "2003-02-36")
		Tmax				= ObsFor$TxC, 				# Max daily temperature (C)
		Tmin				= ObsFor$TnC, 				# Max daily temperature (C)
		percentImpervious	= IMPERVIOUS_AREA, 		# Percent of the watershed that is impervious
		no_wet_class		= WETNESS_CLASSES, 		# The number of wetness classes for saturated area designation
		PreviousOutput		= if(opt=="PrevRun") PrevRun[1:which(PrevRun$Date == yesterday),],	
							# Allows us to take previous model output to initiate the model
		Tp 					= TIME_PEAK 		# Time to peak (hours [0.35*Tc+3.5])
)



#####			Make files for reading to webpage
#####  NOTE:  Edit here to extend the predictions out further than 3 days
##  Here are the percentage saturated values for today, tomorrow and two following days.
n<-nrow(TodaysMR)
TodaysMR$MaxWetClass[(n-3):n]/10  # as a percentage (assuming 10% wetness classes)	
o1 <- paste("var satPercent = ", TodaysMR$MaxWetClass[(n-3)]*10, " ;", sep="")
o2 <- paste("var satPercentTomorrow = ", TodaysMR$MaxWetClass[(n-2)]*10, " ;", sep="")
o3 <- paste("var satPercentDay2 = ", TodaysMR$MaxWetClass[(n-1)]*10, " ;", sep="")
o4 <- paste("var satPercentDay3 = ", TodaysMR$MaxWetClass[n]*10, " ;", sep="")
o5 <- paste("var satPercentMax = ", max(TodaysMR$MaxWetClass[(n-3):n]*10), " ;", sep="")
if (COMPUTER == "server" ){ 
 write.table(rbind(o1,o2,o3,o4,o5),file=paste("../../www/", WSH, "/sat_percent.js", sep=""), quote=FALSE, col.names=FALSE, row.names=FALSE)
 } else  write.table(rbind(o1,o2,o3,o4,o5),file="sat_percent.js", quote=FALSE, col.names=FALSE, row.names=FALSE)
	
	
# write out predictions table 
Date <- vector()
Days<-c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun", "Mon", "Tues")
for(i in 1:3) {
	Date <- append(Date, Days[as.POSIXlt(Sys.time())$wday+1+i])
	# Date <- append(Date, paste(Days[as.POSIXlt(Sys.time())$wday+1+i], ", ", strftime(Sys.time()+86400*i, format="%b %d, %Y"), sep=""))
}
Label<-c("Rain/Snow melt, mm", "Chance of precip (%)", "% watershed saturated")
Today <- c(round(rsm[(n-3)]), NA, TodaysMR$MaxWetClass[(n-3)]*10)
Tomorrow <- c(round(rsm[(n-2)]), Pred_Prob[1], TodaysMR$MaxWetClass[(n-2)]*10)
DayAfter <-	c(round(rsm[n-1]), Pred_Prob[2], TodaysMR$MaxWetClass[(n-1)]*10)
D3 <- c(round(rsm[n]), Pred_Prob[3], TodaysMR$MaxWetClass[(n)]*10)
library(rjson)
if (COMPUTER == "server" ){ 
cat(paste("var t = ", toJSON(data.frame(Date,Label,Today, Tomorrow, DayAfter, D3))), file=paste("/var/www/", WSH,"/table.jsonp", sep=""))
} else cat(paste("var t = ", toJSON(data.frame(Date,Label,Today, Tomorrow, DayAfter, D3))), file="table.jsonp")




############  Now save model and prediction output :

# 	Save Model Run
PrevRun <- rbind(PrevRun[1:which(PrevRun$Date==yesterday),],TodaysMR[which(TodaysMR$Date==today),])
write.csv(PrevRun, file="InputOutput/PrevModelResults.csv")	# Backup save
save(list=c("PrevRun"), file="InputOutput/PrevRun.RData")			# Needed for tomorrow's run

##  Backup of input
PrevSM <- rbind(PrevSM[1:which(PrevSM$Date==yesterday),], SM[which(SM$Date == today),])
save(list=c("DS", "Pmm", "TnC", "TxC", "PrevSM"), file="InputOutput/InputToModel.RData")
input <-data.frame(DS,Pmm,TxC,TnC, PrevSM)
write.csv(input, file="InputOutput/Input.csv")  # Back up of input data

##  Record Predictions and Observations in one file
ID <- c(as.character(today),PastDay$P_mm_NOAA, PastDay$P_mm_coop, PastDay$CoopStnsReporting, Pred_P, PastDay$Tx, Pred_Tx, PastDay$Tn, Pred_Tn, Pred_Prob)

write(ID, file="InputOutput/DataTables/WeatherData1030.csv", append=TRUE, ncolumns=26, sep=",")


##  Plotting modeled streamflow results at the end:
if (! is.null(USGS_GAGE)){

##  Get observed data from the USGS site
	obs <- get_usgs_gage(USGS_GAGE, "2006-10-01", as.character(today))
	ObsFlow <- ConvFlow(cmd=obs$flowdata$flow, WA=obs$area)
	obsFill <- FillMissing(obs$flowdata$mdate,ObsFlow)

## Currently we are going to look at 90 days of output, can make this more or less
	Aobs <- max(which(obsFill$Date==(today-90)), 1, na.rm=TRUE)
	Bobs <- nrow(obsFill)
	Amod <- max(which(PrevRun$Date==(today-90)), 1, na.rm=TRUE)
	Bmod <- nrow(PrevRun)

## Print graph:
	if (COMPUTER == "server" ){ 
	pdf(file=paste("/var/www/", WSH,"/ModelResults.pdf", sep=""), width=12, height=6)
	} else 	pdf(file="ModelResults.pdf", width=12, height=6)
	# postscript("ModelResults.eps",width=12, height=6)
	plot(c(PrevRun$Date[Amod:Bmod],seq(today+1, today+DAYS_PREDICTION, by=1)), c(PrevRun$modeled_flow[Amod:Bmod],TodaysMR$modeled_flow[-1]), col="red", type="l", ylim=c(0, max(ObsFlow[Aobs:Bobs],c(PrevRun$modeled_flow[Amod:Bmod],TodaysMR$modeled_flow[-1]), na.rm=TRUE)), ylab="Flow depth (mm/d)", xlab="Date", lwd=2, lty=2)
	points(obsFill$Date[Aobs:Bobs], obsFill$x[Aobs:Bobs], col="black", ylab="Flow depth (mm/d)", xlab="Date")
	lines(PrevRun$Date[Amod:Bmod], PrevRun$modeled_flow[Amod:Bmod], col="blue", lwd=3)
	legend("topleft", legend = c("USGS gage", "Modeled Flow", "Predicted"), lwd=c(NA,2.5,2), lty=c(NA,1,2),pch=c(1,NA,NA), col=c("black", "blue", "red"))
	dev.off()
}

