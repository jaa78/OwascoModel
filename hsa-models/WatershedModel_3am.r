##		Watershed Model  
# 		March 2013 version
#		This version will only look at NOAA stations 
#  USE before 9:30 am, when co-op data is not available

##  Get watershed run input:
source("Watershed_Input.R")  # watershed-specific info

###################################################################################################################################

load("InputOutput/PrevRun.RData")  ##  Gives model output from earlier (data frame: PrevRun)
load("InputOutput/InputToModel.RData")  
	##  This gives us the input values from yesterday, we will add to them today. 
	##	(vectors: DS, Pmm, TxC, TnC, data frame: PrevSM)

PastDay <- Past24hrData(state=STATE, CoopStns = COOPstationsBEST, CoopBkup = COOPstationsBACKUP, NOAAstn = NOAA_STATIONS, UseCoopPrecip = UseCoop, PrintDataTables = PRINT_TABLES)	  # If you don't need quality control to check the input data, change to PrintDataTables = FALSE

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
write.csv(PrevRun, file="Files3am/OwascoModelResultsNew.csv")	# Backup save
save(list=c("PrevRun"), file="Files3am/PrevRun.RData")			# Needed for tomorrow's run

##  Backup of input
PrevSM <- rbind(PrevSM[1:which(PrevSM$Date==yesterday),], SM[which(SM$Date == today),])
save(list=c("DS", "Pmm", "TnC", "TxC", "PrevSM"), file="Files3am/InputToModel.RData")
input <-data.frame(DS,Pmm,TxC,TnC, PrevSM)
write.csv(input, file="Files3am/Input.csv")  # Back up of input data

##  Record Predictions and Observations in one file
ID <- c(as.character(today),PastDay$P_mm_NOAA, PastDay$P_mm_coop, PastDay$CoopStnsReporting, Pred_P, PastDay$Tx, Pred_Tx, PastDay$Tn, Pred_Tn, Pred_Prob)
write(ID, file="InputOutput/DataTables/WeatherData3am.csv", append=TRUE, ncolumns=26, sep=",")



##   To print out a graph of predicted and obs flow  
## 		Need to fix get_usgs_gage function
# library(EcoHydRology)  #  But we use a variation of the transmissivity, EstCloudiness, SnowMelt functions
# plot(c(PrevRun$Date[(nr-30):nr],seq(today+1, today+DAYS_PREDICTION, by=1)), c(PrevRun$modeled_flow[(nr-30):nr],TodaysMR$modeled_flow[-1]), type='l', xlab="Date", ylab="flow depth (mm)", col="red")

# obs <- get_usgs_gage(flowgage_id="04235299", begin_date = DS[nr-30], end_date = yesterday)



