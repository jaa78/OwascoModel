##		Initiating a model in the Ithaca area
##		Watershed : Salmon Creek 
library(EcoHydRology)
source("Functions/GetIthacaData.r")
source("Functions/PriestleyTaylorEquations.r")  
# 	also includes SnowMelt - different from function in EcoHydRology
# 	updated in March 2013 - includes Energy, Albedo, and Snow Temp in input and output
source("Functions/StreamFN_UNCALIBRATED.r")					# UPDATE?   StreamFN_UNCALIBRATED.r
source("Functions/NashSutcliffe.r")		

IthData <- GetIthacaData(startMonth=10, startYear=2006, EndDate=strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"))	# Get at least 10 days of data to initiate the model.  

IthData <- IthData[	1:which(IthData$Date==today) ,]	
setwd("FallCreek")  # Set to the watershed folder
source("Watershed_Input.R")
today <- as.Date(Sys.time())

#  Snow Melt calculation
SM <- SnowMelt(Date=IthData$Date, precip_mm=IthData$P_mm, Tmax_C=IthData$Tmax_C, Tmin_C=IthData$Tmin_C, lat_deg=LAT_DEG, startingSnowDepth_m = IthData$snowDepth_mm[1]/1000)
		
rsm <- SM$SnowMelt_mm + SM$Rain_mm  ## new input to stream function


##  Model Run
TodaysMR	<-	StreamflowFN(
		Depth				= SOIL_DEPTH, 		# Average wshd soil depth (mm) 
		SATper				= POROSITY , 	# Porosity (fraction)	
		AWCper				= AWC_PERCENTAGE, 	# Available Water Capacity (AWC) (fraction) 
		latitudeDegrees		= LAT_DEG, 
		P					= rsm, 					# Rain + Snow melt (mm)
		dateSeries			= IthData$Date, 		# Date series (format should be: "2003-02-36")
		Tmax				= IthData$Tmax_C, 				# Max daily temperature (C)
		Tmin				= IthData$Tmin_C, 				# Max daily temperature (C)
		BF1					= 1, 		#  Assume initial baseflow is 1 mm/day
		percentImpervious	= 0, 		# Percent of the watershed that is impervious
		no_wet_class		= 10, 		# The number of wetness classes for saturated area designation
		Tp 					= TIME_PEAK, 		# Time to peak (hours [0.35*Tc+3.5])
		StartCond 			= "avg"		# Watershed conditions before first day of run ("wet", "dry", "avg")
)


##  Now save these files to be used as inputs in the future:
# 	Save Model Run
PrevRun <- TodaysMR
save(list=c("PrevRun"), file="InputOutput/PrevRun.RData")			# Needed for tomorrow's run

##  Backup of input
PrevSM <- SM
DS <- IthData$Date
Pmm <- IthData$P_mm
TnC <- IthData$Tmin_C
TxC <- IthData$Tmax_C
save(list=c("DS", "Pmm", "TnC", "TxC", "PrevSM"), file="InputOutput/InputToModel.RData")


##  Check with USGS gage if observations exist for this watershed
obs <- get_usgs_gage("04234000", "2006-10-01", "2013-03-20")
ObsFlow <- ConvFlow(cmd=obs$flowdata$flow, WA=obs$area)

Aobs <- max(which(obs$flowdata$mdate==(today-365)), 1, na.rm=TRUE)
Bobs <- nrow(obs$flowdata)
Amod <- max(which(PrevRun$Date==(today-365)), 1, na.rm=TRUE)
Bmod <- nrow(PrevRun)


## One year before:
plot(obs$flowdata$mdate[Aobs:Bobs], ObsFlow[Aobs:Bobs], col="blue", type='l', ylab="Flow (mm/d)", xlab="Date", lwd=2)
lines(TodaysMR$Date[Amod:Bmod], TodaysMR$modeled_flow[Amod:Bmod], col="red", lwd=2)
legend("topleft", legend = c("Observed Flow", "Modeled Flow"), lwd=2, col=c("blue", "red"))

NashSutcliffe(ObsFlow[1:2000],TodaysMR$modeled_flow[1:2000])


##  This plots 30 days before, no predictions
plot(obs$flowdata$mdate[(nrow(obs$flowdata)-30):nrow(obs$flowdata)], ConvFlow(cmd=obs$flowdata$flow[(nrow(obs$flowdata)-30):nrow(obs$flowdata)], WA=obs$area), col="blue", type='l', ylab="Flow (mm/d)", xlab="Date", lwd=2)
lines(TodaysMR$Date[(nrow(TodaysMR)-30):nrow(TodaysMR)], TodaysMR$modeled_flow[(nrow(TodaysMR)-30):nrow(TodaysMR)], col="red", lwd=2)
legend("topleft", legend = c("Observed Flow", "Modeled Flow"), lwd=2, col=c("blue", "red"))


