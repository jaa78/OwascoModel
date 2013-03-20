##  		Master file - will run all watersheds at 10:30 am

setwd("/var/opt/hsa-models.R")

library(EcoHydRology)
# library(gplots)

source("Functions/DataFunctions.r")
source("Functions/PriestleyTaylorEquations.r")  
# 	also includes SnowMelt - different from function in EcoHydRology
# 	updated in March 2013 - includes Energy, Albedo, and Snow Temp in input and output
source("Functions/StreamFN_UNCALIBRATED.r")					# UPDATE?   StreamFN_UNCALIBRATED.r
source("Functions/NashSutcliffe.r")				
source("Functions/GetPredictions.r")  	# Gets NOAA predictions.  	fn = forcastsNOAA()  	Updated March 2013
source("Functions/GetCoopNOAA_Data.r")  # Gets observed data.	  	fn = Past24hrData()		Updated March 2013


Hour <- as.POSIXlt(Sys.time())$h
UseCoop <- ifelse(Hour >= 9,  TRUE, FALSE)
	# Put FALSE here when collecting data before 9:30 am

WATERSHEDS <- c(
			"OwascoLake"
			,"SalmonCreek"
			,"FallCreek"
			)
			
COMPUTER 	<- "server"			# COMPUTER	<- "Jo's"  		# Alows for testing on Jo's computer	

for (b in WATERSHEDS) {
	if (Hour >= 9) cat(" 10:30 AM Run\n") else cat(" 3:00 AM Run\n")
	print(as.Date(Sys.time()))
	print(b)
	setwd(b)
	if (Hour >= 9){
		source("../WatershedModel1030.r")
	} else source("../WatershedModel_3am.r")
	setwd("../")
}

	
##  To add a new watershed:
#	1.	 Add new watershed name to the list WATERSHEDS 
#	2.	Create a folder tree for that watershed in hsa-models (copy file structure from 	
		#  FileStructureForNewWatershed )
#	3. 	Create folder in www
#	4.	Initialize model (can use the "InitiateNewWatershedModel.R, change for new watershed)
# These need to be initialized.
# In addition to copying the file structure into a new folder with the new watershed's name, 
# also run the model once to get the InputToModel.RData file, and the PrevRun.RData file







