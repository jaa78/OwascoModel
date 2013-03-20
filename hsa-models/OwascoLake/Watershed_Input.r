###################################################################################################################################
##  Owasco Lake 
###  INPUT (change for each watershed)
WSH <- "OwascoLake"  #will be the folder name for this watershed

STATE <- "NY" 		
	#  In what state/s is the watershed?  If multiple, separate with "|" 	e.g. "NY | PA"
	
COOPstationsBEST <-  c("Auburn", "Freeville", "Locke", "Moravia", "Lansing 6.6 NW") 
	# The names of the stations that are in or close to the watershed 
	#  The precip values from all of these that report on a particular day will be averaged.
	# Advantage to being specific in the name (e.g. Lansing 6.6 NW ) 
		#	- This will make sure you are not picking up a station in a distant town with the same name
	# Advantage to being general in the name (e.g. Ithaca)
		#	- There could be multiple stations within one town, so if one doesn't report, you can still get the other.
		#	- CAUTION: Use this only if there is no other town with the same name in your specified state
	##  To find Coop Stations near/in watershed:
		#	- Here is a useful map: 	http://data.cocorahs.org/cocorahs/maps/?country=USA
		# 	- Or check this website: 	http://www.nws.noaa.gov/data/TAR/HYDTAR
		
COOPstationsBACKUP <- c("Syracuse", "Cortland","Ithaca")  
	# Backup co-op stations in case best stations all don't report

NOAA_STATIONS = c("KITH", "KSYR")
	# 	These are the NOAA stations where we will get recorded max/min temp data, and predicted values.  
	#	If UseCoop = FALSE, we will also get past precip values from these NOAA stations
	#  	Note, these update frequently (every 1-6 hours), but have fewer locations
	##  This website can help find NOAA stations: http://weather.uwyo.edu/models/mos/mos.html

P_cutoff <- 30 
	#	Don't count predicted precip in model if probability of precip is less than this (%)

DAYS_PREDICTION <- 3
	#  	How many days in the future do we want this to predict until
	#	NOTE:  Might want the web-site side of things to link to this value too
		#	- JSN table
		# 	- Prediction table size
		
LAT_DEG <- 42.66			

SOIL_DEPTH <- 2010			#  [mm]

POROSITY 	<- 0.27			# [decimal %]

AWC_PERCENTAGE	<-	0.13	# [decimal %]

TIME_PEAK		<-	0.35*6.5 + 3.5		#  [hr] Time to peak ( 0.35*Tc+3.5 )

WETNESS_CLASSES	<- 10

IMPERVIOUS_AREA <- 0		# [0-100 %]

opt 	<- "PrevRun"		#  This will only run the model for today and the future predictions 

USGS_GAGE	<-  "04235299"		
	# USGS gage number (as text, include leading 0)
	# If no gage, can put a nearby one for comparason, or NULL

PRINT_TABLES <- FALSE		# 	If you need quality control to check the input data, change to TRUE
		
#####################################################################################################################################