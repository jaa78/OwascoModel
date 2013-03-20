# M.Todd Walter, Cornell University, 2010
# Priestly-Taylor caclulator
# This code follows (approximately): Walter et al. 2005 Journal of Hydrology 300(1-4): 65-75.
#################################################################################################
##	Returns a vector of PET values calculated by the Priestly-Taylor Method, in meters
#	Revised December 2011

####################################################################################################
##		Environmental Energy Functions
####################################################################################################
####### FUNCTIONS FOR CALCULATING SOLAR RADIATION #######

declination<-function(Jday){
# solar declination [rad]
#Jday: Julian date or day of the year [day]
return(0.4102*sin(pi*(Jday-80)/180))
}

solarangle<-function(lat,Jday){
# angle of solar inclination from horizontal at solar noon [rad]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	# solar declination [rad]
dec<-declination(Jday)
return(asin(sin(lat)*sin(dec)+cos(lat)*cos(dec)*cos(0)))
}

solaraspect<-function(lat,Jday){
# aspect angle of sun from north at solar noon [rad]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
return(acos((sin(0)*sin(lat) - sin(declination(Jday)))/(cos(0)*cos(lat))))
}

PotentialSolar<-function(lat,Jday){
# potential solar radiation at the edge of the atmosphere [kJ m-2 d-1]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	# solar declination [rad]
dec<-declination(Jday)
return(117500*(acos(-tan(dec)*tan(lat))*sin(lat)*sin(dec)+cos(lat)*cos(dec)*sin(acos(tan(dec)*tan(lat))))/pi)
}

transmissivity<-function(Tx,Tn, avDeltaT=NULL){ 	
# fraction of direct solar radiation passing through 
# the atmosphere based on the Bristow-Campbell eqn
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
len<-length(Tx)
if (is.null(avDeltaT )){
	if(len<30){ 
		avDeltaT<-mean(Tx-Tn)
	}else {
		avDeltaT<-vector(length=len)
		avDeltaT[1:14]<-mean(Tx[1:30]-Tn[1:30])
		avDeltaT[(len-14):len]<-mean(Tx[(len-30):len]-Tn[(len-30):len])
		for (i in 15:(len-15)){
			avDeltaT[i]<-mean(Tx[(i-14):(i+15)]-Tn[(i-14):(i+15)])
		}
	}
}
B<-0.036*exp(-0.154*avDeltaT)
return(0.75*(1-exp(-B*(Tx-Tn)^2.4)))
}

slopefactor<-function(lat,Jday,slope,aspect){
# slopefactor: adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	#slope: slope of the ground [rad]
	#aspect: ground aspect [rad from north]
return(cos(slope)-sin(slope)*1/tan(solarangle(lat,Jday))*cos(aspect-solaraspect(lat,Jday)))
}


Solar<-function(lat,Jday,Tx,Tn,albedo,forest,slope,aspect){
# solar radiation at the ground surface [kJ m-2 d-1]
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
	#albedo: surface albedo or reflectivity [-]
	#forest: forest or vegeation cover [-]
	#slope: slope of the ground [rad]
	#aspect: ground aspect [rad from north]
return((1-albedo)*(1-forest)*transmissivity(Tx,Tn)*PotentialSolar(lat,Jday)*slopefactor(lat,Jday,slope,aspect))
}



####### FUNCTIONS FOR CALCULATING LONGWAVE RADIATION #######

AtmosphericEmissivity<-function(airtemp,cloudiness){
# the emissivity of the atmsophere [-]
	#airtemp: air temperature [C]
	#cloudiness: fraction of the sky covered in clouds [-]
return((0.72+0.005*airtemp)*(1-0.84*cloudiness)+0.84*cloudiness)
}


EstCloudiness<-function(Tx,Tn, avDeltaT=NULL){
# estimates the cloudiness of the atmosphere by scaling to atmospheric transmissivity
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
trans<-transmissivity(Tx,Tn, avDeltaT)
cloudiness<-1-(trans-0.15)/(0.75-0.15)
cloudiness[which(cloudiness>1)]<-1
cloudiness[which(cloudiness<0)]<-0
return(cloudiness)		
}


Longwave<-function(emissivity,temp){
# daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]
	#emissivity: [-]
	#temp: temperature of the emitting body [C]
SBconstant<-0.00000489 #[kJ m-2 K-4 d-1]
tempK<-temp+273.3 #[degrees K]
return(emissivity*SBconstant*tempK^4)
}


####### FUNCTION FOR CALCULATING NET RADIATION #######

NetRad<-function(lat, Jday, Tx, Tn, albedo, forest, slope, aspect, airtemp, cloudiness, surfemissivity, surftemp){
# daily net radiation [kJ m-2 d-1]
return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp))
}


####### FUNCTION FOR CALCULATING SENSIBLE HEAT EXCHANGE #######

SensibleHeat<-function(surftemp,airtemp,wind){
# sensible heat exchange between a surface and the surrounding air [kJ m-2 d-1]
	#surftemp: surface temperature [C]
	#airtemp: average dailiy air temperature [C]
	#wind: average daily windspeed [m/s]
latentht<-2500 #latent heat of vaporization [kJ kg-1]
heatcapacity<-1.25 #approx. heat capacity of air [kJ m-3 C-1]
windfunction<-5.3*(1+wind)
return(86400*heatcapacity*(surftemp-airtemp)*windfunction/latentht)
}


####### FUNCTIONS FOR CALCULATING EVAPORATIVE HEAT EXCHANGE #######

SatVaporDensity<-function(temp){
# saturated vapor density at a given temperature
	#temp: temperature [C]
return(exp((16.78*temp-116.9)/(temp+273.3))*1/((273.15+temp)*0.4615))
}


EvapHeat<-function(surftemp,airtemp,relativehumidity,Tn,wind){
# evaporative heat exchange between a surface and the surrounding air; usually cooling [kJ m-2 d-1]
# this function is only intended for wet surfaces, i.e., it assumes the vapor density at the surface is the saturation vapor density
	#surftemp: surface temperature [C]
	#airtemp: average dailiy air temperature [C]
	#relativehumidity: relative humidity [-]
	#Tn: minimum dailiy air temperature, assumed to be the dewpoint temperature [C]
	#wind: average daily windspeed [m/s]
#NOTE: this function will use the relative humidity to esimate air vapor density if the value passed is greater than zero (0)
# If the relative humidity is less than one, we will assume the minimum daily air temperature is approsimate the dew point temp.
windfunction<-5.3*(1+wind)
if(relativehumidity>0){airvapordensity<-relativehumidity*SatVaporDensity(airtemp)}
else{airvapordensity<-SatVaporDensity(Tn)}
surfacevapordensity<-SatVaporDensity(surftemp)
return(86400*windfunction*(surfacevapordensity-airvapordensity))
}



####### FUNCTION FOR CALCULATING HEAT FROM RAIN #######

RainHeat<-function(airtemp,rain){
# temperature added to the land from heat exchange with rain (usually in the context of snowmelt) [kJ m-2 d-1]
	#airtemp: average dailiy air temperature [C]
	#rain: depth of rainfall [m]
heatcapacity<-4.2  #heat capacity of water [kJ kg-1 C-1]
waterdensity<-1000 #density of water [kg m-3]
return(heatcapacity*waterdensity*rain*(airtemp-0))
}



####### FUNCTION FOR CALCULATING HEAT FROM GROUND CONDUCTION #######

GroundHeat<-function(){
# the heat conducted to the bottom of a snowpack, assumed constant [kJ m-2 d-1]
return(173)
}



####### FUNCTIONS FOR CALCULATING ENERGY INPUTS #######

EnvirEnergy<-function(lat,Jday,Tx,Tn,wind,relativehumidity,cloudiness,albedo,forest,slope,aspect,surftemp,surfemissivity){
# the total energy exchange between the surface and the surrounding air
	#lat: latitdue [rad]
	#Jday: Julian date or day of the year [day]
	#Tx: maximum daily temperature [C]
	#Tn: minimum daily temperature [C]
	#wind: average daily windspeed [m/s]
	#relativehumidity: relative humidity; if negative, air vapor density will be approximated [-]
	#cloudiness: fraction of the sky covered in clouds,if negative, cloudiness will be approximated [-]
	#albedo: surface albedo or reflectivity [-]
	#forest: forest or vegeation cover [-]
	#slope: slope of the ground [rad]
	#aspect: ground aspect [rad from north]
	#surftemp: surface temperature [C]
	#surfemissivity: [-]
if(cloudiness<0){cloudiness<-EstCloudiness(Tx,Tn)}
airtemp<-(Tx+Tn)/2 #average daily air temperature [C]
return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp)+SensibleHeat(surftemp,airtemp,wind)+EvapHeat(surftemp,airtemp,relativehumidity,Tn,wind)+RainHeat(airtemp,rain)+GroundHeat())
}


####### Psychrometric Chart Slope (delta) #######
#Delta: slope of the saturation vapor pressure vs. T curve [kPa/K]
SatVapPresSlope<-function(temp_C){
(2508.3/(temp_C+237.3)^2)*exp(17.3*temp_C/(temp_C+237.3))
}


PETfromTemp<-function(Jday, Tmax_C, Tmin_C, lat_radians, AvgT=(Tmax_C+Tmin_C)/2, albedo=0.18, TerrestEmiss=0.97, aspect=0, slope=0, forest=0){
	##	forest = Forest cover, but will always be left at zero for watershed-wide processes.  Only need to put
	## 	actual percent forest when calculating eg. PET under canopy.
#	lat: in radians (degLat*pi/180)
#	albedo can be a vector or single value

if (length(Jday)!=length(Tmax_C) | length(Jday)!=length(Tmin_C)){ 
	cat("Warning, input vectors unequal length: Longer data sets truncated.\n")
	length(Jday)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
	length(Tmax_C)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
	length(Tmin_C)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
	}

cloudiness<-EstCloudiness(Tmax_C,Tmin_C)
DailyRad<-NetRad(lat_radians,Jday,Tmax_C,Tmin_C,albedo,forest,slope,aspect,AvgT,cloudiness,TerrestEmiss,AvgT)

#####	Constants
PTconstant<-1.26 		# 	[-] Generic Priestly-Taylor constant
LatentHtEvap<-2500 		# 	[kJ/kg]
DensityWater<-1000 		# 	[kg/m3]
PsychConstant<-0.066	#	[kPa/K]

potentialET<-PTconstant*SatVapPresSlope(AvgT)*DailyRad/((SatVapPresSlope(AvgT)+PsychConstant)*(LatentHtEvap*DensityWater))
potentialET[which(potentialET<0)]<-0
potentialET[which(Tmax_C==-999 | Tmin_C==-999)]<-(-999)
return(potentialET)
}


##  SNOW MELT FUNCTION:  (Updated version from Feb 2013 - Steve Pacenka found some bugs).  This version is _2013_02_05
SnowMelt <- function(Date, precip_mm, Tmax_C, Tmin_C, lat_deg, slope=0, aspect=0, tempHt=1, windHt=2, groundAlbedo=0.25,  SurfEmissiv=0.95, windSp=2, forest=0, startingSnowDepth_m=0, startingSnowDensity_kg_m3=450, YestSnowTemp=0, YestAlbedo=NULL, SnowMeltYest=0, YestEnergy=NULL, Av30dayDeltaT=NULL){	
## Constants :
	WaterDens <- 1000			# kg/m3
	lambda <- 3.35*10^5			# latent heat of fusion (kJ/m3)
	lambdaV <- 2500				# (kJ/kg) latent heat of vaporization
	SnowHeatCap <- 2.1			#kJ/kg/C
	LatHeatFreez <- 333.3		# kJ/kg
	Cw <- 4.2*10^3				# Heat Capacity of Water (kJ/m3/C)
	
##	Converted Inputs :
	Tav <- (Tmax_C+Tmin_C)/2		# degrees C
	precip_m <- precip_mm*0.001	 	# precip in m 
	R_m <- precip_m					# (m) depth of rain
	R_m[which(Tav < 0)] <- 0		# ASSUMES ALL SNOW at < 0C
	NewSnowDensity <- 50+3.4*(Tav+15)		# kg/m3
	NewSnowDensity[which(NewSnowDensity < 50)] <- 50
	NewSnowWatEq <- precip_m				# m
	NewSnowWatEq[which(Tav >= 0)] <- 0			# Now new snow if average temp above or equals 0 C
	NewSnow <- NewSnowWatEq*WaterDens/NewSnowDensity		# m
	JDay <- strptime(Date, format="%Y-%m-%d")$yday+1
	lat <- lat_deg*pi/180		#	latitude in radians
	rh 	<- log((windHt+0.001)/0.001)*log((tempHt+0.0002)/0.0002)/(0.41*0.41*windSp*86400)	# (day/m) Thermal Resistance	 
	if (length(windSp)==1) rh <- rep(rh,length(precip_mm))									##	creates a vector of rh values
	cloudiness 		<- EstCloudiness(Tmax_C,Tmin_C, Av30dayDeltaT)
	AE 				<- AtmosphericEmissivity(Tav, cloudiness)	# (-) Atmospheric Emissivity

#  New Variables	:
	SnowTemp 		<- rep(0,length(precip_m)) 		# Degrees C
	rhos 			<- SatVaporDensity(SnowTemp)	# 	vapor density at surface (kg/m3)
	rhoa 			<- SatVaporDensity(Tmin_C)		#	vapor density of atmoshpere (kg/m3) 
	SnowWaterEq 	<- vector(length=length(precip_mm))		#  (m) Equiv depth of water
	TE 				<- rep(SurfEmissiv,length(precip_mm))	#	(-) Terrestrial Emissivity
	DCoef 			<- rep(0,length(precip_mm))				#   Density Coefficient (-) (Simplified version)
	SnowDensity 	<- rep(450,length(precip_mm))			#  (kg/m3)  Max density is 450
	SnowDepth 		<- vector(length=length(precip_mm))		#  (m)
	SnowMelt 		<- rep(0,length(precip_mm))				#  (m)
	Albedo 			<- rep(groundAlbedo,length(precip_mm)) 	#  (-) This will change for days with snow
	
##	Energy Terms
	H 		<- vector(length=length(precip_mm))	#	Sensible Heat exchanged (kJ/m2/d)
	E 		<- vector(length=length(precip_mm))	#	Vapor Energy	(kJ/m2/d)
	S 		<- vector(length=length(precip_mm))	#	Solar Radiation (kJ/m2/d)
	La 		<- Longwave(AE, Tav)					#	Atmospheric Longwave Radiation (kJ/m2/d)
	Lt 		<- vector(length=length(precip_mm))	#	Terrestrial Longwave Radiation (kJ/m2/d)
	G 		<- 173								#	Ground Condution (kJ/m2/d) 
	P 		<- Cw * R_m * Tav					# 	Precipitation Heat (kJ/m2/d)
	Energy 	<- vector(length=length(precip_mm))	# Net Energy (kJ/m2/d)

##  Initial Values.  
	SnowWaterEqYest	<- startingSnowDepth_m * startingSnowDensity_kg_m3 / WaterDens		
	SnowDepth[1] 	<- startingSnowDepth_m	
	#  Initial Snow temp 
	if(startingSnowDepth_m > 0 | NewSnow[1] > 0) {
			DCoef[1] <- 6.2
			if(SnowMeltYest == 0 & !is.null(YestEnergy)){ 
				SnowTemp[1] <- max(min(0,Tmin_C[1]),min(0,(YestSnowTemp+min(-YestSnowTemp,YestEnergy/((startingSnowDensity_kg_m3*	startingSnowDepth_m+NewSnow[1]*NewSnowDensity[1])*SnowHeatCap*1000)))))
		}
	}
	# Initial Albedo 
	if (is.null(YestAlbedo)){
		Albedo[1] <-  ifelse(NewSnow[1] > 0, 0.98-(0.98-0.50)*exp(-4*NewSnow[1]*10),ifelse(startingSnowDepth_m == 0, groundAlbedo, max(groundAlbedo, 0.5+(groundAlbedo-0.85)/10)))  # If snow on the ground or new snow and YestAlbedo not specified, assume Albedo yesterday was 0.5
		S[1] <- Solar(lat=lat,Jday=JDay[1], Tx=Tmax_C[1], Tn=Tmin_C[1], albedo=Albedo[1], forest=forest, aspect=aspect, slope=slope)
	} else {
		if (NewSnow[1] > 0){ 
			Albedo[1] <- 0.98-(0.98-YestAlbedo)*exp(-4*NewSnow[1]*10)
		} else if (startingSnowDepth_m < 0.1){ 
			Albedo[1] <- max(groundAlbedo, YestAlbedo+(groundAlbedo-0.85)/10)
		} else Albedo[1] <- 0.35-(0.35-0.98)*exp(-1*(0.177+(log((-0.3+0.98)/(YestAlbedo-0.3)))^2.16)^0.46)
		S[1] <- Solar(lat=lat,Jday=JDay[1], Tx=Tmax_C[1], Tn=Tmin_C[1], albedo=YestAlbedo, forest=forest, aspect=aspect, slope=slope)
	}
	
	if(startingSnowDepth_m > 0) TE[1] <- 0.97 
	rhos[1] <- SatVaporDensity(SnowTemp[1])
	H[1] <- 1.29*(Tav[1]-SnowTemp[1])/rh[1] 
	E[1] <- lambdaV*(rhoa[1]-rhos[1])/rh[1]
	Lt[1] <- Longwave(TE[1],SnowTemp[1])
	Energy[1] <- S[1] + La[1] - Lt[1] + H[1] + E[1] + G + P[1]
	
	if (Energy[1]>0) k <- 2 else k <- 1
	SnowDensity[1] <- 	ifelse((startingSnowDepth_m+NewSnow[1])>0, min(450, 
		((startingSnowDensity_kg_m3+k*30*(450-startingSnowDensity_kg_m3)*exp(-DCoef[1]))*startingSnowDepth_m + NewSnowDensity[1]*NewSnow[1])/(startingSnowDepth_m+NewSnow[1])), 450)	
	
	SnowMelt[1] <- max(0,	min((SnowWaterEqYest+NewSnowWatEq[1]),  # yesterday on ground + today new  
				      (Energy[1]-SnowHeatCap*(SnowWaterEqYest+NewSnowWatEq[1])*WaterDens*(0-SnowTemp[1]))/(LatHeatFreez*WaterDens)		  ) )
	SnowDepth[1] <- max(0,(SnowWaterEqYest + NewSnowWatEq[1]-SnowMelt[1])*WaterDens/SnowDensity[1])  #CHANGED HERE 3/17/2013
	SnowWaterEq[1] <- max(0,startingSnowDepth_m* startingSnowDensity_kg_m3 / WaterDens-SnowMelt[1]+NewSnowWatEq[1])	

##  Snow Melt Loop	
	for (i in 2:length(precip_m)){
		if (NewSnow[i] > 0){ 
			Albedo[i] <- 0.98-(0.98-Albedo[i-1])*exp(-4*NewSnow[i]*10)
		} else if (SnowDepth[i-1] < 0.1){ 
			Albedo[i] <- max(groundAlbedo, Albedo[i-1]+(groundAlbedo-0.85)/10)
		} else Albedo[i] <- 0.35-(0.35-0.98)*exp(-1*(0.177+(log((-0.3+0.98)/(Albedo[i-1]-0.3)))^2.16)^0.46)

		S[i] <- Solar(lat=lat,Jday=JDay[i], Tx=Tmax_C[i], Tn=Tmin_C[i], albedo=Albedo[i-1], forest=forest, aspect=aspect, slope=slope)

		if(SnowDepth[i-1] > 0) TE[i] <- 0.97 	#	(-) Terrestrial Emissivity
		if(SnowWaterEq[i-1] > 0 | NewSnowWatEq[i] > 0) {
			DCoef[i] <- 6.2
			if(SnowMelt[i-1] == 0){ 
				SnowTemp[i] <- max(min(0,Tmin_C[i]),min(0,(SnowTemp[i-1]+min(-SnowTemp[i-1],Energy[i-1]/((SnowDensity[i-1]*	SnowDepth[i-1]+NewSnow[i]*NewSnowDensity[i])*SnowHeatCap*1000)))))
			}
		}

		rhos[i] <- SatVaporDensity(SnowTemp[i])
		H[i] <- 1.29*(Tav[i]-SnowTemp[i])/rh[i] 
		E[i] <- lambdaV*(rhoa[i]-rhos[i])/rh[i]
		Lt[i] <- Longwave(TE[i],SnowTemp[i])
		Energy[i] <- S[i] + La[i] - Lt[i] + H[i] + E[i] + G + P[i]

		if (Energy[i]>0) k <- 2 else k <- 1
		
		SnowDensity[i] <- ifelse((SnowDepth[i-1]+NewSnow[i])>0, min(450, 
			((SnowDensity[i-1]+k*30*(450-SnowDensity[i-1])*exp(-DCoef[i]))*SnowDepth[i-1] + NewSnowDensity[i]*NewSnow[i])/(SnowDepth[i-1]+NewSnow[i])), 450)

		SnowMelt[i] <- max(0,	min( (SnowWaterEq[i-1]+NewSnowWatEq[i]),  # yesterday on ground + today new
				      (Energy[i]-SnowHeatCap*(SnowWaterEq[i-1]+NewSnowWatEq[i])*WaterDens*(0-SnowTemp[i]))/(LatHeatFreez*WaterDens) )  )

		SnowDepth[i] <- max(0,(SnowWaterEq[i-1]+NewSnowWatEq[i]-SnowMelt[i])*WaterDens/SnowDensity[i])
		SnowWaterEq[i] <- max(0,SnowWaterEq[i-1]-SnowMelt[i]+NewSnowWatEq[i])	# (m) Equiv depth of water
	}
	
	Results<-data.frame(as.Date(as.character(Date)), Tmax_C, Tmin_C, precip_mm, R_m*1000, NewSnowWatEq*1000,SnowMelt*1000, NewSnow, SnowDepth, SnowWaterEq*1000, SnowTemp, Albedo, Energy, S, La, Lt, H, E, AE, cloudiness)
	colnames(Results)<-c("Date", "MaxT_C", "MinT_C", "Precip_mm", "Rain_mm", "SnowfallWatEq_mm", "SnowMelt_mm", "NewSnow_m", "SnowDepth_m", "SnowWaterEq_mm", "SnowTemp", "Albedo", "NetEnergy", "S", "La", "Lt", "H", "E", "AE", "cloudiness")
	return(Results)
}	







