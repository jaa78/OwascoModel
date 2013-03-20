####################################################################################################################
#########	This function uses Thornthwaite-Mather Water Budget and SCS-Curve Number to approximate streamflow
####################################################################################################################
##	modified Streamflow Function
##		this uses the idea that runoff on the first day acts as overland runoff
##		everything on subsequent days is shallow interflow
##
#####################################################

depth<-500 #mm
SaturationPercent<-0.5
FieldCapacityPercent<-0.4
WiltingPointPercent<-0.05
AWCinput<-depth*(FieldCapacityPercent-WiltingPointPercent)
Porosity<-depth*(SaturationPercent-WiltingPointPercent)
no_wet_class <-	10

StreamflowFN<-function(rec_coef=0.08,Se_avg=140, Ia_coef=0.05, AWC=AWCinput, SAT=Porosity, latitudeDegrees=42.38, albedo=0.2, aspect=0, P, dateSeries, Tmax, Tmin, shift=1, SW1=NULL, BF1=1, PETcap=5, percentImpervious=0,SAT_coef=0.3, no_wet_class=10, PreviousOutput=NULL){
# Parameters
	latitude<-latitudeDegrees*pi/180			## latitude in radians	

## Effective soil water storage coefficients, eswsc = Se*(2..... see Schneiderman et al 2007)
	eswsc<-vector(mode="numeric", length=no_wet_class)
	eqn16<-function(x){(2*(sqrt(1-(1/no_wet_class)*(x-1))-sqrt(1-(1/no_wet_class)*x))/(1/no_wet_class))-1}
	x<-seq(1,no_wet_class, by=1)
	eswsc<-eqn16(x)


### This is to allow us to use previous model output as an input - useful for calculating just
### one more day of modeled flow..
	if (!is.null(PreviousOutput)){
		nr<-nrow(PreviousOutput)
		dateSeries<-c(PreviousOutput$dateSeries[(nr-1):nr],dateSeries)
		P<-c(PreviousOutput$rain_snowmelt[(nr-1):nr], P)
		Tmax<-c(PreviousOutput$Tmax[(nr-1):nr], Tmax)
		Tmin<-c(PreviousOutput$Tmin[(nr-1):nr], Tmin)
		OutStart<-3
	} else OutStart<-1
################################################
	Tav<-(Tmax+Tmin)/2
	day<-strptime(dateSeries,"%Y-%m-%d")$yday+1
	month<-strptime(dateSeries,"%Y-%m-%d")$mon+1

	PET<-PETfromTemp(Jday=day,Tmax_C=Tmax,Tmin_C=Tmin,AvgT=Tav,albedo=albedo,lat=latitude)*1000	## mm
	PET[which(PET>PETcap)]<-PETcap	##	Sets a cap on PET estimates (usually ~ 5mm)
	ET<-PET
	ET[which(day<=166)]<-(0.1+0.02*(day[which(day<=166)]-121))*PET[which(day<=166)]		##linear increase from May 1- June 15
	ET[which(month<5)]<-0.1*PET[which(month<5)]											## until May 1, ET is only 10% of PET
	ET[which(day>=274)]<-(1-0.02*(day[which(day>=274)]-274))
	ET[which(day>319)]<-0.1*PET[which(day>319)]
	
# Set up vectors, values to be defined in a loop	
	deltaP<-P-ET													## (mm) neg values indicate net evap
	SoilWater<-vector(length=length(P))								##	(mm)
	excess<-vector(length=length(P))
	TM_S<-vector(length=length(P))		##	This is the daily time-step T-M storage, used to calc baseflow
	totQ<-vector(length=length(P))
	Se<-vector(length=length(P))
	impervIa<-Ia_coef*5  			##	Se = 5 mm for impervious areas (CN=98)
	impervPe<-deltaP-impervIa		##  Seemed to match best with Zach's imperve runoff
	impervPe[which(impervPe<0)]<-0
	impervRunoff<-vector(length=length(impervPe))
	Pe<-vector(length=length(P))				##	Effective Precipitation
	sigmaS<-matrix(nrow=length(P), ncol=no_wet_class)
	runoff_by_class<-matrix(nrow=length(P), ncol=no_wet_class)
	DrainingExcess<-vector(length=length(P))   ## Leftover excess from water redistributing itself over watershed
	Q2<-vector(length=length(P))
	baseflow<-vector(length=length(P))
	MaxWetClass<-vector(length=length(P))
	
# Setting up initial values	
	TM_S[1]<-BF1/rec_coef	
	if(OutStart==3){
		SoilWater[1:2]<-PreviousOutput$SoilWater[1:2]
		excess[1:2]<-PreviousOutput$excess[1:2]
		TM_S[1:2]<-	PreviousOutput$baseflow[1:2]/rec_coef	#	baseflow_obs[1]/rec_coef
		totQ[1:2]<-PreviousOutput$totQ[1:2]
		Se[1:2]<-PreviousOutput$Se[1:2]
		sigmaS[1:2,]<-eswsc*Se[1:2]
	} else {
		if (is.null(SW1)){
			SoilWater[1]<-AWC			
		} else SoilWater[1]<-SW1
		excess[1]<-0
		Se[1]<-Se_avg		## Could refine this to make beginning of model better
		sigmaS[1,]<-eswsc*Se[1]
		totQ[1]<-Pe[1]*Pe[1]/(Pe[1]+Se[1])
		impervRunoff[1]<-impervPe[1]
	}
	Ia<-Ia_coef*Se
	


## Thornthwaite-Mather Function and Runoff Generation 
	for(i in (2):length(deltaP)){		  
	
	# First we calculate runoff	
		if (deltaP[i-shift]-Ia[i-1]>0){
			Pe[i-shift]<-deltaP[i-shift]-Ia[i-1]  
		} else Pe[i-shift]<-0
		
		totQ[i]<-Pe[i-shift]*Pe[i-shift]/(Pe[i-shift]+Se[i-1])		## Effective storage must be from previous day
		Q2[i]<-mean(Pe[i-shift]-sigmaS[i-1,which(sigmaS[i-1,]<Pe[i-shift])])  ## Just to check output against totQ
				# might give me an error... (if doesn't like assigning Q2 to NULL)
		DistSoilWater<-SAT-sigmaS[i-1,]  ## This does not correspond exactly to the actual soil water distributed...
		ind<-which(DistSoilWater>AWC)				## Just a representation for calculating excess
		DrainingExcess[i]<-(sum(DistSoilWater[ind]-AWC)/no_wet_class)*SAT_coef

# Water Balance
		if(deltaP[i-shift]<=0){
			#if(SoilWater[i-1]<=AWC){
				SoilWater[i]<-SoilWater[i-1]*exp(deltaP[i-shift]/AWC)-DrainingExcess[i]	
			#} else {
			#excess[i]<-(SoilWater[i-1]-AWC)	#*SAT_coef
			#SoilWater[i]<-AWC*exp(deltaP[i-shift]/AWC)+(SoilWater[i-1]-AWC)	#*(1-SAT_coef)
			#}
		
		} else if (deltaP[i-shift]+SoilWater[i-1]-DrainingExcess[i]<=AWC){
		SoilWater[i]<-deltaP[i-shift]+SoilWater[i-1]-DrainingExcess[i]
		
		} else {
		SoilWater[i]<-AWC  ## So overall SW cannot exceed AWC again
		excess[i]<-(deltaP[i-shift]+SoilWater[i-1]-AWC)	-DrainingExcess[i]
		}
		
		Se[i]<-max(20,(1-SoilWater[i-1]/AWC)*2.381*Se_avg+SoilWater[i-1]/AWC*0.27*Se_avg)	 ##	Linear approx 
		sigmaS[i,]<-eswsc*Se[i]  ## Amount of storage available in each wetness class [mm]
		Ia[i]<-	Ia_coef*Se[i]
		if ((excess[i]+DrainingExcess[i])>=totQ[i]){
			TM_S[i]<-TM_S[i-1]*(1-rec_coef)+excess[i]+DrainingExcess[i]-totQ[i]  
		} else {
			TM_S[i]<-TM_S[i-1]*(1-rec_coef)
			SoilWater[i]<-SoilWater[i]-(totQ[i]-excess[i]-DrainingExcess[i])
		}
		
		baseflow[i]<-rec_coef*TM_S[i]		
		impervRunoff[i]<-impervPe[i-shift] 		
		
	}
### End of Water Balance Loop ############################################################################


# Option 3 - use the coefficients generated from Observed Streamflow   #### Might want to include these in parameters
	FirstDay<-c(.8,0.8)	#FirstDay<-c(.84,0.7)	#dry-wet.   average 0.75
	SecondDay<-c(.2,0.2)	#SecondDay<-c(.12,0.23)		#0.19
	LastBit<-c(0,0)	#LastBit<-c(.04,0.07)		#0.06
	
	OverlandFlow<-vector(length=length(totQ))
	ShallowInterflow<-vector(length=length(totQ))
	OverlandFlow[1]<-totQ[1]*FirstDay[1]
	OverlandFlow[2]<-totQ[2]*FirstDay[1]
	ShallowInterflow[1]<-0
	ShallowInterflow[2]<-totQ[1]*SecondDay[1]
	for (i in 3:length(totQ)){
		if (Se[i-1]>Se_avg){
			OverlandFlow[i]<-totQ[i]*FirstDay[1]
			ShallowInterflow[i]<-totQ[i-2]*LastBit[1]+totQ[i-1]*SecondDay[1]
		}else{
			OverlandFlow[i]<-totQ[i]*FirstDay[2]
			ShallowInterflow[i]<-totQ[i-2]*LastBit[2]+totQ[i-1]*SecondDay[2]
		}
	#Determine the number of wetness classes contributing to flow
		if (OverlandFlow[i]+ShallowInterflow[i]>0){
			MaxRunoffOnlyInEachClass<-vector(length=no_wet_class)
			for(j in 1:(no_wet_class-1)){
				MaxRunoffOnlyInEachClass[j]<-sigmaS[i,j+1]-sigmaS[i,j]
			}
		MaxRunoffOnlyInEachClass[no_wet_class]<-100 ## Last wetness class is assigned a high value - will never reach this.
		intermediateSum<-0
		j<-1  # Now cycle through and determine MaxWetClass contributing to flow
		while (j<(no_wet_class+1)){
			if (((MaxRunoffOnlyInEachClass[j]*j+intermediateSum)/no_wet_class)>=OverlandFlow[i]+ShallowInterflow[i]){
				MaxWetClass[i]<-j   	## so MaxWetClass represents last wetness class that produced runoff
				runoff_by_class[i,j]<-((OverlandFlow[i]+ShallowInterflow[i])*no_wet_class-intermediateSum)/j
				for (k in (j-1):1){
					runoff_by_class[i,k]<-sigmaS[i,j]-sigmaS[i,k]+runoff_by_class[i,j]
					}
				j<-no_wet_class+1  ## ends the while loop
			} else {
				intermediateSum<-sum(sigmaS[i,j+1]-sigmaS[i,1:j])
				j<-j+1
			}
		}
		}else	MaxWetClass[i]<-0
	}
	
	modeled_flow<-(OverlandFlow+ShallowInterflow)*(1-0.01*percentImpervious)+impervRunoff*(0.01*percentImpervious)+baseflow				#totQ+baseflow
	quickflow_combined<-(OverlandFlow+ShallowInterflow)*(1-0.01*percentImpervious)+impervRunoff*(0.01*percentImpervious)
	rain_snowmelt<-P
	Streamflow<-data.frame(dateSeries, rain_snowmelt, modeled_flow, baseflow, OverlandFlow, ShallowInterflow, totQ, deltaP, Pe, Se, Ia, SoilWater, quickflow_combined, impervRunoff, excess, Tmax, Tmin, Q2, MaxWetClass)[OutStart:length(P),] 

	if (nrow(Streamflow)==1){
		return (c(as.character(dateSeries[length(P)]), rain_snowmelt[length(P)], modeled_flow[length(P)], baseflow[length(P)], OverlandFlow[length(P)], ShallowInterflow[length(P)], totQ[length(P)], deltaP[length(P)], Pe[length(P)], Se[length(P)], Ia[length(P)], SoilWater[length(P)], quickflow_combined[length(P)], impervRunoff[length(P)], excess[length(P)], Tmax[length(P)], Tmin[length(P)], Q2[length(P)], MaxWetClass[length(P)]))
	} else (return(Streamflow))
}
