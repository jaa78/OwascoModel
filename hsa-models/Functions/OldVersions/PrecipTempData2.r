## PrecipTempData.r
## Josephine Archibald
## 2011-12-27
## Get temperature and precip Data from Owasco Lake Watershed and NOAA Weather
## stations.

## Current Precip Data NERFC Rain Gauge Totals
todaysDate <- substr(as.character(Sys.time()),1,10)

## This is volunteer data from CoCoRaHS
PrecipURL <- "http://www.nws.noaa.gov/data/TAR/HYDTAR"

## Sometimes doesn't contain all the stations..
t1 <- read.table(PrecipURL, fill=TRUE, skip=12, row.names=NULL)
V6 <- as.character(paste(t1[,7:ncol(t1)]))

if(ncol(t1)>=11) {
	V6 <- as.character(paste(t1[,7],t1[,8], t1[,9], t1[,10], t1[,11]))
} else if(ncol(t1)==10) {
	V6 <- as.character(paste(t1[,7],t1[,8], t1[,9], t1[,10]))
} else if (ncol(t1)==9){
	V6 <- as.character(paste(t1[,7],t1[,8], t1[,9]))
} else if (ncol(t1)==8){
	V6 <- as.character(paste(t1[,7],t1[,8]))
} else V6 <- as.character(t1[,7])

t2 <- data.frame(t1[,1:5],V6)

## Make sure I'm only getting stations in NY in case of duplicate names in 
## another state. Some of the stations put NY on the next line, so read in the 
## date and time from the online precip file.
todaysTable <- t2[c(grep("NY",t2[,6]),(which(t2[,1]=="NY")-1)),]
TableInfo<-readLines(PrecipURL, n=7)[7]
#write.table(todaysTable,file=paste("Precip_",todaysDate, ".csv",
#sep=",", quote=FALSE,col.names=TRUE)
#todaysTable

## Append precipitation in inches.
todaysTable[,5] <- as.numeric(as.character(todaysTable[,5]))
IthPrecip <- mean(todaysTable[grep("Ithaca", todaysTable[,6]),5])
AuburnPrecip <- mean(todaysTable[grep("Auburn", todaysTable[,6]),5])
CortlandPrecip <- mean(todaysTable[grep("Cortland", todaysTable[,6]),5])
FreevillePrecip <- mean(todaysTable[grep("Freeville", todaysTable[,6]),5])
 # Note : Lansing was actually "Lansing, Schoharie county, not useful for us.  
AuroraPrecip<-mean(todaysTable[grep("Aurora", todaysTable[,6]),5])
LockePrecip<-mean(todaysTable[grep("Locke", todaysTable[,6]),5])
SyracusePrecip<- mean(todaysTable[grep("Syracuse", todaysTable[,6]),5])


## Get max and min temp for Syracuse for the 24 hour period before 8 am 
## in degrees F. Also appened NOAA recorded precipitation.
DataDownloadTime <- strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")	
todaysTableTEMP <- read.table("http://www.nws.noaa.gov/data/WBN/SCS04", header=FALSE, skip=15, fill=TRUE)
#todaysTableTEMP<-read.table("http://www.nws.noaa.gov/xml/tpex/scs.php", fill=TRUE, skip=20, header=FALSE)

MaxTempSyr <- as.numeric(as.character(todaysTableTEMP[which(todaysTableTEMP[,1]=="SYRACUSE"),2]))
MinTempSyr <- as.numeric(as.character(todaysTableTEMP[which(todaysTableTEMP[,1]=="SYRACUSE"),3]))
##  Here we meed to make sure that the max/min temperature columns are not switched
if (MaxTempSyr < MinTempSyr) {  
x<-MinTempSyr
MinTempSyr<-MaxTempSyr
MaxTempSyr<-x
}
PrecipSyrNOAA <- as.numeric(as.character(todaysTableTEMP[which(todaysTableTEMP[,1]=="SYRACUSE"),4]))

## When there is no rain the table reports a blank in the precip column.
if(is.numeric(MaxTempSyr) & is.na(PrecipSyrNOAA))
PrecipSyrNOAA<-0

## Get max and min temp from Ithaca (degrees F)
## Record Max temp in past 24 hours (measured in 6 hour intervals)
library(XML)
temp <- readHTMLTable("http://www.nws.noaa.gov/data/obhistory/KITH.html", 
		     skip.rows=c(1,2,3,70,71,72,73,74,75,76,77,78), header=FALSE)[[4]]
MaxTempIth <- max(na.omit(as.numeric(as.character(temp[1:24,9]))))
MinTempIth <- min(na.omit(as.numeric(as.character(temp[1:24,10]))))
precipIth <- sum(na.omit(as.numeric(as.character(temp[1:24,13]))))

## Ithaca Game Farm Road Data Logger
temp <- readHTMLTable("http://www.nrcc.cornell.edu/climate/ithaca/gfr_logger.html",
	skip.rows=c(1,2,3))[[1]]
IthPrecipGameFarm <- as.numeric(as.character(temp[nrow(temp),5]))

## Co-op data (collected by citizen scientists) 
## ONLY Needs to be run ONCE - once the file is set up with these headings, 
## then we want to add the daily data everyday (See below).
# titles<-c("Co-op Data Date Time", "Aburn P (in)", "Cortland Precip (in)", "Ithaca Precip (in)", "Moravia Precip (in)",
# 	"Lansing P (in)", "Freeville P (in)", "Aurora", "Locke")
# write(titles, file="CoopData2.csv", ncolumns= 10, append=FALSE, sep=",")
# setwd("E:\\Soil and Water Lab\\Modeling\\Owasco\\Precip Data")
## This needs to be run EVERY DAY at approximately 10:30 am
TodaysCoopData <- c(TableInfo,AuburnPrecip,CortlandPrecip, IthPrecip, 
		    FreevillePrecip, AuroraPrecip, LockePrecip, SyracusePrecip)
write(TodaysCoopData, file="CoopData2.csv", ncolumns= 10, append=TRUE, sep=",")

## NOAA Data
## This first part only needs to be run once, to set up the file...
# NOAA_NRCCtitles<-c("Time of Download (Data is from 24 hours before this)", "Syr (in)", "Ith (in)", "Ith Game Farm (in)", 
# 	"SYR max temp (F)",	"SYR min temp (F)", "Ith max temp (F)", "Ith min temp (F)")
# write(NOAA_NRCCtitles, file="NOAA_NRCCData.csv", ncolumns= 8, append=FALSE, sep=",")

## This needs to be run EVERY DAY at approximately 10:30 am
TodaysNOAAData <- c(as.character(DataDownloadTime),PrecipSyrNOAA,precipIth,
		    IthPrecipGameFarm,MaxTempSyr,MinTempSyr,MaxTempIth,MinTempIth)
write(TodaysNOAAData, file="NOAA_NRCCData.csv", ncolumns= 8, append=TRUE, sep=",")






