## Changes to Owasco Input data because of incomplete records

## Records are missing before these index values: 1205, 1262,1270, 1283, 1329

DS[c(1204,1205,1261,1262,1269, 1270, 1282,1283,1328, 1329)]
DS <- c(DS[1:1328],as.Date("2012-12-27"), DS[1329:length(DS)])
DS <- c(DS[1:1282],as.Date("2012-11-10"), DS[1283:length(DS)])
DS <- c(DS[1:1269],as.Date("2012-10-27"), DS[1270:length(DS)])
DS <- c(DS[1:1261],as.Date(c("2012-10-11","2012-10-12","2012-10-13","2012-10-14","2012-10-15","2012-10-16","2012-10-17","2012-10-18")), DS[1262:length(DS)])
DS <- c(DS[1:1204],as.Date(c("2012-08-08","2012-08-09","2012-08-10","2012-08-11","2012-08-12","2012-08-13","2012-08-14")), DS[1205:length(DS)])

# DS[which(is.na(as.Date(strptime(DS,"%Y-%m-%d"))))] <- as.Date(c("2012-08-08","2012-08-09","2012-08-10","2012-08-11","2012-08-12","2012-08-13","2012-08-14","2012-10-11","2012-10-12","2012-10-13","2012-10-14","2012-10-15","2012-10-16","2012-10-17","2012-10-18","2012-10-27","2012-11-10","2012-12-27"))

summary(as.Date(strptime(DS,"%Y-%m-%d")))
which(is.na(as.Date(strptime(DS,"%Y-%m-%d"))))
DS[1205:1212]


length(DS)
nrow(OwascoUSGS)
tail(DS)
tail(OwascoUSGS)


#DS[c(1204,1205,1261,1262,1269, 1270, 1282,1283,1328, 1329)]
# [1] "2012-08-07" "2012-08-15" "2012-10-10" "2012-10-19" "2012-10-26" "2012-10-28" "2012-11-09" "2012-11-11" "2012-12-26" "2012-12-28"

####  Pmm
Pmm <- c(Pmm[1:1328],5.7, Pmm[1329:length(Pmm)])
Pmm <- c(Pmm[1:1282],0, Pmm[1283:length(Pmm)])
Pmm <- c(Pmm[1:1269],0, Pmm[1270:length(Pmm)])
Pmm <- c(Pmm[1:1261],0,1.3,0.3,8.4,0,0,0,0, Pmm[1262:length(Pmm)])
Pmm <- c(Pmm[1:1204],0,0,21,0,0,4,0, Pmm[1205:length(Pmm)])

####  TxC
TxC <- c(TxC[1:1328],-2, TxC[1329:length(TxC)])
TxC <- c(TxC[1:1282],6, TxC[1283:length(TxC)])
TxC <- c(TxC[1:1269],18, TxC[1270:length(TxC)])
TxC <- c(TxC[1:1261],13,12,7,10,20,18,9,17, TxC[1262:length(TxC)])
TxC <- c(TxC[1:1204],28,31,31,31,24,27,25, TxC[1205:length(TxC)])

####  TnC
TnC <- c(TnC[1:1328],-6.4, TnC[1329:length(TnC)])
TnC <- c(TnC[1:1282],-5, TnC[1283:length(TnC)])
TnC <- c(TnC[1:1269],9, TnC[1270:length(TnC)])
TnC <- c(TnC[1:1261],0.6,2,-5.6,-3,10,4.4,-1.7,-1.1, TnC[1262:length(TnC)])
TnC <- c(TnC[1:1204],12,15,18,18,18,13,13, TnC[1205:length(TnC)])

