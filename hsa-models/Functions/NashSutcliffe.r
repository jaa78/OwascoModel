################
###	Nash-Sutcliffe Efficiency Coefficient (Wikipedia)
#####################

NashSutcliffe<-function(Qobs, Qmod, begin=1, endindex=min(length(Qobs),length(Qmod))){
	if(length(Qobs)==length(Qmod)){
	num<-vector(length=endindex)
	denom<-vector(length=endindex)
	QobsMean<-mean(Qobs[begin:endindex])
	num[begin]<-(Qobs[begin]-Qmod[begin])^2
	denom[begin]<-(Qobs[begin]-QobsMean)^2
	for (i in (begin +1):endindex){
		num[i]<-(Qobs[i]-Qmod[i])^2+num[i-1]
		denom[i]<-(Qobs[i]-QobsMean)^2+denom[i-1]
		}
	return(1-num[length(num)]/denom[length(num)])
	} else return ("Error: observed and modeled data-sets are not the same size")
}


################################################################################################################################
####	To reduce bias by a few extreme events: E:\Soil and Water Lab\Modeling\Efficiency Index\White2009
##################################################################################################################

NashSutcliffe2<-function(Qobs, Qmod, begin=1, endindex=min(length(Qobs),length(Qmod))){
	if(length(Qobs)==length(Qmod)){
	num<-vector(length=endindex)
	denom<-vector(length=endindex)
	QobsMean<-mean(Qobs[begin:endindex])
	num[begin]<-abs(Qobs[begin]-Qmod[begin])
	denom[begin]<-abs(Qobs[begin]-QobsMean)
	for (i in (begin +1):endindex){
		num[i]<-abs(Qobs[i]-Qmod[i])+num[i-1]
		denom[i]<-abs(Qobs[i]-QobsMean)+denom[i-1]
		}
	return(1-num[length(num)]/denom[length(num)])
	} else return ("Error: observed and modeled data-sets are not the same size")
}







#############################################################################################################################
##	Another Option for cutting off the end

#################################################################################################################################
###	Event-Based Nash-Sutcliffe Efficiency
#################################################################################################################################
