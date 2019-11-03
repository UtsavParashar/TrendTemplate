library("zoo", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("quantmod")

################################
##Earnings, PAT and Revenues Checks####
################################


results<- function(comp){
  base <- rev(comp)
  percentageBaseChange <- round(((Delt(base))*100),2)
  TwoAvg <- round ((rollmean(percentageBaseChange, 2, na.pad=TRUE, align="right")),2)
  cat("QoQ/YoY:", c(percentageBaseChange), "\n")
  cat("2Q/YAvg", c(TwoAvg), "\n")
  percentageBaseChangeYoY <- round((((tail(base, n=1) - head(base, n=1))/head(base, n=1))*100),2)
  cat("YoY/5YoY", percentageBaseChangeYoY, "\n")
}


#####Quarterly#######
print("Quarterly - EPS")
results( c(209.14,	118.96,	113.21,	142.31,	68.92))
print("Quarterly - Sales")
results( c(60.47,	32.11,	31.34,	26.09,	24.58))
#print("Quarterly -PAT")
#results( c(77.23,	70.67,	41.32,	40.51,	64.08))

#####Yearly############
print("Yearly - EPS")
results( c(17.04,	55.51,	52.93,	94.32,	65.31))
print("Yearly - Sales")
results( c(1867.88,	1463.81,	1416.89,	1366.78,	1024.08))
#print("Yearly - PAT")
#results( c(1172.13,	2896.45,	729.20,	621.29,	529.00))





