library("zoo", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("quantmod")

################################
##QUARTERLY -- Earnings and Revenues Checks####
################################

eps <- c(10.46, 5.79, 4.19,7.20, 12.91 )
percentageEPSChange <- (Delt(eps))*100
TwoQuarterAvg <- rollmean(percentageEPSChange, 2, na.pad=TRUE, align="right")
print(percentageEPSChange)
print(TwoQuarterAvg)
#plot(percentageEPSChange, type = "o", col = "red", main = "Quartley EPS Change", ylab = "Percentage Change", 
 #    xlab = "Quarter")
#lines(TwoQuarterAvg, type = "o", col = "blue")

if( 1 == 2){
revenues <- c(1986.81, 1960.39, 1984.91, 2222.39, 2115.96, 2089.34, 2119.30, 2385.38, 2411.00, 2388.38)
percentageRevenueChange <- (Delt(revenues))*100
TwoQuarterAvg <- rollmean(percentageRevenueChange, 2, na.pad=TRUE, align="right")
plot(percentageRevenueChange, type = "o", col = "red", main = "Quartely Revenues Change", ylab = "Percentage Change", 
     xlab = "Quarter")
lines(TwoQuarterAvg, type = "o", col = "blue")

PAT <- c(196.72, 191.86, 210.39, 227.57, 210.76, 196.73, 208.69, 245.34, 249.17, 244.69)
percentagePATChange <- (Delt(PAT))*100
TwoQuarterAvg <- rollmean(percentagePATChange, 2, na.pad=TRUE, align="right")
plot(percentagePATChange, type = "o", col = "red", main = "Quaterly PAT Change", ylab = "Percentage Change", 
     xlab = "Quarter")
lines(TwoQuarterAvg, type = "o", col = "blue")

quarterlyMargins <- (PAT/revenues)*100
plot(quarterlyMargins, type = "o", main = "Quarterly Profit Margins(PAT/Revenue)",
     xlab = "Years", ylab = "Profit Percentage")

#Inventories
inventories<- c(301.53, 253.63, 268.34, 311.20, 382.28, 331.49, 366.86, 345.74, 384.01, 602.61)
percentageInvetoryChange <- (Delt(inventories))*100
plot(percentageInvetoryChange, type = "o", col = "red",
     main = "Annual Inventory Revenues Comparision", xlab = "Quarters", ylab = "Percentage")
lines(percentageRevenueChange, type = "o", col = "blue")

#Receivables
receivables <- c(46.33,  49.61,  39.49,  57.26,  52.14,  77.12,  53.69,  70.98, 106.70, 126.41)
percentageReceivablesChange <- Delt(receivables)*100
lines(percentageReceivablesChange, type = "o", col = "green")

#Code 33 Earnings, Revenues and profit margins
eps <- c(5.90, 6.13, 6.24, 7.06)
percentageEPSChange <- Delt(eps)*100
revenues <- c(8309, 8590, 9097, 9487)
percentageRevenuesChange <- Delt(revenues)*100
PAT <- c(1276, 1326, 1351, 1529)
percentagePATChange <- Delt(PAT)*100
plot(percentageEPSChange, type = "o", col = "red",
     main = "Code 33", xlab = "Quarters", ylab = "Percentage")
lines(percentageRevenuesChange, type = "o", col = "blue")
lines(percentagePATChange, type = "o", col = "green")

#####################
## Annual Changes:##
####################
revenues <- c(2621, 3145, 3404, 4272, 5032, 5670, 6342, 7263, 8046, 8559)
year<- 2008:2017
percentageRevenueChange <- Delt(revenues)*100
PAT <- c(191, 180, 116, 145, 186, 233, 369, 622, 749, 843)
percentagePATChange <- Delt(PAT) * 100
eps <- c(79, 75, 48, 12, 15, 19, 30, 51, 62, 70)
percentageEPSChange <- Delt(eps) * 100
AnnualData <- data.frame(year, revenue <- revenues, RevChange <- percentageRevenueChange, 
                         pat <- PAT, PATChange <- percentagePATChange,
                         EPS <- eps, EPSChange <- percentageEPSChange)
print(AnnualData)
plot(percentageRevenueChange, type = "o", col = "red", main = "Annual Revenue Changes", ylab = "Percentage Change", 
     xlab = "Year")

#lines(percentageEPSChange, type = "o", col = "blue")
#lines(percentagePATChange, type = "o", col = "green")

#Revenue to Profit ~ Margins
margins <- (PAT/revenues)*100
plot(margins, type = "o", main = "Annual Profit Margins(PAT/Revenue)", 
     xlab = "Years", ylab = "Profit Percentage")

#Inventories
inventories<- c(301.53, 253.63, 268.34, 311.20, 382.28, 331.49, 366.86, 345.74, 384.01, 602.61)
percentageInvetoryChange <- (Delt(inventories))*100
plot(percentageInvetoryChange, type = "o", col = "red",
     main = "Annual Inventory Revenues Comparision", xlab = "Quarters", ylab = "Percentage")
lines(percentageRevenueChange, type = "o", col = "blue")

#Receivables
receivables <- c(46.33,  49.61,  39.49,  57.26,  52.14,  77.12,  53.69,  70.98, 106.70, 126.41)
percentageReceivablesChange <- Delt(receivables)*100
lines(percentageReceivablesChange, type = "o", col = "green")

}
