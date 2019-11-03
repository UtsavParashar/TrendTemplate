library(plyr)
library(gtools)

system("/Users/utsav/Investment/trading/data/scripts/removeFiles.sh /Users/utsav/Desktop/ProfitPercentage.csv")
currentProfit <- function(lastTradedPrice, costPrice){
  ((lastTradedPrice - costPrice)/costPrice)*100;
}

maxPriceAndLastTradedPriceDifference <- function(maxPrice, lastTradedPrice){
  ((lastTradedPrice - maxPrice)/maxPrice)*100
}

df <- data.frame(matrix(ncol = 8, nrow = 0))
x <- c("symbol", "maxPrice", "broughtDate", "costPrice", "lastTradedPrice", "profitPercentage", "MPDiffLTP", "MPGtLTP")
colnames(df) <- x
#print (df)
write.csv(df, file = "/Users/utsav/Desktop/ProfitPercentage.csv")

growth <- function(symbol, maxPrice, tradeDate, costPrice, lastTradedPrice ){
symbol <- c(symbol)
maxPrice <- c(maxPrice)
broughtDate <- c(tradeDate)
costPrice <- c(costPrice)
lastTradedPrice <- c(lastTradedPrice)
profitPercentage <- currentProfit(lastTradedPrice, costPrice)
MPDiffLTP <- maxPriceAndLastTradedPriceDifference(maxPrice, lastTradedPrice)
if(lastTradedPrice > maxPrice){
  allTimeHigh = TRUE
}else{
  allTimeHigh = FALSE
}
MPGtLTP = allTimeHigh

x = data.frame(symbol, maxPrice, broughtDate, costPrice, lastTradedPrice, profitPercentage, MPDiffLTP, MPGtLTP)
names(x)=NULL
#print(x, row.names = FALSE)
write.table(x, "/Users/utsav/Desktop/ProfitPercentage.csv", sep = ",", col.names = T, append = T)
}


#####ADITYA BIRLA CAPITAL######
growth("ADITYA BIRLA CAPITAL", 150, "02-07-2018", 137.33, 132.30)
#####BharatFinance######
growth("Bharat Finance", 1234, "30-07-2018", 1202.80, 1120.95)
#####IBVentures######
growth("IBVentures", 819.55, "01-08-2018", 601.11, 727.55)
#####MuthootCapitalService######
growth("MuthootCapital", 1300, "30-07-2018", 1216.23, 980.60)
#####HeidelbergCement######
growth("HeidelbergCement", 163, "26-07-2018", 156.51, 166.25)
#####VIPIndustries######
growth("VIPIndustries", 633.45, "30-07-2018", 486.63, 540.55)
#####Marico######
growth("Marico", 387.75, "30-07-2018", 365.81, 336.50)
#####Prabhat Dairy######
growth("Prabhat Dairy", 173, "16-07-2018", 149.72, 156.10)
#####KPIT Technologies######
growth("KPIT Technologies", 311.59, "02-08-2018", 309.95, 306.25)
#####NIIT Technologies######
growth("NIIT Technologies", 1394.15, "01-08-2018", 1256.37, 1319.15)
#####DISH TV######
growth("DISH TV", 73, "19-06-2018", 71.29, 67.05)
#####Orient Refractories######
growth("Orient Refractories", 279, "01-08-2018", 207.55, 238.20)
#####Relaxo Footwares######
growth("Relaxo Footwares", 873, "27-07-2018", 824.70, 831.75)
#####Suryaamba spinning wheels######
growth("Suryaamba spinning", 136, "31-07-2018", 105.82, 82.15)
#####Universal Cables Ltd.######
growth("Universal Cables Ltd.", 230, "20-08-2018", 223.78, 199.15)
#####HIL Ltd.######
growth("HIL Ltd.", 2555, "20-08-2018", 2446, 2540.15)
#####Tanfac Inds######
growth("Tanfac Inds", 330, "20-08-2018", 330, 359.15)
#####FINE_ORGANICS_INDUS######
growth("FINE_ORGANICS_INDUS", 1244, "20-08-2018", 1094, 1097.15)
#####Whirlpool######
growth("Whirlpool", 1793, "20-08-2018", 1793, 1668.15)
#####Atul######
growth("Atul", 3265, "20-08-2018", 3275, 3128)
#####Ruchisoya######
growth("Ruchisoya", 10.85, "28-08-2018", 9.89, 9.10)
#####Yes Bank######
growth("Yes Bank", 404, "20-08-2018", 399, 339.15)
#####RDB RASAYAN######
growth("RDB RASAYAN", 108.50, "20-08-2018", 105.10, 106.55)
#####Dabur######
growth("Dabur", 490, "28-08-2018", 487.35, 454.10)
#####Cipla######
growth("Cipla", 677, "28-08-2018", 651, 660)
#####V-mart######
growth("V-mart", 3110, "28-08-2018", 3102, 2809)

