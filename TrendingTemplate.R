library("zoo", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("quantmod")

##Invoking Shell Scripts
setwd("/Users/utsav/Investment/trading/data/scripts")
system("./getData.sh")

  
#Functions
DMA200 <- function(data){
  mean(tail(data, 200))
}

DMA150 <- function(data){
  mean(tail(data, 150))
}

DMA50 <- function(data){
  mean(tail(data, 50))
}

DMA20 <- function(data){
  mean(tail(data, 20))
}

DMA5 <- function(data){
  mean(tail(data, 5))
}

get52WeekLow <- function(data){
  min(tail(data, 200))
}

get52WeeksHigh <- function(data){
  max(tail(data, 200))
}

updateDataFrameWith200DMA <- function(data){
  data$MV200 <- rollmean(dataCol, 200, na.pad=TRUE, align="right")
}

VolumeDMA5 <- function(data){
  mean(tail(data, 5))
}

VolumeDMA50 <- function(data){
  mean(tail(data, 50))
}

VolumeDMA200 <- function(data){
  mean(tail(data, 200))
}

DeliveredToTradedQtyMean <- function(data, days){
  mean(tail(data,days))
}

#Variables
setwd("/Users/utsav/Investment/trading/data")
data <- read.csv("data.csv")
if("Close.Price" %in% colnames(data)){
  dataCol <- data$Close.Price #BSE Data
  dataVolume <- data$No.of.Shares
}else{
  dataCol <- data$Average.Price #NSE Data
  
}

DMAVar200 <- DMA200(dataCol)
DMAVar150 <- DMA150(dataCol)
DMAVar50<- DMA50(dataCol)
DMAVar20<- DMA20(dataCol)
DMAVar5 <- DMA5(dataCol)
currentPrice <- tail(dataCol,1)
YearLow <- get52WeekLow(dataCol)
YearHigh <- get52WeeksHigh(dataCol)
Greater20PercentFromYearLow <- ((((YearHigh - YearLow)*20)/100) + YearLow)
Lower50PercentFromYearHigh <- (YearHigh - (((YearHigh - YearLow)*50)/100))
Lower25PercentFromYearHigh <- (YearHigh - (((YearHigh - YearLow)*25)/100))
dataNew<- updateDataFrameWith200DMA(data)
VolumeMV50Days <- VolumeDMA50(dataVolume)
VolumeMV200Days <- VolumeDMA200(dataVolume)
VolumeMV5Days <- VolumeDMA5(dataVolume)

print("=========FUNDAMENTALS=================")
fundamentalsDataFrame <- data.frame(
  PriceDetails = c("Current Price:","52 weeks high:",
                    "52 weeks low:","200Day Moving Average:", 
                    "150Day Moving Average:","50Day Moving Average:",
                    "20Day Moving Average:", "5Day Moving Average", "20% greater than 52 Weeks low:",
                    "25% lower than 52 Weeks high:", "50% lower than 52 Weeks high:"
                    ),
  PriceValues = c(currentPrice, YearHigh, YearLow, DMAVar200, DMAVar150,
                          DMAVar50,DMAVar20, DMAVar5, Greater20PercentFromYearLow, 
                          Lower25PercentFromYearHigh,Lower50PercentFromYearHigh
                          )
)
print(fundamentalsDataFrame)

print("====BUYING CONDITIONS/TRENDING TEMPLATE======")
if((currentPrice > DMAVar150) && (currentPrice > DMAVar200)){
  ConditionStatus <- c("Satisfied")
}else{
  ConditionStatus <- c("Failed")
}

if(DMAVar150 > DMAVar200){
  ConditionStatus[2] = c("Satisfied")
}else{
  ConditionStatus[2] = c("Failed")
}

if((DMAVar50 > DMAVar150) && (DMAVar50 > DMAVar200)){
  ConditionStatus[3] = c("Satisfied")
}else{
  ConditionStatus[3] = c("Failed")
}

if(currentPrice > DMAVar50){
  ConditionStatus[4] = c("Satisfied")
}else{
  ConditionStatus[4] = c("Failed")
}
      
if(currentPrice > Greater20PercentFromYearLow){
  ConditionStatus[5] = c("Satisfied")
}else{
  ConditionStatus[5] = c("Failed")
}

if(currentPrice > Lower25PercentFromYearHigh){
  ConditionStatus[6] = c("Satisfied")
}else{
  ConditionStatus[6] = c("Failed")
}

##My Condition
if(currentPrice < Lower50PercentFromYearHigh){
  #ConditionStatus[7] = c("Satisfied")
}else{
  #ConditionStatus[7] = c("Failed")
}

ConditionNumber <- c("Condition1", "Condition2", "Condition4", "Condition5",
                   "Condition6", "Condition7")

ConditionValue <- c("currentPrice>150&200", "150>200", "50>150&200",
                    "currentPrice>50", "currentPrice>20%52WL", 
                    "currentPrice>25%52WH")

#"MyCondition", "currentPrice<50%52WH"
conditionDataFrame <- data.frame(ConditionNumber, ConditionValue, ConditionStatus)
print(conditionDataFrame)



#50 day moving average of volume
# We need to buy when the volume is below 50 day moving average for a day or two,
# Here is the place when liquidity is less and then the stock moves 
# up rapidly.
print("==========VOLUME CHECK BEFORE BUYING===================")
lastDayVolume<- tail(dataVolume, 1)
lastToLastDayVolume <- head(tail(dataVolume,2),1)
VolumeDays<- c("200DaysMVA", "50DaysMVA", "5DaysMVA", "LastToLastDay", "LastDay", "Volume Condition V<50DMA")
VolumeAverage <- c(VolumeMV200Days, VolumeMV50Days, VolumeMV5Days, lastToLastDayVolume, lastDayVolume)

if(VolumeMV50Days > lastDayVolume){
  VolumeAverage[length(VolumeAverage)+1] = "SATISFIED"
}else{
  VolumeAverage[length(VolumeAverage)+1] = "FAILED"
}
VolumeDataFrame <- data.frame(VolumeDays, VolumeAverage)
print(VolumeDataFrame)

##########################
###Special Conditions#####
##########################
print("========SPECIAL CONDITIONS========")
if(DMAVar5 > DMAVar20){
  print("5DMA>20DMA, PLEASE BUY")
}else{
  print("5DMA<20DMA, PLEASE SELL")
}

####################
###SELL#############
####################
#If the stock after buying drops to less than 20DMA then sell
print("===============SELLING CONDITIONS=============")
if(currentPrice < DMAVar20){
  print ("Current Prize is less than 20DMA, PLEASE SELL")
}else{
  print ("Current Prize is greater than 20DMA, Please DON'T SELL, check stop loss")
}


#Plot Spread High Low
#hist(data$Spread.High.Low)
#hist(data$Spread.Close.Open)

####################
#####GRAPHS#########
####################
#Condition 3 for one month
filledRow200DMA <- nrow(data) - 200
cat("Condition 3 Prices for last 30 days - Row filled: ", filledRow200DMA, "\n")
MV200ForLast30Days <- tail(dataNew, 30)
#print(MV200ForLast30Days)
if(!is.na(tail(MV200ForLast30Days,1))){
plot(MV200ForLast30Days, type = "o", main = "Condition 3", xlab = "Days", ylab = "Price")
}

#Condition 3 for 5 months
#cat("Condition 3 Prices for last 150 days - Row filled: ", filledRow200DMA, "\n")
MV200ForLast150Days <- tail(dataNew, 150)
if(!is.na(tail(MV200ForLast150Days,1))){
plot(MV200ForLast150Days, type = "o", main = "Condition 3 - 5 months", xlab = "Days", ylab = "Price")
}
#Condition 3 for last 12months
#cat("Condition 3 Prices for last 365 days - Row filled: ", filledRow200DMA, "\n")
MV200ForLast365Days <- tail(dataNew, 365)
if(!is.na(tail(MV200ForLast365Days,1))){
plot(MV200ForLast365Days, type = "o", main = "Condition 3 - 12 months", xlab = "Days", ylab = "Price")
}
#Condition 3 for last 3 years
#cat("Condition 3 Prices for last 3 and 5 years - Row filled: ", filledRow200DMA, "\n")
MV200ForLast1100Days <- tail(dataNew, 1100)
if(!is.na(tail(MV200ForLast1100Days,1))){
plot(MV200ForLast1100Days, type = "o", main = "Condition 3 - 3 Years", xlab = "Days", ylab = "Price")
}
#Condition 3 for last 5 years
#cat("Condition 3 Prices for last 3 and 5 years - Row filled: ", filledRow200DMA, "\n")
MV200ForLast1800Days <- tail(dataNew, 1800)
if(!is.na(tail(MV200ForLast1800Days,1))){
plot(MV200ForLast1800Days, type = "o", main = "Condition 3 - 5 Years", xlab = "Days", ylab = "Price")
}
#Plot Volumne of shares trades
data$ID<- seq.int(nrow(data)) #for getting numbers like seq(1,5)
#barplot(data$No.of.Shares, names.arg=data$ID, main = "Volume")
plot(data$No.of.Shares, type="o", main = "Volume", ylab = "Shares", xlab = "days")

#Plot close price
plot(dataCol, type = "o", xlab = "days", ylab = "Price", main = "Close Price")
#print(head(sort(data$Spread.Close.Open, decreasing = TRUE), n=5))

#Plot Volumne for last 600 days
volume600Days<- tail(data$No.of.Shares, 600)
plot(volume600Days, type = "o", xlab = "days", ylab = "Volumne", main = "Volume 600 days")

#Plot close price for last 600 days
price600Days <- tail(data$Close.Price, 600)
plot(price600Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 600 days")



#Plot Volumne for last 200 days
volume200Days<- tail(data$No.of.Shares, 200)
#barplot(volume200Days, names.arg=seq.int(1, length(volume200Days)), main = "Volume 200 days")
plot(volume200Days, type = "o", xlab = "days", ylab = "Volumne", main = "Volume 200 days")

#Plot close price for last 200 days
price200Days <- tail(data$Close.Price, 200)
plot(price200Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 200 days")

#Plot Volumne for last 100 days
volume100Days<- tail(data$No.of.Shares, 100)
barplot(volume100Days, names.arg=seq.int(1, length(volume100Days)), main = "Volume 100 days")


#Plot close price for last 100 days
price100Days <- tail(data$Close.Price, 100)
plot(price100Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 100 days")

#Plot Volumne for last 50 days
volume50Days<- tail(data$No.of.Shares, 50)
barplot(volume50Days, names.arg=seq.int(1, length(volume50Days)), main = "Volume 50 days")
#plot(volume50Days, type = "o", xlab = "days", ylab = "Volumne", main = "Volume 50 days")
#Plot close price
price50Days <- tail(data$Close.Price, 50)
plot(price50Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 50 days")

#Plot Volumne for last 35 days
volume35Days<- tail(data$No.of.Shares, 35)
barplot(volume35Days, names.arg=seq.int(1, length(volume35Days)), main = "Volume 35 days")
price35Days <- tail(data$Close.Price, 35)
plot(price35Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 35 days")

#Plot Volumne for last 20 days
volume20Days<- tail(data$No.of.Shares, 20)
barplot(volume20Days, names.arg=seq.int(1, length(volume20Days)), main = "Volume 20 days")
price20Days <- tail(data$Close.Price, 20)
plot(price20Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 20 days")

#Plot Volumne for last 7 days
volume7Days<- tail(data$No.of.Shares, 7)
barplot(volume7Days, names.arg=seq.int(1, length(volume7Days)), main = "Volume 7 days")
price7Days <- tail(data$Close.Price, 7)
plot(price7Days, type = "o", xlab = "days", ylab = "Price", main = "Close Price - 7 days")

if(FALSE){
#Plot Deliver to traded Quantity - 200 days
barplot(tail(data$X..Deli..Qty.to.Traded.Qty,200), names.arg = (1:200), ylab = "quantity",
        xlab = "days", main = "Delivery to traded quantity - 200 days")
#hist(data$X..Deli..Qty.to.Traded.Qty)
}


#Plot Deliver to traded Quantity - 25 days
barplot(tail(data$X..Deli..Qty.to.Traded.Qty,25), names.arg = (1:25), ylab = "quantity",
        xlab = "days", main = "Delivery to traded quantity - 25 days")

#Plot Deliver to traded Quantity - 50 days
barplot(tail(data$X..Deli..Qty.to.Traded.Qty,50), names.arg = (1:50), ylab = "quantity",
        xlab = "days", main = "Delivery to traded quantity - 50 days")

### Delivered to Traded Qty
print("=====Delivered to traded qty mean=======")
cat('200 day -->', DeliveredToTradedQtyMean(data$X..Deli..Qty.to.Traded.Qty,200), '\n')
cat('100 day -->', DeliveredToTradedQtyMean(data$X..Deli..Qty.to.Traded.Qty,100), '\n')
cat('50 day -->', DeliveredToTradedQtyMean(data$X..Deli..Qty.to.Traded.Qty,50), '\n')
cat('10 day -->', DeliveredToTradedQtyMean(data$X..Deli..Qty.to.Traded.Qty,10), '\n')
cat('5 day -->', DeliveredToTradedQtyMean(data$X..Deli..Qty.to.Traded.Qty,5), '\n')

#Plot for Volatility
barplot(tail(data$Spread.High.Low,50), names.arg = (1:50), ylab = "High-Low",
        xlab = "days", main = "Volatility")

#Delivered to traded histogram
hist(tail(data$X..Deli..Qty.to.Traded.Qty,10), col='red')
hist(tail(data$X..Deli..Qty.to.Traded.Qty,50), col='green')
hist(tail(data$X..Deli..Qty.to.Traded.Qty,200), col='blue')
hist(tail(data$X..Deli..Qty.to.Traded.Qty,400), col='yellow')

#####MF Condition check#####
setwd ("/Users/utsav/Investment/trading/data/mfHoldings")
system("./mfholdings.sh all RBL")


