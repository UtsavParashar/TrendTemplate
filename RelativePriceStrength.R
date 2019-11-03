#Relative strength is a measure of the price trend of a stock or other financial instrument compared to another stock, instrument or industry. It is calculated by taking the price of one asset and dividing it by another.
#For example, if the price of Ford shares is $7 and the price of GM shares is $25, the relative strength of Ford to GM is 0.28 ($7/25). 

relativePriceNow <- function(price1, price2){
  price1/price2
}

cat("relative price 5 years back",relativePriceNow(374, 490) ,"\n")
cat("relative price now",relativePriceNow(1404, 1923) ,"\n")
#In:10724
#ko:14840
#IC:26933
#HD:50155
#AX:24724
#RB:723