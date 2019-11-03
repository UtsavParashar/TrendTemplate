library(gridExtra)
library(grid)

CompundedGrowth <- function(amount, principal, n){
  (((amount/principal)^(1/n))-1)*100
}

PEG <- function(PE,Growth){
  (PE/Growth)
}

GrowthRate <- function(currentYearPAT, previousYearPAT){
    (((currentYearPAT-previousYearPAT)/previousYearPAT)*100)
}


PEGTable <- function(company, growthRate, peg){
  DESCRIPTION<- c("Company", "GrowthRate", "PEG")
  STATS<- c(company, growthRate, peg)
  x = data.frame(DESCRIPTION,STATS)
  #print(x)
  #grid.table(x)
  companyVar <- c(company)
  grVar<-c(growthRate)
  pegVar<-c(peg)
  y = data.frame(companyVar, format(round(grVar, 2), nsmall = 2), format(round(pegVar, 2), nsmall = 2))
  names(y)=NULL
  Sys.sleep(0.5)
  print(y)
}

PEGFinalDetails <- function(company,PE,currentYearPAT, previousYearPAT, time){
  Growth <-CompundedGrowth(currentYearPAT, previousYearPAT, time)
  PEGVar<-PEG(PE, Growth)
  PEGTable(company, Growth, PEGVar)
}

#(COMPANY,PE,currentYearPAT, previousYearPAT, time)
#######5,2,1 year growth ############
PEGFinalDetails("5 Y G",20, 272.60, 130.63, 4)
PEGFinalDetails("2 Y G",20, 272.60, 211.71, 2)
PEGFinalDetails("1 Y G",20, 272.60, 222.02, 1)
#######5,2,1 year price ############
PEGFinalDetails("5 Y P",8.45,100,44,5)
PEGFinalDetails("2 Y G",8.45,100,111,2)
PEGFinalDetails("1 Y G",8.45,100,183,1)

### Mean of ROCE and ROE
cat('Mean ROE-',mean(c(26.13,	23.42,	27.58,	34.79,	29.40)), '\n')
cat('Mean ROCE-',mean(c(38.96,	30.20,	34.13,	43.02,	35.82)), '\n')
cat('Mean EPS -',mean(c(1.80,	0.95,	0.93,	0.78,	0.73)), '\n')
print(100/(mean(c(1.80,	0.95,	0.93,	0.78,	0.73))))
cat('Mean BookValue -',mean(c(27.23,	27.43,	27.40,	9.27,	9.50)), '\n')
print(100/(mean(c(27.23,	27.43,	27.40,	9.27,	9.50))))
