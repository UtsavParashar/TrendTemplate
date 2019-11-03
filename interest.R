rate<- function(amount, principal, n){
  (((amount/principal)^(1/n))-1)*100
}

siRate <- function(amount, principal, n){
  si <- amount - principal
  (100*si)/(principal*n)
}

amount<- function(princ, rate, ti){
  princ*((1+rate/100)^ti)
}

prinSimInt <- function(int, rate, time){
  (int*100)/(rate*time)
}

#cat("Amount", amount(2500000,10, 20))

#cat("Earning", rate(242, 79, 4), "\n")
#cat("Book Value", rate(1197, 612, 4), "\n")
#cat("Price", rate(9434, 2635, 4), "\n")

cat(rate(1975, 1869, 0.4), "Interest \n")