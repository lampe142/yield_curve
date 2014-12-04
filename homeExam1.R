#-----------------------------------------------------------------------------
#Filename: homeExam1.R
#Description: Bootstrapping the discount curve from overnight deposits, money
# market-market deposits interest rate futures and interest rate swaps  
#Authors: Max Lampe
#Created: 29.11.2013
#-----------------------------------------------------------------------------
# homeExam1.R starts here
#-----------------------------------------------------------------------------

###### import data from Nov. 19 #####
# clear memory
rm(list=ls()) 
year <- 365.242
# define instruments 
overNight <- list(time=c(1, 2)/year, rate=c(0.015, 0.05)*10^(-2)) 
moneyMarket <- list(delta=c(1*7, 1*30)/year, rate=c(0.077, 0.108)*10^(-2))
intRateFuture <- list(T1=as.Date(c("2012-12-19", "2013-03-20", "2013-06-19", "2013-09-18", "2013-12-18")), T2=as.Date(c("2013-03-20", "2013-06-19", "2013-09-18", "2013-12-18", "2014-03-19" )), priceMid=c(99.810+99.815, 99.835 + 99.840, 99.820+99.825, 99.805+99.810, 99.770+99.775)/2)
intRateSwap <- list(time=c(2,3,4,5,7,10,12,15,20), mid=c(0.399, 0.506, 0.666, 0.860,
				1.239, 1.676, 1.893, 2.110, 2.242)*10^(-2))
# load functions, make sure both files are in the same directory
source("functions_discount_curve.R")
#computes the entire discount curve
(curve <- ComputeDiscountCurve(overNight, moneyMarket, intRateFuture, intRateSwap))
# compare results to slides results
check(curve)
