# Discount factors implied by the Over night and tomorrow Next #
OverNightsToDiscount <- function(oveNig, year=365.242){
	Z <- 1/(1+ 1/year* oveNig$rate)
	for (i in 1:length(Z)){
		Z[i] <- prod(Z[1:i])
	}
	disCur <- list(time=c(0, oveNig$time), discount=c(1,Z))
	return(list(time=oveNig$time, discount=Z))
}

# Discount factors implied by money market instruments #
MoneyMarketToDiscount <- function(disCur, monMar, ts=2, year=365.242){
	ts <- 2/year
	Z0ts <- numeric(1)
	if(ts %in% disCur$time){
		Z0ts <- disCur$discount[match(ts, disCur$time)]
	} else {
		Z0ts <- InterpolateDiscountFactor(disCur=disCur, tx=ts)
	}
	Z <- Z0ts/(1+monMar$delta*monMar$rate)
	t <- monMar$delta + ts
	disCur <- list(time=c(disCur$time, t), discount=c(disCur$discount, Z))
	return(disCur)
}

# computes discount Factors inbetween two discount Factors by linear interpolation
InterpolateDiscountFactor <- function(disCur, tx=0.5){	
	Zxt	<-	approx(disCur$time, -log(disCur$discount), xout= tx, rule=2)$y
	return(exp(-Zxt))
}

## Interpolation
interpolateDF <- function( disCur, tx) {
	Z <- disCur$discount
	t <- disCur$time
    if (tx < t[1]) {
        return(exp(log(Z[1]) * tx / t[1]))
    } else if (tx > t[length(t)]) {
        return(exp(log(Z[length(t)]) * tx / t[length(t)]))
    } else {
        for (i in 2:length(t)) {
            if (tx < t[i]) {
                totalDist <- t[i] - t[i - 1]
                fracLeft <- (tx - t[i - 1]) / totalDist
                fracRight <- (t[i] - tx) / totalDist
                return(exp(log(Z[i - 1]) * fracLeft + log(Z[i]) * fracRight))
            }
        }
    }
}

# computes the difference between two dates as a fraction of year
# standard is a day count conevtion of 360
CalcDeltaDates <- function(t1, t2, dayCount=360){
	delta <- as.numeric((t2-t1)) / dayCount
	return(delta)
}

# computes the discount actors implied by a Forward rate agreement
ForwardToDiscount <-function(disCur, forRate, T0=as.Date("2012-11-19")){
	delta <- CalcDeltaDates(t1=forRate$T1, t2=forRate$T2)
	for (i in 1:length(forRate$T1)){
		if(CalcDeltaDates(t2=forRate$T1[i], t1=T0, dayCount=365.242) %in% disCur$time){
			Z0T1 <- disCur$discount[match(forRate$T1[i], disCur$time)]
		} else{
			Z0T1 <- InterpolateDiscountFactor(disCur=disCur, tx= forRate$T1[i])
		}
			Z0T2 <-  Z0T1 / (forRate$rate[i]*delta[i]+1)
			disCur$time[length(disCur$time)+1] <- CalcDeltaDates(t1=T0, t2=forRate$T2[i], dayCount=360) 
			disCur$discount[length(disCur$discount)+1] <- Z0T2
	}
	return(disCur)
}
# transforms a Intetrest rate future to a Forward rate agreement
FutureToForward <- function(intRatFut, sigma=0.006, T0=as.Date("2012-11-19")){
	F <- (100 - intRatFut$priceMid)*10^(-2)
	T1 <- as.numeric((intRatFut$T1 - T0)/360)
	T2  <-as.numeric((intRatFut$T2 - T0)/360)
	f <- F - sigma^2*T1*T2/2
	return(list(rate=f, T1=intRatFut$T1, T2=intRatFut$T2))
}

# computes the discount factors implied by interest rate future
# by transforming it into a forward rate agreement and computing its implied
# discount factors
FutureToDiscount <- function(disCur, intRatFut){
	forwardRate <- FutureToForward(intRatFut) 
	return(ForwardToDiscount(disCur, forwardRate))
}

# computes the squared errors of interest rate swap fixed rate and the given 
# set of discount factors of a interest rate swap
H2 <- function(Z, correctH){
    (correctH-(1-tail(Z, n=1))/sum(Z))^2
}

Hoptim <- function(tknown, tnot, Zknown, Znguess, correctH) {
    last <- tail(Zknown, n = 1)
    guess <- Znguess
    guessWeights <- (tnot - tail(tknown, n = 1)) / (tail(tnot, n = 1) - tail(tknown, n = 1))
    lastWeights <- 1 - guessWeights
    Znot <- exp(guessWeights * log(guess) + lastWeights * log(last))
    return(H2(c(Zknown, Znot), correctH))
}

# computes the discount factors implied by a set of interest rate swap 
InterestSwapToDiscount  <- function(disCur, intRateSwap, ts=2, year=360){
	ts <- 2/365.242
	Z0ts <- disCur$discount[match(ts, disCur$time)]
	 d  <- length(intRateSwap$mid)	
	for (i in 1:d) {
		# the following case division is for swapsfor which all nessecary
		# discount factors are known and for one has to optimize them
		if(intRateSwap$time[i] < 6){
			t <- (1:intRateSwap$time[i])+ts
			H <- intRateSwap$mid[i]
			Z  <- sapply(X=t, FUN=InterpolateDiscountFactor, disCur=disCur)
			Z0tN  <- (Z0ts-H*sum(Z[-length(Z)]))/(1+H) 
			l <- length(disCur$time)
			disCur$time[l+1] <- intRateSwap$time[i]+ts
			intRateSwap$time[i]+ts
			disCur$discount[l+1]  <- Z0tN
		} else{
			tknown <- (1:intRateSwap$time[i-1])+ts
			tnot <- (intRateSwap$time[i-1]+1) : intRateSwap$time[i]	+ ts
			Zknown  <- sapply(X=tknown, FUN=InterpolateDiscountFactor, disCur=disCur) 
            result <- optim(par = 1, fn = Hoptim, tknown = tknown, tnot = tnot, Zknown = Zknown, correctH = intRateSwap$mid[i], method = "L-BFGS-B", lower = 0, upper = 1)
			l <- length(disCur$time)
			disCur$time[l+1] <- intRateSwap$time[i]+ts
			disCur$discount[l+1]  <- result$par
		}
	}	
	return(disCur)
}

# computes the discount factors based on overNight, money market yields
# interest rate futures and interest rate swaps
# the verbose option is for plotting the discount curve standard is TRUE


ComputeDiscountCurve <- function(overNight, moneyMarket, intRateFuture, intRateSwap, verbose=TRUE){
	discountCurve <- OverNightsToDiscount(oveNig=overNight)
	discountCurve <- MoneyMarketToDiscount(disCur=discountCurve, monMar=moneyMarket)
	discountCurve <- FutureToDiscount(disCur=discountCurve, intRatFut=intRateFuture)
	discountCurve  <- InterestSwapToDiscount(discountCurve, intRateSwap)	
	if(verbose){
		plot(x=discountCurve$time, y=discountCurve$discount, type='o', ylab="discount Factors", xlab="Time in years")
	}
	timePoints  <- c(0.00548, 0.02464, 0.08761, 0.32855, 0.58317, 0.83232, 1.07874, 1.32515, 2.00415, 3.00349, 4.00556, 5.0049, 7.00358, 10.00433, 12.00574, 15.00375, 20.00591)
    dis <- InterpolateDiscountFactor(discountCurve, timePoints)
	discountCurve <- list(time=c(0,timePoints), discount=c(1, dis))
	return(discountCurve)
}

# compares the results from the discount curve computations with the slides results
check <- function(discountCurve){
    slidetz  <- c(0, 0.00548, 0.02464, 0.08761, 0.32855, 0.58317, 0.83232, 1.07874, 1.32515, 2.00415, 3.00349, 4.00556, 5.0049, 7.00358, 10.00433, 12.00574, 15.00375, 20.00591)
    slideZ <- c(1, 1, 0.99998, 0.99991, 0.99945, 0.99903, 0.99859, 0.99812, 0.99756, 0.99206, 0.98494, 0.9737, 0.9578, 0.91624, 0.84311, 0.79231, 0.72138, 0.62941)
    r  <- discountCurve$discount - slideZ
    names(r) <- slidetz
    return(r)
}

