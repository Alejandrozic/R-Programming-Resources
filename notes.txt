# Cleanup #
rm(list=ls())

# --------- #
# -- 5.2 -- #
# --------- #

x <- 1:6
px <- x*(x+1)/112 # Function that generates probability
pdf <- data.frame(x, px)
plot(x, px, type="h", xlab="X", ylab="P(X=x)", main="PDF of X")

# mu + c(-1, 1)*2*sd
# data_name [c(rowA:rowN), c(columnA:columnN)]

# --------- #
# -- 5.3 -- #
# --------- #

# -- x
x <- 88:93

# -- P(x)
px <- c(.03, .07, .3, .25, .2, .15)

# -- Table x / P(x)
chips <- data.frame(x, px)

# -- Mean
mean <- sum(ch*pch)

# -- Variance
# -- Theoretical Forumla: 	sum(((x-mean)**2)*px)
# -- Computational Formula: 	sum((x**2)*px) - (mean)**2
variance <- sum((x**2)*px) - (mean)**2

# -- Standard Deviation
standard_dev <- sqrt(variance)

# -- Print
round(cbind(mean, variance, standard_dev), 4)
#      mean variance standard_dev
#[1,] 90.97   1.6491       1.2842

# -- Find probability within 1 standard deviation of the mean
#	> chips
#	  x  px
#	1 88 0.03
#	2 89 0.07
#	3 90 0.30
#	4 91 0.25
#	5 92 0.20
#	6 93 0.15
interval <- mean + c(-1, 1)*standard_dev # Compute Interval
set <- chips[c(3:5), c(1:2)]
sum(set$px) # Compute probability

# --------- #
# -- 5.4 -- #
# --------- #

# Let X~B(20, 0.45)

# x
x <- 0:20

# P(X=x) binomial random variable
px <- round(dbinom(x, 20, 0.45), 4)

# P(X<=x) cumulative distribution
cx <- round(pbinom(x, 20, 0.45), 6)

# Plots
plot(x, px, type="h", xlab="X", ylab="P(X=x)", main="PDF of X~B(20, 0.45)")
plot(x, cx, type="h", xlab="X", ylab="P(X<=x)", main="CDF of X~B(20, 0.45)")

# EXTRA
# Let X~B(15, 0.25)
# P(X>6) cumulative distribution
1 - round(pbinom(6, 15, 0.25), 4)
# P(X>=6) cumulative distribution
1 - round(pbinom(5, 15, 0.25), 4)
# P(3<=X<=10) cumulative distribution
round(pbinom(10,15,0.25)-pbinom(2,15,0.25),4)
# P(42<=X<=47) cumulative distribution
round(pbinom(47,15,0.25)-pbinom(41,15,0.25),4)
# Let X~B(15, 0.75)
# P(X<10) cumulative distribution
round(pbinom(9,15,0.75),4)
# P(X>12)
round(1 - pbinom(12,n,p),4)
# P(X<=15) cumulative distribution
round(pbinom(15,25,0.40),4)

# --------------- NEXT --------------- #

n <- 20	# The number of trials for X~ B(n, p).
p <- 0.45	# The probability of success for X~ B(n, p).

# -- Mean
mean <- n*p

# -- Variance
variance <- n*p*(1-p)

# -- Standard Deviation
standard_dev <- sqrt(variance)

# -- Print
round(cbind(mean, variance, standard_dev), 4)

# -- Find probability within 1 standard deviation of the mean
# -- Compute the lower/upper bound of the interval
lwr_endpt <- mean - standard_dev
upr_endpt <- mean + standard_dev
prtc <- pbinom(lwr_endpt, n, p) + (1 - pbinom(upr_endpt, n, p))

# -- Print
round(prtc, 2)

# Compute P(X>mean+sd | x>mean)
# Caclulcate mean+sd and then mean
ptrd <- (1-pbinom(11, n, p)) / (1-pbinom(9, n, p))

# --------- #
# -- 5.5 -- #
# --------- #

# Let X~Geom(0.25)

ps <- 0.25	# Probability of Success

# The count of failures before the first success
x <- 0:35

# The probability mass function values of X~Geom(0.25)
px <- dgeom(x, ps)

# The cumulative distributions values of X~Geom(0.25)
cx <- pgeom(x, ps)

# Plots
plot(x, px, type="h", xlab="X", ylab="P(X=x)", main="PDF of X~Geom(0.25))")
plot(x, cx, type="h", xlab="X", ylab="P(X<=x)", main="CDF of X~Geom(0.25)")

# ALTERNATIVE
# First success on 5th trial
# px <- dgeom(4, ps)
# cx <- pgeom(4, ps)

# -- Mean
mean <- 1/ps

# -- Variance
variance <- (1-ps) / (ps**2)

# -- Standard Deviation
standard_dev <- sqrt(variance)

# -- Two standard deviations above the mean
mu_2sd <- mean + 2*standard_dev

# -- Print values
round(cbind(ps, mean, variance, standard_dev, mu_2sd), 3)

# -- Compute P(X>mu+2sd)
prtc <- 1 - pgeom((mu_2sd-1), ps)
round(prtc, 4)

# -- Compute P(X<3 | X<10)
prtd <- pgeom(1, ps) / pgeom(8, ps)
round(prtd, 4)

# EXTRA
# Let X~Geom(0.25)
# P(X=1)
round(pgeom(0, 0.25), 4)
# P(3<=X<=7) ==> P(x<=7) - P(x<=2)
pgeom(6, ps)-pgeom(1, ps)


# --------------- NEXT --------------- #

# Let X~Poisson(4.5) where lambda is 4.5

# Count of events in a given interval
x = <- 0:18

# The probability disribution P(X=x)
px = dpois(x, 4.5)

# The cumulative distributions P(X<=x)
cx = ppois(x, 4.5)

# Plots
plot(x, px, type="h", xlab="X", ylab="P(X=x)", main="PDF of X~Poisson(4.5)")
plot(x, cx, type="h", xlab="X", ylab="P(X<=x)", main="CDF of X~Poisson(4.5)")

# Mean
mean = 4.5 # Mean is the same as the lambda

# Standard Deviation
standard_dev = sqrt(4.5)

# Compute P(X >mu)
prtc <- 1 - ppois(mean, 4.5)

# -- Two standard deviations above the mean
# -- P(X <= mu+2sd)
mu_2sd <- mean + 2*standard_dev
prtd <- ppois(mu_2sd, 4.5)

# EXTRA
# Let X~Poisson(4)
# P (X<3)
round(ppois(2, 4), 3)
# P (X=1)
round(dpois(1, 4), 4)
# P (x=4 U x=5)  where "U" is union, which equals P(x=4)+P(x=5)
round(dpois(4, 4.5) + dpois(5, 4), 4)
