# --------- #
# -- 6.2 -- #
# The Normal Distribution
# --------- #

# *************************************************************************************** #
# People who ride in hot air balloons usually fly just above the treetops at 
# 200−500 ft.  In populated areas, riders usually stay at an altitude of at 
# least 1000 ft.  The amount of flying time possible in a hot air balloon depends
# on many factors, including the number of propane burners, the number of people 
# in the gondola (basket), and the weather. Assume the time spent aloft is a normally
# distributed random variable  X  with mean  mean=1.5 hours  and standard deviation  
# sd=0.45 hour.

# Units = Hours
mean <- 1.5
sd <- 0.45

# What is the probability that the flight time is between one and two hours?
# P(1<=X<=2)
pnorm(2, mean, sd)-pnorm(1, mean, sd)

# What is the probability that the flight time is more than one hour and fifteen minutes
# P(x > 1.25)
pnorm(1.25, mean, sd, lower.tail = FALSE)

# Find a value  t  such that 10% of all flights last less than  t  hours.
# 10% = 0.1
# Find the value x, with P(Height < X) = 0.1
qnorm(0.1, mean, sd)

# Suppose a person offering hot‑air balloon rides charges  $50  for each ride of at least 
# one hour, and  $1.00  for every minute after one hour. What proportion of rides cost more  
# than $100?  Give your answer to four decimal places.
#	100 = 50 + 1*(x-1)*60
#	50 = (x-1)*60
#	50/60 = x-1
#	50/60 + 1 = x = 1.83333
# P (X > 1.8333)
pnorm(1.8333, mean, sd, lower.tail = FALSE)

# *************************************************************************************** #
# Use technology to compute each probability and choose a graph with the corresponding shaded 
# region. Suppose  X  is a normal random variable with given mean and variance.
# Compute the probability and choose the graph corresponding to the expression

# X ~ N(3.7, 4.55)
# P(3.0 <= X <= 4.0)
mean <- 3.7
var <- 4.55
sd <- sqrt(var)
pnorm(4.0, mean, sd)-pnorm(3.0, mean, sd)

# X ~ N(62, 100)
# P(50 < X < 70) ... P(X<70) - P(X<50)
mean <- 62
var <- 100
sd <- sqrt(var)
pnorm(70, mean, sd)-pnorm(50, mean, sd)

# X ~ N(32, 30)
# P(X>=45) ... 1 - P(X<45)
mean <- 32
var <- 30
sd <- sqrt(var)
1 - pnorm(45, mean, sd)

# X ~ N(77, 0.01)
# P(X < 76.95)
mean <- 77
var <- 0.01
sd <- sqrt(var)
pnorm(76.95, mean, sd)


# X ~ N(-50, 16)
# P(X < -55 U X > -45)
# = P(X <-55)+ P(X > -45)
mean <- -50
var <- 16
sd <- sqrt(var)
pnorm(-55, mean, sd) + (1-pnorm(-45, mean, sd))


# X ~ N(7.6, 12)
# P(8<= X <=9)
mean <- 7.6
var <- 12
sd <- sqrt(var)
pnorm(9, mean, sd)-pnorm(8, mean, sd)

# *************************************************************************************** #
# Sand dollars are a favorite of serious conchologists (people who collect sea shells). They
# can be found on the beaches of Fort Myers and Sanibel Island in Florida. Found on shore, a sand
# dollar is white, with mean diameter  3 in.  Suppose the diameter of a sand dollar is normally 
# distributed with standard  deviation 0.55 in.

# Combinational Foormula
nCr <- function(x1, x2) {
	factorial(x1)/(factorial(x2)*(factorial(x1-x2)))
}

# Units = inches
mean <- 3
sd <- 0.55

# Find a value d such that 90% of all sand dollars have a diameter greater  than d in.
# 90% = 0.9
# Find the value x, with P(Height > X) = 0.9
qnorm(0.9, mean, sd, lower.tail = FALSE)

# Large sand dollars, those with a diameter greater than  4 in,  can be sold to Kelly’s 
# Shell Shack. What is the probability that a randomly selected sand dollar can be sold?
# P(X>4)
pnorm(4, mean, sd, lower.tail = FALSE)

# Suppose  10  sand dollars are selected at random. What is the probability that exactly  
# 7  of the  10  are between  2.5  and  3.5 in  in diameter?
# P(2.5 <= X <=3.5)
probability_at <- function(x, prob_of_succes) {
	nCr(10, x)*(prob_of_success^x)*(1-prob_of_success)^(10-x)
}
prob_of_success <- pnorm(3.5, mean, sd)-pnorm(2.5, mean, sd)
probability_at(7, prob_of_success)

# What is the probability that a randomly selected sand dollar will have a diameter less than 
# 2 in.
# P(X<2)
1 - pnorm(2, mean, sd, lower.tail = FALSE)

# *************************************************************************************** #
# Let Z be a stnadrd normal random variable. Given a graph,  find multiple items.

mean <- 0
sd <- 1

# Find Q1
# P (Z < Q1) = 0.25
q1 <- qnorm(0.25, mean, sd)

# Find Q3
# P (Z < Q3) = 0.25
q3 <- qnorm(0.75, mean, sd)

# Find IQR
# Q3-Q1
iqr <- q3-q1

# Find IFL (Inner Fence Low)
# Q1 - (3/2)(IQR)
ifl <- q1 - (3/2)*iqr

# Find IFH (Inner Fence High)
# Q3 + (3/2)(IQR)
ifh <- q3 + (3/2)*iqr

# Probability beyond inner fences
# 2 * P (Z < ifl )
p_beyond_inner <- 2 * pnorm(ifl, mean, sd)

# OFL Outer Fence Low
# OFL = Q1 - (3)IQR
ofl <- q1 - (3)*iqr

# OFH Outer Fence High
# OFH = Q3 + (3)IQR
ofh <- q3 + (3)*iqr
