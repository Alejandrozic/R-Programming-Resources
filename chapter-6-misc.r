#########
# NOTES #
#########

############################################################################
Set the mean and standard deviation to  0  and  5 , respectively. Find 
the value for which approximately  5%  of the area under the normal curve
lies to the right of this value.

P(X>x) = 0.05   
P(X≤x) =   0.95 <-- For this example

z=(x-µ)/σ 
z*σ +µ = x

x <- qnorm(0.95) * 5 + 0
x is 8.2

############################################################################
Set the mean and standard deviation to  0  and  5 , respectively. Approximately
5%  of the area under the normal curve lies to the right of  8.2 . Consider what
would happen if you were to change the mean to  50  and keep the standard 
deviation at  5 .

Can you predict the new value for which approximately  5%  of the area under 
the normal curve lies to the right of this value? Enter your answer accurate 
to one decimal place.

Use data from above

x <- qnorm(0.95) * 5 + 50
x is 58.2

############################################################################
Suppose you know that the amount of time it takes your friend Susan to get 
from her residence to class averages  50  minutes, with a standard deviation 
of  5  minutes. What proportion of Susan's trips to class would take more than  
50  minutes? You should not need to use the applet to know this value.

µ <- 50
σ <- 5

proportion more than  50  minutes? if 50 is the average (middle), then 50% or 0.5

proportion less than  40  minutes?
P( X ≤ 40)
P( (X-µ)/σ ≤ (40-50) /5)
P( Z ≤ -2)
pnorm(-2, mean = 0, sd = 1)

############################################################################
P( Z > 2.13)
pnorm(2.13, mean = 0, sd = 1, lower.tail = TRUE)

############################################################################
What is P(1.39 < Z < 2.03)?

P( Z < 2.03) - P( Z < 1.39)
0.9177 <- pnorm(1.39, mean = 0, sd = 1, lower.tail = TRUE)
0.9788 <- pnorm(2.03, mean = 0, sd = 1, lower.tail = TRUE)

0.9788 - 0.9177 = 0.0611 or 6.11%

############################################################################
Set the mean and standard deviation to  0  and  5 , respectively. What proportion
of the normal density curve with this mean and standard deviation falls below the 
value  −10.0 ?

P( X < -10   ) 
P( (X-µ)/σ ≤ (-10-0) /5)
P(Z < -2.00)
pnorm(-2, mean = 0, sd = 1)

############################################################################
Normal Distributions

N(10,2) reads mean 10 and sd 2

############################################################################
N(10,2)
P( 10 < Y < 12)
P( (10-10)/2 < (X-µ)/σ < (12-10)/2)
P( 0 <= Z <= 1)
P( Z <= 1) - P( Z <= 0)

pnorm(1, mean = 0, sd = 1) - pnorm(0, mean = 0, sd = 1)
... 0.34

############################################################################
N(10,2)
P( 8 < Y < 12)
P( (8-10)/2 < (X-µ)/σ < (12-10)/2)
P( -1 <= Z <= 1)
P( Z <= 1) - P( Z <= -1)

pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)
... 0.68


############################################################################
The Emperical Rule 

standared deviation
Within one 		0.68
Within two		0.95
Within three	0.997

############################################################################
X ~ U(25, 75)

height = 1/(75-25) = 1/50

Find P(30 <= X <= 40)
Area = b*h
 = (40-30)*(1/50)
 = 1/5

Find mean / variance / sd
a <- 25
b <- 75
mean <- (a+b)/2
variance <- ((b-a)^2) / 12
sd <- sqrt(variance)

Find P (X<= mean -2sd U X>= mean +2sd)
= P(x <= 21.44) + P(X >= 78.86)
Since these are outside of our range ... this equals 0.

Find P (x >= c) = 0.4
c <- (1 - 0.4) * mean + a
