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
