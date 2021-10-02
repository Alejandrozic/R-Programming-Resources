# --------- #
# -- 6.1 -- #
# Probability Distributions for a Continuous Random Variable
# --------- #

# Five types of problems for normal distributions
# (Calculate Areas Given Values)
# - Find area to left of value
# - Find area to right of value
# - Find the area between two values
# (Calculate Values given Areas)
# - Find a value with a certain are to the left of it
# - Find a value with a certain are to the right of it

mean <- 68
sd <- 2.5

# Find P(X<=65)
# Method 1
pnorm(65, mean, sd)
# Method 2
z = (65-mean)/sd
pnorm(z)

# Find P(X>65)
# Method 1
pnorm(65, mean, sd, lower.tail = FALSE)
# Method 2
z = (mean-65)/sd
pnorm(z)

# Find P(65<=X<=68)
pnorm(68, mean, sd)-pnorm(65, mean, sd)

# Find the value x, with P(Height <= X) = 0.9
qnorm(0.9, mean, sd)

# Find the value x, with P(Height > X) = 0.25
qnorm(0.25, mean, sd, lower.tail = FALSE)


# CONTINUOUS #
# F(x) = x/8 where 0<=x<=4
f <- function(x) {x/8}
lower_limit <- 0
upper_limit <- 4

# P (x <= 1)
integrate(f, lower = lower_limit, upper = 1)$value

# P (x > 3)
1 - integrate(f, lower = lower_limit, upper = 3)$value
# or
integrate(f, lower = 3, upper = upper_limit)$value

# P (x > 4)
integrate(f, lower = 4, upper = upper_limit)$value

# P (2 <= X <= 3)
integrate(f, lower = 2, upper = 3)$value

# P (X <=2 | X<=3)
a <- 2
b <- 3
(a^2 * integrate(f, lower = a-1, upper = upper_limit)$value) / (b^2 * integrate(f, lower = a-1, upper = upper_limit)$value)

# P (X <= c) = 0.5
target_density <- 0.5
find_upper_bnd<- function(f, interval, target) {
    optimize(function(x) {
        abs(integrate(f, lower_limit, x)$value-target)
    }, interval)$minimum
}
find_upper_bnd(f, interval=c(lower_limit,upper_limit), target=target_density)

# When the Department of Transportation (DOT) repaints the center lines, 
# edge lines, or no-passing-zone lines on a highway, epoxy paint is sometimes 
# applied. This paint is more expensive than latex but lasts longer. If this 
# paint splashes onto a vehicle, it has to be completely sanded off, and that 
# area of the vehicle has to be repainted. The DOT has warned motorists that 
# the drying time for this epoxy paint (in minutes) has a uniform distribution 
# with  a=30  and  b=60.  Suppose epoxy paint is applied to a small section of 
# center line.

# Provided
lower_limit <- 30	# a
upper_limit <- 60	# b

f <- function(x) {(1*x)/((upper_limit-lower_limit)*x)}

# P(X<=45)
integrate(f, lower = lower_limit, upper = 45)$value

# P(40<=X<=50)
integrate(f, lower = 40, upper = 50)$value

# P(X>=55)
 1 - integrate(f, lower = lower_limit, upper = 55)$value

# P (X >= c) = 0.75
target_density <- 0.75
find_lower_bnd<- function(f, interval, target) {
    optimize(function(x) {
        abs(integrate(f, x, upper_limit)$value-target)
    }, interval)$minimum
}
find_lower_bnd(f, interval=c(lower_limit,upper_limit), target=target_density)

# The Jasper SkyTram transports passengers to the top of Whistler Mountain 
# in the Canadian Rockies where there are spectacular views, boardwalks, and 
# hiking trails. The SkyTram travel time (in minutes) up the mountain is a 
# random variable  X  with probability density function given.

# Probability Density Function
prob_f <- function(x) {(-0.75*x^2)+(11.25*x)-41.4375}

lower_limit <- 6.5
upper_limit <- 8.5

# Cumulative Distribution Function
cumul_f <- function(x) {
	if (x < lower_limit) {
		result <- 0
	}
	else if ((upper_limit >= x) & (x >= lower_limit)) {
		result <- (-0.25)*((6.5-x)^2)*(x-9.5)
	}
	else {
		result <- 1
	}
	return(result)
}

# Plot "Probability Density Function"
curve(expr=prob_f, from=lower_limit, to=upper_limit)

# Plot "Cumulative Distribution Function"
curve(expr=cumul_f, from=lower_limit, to=upper_limit)

# P(X < 7)
1 - integrate(prob_f, lower = 7, upper = upper_limit)$value

# P(X > 8.25)
1 - integrate(prob_f, lower = lower_limit, upper = 8.25)$value

# Select the probability that five SkyTram rides up the mountain 
# all take more than  8  minutes each.
# More then 8 minutes
# 5 members
# P(X > 8)
(1 - integrate(prob_f, lower = lower_limit, upper = 8)$value)^ 5

# Some of the common over‑the‑counter medications to help people with 
# relieve headaches include Advil, Aleve, Excedrin, and aspirin. Suppose 
# Excedrin is formulated so that an individual will feel headache pain 
# relief within  30  minutes. The probability density function for  X,  
# the time (in minutes) it takes to to feel headache relief after taking 
# an Excedrin tablet, is given by

# Probability Density Function
f <- function(x) {
 	if (0 <= x & x <= 10)		{return(0.05)}
 	else if (10< x & x <= 30) 	{return((-0.0025)*(x-30))}
 	else 				{return(0)}
}

# P(X<=10)
integrate(Vectorize(f), lower = 0, upper = 10)$value

# P (10 < X <= 30)
integrate(Vectorize(f, lower = 10, upper = 30)$value

# P(X<=5)
integrate(Vectorize(f), lower = 0, upper = 5)$value

# P (20 <= X <= 30)
integrate(Vectorize(f), lower = 20, upper = 30)$value

# Find a value  𝑡  such that the probability of experiencing relief within  t  
# minutes after taking a tablet  is 0.75.  Provide your answer precise to three 
# decimal places.
# P (X <= c) = 0.75
upper_limit = 30
lower_limit = 0
target_density <- 0.75
find_upper_bnd<- function(f, interval, target) {
    optimize(function(x) {
        abs(integrate(f, lower_limit, x)$value-target)
    }, interval)$minimum
}
find_upper_bnd(Vectorize(f), interval=c(lower_limit,upper_limit), target=target_density)

# If it takes less than fifteen minutes to experience relief after taking a tablet, 
# people consider the medication a success. Suppose 20 people, each of whom has a 
# headache, are selected at random and take an Excedrin tablet. Let  𝑌  be the number 
# of people of the  20  who experience relief successfully. What is the probability that 
# exactly  14  of the  20  experience headache relief within  15  minutes? Give your 
# answer precise to three decimal places.

# Combinational Foormula
nCr <- function(x1, x2) {
factorial(x1)/(factorial(x2)*(factorial(x1-x2)))
}

# P(X <= 15)
prob_of_success <- integrate(Vectorize(f), lower = 0, upper = 15)$value

f_prob_at_least <- function(x) {
	nCr(20, x)*(prob_of_success^x)*(1-prob_of_success)^(20-x)
}

#P(Y=14)
nCr(20, 14)*(prob_of_success^14)*(1-prob_of_success)^(20-14)

# P(Y >= 16)
f_prob_at_least(16)+f_prob_at_least(17)+f_prob_at_least(18)+f_prob_at_least(19)+f_prob_at_least(20)

# P(Y <= 10)
f_prob_at_least(0)+f_prob_at_least(1)+f_prob_at_least(2)+f_prob_at_least(3)+f_prob_at_least(4)+f_prob_at_least(5)+f_prob_at_least(6)+f_prob_at_least(7)+f_prob_at_least(8)+f_prob_at_least(9)+f_prob_at_least(10)
