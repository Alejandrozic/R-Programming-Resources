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
