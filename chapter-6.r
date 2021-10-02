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

# P (x <= 1)
integrate(f, lower = 0, upper = 1)$value

# P (x > 3)
1 - integrate(f, lower = 0, upper = 3)$value
# or
integrate(f, lower = 3, upper = 4)$value

# P (x > 4)
integrate(f, lower = 4, upper = 4)$value

# P (2 <= X <= 3)
integrate(f, lower = 2, upper = 3)$value

# P (X <=2 | X<=3)
a <- 2
b <- 3
(a^2 * integrate(f, lower = a-1, upper = 4)$value) / (b^2 * integrate(f, lower = a-1, upper = 4)$value)

# P (X <= c) = 0.5
target_density <- 0.5
lower_limit <- 0
upper_limit <- 4
find_upper_bnd <- function(f, interval, target) {
    optimize(function(x) {
        abs(integrate(f, lower_limit, x)$value-target)
    }, interval)$minimum
}
find_upper_bnd(f, interval=c(lower_limit,upper_limit), target=target_density)
