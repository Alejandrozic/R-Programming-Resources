# --------- #
# -- 6.1 -- #
# --------- #

# Five types of problems for normal distributions
# (Calculate Areas Given Values)
# - Find area to left of value
# - Find area to right of value
# - Find the area between two values
# (Calculate Values given Areas)
# - Find a value with a certain are to the left of it
# - Find a value with a certain are to the right of it

mean = 68
sd = 2.5

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
