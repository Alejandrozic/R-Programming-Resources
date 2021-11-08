##################################################################
# 9.4
#

mean 		<- 44.4	# From trials
n 		<- 14	# From trials
x 		<- 40	# Provided as u
standard_d 	<- 3.384231	# Provided with u

# Find z-stat

z_stat <- (mean - x) / ( standard_d / sqrt(n) )

# Find p-value

# -- if inequality Ha is "greater then"|">"
p_value <- 1 - pnorm(z_stat, mean = 0, sd = 1)

# -- if inequality Ha is "no equal to"|"!=" and z_stat is positive
p_value <- 2 * ( 1 - pnorm(z_stat, mean = 0, sd = 1) )

# -- if inequality Ha is "no equal to"|"!=" and z_stat is negative
p_value <- 2 *  pnorm(z_stat, mean = 0, sd = 1) 

# -- if inequality Ha is "less then"|"<"
p_value <- pnorm(z_stat, mean = 0, sd = 1)

##################################################################
# 9.5
#

# Find SE
standard_d <- 0.7409
n <- 8
SE <- standard_d / sqrt(n)
