##################################################################
# 9
#

# Z-Distribution

mean 		<- 44.4	# From trials [Sample Mean]
n 		<- 14		# From trials [Number of Trials]
x 		<- 40		# Provided as u
standard_d 	<- 3.384231	# Provided with u

# Find test statistic / Z Stat

test_stat <- (mean - x) / ( standard_d / sqrt(n) )

# Find p-value

# -- if inequality Ha is "greater then"|">"
p_value <- 1 - pnorm(test_stat, mean = 0, sd = 1)

# -- if inequality Ha is "no equal to"|"!=" and z_stat is positive
p_value <- 2 * ( 1 - pnorm(test_stat, mean = 0, sd = 1) )

# -- if inequality Ha is "no equal to"|"!=" and z_stat is negative
p_value <- 2 *  pnorm(test_stat, mean = 0, sd = 1) 

# -- if inequality Ha is "less then"|"<"
p_value <- pnorm(test_stat, mean = 0, sd = 1)

#################################################

mean 		<- 44.4	# From trials [Sample Mean]
n 		<- 14		# From trials [Number of Trials]
x 		<- 40		# Provided as u
standard_d 	<- 3.384231	# Provided with u

# T-Distribution

# Find test statistic

test_stat <- (mean - x) / ( standard_d / sqrt(n) )

# Find T-Stat
df <- n - 1

# p_value_lwr <- pt(test_stat, df)  # Lower Tail Area
# p_value_lwr <-1 - pt(test_stat, df)  # Upper Tail Area

# -- if inequality Ha is "greater then"|">"
p_value <- 1 - pt(test_stat, df)

# -- if inequality Ha is "no equal to"|"!=" and z_stat is positive
p_value <- 2 * ( 1 - pt(test_stat, df)) )

# -- if inequality Ha is "no equal to"|"!=" and z_stat is negative
p_value <- 2 *  pt(test_stat, df)

# -- if inequality Ha is "less then"|"<"
p_value <- pt(test_stat, df)


#################################################

# Find SE
standard_d <- 0.7409
n <- 8
SE <- standard_d / sqrt(n)
