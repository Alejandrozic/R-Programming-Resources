# --------- #
# -- 6.3 -- #
# Checking The Normality Assumption
# --------- #

# Import Data

path_tunnel <- "ex06-93tunnel.csv"
path_oxyben <- "ex06-94oxyben.csv"

tunnel <- read.csv(path_tunnel)
oxyben <- read.csv(path_oxyben)

colnames(tunnel) <- c('sec')
attach(tunnel)

colnames(oxyben) <- c('ppt')
attach(oxyben)

# Assessing Normaloty Method 1
# Graphs for tunnel/sec

hist(sec, main="Distribution of tunnel travel time", xlab="Seconds", ylab="Frequency")

boxplot(sec, data=tunnel, main="Distribution of Tunnel Travel Time")

stem(sec)

# Assessing Normaloty Method 2
# Check how close tunnel_ratio is to 1.3

tunnel_iqr <- IQR(sec)
tunnel_sd <- sd(sec)
tunnel_ratio <- (tunnel_iqr/tunnel_sd)

# Assessing Normaloty Method 3
# Observe whether the pattern in the normal probability plot is linear

qqnorm(sec, main="Normal Quantile Plot of Tunnel Travel Time",
	xlab="Theoreticval Quantiles", ylab="Sample Quanitiles",
	pch=20)
qqline(sec)

# Assessing Normaloty Method 4
# Computing proportion of sampke values with 1,2,3 standard deviations 
# from mean

tunnel_mean <- mean(sec)

tunnel_interval <- c(
	paste("(", round(tunnel_mean - 1*tunnel_sd, 2), ")", sep=""),
	paste("(", round(tunnel_mean - 2*tunnel_sd, 2), ")", sep=""),
	paste("(", round(tunnel_mean - 3*tunnel_sd, 2), ")", sep="")

)
tunnel_frequency <- c(
	sum( sec > tunnel_mean - 1* tunnel_sd & sec < tunnel_mean + 1* tunnel_sd),
	sum( sec > tunnel_mean - 2* tunnel_sd & sec < tunnel_mean + 2* tunnel_sd),
	sum( sec > tunnel_mean - 3* tunnel_sd & sec < tunnel_mean + 3* tunnel_sd)
)

tunnel_proportion <- round(tunnel_frequency / nrow(tunnel), 2)

backward_emperical_table <- data.frame(tunnel_interval, tunnel_frequency, tunnel_proportion)
