#############################################################################
# Chapter 12
# Simple Regression

# Data input
x <- c(76, 54, 146, 7, 91, 130, 131, 117)
y <- c(11, 16.9, 8, 24.7, 17.4, 16, 11.1, 16.5)
water <- data.frame(x,y)

# Generate regression line from data
eline <- lm(y~x, data=water)
anova(eline)

# Generate regression coefficients and HTs and CIs
summary(eline)
confint(eline, level=0.95)

# Plot
par(mfrow=c(2,2))
plot(eline)


#############################################################################
# Example 1
# Data from file
df = read.csv("ex11-003a.csv")
colnames(df) <- c('Barefoot', 'FW1','FW2','FW3','FW4','FW5')
attach(df)
x <- Barefoot
y <- FW5
data <- data.frame(x,y)
eline <- lm(y~x, data=data)

y_intercept <- eline$coefficients[1]
slope <- eline$coefficients[2]

# -- Calculate the equation of the least-squares linear regression line
#anova(eline)
#summary(eline)
#confint(eline, level=0.95)
paste('y = (', y_intercept, ') + (', slope, ')x')

# -- A physical therapist determines that her patient Jan has a range of ankle motion 
# -- of  7.26°  while barefoot. Predict Jan's range of ankle motion while wearing compression 
# -- hosiery and medical shoes,  ŷ  

motion <- 7.26
prediction <- y_intercept + ( slope * motion)

# -- Jan's actual range of ankle motion while wearing compression hosiery and medical shoes is 9.79
9.79-prediction

#############################################################################
# Example 2

# y=−5.399+5.790x.
# -- Suppose  o=1.5.  Find the probability that an observed value of  Y  is more than  19  
# -- when  x=4.  Round your answer to four decimal places.

# -- P(Y > 19) = 1 - P(Y < 19)
y_intercept <- -5.399
slope <- 5.790
target_y <- 19
target_x <- 4
o <- 1.5
z <- (target_y - ( y_intercept + slope*target_x ))/o
answer <- 1 - pnorm(z)
