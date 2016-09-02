#=========================================================
# Written by Brian A. Fannin
# 

# Generate a random set of data with a linear relationship
N <- 100
B0 <- 5
B1 <- 1.5

set.seed(1234)
e <- rnorm(N, mean = 0, sd = 1)
X <- rep(seq(1, 10), 10)

Y <- B0 + B1 * X + e

plot(X, Y)

myFit <- lm(Y ~ X)

plot(myFit)

dfBasic <- data.frame(Y, X, e)

dfBasic$Prediction <- predict(myFit)

hist(exp(Y))
