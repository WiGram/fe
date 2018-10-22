t <- 100
n <- 10000

alpha_0 <- 0.2

set.seed(14)

sigma2 <- matrix(0, t, n)
x      <- matrix(0, t, n)
stat   <- rep(0, n)

z <- rnorm(n = n * t, 0, 1)
z <- matrix(z, t, n)

for (i in 2:t){
  sigma2[t] <- 1 + alpha_0 * x[t-1]
  x[t]      <- sqrt{sigma2[t]z[t]}
}

y <- x ** 2 - rep(1, t)
x <- lag(x ** 2, 1)

alpha_OLS <- sum(y * x) / sum(x*x)

stat <- sqrt(t) * (alpha_OLS - alpha_0)