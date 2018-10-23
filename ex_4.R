library(stats4)       # base package; use for mle()
library(ggplot2)      # Graphing package (sweet)
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation

# ===== Simulation settings ===== #
T_ <- 100   #time periods
n  <- 10000   # simulations

# True alpha-value 
alpha_0 <- 0.3

# Fix random numbers
# set.seed(12345)

# Initialisation
sigma2 <- matrix(0, n, T_)
x      <- matrix(0, n, T_)
stat   <- rep(0, n)

# Random number generation
z <- rnorm(n = n * T_, 0, 1)
z <- matrix(z, n, T_)

for (t in 2:T_){
  sigma2[, t] <- 1 + alpha_0 * x[, t-1] ** 2
  x[, t]      <- sqrt(sigma2[, t]) * z[, t]
}

Y <- x ** 2 - 1          #sig2 = 1 + alpha_0 * x^2 + e
X <- x[, -T_] ** 2
X <- cbind(rep(0, n), X)

alpha_OLS <- rowSums(Y * X) / rowSums(X * X)

stat <- sqrt(T_) * (alpha_OLS - alpha_0)

ggplot(NULL) +
  geom_histogram(aes(x = stat, y = ..density..), 
                 col = 'black', fill = 'white', bins = 50)

ggplot(NULL, aes(sample = stat)) +
  stat_qq() +
  stat_qq_line(aes(col = 'red'), show.legend = F) +
  ggtitle("Normal Q-Q Plot")