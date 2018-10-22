# Importance sampling examples
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation
library(stats4)       # base package; use for mle()
# install.packages("ggplot2") # if not already installed
library(ggplot2)      # Graphing package (sweet)

# ===== Exercise 3.1 ========================== #

# periods simulated
n <- 1000
set.seed(12345)

# Parameters
a <- phi <- 1

# Random number (n long)
z    <- rnorm(n = n, mean = 0, sd = 1)
test <- log(abs(phi + sqrt(a) * z))
mean <- rep(mean(test), n)
zero <- rep(0, n)
sd   <- sd(test) / sqrt(n)

ggplot(NULL) +
  geom_histogram(aes(x = z, y = ..density..), 
                 col = 'black', fill = 'white', bins = 50) + 
  geom_density(aes(x = z), col = 'blue') + 
  geom_vline(aes(xintercept = mean(test), col = 'mean')) +
  guides(color = guide_legend(title = ''))

# ===== Exercise 3.2 ========================== #

dar_process <- function(n, x0, phi, s2, a){
  z <- rnorm(n, 0, 1)
  x <- rep(x0, n)
  sig2 <- rep(1,n)
  for(t in 2:n){
    sig2[t] <- s2 + a * x[t-1] ** 2
    x[t]    <- phi * x[t-1] + sqrt(sig2[t]) * z[t]
  }
  x
}

n    <- 500
z    <- rnorm(n = n, mean = 0, sd = 1)
s2   <- phi <- 1
a    <- 0

x    <- rep(0, n)
sig2 <- rep(1, n)

for (t in 2:n){
  sig2[t] <- s2 + a * x[t-1] ** 2
  x[t]    <- phi * x[t-1] + sqrt(sig2[t]) * z[t]
}

ggplot(NULL)+
  geom_line(aes(x = 1:n, y = x)) + 
  labs(x = 'period', y = '')

x    <- rep(0, n)
sig2 <- rep(1, n)
a    <- 1

# loop to recursively define x
for (t in 2:n){
  # t <- 2
  sig2[t] <- s2 + a * x[t-1] ** 2
  x[t]    <- phi * x[t-1] + sqrt(sig2[t]) * z[t]
}

ggplot(NULL)+
  geom_line(aes(x = 1:n, y = x)) +
  labs(x = 'period', y = '')


# ============================================= #
# ===== Exercise 5 ============================ #
# ============================================= #

# 5.1 Illustration
data <- readxl::read_xlsx('C:/Users/wigr11ab/Dropbox/KU/K3/FE/Exercises/Cryptocurrency.xlsx')

ggplot(data) +
  geom_line(aes(x = data$date, y = data$xbtusd_detrend)) +
  labs(x = 'Date', y = 'Value in USD')

# 5.2 Estimation
dar_likelihood <- function(theta, y){
  phi   <- theta[1]
  vol2  <- theta[2]
  alpha <- theta[3]
  cons  <- log(2 * pi)
  
  n   <- length(y)
  
  e  <- y[2:n] - phi * y[1:(n - 1)]
  s2 <- vol2 + alpha * y[1:n - 1] ** 2
  
  llh <- -0.5 * sum(cons + log(s2) + e ** 2/s2)
  
  -llh
}

ts <- data$xbtusd_detrend
theta <- c(phi = 0.9, vol2 = 0.5, alpha = 0.2)
-dar_likelihood(theta, ts)

fit <- optim(par = theta, fn = dar_likelihood, y = ts, hessian = TRUE)

fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
t_val       <- fit$par / std_errors

x0    <- data$xbtusd_detrend[1]
phi   <- fit$par['phi']
alpha <- fit$par['alpha']
vol2  <- fit$par['vol2']

set.seed(12345)
dar <- dar_process(length(y), x0, phi, vol2, alpha)

ggplot(NULL) +
  geom_line(aes(x = data$date, y = dar, col = 'simulation')) +
  geom_line(aes(x = data$date, y = data$xbtusd_detrend, col = 'bitcoin')) + 
  labs(x = '', y = 'Value in USD') +
  guides(color = guide_legend(title = ''))


# ===== Testing strict stationarity =========== #
# Random number (n long)
z    <- rnorm(n = 1000, mean = 0, sd = 1)
test <- log(abs(phi + sqrt(alpha) * z))
mean <- rep(mean(test), n)
zero <- rep(0, n)
sd   <- sd(test) / sqrt(n)

ggplot(NULL) +
  geom_histogram(aes(x = z, y = ..density..), 
                 col = 'black', fill = 'white', bins = 50) + 
  geom_density(aes(x = z), col = 'blue') + 
  geom_vline(aes(xintercept = mean(test), col = 'mean')) +
  guides(color = guide_legend(title = ''))
