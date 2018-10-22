dev.off()
rm(list = ls())
cat("\014")
options(scipen = 999)

# ================================================= #
# ================= Exercise 2 ==================== #
# ================================================= #

# Simulate y = 0.5 + 0.8 * x + eps,
# where x ~ U(0,1) and eps ~ N(0,1)

# ===== Model parameters ===== #
sims <- 500000
mean <- 0
sd   <- 1
min  <- 0
max  <- 1
b0   <- 0.5
b1   <- 0.8

# First approach - ptm for starting time
# ============================================== #
ptm   <- proc.time()

eps   <- rnorm(n = sims, mean = mean,  sd = sd)
x     <- runif(n = sims, min = min,    max = max)

y     <- b0 + b1 * x + eps

reg1  <- lm(y ~ x)
time1 <- proc.time() - ptm
# ============================================== #

# second approach - again, start timing with ptm
# ============================================== #
ptm <- proc.time()

y2   <- rep(0, sims)
x2   <- rep(0, sims)
eps2 <- rep(0, sims)

for (i in 1:sims){
  # i <- 1
  eps2[i] <- rnorm(n = 1, mean = mean, sd = sd)
  x2[i]   <- runif(n = 1, min = min,   max = max)
  
  y2[i]   <- b0 + b1 * x2[i] + eps2[i]
}

reg2  <- lm(y2 ~ x2)
time2 <- proc.time() - ptm
# ============================================== #

# ===== Comparison ============================= #
paste0("Method 1: Time elapsed: ", round(time1[3], 4), sep = "")
paste0("Method 2: Time elapsed: ", round(time2[3], 4), sep = "")
# ============================================== #

# ============================================== #
# ===== Analyse coefficients from method 1 ===== #
# ============================================== #

# The coefficients are found in either
# method below:

X <- cbind(1, x)

# (1)
reg_a <- lm(y ~ x)
eps_a <- reg_a$residuals
var_a <- 1 / (sims - 2) * t(eps_a) %*% eps_a
cov_a <- var_a[1,1] * solve(t(X) %*% X)

# (2)
reg_b <- solve(t(X) %*% X) %*% (t(X) %*% y)
eps_b <- y - reg_b[1,] - reg_b[2,] * x
var_b <- 1 / (sims - 2) * t(eps_b) %*% eps_b
cov_b <- var_b[1,1] * solve(t(X) %*% X)

reg_a
reg_b
cov_a
cov_b

# ============================================== #
# ===== Analyse coefficients from method 2 ===== #
# ============================================== #

X2    <- cbind(1, x2)
eps_2 <- reg2$residuals
var_2 <- 1 / (sims - 1) * t(eps_2) %*% eps_2
cov_2 <- var_2[1,1] * solve(t(X2) %*% X2)

reg2
cov_2

# ============================================== #

#' Conclusion:
#' Whether done in a for loop or by vectorisation,
#' both methods seem to produce the same results.
#' Vectorisation is considerably faster, which
#' is especially fruitful when doing many simulations.
#' 
#' Finally, the lm method (a) can be done manually (b)
#' to produce the exact same results.

# ================================================= #
# ================= Exercise 3 ==================== #
# ================================================= #

# * Simulate x ~ N(mu = 0.5, sd = 0.4)
# * Estimate parameters using Maximum Likelihood
# * Use normal distribution for the estimation,
#   in accord with x being normally distributed.

# ===== Model parameters ======== #
n    <- 500
mean <- 0.5
sd   <- 0.4

mu_start <- 0
sd_start <- 1

# ===== Simulation of x  ======== #
x <- rnorm(n = n, mean = mean, sd = sd)

# ===== Likelihood function ===== #
llf  <- function(theta, y){
  mu <- theta[1]
  sd <- theta[2]
  n  <- length(y)
  
  cons <- log(2 * pi * sd ** 2)
  summ <- sum(((y - mu) / sd) ** 2) 
  
  llog <- - 0.5 * (n * cons + summ)
  
  return(-llog)
}

# ===== Maximisation problem ==== #

# Initial parameters
theta <- c(mu_hat = mu_start,
           sd_hat = sd_start)

# Maximisation
fit <- optim(theta, llf, y = x, hessian = TRUE)

# Finding std errors and interval for parameters
fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
upper       <- fit$par + 1.96 * std_errors
lower       <- fit$par - 1.96 * std_errors
interval    <- data.frame(value = fit$par,
                          lower = lower,
                          upper = upper,
                          st_er = std_errors)

interval

# Analyse the estimated error term
eps_hat <- x - interval$value[1]

ggplot(NULL, aes(sample = eps_hat)) +
  stat_qq() +
  stat_qq_line(aes(col = 'red'), show.legend = F) +
  ggtitle("Normal Q-Q Plot")

qqnorm(y = eps_hat, 
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(eps_hat, col = 2)

# ===== Conclusion ========================= #
#' The derived parameters are fairly close.
#' The derived std errors are acceptably
#' small. 
#' 
#' The QQ plots confirm normality of the error
#' term.
#' 
#' Increasing amount of observations from 500
#' to 50,000 almost perfects the fit.
#' 
#' The computing time is still fast. Plotting
#' becomes slower.

# ================================================= #