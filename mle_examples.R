dev.off()
rm(list = ls())
cat("\014")
# install.packages("ggplot2")
library(ggplot2)

# ========================================== #
# ===== Global parameters ================== #
# ========================================== #

periods <- 100

# ========================================== #

# ========================================== #
# ===== Standard normal max likelihood ===== #
# ========================================== #

y     <- rep(1, periods)
eps_y <- rnorm(periods, 0, 1)
x     <- runif(periods, 0, 1)

b_0   <- 0.4
b_1   <- 1.2

y <- b_0 + b_1 * x + eps_y

# Likelihood function to optimise over
llf <- function(theta, x, y){
  # x <- x
  # y <- y
  
  b0 <- theta[1]
  b1 <- theta[2]
  sd <- theta[3]
  T  <- length(y)
  
  mu <- b0 + b1 * x
  s2 <- sd ** 2
  
  c    <- log(2 * pi * s2)
  summ <- sum((y - mu) ** 2 / s2)
  
  llog <- -0.5 * (T * c + summ)
  
  return(-llog)
}

# Starting guess of parameters to be estimated
b0_start <- 0.3
b1_start <- 0.5
sd_start <- 0.9

theta <- c(b0_hat = b0_start,
           b1_hat = b1_start,
           sd_hat = sd_start)

# Optimisation of theta parameters
fit   <- optim(par      = theta, 
               fn       = llf, 
               x = x, y = y, 
               hessian  = TRUE,
               method   = "BFGS")

# Finding std.ers and interval for parameters
fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
lower       <- fit$par - 1.96 * std_errors
upper       <- fit$par + 1.96 * std_errors
interval    <- data.frame(value   = fit$par,
                          lower   = lower,
                          upper   = upper,
                          sdt_err = std_errors)
interval

# Study the produced error term
eps_hat <- y - fit$par[1] - fit$par[2] * x

ggplot(NULL, aes(sample = eps_hat)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")

# ===== Conclusion ========================= #
#' The derived parameters are somewhat close,
#' b1 being the most imprecisely estimated.
#' The derived std errors are fairly prcise.
#' In conclusion the implementation of mle is
#' working.
#' The QQ plots confirms normality of the
#' error term.

# ========================================== #
# ===== Random normal max likelihood ======= #
# ========================================== #

# Some random number x <- mu + eps(0,1)
x     <- rnorm(n = periods, mean = 0, sd = 1)

llf_sn <- function(theta, y){
  mu <- theta[1]
  sd <- theta[2]
  T  <- length(y)
  
  sig2 <- sd ** 2
  
  c    <- log(2 * pi * sig2)
  summ <- sum((y - mu) ** 2 / sig2)
  
  llog <- -0.5 * (T * c + summ)
  
  return(-llog)
}

theta <- c(mu_hat = mu_start,
           sd_hat = sd_start)

fit   <- optim(theta, llf_sn, y = x, 
               hessian = TRUE,
               method = "BFGS")

fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
lower       <- fit$par - 1.96 * std_errors
upper       <- fit$par + 1.96 * std_errors
interval    <- data.frame(value   = fit$par,
                          lower   = lower,
                          upper   = upper,
                          sdt_err = std_errors)
interval

# Study normality of the empirical error term
eps_hat <- x - interval$value[1]

ggplot(NULL, aes(sample = eps_hat)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")
  
qqnorm(y = eps_hat, 
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(eps_hat, col = 2)

# ===== Conclusion ========================= #
#' The derived parameters are fairly close.
#' The derived std errors are acceptably
#' small. In conclusion the idea of mle is
#' working, but the hessian produces intervals
#' that are unacceptably wide.
#' The QQ plots confirm normality of the error
#' term.

# ========================================== #
# ===== AR(1) maximum likelihood =========== #
# ========================================== #

y   <- rep(1, periods)
eps <- rnorm(periods, 0, 1)

mu  <- 0.4
sd  <- 0.3
rho <- 0.5

for (t in 2:periods){
  y[t] <- mu + rho * y[t-1] + eps[t]
}

llf_ar <- function(theta, y){
  mu  <- theta["mu_hat"]
  sd  <- theta["sd_hat"]
  rho <- theta["rho_hat"]
  T   <- length(y)
  
  #' The first y has cond expectation y_0
  #' The others have mu + rho * y(t-1)
  #' Cond var is always sig2_eps
  mu_1 <- mu / (1 - rho)
  mu_2 <- mu + rho * lag(y[2:T],1)
  s2_1 <- sd ** 2 / (1 - rho ** 2)
  s2_2 <- sd ** 2
  
  c_1  <- log(2 * pi * s2_1)
  c_2  <- log(2 * pi * s2_2)

  p1 <- c_1 + (y[1] - mu_1) ** 2 / s2_1
  p2 <- (T-1) * c_2 + sum( (y[2:T] - mu_2) ** 2 / s2_2)
  
  llog <- -0.5 * (p1 + p2)
  
  return(-llog)
}

theta <- c(mu_hat  = 0.4, 
           sd_hat  = 0.3, 
           rho_hat = 0.8)

ui    <- rbind(c(0, 0, 1), c(0, 0, -1))
ci    <- c(0, -1)

fit   <- optim(par = theta, fn = llf_ar, y = y,
               method = 'L-BFGS-B',
               lower = c(-10, 0.01, 0.01),
               upper = c( 10, 2.00, 0.99),
               hessian = TRUE)

fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
lower       <- fit$par - 1.96 * std_errors
upper       <- fit$par + 1.96 * std_errors
interval    <- data.frame(value   = fit$par,
                          lower   = lower,
                          upper   = upper,
                          sdt_err = std_errors)
interval

# Study normality of the empirical error term
s2_hat  <- c(interval$value[2] / (1 - interval$value[3] ** 2), 
             interval$value[2] + interval$value[3] * lag(x[2:periods], 1) ** 2)
eps_hat <- x[2:periods] - interval$value[1] - interval$value[3] * lag(x[2:periods], 1)

ggplot(NULL, aes(sample = eps_hat)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")

# ===== Conclusion ========================= #
#' The derived parameters are not close.
#' The QQ plot rejects normality of the error
#' term.
#' It is undeniable that the function is not
#' correct.

# ========================================== #