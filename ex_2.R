dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation
library(stats4)       # base package; use for mle()
library(ggplot2)      # Graphing package (sweet)
library(dplyr)

# Exercise week 2
path <- "C:/Users/wigr11ab/Dropbox/KU/K3/FE/Exercises/USspreadraw.xls"
# ============================================= #
# ===== Loading pre-existing data ============= #
# ============================================= #

# The read and specification to spread
data <- readxl::read_xls(path)
spread  <- data$Y_uncor
dates <- data$X__1 %>% 
  paste0('-1', sep = '') %>% 
  as.Date()

n         <- length(spread)
n_neg     <- length(spread[spread < 0])
spread_50 <- spread[(n - 49): n]
spread_0  <- ifelse(spread < 0, 0, spread)
spread_2  <- spread ** 2
spread_c  <- cumsum(spread)
minimum   <- min(spread)
maximum   <- max(spread)
variance  <- var(spread_50)
average   <- mean(spread_50)

# Visualise data
ggplot(NULL) + 
  geom_line(aes(dates, spread)) +
  labs(x = '', y = 'spread')

ggplot(NULL) +
  geom_line(aes(dates, spread_0)) +
  labs(x = '', y = 'non-zero spread')

ggplot(NULL) + 
  geom_line(aes(dates, spread_2)) +
  labs(x = '', y = 'squared spread')

ggplot(NULL) + 
  geom_line(aes(dates, spread_c)) +
  labs(x = '', y = 'cummulated spread')

# ============================================= #
# ===== AR(1):y(t) = mu + rho y(t-1) + eps(t) = #
# ============================================= #

ar_function <- function(sig, mu, rho, periods){
  # y0 = 0
  y <- rep(0, periods)

  # Simulating white noise
  eps <- rnorm(periods, 0, sig)
  
  # Determine path of dependent variable
  for (t in 2:periods){
    y[t] <- mu + rho * y[t-1] + eps[t]
  }
  
  return(y)
}

# ============================================= #

# ============================================= #
# ===== AR(1) maximum likelihood ============== #
# ============================================= #

# ============================================= #
# Approach 1: stats4::mle built-in function
llf_ar <- function(mu, sd, rho){
  end <- length(spread)
  
  mu_cond <- mu + rho * spread[1:(end - 1)]

  - sum(-0.5 * (log(sd ** 2) + ((spread[2:end] - mu_cond) / sd) ** 2))
}

#' Not on the likelihood function:
#' The first term should be supressed
#' The constant log(2 * pi) is omitted

theta <- list(mu = 5, sd = 1, rho = 0.05)

mle(llf_ar, start = theta, method = "L-BFGS-B")
# ============================================= #

# ============================================= #
# Approach 2: minimize more or less manually
llf_ar_optim <- function(theta, y){
  end <- length(y)
  
  mu  <- theta[1]
  sd  <- theta[2]
  rho <- theta[3]
  
  mu_cond <- mu + rho * y[1:(end - 1)]

  - sum(-0.5 * (log(sd ** 2) + ((y[2:end] - mu_cond) / sd) ** 2))
}

#' The first term should be suppressed, as
#' y0 is given and we do not know its distribution
#' Omit log(2 * pi) as it is just a constant.

# Define starting values
theta <- c(mu = 5, sd = 1, rho = 0.05)

# Compute parameters
fit   <- optim(par = theta, fn = llf_ar_optim, y = spread,
               method = 'L-BFGS-B',
               hessian = TRUE)
fit$par
# ============================================= #

# ============================================= #
# Compute standard errors
fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
t_stat      <- fit$par / std_errors
lower       <- fit$par - 1.96 * std_errors
upper       <- fit$par + 1.96 * std_errors
interval    <- data.frame(value   = fit$par,
                          lower   = lower,
                          upper   = upper,
                          std_err = std_errors,
                          t_stat  = t_stat)
interval
# ============================================= #

# ============================================= #
# Study normality of the estimated error term
mu_hat  <- interval$value[1]
sig_hat <- interval$value[2]
rho_hat <- interval$value[3]

# We need to standardise the residuals before plotting.
end     <- length(spread)
eps_hat <- (spread[2:end] - (mu_hat + rho_hat * spread[1:(end-1)])) / sig_hat

ggplot(NULL, aes(sample = eps_hat)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")

# ============================================= #
# ===== Conclusion ============================ #
# ============================================= #

#' Small standard deviations and plausible
#' values.
#' The residuals do not seem to be perfectly
#' normally distributed. Probably the spread 
#' does not follow an AR-process.

# ============================================= #

# ============================================= #
# ===== Generate similar AR-function ========== #
# ============================================= #

y <- ar_function(sig = sig_hat, mu = mu_hat,
                 rho = rho_hat, periods = length(spread))

ggplot(NULL, aes(x = 1:length(spread), y = y)) +
  geom_line(aes(col = 'Simulated data')) +
  geom_line(aes(y = spread, col = 'Spread data')) +
  labs(x = '', y = 'spread')

# ===== Testing MLE on a known process ======== #

# == Compute the parameters =================== #
theta <- c(mu_hat  = 5, 
           sd_hat  = 1, 
           rho_hat = 0.05)

fit2   <- optim(par = theta, 
               fn = llf_ar_optim, 
               y = y,
               method = 'L-BFGS-B',
               hessian = TRUE)
fit2$par

# == Compute standard errors ================== #
fisher_info <- solve(fit2$hessian)
std_errors  <- sqrt(diag(fisher_info))
t_stat      <- fit$par / std_errors
lower       <- fit$par - 1.96 * std_errors
upper       <- fit$par + 1.96 * std_errors
interval2   <- data.frame(value   = fit$par,
                          lower   = lower,
                          upper   = upper,
                          std_err = std_errors,
                          t_stat  = t_stat)
interval2

# == Compute the empirical error term ========= #
mu_hat2  <- interval2$value[1]
sig_hat2 <- interval2$value[2]
rho_hat2 <- interval2$value[3]

# == Standardise and plot ===================== #
end      <- length(y)
eps_hat2 <- (y[2:end] - (mu_hat2 + rho_hat2 * y[1:(end-1)])) / sig_hat2

ggplot(NULL, aes(sample = eps_hat2)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")

# ============================================= #
# ===== Conclusion ============================ #
# ============================================= #

#' Despite not getting perfectly normally
#' distributed residuals, we try to mimic the
#' behaviour of the data. We do not get a process
#' that is as beautifully hilly, but we do get
#' somewhat persistent movements. 
#' 
#' Testing the mle on a known AR process we get
#' an error term which is well behaved. Because
#' data are now simulated, small samples, e.g.
#' 350 might not have well-behaved residuals.
#' With 10000 simulated values, the residuals
#' become perfectly normal.

# ============================================= #

# ============================================= #
# ===== Equivalence between methods =========== #
# ============================================= #

# Preliminary parameters
mu1 <- mu_hat + rho_hat * spread[1:(length(spread) - 1)]
sd2 <- sig_hat ** 2
c   <- log(2 * pi * sd2)

# dnorm for cummulative distribution function
- sum( dnorm(x = spread[-1],
             mean = mu1, 
             sd = sig_hat, 
             log = TRUE))

# applying dnorm manually
- sum(-0.5 * (c + ((spread[2:length(spread)] - mu1) ** 2 / sd2)))

# ============================================= #
# ===== Conclusion ============================ #
# ============================================= #

#' If one forgets how to write the cummulative
#' distribution function, one can use dnorm

# ============================================= #

# ============================================= #
# ===== OLS Regression ======================== #
# ============================================= #
end <- length(spread)

head <- spread[2:end]
tail <- spread[1:end - 1]

lm <- lm(head ~ tail)

# ============================================= #

# Save coefficients
reg_mu  <- lm$coefficients[1]
reg_rho <- lm$coefficients[2]

# Alternative: lm$residuals
residuals <- spread[2:end] - reg_mu - reg_rho * spread[1:(end - 1)]

# Compute sigma
reg_sd  <- sd(residuals)

# Compare
interval$value
c(reg_mu, reg_sd, reg_rho)
