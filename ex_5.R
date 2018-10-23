library(stats4)       # base package; use for mle()
library(ggplot2)      # Graphing package (sweet)
library(quantmod)     # Get stock data
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation

# =================================== #
# ======= SP500 stock data ========== #
# =================================== #

getSymbols("SPY", 
           from = '2010-01-02', 
           to = '2015-09-17', 
           src = 'yahoo',
           adjust = TRUE)

spy <- log(SPY$SPY.Adjusted)
ret <- (spy - lag(spy, 1))[-1]

# =================================== #

# =================================== #
# ======= Likelihood function ======= #
# =================================== #

arch_llhood <- function(theta, y){
  end   <- length(y)
  omega <- abs(theta[1])
  alpha <- abs(theta[2])
  
  sig2  <- omega + alpha * y[1:(end - 1)] ** 2
  
  - 0.5 * sum( - (log(sig2) + y[2:end] ** 2 / sig2))
}

# =================================== #

# =================================== #
# ======= Maximisation problem ====== #
# =================================== #

# Initial parameters
theta <- c(0.05, 0.5)

# Maximisation
fit <- optim(theta, arch_llhood, y = ret, method = 'BFGS')


# Finding std errors and interval for parameters
fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
upper       <- fit$par + 1.96 * std_errors
lower       <- fit$par - 1.96 * std_errors
interval    <- data.frame(value = fit$par,
                          upper = upper,
                          lower = lower)

interval
# ================================================= #
