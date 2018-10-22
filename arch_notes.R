library(ggplot2)

#' ARCH(1) process:
#' x_t     <- vol_t * z_t
#' vol_t^2 <- sig2 + alph * x_(t-1)^2
#' 
#' x_0 given, e.g. 0
#' z_t ~ N(0,1)
#' alph in (0,1)

# ===== Model parameters ===== #
sims  <- 100000
mean  <- 0
sd    <- 1
sig2  <- 0.2
alph  <- 0.7
df    <- 3

# ===== Simulation =========== #
# z <- rnorm(n = sims, mean = mean, sd = sd)
z <- rt(n = sims, df = df)
x <- rep(0, sims)

for (i in 2:sims){
  vol2 <- sig2 + alph * x[i-1] ** 2
  x[i] <- sqrt(vol2) * z[i]
}

# ===== Plotting ============= #
ggplot(NULL, aes(x = 1:sims, y = x)) +
  geom_line() +
  xlab("") + ylab("")
# ============================ #

# ============================ #
# ===== Analysis ============= #
# ============================ #

# Mean: expected to b zero
mu_exp <- mean
mu_hat <- mean(x)
paste0("Expected mu: ", mu_exp,
       ", actual mu: ", mu_hat,
       sep = "")

# Sd: expected to be sig2 / (1 - alph)
sd_exp <- sig2 / (1 - alph)
sd_hat <- sd(x)
paste0("Expected sd: ", sd_exp,
       ", actual sd: ", sd_hat,
       sep = "")

# ============================ #
# ===== Real data analysis === #
# ============================ #

# Quantmod provides a way to "getSymbols"
library(quantmod)

# ================================== #
# ========= Collecting data ======== #
# ================================== #

# Our interest is in the SP500 index
getSymbols("SPY", 
           from = '2010-01-02', 
           to = '2015-09-17', 
           src = 'yahoo',
           adjust = TRUE)

spy <- SPY$SPY.Adjusted

# Take log differences, remove the NA
lret <- (log(spy) - log(lag(spy, k = 1)))[-1]

# ===== Likelihood function ===== #
llf <- function(theta, y){
  mu <- theta[1]
  sd <- theta[2]
  n  <- length(y)
  
  cons <- log(2 * pi * sd ** 2)
  summ <- sum(((x - mu) / sd) ** 2) 
  
  llog <- - 0.5 * (cons + summ / n)
  
  return(-llog)
}

# ===== Maximisation problem ==== #

# Initial parameters
mu_start <- 0
sd_start <- 1

theta <- c(mu_hat = mu_start,
           sd_hat = sd_start)

# Maximisation
fit <- optim(theta, llf, y = lret, hessian = TRUE)

# Finding std errors and interval for parameters
fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
upper       <- fit$par + 1.96 * std_errors
lower       <- fit$par - 1.96 * std_errors
interval    <- data.frame(value = fit$par,
                          upper = upper,
                          lower = lower)

interval

#' Conclusion:
#' Estimated model parameters have unreal
#' estimation interval and unbelievable
#' estimates. The model seems to be a bad fit;
#' or my method does not work.

library(stats4)
mle(minuslogl = F, start = theta, method = llf, y = lret)
