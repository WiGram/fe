dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation
library(ggplot2)      # Graphing package (sweet)

# ============================================= #
# ===== ARCH(1)-process ======================= #
# ============================================= #

# ============================================= #
# Initial parameters

n <- 10000
omega <- 1
alpha <- 0.9
# ============================================= #
# Random numbers
z <- rnorm(n)

# Time series
x <- rep(1, n)
for (t in 2:n){
  sig2 <- omega + alpha * x[t-1] ** 2
  x[t] <- sqrt(sig2) * z[t]
}

# Plotting
plot(x, type = 'l')

# ============================================= #
# ===== Log likelihood function =============== #
# ============================================= #

arch_llhood <- function(theta, y){
  end   <- length(y)
  omega <- theta[1]
  alpha <- theta[2]
  
  sig2  <- (omega + alpha * y[1:(end - 1)] ** 2)
  
  - sum( - 0.5 * (log(sig2) + y[2:end] ** 2 / sig2))
}

# ============================================= #
# ===== Maximum Likelihood Estimation ========= #
# ============================================= #

# Starting guess
theta <- c(omega = 1, alpha = 0.9)

# Minimising negative (maximising)
fit <- optim(par = theta, fn = arch_llhood, y = x,
             method = 'L-BFGS-B', hessian = TRUE)
fit$par

# ============================================= #
# ===== Determine standard errors ============= #
# ============================================= #

fisher_info <- solve(fit$hessian)
std_errors  <- sqrt(diag(fisher_info))
lower       <- fit$par - 1.96 * std_errors
upper       <- fit$par + 1.96 * std_errors
interval    <- data.frame(value   = fit$par,
                          lower   = lower,
                          upper   = upper,
                          std_err = std_errors)
interval

# ============================================= #
# ===== Fit residuals to check normality ====== #
# ============================================= #

# estimated parameters
omega_hat <- interval$value[1]
alpha_hat <- interval$value[2]

end <- length(x)
sig2_hat <- omega_hat + alpha_hat * x[1:(end-1)] ** 2

# Derived residuals
eps_hat <- x[2:end] / sqrt(sig2_hat)

# Q-Q Plot
ggplot(NULL, aes(sample = eps_hat)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")

# ============================================= #