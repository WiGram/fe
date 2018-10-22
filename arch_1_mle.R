# =================================== #
# ===== Simulate ARCH(1) series ===== #
# =================================== #

#' Arch()
#' x      <- sig_t * z_t
#' sig2_t <- omega + alpha * x_(t-1)^2
#' x_0 given
#' z ~ N(0,1)

# =================================== #
options(scipen=999)

periods <- 100

x     <- rep(1, periods)
z     <- rnorm(periods, 0, 1)
omega <- 0.5
alpha <- 0.7
sig2 <- rep(NA, periods)
# sig2_t <- omega + alpha * x_(t-1)

for (t in 2:periods){
  t <- 1
  sig2[t] <- omega + alpha * x[t-1] ** 2
  x[t]    <- sqrt(sig2[t]) * z[t]
}
# =================================== #

# ======= Likelihood function ======= #
llf  <- function(theta, y){
  omega <- theta[1]
  alpha <- theta[2]
  n     <- length(y)
  
  sig2  <- omega + alpha * lag(y,1) ** 2

  p1    <- log(2 * pi * sig2)
  p2    <- y ** 2 / sig2
  
  llog <- - 0.5 * sum(p1 + p2) / n
  
  return(llog)
}

# ======= Maximisation problem ====== #

# Initial parameters
omega_start <- 0.5
alpha_start <- 0.7

theta <- c(omega_start,
           alpha_start)

# Maximisation
fit <- optim(theta, llf, y = x, hessian = TRUE)

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
