library(stats4)       # base package; use for mle()
library(ggplot2)      # Graphing package (sweet)
library(quantmod)     # Get stock data
library(numDeriv)     # Achieve score and information
library(matrixStats)  # Does e.g. quantiles over cols
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation

# =================================== #
# ======= SP500 stock data ========== #
# =================================== #

spy <- readxl::read_xlsx('C:/Users/wigr11ab/Dropbox/KU/K3/FE/Exercises/SP500.xlsx')
spy <- spy[spy$Date > '2010-01-04',] # correct the dataset
spy$Date <- as.Date(spy$Date)
ret <- as.numeric(spy$`log-ret_x100`[-1]) #rm NA and turn numeric

# =================================== #

# =================================== #
# ======= Likelihood function ======= #
# =================================== #

gjr_arch <- function(theta, y){
  omega <- abs(theta[1])
  kappa <- abs(theta[2])
  gamma <- abs(theta[3])
  
  y_lag <- y[1:(length(y)-1)]
  y     <- y[2:length(y)]
  
  s2 <- omega + 
    kappa * y_lag ** 2 + 
    (y_lag < 0) * gamma * y_lag ** 2
  
  z <- y / sqrt(s2)
  
  lik <- -0.5 * (log(s2) + z ** 2)
  
  - sum(lik)
}

a_arch <- function(theta, y){
  sig2    <- abs(theta[1])
  alpha_n <- abs(theta[2])
  alpha_p <- abs(theta[3])
  
  y_lag <- y[1:(length(y)-1)]
  y     <- y[2:length(y)]
  
  s2 <- sig2 + 
    alpha_n * (y_lag < 0) * y_lag ** 2 +
    alpha_p * (y_lag > 0) * y_lag ** 2
  
  z <- y / sqrt(s2)
  
  lik <- -0.5 * (log(2 * pi) + log(s2) + z ** 2)
  
  -sum(lik)
}

# =================================== #

# =================================== #
# ======= Maximisation problem ====== #
# =================================== #

# Initial parameters
theta <- c(0.88, 0.05, 0.16)

# Maximisation
# fit <- optim(theta, a_arch, y = ret, hessian = TRUE)
# fit$par
fit <- optim(theta, gjr_arch, y = ret, hessian = T)
j <- numDeriv::jacobian(gjr_arch, x = fit$par, y = ret)
s <- t(j) %*% j
h <- numDeriv::hessian(gjr_arch, x = fit$par, y = ret)
i <- solve(h)
se <- sqrt(diag(i))
robust_se <- sqrt(diag(i %*% s %*% i))

interval    <- data.frame(value = fit$par,
                          se    = se,
                          r_se  = robust_se,
                          t_val = fit$par / se,
                          r_tval= fit$par / robust_se)
interval
# ================================================= #

o  <- fit$par[1]
k  <- fit$par[2]
g  <- fit$par[3]

s2 <- o + 
  k * ret[1] ** 2 + 
  g * ret[1] ** 2 * (ret[1] < 0)
s2 <- rep(s2, length(ret))
for (t in 2:length(ret)){
  s2[t] <- o + 
    k * ret[t-1] ** 2 +
    g * ret[t-1] ** 2 * (ret[t-1] < 0)
}

z <- ret / s2

ggplot(NULL, aes(sample = z)) +
  stat_qq() + stat_qq_line() +
  labs(main = "Normal Q-Q Plot")

ggplot(NULL, aes(x = z)) +
  geom_histogram(aes(y = ..density..),
                 col = 'black',
                 fill = 'white',
                 bins = 50) +
  labs(x = '') +
  stat_density(geom = 'line', 
               aes(colour = 'Empirical density')) +
  stat_function(fun = dnorm,
                args = list(mean = mean(z),
                            sd = sd(z)),
                aes(color = 'standard normal density')) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.title = element_blank())

ggplot(NULL) +
  geom_line(aes(x = 1:length(z), y = z))

# ============================================= #
# ===== Exercise 4 ============================ #
# ============================================= #

filt_hist_sim <- function(r, theta, m, sims, a){
  # start_date <- 2014-11-28
  # r <- spy$`log-ret_x100`[spy$Date == 2014-11-28]
  
  o <- theta[1]
  k <- theta[2]
  g <- theta[3]
  
  v  <- matrix(0, nrow = sims, ncol = m)
  z1 <- rnorm(m * sims)
  z1 <- matrix(z1, nrow = sims, ncol = m)
  z2 <- rnorm(m * sims)
  z2 <- matrix(z2, nrow = sims, ncol = m)
  
  # t = 1 is actual data, hence t = 2 : t+1
  for (t in 1:m){
    r0 <- r[t]
    s1 <- sqrt(o + k * r0 ** 2 + g * (r0 < 0) * r0 ** 2)
    r1 <- s1 * z1[,t]
    s2 <- sqrt(o + k * r1 ** 2 + g * (r1 < 0) * r1 ** 2)
    r2 <- s2 * z2[,t]
    v[,t] <- r1 + r2
  }
  
  v2 <- colQuantiles(v, probs = a)
  v2
}

theta <- c(omega = 0.88142,
           kappa = 0.051045,
           gamma = 0.16018)
idx   <- spy$Date > '2014-11-27'
r     <- as.numeric(spy$`log-ret_x100`[idx])
mat   <- length(spy$Date[idx])
sims  <- 1000000
a     <- 0.05

var <- filt_hist_sim(r, theta, mat-2, sims, a)

# For plotting
x_ser <- spy$Date[idx]
rets  <- as.numeric(spy$`log-ret_x100`[idx])
r2    <- rets[2:(mat-1)] + rets[3:mat]

ggplot(NULL) + 
  geom_line(aes(x = x_ser[-c(1,2)],
                y = var),
            colour = 'red') +
  geom_line(aes(x = x_ser[-c(1,2)],
                y = r2)) +
  labs(x = 'Time', y = 'Percent')

df <- data.frame(x_ser[-c(1,2)], r2, var)
df$breach <- as.numeric(df$r2 < df$var)
sum(df$breach) / length(df$r2)

# Kupiec's test
t  <- length(df$breach)
t1 <- sum(df$breach)
t0 <- t - t1
p <- t1 / t

-2* log((1-a)**t0*a**t1 / ((1 - p)**t0 * p * t1))
