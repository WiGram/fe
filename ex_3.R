dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation
library(stats4)       # base package; use for mle()
# install.packages("ggplot2") # if not already installed
library(ggplot2)      # Graphing package (sweet)

# Exercise sheet no. 3

# ============================================= #
# ===== Exercise 3 ============================ #
# ============================================= #

# 0. Initial parameters
# 1. ARCH(1)
#    x = sd * z, sd2 = s2 + a * x_t-1 ** 2
# 2. Asymmetrical ARCH(1)
#    x = 

# ============================================= #
# 0. Initial, universal parameters

T_      <- 1000
z       <- rnorm(T_, 0, 1)
sig2    <- 0.2
periods <- seq(0, T_, length.out = T_ - 1)
# ============================================= #

# ============================================= #
# 1. x = sd * z, sd2 = s2 + a * x_t-1 ** 2
x    <- rep(0, T_)
s2_s <- rep(0, T_)
a    <- 0.8

for (t in 2:T_){
  s2_s[t] <- sig2 + a * x[t-1] ** 2
  x[t] <- sqrt(s2_s[t]) * z[t]
}
# ============================================= #

# ============================================= #
# 2.
x    <- rep(0, T_)
s2_a <- rep(0, T_)
a_n  <- 0.9
a_p  <- 0.7

for (t in 2:T_){
  i_n  <- ifelse(x[t-1] < 0, 1, 0)
  i_p  <- ifelse(x[t-1] > 0, 1, 0)
  s2_a[t] <- sig2 + i_n * a_n * x[t-1] ** 2 + 
    i_p * a_p * x[t-1] ** 2
  x[t] <- sqrt(s2_a[t]) * z[t]
}
# ============================================= #

# ============================================= #
# 3. Studying the difference
diff <- -(s2_a[-1] - s2_s[-1])

# Notice the sign on diff, due to its definition
mu_diff <- mean(-diff)
sd_diff <- sd(-diff)
length <- length(diff)

lower  <- mu_diff - 1.96 * sd_diff / sqrt(length)
upper  <- mu_diff + 1.96 * sd_diff / sqrt(length)
t_stat <- mu_diff / sd_diff

print(c('estimate' = mu_diff, 'lower' = lower,
        'upper' = upper, 't-stat' = t_stat))
# ============================================= #

# ============================================= #
# 4. Plotting
ggplot(NULL) +
  geom_line(aes(x = periods, y = s2_s[-1], 
                col = 'symmetrical')) + 
  geom_line(aes(x = periods, y = s2_a[-1], 
                col = 'asymmetrical'),
            alpha = 0.5) +
  geom_line(aes(x = periods, y = diff, 
                col = 'difference')) +
  labs(x = "", y = "")
# ============================================= #

# ============================================= #
# ===== Conclusion ============================ #
# ============================================= #

#' The news impact curve is strongets for the
#' asymmetrical ARCH(1) model. This seems to
#' be robust to any average of a equalling
#' that alpha in the symmetrical ARCH(1) model.

# ============================================= #

# ============================================= #
# ===== Exercise 4 ============================ #
# ============================================= #

#' The model here is a threshold auto-regressive
#' model.

T_    <- 5000
z     <- rnorm(T_, 0, 1)
y     <- rep(0, T_)
rho_1 <- 0.8
rho_2 <- 0.2
lambd <- 1.5
rho   <- 0

for (t in 2:T_){
  rho = ifelse(abs(y[t-1]) > lambd, rho_1, rho_2)
  y[t] = rho * y[t-1] + z[t]
}

ggplot(NULL) +
  geom_point(aes(x = y[-T_], y = y[-1]), 
             alpha = 0.5, 
             shape = 'o', 
             size = 3, 
             col = 'blue') +
  labs(x = '', y = '')
