library(stats4)       # base package; use for mle()
library(ggplot2)      # Graphing package (sweet)
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation

# ===== Simulation settings ===== #
T_ <- 100   #time periods
n  <- 10000   # simulations

# True alpha-value 
alpha_0 <- c(0.2, 0.4, 0.8, 1.2)
stat   <- matrix(0, length(alpha_0), n)

# Fix random numbers
# set.seed(12345)

# Initialisation
sigma2 <- matrix(0, n, T_)
x      <- matrix(0, n, T_)


# Random number generation
for (a in alpha_0){
  i <- which(alpha_0 == a)
  z <- rnorm(n = n * T_, 0, 1)
  z <- matrix(z, n, T_)
  
  for (t in 2:T_){
    sigma2[, t] <- 1 + a * x[, t-1] ** 2
    x[, t]      <- sqrt(sigma2[, t]) * z[, t]
  }
  
  Y <- x ** 2 - 1          #sig2 = 1 + alpha_0 * x^2 + e
  X <- x[, -T_] ** 2
  X <- cbind(rep(0, n), X)
  
  alpha_OLS <- rowSums(Y * X) / rowSums(X * X)
  
  stat[i, ] <- sqrt(T_) * (alpha_OLS - a)
  
  title1 <- paste0('Density plot for alpha = ', a, sep = '')
  title2 <- paste0('Normal Q-Q Plot for alpha = ', a, sep = '')
  
  file1 <- paste0(gsub("\\.", "_", a), '_density.png')
  file2 <- paste0(gsub("\\.", "_", a), '_qqplot.png')
  
  ggplot(NULL) +
    geom_histogram(aes(x = stat[i, ], y = ..density..), 
                   col = 'black', fill = 'white', bins = 50) +
    ggtitle(title1)
  ggsave(file1)
  
  ggplot(NULL, aes(sample = stat[i, ])) +
    stat_qq() +
    stat_qq_line(aes(col = 'red'), show.legend = F) +
    ggtitle(title2)
  ggsave(file2)
}