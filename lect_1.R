# Quantmod provides a way to "getSymbols"
library(quantmod)

# gridExtra provides an easy way to illustrate multiple plots
library(gridExtra)
library(ggplot2)
# source contains function for auto correlation analysis
source('C:/Users/wigr11ab/Dropbox/KU/K3/FE/Code/acf_function.R')


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

# Quickly see what it looks like
ggplot(NULL, aes(x = index(spy), 
                 y = spy,
                 col = 'red')) + 
  geom_line() +
  ylab("Price (USD)") +
  xlab(NULL) +
  guides(col = FALSE)

# =================================== #
# ======== Analysing returns ======== #
# =================================== #

# First we transform price data into log returns (and squared)
spy_log_ret <- log(spy) - log(lag(spy))
spy_log_ret <- spy_log_ret[-1]

slr2 <- spy_log_ret ** 2

# Second, we analyse their autocorrelation
acf_ret  <- acf_fct(spy_log_ret, 20)
acf_ret2 <- acf_fct(slr2, 20) 

# =================================== #
# ======= Analysing volatility ====== #
# =================================== #

# 1. ARCH volatility

# 2. Stochastic volatility

# 3. Realised volatility (rvol)
#  - plot at end of programme illustrates non-constant vol

# First a couple of functions for rvol and annualised rvol
rvol <- function(returns){
  sqrt(sum(returns ** 2) / length(returns))
}

ann_rvol <- function(rvol, periods){
  rvol * sqrt(periods)
}

# Computation of rvol respectively ann_rvol
realised_vol <- rep(0, length(spy_log_ret))
for (i in 1:length(realised_vol)){
  realised_vol[i] <- rvol(spy_log_ret[1:i])
}

ann_realised_vol <- ann_rvol(realised_vol, 252)


# Ivol
#  - her skal vi bruge optionspriser på sp500, som jeg ikke har

ivol <- "Isolate volatility in BS-formula"

# ================================================ #
# =================== PLOTTING =================== #
# ================================================ #

ret_plot <- ggplot(NULL, aes(x = index(spy_log_ret), 
                             y = spy_log_ret,
                             col = 'red')) + 
  geom_line() +
  ylab("Return rate (log returns)") +
  xlab(NULL) +
  guides(col = FALSE)

ret2_plot <- ggplot(NULL, aes(x = index(slr2), 
                              y = slr2,
                              col = 'red')) + 
  geom_line() +
  ylab("Squared log returns") +
  xlab(NULL) +
  guides(col = FALSE)


acf_plot <- ggplot(NULL, aes(x = index(acf_ret), y = acf_ret, fill = 'red')) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = rep(0.05, length(acf_ret)))) +
  geom_line(aes(y = rep(-0.05, length(acf_ret)))) +
  guides(fill = FALSE) +
  ylim(c(-1,1)) +
  xlab(NULL) +
  ylab(NULL)

acf2_plot <- ggplot(NULL, aes(x = index(acf_ret2), 
                 y = acf_ret2, 
                 fill = 'red')) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = rep(0.05, length(acf_ret2)))) +
  geom_line(aes(y = rep(-0.05, length(acf_ret2)))) +
  guides(fill = FALSE) +
  ylim(c(0,1)) +
  xlab(NULL) +
  ylab(NULL)

# Combining plots to illustrate returns and auto correlation
grid.arrange(ret_plot, 
             ret2_plot, 
             acf_plot, 
             acf2_plot, ncol = 2)

# Plotting realised volatility
rvol_plot  <- ggplot(NULL, aes(x = index(spy_log_ret), 
                               y = realised_vol,
                               col = 'red')) + 
  geom_line() +
  ylab("Volatility of returns") +
  xlab(NULL) +
  guides(col = FALSE)

ann_rvol_plot  <- ggplot(NULL, aes(x = index(spy_log_ret), 
                                   y = ann_realised_vol,
                                   col = 'red')) + 
  geom_line() +
  ylab("Annualised volatility of returns") +
  xlab(NULL) +
  guides(col = FALSE)

grid.arrange(rvol_plot,
             ann_rvol_plot,
             ncol = 2)
