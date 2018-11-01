library(stats4)       # base package; use for mle()
library(ggplot2)      # Graphing package (sweet)
library(quantmod)     # Get stock data
library(numDeriv)     # Achieve score and information
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

spy <- log(SPY$SPY.Adjusted)*100
ret <- (spy - lag(spy, 1))[-1]
rm(SPY, spy)