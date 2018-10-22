acf_fct <- function(x, lags){
  
  # Intermediate computations
  diff        <- x - mean(x)
  diff_sqr    <- diff ** 2
  diff_lag    <- lag(diff, c(0:lags))
  
  acf <- c()
  # Final computation
  for (t in 1:(lags+1)){
    acf[t] <- sum(na.trim(diff * diff_lag[,t])) / sum(diff_sqr)
  }
  return(acf)
}