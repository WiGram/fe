acf_fct <- function(x, lags){
  
  # Intermediate computations
  diff <- x - mean(x)
  n    <- length(diff)
  
  acf <- rep(NA, lags)
  for (t in seq(0, lags)){
    acf[t + 1] <- sum(na.trim(diff * lag(diff, t))) / n
  }
  acf <- acf / acf[1]

  return(acf)
}