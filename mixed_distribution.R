library(ggplot2)
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation

# ============================================= #
# ===== Functions to be used ================== #
# ============================================= #

plotting <- function(x, y1, y2, c1, c2, x_lab, y_lab){
  ggplot(NULL) + 
    geom_point(aes(x = x,
                   y = y1,
                   colour = c1)) +
    geom_line(aes(x = x,
                  y = y2,
                  colour = c2)) +
    labs(x = x_lab, y = y_lab) +
    theme(legend.position = 'top',
          legend.direction = 'horizontal',
          legend.title = element_blank())
}
# ============================================= #

# ============================================= #
# ===== Mixture model ========================= #
# ============================================= #

# set.seed(12345)

mat <- 250 # Length of ts
s_h <- 4.0 # High vol
s_l <- 0.1 # Low vol
p   <- 0.5 # Prob of state

z <- rnorm(mat, 0, 1) # Random no. for vol
u <- runif(mat, 0, 1) # Random no. for state

state  <- (u > p) * 1 + (u < p) * 0
return <- (u > p) * s_h * z + (u < p) * s_l * z

# ===== Plotting the series =================== #
x <- seq(1, mat, 1)
c1 <- 'State'
c2 <- 'Return'
x_lab <- 'Time'
y_lab <- 'Return rate'

plotting(x, state, return, c1, c2, x_lab, y_lab)
# ============================================= #

# ============================================= #
# ===== Markov Switching SV model ============= #
# ============================================= #

p11 <- 0.95 # Prob of state 1, given state 1
p22 <- 0.90 # Prob of state 2, given state 2

state_ms  <- rep(0, mat)
for (t in 2:mat){
  state_ms[t]  <- ifelse(state_ms[t-1] == 0,
                         (u[t] < p11) * 0 + (u[t] > p11) * 1,
                         (u[t] < p22) * 1 + (u[t] > p22) * 0)
}
return_ms <- (state_ms == 1) * s_h * z + (state_ms == 0) * s_l * z

# ===== Plotting, reusing labels from before == #

plotting(x, state_ms, return_ms, c1, c2, x_lab, y_lab)
# ============================================= #

# ============================================= #
# ===== Continuous SV model =================== #
# ============================================= #

mat   <- 1000
gamma <- -0.50
sigma <-  0.31
phi   <-  0.95
eta   <- rnorm(mat, 0, sigma)
z     <- rnorm(mat, 0, 1)
x     <- seq(1,mat,1)

log_s <- rep(-10, mat)

for (t in 2:mat){
  log_s[t] <- gamma + phi * log_s[t-1] + eta[t]
}
s <- sqrt(exp(log_s))
return <- s * z

plotting(x, s, return, c1, c2, x_lab, y_lab)
plot(log(s), type = 'l')
