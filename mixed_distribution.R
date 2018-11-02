library(ggplot2)
dev.off()             # delete plots
rm(list = ls())       # delete variables
cat("\014")           # clear console
options(scipen = 999) # disable scientific notation

set.seed(12345)

mat <- 250 # Length of ts
s_h <- 4.0 # High vol
s_l <- 0.1 # Low vol
p   <- 0.5 # Prob of state

z <- rnorm(mat, 0, 1) # Random no. for vol
u <- runif(mat, 0, 1) # Random no. for state

state  <- (u > p) * 1 + (u < p) * 0
return <- (u > p) * s_h * z + (u < p) * s_l * z

ggplot(NULL) + 
  geom_point(aes(x = seq(1, mat, 1),
                y = state,
                colour = 'State')) +
  geom_line(aes(x = seq(1, mat, 1),
                y = return,
                colour = 'Return')) +
  labs(x = 'Time', y = 'Return rate') +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.title = element_blank())
