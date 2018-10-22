library(ggplot2)
library(gridExtra)

# Data generation
time_steps <- 1000
a <- c(0.1, 0.7, 1.1, 3.1)
z <- rnorm(n = length(a) * time_steps, 
           mean = 0, 
           sd = 1)

# Matrix generation
x <- matrix(0, length(a), time_steps)
z <- matrix(z, length(a), time_steps)

# Simulate time series using ARCH(1)
for (i in 2:length(x[1,])){
  s <- sqrt(1 + a * x[, i-1] ** 2)
  x[, i] <- s * z[, i]
}

# Generate graphs for each level of a (alpha)
g <- lapply(1:length(a), function(i) {
  ggplot(NULL, aes(x = 1:time_steps,
                   y = x[i, ],
                   col = 'red')) +
    geom_line() +
    labs(title = paste0("alpha: ", a[i], sep = ""),
         x = NULL,
         y = "ARCH(1) Returns") +
    guides(col = FALSE)
})

# Plotting.
grid.arrange(grobs = g, ncol = 2)

# ==================================== #
# ===== Normal distribution fct ====== #
# ==================================== #


sim_norm <- rnorm(1000, 0, 1)
xfit     <- seq(min(sim_norm), 
                max(sim_norm), 
                length = length(sim_norm))
yfit     <- dnorm(xfit,
                  mean = mean(sim_norm),
                  sd = sd(sim_norm))

ggplot(NULL, aes(sim_norm)) + 
  geom_histogram(col = 'white',
                 fill = 'black',
                 bins = 50,
                 aes(y = ..density..)) +
  geom_line(aes(x = xfit, 
                y = yfit, 
                col = 'red')) +
  guides(col = FALSE)

# Notice something does not work with cauchy
sim_cchy <- rcauchy(100, 0, 1)
xfit     <- seq(min(sim_cchy),
                max(sim_cchy),
                length = length(sim_cchy))
yfit     <- dcauchy(xfit, 0, 1)

ggplot(NULL, aes(sim_cchy)) +
  geom_histogram(col = 'white',
                 fill = 'black',
                 bins = 25,
                 aes(y = ..density..)) +
  geom_line(aes(x = xfit,
                y = yfit, 
                col = 'red')) +
  guides(col = FALSE)


# ==================================== #
# ===== Cauchy distribution fct ====== #
# ==================================== #

norm_pdf <- function(x, mean, vol){
  1 / sqrt(2 * pi * vol ** 2) * exp(-((x - mean)/vol) ** 2 / 2)
}

cchy_pdf <- function(x){
  1 / pi * 1 / (1 + x ** 2)
}

vector <- seq(-20,20,0.01)
npdf <- norm_pdf(vector, 0, 1)
cpdf <- cchy_pdf(vector)

pdfs <- ggplot(NULL, aes(x = vector)) +
  geom_line(aes(y = npdf, col = 'normal')) +
  geom_line(aes(y = cpdf, col = 'cauchy'))

ncdf <- cumsum(npdf * 0.01)
ccdf <- cumsum(cpdf * 0.01)

cdfs <- ggplot(NULL, aes(x = vector)) +
  geom_line(aes(y = ncdf, col = 'normal')) +
  geom_line(aes(y = ccdf, col = 'cauchy'))

grid.arrange(pdfs, cdfs, ncol = 1)
