library(ggplot2)

x <- seq(0,10,0.01)

delta <- 0.3

fx <- x**delta

a <- 0.5
b <- 8

y <- seq(a, b, 0.01)

slope = (fx[which(x == b)] - fx[which(x == a)] ) / ( b - a)

fy <- fx[which(x == a)] + slope * (y-a)

ggplot(NULL) +
  geom_line(aes(x = x, y = fx, col = 'concave function')) +
  geom_line(aes(x = y, y = fy, col = 'linear function')) +
  guides(color = guide_legend(title = ''))

x_1 <- -3
x_2 <- abs(x_1)
x_3 <- 1

x_4 <- abs(x_1 + x_3)
x_5 <- abs(x_1) + abs(x_3)

x <- c("  x1", " |x1|", " x2", "|x1 + x2|", "|x1| + |x2|")
y <- c(x_1, x_2, x_3, x_4, x_5)

ggplot(NULL, aes(x = x, y = y,
                 fill = as.factor(x), guide = F)) +
  geom_bar(stat = 'identity', width = .5) +
  labs(x = "", y = "")