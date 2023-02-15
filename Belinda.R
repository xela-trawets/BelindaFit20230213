h <- 1.4
fo <- y ~ sat * x ^ h / (k + x ^ h) #Y = Bmax  x [Tm]h/(Kdh + [Tm]h)

df0   <- read.csv(file = "TM1.txt", header = TRUE,  
sep = "\t", stringsAsFactors = FALSE)
df1 <- data.frame(x = df0[, 2], y = df0[, 1])

fm <- nls(fo, data = df1, start = list(k = 1, sat = 1))
fm1 <- fm



df0   <- read.csv(file = "TM2.txt", header = TRUE,
sep = "\t",  stringsAsFactors = FALSE)
df2 <- data.frame(x = df0[, 2], y = df0[, 1])

fm <- nls(fo, data = df2, start = list(k = 1, sat = 1))
fm2 <- fm




km1 <- summary(fm1)$parameters[1]
km2 <- summary(fm2)$parameters[1]

sat1 <- summary(fm1)$parameters[2]
sat2 <- summary(fm2)$parameters[2]


summary(fm1)
summary(fm2)

x_axis <- (1:100) * 4.0 * 0.01

plot(df1$x, df1$y / sat1, pch = 19, col = 2, ylim = c(0, 1), 
xlab = '	µM Tm in Supernatant ', 
ylab = 'Fractional Saturation Tm/Actin',
main = 'TM binding'
)
lines(x_axis, predict(fm1, newdata = list(x = x_axis)) / sat1, col = 2, lwd = 3)

lines(x_axis, predict(fm2, newdata = list(x = x_axis)) / sat2, col = 3, lwd = 3)
points(df2$x, df2$y / sat2, pch = 19, col = 3)

txt1 <- formatC(km1, digits = 4, format = "f")
txt2 <- formatC(km2, digits = 4, format = "f")
legend("bottomright",
legend = c(txt1, txt2), col = c(2, 3), pch = 19, lwd = 3,
title = "Tm data set")
