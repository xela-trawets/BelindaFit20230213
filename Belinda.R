h <- 1.5
fo <- y ~ sat * x ^ h / (k + x ^ h) #Y = Bmax  x [Tm]h/(Kdh + [Tm]h)

df0   <- read.csv(file = "TM1.txt", header = TRUE,  
sep = "\t", stringsAsFactors = FALSE)
df <- data.frame(x = df0[, 2], y = df0[, 1])

fm <- nls(fo, data = df, start = list(k = 1, sat = 1))
fm1 <- fm
summary(fm)


plot(df$x, df$y, pch = 19, col = 2, 
xlab = '	µM Tm in Supernatant ', 
ylab = 'Fractional Saturation Tm/Actin',
main = 'TM binding'
)

x_axis <- (1:100) * 4.0 * 0.01
lines(x_axis, predict(fm, newdata = list(x = x_axis)), col = 2, lwd = 3)
summary(fm)


df0   <- read.csv(file = "TM2.txt", header = TRUE,
sep = "\t",  stringsAsFactors = FALSE)
df <- data.frame(x = df0[, 2], y = df0[, 1])

fm <- nls(fo, data = df, start = list(k = 1, sat = 1))
fm2 <- fm
summary(fm)


points(df$x, df$y, pch = 19, col = 3)

x_axis <- (1:100) * 4.0 * 0.01
lines(x_axis, predict(fm, newdata = list(x = x_axis)), col = 3, lwd = 3)

km1 <- summary(fm1)$parameters[1]
km2 <- summary(fm2)$parameters[1]
#options(digits=5)
#txt1 = as.character(km1)
txt1 <- formatC(km1, digits = 4, format = "f")
txt2 <- formatC(km2, digits = 4, format = "f")
legend("bottomright",
legend = c(txt1, txt2), col = c(2, 3), pch = 19, lwd = 3,
title = "Tm data set")

summary(fm1)
summary(fm2)
