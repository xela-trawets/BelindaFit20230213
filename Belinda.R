h <- 1.0
fo <- y ~ sat * x ^ h / (k + x ^ h) #Y = Bmax  x [Tm]h/(Kdh + [Tm]h)

sat <- 1

df0   <- read.csv(file = "TM1.txt", header = TRUE,  
sep = "\t", stringsAsFactors = FALSE)
df1 <- data.frame(x = df0[, 2], y = df0[, 1])

fm_k <- nls(fo, data = df1, start = list(k = 1))
fm_ks <- nls(fo, data = df1, start = list(k = 1, sat = 1))
fm1 <- fm_ks



df0   <- read.csv(file = "TM2.txt", header = TRUE,
sep = "\t",  stringsAsFactors = FALSE)
df2 <- data.frame(x = df0[, 2], y = df0[, 1])

fm_k <- nls(fo, data = df2, start = list(k = 1))
fm_ks <- nls(fo, data = df2, start = list(k = 1, sat = 1))
fm2 <- fm_ks




km1 <- summary(fm1)$parameters[1]
km2 <- summary(fm2)$parameters[1]

sekm1 = summary(fm1)[10]$coefficients[1,2]
sekm2 = summary(fm2)[10]$coefficients[1,2]

sat1 <- 1
sat2 <- 1

sat1 <- summary(fm1)$parameters[2]
sat2 <- summary(fm2)$parameters[2]


summary(fm1)
summary(fm2)

library(httpgd)
hgd()
 .vsc.browser(httpgd::hgd_url(), viewer = "Beside")

x_max <- 5
x_axis <- (1:100) * x_max * 0.01

#x11()

par(oma=c(2,2,2,2))
par(mar=c(6,6,1,1))

col_list =  c("#00AFBB", "#E7B800", "#FC4E07", "#4169E1", "#F08080")
col1 = col_list[5]
col2 = col_list[4]

# plot(cx, cy, xaxt = "n")
# axis(1, at = seq(0, 2, .5), labels = seq(0, 2, .5), lwd = 0, lwd.ticks = 1)
# axis(1, at = setdiff(cx, seq(0, 2, .5)), labels = NA, lwd = NA, lwd.ticks = 1)
# box()

plot(df1$x, df1$y / sat1, 
type = 'n',  
frame = FALSE, cex = 2, pch=19,
tick = FALSE, 
col = col1, 
xlim = c(0, x_max), ylim = c(0, 1.5), 

#plot(df1$x, df1$y / sat1, pch = 19, col = 2, xlim = c(0, x_max), ylim = c(0, 1), 
###plot(df1$x, df1$y, pch = 19, col = 2, xlim = c(0, 5), ylim = c(0, 1.5), 

xaxs = "i",
yaxs = "i",
xlab = '	Free Tropomyosin (µM) ', 
ylab = 'Fractional Saturation (Tm/Actin)',
     cex.lab = 2.5,
     cex.axis = 2,
     cex.main = 1,
     cex.sub = 1)

box(bty = "L", lwd = 4)

lines(x_axis, predict(fm1, newdata = list(x = x_axis)) / sat1, col = col1, lwd = 3)
lines(x_axis, predict(fm2, newdata = list(x = x_axis)) / sat2, col = col2, lwd = 3)
points(df2$x, df2$y / sat2, cex = 2.5, pch = 15, col = col2)
points(df1$x, df1$y / sat2, cex = 2.3, pch = 19, col = col1)

#lines(x_axis, predict(fm1, newdata = list(x = x_axis)) , col = 2, lwd = 3)
#lines(x_axis, predict(fm2, newdata = list(x = x_axis)) , col = 3, lwd = 3)
#points(df2$x, df2$y , pch = 19, col = 3)

txt1 <- formatC(km1, digits = 4, format = "f")
txt2 <- formatC(km2, digits = 4, format = "f")

(km1 - km2) / sqrt(sekm1 * sekm1 + sekm2 * sekm2)

# legend("bottomright",
# legend = c(txt1, txt2), col = c(2, 3), pch = 19, lwd = 3,
# title = "Tm data set")

predict(fm1, newdata = list(x = c(0,1,2,3,4,5,6,7,8,9,11,44)))
predict(fm2, newdata = list(x = c(0,1,2,3,4,5,6,7,8,9,11,44)))
sat1
sat2
