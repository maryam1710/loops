x1= rnorm(500)
y1= x1+rnorm(500)
plot.new()
plot.window(xlim = c(-4.5, 4.5), xaxs = "i", ylim = c(-4.5, 4.5), yaxs = "i")
for(i in 2:200){
x =x1[1:i]
y =y1[1:i]
z = lm(y1~x1)
abline(h = -4:4, v = -4:4, col = "lightgrey")
abline(a = coef(z)[1], b = coef(z)[2], col = "lightblue", lty = 6, lwd = 2)
points(x,y, col = heat.colors(5), pch = 15, lwd = 2)
axis(1)
axis(2, las = 1)
box()
title(main = "A Fitted Regression Line")
title(sub = "(500 Observations)")
Sys.sleep(1)
}
#################
plot.new()
plot.window(xlim = c(-1,1), ylim = c(-1,1), asp =1)
x = c(-1, 1, 1, -1)
y = c(1, 1, -1, -1)
polygon(x, y, col = "gray98" )
vertex1 = c(1, 2, 3, 4)
vertex2 = c(2, 3, 4, 1)
for (z in 1:50){
x = 0.9 * x[vertex1] + 0.1 * x[vertex2]
y = 0.9 * y[vertex1] + 0.1 * y[vertex2]
polygon(x, y, col = "gray98")
Sys.sleep(1)
}
#########
x1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
y1 = c(10, 18, 25, 29, 30, 28, 25, 22, 18, 15, 11, 8)

for(i in 4:12) {
  x = x1[1:i]
  y = y1[1:i]
  df = data.frame(x,y)
 
  fit = lm(y ~ poly(x, 3))  
  plot(x, y)  
  
  x0 = seq(min(x), max(x), length = 20) 
  y0 = predict.lm(fit, newdata = list(x = x0)) 
  lines(x0, y0, col = 2) 
  Sys.sleep(1)
}
###########
