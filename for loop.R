x1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
y1 = c(10, 18, 25, 29, 30, 28, 25, 22, 18, 15, 11, 8)

for(i in 4:12) {
  x = x1[1:i]
  y = y1[1:i]
  df = data.frame(x,y)
 
  fit = lm(y ~ poly(x, 3))   ## polynomial of degree 3
  plot(x, y)  ## scatter plot (colour: black)
  
  x0 = seq(min(x), max(x), length = 20)  ## prediction grid
  y0 = predict.lm(fit, newdata = list(x = x0))  ## predicted values
  lines(x0, y0, col = 2) 
  Sys.sleep(1)
}