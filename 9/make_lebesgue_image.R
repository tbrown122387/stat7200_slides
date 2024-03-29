library(grDevices)

col=gray.colors(n=50, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)

no.partitions <- 4

# make nonnegative random variable
x = seq(-1,1,0.0001)
y = 1-x^2
plot(x,y,type='l', lwd=2, col="purple4", xlab=expression(omega), ylab=expression(X(omega)),
     main = "")
  
i = 1:no.partitions
x_segment = sqrt(1 - i/no.partitions)
height = (i - 1)/no.partitions
base = - sqrt(1 - i/no.partitions) + sqrt(1  - (i - 1)/no.partitions)
  
for(i in 1:length(i)){
  x0=x_segment[i]
  x0.neg = -x_segment[i]
  y0=height[i]
  x1=x_segment[i] + base[i]
  x1.neg = -(x0 + base[i])
  y1=y0
    
  xrun = seq(x0,x1,0.0001)
  xrun.neg=seq(x0.neg, x1.neg,-0.0001)
  yrun = rep(y0,length(xrun))
  yrun_reverse = rep(0, length(xrun))
  
  polygon(c(xrun,rev(xrun)), c(yrun, yrun_reverse),
          col=col[i],border=col[i])
  polygon(c(xrun.neg,rev(xrun.neg)), c(yrun, yrun_reverse),
          col=col[i],border=col[i])
    
  segments(x0=x0,y0=y0,x1=x1,y1=y1,lwd=3, col="yellow")
  segments(x0=x0.neg,y0=y0,x1=x1.neg,y1=y1,lwd=3, col="yellow")
}
