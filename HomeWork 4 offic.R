layout(matrix(c(1,1,4,1,1,5,2,3,5,2,3,6),ncol=4))
par(mar = c(3,3,1,1),
    oma = c(1,1,1,1),
    ps = 9,
    tck = -0.02,
    mgp = c(1,0.2,0))

#cor
cor1 <- cor(mtcars)
image(cor(mtcars)[order(cor1[1:11]),order(cor1[1:11])],col = rev(heat.colors(100)), axes = FALSE, main = 'Correlation')
axis(side = 1,
     labels = colnames(cor(mtcars)),
     at= seq(0,1,length = length(colnames(mtcars))), 
     las = 2)

#weight
hist(as.numeric(names(table(mtcars$wt))), 
     main = "Weight", 
     xlab = 'lb', 
     ylab = '# of cars',
     ylim = c(0,8))
k = 8/max(mtcars$mpg)
lines(mtcars$wt, mtcars$mpg*k, col = 'red', type = 'p')
lines(predict(smooth.spline(mtcars$wt, mtcars$mpg*k, df = 5)), col = 'red', lwd = 2)     
axis(side = 4,at = seq(0,35,7)*k,labels=seq(0,35,7))
mtext('mpg',4,1.5)
legend('topright', col = 'red', lwd = 2, legend = 'mpg', bty = 'n')

#carb - mpg
boxplot(mtcars$mpg~mtcars$carb, 
        col = 'grey',
        xlab='# of carburetors',
        ylab='mpg')
lines(predict(smooth.spline(mtcars$carb, mtcars$mpg)),lwd=2)
m = lm(mtcars$mpg~mtcars$carb)

#drat ~ hp
plot(mtcars$drat ~ mtcars$hp , 
     xlab = 'Gross horsepower',
     ylab = 'Rear axle ratio',
     pch = 16,
     cex = 1.5,
     col = '#33333390')
l <- lm(mtcars$drat ~ mtcars$hp)
lines(mtcars$hp, predict(l),
      lty = 'dashed', 
      col = 'red', 
      cex = 0.5)
legend('topright', col = 'red', lty = 'dashed', legend = 'rho = -0.449', cex = 0.75)


#am and carb
counts <- table(mtcars$carb, mtcars$am)
barplot(counts,
        col=heat.colors(6),
        names.arg = c('auto','manual'))
legend('topright', fill = heat.colors(6), 
       title = '#carb',
       legend = rownames(counts))

#speed vs am
d_auto = mtcars[mtcars$am ==0,] 
d_manual = mtcars[mtcars$am ==1,] 
plot(density(d_auto$mpg, from=10, to=35, bw=5),
     col = 'red',
     main = 'speed vs transm',
     ylab = 'Density',
     xlab = 'mpg',
     xlim = c(10, 35),
     ylim = c(0, 0.07))
lines(density(d_manual$mpg,from=10, to=35, bw=5), col = 'blue')
legend('topright', col = c('red', 'blue'), legend = c('auto','manual'), lty = 1, cex = 0.75)

