library("forecast")
library("fpp")
library("fpp2")
library("GGally")
library("chron")

# seasonal
data(elecequip)

loss.functions = function(x.hat, x) {
  return(c(mean((x-x.hat)^2), mean(abs(x-x.hat)), mean(abs( (x-x.hat)/x )) ));
}

# getting nice figures
ggseasonplot(elecequip)
ggmonthplot(elecequip)

# trying different MA orders
Z.ma= ma(elecequip, order=3)
Z.ma= ma(elecequip, order=5)
Z.ma= ma(elecequip, order=12, centre=TRUE)

plot(elecequip)
lines(Z.ma, col=3)

# trying loess
Z.loess = loess(elecequip~time(elecequip), order=2, family="gaussian", span=0.1)
plot(elecequip, col="blue")
lines(ts(Z.loess$fitted, start=start(elecequip), end=end(elecequip), frequency=12), col="green")

# getting the seasonal component
Z.trend = ma(elecequip, order=12, centre=TRUE);
# Z.trend = ts(Z.loess$fitted, start=start(elecequip), end=end(elecequip), frequency=12);
S=ts(rep(tapply(elecequip-Z.trend, cycle(elecequip-Z.trend), mean, na.rm=T), end(elecequip)[1]-start(elecequip)[1]+1),  frequency = 12, start = start(elecequip))

# puting everything together
Z.all = ts.intersect(elecequip, S, Z.trend)
Z.resid = Z.all[,1] - Z.all[,2]- Z.all[,3]
Z.all = ts.intersect(Z.all, Z.resid);
plot(Z.all, plot.type="single", col=1:4)

# acf of the residual component
acf(na.omit(Z.resid))