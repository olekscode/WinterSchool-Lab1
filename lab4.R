library("forecast")
library("fpp")
library("fpp2")

data(elecequip)

tau=10;
n=length(elecequip);
loss.all=list();

loss.functions = function(x.hat, x) {
  c(mean((x-x.hat)^2), mean(abs(x-x.hat)), mean(abs( (x-x.hat)/x )) )
}

true.forecast <- function() {
  sapply(tau:1, function(x) naive(head(elecequip,-x),1)$mean)
}

naive.forecast <- function() {
  sapply(tau:1, function(x) snaive(head(elecequip,-x),1)$mean)
}

seas.naive.forecast <- function() {
  2*elecequip[(n-tau):(n-1)]-elecequip[(n-tau-1):(n-2)]
}

abs.trend.forecast <- function() {
  elecequip[(n-tau):(n-1)]^2/elecequip[(n-tau-1):(n-2)]
}

get.maf.forecast <- function(width) {
  function() {
    tail(rollapply(elecequip, width = width, by = 1, FUN = mean, align = "right"), tau)
  }
}

get.holtwinters.forecast <- function(beta = NULL, gamma = NULL) {
  function() {
    my.mod = HoltWinters(elecequip, beta=beta, gamma=gamma)
    tail(my.mod$fitted[,1], tau)
  }
}

models <- c(
  true.forecast,
  naive.forecast,
  seas.naive.forecast,
  abs.trend.forecast,
  get.maf.forecast(width = 3),
  get.maf.forecast(width = 5),
  get.maf.forecast(width = 7),
  get.holtwinters.forecast(beta = FALSE, gamma = FALSE),
  get.holtwinters.forecast(gamma = FALSE),
  get.holtwinters.forecast()
);

all.forec = matrix(0, ncol=10, nrow=tau);
for(i in 1:10) {
  all.forec[,i] = models[[i]]();
}

# computing all losses
apply(all.forec, 2, function(x) loss.functions(x, tail(elecequip,tau)))

# plot all forecasts
model.names = c("true", "naive", "seas. naive","abs trend", "rel. trend", "maf 3", "maf 5", "maf 7", "emwa", "holt", "holtwinters")
all = cbind(tail(elecequip,tau), all.forec);
names(all) <- model.names;
plot.ts(all, plot.type="single", col=c(1:6,1:5), lty=c(rep(1,6),rep(2,6)), ylab="forecasts", ylim=c(30,120))
legend(x = "bottomleft", legend=model.names, ncol=3, bty="n", col=c(1:6,1:5), lty=c(rep(1,6),rep(2,6)))

# plot holtwinters
hw = HoltWinters(elecequip);
plot(hw$fitted)