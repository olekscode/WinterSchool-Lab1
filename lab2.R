library("xts"); library("fpp"); library("fpp2");
library("forecast")
data(ukcars)
#write.csv2(ukcars, file="ukcars.csv", quote=FALSE)
plot(ukcars)

#losses
loss.functions = function(x.hat, x)
{
  return(c(mean((x-x.hat)^2), mean(abs(x-x.hat)), mean(abs( (x-x.hat)/x )) ));
}


#dropping the last pass of the data
ukcars= ts(ukcars[time(ukcars)<=2000], start=c(1977,2), frequency=4)
ukcars.pred.all = list();
ukcars.fitted.all = list();

# creating dummies for quarters and the breakpoint
Q1 = (cycle(ukcars)==1)
Q2 = (cycle(ukcars)==2)
Q3 = (cycle(ukcars)==3)
D = rep(0,length(ukcars)); D[time(ukcars)<=1982]=1;
Xcos= cos(2*pi*cycle(ukcars)/4);
Xsin= sin(2*pi*cycle(ukcars)/4);

# making a big data frame
ukcars.df = as.data.frame(cbind(ukcars, 1:length(ukcars), (1:length(ukcars))^2, as.numeric(Q1), as.numeric(Q2), as.numeric(Q3), D, Xcos, Xsin));
colnames(ukcars.df) <- c("ukcars", "t", "t2", "Q1", "Q2", "Q3", "D", "Xcos", "Xsin");


# spliting in training and test
ukcars.test = tail(ukcars.df,10)
ukcars.train = head(ukcars.df,83)
ukcars.test.ts = tail(ukcars,10)
ukcars.train.ts = head(ukcars,83)


# change here 1,2,3  (either a loor or one-by-one)

#which.model = 4;
for(which.model in 1:4)
{
  
  # defining the model
  if (which.model == 1) {Z.lm = lm(ukcars ~ t + t2, data=ukcars.train);}
  if (which.model == 2) {Z.lm = lm(ukcars ~ t + t2 + Q1 + Q2 + Q3, data=ukcars.train);}
  if (which.model == 3) {Z.lm = lm(ukcars ~ t +  Q1 + Q2 + Q3 + D*t, data=ukcars.train);}
  if (which.model == 4) {Z.lm = lm(ukcars ~ t +  D*t + Xsin + Xcos, data=ukcars.train);}
  
  # estimation results
  summary(Z.lm);
  par(mfrow = c(2, 2));
  plot(Z.lm);
  
  dev.off();  # run this to close the previous figure
  
  # plot minzer-zarnowitz
  plot(ukcars.train$ukcars,Z.lm$fitted);
  abline(lm(Z.lm$fitted~ukcars.train$ukcars))
  
  # check the residuals
  acf(Z.lm$residuals);
  dwtest(Z.lm)
  
  # make a data frame with true and fitted for training
  ukcars.res = as.ts(cbind(ukcars.train.ts, Z.lm$fitted));
  # make a data frame with true and forecasted  for test
  ukcars.pred = as.ts(cbind(ukcars.test.ts, predict(Z.lm,newdata=ukcars.test)));
  
  # plotting all the results
  plot(ukcars, plot.type="single", col=c(2), ylim=c(150,500))
  lines(ukcars.res[,2],  col=c(3))
  lines(ukcars.pred[,2], col=c(4))
  
  # storing all the resuls in a list (should run for all which,model's, one-by-one)
  
  ukcars.pred.all[[which.model]] = ukcars.pred;
  ukcars.fitted.all[[which.model]] = ukcars.res;
  
}

########## run to here with different which.model's

# plotting all
plot(ukcars, plot.type="single", col=c(1), ylim=c(150,500), lwd=1.5);
for(i in 1:4) lines(ukcars.pred.all[[i]][,2], col=c(i+1), lty=2);
for(i in 1:4) lines(ukcars.fitted.all[[i]][,2], col=c(i+1), lty=1);

# getting the loss function
for(i in 1:4) cat(loss.functions(ukcars.pred.all[[i]][,2], ukcars.pred.all[[i]][,1]), "\n");