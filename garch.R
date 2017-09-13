## Script to illustrate GARCH models using SP 500

library(fGarch)

dat = scan('sp500.txt')
t.test(dat)
Box.test(dat,lag=10,type='Ljung')

pacf(dat)

## fitting an AR(3) model
m1 = arima(dat,order=c(3,0,0))
m1
Box.test(m1$resid,lag=6,type='Ljung')
Box.test(m1$resid^2,lag=6,type='Ljung')
acf(m1$resid^2)
pacf(m1$resid^2)

## try a GRACH(1,1)
m2 = garchFit(~arma(3,0)+garch(1,1),data=dat,trace=F)
summary(m2)

## simplify the model
m3 = garchFit(~garch(1,1),data=dat,trace=F)
summary(m3)
plot(m3)

## try t innovation
m4 = garchFit(~garch(1,1),data=dat,trace=F,cond.dist=c("std"))
summary(m4)
plot(m4)

## try skewed t innovation
m5 = garchFit(~garch(1,1),data=dat,trace=F,cond.dist=c("sstd"))
summary(m5)
plot(m5)

