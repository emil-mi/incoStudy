library(lattice)
library(samplesize)
library(pwr)
library(gsDesign)

x<-seq(5,15,0.1)

y1<-data.frame(x=x,y=dnorm(x,mean=10,sd=.3),type='A',seq=1)
y2<-data.frame(x=x,y=dnorm(x,mean=9,sd=.3),type='B',seq=1)

y3<-data.frame(x=x,y=dnorm(x,mean=10,sd=1),type='C',seq=2)
y4<-data.frame(x=x,y=dnorm(x,mean=9,sd=1),type='D',seq=2)

xyplot(y~x|seq,data=rbind(y1,y2,y3,y4),groups=type, type='l', scales=list(y='free'),layout=c(1,2))

power.t.test(delta=1,sd=.3,power=0.85)
power.t.test(delta=1,sd=1,power=0.85)

m1=mean(round(TOT$I2D.POST))

m1=mean(round(TOT$I2D.POST*3/2))
sd1=sd(round(TOT$I2D.POST*3/2))

m3=mean(round(AMB$I2D.PRE*3/2))
sd3=sd(round(AMB$I2D.PRE*3/2))

m2=mean(round(AMB$I2D.POST*3/2))
sd2=sd(round(AMB$I2D.POST*3/2))

psd=sqrt((sd1*sd1+sd2*sd2)/2)
d=(m1-m2)/psd

power.t.test(delta=2.5,sd=sd3,power=0.85,alternative='one.sided')

power.t.test(n=20,sd=sd3,power=0.85,alternative='one.sided')
power.t.test(n=20,sd=sd3,power=0.80,alternative='one.sided')

power.t.test(n=20,sd=sd3,delta=5,alternative='one.sided')
power.t.test(n=20,sd=sd3,delta=3,alternative='one.sided')

power.t.test(n=20,sd=sd2,delta=0.4 ,alternative='one.sided')

power.t.test(n=20,sd=sd2,power=0.80,alternative='one.sided')

pwr.t.test(d=1/sd2,power=0.85,alternative='greater')
