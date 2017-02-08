#15'0714 HW11

a<-c(2.3,1.5,2.45,2.1,2.0,2.1,1.55,1.3,2.57,2.55,1,1.12,2.69,2.53,2.8,2.21,1.89,1.09,2.95,3.75,1.27,2.84,2.72,2.43,1.96,3.49,3.54,3.38,1.37)
mean(a)
plot(a)
hist(a)
boxplot(a)

#plot the kernel density
hist(a, probability = T)
xd <- seq(1, 4, by=0.01)
yd <- dnorm(xd, mean = mean(a), sd = sd(a))
lines(xd, yd)

#1a
yeah maybe
google it normal 0.5~2
#1b
more outlier recently 3 times
#1c
boxplot(a)
hist(a)
rug(a)
stem(a) #maybe reasonable cause most out of 2.4 and normal 2
#1d
fivenum(a) #>2.72 should be outlier
summary(a)

###dozent###
install.packages("psych")
library("psych")
describe(a)
require("MASS")
huber(a) #use huber m-estimater for mean and sd
        #lower than moment est. have background assum on distribution
IQR(a)

#1e
install.packages("robustbase")
library("robustbase")
x<-a
set.seed(0)
out<-adjOutlyingness(x,ndir=2500)
hist(out$adjout)
plot(mean(x),apply(x,1,sd),log="xy",main="Mean vs.Stddev")
plot(out$adjout)

###dozent###
out<-adjOutlyingness(x,clower=0,cupper=0)
boxplot(out$adjout) #extract the outlier into 0-1
plot(x,out$adjout)

#dunno 3 small
#1f
yes

#2a
library("foreign")
x <- read.spss("z:/daten/BANK2.SAV", to.data.frame=T)
install.packages("andrews")
require("andrews")
andrews(x,type=1)

###dozent###
andrews(x,ymax=3)
z<-scale(x)
andrews(z,ymax=3)
andrews(z[,6:1],ymax=3)
zs<-subset(x,runif(nrow(z))<0.2)
andrews(zs[,c(6,4,5)],ymax=3)

#2b
set.seed(0)
out<-adjOutlyingness(x,ndir=2500)
hist(out$adjout)
plot(colMeans(x),apply(x,2,sd),main="Mean vs. Stddev")

###dozent###
out<-adjOutlyingness(x,clower=0,cupper=0)
boxplot(out$adjout)
plot(x[,6],out$adjout)

out<-adjOutlyingness(z,clower=0,cupper=0)
boxplot(out$adjout)
plot(z[,6],out$adjout)

