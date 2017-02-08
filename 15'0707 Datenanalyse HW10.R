#HW 10 not finished...
library()



income<-na.omit(allbus$V344)
boxplot(income)
boxplot(income,ylim=c(0,11000))
#1c
z<-scale(income)
describe(z)
describe(income)

#2a
boxplot(body$weight)
subset(body,body$wieght>160)
#2b
library("outliers")
grubbs.test(body$Age)
grubbs.test(body$weight)
grubbs.test(body$)
#2c
shapiro.test()
shapiro.test()
#2d
body$bmi<-na.omit((body$weight)/2.2)/(body$weight/39.37)/(body$weight/39.37))
hist(body$bmi)
describe(body$bmi)
boxplot(body$bmi)
subset(body,body$bmi>25.5)
shapiro.test(body$bmi[-152])
grubbs.test(body$bmi)

#3a ¤U¤@¦¸
