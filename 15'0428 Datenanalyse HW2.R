#15'0428 Datenanalyse homework


#1a
#Seit 1991 umfasst die Grundgesamtheit der ALLBUS-Studien damit die gesamte erwachsene Wohnbevölkerung (d.h. Deutsche und Ausländer) 
#seit 1992 wird im Allgemeinen eine Nettofallzahl von 2.400 Personen im Westen und ca. 1.100 im Osten angestrebt, d.h. die neuen Bundesländer sind überrepräsentiert, um ausreichende Fallzahlen für differenzierte Analysen, insbesondere für den West-Ost-Vergleich, zur Verfügung stellen zu können.
#Überrepräsentation in Ost-Deutschland, um ausreichende Beobachtungen für Detailauswertung zu erhalten (Stichprobe kann nicht einfach vergrößert werden, da zu teuer)
#-> Gewichtungen, um gleiche Ziehungswahrscheinlichkeiten zu bekommen


#1b read data
setwd("C:/users/user/desktop")
install.packages("foreign") #read.spss in this package
require(foreign)
warnings()
names(x)
x<-read.spss("ALLBUS2012.SAV",to.data.frame=T)

#1c
befragte<-x[,344]
nettoeinkommen<-x[,345]

head(c(x[,344],x[,345]))
hist(x[,344])
table(x[,344])
list(x[,344]==NA)
length(which(is.na(x[,344])))
length(which(is.na(x[,345])))

summary(befragte) #940 missing
summary(nettoeinkommen) #3061 missing
#answer
is.data.frame(x)
x$V344
summary(x$v344)
table(x$V344,exclude=NULL)
table(x$V345,exclude=NULL)

#1d
erhgebiet<-x[,8]
bsland<-x[,749]

table(bsland,erhgebiet)

length(which(x[,8]==x[,749]))
which(x[,8]==x[,749])
head(x[,8],50)
levels(x[,749])
#answer
table(x$V749,x$V8)

#1e
t1<-table(bsland,befragte);t1
t2<-table(bsland,nettoeinkommen);t2
chisq.test(t1) #not signi not reject, indepent
chisq.test(t2) #too poor, too many missing
#answer
chisq.test(x$V749,x$V345)
chisq.test(x$V749,x$V345,simulate.p.value=T) #use montecarlo method to est p, more rubost, greater p value

#1f
wbh<-x[,119]
wbj<-x[,121]
chisq.test(table(wbh,wbj)) #sign reject null, not indep
#answer
chisq.test(x$V119,x$V121)
chisq.test(x$V119,x$V121, simulate.p.value=T)

#1g
swbh<-sample(wbh,size=length(wbh)*0.01)
swbj<-sample(wbj,length(wbj)*0.01)
chisq.test(table(swbh,swbj))  #10% not sign>>indep le 1% to few
#answer
index<-sample(nrow(x),0.1*nrow(x))#10%
chisq.test(x$V119[index],x$V121[index])
test10<-chisq.test(x$V119[index],x$V121[index],simulate.p.value=T)
index<-sample(nrow(x),0.01*nrow(x))#1%
chisq.test(x$V119[index],x$V121[index])
test1<-chisq.test(x$V119[index],x$V121[index],simulate.p.value=T)

#1h
sqrt(test10$statistic/sum(test10$expectation))
