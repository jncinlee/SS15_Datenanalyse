#HW9


library("foreign")
x <- read.spss("z:/daten/GSS.SAV", to.data.frame=T)

medianTest <- function(x, f) {
  med <- median(x, na.rm=T)
  below <- (x<med)
  tab<-table(below, as.factor(f))
  chisq.test(tab)
}

compare.means <- function (x, f) {
  f <- as.factor(f)
  print(leveneTest(x, f))
  print(tapply(x, f, shapiro.test))
  print(tapply(x, f, describe))
  #
  anova<-aov(x~f)
  print(summary(anova))
  print(kruskal.test(x~f))
  print(medianTest(x, f))
  #
  boxplot(x~f)
  print(pairwise.t.test(x, f, p.adjust.method="bonferroni"))
  print(TukeyHSD(anova))
  print(scheffe.test(anova, "f"))
}

library("foreign")
library("psych")
#x <- read.spss("C:/Users/sk/unison/Daten/GSS.SAV", to.data.frame=T)
colnames(x) <- tolower(colnames(x))
##1
#a
tapply(x$age, x$happy, describe)
boxplot(age~happy, data=x)
#
medianTest(x$age, x$happy)
#b
diff <- x$husbhr-x$wifehr
hist(diff)
#
t.test(x$husbhr, x$wifehr, paired=T)
#
t.test(diff)
#c
xm <- subset(x, !is.na(x$richwork))
t.test(age~richwork, data=xm)  #�����o�Ӧ���ۮt�O �b�~���|���t ���P�դ�
t.test(educ~richwork, data=xm) #��ըS�t
t.test(tvhours~richwork, data=xm) #��ըS�t
#d
wilcox.test(x$husbeduc, x$wifeduc, paired=T) #not reject null ��ӨS�t
t.test(x$husbeduc, x$wifeduc, paired=T) #�@�˻\���
#e
library("car")
leveneTest(x$age, x$hapmar) #���ݥL��median�@���@�� �����D������ �٬Ohomo or not?
tapply(x$age, x$hapmar, shapiro.test) 
tapply(x$age, x$hapmar, describe)

#
anova<-aov(age~hapmar, data=x) #��anova ����� mean�S�t
summary(anova)
kruskal.test(age~hapmar, data=x)
medianTest(x$age, x$hapmar)
#
require("agricolae")
pairwise.t.test(x$age, x$hapmar)
pairwise.t.test(x$age, x$hapmar, p.adjust.method="bonferroni")
TukeyHSD(anova)
scheffe.test(anova, "hapmar")

#f
leveneTest(x$age, x$pres92) #reject null variance���@��
tapply(x$age, x$pres92, shapiro.test) #�ӧOfactor�O���Onormal�ɤ�
#
anova<-aov(age~pres92, data=x)
summary(anova) #reject null mean ���@�� �뵹���Ƹ�뵹�_�L�y���~�����@��
kruskal.test(age~pres92, data=x) #��mean��median�@�B�@�� ���夣�@��reject
medianTest(x$age, x$pres92)
#
pairwise.t.test(x$age, x$pres92, p.adjust.method="bonferroni") #�̹��test���bP����U
TukeyHSD(anova) # ������mean���@��?
scheffe.test(anova, "pres92") #���ʲզX�O���O0
#


#g
compare.means(x$rincmdol, x$partyid)
compare.means(x$educ,x$partyid)
compare.means(x$age,x$partyid)

#h
compare.means(x$rincmdol, x$degree)