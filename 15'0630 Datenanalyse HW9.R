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
t.test(age~richwork, data=xm)  #指有這個有顯著差別 在年紀尚有差 不同組中
t.test(educ~richwork, data=xm) #兩組沒差
t.test(tvhours~richwork, data=xm) #兩組沒差
#d
wilcox.test(x$husbeduc, x$wifeduc, paired=T) #not reject null 兩個沒差
t.test(x$husbeduc, x$wifeduc, paired=T) #一樣蓋到凌
#e
library("car")
leveneTest(x$age, x$hapmar) #先看他的median一不一樣 不知道怎麼解釋 還是homo or not?
tapply(x$age, x$hapmar, shapiro.test) 
tapply(x$age, x$hapmar, describe)

#
anova<-aov(age~hapmar, data=x) #做anova 不顯著 mean沒差
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
leveneTest(x$age, x$pres92) #reject null variance不一樣
tapply(x$age, x$pres92, shapiro.test) #個別factor是不是normal紛不
#
anova<-aov(age~pres92, data=x)
summary(anova) #reject null mean 不一樣 投給不希跟投給柯林頓的年紀不一樣
kruskal.test(age~pres92, data=x) #測mean跟median一步一樣 任醫不一樣reject
medianTest(x$age, x$pres92)
#
pairwise.t.test(x$age, x$pres92, p.adjust.method="bonferroni") #依對伊test但在P限制底下
TukeyHSD(anova) # 全部的mean都一樣?
scheffe.test(anova, "pres92") #限性組合是不是0
#


#g
compare.means(x$rincmdol, x$partyid)
compare.means(x$educ,x$partyid)
compare.means(x$age,x$partyid)

#h
compare.means(x$rincmdol, x$degree)
