rm(list=objects());graphics.off()
setwd("/Users/yangyang/study/stat avancee/TD/TP_final")
#Q1
set=read.table("Centrale-DM.data",header = TRUE)
dim(set)
#renommer la nom de extra1 en ABS
colnames(set)[5] <- "ABS"
names(set)
#Q2 
#Représenter un scatter plot des quatre premières variables
library(corrplot)
corrplot(cor(set[,1:4]), method="circle")
#boxplot the two relations
par(mfrow=c(2,1))
boxplot(set$price~set$ABS,main="price/ABS",xlab = "ABS",ylab = "Price")
boxplot(set$price~set$extra2,main = "price/TIA",xlab = "toit de ouvrant",ylab = "Price")
#signification:
# according to the first boxplot, the presence of ABS can increase the price
# according to the second boxplot, the presence of "toit d'ouvrant" has no influence on the price
#Q3
#1) la presence de ABS: 
#   si on conside ABS comme quantitatif, il sera une problem de classification
#   si on conside ABS comme quanlitatif, il sera une problem de regression
#2)
model1=lm(set$price~set$ABS,data=set)
summary(model1)
# interpretation: -----------
#Q4 Étude du prix en fonction du kilométrage:
#a)
model2 = lm(set$price~set$km,data = set)
summary(model2) 
# interpretation: -----------
# it seems the longer the kM is, the smaller the price is 
#b)我不太确定怎么做，把50 000km的数据挑出来，然后用bootstrap?是不是我想麻烦了？



#c) this is almost a linear function: kop1 = 0.02243*km - 3.02203
lm(set$kop1~set$km,data = set)
summary(lm(set$kop1~set$km,data = set))
#Coefficients:
#  (Intercept)       set$km  
#-3.02203      0.02243  
#d)
km = set$km
model3 = lm(set$price~km+I(km^2)+I(km^3),data = set)
summary(model3)
# in fact, it is still a polynomial regression
#e) non, it's different.如果因子不止一个，且别是非平衡设计，或者存在协变量，表达式中的顺序会对结果造成影响。
#样本大小越不平衡，效应项的顺序对结果影响越大
model3_1 = lm(set$price~I(km^3) + I(km^2)+ km,data = set)
summary(model3_1)
anova(model3_1)
anova(model3)
anova(model3,model3_1)
#f) you need to install this package, because it's not contained initially by R
library(car)
res = vif(lm(set$price~km+I(km^2)+I(km^3),data = set))
#comment: -------
#g)




