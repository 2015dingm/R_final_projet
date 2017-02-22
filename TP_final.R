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
model2 = lm(price~km,data = set)
summary(model2) 
# interpretation: -----------
# it seems the longer the kM is, the smaller the price is 
#b)我不太确定怎么做，把50 000km的数据挑出来，然后用bootstrap?是不是我想麻烦了？
confint(model2, "set$km")
IC1=predict(model2, newdata = data.frame(km = 50), interval = "confidence")
max(IC1)-min(IC1)#0.6604941
IC2=predict(model2, newdata = data.frame(km=135), interval = "confidence")
max(IC2)-min(IC2)#0.3075579
dev.off()
#c) this is almost a linear function: kop1 = 0.02243*km - 3.02203
plot(set$km,set$kop1,main = "linear")
lm= lm(kop1~km,data = set)
#Coefficients:
#  (Intercept)       set$km  
#-3.02203      0.02243  
summary(lm(set$kop1~set$km,data = set))
qqnorm(rstudent(lm),
       main=c("pval=", round(shapiro.test(rstudent(lm))$p.value,2)))
qqline(rstudent(lm))
max(rstudent(lm))
which.max(rstudent(lm)) # plus grand residu
plot(lm)


#reduis = set$kop1-(lm$coefficients[1]+lm$coefficients[2]*set$km)
#m = mean(reduis)
#sd = sd(reduis)
#k=(reduis-m)/sd
#qq-plot:observe a good fit of the straight line
#qqnorm(k)
#qqline(k)

#d)
model3 = lm(price~km+I(km^2)+I(km^3),data = set)
summary(model3)
# in fact, it is still a polynomial regression
#e) non, it's different.如果因子不止一个，且别是非平衡设计，或者存在协变量，表达式中的顺序会对结果造成影响。
#样本大小越不平衡，效应项的顺序对结果影响越大
model3_b = lm(price~I(km^3) + I(km^2)+ km,data = set)
summary(model3_b)
anova(model3_b)
anova(model3)
anova(model3,model3_b)
#f) you need to install this package, because it's not contained initially by R
library(car)
result = vif(lm(price~km+I(km^2)+I(km^3),data = set))
#km  I(km^2)  I(km^3) 
#165.3163 787.3501 262.5217 

#calcul direct:1/(1-Rj^2) 
mkm=lm(km~I(km^2)+I(km^3), data = set)
(vif_km=1/(1-summary(mkm)$r.squared))
#165.3163
mkm2=lm(I(km^2)~I(km^3)+km, data = set)
(vif_km2=1/(1-(summary(mkm2)$r.squared)))
#787.3501
mkm3=lm(I(km^3)~I(km^2)+km, data = set)
(vif_km3=1/(1-(summary(mkm3)$r.squared)))
#262.5217 
#comment: -------
#g)
#The VIF equals 1 when the vector Xj is orthogonal to each column of the design matrix for the regression of Xj on the other covariates.
model4 = lm(price~kop1+kop2+kop3,data = set)
(vif_model4 = vif(model4))
mean(set$kop1)
sd(set$kop1)
mean(set$kop2) 
sd(set$kop2) 
mean(set$kop3)
sd(set$kop3) 

lr2 = lm(kop2~km+I(km^2),data = set)
summary(lr2)
#Coefficients:
#  Estimate Std. Error    t value Pr(>|t|)    
#(Intercept)  6.288e+00  1.114e-08  564703221   <2e-16 ***
#  km          -1.022e-01  1.653e-10 -618221041   <2e-16 ***
#  I(km^2)      3.714e-04  5.868e-13  632936828   <2e-16 ***
lr3 = lm(kop3~km+I(km^2)+I(km^3),data = set)
summary(lr3)
#Coefficients:
#  Estimate Std. Error    t value Pr(>|t|)    
#(Intercept) -9.461e+00  1.788e-08 -529189693   <2e-16 ***
#  km           2.687e-01  4.370e-10  614916710   <2e-16 ***
#  I(km^2)     -2.203e-03  3.386e-12 -650691516   <2e-16 ***
#  I(km^3)      5.406e-06  8.192e-15  659873143   <2e-16 ***

#Q5 
#1)methode1
library(MASS)
lmf = stepAIC(lm(price~.,data = set))
#set$price ~ age + km + kop2 + ageop2
#Df Sum of Sq     RSS     AIC
#<none>                 91.391 -98.764
#- kop2    1     3.301  94.692 -94.661
#- ageop2  1     5.858  97.248 -90.079
#- km      1    28.140 119.531 -54.594
#- age     1    71.985 163.376  -0.848
#methode2  stepwise
lmf_b=lm(price~km,data=set); 
summary(lmf_b)
#mod=paste("~",paste("x",1:10,sep="",collapse="+"))
mod = "~price + age+km+TIA+ABS+extra2+kop1+kop2+kop3+ageop1+ageop2+ageop3"
st2=stepAIC(lmf_b,scope=list(lower=~1,upper=mod),
            direction="both")
st2$anova
#2)
summary(lmf)
qqnorm(rstudent(lmf),
       main=c("pval=", round(shapiro.test(rstudent(lmf))$p.value,2)))
qqline(rstudent(lmf))
max(rstudent(lmf))       # 4.364175
which.max(rstudent(lmf)) # plus grand residu  35
plot(lmf)

