a=((sqrt(5)+1)/2)^2
nb=sqrt(a)
rm(list=objects())
setwd("~/Desktop")

v1=c(-1,3.2,-2,8)
v2=-2:6
v6=2*v2-3
v7=v6[length(v6)-(2:0)]

df=read.table("hFE.csv", sep=";", dec=",",skip=1)
df
head(df)
dim(df)
summary(df)
apply(df,2,sd)
pairs(df)
cor(df[,])

X=as.matrix(df)
X[,1]=1
Y=cbind(df[,1])
theta.est=solve(t(X)%*%X)%*%t(X)%*%Y

n=length(Y)
p=dim(X)[2]
sigma.est=sqrt(sum(((X)%*%theta.est-Y)^2))/(n-p)
v=solve(t(X)%*%X)*sigma.est^2
stddev=sqrt(diag(v))

model1=lm(Y~V1+V2+V3, data=df)
summary(model1)
