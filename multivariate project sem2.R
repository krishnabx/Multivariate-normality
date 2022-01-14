

data=read.csv("C:/Users/user/Desktop//GDS.csv")
data
data[1:6,]
mean=apply(data,2,mean)
mean

par(mfrow=c(2,3))
hist(data$Household.sector)
hist(data$Private.Corporate.Sector)
hist(data$Public.Sector )
hist(data$Total)

par(mfrow=c(2,3))
boxplot(data$Household.sector)
boxplot(data$Private.Corporate.Sector)
boxplot(data$Public.Sector )
boxplot(data$Total)
plot(data)

x1=data$Household.sector
x2=data$Private.Corporate.Sector
x3=data$Public.Sector
x4=data$Total
y1=(x1-mean(x1))/sqrt(var(x1))
y2=(x2-mean(x2))/sqrt(var(x2))

y3=(x3-mean(x3))/sqrt(var(x3))
y4=(x4-mean(x4))/sqrt(var(x4))
y1
y2
y3
y4


par(mfrow=c(2,2))
qqnorm(y1)
qqline(y1)
qqnorm(y2)
qqline(y2)
qqnorm(y3)
qqline(y3)
qqnorm(y4)
qqline(y4)

x=rbind(x1,x2,x3)
x
mn=matrix(apply(x,1,mean),3,1)
mn

s=var(t(x))
sinv=solve(s)
sinv
f=rep(0,0)
j=1
for (j in 1:53)
{
d=x[,j]-mn
f[j]=t(d)%*%sinv%*%d
}

f

qqplot(qchisq(ppoints(53),df=x),f)
abline(0,1)

sinv
mu=0
cv=t(mn-mu)%*%sinv%*%(mn-mu)
cv

p=3
fcal=cv*(n-p)/((n-1)*p)
fcal
pval=1-pf(fcal,p,n-p)
pval


