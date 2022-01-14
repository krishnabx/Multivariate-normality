

df=read.csv("C:\\Users\\user\\Desktop\\GDS.csv")
mean=apply(df,2,mean)
mean
sn=(n-1)*var(df1)/n
sn
y1= var(df1$Household.sector)
y1
y2=var(df1$Private.Corporate.Sector)
y2
y3=var(df1$Public.Sector )
y3
par(mfrow=c(2,3))
qqnorm(y1)
qqline(y1)
qqnorm(y2)
qqline(y2)
qqnorm(y3)
qqline(y3)

mn=matrix(apply(df1,1,mean),53,1)
mn
s=var(t(df1))
s





