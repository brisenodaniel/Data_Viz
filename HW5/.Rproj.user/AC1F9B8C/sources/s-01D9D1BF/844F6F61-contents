require(DAAG)
attach(possum)

#1
hist(age)
colnames(possum)
sex
hist(age, seq(0,9,1.5))

plot(density(age,na.rm=TRUE))
#2

hist(earconch)
boxplot(earconch~sex)

#3


pairs(~age+hdlngth+skullw+totlngth+taill+footlgth+earconch+eye+chest+belly, data=possum)
plot(hdlngth,skullw)
points(mean(hdlngth),mean(skullw),pch=2,col="red")