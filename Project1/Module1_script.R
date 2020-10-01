require("dplyr")

tips_df = read.csv("./Data/tips.csv")

attach(tips_df)

colnames(tips_df)
plot(TIP,TOTBILL)

smoker = tips_df[SMOKER==1,]
n_smoker =  tips_df[SMOKER==0,]
female = tips_df[SEX==1,]
male = tips_df[SEX==0,]


m_smoker = tips_df[SEX==0 & SMOKER==1,]
f_smoker = tips_df[SEX==1 & SMOKER==1,]
m_nsmoker = tips_df[SEX==0 & SMOKER==0,]
f_nsmoker = tips_df[SEX==1 & SMOKER==0,]



plot(smoker$TIP, smoker$TOTBILL)
plot(n_smoker$TIP, n_smoker$TOTBILL)
plot(female$TIP, female$TOTBILL)
plot(male$TIP, male$TOTBILL)

plot(m_smoker$TIP, m_smoker$TOTBILL)
plot(f_smoker$TIP, f_smoker$TOTBILL)
plot(m_nsmoker$TIP, m_nsmoker$TOTBILL)
plot(f_nsmoker$TIP, f_nsmoker$TOTBILL)

boxplot((smoker$TIP/smoker$TOTBILL),(n_smoker$TIP/n_smoker$TOTBILL),
        names=c("s","ns")) 

boxplot((female$TIP/female$TOTBILL),(male$TIP/male$TOTBILL),
        names=c("f",'m'))

boxplot((m_smoker$TIP/m_smoker$TOTBILL),(f_smoker$TIP/f_smoker$TOTBILL),
        names=c("ms","fs"))

boxplot((m_nsmoker$TIP/m_nsmoker$TOTBILL), (f_nsmoker$TIP/f_nsmoker$TOTBILL), 
        names=c("mns","fns"))


med <- median(TIP)

tipsD = tips_df[TIME == 0,]
tipsN = tips_df[TIME == 1,]

thurD =tipsD[tipsD$DAY==3,]$TIP
thurN = tipsN[tipsN$DAY==3,]$TIP
friD = tipsD[tipsD$DAY==4,]$TIP

friN = tipsN[tipsN$DAY==4,]$TIP
satD = tipsD[tipsD$DAY==5,]$TIP
satN = tipsN[tipsN$DAY==5,]$TIP
sunD = tipsN[tipsD$DAY==6,]$TIP
sunN = tipsN[tipsN$DAY==6,]$TIP

thurD
boxplot(thurD,thurN,friD,friN,satD,satN,sunD,sunN,at=c(1,2,4,5,7,8,10,11))


