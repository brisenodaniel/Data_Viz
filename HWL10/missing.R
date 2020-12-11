require(randomForest)
require(mice)
require(dplyr)
require(reshape2)
require(ggplot2)
require(EnvStats)

##################Load Data######################
rm(list=ls())

df = read.csv('./Data/Titanic_r.csv', stringsAsFactors = F)

##########################1######################

summary(df)
which(is.na(df$age))
mean(df$age)
mean(df$age, na.rm=T)
range(df$age)
range(df$age, na.rm=T)

#Pre-process
df = rename(df, colnames(df)[1] = class  )
df = rename(df, residence = Residence)
df = rename(df, gender = Gender)

df$gender = factor(df$gender)
df$residence = factor(df$residence)
df$class = factor(df$class)

df = df %>% select(survived, class, residence, age, sibsp, parch, fare, gender)

x = na.omit(df$fare)

df = df[-which(is.na(df$fare)),] #Eliminate the row with NA fare

##########################2######################

df$age_mis = factor(ifelse(is.na(df$age),1,0))

df %>% group_by(age_mis) %>% summarize(mean(fare))
table(df$age_mis,df$residence)

chisq.test(df$gender,df$age_mis)
chisq.test(df$residence,df$age_mis)
chisq.test(df$class,df$age_mis)
t.test(df$fare~df$age_mis)
t.test(df$sibsp~df$age_mis)
t.test(df$parch~df$age_mis)

##########################3######################
RFmod = randomForest(age_mis~gender+residence+class+fare+sibsp+parch, data=df)
predictTest = predict(RFmod)

sum(df$age_mis != predictTest)/nrow(df)
prop.table(table(df$age_mis))


##########################4######################
df_lw = df[complete.cases(df),]

fullmod = glm(survived~gender+residence+class+fare+sibsp+parch, data=df, family='binomial')
lwmod = glm(survived~gender+residence+class+fare+sibsp+parch, data=df_lw, family='binomial')

summary(fullmod)
summary(lwmod)

##########################5######################
df = df %>% select(-age_mis)

imp = mice(df, maxit=0)
predM = imp$predictorMatrix
predM[,c('sibsp')] = 0
predM

# meth = imp$method
# meth
# 
# meth['age'] = "norm"
# meth
# meth['age'] = "pmm"

imp2 <- mice(df, maxit = 5, predictorMatrix = predM, method = meth, print =  FALSE)

imp2$imp
head(imp2$imp$age)

dflong = complete(imp2, action="long", include = TRUE)

dflong_mids = as.mids(dflong)

fitimp = with(dflong_mids,
              glm(survived~gender+residence+class+fare+sibsp+parch+age, family='binomial'))

summary(pool(fitimp))

summary(glm(survived~gender+residence+class+fare+sibsp+parch+age, data=df, family='binomial'))


########Outliers#######
#Visualize
df_long = melt(df, measure.vars = c('age','sibsp','parch','fare'), variable.name = 'numvar', value.name = 'val')
ggplot(df_long, aes(x=val)) + geom_boxplot() + facet_wrap(~numvar,scales = 'free')
ggplot(df_long, aes(x=val)) + geom_histogram() + facet_wrap(~numvar,scales = 'free')
hist(log(df$fare))

#Numerically Calcualte

ol_SD = function(z){
  z = na.omit(z)
  sz = scale(z)
  w = which(abs(sz)>3)
  list(idx = w,
       val = z[w],
       mu = mean(z),
       thres_l = mean(z) - 3*sd(z),
       thres_u = mean(z) + 3*sd(z)
  )
}

ol_IQR = function(z){
  z = na.omit(z)
  names(z) = NULL
  p25 = quantile(z)[2]
  p75 = quantile(z)[4]
  iqr = (p75-p25)
  iqr_l = p25-1.5*iqr
  iqr_u = p75+1.5*iqr
  w = which(z<iqr_l | z>iqr_u)
  
  list(idx = w,
       val = z[w],
       mu = mean(z),
       thres_l = mean(z) - 3*sd(z),
       thres_u = mean(z) + 3*sd(z)
  )
}

ols_SD = apply(df[,4:7],2,ol_SD)
ols_IQR = apply(df[,4:7],2,ol_IQR)

#####Rosners Test
out = rosnerTest(df$fare,100)
out
