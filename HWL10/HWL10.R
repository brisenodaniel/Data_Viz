require(randomForest)
require(mice)
require(dplyr)
require(reshape2)
require(ggplot2)
require(EnvStats)

load('./Data/HW6.RData')

summary(df)
summary(demo)

ggplot(demo, aes(x=Prof_Score)) + geom_boxplot()
ggplot(demo, aes(x=Prof_Score)) + geom_histogram()

boxplot(demo$Prof_Score)


outliers_IQR = function(c) {
  c = na.omit(c)
  names(c) = NULL
  quantile_25 = quantile(c)[2]
  quantile_75 = quantile(c)[4]
  iqr = (quantile_75-quantile_25)
  iqr_l = quantile_25-1.5*iqr
  iqr_u = quantile_75+1.5*iqr
  w = which(c<iqr_l | c>iqr_u)
  
  list(idx = w,
       val = c[w],
       mu = mean(c),
       count = length(w),
       thres_l = iqr_l,
       thres_u = iqr_u
  )
}

outliers_SD = function(c) {
  c = na.omit(c)
  names(c) = NULL
  sc = scale(c)
  w = which(abs(sc)>3)
  
  list(
    idx = w,
    val = c[w],
    count = length(w),
    mu = mean(c),
    thres_l = mean(c)-3*sd(c),
    thres_u = mean(c)+3*sd(c)
  )
}


ols_SD = apply(demo[,6,drop=F],2,outliers_SD)
ols_IQR = apply(demo[,6,drop=F],2,outliers_IQR)  

out = rosnerTest(demo$Prof_Score,50)
out





###################################################### 2 ########################

#identify all missing variables in the annotation data frame

summary(df)
#Summary staistics determine that we have missing variables in Image_Quality and Smile

df$IQ_Miss =  factor(ifelse(is.na(df$Image_Quality),1,0))
df$Smile_Miss =  factor(ifelse(is.na(df$Smile),1,0))
colnames(df)
RFmod = randomForest(IQ_Miss~Gender + Adult + Face_Angle + Image_Color + Image_Type + Context + Multiface +
                       Race + Area, data=df)
predictTest = predict(RFmod)

sum(df$IQ_Miss != predictTest)/nrow(df)
prop.table(table(df$IQ_Miss))
#We can conclude that Image_Quality is likely MCAR

RFmod = randomForest(Smile_Miss~Gender + Adult + Face_Angle + Image_Color + Image_Type + Context + Multiface +
                       Race + Area, data=df)
predictTest = predict(RFmod)

sum(df$Smile_Miss != predictTest)/nrow(df)
prop.table(table(df$Smile_Miss))
# We can conclude that Smile is MCAR

#Listwise Deletion

df_del = filter(df, IQ_Miss == 0 & Smile_Miss == 0)

del_mod = glm(Area ~ Gender + Adult + Face_Angle + Image_Color +
      Image_Quality +Image_Type + Context + Multiface + Race + Smile, data=df_del, family=poisson )

summary(del_mod)



# Multiple Imputation

df = df %>% select(-IQ_Miss)
df = df %>% select(-Smile_Miss)
df$Smile = factor(df$Smile)
imp = mice(df, maxit =5, method= 'logreg')
df_complete = complete(imp, action='long', include = TRUE)

df_complete_mids = as.mids(df_complete)
fitimp = with(df_complete_mids,
              glm(Area ~ Gender + Adult + Face_Angle + Image_Color +
                    Image_Quality +Image_Type + Context + Multiface + Race + Smile ))
summary(pool(fitimp))








imp <- mice(df, maxit = 5, print =  FALSE, method = "logreg")
df_long = complete(imp, action="long", include = TRUE)
df_long_mids = as.mids(df_long)

fitimp = with(df_long_mids,
              lm(Area~Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race +Smile))

summary(pool(fitimp))
#summary(lm(Area~., data=df))


df_noNA = df[complete.cases(df),]
mod_noNA = lm(Area~Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race +Smile, data=df_noNA)
summary(mod_noNA)

