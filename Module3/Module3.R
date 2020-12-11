require(ggplot2)
require(lme4)
require(dplyr)
require(lmerTest)

########## 1 #############################
df <- read.csv('./Data/politeness_data.csv')

mod1 <- lmer(frequency ~ gender+attitude+(1|subject), data=df, REML = F)
summary(mod1)

################## 2 ########################

mod2 <- lmer(frequency~ gender + attitude + (attitude|subject), data=df, REML = F)
summary(mod2)


##################### 3 ##############

mod3 <- lmer(frequency ~ gender + attitude +(1|subject)+(1|scenario), data = df, REML = F)
summary(mod3)




# Q1:
  # Mod1: Moderatley likely that gender and additude are good predictor variables
  # Since p values have ** sig. code
  #error in genderM is 0.1996488 of fixed effect estimate
  #error in additudepol is 0.3300876 of fixed effect estimate
  #negative value indicates that if genderM has positive value, frequency will be lower
  #
# Mod2: moderately likey gender and additude are good predictor vars
  #err genderM:0.1903609 of fixed effect estimate
#err attitude:0.3313061 of fixed effect estimate
# Mode3: gender moderatley sig. additude very significant
# gender err 0.1936396 of fixed effect estimate
# attitude err 0.2831499 of fixed effect estimate






#Q2
#mod1 
603.9/(603.9+850.9)
# we see that ~42% of the variance can be attributed to subject
# with ~58% of the variance due to unknown factors
#mod3
615.6/sum(c(219.5,615.6,645.9))
219.5/sum(c(219.5,615.6,645.9))
219.5+ 645.9 
# we see that ~42% of the variance can still be attributed to subject, as we expect
# now, we can attribute ~15% of the variance to scenario
# Additionally, we can see that var(scenario)+var(residual) is nearly the residual variance
#in model 1. Indicating that some of the residual variance due to unkown factors in mod 1
# was due to scenario


#Q3 
AIC(mod1,mod2,mod3)
anova(mod1,mod2,mod3)

#mod1 and mod2 are not found to have a statisitcally significant difference by ANOVA,
#but mod3 is different with high significance,
#and ANOVA finds mod3 to be favorable.

#AIC also favors mod3


#Q4
rmod1 <- unlist(ranef(mod1)$subject)
plot(density(rmod1))

#mod1 has subject random effect distributed approximately normally. There is a hump 
# to the left of the distribution, but it does not appear to be significant

rmod3_sub <- unlist(ranef(mod3)$subject)
plot(density(rmod3_sub))

rmod3_sce <- unlist(ranef(mod3)$scenario)
plot(density(rmod3_sce))
#a bit more unsure on this one, the data seems skewed to the right, but it looks approx. normal
