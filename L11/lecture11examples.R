require(DAAG)
require(ggplot2)
require(lme4)
require(dplyr)

#########################################
###########Antigua Data##################
#########################################

################################################
head(ant111b)
# ggplot(ant111b, aes(x = harvwt, y = site, col=  site)) + geom_point(size=2) + xlab("harvest weight")


######################1#########################

summary(lm(harvwt~1,data=ant111b))

summary(lm(harvwt~site,data=ant111b))


###ANOVA analyses
summary(aov(harvwt~site,data = ant111b))

######################2#########################
df = ant111b %>% mutate(grandmu = mean(harvwt)) %>% group_by(site) %>% arrange(site) %>%
  mutate(sitemu = mean(harvwt)) %>% mutate(siteres = sitemu-grandmu, parcres = harvwt-sitemu) %>% ungroup()

View(df)

ggplot(df, aes(x = harvwt, y = site, col=  site)) + geom_point(size=2) + 
  geom_point(aes(x=sitemu), size=3, shape = 7) + xlab("harvest weight") + 
  geom_vline(xintercept = mean(df$harvwt), linetype = 'dashed', size=1)


######################2#########################
summary(aov(harvwt~Error(site),data = ant111b))

######################3#########################
###Random effects using lmer
ant111b.lmer = lmer(harvwt ~ 1 + (1|site), data = ant111b)
summary(ant111b.lmer)

######################4#########################
###Random effects using lmer
fitted(ant111b.lmer)
round(unique(fitted(ant111b.lmer)),2)

#Random Effects
ranef(ant111b.lmer)

#Plot density of random effects
r = unlist(ranef(ant111b.lmer)$site)
plot(density(r))

######################5#########################
###Add fixed effect
ant111b.lmer2 = lmer(harvwt ~ plot + (1|site), data = ant111b)
summary(ant111b.lmer2)

#Fitted values and random effects
fitted(ant111b.lmer2)
ranef(ant111b.lmer2)

#Plot density of random effects
r = unlist(ranef(ant111b.lmer2)$site)
plot(density(r))

######################6#########################
#CIs

confint(ant111b.lmer)
confint(ant111b.lmer2)

#########################################
###########Science Data##################
#########################################
head(science)

######################7#########################
###Mixed model 
science.lmer = lmer(like ~ sex + PrivPub + (1|school) + (1|school:class),
                    data = science, na.action = na.exclude)

summary(science.lmer)
confint((science.lmer))

######################8#########################
###Mixed model with school level removing
science.lmer2 = lmer(like ~ sex + PrivPub + (1|school:class),
                    data = science, na.action = na.exclude)

summary(science.lmer2)
confint((science.lmer2))

#require(lmerTest)

######################9#########################
#Plot density of random effects
r = unlist(ranef(ant111b.lmer2)$site)
plot(density(r))

###Class size vs random effects stratified by shool type
n = c(table(science$Class)) # Number of students in the class
schooltype = science %>% group_by(Class) %>% summarize(type = PrivPub[1]) #School type per class

randeff = unlist(ranef(science.lmer2)[["school:class"]]) #Random effect of class

df = data.frame(N = n, type = schooltype$type, RE = randeff)
ggplot(df, aes(x=N,y=RE,shape=type, col = type)) + geom_point() + geom_smooth()



######################10#########################
###Faulty analysis 1
science.lmer3 = lmer(like ~ sex + PrivPub + (1|school),
                    data = science, na.action = na.exclude)

summary(science.lmer3)

######################11#########################

###Faulty analysis 2
science.lm = lm(like ~ sex + PrivPub, data = science, na.action = na.exclude)
CI95 = confint((science.lm))
CI95


#########################################
###########Sleep Data####################
#########################################

###Random intercept model
sleep.lmer = lmer(Reaction ~ Days + (1|Subject), data = sleepstudy, na.action = na.exclude)

cc = coef(sleep.lmer)[[1]]
pred = apply(cc,1,function(z) z[1] + z[2]*0:9)
predvec = c(pred)

sleepstudy$pred  = predvec

ggplot(sleepstudy, aes(x = Days)) + 
       geom_point(aes(y = Reaction)) + geom_line(aes(y = pred))+
       facet_wrap(~Subject,nrow = 3)

###Random slope model
sleep.lmer2 = lmer(Reaction ~ Days + (Days|Subject),data = sleepstudy, na.action = na.exclude)

cc = coef(sleep.lmer2)[[1]]
pred = apply(cc,1,function(z) z[1] + z[2]*0:9)
predvec = c(pred)

sleepstudy$pred  = predvec

ggplot(sleepstudy, aes(x = Days)) + 
  geom_point(aes(y = Reaction)) + geom_line(aes(y = pred))+
  facet_wrap(~Subject,nrow = 3)

###Model comparison
m1 = lm(Reaction ~ Days, sleepstudy)
m2 = sleep.lmer = lmer(Reaction ~ Days + (1|Subject), sleepstudy, REML = F)
m3 = sleep.lmer = lmer(Reaction ~ Days + (Days|Subject), sleepstudy, REML = F)

aic(m1,m2,m3)
anova(m2,m3)

###Results of best sleep model
summary(sleep.lmer2)

###Calculation of variance partition coef function
u0sq = data.frame(VarCorr(sleep.lmer2))[1,4]
u1sq = data.frame(VarCorr(sleep.lmer2))[2,4]
u01 = data.frame(VarCorr(sleep.lmer2))[3,4]
uwsq = data.frame(VarCorr(sleep.lmer2))[4,4]

x = seq(min(sleepstudy$Days), max(sleepstudy$Days),length.out=100)
VPC = (u0sq + 2*u01*x +u1sq*x^2)/(u0sq + 2*u01*x +u1sq*x^2 + uw)

plot(x,VPC, type = "l", xlab = "Days")

#########################################
#################ITS#####################
#########################################

###Level change only
m1 = lm(meancounts~Day + IVflag, data = pfa_1hm)
summary(m1)

pfa_1hm$m1 = fitted(m1)
on = pfa_1hm$Day[min(which(pfa_1hm$IVflag==1))]-0.5

ggplot(pfa_1hm, aes(x=Day)) + geom_point(aes(y=meancounts)) + 
      geom_line(aes(y=m1, group = IVflag )) + 
      geom_vline(xintercept = on, col = "red", linetype = "dashed")

###Level change + slope change
m2 = lm(meancounts~Day + IVflag + IVflagm, data = pfa_1hm)
summary(m2)

pfa_1hm$m2 = fitted(m2)
on = pfa_1hm$Day[min(which(pfa_1hm$IVflag==1))]-0.5

ggplot(pfa_1hm, aes(x=Day)) + geom_point(aes(y=meancounts)) + 
  geom_line(aes(y=m2, group = IVflag )) + 
  geom_vline(xintercept = on, col = "red", linetype = "dashed")

###Mixed effects random intercept model 
m3 = lmer(meancounts ~ IVflag  + (1|HomeID), data = pfa_all)
summary(m3)

###Mixed effects random slope-ish model
m4 = lmer(meancounts ~ Day + IVflag + IVflagm  + (Day|HomeID), data = pfa_all)
summary(m4)