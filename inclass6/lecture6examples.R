rm(list=ls())
source("C:\\Dropbox\\dbpath.R")
wd = paste(dbpath,"Teaching\\CS614\\Lectures\\Lecture 6 - ggplot\\ExampleScripts",sep = "")
setwd(wd)

#Load Data
load(".\\Data\\twdkills.RData")

#____________________1________________________
#Reshape data
require(reshape2)
kills.long = melt(kills, id.vars = "Season", variable.name = "Character", value.name = "nKills")
kills.long
# kills.long = melt(kills, id.vars = "Season", measure.vars = colnames(kills)[1:3],variable.name = "Character", value.name = "nKills")
# kills.long

#____________________2________________________
require(ggplot2)
ggplot(kills.long, aes(x = nKills)) + geom_histogram()

ggplot(kills.long, aes(x = nKills)) +
geom_histogram(col = "red", fill="blue", binwidth = 10)

ggplot(kills.long, aes(x = nKills)) + 
geom_histogram(fill = "orange", alpha = 0.25, breaks = seq(5, 85, 13.5))

ggplot(kills.long, aes(x = nKills)) +
  geom_histogram(aes(y=..density..),col = "red", fill = "blue",binwidth = 10)

#____________________3________________________
#Layering geoms
ggplot(kills.long, aes(x = nKills)) +
geom_histogram(aes(y=..density..),col = "red", fill = "blue",binwidth = 10)+
geom_density(col = "red", size = 2)

medval = median(kills.long$nKills,na.rm = T)
ggplot(kills.long, aes(x = nKills)) +
  geom_histogram(aes(y=..density..),col = "red", fill = "blue",binwidth = 10)+
  geom_density(col = "red", size = 2)+
  geom_vline(xintercept = medval, col = "purple",linetype = "dashed", size = 1.5)

#____________________4________________________
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line()
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_point()

#____________________5________________________
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line()
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_point()
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(aes(group = Character))
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(aes(col = Character))
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(aes(linetype = Character))

#____________________6________________________
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(aes(color = Character))
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(color = "green")
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(aes(group = Character), col="green")

#____________________7________________________
kills.long$Gender = ifelse(kills.long$Character %in% c("Rick","Morgan", "Glen", "Daryl", "Carl"),"M","F")

ggplot(kills.long, aes(x = Season, y = nKills)) + 
  geom_line(aes(group = Character, color = Gender)) + 
  geom_point(aes(shape = Character, color = Gender))+
  scale_shape_manual(values=1:8)

ggplot(kills.long, aes(x = Season, y = nKills)) + 
  geom_line(aes(color = Gender)) + 
  geom_point(aes(shape = Character, color = Gender))+
  scale_shape_manual(values=1:8)

ggplot(kills.long, aes(x = Season, y = nKills)) + 
  geom_line(aes(group = Character, color = Gender)) + 
  geom_point(aes(shape = Character, color = Gender),size=4)+
  scale_shape_manual(values=1:8)

#____________________8________________________
# _Example of adding title
ggplot(kills.long, aes(x = nKills)) +
geom_histogram(col = "red", fill="blue", binwidth = 10)+
labs(title = "Walking Dead S1-S6",x = "# of Kills", y = "Frequency")

#____________________9________________________
ggplot(kills.long, aes(x = Season, y = nKills)) + 
geom_line(aes(color = Gender, group = Character))

ggplot(kills.long, aes(x = Season, y = nKills)) + 
geom_line(aes(color = Gender, group = Character))+
scale_colour_manual(values = c("green","black"))

ggplot(kills.long, aes(x = Season, y = nKills)) + 
geom_line(aes(color = Gender, group = Character))+
scale_colour_manual(values = c("green","black"), breaks = c("M","F"))

ggplot(kills.long, aes(x = Season, y = nKills)) + 
geom_line(aes(color = Gender, group = Character))+
scale_colour_manual(values = c("green","black"), breaks = c("M","F"),labels = c("Men", "Women"))

#____________________10________________________
ggplot(kills.long, aes(x = Season, y = nKills)) + 
geom_line(aes(color = Gender, group = Character)) + 
geom_point(aes(shape = Character, color = Gender))+
scale_shape_manual(values= c(1,2,5,3,4,16,17,18))+
scale_colour_manual(values = c("green","black"))

ggplot(kills.long, aes(x = nKills)) + 
geom_histogram(aes(fill = Gender),col = "yellow", binwidth = 10)

ggplot(kills.long, aes(x = nKills)) + 
geom_histogram(aes(fill = Gender),col = "yellow", binwidth = 10)+
scale_fill_manual(values = c("blue","green"))

ggplot(kills.long, aes(x = nKills)) + 
geom_histogram(aes(fill = Gender),col = "yellow", binwidth = 10,position = "dodge")+
scale_fill_manual(values = c("blue","green"))

#____________________11________________________
ggplot(kills.long, aes(x = Season, y = nKills)) +
geom_line(aes(group = Character, color = Gender))+
scale_x_continuous(limits = c(0.5,7.5),breaks = 1:6, labels = c("","2","","4","","6"))

ggplot(kills.long, aes(x=Gender,y = nKills)) + geom_boxplot() +
scale_x_discrete(labels = c("Women","Men"))

#____________________12________________________
ggplot(kills.long, aes(x = nKills)) + 
geom_histogram(col = "red", fill="blue", binwidth = 10)+
theme(axis.text = element_text(size=18))

ggplot(kills.long, aes(x = nKills)) + 
geom_histogram(col = "red", fill="blue", binwidth = 10)+
theme(axis.text = element_text(size=18, colour = "green", angle = 45),
      axis.title.y = element_text(family = "mono",size = 24)) 

ggplot(kills.long, aes(x = Season, y = nKills)) +
geom_line(aes(group = Character, color = Gender)) +
scale_colour_discrete(name = "New Title")+
theme(legend.position = c(0.15, 0.85))
  
ggplot(kills.long, aes(x = Season, y = nKills)) +
geom_line(aes(group = Character, color = Gender)) + 
theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

#____________________13________________________
ggplot(kills.long, aes(x=Season, y = nKills)) + geom_point() + geom_smooth()

ggplot(kills.long, aes(x=Season, y = nKills)) + geom_point() + geom_smooth(span=0.25)

ggplot(kills.long, aes(x=Season, y = nKills)) + geom_point() + geom_smooth(method = "lm")

ggplot(kills.long, aes(x=Season, y = nKills)) + geom_point() + geom_smooth(method = "lm", se=F, col = 'black')


#____________________14________________________
ggplot(kills.long, aes(x=Season, y = nKills)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, col = 'black')+
  facet_wrap(~Character)

ggplot(kills.long, aes(x=Season, y = nKills)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, col = 'black')+
  facet_wrap(~Character, scales='fixed')


ggplot(kills.long, aes(x=Season, y = nKills)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, col = 'black')+
  facet_wrap(~Gender)

ggplot(kills.long, aes(x=Season, y = nKills)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, col = 'black')+
  facet_wrap(~Gender, nrow = 2)
