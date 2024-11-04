# Music Personality Creativity Study
# University of Cambridge
# Department of Psychology

# before you do anything, go to Session > Set Working Directory > to Source File Location

# install packages necessary
install.packages("car")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("stargazer")
install.packages("ggpubr")
install.packages("RColorBrewer")
install.packages("cocor")
install.packages("psych")
install.packages("corrplot")
install.packages("scales")
install.packages("effsize")
install.packages("ez")

# load packages necessary
library(car)
library(ggplot2)
library(reshape2)
library(stargazer)
library(ggpubr)
library(RColorBrewer)
library(cocor)
library(psych)
library(scales)
library(corrplot)
library(effsize)
library(ez)

------

## load neda dataset (Music Personality Creativity)
nedaDataRaw <- read.csv("nedaDataComplete.csv", header=T, stringsAsFactors=T)
dataLegend <- read.csv("legend.csv", header=T)

# check gender and age. Remove non male/female, and check age range to use for MU restriction
unique(nedaDataRaw$Gender)
nedaData <- subset(nedaDataRaw, Gender=="Male"|Gender=="Female")
range(nedaData$Age) # min 18, max 79
dim(nedaData) # 273 total 

#mean(nedaData$Age, na.rm = TRUE)
#sd(nedaData$Age, na.rm = TRUE)

with(nedaData, tapply(Gender, Gender, length)) / sum(with(nedaData, tapply(Gender, Gender, length)), na.rm=T)
with(nedaData, tapply(Gender, Gender, length)) 

#Gender_levels <- levels(nedaData$Gender)
#print(Gender_levels)

------

# load MU data (Musical Universe)
MUdata <- read.csv("musicalUniverse.csv", header = T)
MUdata <- MUdata[,c(21,22,41:45,81:89)]
names(MUdata) <- c("Gender", "Age", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism",
                   "MusicSum", "Mellow", "Unpretentious", "Sophisticated", "Intense", "Contemporary", "Arousal", "Valence", "Depth")
MUdata$Gender <- factor(MUdata$Gender, labels = c("Female", "Male", "Transgender", "Other Non Binary", "I Rather Not Say"))
dim(MUdata) # originally have 64815 people
MUdata <- subset(MUdata, Gender=="Male"|Gender=="Female")
dim(MUdata) # after removing non-male and non-female answers, have 62161 people
MUdata <- subset(MUdata, Age<80&Age>17)
dim(MUdata) # after limiting age range to match nedaData, have 47540 people. THIS IS THE FINAL MU DATASET TO BE USED.

#mean(MUdata$Age, na.rm = TRUE)
#sd(MUdata$Age, na.rm = TRUE)

with(MUdata, tapply(Gender, Gender, length)) / sum(with(MUdata, tapply(Gender, Gender, length)), na.rm=T)
with(MUdata, tapply(Gender, Gender, length)) 

Gender_levels <- levels(MUdata$Gender)
print(Gender_levels)

###################### AIM 1 ###################################################

## Gender Boxplots

# First, convert two datasets to long format and combine them to one
MUdata$Participant <- 1:dim(MUdata)[1] # need to give unique IDs here since it doesn't have prolific IDs
MUdatalong <- melt(MUdata[,c(1,8:17)], id.vars=c("Participant","Gender"))
MUdatalong$Dataset <- "MU"

names(nedaData)
nedaDatalong <- melt(nedaData[,c(1,2,4:12)], id.vars=c("ProlificID","Gender"))
nedaDatalong$Dataset <- "MPC"
names(nedaDatalong)[1] = "Participant"

allDatalong <- rbind(MUdatalong, nedaDatalong)

Gender_levels <- levels(allDatalong$Gender)
print(Gender_levels)

------

# to create correlation matrices...
install.packages("Hmisc")
library("Hmisc")
MU_aim1cors <- rcorr(as.matrix(MUdata[,2:16]))
write.csv(MU_aim1cors$r, "MU_aim1cors_rvals.csv")
write.csv(MU_aim1cors$P, "MU_aim1cors_pvals.csv")

nedaDataRESORTED <- nedaData[,c(names(MUdata[,2:16]))]
MPC_aim1cors <- rcorr(as.matrix(nedaDataRESORTED))
write.csv(MPC_aim1cors$r, "MPC_aim1cors_rvals.csv")
write.csv(MPC_aim1cors$P, "MUPCaim1cors_pvals.csv")

------

# (1) Gender and Mellow
ggplot(subset(allDatalong, variable=="Mellow"), aes(x=Dataset,y=value, col=Gender))+
  geom_boxplot() +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Mellow") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Mellow")))
ezANOVA(subset(allDatalong, variable=="Mellow"), dv=value, wid=Participant, between=.(Gender, Dataset), 
        observed=.(Gender, Dataset))
ggsave("boxplot_GenderxMellow_raw_new.png", width=5, height=5)

# Mellow scores were significantly different between male and females (F(1,47809)=90.91, p<0.001),
# but this difference was not significantly different between datasets (F(1,47809)=0.09, p=0.77)

mean(MUdata$Mellow, na.rm = TRUE)
mean(nedaData$Mellow, na.rm = TRUE)

sd(MUdata$Mellow, na.rm = TRUE)
sd(nedaData$Mellow, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Mellow"), FUN=mean, na.rm=T)
aggregate(value ~ Dataset, data=subset(allDatalong, variable=="Mellow"), FUN=mean, na.rm=T)

# Mellow - Mean and SD
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Mellow"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Mellow"), FUN=sd, na.rm=T)

# Mellow - t-tests for individual dataset gender 
t.test(Mellow ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Mellow ~ Gender, data=MUdata, var.equal=TRUE)

#Mellow - Cohen'd d for individual dataset gender
cohen.d(Mellow ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Mellow ~ Gender, data=MUdata, var.equal=TRUE)

------

# (2) Gender and Unpretentious 
ggplot(subset(allDatalong, variable=="Unpretentious"), (aes(x=Dataset,y=value, col=Gender))) +
  geom_boxplot() +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Unpretentious") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Unpretentious")))
ezANOVA(subset(allDatalong, variable=="Unpretentious"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxUnpretentious_raw_new.png", width=5, height=5)

mean(MUdata$Unpretentious, na.rm = TRUE)
mean(nedaData$Unpretentious, na.rm = TRUE)

sd(MUdata$Unpretentious, na.rm = TRUE)
sd(nedaData$Unpretentious, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Unpretentious"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Unpretentious"), FUN=mean, na.rm=T)

# Unpretentious - t-tests for individual dataset gender 
t.test(Unpretentious ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Unpretentious ~ Gender, data=MUdata, var.equal=TRUE)

#Unpretentious - Cohen'd d for individual dataset gender
cohen.d(Unpretentious ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Unpretentious ~ Gender, data=MUdata, var.equal=TRUE)

------

# (3) Gender and Intense 
ggplot(subset(allDatalong, variable=="Intense"), (aes(x=Dataset,y=value, col=Gender))) + 
  geom_boxplot() +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Intense") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Intense")))
ezANOVA(subset(allDatalong, variable=="Intense"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxIntense_raw_new.png", width=5, height=5)

mean(MUdata$Intense, na.rm = TRUE)
mean(nedaData$Intense, na.rm = TRUE)

sd(MUdata$Intense, na.rm = TRUE)
sd(nedaData$Intense, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Intense"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Intense"), FUN=mean, na.rm=T)

#Intense Mean & SD
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Intense"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Intense"), FUN=sd, na.rm=T)

# Intense - t-tests for individual dataset gender 
t.test(Intense ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Intense ~ Gender, data=MUdata, var.equal=TRUE)

#Intense - Cohen's d for individual dataset gender
cohen.d(Intense ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Intense ~ Gender, data=MUdata, var.equal=TRUE)

------

# (4) Gender and Sophisticated 
ggplot(subset(allDatalong, variable=="Sophisticated"), (aes(x=Dataset,y=value, col=Gender))) +
  geom_boxplot() +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Sophisticated") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Sophisticated")))
ezANOVA(subset(allDatalong, variable=="Sophisticated"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxSophisticated_raw_new.png", width=5, height=5)

mean(MUdata$Sophisticated, na.rm = TRUE)
mean(nedaData$Sophisticated, na.rm = TRUE)

sd(MUdata$Sophisticated, na.rm = TRUE)
sd(nedaData$Sophisticated, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Sophisticated"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Sophisticated"), FUN=mean, na.rm=T)

# Sophisticated - t-tests for individual dataset gender 
t.test(Sophisticated ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Sophisticated ~ Gender, data=MUdata, var.equal=TRUE)

#Sophisticated - Cohen's d for individual dataset gender
cohen.d(Sophisticated ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Sophisticated ~ Gender, data=MUdata, var.equal=TRUE)

------

# (5) Gender and Contemporary 
ggplot(subset(allDatalong, variable=="Contemporary"), (aes(x=Dataset,y=value, col=Gender))) +
  geom_boxplot() +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Contemporary") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Contemporary")))
ezANOVA(subset(allDatalong, variable=="Contemporary"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxContemporary_raw_new.png", width=5, height=5)

mean(MUdata$Contemporary, na.rm = TRUE)
mean(nedaData$Contemporary, na.rm = TRUE)

sd(MUdata$Contemporary, na.rm = TRUE)
sd(nedaData$Contemporary, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Contemporary"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Contemporary"), FUN=mean, na.rm=T)

# Contemporary - t-tests for individual dataset gender 
t.test(Contemporary ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Contemporary ~ Gender, data=MUdata, var.equal=TRUE)

#Contemporary - Cohen's d for individual dataset gender
cohen.d(Contemporary ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Contemporary ~ Gender, data=MUdata, var.equal=TRUE)

------

# (6) Gender and Arousal 
ggplot(subset(allDatalong, variable=="Arousal"), (aes(x=Dataset,y=value, col=Gender))) +
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Arousal") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Arousal")))
ezANOVA(subset(allDatalong, variable=="Arousal"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxArousal_raw_new.png", width=5, height=5)

mean(MUdata$Arousal, na.rm = TRUE)
mean(nedaData$Arousal, na.rm = TRUE)

sd(MUdata$Arousal, na.rm = TRUE)
sd(nedaData$Arousal, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Arousal"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Arousal"), FUN=mean, na.rm=T)

# Arousal Mean & SD
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Arousal"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Arousal"), FUN=sd, na.rm=T)

# Arousal - t-tests for individual dataset gender 
t.test(Arousal ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Arousal ~ Gender, data=MUdata, var.equal=TRUE)

#Arousal - Cohen's d for individual dataset gender
cohen.d(Arousal ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Arousal ~ Gender, data=MUdata, var.equal=TRUE)

------

# (7) Gender and Depth 
ggplot(subset(allDatalong, variable=="Depth"), (aes(x=Dataset,y=value, col=Gender)))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Depth") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Depth")))
ezANOVA(subset(allDatalong, variable=="Depth"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxDepth_raw_new.png", width=5, height=5)

mean(MUdata$Depth, na.rm = TRUE)
mean(nedaData$Depth, na.rm = TRUE)

sd(MUdata$Depth, na.rm = TRUE)
sd(nedaData$Depth, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Depth"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Depth"), FUN=mean, na.rm=T)

# Depth Mean and SD
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Depth"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Depth"), FUN=sd, na.rm=T)

# Depth - t-tests for individual dataset gender 
t.test(Depth ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Depth ~ Gender, data=MUdata, var.equal=TRUE)

# Depth - Cohen's d for individual dataset gender
cohen.d(Depth ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Depth ~ Gender, data=MUdata, var.equal=TRUE)

------

# (8) Gender and Valence 
ggplot(subset(allDatalong, variable=="Valence"), (aes(x=Dataset,y=value, col=Gender)))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("Valence") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Valence")))
ezANOVA(subset(allDatalong, variable=="Valence"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxValence_raw_new.png", width=5, height=5)

mean(MUdata$Valence, na.rm = TRUE)
mean(nedaData$Valence, na.rm = TRUE)

sd(MUdata$Valence, na.rm = TRUE)
sd(nedaData$Valence, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Valence"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Valence"), FUN=mean, na.rm=T)

#Valence Mean and SD
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Valence"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Valence"), FUN=sd, na.rm=T)

# Valence - t-tests for individual dataset gender 
t.test(Valence ~ Gender, data=nedaData, var.equal=TRUE)
t.test(Valence ~ Gender, data=MUdata, var.equal=TRUE)

# Valence - Cohen's d for individual dataset gender
cohen.d(Valence ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(Valence ~ Gender, data=MUdata, var.equal=TRUE)

------

# (9) Gender and MusicSum 
ggplot(subset(allDatalong, variable=="MusicSum"), (aes(x=Dataset,y=value, col=Gender)))+
  geom_boxplot() +
  stat_summary(fun.y="mean", position = position_nudge(x = c(-0.185, 0.19))) +
  ylab("MusicSum") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() +
  theme(legend.position = "none")  # Remove legend
#anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="MusicSum")))
ezANOVA(subset(allDatalong, variable=="MusicSum"), dv=value, wid=Participant, between=.(Gender, Dataset), observed=.(Gender, Dataset))
ggsave("boxplot_GenderxMusicSum_raw_new2.png", width=5, height=5)

mean(MUdata$MusicSum, na.rm = TRUE)
mean(nedaData$MusicSum, na.rm = TRUE)

sd(MUdata$MusicSum, na.rm = TRUE)
sd(nedaData$MusicSum, na.rm = TRUE)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="MusicSum"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="MusicSum"), FUN=mean, na.rm=T)

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="MusicSum"), FUN=mean, na.rm=T)
aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="MusicSum"), FUN=sd, na.rm=T)

# MusicSum - t-tests for individual dataset gender 
t.test(MusicSum ~ Gender, data=nedaData, var.equal=TRUE)
t.test(MusicSum ~ Gender, data=MUdata, var.equal=TRUE)

# MusicSum - Cohen's d for individual dataset gender
cohen.d(MusicSum ~ Gender, data=nedaData, var.equal=TRUE)
cohen.d(MusicSum ~ Gender, data=MUdata, var.equal=TRUE)

##### Age Correlations #####----------------------------------------------------

# (1) AGE X MELLOW
(tmpMU <- with(MUdata, cor.test(x=Age, y=Mellow))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Mellow))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 3.1399, p-value = 0.0017
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Mellow), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Mellow), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Mellow), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Mellow), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.32, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexMellow_raw_new.png", width=5, height=5)

------

# (2) AGE X INTENSE 
(tmpMU <- with(MUdata, cor.test(x=Age, y=Intense))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Intense))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Intense), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Intense), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Intense), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Intense), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.33, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexIntense_raw_new.png", width=5, height=5)

------

# (3) AGE X UNPRETENTIOUS  
(tmpMU <- with(MUdata, cor.test(x=Age, y=Unpretentious))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Unpretentious))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, 
                   alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 4.1421, p-value = 0.0000
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Unpretentious), col="darkgrey") + # add the neda data
  geom_point(data=nedaData, aes(x=Age, y=Unpretentious), col="springgreen3") + # add the neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Unpretentious), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Unpretentious), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexUnpretentious_raw_new.png", width=5, height=5)

------

# (4) AGE X SOPHISTICATED
(tmpMU <- with(MUdata, cor.test(x=Age, y=Sophisticated))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Sophisticated))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 1.8646, p-value = 0.0622
  #Null hypothesis retained

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Sophisticated), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Sophisticated), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Sophisticated), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Sophisticated), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.35, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexSophisticated_raw_new.png", width=5, height=5)

------

# (5) AGE X CONTEMPORARY  
(tmpMU <- with(MUdata, cor.test(x=Age, y=Contemporary))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Contemporary))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = -3.1936, p-value = 0.0014
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Contemporary), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Contemporary), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Contemporary), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Contemporary), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexContemporary_raw_new.png", width=5, height=5)

------

# (6) AGE X VALENCE   
(tmpMU <- with(MUdata, cor.test(x=Age, y=Valence))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Valence))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 1.4106, p-value = 0.1584
  #Null hypothesis retained

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Valence), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Valence), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Valence), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Valence), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=-0.06, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexValence_raw_new.png", width=5, height=5)

------

# (7) AGE X AROUSAL   
(tmpMU <- with(MUdata, cor.test(x=Age, y=Arousal))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Arousal))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Arousal), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Arousal), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Arousal), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Arousal), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.30, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexArousal_raw_new.png", width=5, height=5)

------

# (8) AGE X DEPTH 
(tmpMU <- with(MUdata, cor.test(x=Age, y=Depth))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Depth))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 1.1150, p-value = 0.2648
  #Null hypothesis retained

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Depth), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Depth), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Depth), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Depth), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=-0.27, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexDepth_raw_final.png", width=5, height=5)

------

# (9) AGE X MUSICSUM   
(tmpMU <- with(MUdata, cor.test(x=Age, y=MusicSum))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=MusicSum))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = -1.5226, p-value = 0.1279
  #Null hypothesis retained

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=MusicSum), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=MusicSum), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=MusicSum), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=MusicSum), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=190, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexMusicSum_raw_new.png", width=5, height=5)


#### Openness and MUSIC Model Correlations -------------------------------------

# 1 - Openness (IV) and Mellow (DV)

(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Mellow))) # do the correlation for neda's data

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

#-------------------------------------------------------------------------------

# 2 - Openness (IV) and Unpretentious (DV)

(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Unpretentious))) # do the correlation for neda's data

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")


#-------------------------------------------------------------------------------

# 3 - Openness (IV) and Intense (DV)

(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Intense))) # do the correlation for neda's data

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

#-------------------------------------------------------------------------------

# 4 - Openness (IV) and Contemporary (DV)

(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Contemporary))) # do the correlation for neda's data

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")


##### All Key Correlations -----------------------------------------------------

# 1 - Openness (IV) and Sophisticated (DV)

(tmpMU <- with(MUdata, cor.test(x=Openness, y=Sophisticated))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Sophisticated))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

#fisher1925: Fisher's z (1925)
  #z = 3.2595, p-value = 0.0011
  #Null hypothesis rejected

ggplot() + # start a ggplot (correlation figure)
  #geom_point(data=MUdata, aes(x=Openness, y=Sophisticated), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Openness, y=Sophisticated), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Openness, y=Sophisticated), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Openness, y=Sophisticated), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.70, y=0.35, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_OpennessxSophisticated_raw_new.png", width=5, height=5)

------

# 2 - Conscientiousness (IV) and Intense (DV)

(tmpMU <- with(MUdata, cor.test(x=Conscientiousness, y=Intense))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Conscientiousness, y=Intense))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = -0.5953, p-value = 0.5517
  #Null hypothesis retained

ggplot() + # start a ggplot (correlation figure)
  #geom_point(data=MUdata, aes(x=Conscientiousness, y=Intense), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Conscientiousness, y=Intense), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Conscientiousness, y=Intense), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Conscientiousness, y=Intense), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.70, y=0.33, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Conscientiousness, y=Intense)) # do the correlation for musical universe
with(nedaData, cor.test(x=Conscientiousness, y=Intense)) # do the correlation for neda's data
ggsave("correlation_ConscientiousnessxIntense_raw_new.png", width=5, height=5)

------

# 3 - Extraversion (IV) and Unpretentious (DV)

(tmpMU <- with(MUdata, cor.test(x=Extraversion, y=Unpretentious))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=Unpretentious))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Extraversion, y=Unpretentious), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Extraversion, y=Unpretentious), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Extraversion, y=Unpretentious), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Extraversion, y=Unpretentious), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.65, y=0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Extraversion, y=Unpretentious)) # do the correlation for musical universe
with(nedaData, cor.test(x=Extraversion, y=Unpretentious)) # do the correlation for neda's data
ggsave("correlation_ExtraversionxUnpretentious_raw_new.png", width=5, height=5)

------

# 4 - Extraversion (IV) and Contemporary (DV)

(tmpMU <- with(MUdata, cor.test(x=Extraversion, y=Contemporary))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=Contemporary))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 4.0452, p-value = 0.0001
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Extraversion, y=Contemporary), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Extraversion, y=Contemporary), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Extraversion, y=Contemporary), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Extraversion, y=Contemporary), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.65, y=0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Extraversion, y=Contemporary)) # do the correlation for musical universe
with(nedaData, cor.test(x=Extraversion, y=Contemporary)) # do the correlation for neda's data
ggsave("correlation_ExtraversionxContemporary_raw_new.png", width=5, height=5)

------

# 5 - Agreeableness (IV) and Mellow (DV)

(tmpMU <- with(MUdata, cor.test(x=Agreeableness, y=Mellow))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Mellow))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 4.4754, p-value = 0.0000
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Agreeableness, y=Mellow), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Agreeableness, y=Mellow), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Agreeableness, y=Mellow), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Agreeableness, y=Mellow), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.70, y=0.32, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Agreeableness, y=Mellow)) # do the correlation for musical universe
with(nedaData, cor.test(x=Agreeableness, y=Mellow)) # do the correlation for neda's data
ggsave("correlation_AgreeablenessxMellow_raw_new.png", width=5, height=5)

------

# 6 - Agreeableness (IV) and Unpretentious (DV)

(tmpMU <- with(MUdata, cor.test(x=Agreeableness, y=Unpretentious))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Unpretentious))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 0.7058, p-value = 0.4803
  #Null hypothesis retained

# make a plot! 6) Agreeableness and Unpretentious  
ggplot() + # start a ggplot
  #geom_point(data=MUdata,aes(x=Agreeableness, y=Unpretentious), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Agreeableness, y=Unpretentious), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata,aes(x=Agreeableness, y=Unpretentious), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Agreeableness, y=Unpretentious), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.64, y=0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Agreeableness, y=Unpretentious)) # do the correlation for musical universe
with(nedaData, cor.test(x=Agreeableness, y=Unpretentious)) # do the correlation for neda's data   
ggsave("correlation_AgreeablenessxUnpretentious_raw_new.png", width=5, height=5)

------

# 7 - Agreeableness (IV) and Intense (DV)

(tmpMU <- with(MUdata, cor.test(x=Agreeableness, y=Intense))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Intense))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = -3.0441, p-value = 0.0023
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Agreeableness, y=Intense), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Agreeableness, y=Intense), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata,aes(x=Agreeableness, y=Intense), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Agreeableness, y=Intense), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.65, y=0.33, label= 
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Agreeableness, y=Intense)) # do the correlation for musical universe
with(nedaData, cor.test(x=Agreeableness, y=Intense)) # do the correlation for neda's data
ggsave("correlation_AgreeablenessxIntense_raw_new.png", width=5, height=5)

------

# 8 - Neuroticism (IV) and Intense (DV)

(tmpMU <- with(MUdata, cor.test(x=Neuroticism, y=Intense))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=Intense))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 1.6018, p-value = 0.1092
  #Null hypothesis retained

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Neuroticism, y=Intense), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Neuroticism, y=Intense), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Neuroticism, y=Intense), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Neuroticism, y=Intense), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.65, y=0.34, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Neuroticism, y=Intense)) # do the correlation for musical universe
with(nedaData, cor.test(x=Neuroticism, y=Intense)) # do the correlation for neda's data
ggsave("correlation_NeuroticismxIntense_raw_new.png", width=5, height=5)

------

# 9 - Openness (IV) and MusicSum (DV)

(tmpMU <- with(MUdata, cor.test(x=Openness, y=MusicSum))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=MusicSum))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Openness, y=MusicSum), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Openness, y=MusicSum), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Openness, y=MusicSum), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Openness, y=MusicSum), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.60, y=184, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Openness, y=MusicSum)) # do the correlation for musical universe
with(nedaData, cor.test(x=Openness, y=MusicSum)) # do the correlation for neda's data
ggsave("correlation_OpennessxMusicSum_raw_new.png", width=5, height=5)

------

# 10 - Extraversion (IV) and Intense (DV)

(tmpMU <- with(MUdata, cor.test(x=Extraversion, y=Intense))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=Intense))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
#z = 4.0452, p-value = 0.0001
#Null hypothesis rejected

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Extraversion, y=Intense), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Extraversion, y=Intense), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Extraversion, y=Intense), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Extraversion, y=Intense), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.71, y=0.34, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Extraversion, y=Intense)) # do the correlation for musical universe
with(nedaData, cor.test(x=Extraversion, y=Intense)) # do the correlation for neda's data
ggsave("correlation_ExtraversionxIntense_raw_new.png", width=5, height=5)

------

# 11 - Neuroticism (IV) and MusicSum (DV)

(tmpMU <- with(MUdata, cor.test(x=Neuroticism, y=MusicSum))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=MusicSum))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

# fisher1925: Fisher's z (1925)
#  z = -0.9226, p-value = 0.3562
#  Null Hypothesis retained

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Neuroticism, y=MusicSum), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Neuroticism, y=MusicSum), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Neuroticism, y=MusicSum), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Neuroticism, y=MusicSum), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=191, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Neuroticism, y=MusicSum)) # do the correlation for musical universe
with(nedaData, cor.test(x=Neuroticism, y=MusicSum)) # do the correlation for neda's data
ggsave("correlation_NeuroticismxMusicSum_raw_new.png", width=5, height=5)


##### AVD Model Correlations ---------------------------------------------------

# 10 - Openness (IV) and Arousal (DV)

(tmpMU <- with(MUdata, cor.test(x=Openness, y=Arousal))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Arousal))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

# fisher1925: Fisher's z (1925)
#  z = -0.9226, p-value = 0.3562
#  Null Hypothesis retained

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Openness, y=Arousal), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Openness, y=Arousal), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Openness, y=Arousal), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Openness, y=Arousal), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=0.25, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Openness, y=Arousal)) # do the correlation for musical universe
with(nedaData, cor.test(x=Openness, y=Arousal)) # do the correlation for neda's data
ggsave("correlation_OpennessxArousal_raw_new.png", width=5, height=5)

------

# 11 - Neuroticism (IV) and Depth (DV)

(tmpMU <- with(MUdata, cor.test(x=Neuroticism, y=Depth))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=Depth))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

# fisher1925: Fisher's z (1925)
#  z = -0.9226, p-value = 0.3562
#  Null Hypothesis retained

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Neuroticism, y=Depth), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Neuroticism, y=Depth), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Neuroticism, y=Depth), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Neuroticism, y=Depth), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Neuroticism, y=Depth)) # do the correlation for musical universe
with(nedaData, cor.test(x=Neuroticism, y=Depth)) # do the correlation for neda's data
ggsave("correlation_NeuroticismxDepth_raw_new.png", width=5, height=5)

------
  
# 12 - Conscientiousness (IV) and Arousal (DV)
(tmpMU <- with(MUdata, cor.test(x=Conscientiousness, y=Arousal))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Conscientiousness, y=Arousal))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Conscientiousness, y=Arousal), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Conscientiousness, y=Arousal), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Conscientiousness, y=Arousal), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Conscientiousness, y=Arousal), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=0.25, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Conscientiousness, y=Arousal)) # do the correlation for musical universe
with(nedaData, cor.test(x=Conscientiousness, y=Arousal)) # do the correlation for neda's data
ggsave("correlation_ConscientiousnessxArousal_raw_new.png", width=5, height=5)

------
  
# 13 - Conscientiousness (IV) and Depth (DV)
(tmpMU <- with(MUdata, cor.test(x=Conscientiousness, y=Depth))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Conscientiousness, y=Depth))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Conscientiousness, y=Depth), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Conscientiousness, y=Depth), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Conscientiousness, y=Depth), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Conscientiousness, y=Depth), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Conscientiousness, y=Depth)) # do the correlation for musical universe
with(nedaData, cor.test(x=Conscientiousness, y=Depth)) # do the correlation for neda's data
ggsave("correlation_ConscientiousnessxDepth_raw_new.png", width=5, height=5)

------

# 14 - Extraversion (IV) and Arousal (DV)
(tmpMU <- with(MUdata, cor.test(x=Extraversion, y=Arousal))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=Arousal))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Extraversion, y=Arousal), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Extraversion, y=Arousal), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Extraversion, y=Arousal), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Extraversion, y=Arousal), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=0.25, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Extraversion, y=Arousal)) # do the correlation for musical universe
with(nedaData, cor.test(x=Extraversion, y=Arousal)) # do the correlation for neda's data
ggsave("correlation_ExtraversionxArousal_raw_new.png", width=5, height=5)

------

# 15 - Agreeableness (IV) and Arousal (DV)
(tmpMU <- with(MUdata, cor.test(x=Agreeableness, y=Arousal))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Arousal))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Agreeableness, y=Arousal), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Agreeableness, y=Arousal), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Agreeableness, y=Arousal), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Agreeableness, y=Arousal), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=0.25, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Agreeableness, y=Arousal)) # do the correlation for musical universe
with(nedaData, cor.test(x=Agreeableness, y=Arousal)) # do the correlation for neda's data
ggsave("correlation_AgreeablenessxArousal_raw_new.png", width=5, height=5)

------

# 16 - Agreeableness (IV) and Valence (DV)
(tmpMU <- with(MUdata, cor.test(x=Agreeableness, y=Valence))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Valence))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Agreeableness, y=Valence), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Agreeableness, y=Valence), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Agreeableness, y=Valence), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Agreeableness, y=Valence), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.07, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Agreeableness, y=Valence)) # do the correlation for musical universe
with(nedaData, cor.test(x=Agreeableness, y=Valence)) # do the correlation for neda's data
ggsave("correlation_AgreeablenessxValence_raw_new.png", width=5, height=5)

------

# 17 - Agreeableness (IV) and Depth (DV)
(tmpMU <- with(MUdata, cor.test(x=Agreeableness, y=Depth))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Depth))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Agreeableness, y=Depth), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Agreeableness, y=Depth), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Agreeableness, y=Depth), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Agreeableness, y=Depth), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.27, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Agreeableness, y=Depth)) # do the correlation for musical universe
with(nedaData, cor.test(x=Agreeableness, y=Depth)) # do the correlation for neda's data
ggsave("correlation_AgreeablenessxDepth_raw_new.png", width=5, height=5)

------

# 18 - Neuroticism (IV) and Arousal (DV)
(tmpMU <- with(MUdata, cor.test(x=Neuroticism, y=Arousal))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=Arousal))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Neuroticism, y=Arousal), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Neuroticism, y=Arousal), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Neuroticism, y=Arousal), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Neuroticism, y=Arousal), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=0.29, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Neuroticism, y=Arousal)) # do the correlation for musical universe
with(nedaData, cor.test(x=Neuroticism, y=Arousal)) # do the correlation for neda's data
ggsave("correlation_NeuroticismsxArousal_raw_new.png", width=5, height=5)

------

# 19 - Neuroticism (IV) and Valence (DV)
(tmpMU <- with(MUdata, cor.test(x=Neuroticism, y=Valence))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=Valence))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Neuroticism, y=Valence), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Neuroticism, y=Valence), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Neuroticism, y=Valence), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Neuroticism, y=Valence), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.04, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Neuroticism, y=Valence)) # do the correlation for musical universe
with(nedaData, cor.test(x=Neuroticism, y=Valence)) # do the correlation for neda's data
ggsave("correlation_NeuroticismsxValence_raw_new.png", width=5, height=5)

------

# 20 - Openness (IV) and Valence (DV)
(tmpMU <- with(MUdata, cor.test(x=Openness, y=Valence))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Valence))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Openness, y=Valence), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Openness, y=Valence), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Openness, y=Valence), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Openness, y=Valence), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.07, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Openness, y=Valence)) # do the correlation for musical universe
with(nedaData, cor.test(x=Openness, y=Valence)) # do the correlation for neda's data
ggsave("correlation_OpennessxValence_raw_new.png", width=5, height=5)

------

# 21 - Openness (IV) and Depth (DV)
(tmpMU <- with(MUdata, cor.test(x=Openness, y=Depth))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Depth))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

p_value <- tmpMPC$p.value #to round p-value to 5-decimal places
formatted_p_value <- sprintf("%.5f", p_value)
cat("The p-value is:", formatted_p_value, "\n")

ggplot() + # start a ggplot
  #geom_point(data=MUdata, aes(x=Openness, y=Depth), col="grey", size=0.5) + # add the musical universe data
  geom_point(data=nedaData, aes(x=Openness, y=Depth), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Openness, y=Depth), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Openness, y=Depth), col="springgreen3") + # add the neda line
  annotate(geom="text", x=2.75, y=-0.27, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)),
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), " , df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
with(MUdata, cor.test(x=Openness, y=Depth)) # do the correlation for musical universe
with(nedaData, cor.test(x=Openness, y=Depth)) # do the correlation for neda's data
ggsave("correlation_OpennessxDepth_raw_new.png", width=5, height=5)


#################===============================================================

### REGRESSIONS WITH AGE AND GENDER AS FACTORS

# 1 - Openness (IV) and Sophisticated (DV)

# MUdata
# create regression model. Here, Sophisticated is being predicted by Gender, Age, and Openness. Effect of Openness is calculated AFTER accounting for Gender and Age.
lm.01a <- lm(Sophisticated ~ Gender+Age+Openness, data=MUdata)

# run this and only look at the paragraph at the bottom. Ignore the table. The R2 value from your regression is 0.02743, and it is statistically significant (R2=0.03, F(3,47536)=447, p<0.0001). This means that AT LEAST ONE of the predictor factors is associated with a difference in the dependent variable (Sophisticated).
summary(lm.01a)

# to figure out WHICH predictor variable is causing the significant regression, run this:
Anova(lm.01a)
# based on this, there's a gender difference in Sophisticated. There's an age difference in Sophisticated. After controlling for gender and age, there is an association between Openness and Sophisticated


# nedaData
# create regression model. Here, Sophisticated is being  predicted by Gender, Age, and Openness. Yes, effect of Openness is calculated AFTER accounting for Gender and Age.
lm.01b <- lm(Sophisticated ~ Gender+Age+Openness, data=nedaData)

# run this and JUST look at the paragraph at the bottom. Ignore the table. The R2 value from your regression is 0.01, and is not statistically significant (R2=0.01, F(3,269)=1.029, p=0.38).
summary(lm.01b)

# to figure out WHICH predictor variable is causing the significant regression, run this:
Anova(lm.01b)
# based on this, there is not a significant Gender difference in Sophisticated (F(2,269)=1.53, p=0.22). Nor is there a significant Age difference in Sophisticated (F(1,269)=0.02, p=0.94). After controlling for Gender and Age, the association is not significant between Openness and Sophisticated (F(1,269)=1.38, p=0.24).

# ------

# 2 - Conscientiousness (IV) and Intense (DV)

# MUdata
lm.02a <- lm(Intense ~ Gender+Age+Conscientiousness, data=MUdata)
summary(lm.02a)
Anova(lm.02a)

# nedaData
lm.02b <- lm(Intense ~ Gender+Age+Conscientiousness, data=nedaData)
summary(lm.02b)
Anova(lm.02b)

# ------

# 3 - Extraversion (IV) and Unpretentious (DV)

# MUdata
lm.03a <- lm(Unpretentious ~ Gender+Age+Extraversion, data=MUdata)
summary(lm.03a)
Anova(lm.03a)

# nedaData
lm.03b <- lm(Unpretentious ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.03b)
Anova(lm.03b)

# ------

# 4 - Extraversion (IV) and Contemporary (DV)

# MUdata
lm.04a <- lm(Contemporary ~ Gender+Age+Extraversion, data=MUdata)
summary(lm.04a)
Anova(lm.04a)

# nedaData
lm.04b <- lm(Contemporary ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.04b)
Anova(lm.04b)

# ------

# 5 - Agreeableness (IV) and Mellow (DV)

# MUdata
lm.05a <- lm(Mellow ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.05a)
Anova(lm.05a)

# nedaData
lm.05b <- lm(Mellow ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.05b)
Anova(lm.05b)

# ------

# 6 - Agreeableness (IV) and Unpretentious (DV)

# MUdata
lm.06a <- lm(Unpretentious ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.06a)
Anova(lm.06a)

# nedaData
lm.06b <- lm(Unpretentious ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.06b)
Anova(lm.06b)

# ------

# 7 - Agreeableness (IV) and Intense (DV)

# MUdata
lm.07a <- lm(Intense ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.07a)
Anova(lm.07a)

# nedaData
lm.07b <- lm(Intense ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.07b)
Anova(lm.07b)

# ------

# 8 - Neuroticism (IV) and Intense (DV)

# MUdata
lm.08a <- lm(Intense ~ Gender+Age+Neuroticism, data=MUdata)
summary(lm.08a)
Anova(lm.08a)

# nedaData
lm.08b <- lm(Intense ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.08b)
Anova(lm.08b)

# ------

# 9 - Openness (IV) and MusicSum (DV)

# MUdata
lm.09a <- lm(MusicSum ~ Gender+Age+Openness, data=MUdata)
summary(lm.09a)
Anova(lm.09a)

# nedaData
lm.09b <- lm(MusicSum ~ Gender+Age+Openness, data=nedaData)
summary(lm.09b)
Anova(lm.09b)

# ------ AVD Model Regression Replications -------------------------------------

# 10 - Neuroticism (IV) and Depth (DV)

# MUdata
lm.10a <- lm(Depth ~ Gender+Age+Neuroticism, data=MUdata)
summary(lm.10a)
Anova(lm.10a)

# nedaData
lm.10b <- lm(Depth ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.10b)
Anova(lm.10b)

# ------

# 11 - Conscientiousness (IV) and Arousal (DV)

# MUdata
lm.11a <- lm(Arousal ~ Gender+Age+Conscientiousness, data=MUdata)
summary(lm.11a)
Anova(lm.11a)

# nedaData
lm.11b <- lm(Arousal ~ Gender+Age+Conscientiousness, data=nedaData)
summary(lm.11b)
Anova(lm.11b)

# ------

# 12 - Conscientiousness (IV) and Depth (DV)

# MUdata
lm.12a <- lm(Depth ~ Gender+Age+Conscientiousness, data=MUdata)
summary(lm.12a)
Anova(lm.12a)

# nedaData
lm.12b <- lm(Depth ~ Gender+Age+Conscientiousness, data=nedaData)
summary(lm.12b)
Anova(lm.12b)

# ------

# 13 - Extraversion (IV) and Arousal (DV)

# MUdata
lm.13a <- lm(Arousal ~ Gender+Age+Extraversion, data=MUdata)
summary(lm.13a)
Anova(lm.13a)

# nedaData
lm.13b <- lm(Arousal ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.13b)
Anova(lm.13b)

# ------

# 14 - Agreeableness (IV) and Arousal (DV)

# MUdata
lm.14a <- lm(Arousal ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.14a)
Anova(lm.14a)

# nedaData
lm.14b <- lm(Arousal ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.14b)
Anova(lm.14b)

# ------

# 15 - Agreeableness (IV) and Valence (DV)

# MUdata
lm.15a <- lm(Valence ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.15a)
Anova(lm.15a)

# nedaData
lm.15b <- lm(Valence ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.15b)
Anova(lm.15b)

# ------

# 16 - Agreeableness (IV) and Depth (DV)

# MUdata
lm.16a <- lm(Depth ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.16a)
Anova(lm.16a)

# nedaData
lm.16b <- lm(Depth ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.16b)
Anova(lm.16b)

# ------

# 17 - Neuroticism (IV) and Arousal (DV)

# MUdata
lm.17a <- lm(Arousal ~ Gender+Age+Neuroticism, data=MUdata)
summary(lm.17a)
Anova(lm.17a)

# nedaData
lm.17b <- lm(Arousal ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.17b)
Anova(lm.17b)

# ------

# 18 - Neuroticism (IV) and Valence (DV)

# MUdata
lm.18a <- lm(Valence ~ Gender+Age+Neuroticism, data=MUdata)
summary(lm.18a)
Anova(lm.18a)

# nedaData
lm.18b <- lm(Valence ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.18b)
Anova(lm.18b)

# ------

# 19 - Openness (IV) and Valence (DV)

# MUdata
lm.19a <- lm(Valence ~ Gender+Age+Openness, data=MUdata)
summary(lm.19a)
Anova(lm.19a)

# nedaData
lm.19b <- lm(Valence ~ Gender+Age+Openness, data=nedaData)
summary(lm.19b)
Anova(lm.19b)

# ------

# 20 - Openness (IV) and Depth (DV)

# MUdata
lm.20a <- lm(Depth ~ Gender+Age+Openness, data=MUdata)
summary(lm.20a)
Anova(lm.20a)

# nedaData
lm.20b <- lm(Depth ~ Gender+Age+Openness, data=nedaData)
summary(lm.20b)
Anova(lm.20b)

# ------

# 21 - Openness (IV) and Arousal (DV)

# MUdata
lm.21a <- lm(Arousal ~ Gender+Age+Openness, data=MUdata)
summary(lm.21a)
Anova(lm.21a)

# nedaData
lm.21b <- lm(Arousal ~ Gender+Age+Openness, data=nedaData)
summary(lm.21b)
Anova(lm.21b)

# ------

###### Interesting Associations Found Outside of Replication -------------------

# 22 - Extraversion (IV) and Intense (DV)

# MUdata
lm.22a <- lm(Intense ~ Gender+Age+Extraversion, data=MUdata)
summary(lm.22a)
Anova(lm.22a)

# nedaData
lm.22b <- lm(Intense ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.22b)
Anova(lm.22b)

# ------

# 23 - Neuroticism (IV) and MusicSum (DV)

# MUdata
lm.23a <- lm(MusicSum ~ Gender+Age+Neuroticism, data=MUdata)
summary(lm.23a)
Anova(lm.23a)

# nedaData
lm.23b <- lm(MusicSum ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.23b)
Anova(lm.23b)

# ------

# make the MU and MPC (without Creativity) Regression Coeffcient Tables
stargazer(lm.01a, lm.01b, type="html", out="/Users/nedarad/Desktop/lm01table.html")
stargazer(lm.02a, lm.02b, type="html", out="/Users/nedarad/Desktop/lm02table.html")
stargazer(lm.03a, lm.03b, type="html", out="/Users/nedarad/Desktop/lm03table.html")
stargazer(lm.04a, lm.04b, type="html", out="/Users/nedarad/Desktop/lm04table.html")
stargazer(lm.05a, lm.05b, type="html", out="/Users/nedarad/Desktop/lm05table.html")
stargazer(lm.06a, lm.06b, type="html", out="/Users/nedarad/Desktop/lm06table.html")
stargazer(lm.07a, lm.07b, type="html", out="/Users/nedarad/Desktop/lm07table.html")
stargazer(lm.08a, lm.08b, type="html", out="/Users/nedarad/Desktop/lm08table.html")
stargazer(lm.09a, lm.09b, type="html", out="/Users/nedarad/Desktop/lm09table.html")

stargazer(lm.10a, lm.10b, type="html", out="/Users/nedarad/Desktop/NeuroticismXDepth.html")
stargazer(lm.11a, lm.11b, type="html", out="/Users/nedarad/Desktop/ConscientiousnessXArousal.html")
stargazer(lm.12a, lm.12b, type="html", out="/Users/nedarad/Desktop/ConscientiousnessXDepth.html")
stargazer(lm.13a, lm.13b, type="html", out="/Users/nedarad/Desktop/ExtraversionXArousal.html")
stargazer(lm.14a, lm.14b, type="html", out="/Users/nedarad/Desktop/AgreeablenessXArousal.html")
stargazer(lm.15a, lm.15b, type="html", out="/Users/nedarad/Desktop/AgreeablenessXValence.html")
stargazer(lm.16a, lm.16b, type="html", out="/Users/nedarad/Desktop/AgreeablenessXDepth.html")
stargazer(lm.17a, lm.17b, type="html", out="/Users/nedarad/Desktop/NeuroticismXArousal.html")
stargazer(lm.18a, lm.18b, type="html", out="/Users/nedarad/Desktop/NeuroticismXValence.html")
stargazer(lm.19a, lm.19b, type="html", out="/Users/nedarad/Desktop/OpennessXValence.html")
stargazer(lm.20a, lm.20b, type="html", out="/Users/nedarad/Desktop/OpennessXDepth.html")
stargazer(lm.21a, lm.21b, type="html", out="/Users/nedarad/Desktop/OpennessXArousal.html")

stargazer(lm.22a, lm.22b, type="html", out="/Users/nedarad/Desktop/ExtraversionXIntense.html")
stargazer(lm.23a, lm.23b, type="html", out="/Users/nedarad/Desktop/NeuroticismXMusicSum.html")





######################################
######################################
######################################
######################################
######################################

# AIM 2 - Creativity!!!

##### Creativity and Gender ----------------------------------------------------

#nedaDatalong_creativity <- melt(nedaData[,c(1,2,18:20,22,28,29)], id.vars=c("ProlificID","Gender"))

#ggplot(nedaDatalong_creativity) +
# geom_boxplot(aes(y=value, x=Gender, col=Gender)) +
# facet_wrap(~variable, scales="free") + 
#  scale_color_manual(values = c("orchid1", "steelblue1")) +
#  theme_bw()
# ggsave("boxplot_GenderxAllCreativity_raw.png", width=8.5, height=8.5)

nedaDatalong_creativity <- melt(nedaData[,c(1,2,18,19,28)], id.vars=c("ProlificID","Gender"))

# code to have the means to show up
ggplot(nedaDatalong_creativity, aes(y=value, x=Gender, col=Gender)) +
  geom_boxplot() +
  stat_summary(fun.y="mean") +
  facet_wrap(~variable, scales="free") + 
  scale_color_manual(values = c("orchid1", "steelblue1")) +
  theme_bw()
ggsave("boxplot_GenderxAllCreativity_raw2.png", width=8.5, height=5.0)


------

# 1 - CAQ and Gender 
with(nedaData, t.test(CAQ ~ Gender, data=nedaData, var.equal=T))
with(nedaData, cohen.d(CAQ ~ Gender, data=nedaData, var.equal=T))

aggregate(value ~ Dataset * Gender, data=subset(nedaData, variable=="CAQ"), FUN=mean, na.rm=T)

#CAQ - Mean and SD
aggregate(value ~ Gender, data=subset(nedaDatalong_creativity, variable=="CAQ"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(nedaDatalong_creativity, variable=="CAQ"), FUN=sd, na.rm=T)

------

# 2 - Number of Galleries and Gender
with(nedaData, t.test(NumberofGalleries ~ Gender, data=nedaData, var.equal=T))
with(nedaData, cohen.d(NumberofGalleries ~ Gender, data=nedaData, var.equal=T))

aggregate(value ~ Dataset * Gender, data=subset(allDatalong, variable=="Intense"), FUN=mean, na.rm=T)
aggregate(value ~ Gender, data=subset(allDatalong, variable=="Intense"), FUN=mean, na.rm=T)

------

# Number of Clusters and Gender
#with(nedaData, t.test(NumClusters ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, cohen.d(NumClusters ~ Gender, data=nedaData, var.equal=T))

------

# Percent Time Explored and Gender
#with(nedaData, t.test(PercentTimeExplore ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, cohen.d(PercentTimeExplore ~ Gender, data=nedaData, var.equal=T))

------

# 3 - Originality and Gender
with(nedaData, t.test(Originality ~ Gender, data=nedaData, var.equal=T))
with(nedaData, cohen.d(Originality ~ Gender, data=nedaData, var.equal=T))

------

# Percent Clusters GC and Gender
#with(nedaData, t.test(PercentClustersGC ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, cohen.d(PercentClustersGC ~ Gender, data=nedaData, var.equal=T))

------

names(nedaData)
aggregate(CAQ ~ Gender, data=nedaData, FUN=mean, na.rm=T)

#with(nedaData, t.test(PercentShapesExplore ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, t.test(ExploitOptimality ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, t.test(ExploreOptimality ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, t.test(OptimalityRatio ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, t.test(MedianNumStepsExploit ~ Gender, data=nedaData, var.equal=T))
#with(nedaData, t.test(MedianNumStepsExplore ~ Gender, data=nedaData, var.equal=T))

# How to report this, example: t(271) = -2.37, p = 0.018, d = x 


##### Correlation Matrix -------------------------------------------------------
allCors = cor(nedaData[,c(-1,-2,-20,-21,-22,-23,-24,-25,-26,-27,-29)], use = "pairwise.complete.obs")
cor_test_mat <- corr.test(nedaData[,c(-1,-2,-20,-21,-22,-23,-24,-25,-26,-27,-29)])$p
#"complete.obs" removed and put "pairwise.complete.obs" instead

# Create Correlation Matrix and customize the colours
corrplot(allCors, method="ellipse", type="lower", diag=T, order="original",  
         col=brewer.pal(n=8, name="RdBu"), p.mat = cor_test_mat, insig = "blank", sig.level=0.005,
         tl.col=c("midnightblue", "midnightblue","midnightblue",
                                                                                            
                  "midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue",
                                                                                            
                  "midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue",
                                                                                            
                  "midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue"), tl.srt=45, tl.cex=0.6)

# code to create and save the correlation coefficient matrix in csv format on desktop, only necessary once
write.csv(round(allCors,4), "nedaCorMatrix.csv", row.names=T)

##### Creativity Correlations MPC ----------------------------------------------

# CAQ

# 1 -  Openness and CAQ
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Openness, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=3.3, y=65, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_OpennessxCAQ.png", width=5, height=5)
# As seen in Figure 5, CAQ and Openness were significantly correlated (r=0.37, df=282, p<0.001).
# The correlation between CAQ and and Openness was significant (r=0.37, df=282, p<0.001).

mean(nedaData$CAQ, na.rm = TRUE)
sd(nedaData$CAQ, na.rm = TRUE)


# 2 -  Neuroticism and CAQ 
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Neuroticism, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=3, y=67.5, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_NeuroticismxCAQ_raw.png", width=5, height=5)


# 3 -  CAQ and Depth
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Depth))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Depth)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23.5, y=-0.25, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxDepth_raw.png", width=5, height=5)


# 4 -  CAQ and Mellow
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Mellow))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Mellow)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.32, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxMellow_raw.png", width=5, height=5)


# 5 -  CAQ and Unpretentious
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Unpretentious))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Unpretentious)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.32, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxUnpretentious_raw.png", width=5, height=5)


# 6 -  CAQ and Sophisticated
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Sophisticated))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Sophisticated)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.35, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxSophisticated_raw.png", width=5, height=5)


# 7 -  CAQ and Intense
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Intense))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Intense)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.35, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxIntense_raw.png", width=5, height=5)


# 8 -  CAQ and Contemporary
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Contemporary))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Contemporary)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.30, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxContemporary_raw.png", width=5, height=5)


# 9 -  CAQ and Depth
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Depth))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Depth)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=-0.29, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxDepth_raw.png", width=5, height=5)


# 10 -  CAQ and Valence
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Valence))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Valence)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=-0.05, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxValence_raw.png", width=5, height=5)


# 11 -  CAQ and Arousal
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Arousal))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Arousal)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.30, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxArousal_raw.png", width=5, height=5)


# 12 -  CAQ and MusicSum
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=MusicSum))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=MusicSum)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=22, y=190, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxMusicSum_raw.png", width=5, height=5)


# 13 -  CAQ and Openness
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Openness))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Openness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=21.5, y=8.5, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxOpenness_raw.png", width=5, height=5)


# 14 -  CAQ and Conscientiousness
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Conscientiousness))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Conscientiousness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=22, y=7.2, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxConscientiousness_raw.png", width=5, height=5)


# 15 -  CAQ and Extraversion
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Extraversion))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Extraversion)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=22, y=7.2, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxExtraversion_raw.png", width=5, height=5)


# 16 -  CAQ and Agreeableness
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Agreeableness))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Agreeableness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=22, y=7.2, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxAgreeableness_raw.png", width=5, height=5)


# 17 -  CAQ and Neuroticism
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Neuroticism))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Neuroticism)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=22, y=7.2, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxNeuroticism_raw.png", width=5, height=5)


# 18 -  CAQ and Sophisticated 
(tmpMPC <- with(nedaData, cor.test(x=CAQ, y=Sophisticated))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=CAQ, y=Sophisticated)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=23, y=0.35, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_CAQxSophisticated_raw.png", width=5, height=5)


# 19 -  Extraversion and CAQ 
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Extraversion, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=3, y=70, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_ExtraversionxCAQ_raw.png", width=5, height=5)


# 20 -  Openness and CAQ 
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Openness, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=3.3, y=70, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_OpennessxCAQ_raw.png", width=5, height=5)


# 21 -  Conscientiousness and CAQ 
(tmpMPC <- with(nedaData, cor.test(x=Conscientiousness, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Conscientiousness, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=3.3, y=70, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_ConscientiousnessxCAQ_raw.png", width=5, height=5)


# 22 -  Agreeableness and CAQ 
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Agreeableness, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=4, y=70, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_AgreeablenessxCAQ_raw.png", width=5, height=5)



# 23 -  Neuroticism and CAQ 
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=CAQ))) # do the correlation for neda's data

ggplot(data=nedaData, aes(x=Neuroticism, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + # add the correlation line
  annotate(geom="text", x=3, y=70, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13, face="bold"))
ggsave("correlation_NeuroticismxCAQ_raw.png", width=5, height=5)


------

# Openness and MusicSum
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=MusicSum))) # do the correlation for neda's data

with(nedaData, cor.test(x=Openness, y=MusicSum)) # do the correlation
ggplot(data=nedaData, aes(x=Openness, y=MusicSum)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.3, y=190, size=4.5, label= 
            paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OpennessxMusicSum_raw.png", width=5, height=5)

------

# Openness and Arousal
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Arousal))) # do the correlation for neda's data

with(nedaData, cor.test(x=Openness, y=Arousal)) # do the correlation
ggplot(data=nedaData, aes(x=Openness, y=Arousal)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.4, y=0.26, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OpennessxArousal_raw.png", width=5, height=5)

------

# NumberofGalleries
  
# 1 - NumberofGalleries and Contemporary
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Contemporary))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Contemporary)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Contemporary)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=80, y=0.30, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxContemporary_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 2 - NumberofGalleries and Mellow
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Mellow))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Mellow)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Mellow)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=81, y=0.32, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxMellow_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 3 - NumberofGalleries and Unpretentious
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Unpretentious))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Unpretentious)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Unpretentious)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=81, y=0.31, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxUnpretentious_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 4 - NumberofGalleries and Sophisticated
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Sophisticated))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Sophisticated)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Sophisticated)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=81, y=0.35, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxSophisticated_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 5 - NumberofGalleries and Intense
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Intense))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Intense)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Intense)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=83, y=0.33, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxIntense_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 6 - NumberofGalleries and Depth
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Depth))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Depth)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Depth)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=82, y=-0.28, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxDepth_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 7 - NumberofGalleries and Valence
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Valence))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Valence)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Valence)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=82, y=-0.06, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxValence_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 8 - NumberofGalleries and Arousal
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Arousal))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Arousal)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Arousal)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=82, y=0.26, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxArousal_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 9 - NumberofGalleries and MusicSum
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=MusicSum))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=MusicSum)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=MusicSum)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=82, y=189, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxMusicSum_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 10 - NumberofGalleries and Conscientiousness
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Conscientiousness))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Conscientiousness)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Conscientiousness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=81, y=7.5, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxConscientiousness_raw.png", width=5, height=5)

mean(nedaData$NumGalleries, na.rm = TRUE)
sd(nedaData$NumGalleries, na.rm = TRUE)


# 11 - NumberofGalleries and Extraversion
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Extraversion))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Extraversion)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Extraversion)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=78, y=7.4, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxExtraversion_raw.png", width=5, height=5)


# 12 - NumberofGalleries and Agreeableness
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Agreeableness))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumberofGalleries, y=Agreeableness)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Agreeableness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=79, y=7.4, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxAgreeableness_raw.png", width=5, height=5)


# 13 - NumberofGalleries and Neuroticism
(tmpMPC <- with(nedaData, cor.test(x=NumberofGalleries, y=Neuroticism))) # do the correlation for nedaData

with(nedaData, cor.test(x=NumberofGalleries, y=Neuroticism)) # do the correlation
ggplot(data=nedaData, aes(x=NumberofGalleries, y=Neuroticism)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=79, y=7.4, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumberofGalleriesxNeuroticism_raw.png", width=5, height=5)


# 14 - Neuroticism and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Neuroticism, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Neuroticism, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3, y=201, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NeuroticismxNumberofGalleries_raw.png", width=5, height=5)


# 15 - Openness and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Openness, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Openness, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.5, y=201, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_OpennessxNumberofGalleries_raw.png", width=5, height=5)


# 16 - Conscientiousness and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Conscientiousness, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Conscientiousness, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Conscientiousness, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.5, y=201, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_ConscientiousnessxNumberofGalleries_raw.png", width=5, height=5)


# 17 - Extraversion and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Extraversion, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Extraversion, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3, y=201, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_ExtraversionxNumberofGalleries_raw.png", width=5, height=5)


# 18 - Agreeableness and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Agreeableness, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Agreeableness, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=4.1, y=201, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgreeablenessxNumberofGalleries_raw.png", width=5, height=5)


# 19 - Extraversion and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Extraversion, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Extraversion, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3, y=201, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_ExtraversionxNumberofGalleries_raw.png", width=5, height=5)


------

# 8 - NumberofClusters and Contemporary
#(tmpMPC <- with(nedaData, cor.test(x=NumberofClusters, y=Contemporary))) # do the correlation for neda's data

#with(nedaData, cor.test(x=NumberofClusters, y=Contemporary)) # do the correlation
#ggplot(data=nedaData, aes(x=NumberofClusters, y=Contemporary)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=14.5, y=0.30, size=4.5, label=
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#       axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_NumberofClustersxContemporary_raw.png", width=5, height=5)

#mean(nedaData$NumClusters, na.rm = TRUE)
#sd(nedaData$NumClusters, na.rm = TRUE)

------

# PercentofShapesExplored and Sophisticated
#(tmpMPC <- with(nedaData, cor.test(x=PercentShapesExplore, y=Sophisticated))) # do the correlation for neda's data

#with(nedaData, cor.test(x=PercentShapesExplore, y=Sophisticated)) # do the correlation
#ggplot(data=nedaData, aes(x=PercentShapesExplore, y=Sophisticated)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.17, y=0.35, size=4.5, label=
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_PercentofShapesExploredxSophisticated_raw.png", width=5, height=5)

------

# PercentofShapesExplored and Neuroticism
#(tmpMPC <- with(nedaData, cor.test(x=PercentShapesExplore, y=Neuroticism))) # do the correlation for neda's data

#with(nedaData, cor.test(x=PercentShapesExplore, y=Neuroticism)) # do the correlation
#ggplot(data=nedaData, aes(x=PercentShapesExplore, y=Neuroticism)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.16, y=7.5, size=4.5, label=
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_PercentofShapesExploredxNeuroticism_raw.png", width=5, height=5)

------

# 9 - PercentofTimeExplored and Age
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=PercentofTimeExplored))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=PercentofTimeExplored)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=PercentofTimeExplored)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=0.55, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexPercentofTimeExplored_raw.png", width=5, height=5)

#mean(nedaData$PercentTimeExplore, na.rm = TRUE)
#sd(nedaData$PercentTimeExplore, na.rm = TRUE)

------

# 10 - PercentofTimeExplored and Unpretentious
#(tmpMPC <- with(nedaData, cor.test(x=PercentofTimeExplored, y=Unpretentious))) # do the correlation for neda's data

#with(nedaData, cor.test(x=PercentofTimeExplored, y=Unpretentious)) # do the correlation
#ggplot(data=nedaData, aes(x=PercentofTimeExplored, y=Unpretentious)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.21, y=0.30, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_PercentofTimeExploredxUnpretentious_raw.png", width=5, height=5)

------

# 11 - PercentofTimeExplored and Sophisticated
#(tmpMPC <- with(nedaData, cor.test(x=PercentofTimeExplored, y=Sophisticated))) # do the correlation for neda's data

#with(nedaData, cor.test(x=PercentofTimeExplored, y=Sophisticated)) # do the correlation
#ggplot(data=nedaData, aes(x=PercentofTimeExplored, y=Sophisticated)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.21, y=0.35, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_PercentofTimeExploredxSophisticated_raw.png", width=5, height=5)

------

# 14 - PercentofTimeExplored and Arousal
#(tmpMPC <- with(nedaData, cor.test(x=PercentofTimeExplored, y=Arousal))) # do the correlation for neda's data

#with(nedaData, cor.test(x=PercentofTimeExplored, y=Arousal)) # do the correlation
#ggplot(data=nedaData, aes(x=PercentofTimeExplored, y=Arousal)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.21, y=0.26, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_PercentofTimeExploredxArousal_raw.png", width=5, height=5)

------

# ExploitOptimality and Valence
#(tmpMPC <- with(nedaData, cor.test(x=ExploitOptimality, y=Valence))) # do the correlation for neda's data

#with(nedaData, cor.test(x=ExploitOptimality, y=Valence)) # do the correlation
#ggplot(data=nedaData, aes(x=ExploitOptimality, y=Valence)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.36, y=-0.07, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_ExploitOptimalityxValence_raw.png", width=5, height=5)

------

# ExploreOptimality and Contemporary
#(tmpMPC <- with(nedaData, cor.test(x=ExploreOptimality, y=Contemporary))) # do the correlation for neda's data

#with(nedaData, cor.test(x=ExploreOptimality, y=Contemporary)) # do the correlation
#ggplot(data=nedaData, aes(x=ExploreOptimality, y=Contemporary)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.36, y=0.30, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_ExploreOptimalityxContemporary_raw.png", width=5, height=5)

------

# ExploreOptimality and Valence
#(tmpMPC <- with(nedaData, cor.test(x=ExploreOptimality, y=Valence))) # do the correlation for neda's data

#with(nedaData, cor.test(x=ExploreOptimality, y=Valence)) # do the correlation
#ggplot(data=nedaData, aes(x=ExploreOptimality, y=Valence)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=0.36, y=-0.06, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_ExploreOptimalityxValence_raw.png", width=5, height=5)

------

# Openness and OptimalityRatio
#(tmpMPC <- with(nedaData, cor.test(x=Openness, y=OptimalityRatio))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Openness, y=OptimalityRatio)) # do the correlation
#ggplot(data=nedaData, aes(x=Openness, y=OptimalityRatio)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=3.4, y=2.0, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=13),
#        axis.title=element_text(size=13,face="bold"))
#ggsave("correlation_OpennessxOptimalityRatio_raw.png", width=5, height=5)

------

# Age and MedianNumStepsExploit
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=MedianNumStepsExploit))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=MedianNumStepsExploit)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=MedianNumStepsExploit)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=16.5, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=13),
#        axis.title=element_text(size=13,face="bold"))
#ggsave("correlation_AgexMedianNumStepsExploit_raw.png", width=5, height=5)

------

# Extraversion and MedianNumStepsExploit
#(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=MedianNumStepsExploit))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Extraversion, y=MedianNumStepsExploit)) # do the correlation
#ggplot(data=nedaData, aes(x=Extraversion, y=MedianNumStepsExploit)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=3, y=16.5, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=13),
#        axis.title=element_text(size=13,face="bold"))
#ggsave("correlation_ExtraversionxMedianNumStepsExploit_raw.png", width=5, height=5)

------

# Age and MedianNumStepsExplore
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=MedianNumStepsExplore))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=MedianNumStepsExplore)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=MedianNumStepsExplore)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=38, y=23, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=13),
#        axis.title=element_text(size=13,face="bold"))
#ggsave("correlation_AgexMedianNumStepsExplore_raw.png", width=5, height=5)

------

# MedianNumStepsExplore and MusicSum
#(tmpMPC <- with(nedaData, cor.test(x=MedianNumStepsExplore, y=MusicSum))) # do the correlation for neda's data

#with(nedaData, cor.test(x=MedianNumStepsExplore, y=MusicSum)) # do the correlation
#ggplot(data=nedaData, aes(x=MedianNumStepsExplore, y=MusicSum)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=9.5, y=188, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=13),
#        axis.title=element_text(size=13,face="bold"))
#ggsave("correlation_MedianNumStepsExplorexMusicSum_raw.png", width=5, height=5)

------

# Originality  
  
# 1 - Originality and Contemporary
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Contemporary))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Contemporary)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Contemporary)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.96, y=0.30, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxContemporary_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 2 - Originality and Mellow
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Mellow))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Mellow)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Mellow)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.91, y=0.31, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxMellow_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 3 - Originality and Unpretentious
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Unpretentious))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Unpretentious)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Unpretentious)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.94, y=0.31, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxUnpretentious_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 4 - Originality and Intense
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Intense))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Intense)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Intense)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.93, y=0.33, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxIntense_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 5 - Originality and Sophisticated
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Sophisticated))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Sophisticated)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Sophisticated)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.92, y=0.35, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxSophisticated_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 6 - Originality and Depth
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Depth))) # do the correlation for nedadata

with(nedaData, cor.test(x=Originality, y=Depth)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Depth)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.92, y=-0.28, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxDepth_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 7 - Originality and Arousal
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Arousal))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Arousal)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Arousal)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.92, y=0.26, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxArousal_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 8 - Originality and Valence
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Valence))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Valence)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Valence)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.93, y=-0.06, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxValence_raw.png", width=5, height=5)

mean(nedaData$Originality, na.rm = TRUE)
sd(nedaData$Originality, na.rm = TRUE)


# 9 - Originality and MusicSum
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=MusicSum))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=MusicSum)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=MusicSum)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.93, y=190, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxMusicSum_raw.png", width=5, height=5)


# 10 - Originality and Openness
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Openness))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Openness)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Openness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.90, y=7.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxOpenness_raw.png", width=5, height=5)


# 11 - Originality and Conscientiousness
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Conscientiousness))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Conscientiousness)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Conscientiousness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.90, y=7.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxConscientiousness_raw.png", width=5, height=5)


# 12 - Originality and Extraversion
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Extraversion))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Extraversion)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Extraversion)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.91, y=7.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxExtraversion_raw.png", width=5, height=5)


# 13 - Originality and Agreeableness
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Agreeableness))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Agreeableness)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Agreeableness)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.90, y=7.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxAgreeableness_raw.png", width=5, height=5)



# 14 - Originality and Neuroticism
(tmpMPC <- with(nedaData, cor.test(x=Originality, y=Neuroticism))) # do the correlation for neda's data

with(nedaData, cor.test(x=Originality, y=Neuroticism)) # do the correlation
ggplot(data=nedaData, aes(x=Originality, y=Neuroticism)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=2.91, y=7.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OriginalityxNeuroticism_raw.png", width=5, height=5)



# 15 - Neuroticism and Originality
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Neuroticism, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Neuroticism, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.06, y=4, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_NeuroticismxOriginality_raw.png", width=5, height=5)



# 16 - Openness and Originality
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Openness, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Openness, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.3, y=4, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OpennessxOriginality_raw.png", width=5, height=5)


# 17 - Conscientiousness and Originality
(tmpMPC <- with(nedaData, cor.test(x=Conscientiousness, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Conscientiousness, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Conscientiousness, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.3, y=4, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_ConscientiousnessxOriginality_raw.png", width=5, height=5)


# 18 - Agreeableness and Originality
(tmpMPC <- with(nedaData, cor.test(x=Agreeableness, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Agreeableness, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Agreeableness, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=4, y=4, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_AgreeablenessxOriginality_raw.png", width=5, height=5)


# 19 - Neuroticism and Originality
(tmpMPC <- with(nedaData, cor.test(x=Neuroticism, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Neuroticism, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Neuroticism, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.1, y=4, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_NeuroticismxOriginality_raw.png", width=5, height=5)


# 19 - Sophisticated and Originality
(tmpMPC <- with(nedaData, cor.test(x=Sophisticated, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Sophisticated, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Sophisticated, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.23, y=4, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_SophisticatedxOriginality_raw.png", width=5, height=5)


------

# 16 - PercentofClustersGC and MusicSum
#(tmpMPC <- with(nedaData, cor.test(x=PercentofClustersGC, y=MusicSum))) # do the correlation for neda's data

#with(nedaData, cor.test(x=PercentofClustersGC, y=MusicSum)) # do the correlation
#ggplot(data=nedaData, aes(x=PercentofClustersGC, y=MusicSum)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=6.5, y=186, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=13),
#        axis.title=element_text(size=13,face="bold"))
#ggsave("correlation_PercentofClustersGCxMusicSum_raw.png", width=5, height=5)

#mean(nedaData$PercentClustersGC, na.rm = TRUE)
#sd(nedaData$PercentClustersGC, na.rm = TRUE)


##### Additional Age and Creativity Correlations -------------------------------

# 17 - Age and CAQ
(tmpMPC <- with(nedaData, cor.test(x=Age, y=CAQ))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=CAQ)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=CAQ)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=68, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexCAQ_raw.png", width=5, height=5)

------

# 18 - Age and NumberofGalleries
(tmpMPC <- with(nedaData, cor.test(x=Age, y=NumberofGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=NumberofGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=NumberofGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=40, y=202, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexNumberofGalleries_raw.png", width=5, height=5)

------

# 19 - Age and NumberofClusters
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=NumberofClusters))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=NumberofClusters)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=NumberofClusters)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=34, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexNumberofClusters_raw.png", width=5, height=5)

------

# Age and PercentShapesExplore
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=PercentShapesExplore))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=PercentShapesExplore)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=PercentShapesExplore)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=0.41, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexPercentShapesExplore_raw.png", width=5, height=5)

------

# Age and ExploitOptimality
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=ExploitOptimality))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=ExploitOptimality)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=ExploitOptimality)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=1.04, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexExploitOptimality_raw.png", width=5, height=5)

------

# Age and ExploreOptimality
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=ExploreOptimality))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=ExploreOptimality)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=ExploreOptimality)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=1.04, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexExploreOptimality_raw.png", width=5, height=5)

------

# Age and OptimalityRatio
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=OptimalityRatio))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=OptimalityRatio)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=OptimalityRatio)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=2.1, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexOptimalityRatio_raw.png", width=5, height=5)

------

# 20 - Age and Originality
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Originality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=Originality)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=Originality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=4.0, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexOriginality_raw.png", width=5, height=5)

------

# 21 - Age and PercentofClustersGC
#(tmpMPC <- with(nedaData, cor.test(x=Age, y=PercentofClustersGC))) # do the correlation for neda's data

#with(nedaData, cor.test(x=Age, y=PercentofClustersGC)) # do the correlation
#ggplot(data=nedaData, aes(x=Age, y=PercentofClustersGC)) + # start a ggplot
#  geom_point(col="steelblue1") + # add the Neda data
#  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
#  annotate(geom="text", x=39, y=19.5, size=4.5, label= 
#             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
#  theme_bw() + # make it simple
#  theme(axis.text=element_text(size=15),
#        axis.title=element_text(size=15,face="bold"))
#ggsave("correlation_AgexPercentofClustersGC_raw.png", width=5, height=5)


##### MPC CREATIVITY REGRESSIONS WITH AGE AND GENDER AS FACTORS ----------------

# 1 - Openness (IV) and CAQ (DV)
# create regression model. Here, CAQ is being predicted by Gender, Age, and Openness. Effect of Openness is calculated AFTER accounting for Gender and Age.
lm.24 <- lm(CAQ ~ Gender+Age+Openness, data=nedaData)

# run this and JUST look at the paragraph at the bottom. Ignore the table. The R2 value from your regression is 0.1435, and it is statistically significant (R2=0.1435, F(4,269)=15.03, p<0.0001). This means that AT LEAST ONE of the predictor factors is associated with a difference in the dependent variable (CAQ).
summary(lm.24)

# to figure out WHICH predictor variable is causing the significant regression, run this:
Anova(lm.24)
# based on this, there's a gender difference in CAQ. There's no age difference in CAQ. There is an Openness difference in CAQ. After controlling for gender and age, there is an association between CAQ and Openness.

------

# 2 - Neuroticism (IV) and CAQ (DV)
lm.25 <- lm(CAQ ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.25)
Anova(lm.25)

------

# 3 - CAQ (IV) and Depth (DV)
lm.26 <- lm(Depth ~ Gender+Age+CAQ, data=nedaData)
summary(lm.26)
Anova(lm.26)

------

# 4 - CAQ (IV) and Mellow (DV)
lm.27 <- lm(Mellow ~ Gender+Age+CAQ, data=nedaData)
summary(lm.27)
Anova(lm.27)

------

# 5 - Openness (IV) and MusicSum (DV)
lm.28 <- lm(MusicSum ~ Gender+Age+Openness, data=nedaData)
summary(lm.28)
Anova(lm.28)

------

# 6 - Openness (IV) and Arousal (DV)
lm.29 <- lm(Arousal ~ Gender+Age+Openness, data=nedaData)
summary(lm.29)
Anova(lm.29)

------

# 7 - NumberofGalleries (IV) and Contemporary (DV)
lm.30 <- lm(Contemporary ~ Gender+Age+NumberofGalleries, data=nedaData)
summary(lm.30)
Anova(lm.30)

------
  
# 8 - NumberofClusters (IV) and Contemporary (DV)
#lm.31 <- lm(Contemporary ~ Gender+Age+NumberofClusters, data=nedaData)
#summary(lm.31)
#Anova(lm.31)

------

# PercentShapesExplore (IV) and Sophisticated (DV)
#lm.18 <- lm(Sophisticated ~ Gender+Age+PercentShapesExplore, data=nedaData)
#summary(lm.18)
#Anova(lm.18)

------  
  
# PercentShapesExplore (IV) and Neuroticism (DV)
#lm.19 <- lm(Neuroticism ~ Gender+Age+PercentShapesExplore, data=nedaData)
#summary(lm.19)
#Anova(lm.19)

------

# 9 - Age (IV) and PercentofTimeExplored (DV)
#lm.32 <- lm(PercentofTimeExplored ~ Gender+Age, data=nedaData)
#summary(lm.32)
#Anova(lm.32)

------

# 10 - PercentofTimeExplored (IV) and Unpretentious (DV)
#lm.33 <- lm(Unpretentious ~ Gender+Age+PercentofTimeExplored, data=nedaData)
#summary(lm.33)
#Anova(lm.33)

------

# 11 - PercentofTimeExplored (IV) and Sophisticated (DV)
#lm.34 <- lm(Sophisticated ~ Gender+Age+PercentofTimeExplored, data=nedaData)
#summary(lm.34)
#Anova(lm.34)

------

# 12 - PercentofTimeExplored (IV) and Arousal (DV)
#lm.35 <- lm(Arousal ~ Gender+Age+PercentofTimeExplored, data=nedaData)
#summary(lm.35)
#Anova(lm.35)

------

# ExploitOptimality (IV) and Valence (DV)
#lm.24 <- lm(Valence ~ Gender+Age+ExploitOptimality, data=nedaData)
#summary(lm.24)
#Anova(lm.24)

------

# ExploreOptimality (IV) and Contemporary (DV)
#lm.25 <- lm(Contemporary ~ Gender+Age+ExploreOptimality, data=nedaData)
#summary(lm.25)
#Anova(lm.25)
  
------  

# ExploreOptimality (IV) and Valence (DV)
#lm.26 <- lm(Valence ~ Gender+Age+ExploreOptimality, data=nedaData)
#summary(lm.26)
#Anova(lm.26)
  
------  

# Openness (IV) and OptimalityRatio (DV)
#lm.27 <- lm(Openness ~ Gender+Age+OptimalityRatio, data=nedaData)
#summary(lm.27)
#Anova(lm.27)

------  
  
# Age (IV) and MedianNumStepsExploit (DV)
#lm.28 <- lm(MedianNumStepsExploit ~ Gender+Age, data=nedaData)
#summary(lm.28)
#Anova(lm.28)
  
------  

# Extraversion (IV) and MedianNumStepsExploit (DV)
#lm.29 <- lm(MedianNumStepsExploit ~ Gender+Age+Extraversion, data=nedaData)
#summary(lm.29)
#Anova(lm.29)

------  
  
# Age (IV) and MedianNumStepsExplore (DV)
#lm.30 <- lm(MedianNumStepsExplore ~ Gender+Age, data=nedaData)
#summary(lm.30)
#Anova(lm.30)

------    

# MedianNumStepsExplore (IV) and MusicSum (DV)
#lm.31 <- lm(MusicSum ~ Gender+Age+MedianNumStepsExplore, data=nedaData)
#summary(lm.31)
#Anova(lm.31)
  
------  

# 13 - Originality (IV) and Contemporary (DV)
lm.36 <- lm(Contemporary ~ Gender+Age+Originality, data=nedaData)
summary(lm.36)
Anova(lm.36)

------

# 14 - PercentofClustersGC (IV) and MusicSum (DV)
#lm.37 <- lm(MusicSum ~ Gender+Age+PercentofClustersGC, data=nedaData)
#summary(lm.37)
#Anova(lm.37)

------

# make the Creativity Regression Coefficient tables
stargazer(lm.10, type="text", out="/Users/nedarad/Desktop/OpennessxCAQ.text")
stargazer(lm.11, type="html", out="/Users/nedarad/Desktop/NeuroticismxCAQ.html")
stargazer(lm.12, type="html", out="/Users/nedarad/Desktop/CAQxDepth.html")
stargazer(lm.13, type="html", out="/Users/nedarad/Desktop/CAQxMellow.html")
stargazer(lm.14, type="html", out="/Users/nedarad/Desktop/OpennessxMusicSum.html")
stargazer(lm.15, type="html", out="/Users/nedarad/Desktop/OpennessxArousal.html")
stargazer(lm.16, type="html", out="/Users/nedarad/Desktop/NumGalleriesxContemporary.html")
stargazer(lm.17, type="html", out="/Users/nedarad/Desktop/NumClustersxContemporary.html")
stargazer(lm.18, type="html", out="/Users/nedarad/Desktop/PercentShapesExplorexSophisticated.html")
stargazer(lm.19, type="html", out="/Users/nedarad/Desktop/PercentShapesExplorexNeuroticism.html")
stargazer(lm.20, type="html", out="/Users/nedarad/Desktop/AgexPercentTimeExplore")
stargazer(lm.21, type="html", out="/Users/nedarad/Desktop/PercentTimeExplorexUnpretentious.html")
stargazer(lm.22, type="html", out="/Users/nedarad/Desktop/PercentTimeExplorexSophisticated.html")
stargazer(lm.23, type="html", out="/Users/nedarad/Desktop/PercentTimeExplorexArousal.html")
stargazer(lm.24, type="html", out="/Users/nedarad/Desktop/ExploitOptimalityxValence.html")
stargazer(lm.25, type="html", out="/Users/nedarad/Desktop/ExploreOptimalityxContemporary.html")
stargazer(lm.26, type="html", out="/Users/nedarad/Desktop/ExploreOptimalityxValence.html")
stargazer(lm.27, type="html", out="/Users/nedarad/Desktop/OpennessxOptimalityRatio.html")
stargazer(lm.28, type="html", out="/Users/nedarad/Desktop/AgexMedianNumStepsExploit.html")
stargazer(lm.29, type="html", out="/Users/nedarad/Desktop/ExtraversionxMedianNumStepsExploit.html")
stargazer(lm.30, type="html", out="/Users/nedarad/Desktop/AgexMedianNumStepsExplore.html")
stargazer(lm.31, type="html", out="/Users/nedarad/Desktop/MedianNumStepsExplorexMusicSum.html")
stargazer(lm.32, type="html", out="/Users/nedarad/Desktop/OriginalityxContemporary.html")
stargazer(lm.33, type="html", out="/Users/nedarad/Desktop/PercentClustersGCxMusicSum.html")


################


###### AIM THREE ---------------------------------------------------------------

# Goal: Do measures of creativity associated with musical preference STILL correlate after we control for Openness?

# 1 - CAQ (IV) + Mellow (DV) -> according to the correlation matrix, CAQ and Mellow are correlated.
with(nedaData, cor.test(CAQ, Mellow))

summary(lm(Mellow ~ CAQ, data=nedaData)) # this shows how much Mellow is predicted by CAQ

summary(lm(Mellow ~ Age+Gender, data=nedaData))
summary(lm(Mellow ~ Age+Gender+Openness, data=nedaData))
summary(lm(Mellow ~ Age+Gender+Openness+CAQ, data=nedaData)) # This shows how much Mellow is predicted by CAQ after controlling for Openness (and also how much Mellow is predicted by Openness after controlling for CAQ)
# After controlling for Age, Gender, and Openness, Mellow was still significantly predicted by CAQ (t(268)=2.73, p=0.03).

# Step-Wise Regression Code 
# define intercept-only model
#intercept_only <- lm(Mellow ~ Age+Gender+Openness, data=nedaData)

# define model with all predictors
#all <- lm(Mellow ~ Age+Gender+Openness+CAQ, data=nedaData)

# perform forward stepwise regression
#forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)

# view results of forward stepwise regression
#forward$anova

# view final model
#forward$coefficients

------

# 2 - CAQ (IV) + Depth (DV)
summary(lm(Depth ~ Age+Gender, data=nedaData))
summary(lm(Depth ~ Age+Gender+Openness, data=nedaData))
summary(lm(Depth ~ Age+Gender+Openness+CAQ, data=nedaData)) # significant

------

# 3 - NumberofGalleries (IV) + Contemporary (DV)
summary(lm(Contemporary ~ Age+Gender, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness+NumberofGalleries, data=nedaData)) #significant

------

# 4 - Number of Clusters (IV) + Contemporary (DV) 
#summary(lm(Contemporary ~ Age+Gender, data=nedaData))
#summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Contemporary ~ Age+Gender+Openness+NumberofClusters, data=nedaData)) #significant

------

# PercentShapesExplore (IV) + Sophisticated (DV)
#summary(lm(Sophisticated ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Sophisticated ~ Age+Gender+Openness+PercentShapesExplore, data=nedaData)) #significant

------  
  
# 5 - PrecentofTimeExplored (IV) + Unpretentious (DV)
#summary(lm(Unpretentious ~ Age+Gender, data=nedaData))
#summary(lm(Unpretentious ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Unpretentious ~ Age+Gender+Openness+PercentofTimeExplored, data=nedaData)) #significant

------

# 6 - PercentofTimeExplored (IV) + Sophisticated (DV)
#summary(lm(Sophisticated ~ Age+Gender, data=nedaData))
#summary(lm(Sophisticated ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Sophisticated ~ Age+Gender+Openness+PercentofTimeExplored, data=nedaData)) #significant

------

# 7 - PercentofTimeExplored (IV) + Arousal (DV)
#summary(lm(Arousal ~ Age+Gender, data=nedaData))
#summary(lm(Arousal ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Arousal ~ Age+Gender+Openness+PercentofTimeExplored, data=nedaData)) #significant

------

# ExploitOptimality (IV) + Valence (DV)
#summary(lm(Valence ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Valence ~ Age+Gender+Openness+ExploitOptimality, data=nedaData)) #significant

# ExploreOptimality (IV) + Contemporary (DV)
#summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Contemporary ~ Age+Gender+Openness+ExploreOptimality, data=nedaData)) #significant

# ExploreOptimality (IV) + Valence (DV)
#summary(lm(Valence ~ Age+Gender+Openness, data=nedaData))
#summary(lm(Valence ~ Age+Gender+Openness+ExploreOptimality, data=nedaData)) #significant

# MedianNumStepsExplore (IV) + MusicSum (DV)
#summary(lm(MusicSum ~ Age+Gender+Openness, data=nedaData))
#summary(lm(MusicSum ~ Age+Gender+Openness+MedianNumStepsExplore, data=nedaData)) #not significant

------

# 8 - Originality (IV) + Contemporary (DV)
summary(lm(Contemporary ~ Age+Gender, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness+Originality, data=nedaData)) #significant

------

# 9 - PercentofClustersGC (IV) + MusicSum (DV)
#summary(lm(MusicSum ~ Age+Gender, data=nedaData))
#summary(lm(MusicSum ~ Age+Gender+Openness, data=nedaData))
#summary(lm(MusicSum ~ Age+Gender+Openness+PercentofClustersGC, data=nedaData)) # not significant

#----FIN-----