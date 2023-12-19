# Everything

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


## load neda dataset
nedaDataRaw <- read.csv("nedaDataComplete.csv", header=T, stringsAsFactors=T)
dataLegend <- read.csv("legend.csv", header=T)

#mean(nedaData$Age, na.rm = TRUE)
#sd(nedaData$Age, na.rm = TRUE)

# check gender and age. Get rid of everyone not male/female, and check age range to use for MU restriction
unique(nedaDataRaw$Gender)
nedaData <- subset(nedaDataRaw, Gender=="Male"|Gender=="Female")
range(nedaData$Age) # min 18, max 79
dim(nedaData) # 273 total 

with(nedaData, tapply(Gender, Gender, length)) / sum(with(nedaData, tapply(Gender, Gender, length)), na.rm=T)
with(nedaData, tapply(Gender, Gender, length)) 

# load MU data
MUdata <- read.csv("musicalUniverse.csv", header = T)
MUdata <- MUdata[,c(21,22,41:45,81:89)]
names(MUdata) <- c("Gender", "Age", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism",
                   "MusicSum", "Mellow", "Unpretentious", "Sophisticated", "Intense", "Contemporary", "Arousal", "Valence", "Depth")
MUdata$Gender <- factor(MUdata$Gender, labels = c("Female", "Male", "Transgender", "Other Non Binary", "I Rather Not Say"))
dim(MUdata) # originally have 64815 people
MUdata <- subset(MUdata, Gender=="Male"|Gender=="Female")
dim(MUdata) # after removing non-male and non-female answers, have 62161 people
MUdata <- subset(MUdata, Age<80&Age>17)
dim(MUdata) # after limiting age range to match nedaData, have 47540 people. THIS IS YOUR FINAL MU DATASET TO BE USED.

mean(MUdata$Age, na.rm = TRUE)
sd(MUdata$Age, na.rm = TRUE)

with(MUdata, tapply(Gender, Gender, length)) / sum(with(MUdata, tapply(Gender, Gender, length)), na.rm=T)
with(MUdata, tapply(Gender, Gender, length)) 


###################### AIM 1 ##################################################

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

# create correlation matrices...
install.packages("Hmisc")
library("Hmisc")
MU_aim1cors <- rcorr(as.matrix(MUdata[,2:16]))
write.csv(MU_aim1cors$r, "MU_aim1cors_rvals.csv")
write.csv(MU_aim1cors$P, "MU_aim1cors_pvals.csv")

nedaDataRESORTED <- nedaData[,c(names(MUdata[,2:16]))]
MPC_aim1cors <- rcorr(as.matrix(nedaDataRESORTED))
write.csv(MPC_aim1cors$r, "MPC_aim1cors_rvals.csv")
write.csv(MPC_aim1cors$P, "MUPCaim1cors_pvals.csv")


# (1) Gender and Mellow
ggplot(subset(allDatalong, variable=="Mellow"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Mellow") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Mellow")))
ggsave("boxplot_GenderxMellow_raw_new.png", width=5, height=5)

# Mellow scores were significantly different between male and females (F(1,47809)=90.91, p<0.001),
# but this difference was not significantly different between datasets (F(1,47809)=0.09, p=0.77)

# (2) Gender and Unpretentious 
ggplot(subset(allDatalong, variable=="Unpretentious"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Unpretentious") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Unpretentious")))
ggsave("boxplot_GenderxUnpretentious_raw_new.png", width=5, height=5)

# Unpretentious scores were significantly different between male and females (F(1,47809)=131.61, p < 0.001)
# but this difference was not significantly different between datasets (F(1,47809)=2.17, p=0.14)

# (3) Gender and Intense 
ggplot(subset(allDatalong, variable=="Intense"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Intense") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Intense")))
ggsave("boxplot_GenderxIntense_raw_new.png", width=5, height=5)

# Intense scores were significantly different between male and females (F (1,47809) = 389.75, p < 0.001)
# but this difference was not significantly different between datasets (F(1,47809)= 1.98, p = 0.16)

# (4) Gender and Sophisticated 
ggplot(subset(allDatalong, variable=="Sophisticated"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Sophisticated") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Sophisticated")))
ggsave("boxplot_GenderxSophisticated_raw_new.png", width=5, height=5)

# Sophisticated scores were significantly different between male and females (F (1,47809)=42.16, p < 0.001)
# but this difference was not significantly different between datasets (F(1,47809)=1.21, p=0.27)


# (5) Gender and Contemporary 
ggplot(subset(allDatalong, variable=="Contemporary"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Contemporary") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Contemporary")))
ggsave("boxplot_GenderxContemporary_raw_new.png", width=5, height=5)

# Contemporary scores were significantly different between male and females (F (1,47809) = 94.01, p < 0.001)
# but this difference was not significantly different between datasets (F(1,47809) = 0.09, p = 0.77)


# (6) Gender and Arousal 
ggplot(subset(allDatalong, variable=="Arousal"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Arousal") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Arousal")))
ggsave("boxplot_GenderxArousal_raw_new.png", width=5, height=5)

# Arousal scores were significantly different between male and females (F (1, 47809) = 286.86, p < 0.001)
# but this difference was not significantly different between datasets (F(1, 47809) = 0.82, p = 0.36)


# (7) Gender and Depth 
ggplot(subset(allDatalong, variable=="Depth"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Depth") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Depth")))
ggsave("boxplot_GenderxDepth_raw_new.png", width=5, height=5)

# Depth scores were significantly different between male and females (F (1, 47809) = 63.53, p < 0.001)
# but this difference was not significantly different between datasets (F(1, 47809) = 0.10, p = 0.75)


# (8) Gender and Valence 
ggplot(subset(allDatalong, variable=="Valence"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("Valence") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="Valence")))
ggsave("boxplot_GenderxValence_raw_new.png", width=5, height=5)

# Arousal scores were significantly different between male and females (F (1, 47809) = 98.48, p < 0.001)
# but this difference was not significantly different between datasets (F(1, 47809) = 2.39, p = 0.12)


# (9) Gender and MusicSum 
ggplot(subset(allDatalong, variable=="MusicSum"))+
  geom_boxplot(aes(x=Dataset,y=value, col=Gender)) +
  ylab("MusicSum") +
  scale_color_manual(values = c("plum1", "springgreen3")) +
  theme_bw() 
anova(lm(value ~ Gender*Dataset, data=subset(allDatalong, variable=="MusicSum")))
ggsave("boxplot_GenderxMusicSum_raw_new.png", width=5, height=5)

# MusicSum scores were not significantly different between male and females (F (1, 47809) = 1.52, p = 0.22)
# but this difference was significantly different between datasets (F(1, 47809) = 7.72, p = 0.01)


##### Age Correlations #####

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


# (3) Age and Unpretentious  
(tmpMU <- with(MUdata, cor.test(x=Age, y=Unpretentious))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Age, y=Unpretentious))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

#fisher1925: Fisher's z (1925)
  #z = 4.1421, p-value = 0.0000
  #Null hypothesis rejected

ggplot() + # start a ggplot
  #  geom_point(data=MUdata, aes(x=Age, y=Unpretentious), col="darkgrey") + # add the Neda data
  geom_point(data=nedaData, aes(x=Age, y=Unpretentious), col="springgreen3") + # add the Neda data
  stat_smooth(method=lm, data=MUdata, aes(x=Age, y=Unpretentious), col="darkgrey") + # add the musical universe line
  stat_smooth(method=lm, data=nedaData, aes(x=Age, y=Unpretentious), col="springgreen3") + # add the neda line
  annotate(geom="text", x=35, y=0.31, label=
             paste0("MU: r = ",signif(as.numeric(tmpMU$estimate),2), ", df = ",as.numeric(tmpMU$parameter), ", p ", if(tmpMU$p.value<0.001)("< 0.001")else(signif(as.numeric(tmpMU$p.value),2)), 
                    "\n MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() # make it pretty
ggsave("correlation_AgexUnpretentious_raw_new.png", width=5, height=5)

# (4) Age and Sophisticated
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

# (5) Age and Contemporary   
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


# (6) Age and Valence   
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


# (7) Age and Arousal   
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


# (8) Age and Depth   
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


# (9) Age and MusicSum   
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


##### All Key Correlations ----------------------------------------------

# 1 - Openness (IV) and Sophisticated (DV)

(tmpMU <- with(MUdata, cor.test(x=Openness, y=Sophisticated))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=Sophisticated))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

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


# 9 - Openness (IV) and Musicsum (DV)

(tmpMU <- with(MUdata, cor.test(x=Openness, y=MusicSum))) # do the correlation for musical universe
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=MusicSum))) # do the correlation for neda's data
cocor.indep.groups(r1.jk=tmpMU$estimate, r2.hm=tmpMPC$estimate, n1=tmpMU$parameter+2, n2=tmpMPC$parameter+2, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

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



#################=============================================================

### REGRESSIONS WITH AGE AND GENDER AS FACTORS
# 1 - Openness (IV) and Sophisticated (DV)

# MUData
# create regression model. Here, MusicSum is being predicted by Gender, Age, and CAQ. Yes, effect of CAQ is calculated AFTER accounting for Gender and Age.
lm.01a <- lm(Sophisticated ~ Gender+Age+Openness, data=MUdata)

# run this and JUST look at the paragraph at the bottom. Ignore the table. The R2 value from your regression is 0.09001, and it is statistically significant (R2=0.09, F(4,279)=6.899, p<0.0001). This means that AT LEAST ONE of the predictor factors is associated with a difference in the dependent variable (MusicSum).
summary(lm.01a)

# to figure out WHICH predictor variable is causing the significant regression, run this:
Anova(lm.01a)
# based on this, there's a gender difference in MusicSum. There's no age difference in MusicSum. After controlling for gender and age, there is no association between MusicSum and CAQ.


# nedaData
# create regression model. Here, Sophisticated is being  predicted by Gender, Age, and Openness. Yes, effect of Openness is calculated AFTER accounting for Gender and Age.
lm.01b <- lm(Sophisticated ~ Gender+Age+Openness, data=nedaData)

# run this and JUST look at the paragraph at the bottom. Ignore the table. The R2 value from your regression is 0.01, and is not statistically significant (R2=0.01, F(4,279)=0.88, p=0.48).
summary(lm.01b)

# to figure out WHICH predictor variable is causing the significant regression, run this:
Anova(lm.01b)
# based on this, there is not a significant Gender difference in Sophisticated (F(2,279)=0.85, p=0.43). Nor is there a significant Age difference in Sophisticated (F(1,279)=0.02, p=0.90). After controlling for Gender and Age, the association is not significant between Openness and Sophisticated (F(1,279)=1.38, p=0.24).

# ------

# 2 - Conscientiousness (IV) and Intense (DV)

# MUData
lm.02a <- lm(Intense ~ Gender+Age+Conscientiousness, data=MUdata)
summary(lm.02a)
Anova(lm.02a)

# nedaData
lm.02b <- lm(Intense ~ Gender+Age+Conscientiousness, data=nedaData)
summary(lm.02b)
Anova(lm.02b)

# ------

# 3 - Extraversion (IV) and Unpretentious (DV)

# MUData
lm.03a <- lm(Unpretentious ~ Gender+Age+Extraversion, data=MUdata)
summary(lm.03a)
Anova(lm.03a)

# nedaData
lm.03b <- lm(Unpretentious ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.03b)
Anova(lm.03b)


# ------

# 4 - Extraversion (IV) and Contemporary (DV)

# MUData
lm.04a <- lm(Contemporary ~ Gender+Age+Extraversion, data=MUdata)
summary(lm.04a)
Anova(lm.04a)

# nedaData
lm.04b <- lm(Contemporary ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.04b)
Anova(lm.04b)

# ------

# 5 - Agreeableness (IV) and Mellow (DV)

# MUData
lm.05a <- lm(Mellow ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.05a)
Anova(lm.05a)

# nedaData
lm.05b <- lm(Mellow ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.05b)
Anova(lm.05b)

# ------

# 6 - Agreeableness (IV) and Unpretentious (DV)

# MUData
lm.06a <- lm(Unpretentious ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.06a)
Anova(lm.06a)

# nedaData
lm.06b <- lm(Unpretentious ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.06b)
Anova(lm.06b)

# ------

# 7 - Agreeableness (IV) and Intense (DV)

# MUData
lm.07a <- lm(Intense ~ Gender+Age+Agreeableness, data=MUdata)
summary(lm.07a)
Anova(lm.07a)

# nedaData
lm.07b <- lm(Intense ~ Gender+Age+Agreeableness, data=nedaData)
summary(lm.07b)
Anova(lm.07b)

# ------

# 8 - Neuroticism (IV) and Intense (DV)

# MUData
lm.08a <- lm(Intense ~ Gender+Age+Neuroticism, data=MUdata)
summary(lm.08a)
Anova(lm.08a)

# nedaData
lm.08b <- lm(Intense ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.08b)
Anova(lm.08b)

# ------

# 9 - Openness (IV) and MusicSum (DV)

# MUData
lm.09a <- lm(MusicSum ~ Gender+Age+Openness, data=MUdata)
summary(lm.09a)
Anova(lm.09a)

# nedaData
lm.09b <- lm(MusicSum ~ Gender+Age+Openness, data=nedaData)
summary(lm.09b)
Anova(lm.09b)

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



######################################
######################################
######################################
######################################
######################################

# AIM 2 - Creativity Stuff!!!

nedaDatalong_creativity <- melt(nedaData[,c(1,2,18:29)], id.vars=c("ProlificID","Gender"))

ggplot(nedaDatalong_creativity) +
  geom_boxplot(aes(y=value, x=Gender, col=Gender)) +
  facet_wrap(~variable, scales="free") + 
  scale_color_manual(values = c("orchid1", "steelblue1")) +
  theme_bw()
ggsave("boxplot_GenderxAllCreativity_raw.png", width=8.5, height=8.5)

# Intense scores were significantly different between male and females (F (1,47809) = 389.75, p < 0.001)
# but this difference was not significantly different between datasets (F(1,47809)= 1.98, p = 0.16)

with(nedaData, t.test(CAQ ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(NumGalleries ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(NumClusters ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(PercentShapesExplore ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(PercentTimeExplore ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(ExploitOptimality ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(ExploreOptimality ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(OptimalityRatio ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(MedianNumStepsExploit ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(MedianNumStepsExplore ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(Originality ~ Gender, data=nedaData, var.equal=T))
with(nedaData, t.test(PercentClustersGC ~ Gender, data=nedaData, var.equal=T))
# How to report this, example: t(271) = -2.37, p = 0.018 


# Correlation Matrix
allCors = cor(nedaData[,c(-1,-2)], use = "complete.obs")
cor_test_mat <- corr.test(nedaData[,c(-1,-2)])$p

# Create Correlation Matrix and customize the colours
corrplot(allCors, method="ellipse", type="lower", diag=T, order="original",
         col=brewer.pal(n=8, name="PuBuGn"), p.mat = cor_test_mat,insig = "blank", tl.col=c("slategray2", "steelblue1","steelblue1",
                                                                                            "steelblue1","steelblue1","steelblue1","steelblue1","steelblue1","steelblue1","steelblue1","midnightblue","midnightblue",
                                                                                            "midnightblue","midnightblue","midnightblue","orchid1","seagreen3","seagreen3","seagreen3","seagreen3","seagreen3",
                                                                                            "seagreen3","seagreen3","seagreen3","seagreen3","seagreen3","seagreen3"), tl.srt=45, tl.cex=0.6)

# code to create and save the correlation coefficient matrix in csv format on desktop, only necessary once
write.csv(round(allCors,4), "nedaCorMatrix.csv", row.names=T)


# Creativity Correlations MPC

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
ggsave("correlation_OpennessxCAQ.png_raw", width=5, height=5)
# As seen in Figure 5, CAQ and Openness were significantly correlated (r=0.37, df=282, p<0.001).
# The correlation between CAQ and and Openness was significant (r=0.37, df=282, p<0.001).


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
# As seen in Figure 5, CAQ and Openness were significantly correlated (r=0.37, df=282, p<0.001).
# The correlation between CAQ and and Openness was significant (r=0.37, df=282, p<0.001).


# 5 - Openness and MusicSum
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


# 6 - Openness and Arousal
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


# 7 - NumGalleries and Contemporary
(tmpMPC <- with(nedaData, cor.test(x=NumGalleries, y=Contemporary))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumGalleries, y=Contemporary)) # do the correlation
ggplot(data=nedaData, aes(x=NumGalleries, y=Contemporary)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=86, y=0.30, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumGalleriesxContemporary_raw.png", width=5, height=5)


# 8 - NumClusters and Contemporary
(tmpMPC <- with(nedaData, cor.test(x=NumClusters, y=Contemporary))) # do the correlation for neda's data

with(nedaData, cor.test(x=NumClusters, y=Contemporary)) # do the correlation
ggplot(data=nedaData, aes(x=NumClusters, y=Contemporary)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=14.5, y=0.30, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_NumClustersxContemporary_raw.png", width=5, height=5)


# 9 - PercentShapesExplore and Sophisticated
(tmpMPC <- with(nedaData, cor.test(x=PercentShapesExplore, y=Sophisticated))) # do the correlation for neda's data

with(nedaData, cor.test(x=PercentShapesExplore, y=Sophisticated)) # do the correlation
ggplot(data=nedaData, aes(x=PercentShapesExplore, y=Sophisticated)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.17, y=0.35, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_PercentShapesExplorexSophisticated_raw.png", width=5, height=5)


# 10 - PercentShapesExplore and Neuroticism
(tmpMPC <- with(nedaData, cor.test(x=PercentShapesExplore, y=Neuroticism))) # do the correlation for neda's data

with(nedaData, cor.test(x=PercentShapesExplore, y=Neuroticism)) # do the correlation
ggplot(data=nedaData, aes(x=PercentShapesExplore, y=Neuroticism)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.16, y=7.5, size=4.5, label=
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_PercentShapesExplorexNeuroticism_raw.png", width=5, height=5)


# 11 - PercentTimeExplore and Age
(tmpMPC <- with(nedaData, cor.test(x=Age, y=PercentTimeExplore))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=PercentTimeExplore)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=PercentTimeExplore)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=0.55, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexPercentTimeExplore_raw.png", width=5, height=5)


# 12 - PercentTimeExplore and Upretentious
(tmpMPC <- with(nedaData, cor.test(x=PercentTimeExplore, y=Unpretentious))) # do the correlation for neda's data

with(nedaData, cor.test(x=PercentTimeExplore, y=Unpretentious)) # do the correlation
ggplot(data=nedaData, aes(x=PercentTimeExplore, y=Unpretentious)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.21, y=0.30, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_PercentTimeExplorexUnpretentious_raw.png", width=5, height=5)


# 13 - PercentTimeExplore and Sophisticated
(tmpMPC <- with(nedaData, cor.test(x=PercentTimeExplore, y=Sophisticated))) # do the correlation for neda's data

with(nedaData, cor.test(x=PercentTimeExplore, y=Sophisticated)) # do the correlation
ggplot(data=nedaData, aes(x=PercentTimeExplore, y=Sophisticated)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.21, y=0.35, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_PercentTimeExplorexSophisticated_raw.png", width=5, height=5)


# 14 - PercentTimeExplore and Arousal
(tmpMPC <- with(nedaData, cor.test(x=PercentTimeExplore, y=Arousal))) # do the correlation for neda's data

with(nedaData, cor.test(x=PercentTimeExplore, y=Arousal)) # do the correlation
ggplot(data=nedaData, aes(x=PercentTimeExplore, y=Arousal)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.21, y=0.26, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_PercentTimeExplorexArousal_raw.png", width=5, height=5)


# 15 - ExploitOptimality and Valence
(tmpMPC <- with(nedaData, cor.test(x=ExploitOptimality, y=Valence))) # do the correlation for neda's data

with(nedaData, cor.test(x=ExploitOptimality, y=Valence)) # do the correlation
ggplot(data=nedaData, aes(x=ExploitOptimality, y=Valence)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.36, y=-0.07, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_ExploitOptimalityxValence_raw.png", width=5, height=5)


# 16 - ExploreOptimality and Contemporary
(tmpMPC <- with(nedaData, cor.test(x=ExploreOptimality, y=Contemporary))) # do the correlation for neda's data

with(nedaData, cor.test(x=ExploreOptimality, y=Contemporary)) # do the correlation
ggplot(data=nedaData, aes(x=ExploreOptimality, y=Contemporary)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.36, y=0.30, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_ExploreOptimalityxContemporary_raw.png", width=5, height=5)


# 17 - ExploreOptimality and Valence
(tmpMPC <- with(nedaData, cor.test(x=ExploreOptimality, y=Valence))) # do the correlation for neda's data

with(nedaData, cor.test(x=ExploreOptimality, y=Valence)) # do the correlation
ggplot(data=nedaData, aes(x=ExploreOptimality, y=Valence)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=0.36, y=-0.06, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_ExploreOptimalityxValence_raw.png", width=5, height=5)


# 18 - Openness and OptimalityRatio
(tmpMPC <- with(nedaData, cor.test(x=Openness, y=OptimalityRatio))) # do the correlation for neda's data

with(nedaData, cor.test(x=Openness, y=OptimalityRatio)) # do the correlation
ggplot(data=nedaData, aes(x=Openness, y=OptimalityRatio)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3.4, y=2.0, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_OpennessxOptimalityRatio_raw.png", width=5, height=5)


# 19 -Age and MedianNumStepsExploit
(tmpMPC <- with(nedaData, cor.test(x=Age, y=MedianNumStepsExploit))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=MedianNumStepsExploit)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=MedianNumStepsExploit)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=16.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_AgexMedianNumStepsExploit_raw.png", width=5, height=5)


# 20 - Extraversion and MedianNumStepsExploit
(tmpMPC <- with(nedaData, cor.test(x=Extraversion, y=MedianNumStepsExploit))) # do the correlation for neda's data

with(nedaData, cor.test(x=Extraversion, y=MedianNumStepsExploit)) # do the correlation
ggplot(data=nedaData, aes(x=Extraversion, y=MedianNumStepsExploit)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=3, y=16.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_ExtraversionxMedianNumStepsExploit_raw.png", width=5, height=5)


# 21 -Age and MedianNumStepsExplore
(tmpMPC <- with(nedaData, cor.test(x=Age, y=MedianNumStepsExplore))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=MedianNumStepsExplore)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=MedianNumStepsExplore)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=38, y=23, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_AgexMedianNumStepsExplore_raw.png", width=5, height=5)


# 22 - MedianNumStepsExplore and MusicSum
(tmpMPC <- with(nedaData, cor.test(x=MedianNumStepsExplore, y=MusicSum))) # do the correlation for neda's data

with(nedaData, cor.test(x=MedianNumStepsExplore, y=MusicSum)) # do the correlation
ggplot(data=nedaData, aes(x=MedianNumStepsExplore, y=MusicSum)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=9.5, y=188, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_MedianNumStepsExplorexMusicSum_raw.png", width=5, height=5)


# 23 - Originality and Contemporary
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


# 24 - PercentClustersGC and MusicSum
(tmpMPC <- with(nedaData, cor.test(x=PercentClustersGC, y=MusicSum))) # do the correlation for neda's data

with(nedaData, cor.test(x=PercentClustersGC, y=MusicSum)) # do the correlation
ggplot(data=nedaData, aes(x=PercentClustersGC, y=MusicSum)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=6.5, y=186, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))
ggsave("correlation_PercentClustersGCxMusicSum_raw.png", width=5, height=5)



##### Additional Age and Creativity Correlations

# 1 - Age and CAQ
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


# 2 - Age and NumGalleries
(tmpMPC <- with(nedaData, cor.test(x=Age, y=NumGalleries))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=NumGalleries)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=NumGalleries)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=40, y=202, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexNumGalleries_raw.png", width=5, height=5)


# 3 - Age and NumClusters
(tmpMPC <- with(nedaData, cor.test(x=Age, y=NumClusters))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=NumClusters)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=NumClusters)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=34, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexNumClusters_raw.png", width=5, height=5)


# 4 - Age and PercentShapesExplore
(tmpMPC <- with(nedaData, cor.test(x=Age, y=PercentShapesExplore))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=PercentShapesExplore)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=PercentShapesExplore)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=0.41, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexPercentShapesExplore_raw.png", width=5, height=5)


# 5 - Age and ExploitOptimality
(tmpMPC <- with(nedaData, cor.test(x=Age, y=ExploitOptimality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=ExploitOptimality)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=ExploitOptimality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=1.04, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexExploitOptimality_raw.png", width=5, height=5)


# 5 - Age and ExploreOptimality
(tmpMPC <- with(nedaData, cor.test(x=Age, y=ExploreOptimality))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=ExploreOptimality)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=ExploreOptimality)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=1.04, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexExploreOptimality_raw.png", width=5, height=5)


# 6 - Age and OptimalityRatio
(tmpMPC <- with(nedaData, cor.test(x=Age, y=OptimalityRatio))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=OptimalityRatio)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=OptimalityRatio)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=2.1, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexOptimalityRatio_raw.png", width=5, height=5)


# 7 - Age and Originality
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


# 8 - Age and PercentClustersGC
(tmpMPC <- with(nedaData, cor.test(x=Age, y=PercentClustersGC))) # do the correlation for neda's data

with(nedaData, cor.test(x=Age, y=PercentClustersGC)) # do the correlation
ggplot(data=nedaData, aes(x=Age, y=PercentClustersGC)) + # start a ggplot
  geom_point(col="steelblue1") + # add the Neda data
  stat_smooth(method=lm, col="steelblue1") + #add the correlation line
  annotate(geom="text", x=39, y=19.5, size=4.5, label= 
             paste0("MPC: r = ",signif(as.numeric(tmpMPC$estimate),2), ", df = ",as.numeric(tmpMPC$parameter), ", p ", if(tmpMPC$p.value<0.001)("< 0.001")else(paste0("= ",signif(as.numeric(tmpMPC$p.value),2)))), color="navyblue")+
  theme_bw() + # make it simple
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))
ggsave("correlation_AgexPercentClustersGC_raw.png", width=5, height=5)



### MPC CREATIVITY REGRESSIONS WITH AGE AND GENDER AS FACTORS

# 10 - Openness (IV) and CAQ (DV)
# create regression model. Here, CAQ is being predicted by Gender, Age, and Openness. Yes, effect of Openness is calculated AFTER accounting for Gender and Age.
lm.10 <- lm(CAQ ~ Gender+Age+Openness, data=nedaData)

# run this and JUST look at the paragraph at the bottom. Ignore the table. The R2 value from your regression is 0.09001, and it is statistically significant (R2=0.09, F(4,279)=6.899, p<0.0001). This means that AT LEAST ONE of the predictor factors is associated with a difference in the dependent variable (MusicSum).
summary(lm.10)

# to figure out WHICH predictor variable is causing the significant regression, run this:
Anova(lm.10)
# based on this, there's a gender difference in MusicSum. There's no age difference in MusicSum. After controlling for gender and age, there is no association between MusicSum and CAQ.

# 11 - Neuroticism (IV) and CAQ (DV)
lm.11 <- lm(CAQ ~ Gender+Age+Neuroticism, data=nedaData)
summary(lm.11)
Anova(lm.11)

# 12 - CAQ (IV) and Depth (DV)
lm.12 <- lm(Depth ~ Gender+Age+CAQ, data=nedaData)
summary(lm.12)
Anova(lm.12)

# 13 - CAQ (IV) and Mellow (DV)
lm.13 <- lm(Mellow ~ Gender+Age+CAQ, data=nedaData)
summary(lm.13)
Anova(lm.13)

# 14 - Openness (IV) and MusicSum (DV)
lm.14 <- lm(MusicSum ~ Gender+Age+Openness, data=nedaData)
summary(lm.14)
Anova(lm.14)

# 15 - Openness (IV) and Arousal (DV)
lm.15 <- lm(Arousal ~ Gender+Age+Openness, data=nedaData)
summary(lm.15)
Anova(lm.15)

# 16 - NumGalleries (IV) and Contemporary (DV)
lm.16 <- lm(Contemporary ~ Gender+Age+NumGalleries, data=nedaData)
summary(lm.16)
Anova(lm.16)

# 17 - NumClusters (IV) and Contemporary (DV)
lm.17 <- lm(Contemporary ~ Gender+Age+NumClusters, data=nedaData)
summary(lm.17)
Anova(lm.17)

# 18 - PercentShapesExplore (IV) and Sophisticated (DV)
lm.18 <- lm(Sophisticated ~ Gender+Age+PercentShapesExplore, data=nedaData)
summary(lm.18)
Anova(lm.18)

# 19 - PercentShapesExplore (IV) and Neuroticism (DV)
lm.19 <- lm(Neuroticism ~ Gender+Age+PercentShapesExplore, data=nedaData)
summary(lm.19)
Anova(lm.19)

# 20 - Age (IV) and PercentTimeExplore (DV)
lm.20 <- lm(PercentTimeExplore ~ Gender+Age, data=nedaData)
summary(lm.20)
Anova(lm.20)

# 21 - PercentTimeExplore (IV) and Unpretentious (DV)
lm.21 <- lm(Unpretentious ~ Gender+Age+PercentTimeExplore, data=nedaData)
summary(lm.21)
Anova(lm.21)

# 22 - PercentTimeExplore (IV) and Sophisticated (DV)
lm.22 <- lm(Sophisticated ~ Gender+Age+PercentTimeExplore, data=nedaData)
summary(lm.22)
Anova(lm.22)

# 23 - PercentTimeExplore (IV) and Arousal (DV)
lm.23 <- lm(Arousal ~ Gender+Age+PercentTimeExplore, data=nedaData)
summary(lm.23)
Anova(lm.23)

# 24 - ExploitOptimality (IV) and Valence (DV)
lm.24 <- lm(Valence ~ Gender+Age+ExploitOptimality, data=nedaData)
summary(lm.24)
Anova(lm.24)

# 25 - ExploreOptimality (IV) and Contemporary (DV)
lm.25 <- lm(Contemporary ~ Gender+Age+ExploreOptimality, data=nedaData)
summary(lm.25)
Anova(lm.25)

# 26 - ExploreOptimality (IV) and Valence (DV)
lm.26 <- lm(Valence ~ Gender+Age+ExploreOptimality, data=nedaData)
summary(lm.26)
Anova(lm.26)

# 27 - Openness (IV) and OptimalityRatio (DV)
lm.27 <- lm(Openness ~ Gender+Age+OptimalityRatio, data=nedaData)
summary(lm.27)
Anova(lm.27)

# 28 - Age (IV) and MedianNumStepsExploit (DV)
lm.28 <- lm(MedianNumStepsExploit ~ Gender+Age, data=nedaData)
summary(lm.28)
Anova(lm.28)

# 29 - Extraversion (IV) and MedianNumStepsExploit (DV)
lm.29 <- lm(MedianNumStepsExploit ~ Gender+Age+Extraversion, data=nedaData)
summary(lm.29)
Anova(lm.29)

# 30 - Age (IV) and MedianNumStepsExplore (DV)
lm.30 <- lm(MedianNumStepsExplore ~ Gender+Age, data=nedaData)
summary(lm.30)
Anova(lm.30)

# 31 - MedianNumStepsExplore (IV) and MusicSum (DV)
lm.31 <- lm(MusicSum ~ Gender+Age+MedianNumStepsExplore, data=nedaData)
summary(lm.31)
Anova(lm.31)

# 32 - Originality (IV) and Contemporary (DV)
lm.32 <- lm(Contemporary ~ Gender+Age+Originality, data=nedaData)
summary(lm.32)
Anova(lm.32)

# 33 - PercentClustersGC (IV) and MusicSum (DV)
lm.33 <- lm(MusicSum ~ Gender+Age+PercentClustersGC, data=nedaData)
summary(lm.33)
Anova(lm.33)



# make the Creativity Regression Coefficient tables
stargazer(lm.10, type="html", out="/Users/nedarad/Desktop/OpennessxCAQ.html")
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



##################--------------------
# AIM THREE 3 YAHAYHAYHAYAH
# ALMOST DONE

# Goal: do measures of creativity associated with musical preference STILL correlate after we control for openness (and other personality traits of Big 5)?

# 1 - CAQ (IV) + Mellow (DV) -> according to the correlation matrix, CAQ and mellow are correlated.
with(nedaData, cor.test(CAQ, Mellow))

summary(lm(Mellow ~ CAQ, data=nedaData)) # this shows how much Mellow is predicted by CAQ

summary(lm(Mellow ~ Age+Gender+Openness, data=nedaData))
summary(lm(Mellow ~ Age+Gender+Openness+CAQ, data=nedaData)) # This shows how much Mellow is predicted by CAQ after controlling for Openness (and also how much Mellow is predicted by Openness after controlling for CAQ)
# After controlling for Age, Gender, and Openness, Mellow was still significantly predicted by CAQ (t(268)=2.39, p=0.018).
# significant

# 2 - CAQ (IV) + Depth (DV)
summary(lm(Depth ~ Age+Gender+Openness, data=nedaData))
summary(lm(Depth ~ Age+Gender+Openness+CAQ, data=nedaData)) # significant

# 3 - NumGalleries (IV) + Contemporary (DV)
summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness+NumGalleries, data=nedaData)) #significant

# 4 - NumClusters (IV) + Contemporary (DV) 
summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness+NumClusters, data=nedaData)) #significant

# 5 - PercentShapesExplore (IV) + Sophisticated (DV)
summary(lm(Sophisticated ~ Age+Gender+Openness, data=nedaData))
summary(lm(Sophisticated ~ Age+Gender+Openness+PercentShapesExplore, data=nedaData)) #significant

# 6 - PrecentTimeExplore (IV) + Unpretentious (DV)
summary(lm(Unpretentious ~ Age+Gender+Openness, data=nedaData))
summary(lm(Unpretentious ~ Age+Gender+Openness+PercentTimeExplore, data=nedaData)) #significant

# 7 - PercentTimeExplore (IV) + Sophisticated (DV)
summary(lm(Sophisticated ~ Age+Gender+Openness, data=nedaData))
summary(lm(Sophisticated ~ Age+Gender+Openness+PercentTimeExplore, data=nedaData)) #significant

# 8 - PercentTimeExplore (IV) + Arousal (DV)
summary(lm(Arousal ~ Age+Gender+Openness, data=nedaData))
summary(lm(Arousal ~ Age+Gender+Openness+PercentTimeExplore, data=nedaData)) #significant

# 9 - ExploitOptimality (IV) + Valence (DV)
summary(lm(Valence ~ Age+Gender+Openness, data=nedaData))
summary(lm(Valence ~ Age+Gender+Openness+ExploitOptimality, data=nedaData)) #significant

# 10 - ExploreOptimality (IV) + Contemporary (DV)
summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness+ExploreOptimality, data=nedaData)) #significant

# 11 - ExploreOptimality (IV) + Valence (DV)
summary(lm(Valence ~ Age+Gender+Openness, data=nedaData))
summary(lm(Valence ~ Age+Gender+Openness+ExploreOptimality, data=nedaData)) #significant

# 12 - MedianNumStepsExplore (IV) + MusicSum (DV)
summary(lm(MusicSum ~ Age+Gender+Openness, data=nedaData))
summary(lm(MusicSum ~ Age+Gender+Openness+MedianNumStepsExplore, data=nedaData)) #not significant

# 13 - Originality (IV) + Contemporary (DV)
summary(lm(Contemporary ~ Age+Gender+Openness, data=nedaData))
summary(lm(Contemporary ~ Age+Gender+Openness+Originality, data=nedaData)) #significant

# 14 - PercentClustersGC (IV) + MusicSum (DV)
summary(lm(MusicSum ~ Age+Gender+Openness, data=nedaData))
summary(lm(MusicSum ~ Age+Gender+Openness+PercentClustersGC, data=nedaData)) # not significant



# compare other personality factors other than Openness ?
# where do I get the percentage of variance that Openness and Creativity each contribute ?
# example : Openness explains 10% of Mellow and one of the Creativity measures explains 40%
# or do they explain the same portion? or separate?
