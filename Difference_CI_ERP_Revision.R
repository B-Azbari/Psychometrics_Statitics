#Task:
#We are interested in the Confidence interval of the difference value

#Difference value = The difference between the compared conditions (Substraction) for each factor

# Substraction = e.g. Emotion (Neutral - Angry) per participant

setwd("~/Desktop/ICON_DATA_FINAL")
getwd()

library(ez)
library(dplyr)

#Start
Anova_170_Start_hemisphere = read.table("Anova_170_Start_hemisphere.txt",header=TRUE)
Anova_EPN_Start_200.250    = read.table("Anova_EPN_Start_200-250.txt",   header=TRUE)
Anova_EPN_Start_250.300    = read.table("Anova_EPN_Start_250-300.txt",   header=TRUE)
Anova_EPN_Start_300.350    = read.table("Anova_EPN_Start_300-350.txt",   header=TRUE)
Anova_EPN_Start_350.400    = read.table("Anova_EPN_Start_350-400.txt",   header=TRUE)
Anova_EPN_Change_200.400   = read.table("Anova_EPN_Start_200-400.txt",   header=TRUE)

#Change
Anova_170_Change_hemisphere = read.table("Anova_170_Change_hemisphere.txt",header=TRUE)
Anova_EPN_Change_200.400    = read.table("Anova_EPN_Change_200-400.txt",   header=TRUE)

#Start
Anova_170_Start_hemisphere$Subject    <- factor(Anova_170_Start_hemisphere$Subject,    levels = c( 1:20 )) 
Anova_170_Start_hemisphere$Gaze       <- factor(Anova_170_Start_hemisphere$Gaze,       levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Emotion    <- factor(Anova_170_Start_hemisphere$Emotion,    levels = c( 1:3 )) 
Anova_170_Start_hemisphere$Hemisphere <- factor(Anova_170_Start_hemisphere$Hemisphere, levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Electrode  <- factor(Anova_170_Start_hemisphere$Electrode,  levels = c( 35,39,31,37 )) 

# What i used to do before 
mean_1 = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==1])
mean_1
mean_2 = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==2])
mean_2
mean_3 = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==3])
mean_3

SD_1 = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==1])
SD_1
SD_2 = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==2])
SD_2
SD_3 = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==3])
SD_3

#Filtering for difference value per participant
#Gaze
filtered <- Anova_170_Start_hemisphere %>% group_by(Gaze, Subject) %>% summarise(means=mean(Amplitude))
Gaze1 <- filtered %>% filter(Gaze==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Gaze2 <- filtered %>% filter(Gaze==2) %>% group_by(Subject) %>% summarise(means=mean(means))
AllGazes <- cbind.data.frame(Subject=Gaze1$Subject, Gaze1=Gaze1$means, Gaze2=Gaze2$means)
# E12=mean(Gaze1) - mean(Gaze2)
G <- AllGazes %>% group_by(Subject) %>% mutate(G12=Gaze1-Gaze2)
G

# T tests
Direct    <- Anova_170_Start_hemisphere[Anova_170_Start_hemisphere$Gaze==1,]$Amplitude
Averted   <- Anova_170_Start_hemisphere[Anova_170_Start_hemisphere$Gaze==2,]$Amplitude
t.test(Direct,Averted , paired = T)

Direct    <- filtered[filtered$Gaze==1,]$means
Averted   <- filtered[filtered$Gaze==2,]$means
t.test(Direct,Averted , paired = T)

# means
apply(G[,4], 2, mean)
# sds
apply(G[,4], 2, sd)
# CI
t.test(G$G12)

#Emotion
filtered <- Anova_170_Start_hemisphere %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21)
t.test(E$E31)
t.test(E$E32)

#Filter based on the hemisphere
Anova_170_Start_hemisphere_Left  = Anova_170_Start_hemisphere [Anova_170_Start_hemisphere$Hemisphere==1,] #Filtering based on the Hemisphere Left 
filtered <- Anova_170_Start_hemisphere_Left %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)


Anova_170_Start_hemisphere_Right = Anova_170_Start_hemisphere [Anova_170_Start_hemisphere$Hemisphere==2,] #Filtering based on the Hemisphere Right
filtered <- Anova_170_Start_hemisphere_Right %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)


#EPN
### 200-250 ### 
Anova_EPN_Start_200.250$Subject   <- factor(Anova_EPN_Start_200.250$Subject,  levels = c( 1:20 )) 
Anova_EPN_Start_200.250$Gaze      <- factor(Anova_EPN_Start_200.250$Gaze,     levels = c( 1:2 )) 
Anova_EPN_Start_200.250$Emotion   <- factor(Anova_EPN_Start_200.250$Emotion,  levels = c( 1:3 )) 
Anova_EPN_Start_200.250$Electrode <- factor(Anova_EPN_Start_200.250$Electrode,levels = c( 31,36,37,41,42,43,44,39,35,40 )) 

Neutral  <- Anova_EPN_Start_200.250[Anova_EPN_Start_200.250$Emotion==1,]$Amplitude
Angry    <- Anova_EPN_Start_200.250[Anova_EPN_Start_200.250$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Neutral  <- Anova_EPN_Start_200.250[Anova_EPN_Start_200.250$Emotion==1,]$Amplitude
Happy    <- Anova_EPN_Start_200.250[Anova_EPN_Start_200.250$Emotion==3,]$Amplitude
t.test(Neutral,Happy , paired = T)

Angry    <- Anova_EPN_Start_200.250[Anova_EPN_Start_200.250$Emotion==2,]$Amplitude
Happy    <- Anova_EPN_Start_200.250[Anova_EPN_Start_200.250$Emotion==3,]$Amplitude
t.test(Angry,Happy , paired = T)

#Solitary t.tests for CI
t.test(Neutral)
t.test(Angry)
t.test(Happy)

#Emotion
filtered <- Anova_EPN_Start_200.250 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

# To reach the degress of freedom of 19
Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21)
t.test(E$E31)
t.test(E$E32)

### 250-300 ###
Anova_EPN_Start_250.300$Subject   <- factor(Anova_EPN_Start_250.300$Subject,  levels = c( 1:20 )) 
Anova_EPN_Start_250.300$Gaze      <- factor(Anova_EPN_Start_250.300$Gaze,     levels = c( 1:2 )) 
Anova_EPN_Start_250.300$Emotion   <- factor(Anova_EPN_Start_250.300$Emotion,  levels = c( 1:3 )) 
Anova_EPN_Start_250.300$Electrode <- factor(Anova_EPN_Start_250.300$Electrode,levels = c( 31,36,37,41,42,43,44,39,40,35 ))

### Attention ####
#You can not run this part and then run the Filtering part #####
Neutral  <- Anova_EPN_Start_250.300[Anova_EPN_Start_250.300$Emotion==1,]$Amplitude
Angry    <- Anova_EPN_Start_250.300[Anova_EPN_Start_250.300$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Neutral  <- Anova_EPN_Start_250.300[Anova_EPN_Start_250.300$Emotion==1,]$Amplitude
Happy    <- Anova_EPN_Start_250.300[Anova_EPN_Start_250.300$Emotion==3,]$Amplitude
t.test(Neutral,Happy , paired = T)

Angry    <- Anova_EPN_Start_250.300[Anova_EPN_Start_250.300$Emotion==2,]$Amplitude
Happy    <- Anova_EPN_Start_250.300[Anova_EPN_Start_250.300$Emotion==3,]$Amplitude
t.test(Angry,Happy , paired = T)

#Solitary t.tests for CI
t.test(Neutral)
t.test(Angry)
t.test(Happy)

#Emotion
filtered <- Anova_EPN_Start_250.300 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21) # pictype 1 & subject type 1
t.test(E$E31) 
t.test(E$E32)

### 300-350 ###
Anova_EPN_Start_300.350$Subject   <- factor(Anova_EPN_Start_300.350$Subject,  levels = c( 1:20 )) 
Anova_EPN_Start_300.350$Gaze      <- factor(Anova_EPN_Start_300.350$Gaze,     levels = c( 1:2 )) 
Anova_EPN_Start_300.350$Emotion   <- factor(Anova_EPN_Start_300.350$Emotion,  levels = c( 1:3 )) 
Anova_EPN_Start_300.350$Electrode <- factor(Anova_EPN_Start_300.350$Electrode,levels = c( 31,36,37,41,42,43,44,39,40,35 ))

Neutral  <- Anova_EPN_Start_300.350[Anova_EPN_Start_300.350$Emotion==1,]$Amplitude
Angry    <- Anova_EPN_Start_300.350[Anova_EPN_Start_300.350$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Neutral  <- Anova_EPN_Start_300.350[Anova_EPN_Start_300.350$Emotion==1,]$Amplitude
Happy    <- Anova_EPN_Start_300.350[Anova_EPN_Start_300.350$Emotion==3,]$Amplitude
t.test(Neutral,Happy , paired = T)

Angry    <-  Anova_EPN_Start_300.350[Anova_EPN_Start_300.350$Emotion==2,]$Amplitude
Happy    <-  Anova_EPN_Start_300.350[Anova_EPN_Start_300.350$Emotion==3,]$Amplitude
t.test(Angry,Happy , paired = T)

#Solitary t.tests for CI
t.test(Neutral)
t.test(Angry)
t.test(Happy)

#Emotion
filtered <- Anova_EPN_Start_300.350 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21)
t.test(E$E31)
t.test(E$E32)

### 350-400 ###
Anova_EPN_Start_350.400$Subject   <- factor(Anova_EPN_Start_350.400$Subject,  levels = c( 1:20 )) 
Anova_EPN_Start_350.400$Gaze      <- factor(Anova_EPN_Start_350.400$Gaze,     levels = c( 1:2 )) 
Anova_EPN_Start_350.400$Emotion   <- factor(Anova_EPN_Start_350.400$Emotion,  levels = c( 1:3 )) 
Anova_EPN_Start_350.400$Electrode <- factor(Anova_EPN_Start_350.400$Electrode,levels = c( 31,36,37,41,42,43,44,39,40,35 ))

Neutral  <- Anova_EPN_Start_350.400[Anova_EPN_Start_350.400$Emotion==1,]$Amplitude
Angry    <- Anova_EPN_Start_350.400[Anova_EPN_Start_350.400$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Neutral  <- Anova_EPN_Start_350.400[Anova_EPN_Start_350.400$Emotion==1,]$Amplitude
Happy    <- Anova_EPN_Start_350.400[Anova_EPN_Start_350.400$Emotion==3,]$Amplitude
t.test(Neutral,Happy , paired = T)

Angry    <- Anova_EPN_Start_350.400[Anova_EPN_Start_350.400$Emotion==2,]$Amplitude
Happy    <- Anova_EPN_Start_350.400[Anova_EPN_Start_350.400$Emotion==3,]$Amplitude
t.test(Angry,Happy , paired = T)

#Solitary t.tests for CI
t.test(Neutral)
t.test(Angry)
t.test(Happy)

#Emotion
filtered <- Anova_EPN_Start_350.400 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21)
t.test(E$E31)
t.test(E$E32)

###########################################################################################################
####################################################
#Change
Anova_170_Change_hemisphere$Subject    <- factor(Anova_170_Change_hemisphere$Subject,   levels = c( 1:20 )) 
Anova_170_Change_hemisphere$Gaze       <- factor(Anova_170_Change_hemisphere$Gaze,      levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Emotion    <- factor(Anova_170_Change_hemisphere$Emotion,   levels = c( 1:3 ))
Anova_170_Change_hemisphere$Electrode  <- factor(Anova_170_Change_hemisphere$Electrode, levels = c( 35,39,31,37 )) 
Anova_170_Change_hemisphere$Hemisphere <- factor(Anova_170_Change_hemisphere$Hemisphere,levels = c( 1:2 )) 

#Filtering for difference value per participant
#Gaze
filtered <- Anova_170_Change_hemisphere %>% group_by(Gaze, Subject) %>% summarise(means=mean(Amplitude))
Gaze1 <- filtered %>% filter(Gaze==1) %>% group_by(Subject) %>% summarise(means=mean(means)) #C_A_F 
Gaze2 <- filtered %>% filter(Gaze==2) %>% group_by(Subject) %>% summarise(means=mean(means)) #C_F_A
AllGazes <- cbind.data.frame(Subject=Gaze1$Subject, Gaze1=Gaze1$means, Gaze2=Gaze2$means)
# E12=mean(Gaze1) - mean(Gaze2)
G <- AllGazes %>% group_by(Subject) %>% mutate(G12=Gaze1-Gaze2)
G

Direct    <- filtered[filtered$Gaze==1,]$means
Averted   <- filtered[filtered$Gaze==2,]$means
t.test(Direct,Averted , paired = T)

#Hemisphere
filtered <- Anova_170_Change_hemisphere %>% group_by(Hemisphere, Subject) %>% summarise(means=mean(Amplitude))

Left    <- filtered[filtered$Hemisphere==1,]$means
Right   <- filtered[filtered$Hemisphere==2,]$means
t.test(Left,Right , paired = T)
# means
apply(G[,4], 2, mean)
# sds
apply(G[,4], 2, sd)
# CI
t.test(G$G12)

#Hemisphere
filtered <- Anova_170_Change_hemisphere %>% group_by(Hemisphere, Subject) %>% summarise(means=mean(Amplitude))
Hemisphere1 <- filtered %>% filter(Hemisphere==1) %>% group_by(Subject) %>% summarise(means=mean(means)) #Left
Hemisphere2 <- filtered %>% filter(Hemisphere==2) %>% group_by(Subject) %>% summarise(means=mean(means)) #Right
AllHemispheres <- cbind.data.frame(Subject=Hemisphere1$Subject, Hemisphere1=Hemisphere1$means, Hemisphere2=Hemisphere2$means)
# E12=mean(Gaze1) - mean(Gaze2)
H <- AllHemispheres %>% group_by(Subject) %>% mutate(H12=Hemisphere1-Hemisphere2)
H

# means
apply(H[,4], 2, mean)
# sds
apply(H[,4], 2, sd)
# CI
t.test(H$H12)

#Emotion
filtered <- Anova_170_Change_hemisphere %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E12=Emotion1-Emotion2, E13=Emotion1-Emotion3, E23=Emotion2-Emotion3)
E

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E12)
t.test(E$E13)
t.test(E$E23)

#Pairweise t test
Neutral  <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Emotion==1,]$Amplitude
Angry    <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Neutral  <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Emotion==1,]$Amplitude
Happy    <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Emotion==3,]$Amplitude
t.test(Neutral,Happy , paired = T)

Angry    <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Emotion==2,]$Amplitude
Happy    <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Emotion==3,]$Amplitude
t.test(Angry,Happy , paired = T)

Left   <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Hemisphere==1,]$Amplitude #Left
Right  <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Hemisphere==2,]$Amplitude #Right
t.test(Left,Right , paired = T)

Avertedtodirect  <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Gaze==1,]$Amplitude #C_A_F 
DirecttoAverted  <- Anova_170_Change_hemisphere[Anova_170_Change_hemisphere$Gaze==2,]$Amplitude #C_F_A
t.test(Avertedtodirect,DirecttoAverted , paired = T)

#Solitary t.tests for CI
t.test(Neutral)
t.test(Angry)
t.test(Happy)
t.test(Left)
t.test(Right)
t.test(Avertedtodirect)
t.test(DirecttoAverted)

#EPN 200-400
Anova_EPN_Change_200.400$Subject   <- factor(Anova_EPN_Change_200.400$Subject,  levels = c( 1:20 )) 
Anova_EPN_Change_200.400$Gaze      <- factor(Anova_EPN_Change_200.400$Gaze,     levels = c( 1:2 )) 
Anova_EPN_Change_200.400$Emotion   <- factor(Anova_EPN_Change_200.400$Emotion,  levels = c( 1:3 )) 
Anova_EPN_Change_200.400$Electrode <- factor(Anova_EPN_Change_200.400$Electrode,levels = c( 31,36,37,41,42,43,44,39,40,35 ))

Neutral  <- Anova_EPN_Change_200.400[Anova_EPN_Change_200.400$Emotion==1,]$Amplitude
Angry    <- Anova_EPN_Change_200.400[Anova_EPN_Change_200.400$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Neutral  <- Anova_EPN_Change_200.400[Anova_EPN_Change_200.400$Emotion==1,]$Amplitude
Happy    <- Anova_EPN_Change_200.400[Anova_EPN_Change_200.400$Emotion==3,]$Amplitude
t.test(Neutral,Happy , paired = T)

Angry    <- Anova_EPN_Change_200.400[Anova_EPN_Change_200.400$Emotion==2,]$Amplitude
Happy    <- Anova_EPN_Change_200.400[Anova_EPN_Change_200.400$Emotion==3,]$Amplitude
t.test(Angry,Happy , paired = T)

t.test(Neutral)
t.test(Angry)
t.test(Happy)
t.test(Direct)
t.test(Averted)

# One gaze rotation
Anova_EPN_Change_200.400_Av_D = Anova_EPN_Change_200.400 [Anova_EPN_Change_200.400$Gaze==1,] #Filtering based on the Gaze type

mean_1 = mean(Anova_EPN_Change_200.400_Av_D$Amplitude[Anova_EPN_Change_200.400_Av_D$Emotion==1])
mean_1
mean_2 = mean(Anova_EPN_Change_200.400_Av_D$Amplitude[Anova_EPN_Change_200.400_Av_D$Emotion==2])
mean_2
mean_3 = mean(Anova_EPN_Change_200.400_Av_D$Amplitude[Anova_EPN_Change_200.400_Av_D$Emotion==3])
mean_3

SD_1 = sd(Anova_EPN_Change_200.400_Av_D$Amplitude[Anova_EPN_Change_200.400_Av_D$Emotion==1])
SD_1
SD_2 = sd(Anova_EPN_Change_200.400_Av_D$Amplitude[Anova_EPN_Change_200.400_Av_D$Emotion==2])
SD_2
SD_3 = sd(Anova_EPN_Change_200.400_Av_D$Amplitude[Anova_EPN_Change_200.400_Av_D$Emotion==3])
SD_3

Neutral_N  <- Anova_EPN_Change_200.400_Av_D[Anova_EPN_Change_200.400_Av_D$Emotion==1,]$Amplitude
Happy_H    <- Anova_EPN_Change_200.400_Av_D[Anova_EPN_Change_200.400_Av_D$Emotion==3,]$Amplitude
t.test(Neutral_N,Happy_H , paired = T)

Neutral  <- Anova_EPN_Change_200.400_Av_D[Anova_EPN_Change_200.400_Av_D$Emotion==1,]$Amplitude
Angry    <- Anova_EPN_Change_200.400_Av_D[Anova_EPN_Change_200.400_Av_D$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

Happy   <- Anova_EPN_Change_200.400_Av_D[Anova_EPN_Change_200.400_Av_D$Emotion==3,]$Amplitude
Angry   <- Anova_EPN_Change_200.400_Av_D[Anova_EPN_Change_200.400_Av_D$Emotion==2,]$Amplitude
t.test(Neutral,Angry , paired = T)

t.test(Neutral_N)
t.test(Happy_H)

filtered <- Anova_EPN_Change_200.400_Av_D %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filtered[filtered$Emotion==1,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry    <- filtered[filtered$Emotion==2,]$means
Happy    <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21)
t.test(E$E31)
t.test(E$E32)

