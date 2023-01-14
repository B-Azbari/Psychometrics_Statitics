########################
### ezANOVA - START ###
########################
# Parenthood Study Statistical Analysis

# here: without plots. Use the following format for plots:
# ezPlot(Anova_170_Start, 
# dv = Amplitude, 
# wid = Subject, 
# within = Hemisphere,
# x = Hemisphere, 
# x_lab="N170 Start Hemisphere")

# set work directory
# setwd("~/Desktop/PA_DATA/") Previous Archeive
setwd("~/Desktop/PA_DATA/EPN/NEW/") #all the corrected data are here! 2021
getwd()

library(ez)
library(apaTables)
library(car)
library(ggplot2)
library(dplyr)
#library(stargazer)
#library(apa)

# import data

Anova_170_Start_region     = read.table("Anova_pa_170_Start_region.txt",    header=TRUE)
Anova_170_Start_peak       = read.table("Anova_pa_170_Start_peak.txt",      header=TRUE)
Anova_170_Start_hemisphere = read.table("Anova_pa_170_Start_hemisphere.txt",header=TRUE)
Anova_EPN_Start_200.250    = read.table("Anova_pa_EPN_Start_200-250.txt",   header=TRUE)
Anova_EPN_Start_250.300    = read.table("Anova_pa_EPN_Start_250-300.txt",   header=TRUE)
Anova_EPN_Start_300.350    = read.table("Anova_pa_EPN_Start_300-350.txt",   header=TRUE)
Anova_EPN_Start_350.400    = read.table("Anova_pa_EPN_Start_350-400.txt",   header=TRUE)
Anova_LPC_Start_300.400    = read.table("Anova_pa_LPC_Start_300-400.txt",   header=TRUE)
Anova_LPC_Start_300.400    = read.table("Anova_pa_LPC_Start_300-400.txt",   header=TRUE)
Anova_LPC_Start_400.500    = read.table("Anova_pa_LPC_Start_400-500.txt",   header=TRUE)

Anova_EPN_Start_200.400_interval_Angry    = read.table("AMP_Pa_START_Anova_EPN_Start_200-400_Interval_Angry.txt",   header=TRUE)
Anova_EPN_Start_200.400_interval_Happy    = read.table("AMP_Pa_START_Anova_EPN_Start_200-400_Interval.txt",   header=TRUE)
Anova_EPN_Start_200.400_interval_Neutral  = read.table("AMP_Pa_START_Anova_EPN_Start_200-400_Interval.txt",   header=TRUE)

options(max.print=1000000)

################## Normalization Process ##################
# Data Primary Check
length(Anova_170_Start_hemisphere$Amplitude) # 5472
length(Anova_EPN_Start_200.250$Amplitude) #13680
length(Anova_EPN_Start_250.300$Amplitude) #13680
length(Anova_EPN_Start_300.350$Amplitude) #13680
length(Anova_EPN_Start_350.400$Amplitude) #13680
# Shapiro Test for normality 
# We can try to do the shapiro test using only the first 5.000 samples
shapiro.test(Anova_170_Start_hemisphere$Amplitude[0:5000]) 
qqnorm(Anova_170_Start_hemisphere$Amplitude);qqline(Anova_170_Start_hemisphere$Amplitude)
Anova_170_Start_hemisphere$Amplitude <- as.numeric(scale(Anova_170_Start_hemisphere$Amplitude)) 
#(Anova_170_Start_hemisphere$Amplitude - mean(Anova_170_Start_hemisphere$Amplitude))/sd(Anova_170_Start_hemisphere$Amplitude) # first of the scale output

# let's check
Anova_170_Start_hemisphere$Amplitude <- scale(Anova_170_Start_hemisphere$Amplitude) 

Anova_170_Start_hemisphere=data.frame(Anova_170_Start_hemisphere) #Change accordingly 
head(Anova_170_Start_hemisphere)
hist(Anova_170_Start_hemisphere$Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_170_Start_hemisphere$Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'MV', ylab='Frequency')
boxplot(Anova_170_Start_hemisphere$Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

# Levene's test for equality of variances
Anova_170_Start_hemisphere$Subject_type <-  as.factor(Anova_170_Start_hemisphere$Subject_type)
Anova_170_Start_hemisphere$Amplitude    <- as.numeric(Anova_170_Start_hemisphere$Amplitude)
result = leveneTest(weight ~ group, Anova_170_Start_hemisphere)
print (result)

################## N170-Start_region ################

Anova_170_Start_region$Subject      <- factor(Anova_170_Start_region$Subject,     levels = c( 1:114)) 
Anova_170_Start_region$Subject_type <- factor(Anova_170_Start_region$Subject_type,levels = c( 1:2)) 
Anova_170_Start_region$Gaze         <- factor(Anova_170_Start_region$Gaze,        levels = c( 1:2 )) 
Anova_170_Start_region$Emotion      <- factor(Anova_170_Start_region$Emotion,     levels = c( 1:3 )) 
Anova_170_Start_region$Pic_type     <- factor(Anova_170_Start_region$Pic_type,    levels = c( 1:2 )) 
Anova_170_Start_region$Electrode    <- factor(Anova_170_Start_region$Electrode,   levels = c( 30,31,35,34 )) 

Anova_170s_1 <- ezANOVA (Anova_170_Start_region, 
                       dv = Amplitude, 
                       wid = Subject, 
                       within  = .(Pic_type, Emotion, Gaze, Electrode), 
                       detailed = TRUE)
Anova_170s_1

Anova_170s_2 <- ezANOVA (Anova_170_Start_region, 
                       dv = Amplitude, 
                       wid = Subject, 
                       between = Subject_type, 
                       within  = .(Pic_type, Emotion, Gaze, Electrode), 
                       detailed = TRUE)
Anova_170s_2

goggles_table <- apa.ezANOVA.table(Anova_170s,
                                   filename="Anova_pa_170_Start_region.doc")

Anova_170_start=data.frame(Anova_170_Start_region) 
head(Anova_170_start)
hist(Anova_170_start$Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude Score', ylab='Frequency')
plot(Anova_170_start$Amplitude, main    = 'Scatterplot of  Amplitude', xlab = 'Amplitude Score', ylab='Frequency')
boxplot(Anova_170_start$Amplitude, main = 'Boxplot of  Amplitude', xlab = 'Amplitude Score', ylab='Frequency')

# Gaze
mean_D= mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Gaze==1])
mean_D
mean_a = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Emotion==1])
mean_Nu
mean_An = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Emotion==2])
mean_An
mean_Ha = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Emotion==3])
mean_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Subject_type==1])
mean_Mo
SD_Nm = sd(Anova_170_Start_region$Amplitude[Anova_170_Start_region$Subject_type==2])
mean_Nm

## Plots
ezPlot(Anova_170_Start_region, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="N170 Start Emotion")

ezPlot(Anova_170_Start_region, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="N170 Start Gaze")

ezPlot(Anova_170_Start_region, 
       dv = Amplitude, 
       wid = Subject, 
       within = Pic_type, 
       x = Pic_type, 
       x_lab="N170 Start Pic_type")

ezPlot(Anova_170_Start_region, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Pic_type,Emotion),
       x = Emotion, 
       split= Emotion,
       x_lab="N170 Start Pic_type x Emotion")

ezPlot(Anova_170_Start_region,  
       dv = Amplitude, 
       wid = Subject,
       within = .(Subject_type,Emotion),
       x = Emotion, 
       split= Emotion,
       x_lab="N170 Start Subj_type x Emotion")

# post-hoc t-test
pairwise.t.test(Anova_170_Start$Amplitude,Anova_170_Start$Emotion, p.adj="bonferroni", paired=T)

################## N170-Start_hemisphere ################
Anova_170_Start_hemisphere$Subject      <- factor(Anova_170_Start_hemisphere$Subject,     levels = c( 1:114)) 
Anova_170_Start_hemisphere$Subject_type <- factor(Anova_170_Start_hemisphere$Subject_type,levels = c( 1:2)) 
Anova_170_Start_hemisphere$Gaze         <- factor(Anova_170_Start_hemisphere$Gaze,        levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Emotion      <- factor(Anova_170_Start_hemisphere$Emotion,     levels = c( 1:3 )) 
Anova_170_Start_hemisphere$Pic_type     <- factor(Anova_170_Start_hemisphere$Pic_type,    levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Hemisphere   <- factor(Anova_170_Start_hemisphere$Hemisphere,  levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Electrode    <- factor(Anova_170_Start_hemisphere$Electrode,   levels = c( 30,26,33,34 )) 

Anova_170s1 <- ezANOVA (Anova_170_Start_hemisphere, 
                        dv = Amplitude, 
                        wid = Subject, 
                        between = Subject_type,
                        within  = .(Pic_type, Emotion, Gaze, Hemisphere), 
                        detailed = TRUE)
Anova_170s1

table(Anova_170_Start_hemisphere$Subject_type)
goggles_table <- apa.ezANOVA.table(Anova_170s,
                                   filename="Anova_170_Start_hemisphere.doc")

# To print in Docx format (APA)
goggles_table <- apa.ezANOVA.table(Anova_170s1,
                                   filename="Anova_170_Start_hemisphere.doc")

#Stmuli X Group interaction
Anova_170_Start_hemisphere$Gaze         <- factor(Anova_170_Start_hemisphere$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_170_Start_hemisphere$Emotion      <- factor(Anova_170_Start_hemisphere$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_170_Start_hemisphere$Subject_type <- factor(Anova_170_Start_hemisphere$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
Anova_170_Start_hemisphere$Pic_type     <- factor(Anova_170_Start_hemisphere$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

#Tukey Results for Emotion Main Effect
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_170_Start_hemisphere$Amplitude~Anova_170_Start_hemisphere$Emotion)), data=Anova_170_Start_hemisphere, conf.level=0.95)
print(TUKEYM1)
#Tukey Results for Emotion x Stimulus interaction
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_170_Start_hemisphere$Amplitude~Anova_170_Start_hemisphere$Pic_type:Anova_170_Start_hemisphere$Emotion)), data=Anova_170_Start_hemisphere, conf.level=0.95)
print(TUKEYM1)

# Gaze
mean_D= mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Gaze==1])
mean_D
mean_a = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Gaze==2])
mean_a

# Hemisphere
mean_L = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Hemisphere==1])
mean_L
mean_R = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Hemisphere==2])
mean_R

SD_L = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Hemisphere==1])
SD_L
SD_R = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Hemisphere==2])
SD_R

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==1])
mean_Nu
mean_An = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==2])
mean_An
mean_Ha = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==1])
SD_Nu
SD_An = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==2])
SD_An
SD_Ha = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Pic_type==2])
mean_Ba

SD_Ad = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Pic_type==1])
SD_Ad
SD_Ba = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Pic_type==2])
SD_Ba

# SubjType
mean_Mo= mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Subject_type==2])
mean_Nm

mean_Mo= sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Subject_type==1])
mean_Mo
mean_Nm = sd(Anova_170_Start_hemisphere$Amplitude[Anova_170_Start_hemisphere$Subject_type==2])
mean_Nm

Anova_170_Start_hemisphere$Subject_type <-  as.factor(Anova_170_Start_hemisphere$Subject_type)
Anova_170_Start_hemisphere$Amplitude    <- as.numeric(Anova_170_Start_hemisphere$Amplitude)
Anova_170_Start_hemisphere$Pic_type     <-  as.factor(Anova_170_Start_hemisphere$Pic_type)
Anova_170_Start_hemisphere$Emotion      <-  as.factor(Anova_170_Start_hemisphere$Emotion)
Anova_170_Start_hemisphere$Gaze         <-  as.factor(Anova_170_Start_hemisphere$Gaze)
Anova_170_Start_hemisphere$Hemisphere   <-  as.factor(Anova_170_Start_hemisphere$Hemisphere)

# # # #  To double check for Tukey Results (Mothers)
Anova_170_Start_hemisphere_Mo  = Anova_170_Start_hemisphere [Anova_170_Start_hemisphere$Subject_type==1,] #Filtering based on the Subject type

#Emotion
filtered <- Anova_170_Start_hemisphere_Mo %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
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

Angry  <- filtered[filtered$Emotion==2,]$means
Happy  <- filtered[filtered$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(E[,5:7], 2, mean)
# sds
apply(E[,5:7], 2, sd)
# CI
t.test(E$E21)
t.test(E$E31)
t.test(E$E32)

# # # #  To double check for Tukey Results (Non-Mothers)
Anova_170_Start_hemisphere_Nm  = Anova_170_Start_hemisphere [Anova_170_Start_hemisphere$Subject_type==2,] #Filtering based on the Subject type

#Emotion
filteredn <- Anova_170_Start_hemisphere_Nm %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1n <- filteredn %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2n <- filteredn %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3n <- filteredn %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1n$Subject, Emotion1n=Emotion1n$means, Emotion2n=Emotion2n$means, Emotion3n=Emotion3n$means)
# E12=mean(Emotion1) - mean(Emotion2)
En <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2n-Emotion1n, E31=Emotion3n-Emotion1n, E32=Emotion3n-Emotion2n)
En

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Angry    <- filteredn[filteredn$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Happy    <- filteredn[filteredn$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry  <- filteredn[filteredn$Emotion==2,]$means
Happy  <- filteredn[filteredn$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(En[,5:7], 2, mean)
# sds
apply(En[,5:7], 2, sd)
# CI
t.test(En$E21)
t.test(En$E31)
t.test(En$E32)

# # # #  To double check for Tukey Results (Stimulus)
Anova_170_Start_hemisphere_Ba  = Anova_170_Start_hemisphere [Anova_170_Start_hemisphere$Pic_type==2,] #Filtering based on the Subject type

#Emotion
filteredb <- Anova_170_Start_hemisphere_Ba %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1b <- filteredb %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2b <- filteredb %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3b <- filteredb %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1b$Subject, Emotion1b=Emotion1b$means, Emotion2b=Emotion2b$means, Emotion3b=Emotion3b$means)
# E12=mean(Emotion1) - mean(Emotion2)
Eb <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2b-Emotion1b, E31=Emotion3b-Emotion1b, E32=Emotion3b-Emotion2b)
Eb

Neutral  <- filteredb[filteredb$Emotion==1,]$means
Angry    <- filteredb[filteredb$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filteredb[filteredb$Emotion==1,]$means
Happy    <- filteredb[filteredb$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry  <- filteredn[filteredn$Emotion==2,]$means
Happy  <- filteredn[filteredn$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(En[,5:7], 2, mean)
# sds
apply(En[,5:7], 2, sd)
# CI
t.test(En$E21)
t.test(En$E31)
t.test(En$E32)


# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_170_Start_hemisphere, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p
# Edit legend title and labels
p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x = "Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_170_Start_hemisphere, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p6

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Direct", "Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Direct","Averted"))

#Emotion
p1e <- ggplot(Anova_170_Start_hemisphere, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mothers", "Nulliparas"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy"))

#Hemisphere
p1h <- ggplot(Anova_170_Start_hemisphere, aes(x = Hemisphere, y = Amplitude, color = Subject_type)) +
  geom_boxplot()

p2h = p1h + labs(color = "Groups", x = "Hemisphere")
p2h

p3h = p2h + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3h

p3h = p2h + scale_fill_manual(x ="Hemisphere", labels = x("Left", "Right"))
p3h
p3h + scale_x_discrete(name ="Hemisphere", labels=c("Left", "Right"))

# To visualize the Emotion x Stimulus Interaction 

p1e <- ggplot(Anova_170_Start_hemisphere, aes(x = Emotion, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Stimuli", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Adult", "Infant"), values = c("purple", "black"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy")) 

+ylim (-15,30)

#############################################
#Anova_EPN_Start_200.400_interval
Anova_EPN_Start_200.400_interval_Angry$Subject      <- factor(Anova_EPN_Start_200.400_interval_Angry$Subject,     levels = c( 1:114)) 
Anova_EPN_Start_200.400_interval_Angry$Subject_type <- factor(Anova_EPN_Start_200.400_interval_Angry$Subject_type,levels = c( 1:2)) 
Anova_EPN_Start_200.400_interval_Angry$Gaze         <- factor(Anova_EPN_Start_200.400_interval_Angry$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Start_200.400_interval_Angry$Emotion      <- factor(Anova_EPN_Start_200.400_interval_Angry$Emotion,     levels = c( 1:1 )) 
Anova_EPN_Start_200.400_interval_Angry$Pic_type     <- factor(Anova_EPN_Start_200.400_interval_Angry$Pic_type,    levels = c( 1:2 )) 
#Anova_EPN_Start_200.400_interval$Hemisphere   <- factor(Anova_EPN_Start_200.400_interval$Hemisphere,  levels = c( 1:2 )) 
Anova_EPN_Start_200.400_interval_Angry$Electrode    <- factor(Anova_EPN_Start_200.400_interval_Angry$Electrode,   levels = c( 30,26,33,34 )) 
Anova_EPN_Start_200.400_interval_Angry$Interval     <- factor(Anova_EPN_Start_200.400_interval_Angry$Interval,    levels = c( 1:4 )) 

#Anova_EPN_Start_200.400_interval_Angry = Anova_EPN_Start_200.400_interval_Angry [Anova_EPN_Start_200.400_interval_Angry$Pic_type==1,] #Filtering based on the Pic-type Factor ()
#Anova_EPN_Start_200.400_interval_Angry = Anova_EPN_Start_200.400_interval [Anova_EPN_Start_200.400_interval$Emotion==2,] #Filtering based on the Pic-type Factor ()

p1e <- ggplot(Anova_EPN_Start_200.400_interval_Angry, aes(x = Interval, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Stimuli", x = "Interval")
p2e

p3e = p2e + scale_color_manual(labels = c("Adult", "Infant"), values = c("dark orange", "dark green"))
p3e

p3e = p2e + scale_fill_manual(x = "Interval", labels = x("200-250", "250-300", "300-350", "350-400"))
p3e
p3e + scale_x_discrete(name ="Time Interval", labels=c("200-250", "250-300", "300-350", "350-400"))

table(Anova_EPN_Start_200.400_interval_A$Emotion)
table(Anova_EPN_Start_200.400_interval_A$Pic_type)

Anova_EPN_Start_200.400_interval_B = Anova_EPN_Start_200.400_interval [Anova_EPN_Start_200.400_interval$Pic_type==2,] #Filtering based on the Pic-type Factor ()

p1e <- ggplot(Anova_EPN_Start_200.400_interval_B, aes(x = Interval, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 


p2e = p1e + labs(color = "Pictype", x = "Interval")
p2e

p3e = p2e + scale_color_manual(labels = c("Baby"), values = c("blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Interval", labels = x("200-250", "250-300", "300-350", "350-400"))
p3e
p3e + scale_x_discrete(name ="Interval", labels=c("200-250", "250-300", "300-350", "350-400"))


## Plots
ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="N170 Start Emotion")

ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="N170 Start Gaze")

ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject, 
       within = Hemisphere,
       x = Hemisphere, 
       x_lab="N170 Start Hemisphere")

ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="N170 Start Gaze x Emotion")

ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Pic_type),
       x = Emotion, 
       split= Pic_type,
       x_lab="N170 Start  Emotion x Stimuli") + ylim(2,6)

ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Subject_type),
       x = Subject_type, 
       split= Emotion,
       x_lab="N170 Start  Emotion x Subject")

ezPlot(Anova_170_Start_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Hemisphere),
       x = Hemisphere, 
       split=Gaze,
       x_lab="N170 Start Gaze x Hemisphere")

# post-hoc t-test
pairwise.t.test(Anova_170_Start_hemisphere$Amplitude,Anova_170_Start_hemisphere$Emotion, p.adj="bonferroni", paired=T)

################## N170-Start-peak ################

Anova_170_Start_peak$Subject    <- factor(Anova_170_Start_peak$Subject,   levels = c( 1:20 )) 
Anova_170_Start_peak$Gaze       <- factor(Anova_170_Start_peak$Gaze,      levels = c( 1:2 )) 
Anova_170_Start_peak$Emotion    <- factor(Anova_170_Start_peak$Emotion,   levels = c( 1:3 )) 
Anova_170_Start_peak$Hemisphere <- factor(Anova_170_Start_peak$Hemisphere,levels = c( 1:2 )) 
Anova_170_Start_peak$Electrode  <- factor(Anova_170_Start_peak$Electrode, levels = c( 37,39 )) 

Anova_170s <- ezANOVA(Anova_170_Start_peak, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Hemisphere), 
                      detailed = TRUE)
Anova_170s

goggles_table <- apa.ezANOVA.table(Anova_170s,
                                   filename="Anova_170_Start_Peak.doc")

# Gaze: Frontal & Averted
mean_1 = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Gaze==1])
mean_1
mean_2 = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Gaze==2])
mean_2

# Emotion: 1, Neutral, 2, Angry, 3, Happy
mean_1 = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Emotion==1])
mean_1
mean_2 = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Emotion==2])
mean_2
mean_3 = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Emotion==3])
mean_3

# Hemisphere: 1, Left  2, Right 
mean_L = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Hemisphere==1])
mean_L
mean_R = mean(Anova_170_Start_peak$Amplitude[Anova_170_Start_peak$Hemisphere==2])
mean_R

## Plots

ezPlot(Anova_170_Start_peak, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="N170 Start Emotion")

ezPlot(Anova_170_Start_peak, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="N170 Start Gaze")

ezPlot(Anova_170_Start_peak, 
       dv = Amplitude, 
       wid = Subject, 
       within = Hemisphere,
       x = Hemisphere, 
       x_lab="N170 Start Hemisphere")

ezPlot(Anova_170_Start_peak, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="N170 Start Gaze x Emotion")

ezPlot(Anova_170_Start_peak, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Hemisphere),
       x = Hemisphere, 
       split=Gaze,
       x_lab="N170 Start Gaze x Hemisphere")


# post-hoc t-test
pairwise.t.test(Anova_170_Start_peak$Amplitude,Anova_170_Start_peak$Emotion, p.adj="bonferroni", paired=T)

######################################################################
################## EPN-Start ################
Anova_EPN_Start_200.250 = data.frame(Anova_EPN_Start_200.250) #Change accordingly 
head(Anova_EPN_Start_200.250)
hist(Anova_EPN_Start_200.250 $Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Start_200.250 $Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
boxplot(Anova_EPN_Start_200.250 $Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

### 200-250 ### 
Anova_EPN_Start_200.250$Subject      <- factor(Anova_EPN_Start_200.250$Subject,     levels = c( 1:114)) 
Anova_EPN_Start_200.250$Subject_type <- factor(Anova_EPN_Start_200.250$Subject_type,levels = c( 1:2)) 
Anova_EPN_Start_200.250$Gaze         <- factor(Anova_EPN_Start_200.250$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Start_200.250$Emotion      <- factor(Anova_EPN_Start_200.250$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Start_200.250$Pic_type     <- factor(Anova_EPN_Start_200.250$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Start_200.250$Electrode    <- factor(Anova_EPN_Start_200.250$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

EPN1sAnova <- ezANOVA(Anova_EPN_Start_200.250, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

summary(Anova_EPN_Start_200.250)

EPNAnova <- ezANOVA(Anova_EPN_Start_200.250, 
                    dv = Amplitude, 
                    wid = Subject, 
                    between = Subject_type, 
                    within = .(Pic_type, Emotion, Gaze), 
                    detailed = TRUE)
EPNAnova

goggles_table <- apa.ezANOVA.table(EPN1sAnova,
                                   filename="Anova_EPN_Start_200.250.doc")

#Labeling
Anova_EPN_Start_200.250$Gaze         <- factor(Anova_EPN_Start_200.250$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_EPN_Start_200.250$Emotion      <- factor(Anova_EPN_Start_200.250$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Start_200.250$Subject_type <- factor(Anova_EPN_Start_200.250$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
Anova_EPN_Start_200.250$Pic_type     <- factor(Anova_EPN_Start_200.250$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

#Tukey Results for Emotion Main Effect
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_200.250$Amplitude~Anova_EPN_Start_200.250$Emotion)), data=Anova_EPN_Start_200.250, conf.level=0.95)
print(TUKEYM1)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_200.250$Amplitude~Anova_EPN_Start_200.250$Pic_type:Anova_EPN_Start_200.250$Subject_type)), data= Anova_EPN_Start_200.250, conf.level=0.95)
print(TUKEYM1)
#m <- is.na(Anova_EPN_Start_200.250) 
#print(m)

sum(is.na(as.numeric(Anova_EPN_Start_200.250$Electrode)))
sum(is.na(as.numeric(Anova_EPN_Start_200.250$Emotion)))
sum(is.na(as.numeric(Anova_EPN_Start_200.250$Amplitude)))
sum(is.na(as.numeric(Anova_EPN_Start_200.250$Pic_type)))
sum(is.na(as.numeric(Anova_EPN_Start_200.250$Subject_type)))
sum(is.na(as.numeric(Anova_EPN_Start_200.250$Gaze)))
sum(is.na(as.numeric(Anova_EPN_Start_200.250$Subject)))

# Gaze
mean_D= mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Subject_type==2])
mean_Nm

mean_Mo= sd(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Subject_type==1])
mean_Mo
mean_Nm = sd(Anova_EPN_Start_200.250$Amplitude[Anova_EPN_Start_200.250$Subject_type==2])
mean_Nm

#Post hoc
filteredn <- Anova_EPN_Start_200.250 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1n <- filteredn %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2n <- filteredn %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3n <- filteredn %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1n$Subject, Emotion1n=Emotion1n$means, Emotion2n=Emotion2n$means, Emotion3n=Emotion3n$means)
# E12=mean(Emotion1) - mean(Emotion2)
En <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2n-Emotion1n, E31=Emotion3n-Emotion1n, E32=Emotion3n-Emotion2n)
En

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Angry    <- filteredn[filteredn$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Happy    <- filteredn[filteredn$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry  <- filteredn[filteredn$Emotion==2,]$means
Happy  <- filteredn[filteredn$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(En[,5:7], 2, mean)
# sds
apply(En[,5:7], 2, sd)
# CI
t.test(En$E21)
t.test(En$E31)
t.test(En$E32)

#Anova_EPN_Start_200.250$Subject_type <-  as.factor(Anova_EPN_Start_200.250$Subject_type)
#Anova_EPN_Start_200.250$Amplitude    <- as.numeric(Anova_EPN_Start_200.250$Amplitude)
#Anova_EPN_Start_200.250$Pic_type     <-  as.factor(Anova_EPN_Start_200.250$Pic_type)
#Anova_EPN_Start_200.250$Emotion      <-  as.factor(Anova_EPN_Start_200.250$Emotion)
#Anova_EPN_Start_200.250$Gaze         <-  as.factor(Anova_EPN_Start_200.250$Gaze)

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Start_200.250, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Start_200.250, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3g

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Direct", "Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Direct","Averted"))


#Emotion
p1e <- ggplot(Anova_EPN_Start_200.250, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy")) +ylim (-15,30)

# Emotion x Stimuli
p1p <- ggplot(Anova_EPN_Start_200.250, aes(x = Emotion, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Stimuli", x = "Emotion")
p2p

p3p = p2p + scale_color_manual(labels = c("Adult", "Infant"), values = c("purple", "black"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Infant"))
p3p
p3p + scale_x_discrete(name ="Emotion", labels=c("Neutral","Angry","Happy")) +ylim (-15,30)


#Emotion
ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Start Emotion")

ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Start Gaze")

ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Gaze),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Start Gaze x Emotion")

EPN <- ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Pic_type),
       x = Emotion,
       split= Pic_type,
       x_lab="Emotion",
       y_lab = "Mean",
       split_lab = "Emotion")

EPN+ylim(2,7)

ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude,
       wid = Subject,
       within = .(Subject_type,Pic_type),
       x = Pic_type,
       split = Subject_type,
       x_lab="EPN Start Subject_type x Stimuli")

ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Subject_type, Emotion),
       x = Subject_type, 
       split = Emotion,
       x_lab="EPN Start Subject_type x Emotion")

# post-hoc t-test
pairwise.t.test(Anova_pa_EPN_Start_200.250$Amplitude,Anova_pa_EPN_Start_200.250$Emotion, p.adj="bonferroni", paired=T)

### 250-300 ###
Anova_EPN_Start_250.300 = data.frame(Anova_EPN_Start_250.300) #Change accordingly 
head(Anova_EPN_Start_250.300)
hist(Anova_EPN_Start_250.300$Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Start_250.300$Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'MV', ylab='Frequency')
boxplot(Anova_EPN_Start_250.300$Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Start_250.300$Subject      <- factor(Anova_EPN_Start_250.300$Subject,     levels = c( 1:114)) 
Anova_EPN_Start_250.300$Subject_type <- factor(Anova_EPN_Start_250.300$Subject_type,levels = c( 1:2)) 
Anova_EPN_Start_250.300$Gaze         <- factor(Anova_EPN_Start_250.300$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Start_250.300$Emotion      <- factor(Anova_EPN_Start_250.300$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Start_250.300$Pic_type     <- factor(Anova_EPN_Start_250.300$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Start_250.300$Electrode    <- factor(Anova_EPN_Start_250.300$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

summary(Anova_EPN_Start_250.300)

EPN1sAnova <- ezANOVA(Anova_EPN_Start_250.300, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

EPNAnova <- ezANOVA(Anova_EPN_Start_250.300, 
                    dv = Amplitude, 
                    wid = Subject, 
                    between = Subject_type, 
                    within = .(Pic_type, Emotion, Gaze), 
                    detailed = TRUE)
EPNAnova

#Tukey Results for Emotion Main Effect
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_250.300$Amplitude~Anova_EPN_Start_250.300$Emotion)), data=Anova_EPN_Start_250.300, conf.level=0.95)
print(TUKEYM1)

#Stmuli X Group interaction
Anova_EPN_Start_250.300$Gaze         <- factor(Anova_EPN_Start_250.300$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_EPN_Start_250.300$Emotion      <- factor(Anova_EPN_Start_250.300$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Start_250.300$Subject_type <- factor(Anova_EPN_Start_250.300$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
Anova_EPN_Start_250.300$Pic_type     <- factor(Anova_EPN_Start_250.300$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_250.300$Amplitude~Anova_EPN_Start_250.300$Pic_type:Anova_EPN_Start_250.300$Subject_type)), data= Anova_EPN_Start_250.300, conf.level=0.95)
print(TUKEYM1)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_250.300$Amplitude~Anova_EPN_Start_250.300$Pic_type:Anova_EPN_Start_250.300$Subject_type:Anova_EPN_Start_250.300$Emotion)), data= Anova_EPN_Start_250.300, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(EPN2sAnova,
                                   filename="Anova_EPN_Start_250.300.doc")

# Gaze
mean_D= mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Subject_type==2])
mean_Nm

mean_Mo= sd(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Subject_type==1])
mean_Mo
mean_Nm = sd(Anova_EPN_Start_250.300$Amplitude[Anova_EPN_Start_250.300$Subject_type==2])
mean_Nm

# Factors
Anova_EPN_Start_250.300$Gaze         <- factor(Anova_EPN_Start_250.300$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_EPN_Start_250.300$Emotion      <- factor(Anova_EPN_Start_250.300$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Start_250.300$Subject_type <- factor(Anova_EPN_Start_250.300$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
Anova_EPN_Start_250.300$Pic_type     <- factor(Anova_EPN_Start_250.300$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

filtered <- Anova_EPN_Start_250.300 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1$Subject, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

# Current project
#Emotion (here i am looking for mean diffrences in groups based on a specific factor)
filtered <- Anova_EPN_Start_250.300 %>% group_by(Emotion, Subject_type) %>% summarise(means=mean(Amplitude))
Emotion1 <- filtered %>% filter(Emotion==1) %>% group_by(Subject_type) %>% summarise(means=mean(means))
Emotion2 <- filtered %>% filter(Emotion==2) %>% group_by(Subject_type) %>% summarise(means=mean(means))
Emotion3 <- filtered %>% filter(Emotion==3) %>% group_by(Subject_type) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject_type=Emotion1$Subject_type, Emotion1=Emotion1$means, Emotion2=Emotion2$means, Emotion3=Emotion3$means)
# E12=mean(Emotion1) - mean(Emotion2)
E <- AllEmotions %>% group_by(Subject_type) %>% mutate(E21=Emotion2-Emotion1, E31=Emotion3-Emotion1, E32=Emotion3-Emotion2)
E

# t-test
Neutral  <- filtered[filtered$Emotion==1,]$means
Angry    <- filtered[filtered$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

#Subject type 1 (Emotion 1-2 (Pic_type))
filtered_mo = filtered[filtered$Subject_type==1,] #Filtering based on the group Factor

Neutral  <- filtered_mo[filtered_mo$Emotion==1,]$means
Angry    <- filtered_mo[filtered_mo$Emotion==2,]$means
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

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Start_250.300, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Start_250.300, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p6

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Direct", "Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Direct","Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Start_250.300, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy")) +ylim (-15,30)

# Emotion
p1p <- ggplot(Anova_EPN_Start_250.300, aes(x = Emotion, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Stimuli", x = "Emotion")
p2p

p3p = p2p + scale_color_manual(labels = c("Adult", "Infant"), values = c("purple", "black"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Infant"))
p3p
p3p + scale_x_discrete(name ="Emotion", labels=c("Neutral","Angry","Happy")) +ylim (-15,30)


## Plots
ezPlot(Anova_EPN_Start_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Start Emotion")

ezPlot(Anova_EPN_Start_250.300, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Start Gaze")

ezPlot(Anova_EPN_Start_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Start Gaze x Emotion")

EPN <- ezPlot(Anova_EPN_Start_250.300, 
              dv = Amplitude, 
              wid = Subject,
              within = .(Emotion,Pic_type),
              x = Emotion, 
              split= Pic_type,
              x_lab="EPN Start Emotion x Stimuli")

EPN+ylim(2,7)

ezPlot(Anova_EPN_Start_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Subject_type,Pic_type),
       x = Pic_type,
       split = Subject_type,
       x_lab="EPN Start Subject_type x Stimuli")

ezPlot(Anova_EPN_Start_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Subject_type, Emotion),
       x = Subject_type, 
       split = Emotion,
       x_lab="EPN Start Subject_type x Emotion")

ezPlot(Anova_EPN_Start_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(250-300) Start Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_EPN_Start_250.300$Amplitude,Anova_EPN_Start_250.300$Emotion, p.adj="bonferroni", paired=T)

### 300-350 ###
Anova_EPN_Start_300.350 =data.frame(Anova_EPN_Start_300.350) #Change accordingly 
head(Anova_EPN_Start_300.350)
hist(Anova_EPN_Start_300.350$Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Start_300.350$Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'MV', ylab='Frequency')
boxplot(Anova_EPN_Start_300.350$Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Start_300.350$Subject      <- factor(Anova_EPN_Start_300.350$Subject,     levels = c( 1:114)) 
Anova_EPN_Start_300.350$Subject_type <- factor(Anova_EPN_Start_300.350$Subject_type,levels = c( 1:2)) 
Anova_EPN_Start_300.350$Gaze         <- factor(Anova_EPN_Start_300.350$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Start_300.350$Emotion      <- factor(Anova_EPN_Start_300.350$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Start_300.350$Pic_type     <- factor(Anova_EPN_Start_300.350$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Start_300.350$Electrode    <- factor(Anova_EPN_Start_300.350$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

EPN1sAnova <- ezANOVA(Anova_EPN_Start_300.350, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

EPN2sAnova <- ezANOVA(Anova_EPN_Start_300.350, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
EPN2sAnova

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_300.350$Amplitude~Anova_EPN_Start_300.350$Emotion)), data=Anova_EPN_Start_300.350, conf.level=0.95)
print(TUKEYM1)

#Stmuli X Group interaction
Anova_EPN_Start_300.350$Gaze         <- factor(Anova_EPN_Start_300.350$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_EPN_Start_300.350$Emotion      <- factor(Anova_EPN_Start_300.350$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Start_300.350$Subject_type <- factor(Anova_EPN_Start_300.350$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
Anova_EPN_Start_300.350$Pic_type     <- factor(Anova_EPN_Start_300.350$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_300.350$Amplitude~Anova_EPN_Start_300.350$Pic_type:Anova_EPN_Start_300.350$Subject_type)), data= Anova_EPN_Start_300.350, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(EPN2sAnova,
                                   filename="Anova_EPN_Start_300.350.doc")

# Gaze
mean_D= mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Subject_type==1])
SD_Mo
SD_Nm = sd(Anova_EPN_Start_300.350$Amplitude[Anova_EPN_Start_300.350$Subject_type==2])
SD_Nm

##
filteredn <- Anova_EPN_Start_300.350 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1n <- filteredn %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2n <- filteredn %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3n <- filteredn %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1n$Subject, Emotion1n=Emotion1n$means, Emotion2n=Emotion2n$means, Emotion3n=Emotion3n$means)
# E12=mean(Emotion1) - mean(Emotion2)
En <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2n-Emotion1n, E31=Emotion3n-Emotion1n, E32=Emotion3n-Emotion2n)
En

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Angry    <- filteredn[filteredn$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Happy    <- filteredn[filteredn$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry  <- filteredn[filteredn$Emotion==2,]$means
Happy  <- filteredn[filteredn$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# means
apply(En[,5:7], 2, mean)
# sds
apply(En[,5:7], 2, sd)
# CI
t.test(En$E21)
t.test(En$E31)
t.test(En$E32)

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Start_300.350, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Start_300.350, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p6

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Direct", "Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Direct","Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Start_300.350, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy")) +ylim (-15,30)
  
# Emotion
p1p <- ggplot(Anova_EPN_Start_300.350, aes(x = Emotion, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Stimuli", x = "Emotion")
p2p

p3p = p2p + scale_color_manual(labels = c("Adult", "Infant"), values = c("purple", "black"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Infant"))
p3p
p3p + scale_x_discrete(name ="Emotion", labels=c("Neutral","Angry","Happy")) +ylim (-15,30)


## Plots
ezPlot(Anova_EPN_Start_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Start Emotion")

ezPlot(Anova_EPN_Start_300.350, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Start Gaze")

ezPlot(Anova_EPN_Start_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Start Gaze x Emotion")

EPN <- ezPlot(Anova_EPN_Start_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Pic_type),
       x = Emotion, 
       split= Pic_type,
       x_lab="EPN Start Emotion x Stimuli")

EPN + ylim (2,7)

ezPlot(Anova_EPN_Start_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(300-350) Start Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_EPN_Start_300.350$Amplitude,Anova_EPN_Start_300.350$Emotion, p.adj="bonferroni", paired=T)

### 350-400 ###
Anova_EPN_Start_350.400 =data.frame(Anova_EPN_Start_350.400) #Change accordingly 
head(Anova_EPN_Start_350.400)
hist(Anova_EPN_Start_350.400$Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Start_350.400$Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'MV', ylab='Frequency')
boxplot(Anova_EPN_Start_350.400$Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Start_350.400$Subject      <- factor(Anova_EPN_Start_350.400$Subject,     levels = c( 1:114)) 
Anova_EPN_Start_350.400$Subject_type <- factor(Anova_EPN_Start_350.400$Subject_type,levels = c( 1:2)) 
Anova_EPN_Start_350.400$Gaze         <- factor(Anova_EPN_Start_350.400$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Start_350.400$Emotion      <- factor(Anova_EPN_Start_350.400$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Start_350.400$Pic_type     <- factor(Anova_EPN_Start_350.400$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Start_350.400$Electrode    <- factor(Anova_EPN_Start_350.400$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

summary(Anova_EPN_Start_350.400)

EPN1sAnova <- ezANOVA(Anova_EPN_Start_350.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

EPN4sAnova <- ezANOVA(Anova_EPN_Start_350.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
EPN4sAnova

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Start_350.400$Amplitude~Anova_EPN_Start_350.400$Emotion)), data=Anova_EPN_Start_350.400, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(EPN4sAnova,
                                   filename="Anova_EPN_Start_350.400.doc")

# Gaze
mean_D= mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Subject_type==1])
SD_Mo
SD_Nm = sd(Anova_EPN_Start_350.400$Amplitude[Anova_EPN_Start_350.400$Subject_type==2])
SD_Nm

##
filteredn <- Anova_EPN_Start_350.400 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
Emotion1n <- filteredn %>% filter(Emotion==1) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion2n <- filteredn %>% filter(Emotion==2) %>% group_by(Subject) %>% summarise(means=mean(means))
Emotion3n <- filteredn %>% filter(Emotion==3) %>% group_by(Subject) %>% summarise(means=mean(means))
AllEmotions <- cbind.data.frame(Subject=Emotion1n$Subject, Emotion1n=Emotion1n$means, Emotion2n=Emotion2n$means, Emotion3n=Emotion3n$means)
# E12=mean(Emotion1) - mean(Emotion2)
En <- AllEmotions %>% group_by(Subject) %>% mutate(E21=Emotion2n-Emotion1n, E31=Emotion3n-Emotion1n, E32=Emotion3n-Emotion2n)
En

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Angry    <- filteredn[filteredn$Emotion==2,]$means
t.test(Neutral,Angry , paired = T)

Neutral  <- filteredn[filteredn$Emotion==1,]$means
Happy    <- filteredn[filteredn$Emotion==3,]$means
t.test(Neutral,Happy , paired = T)

Angry  <- filteredn[filteredn$Emotion==2,]$means
Happy  <- filteredn[filteredn$Emotion==3,]$means
t.test(Angry,Happy , paired = T)

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Start_350.400, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Start_350.400, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p6

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Direct", "Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Direct","Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Start_350.400, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mothers", "Nulliparas"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy")) +ylim (-15,30)

#Emotion
p1p <- ggplot(Anova_EPN_Start_350.400, aes(x = Emotion, y = Amplitude, color = Pic_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Stimuli", x = "Emotion")
p2p

p3p = p2p + scale_color_manual(labels = c("Adult", "Infant"), values = c("purple", "black"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Infant"))
p3p
p3p + scale_x_discrete(name ="Emotion", labels=c("Neutral","Angry","Happy")) +ylim (-15,30)


## Plots
ezPlot(Anova_EPN_Start_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Start Emotion")

ezPlot(Anova_EPN_Start_350.400, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Start Gaze")


EPN <- ezPlot(Anova_EPN_Start_350.400, 
              dv = Amplitude, 
              wid = Subject,
              within = .(Emotion,Pic_type),
              x = Emotion, 
              split= Pic_type,
              x_lab="EPN Start Emotion x Stimuli")

EPN + ylim (2,7)

ezPlot(Anova_EPN_Start_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Start Gaze x Emotion")

ezPlot(Anova_EPN_Start_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(350.400) Start Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_EPN_Start_350.400$Amplitude,Anova_EPN_Start_350.400$Emotion, p.adj="bonferroni", paired=T)

#################################################################
############# LPC-START #################

### 300-400 ###
Anova_LPC_Start_300.400$Subject      <- factor(Anova_LPC_Start_300.400$Subject,     levels = c( 1:114)) 
Anova_LPC_Start_300.400$Subject_type <- factor(Anova_LPC_Start_300.400$Subject_type,levels = c( 0:1)) #0:mother, 1: N-Mothers
Anova_LPC_Start_300.400$Gaze         <- factor(Anova_LPC_Start_300.400$Gaze,        levels = c( 1:2 )) 
Anova_LPC_Start_300.400$Emotion      <- factor(Anova_LPC_Start_300.400$Emotion,     levels = c( 1:3 )) 
Anova_LPC_Start_300.400$Pic_type     <- factor(Anova_LPC_Start_300.400$Pic_type,    levels = c( 1:2 )) 
Anova_LPC_Start_300.400$Electrode    <- factor(Anova_LPC_Start_300.400$Electrode,   levels = c( 22,23,27,28,29,18)) 

table(Anova_LPC_Start_300.400$Subject_type)

LPC1sAnova <- ezANOVA(Anova_LPC_Start_300.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
LPC1sAnova

#with(Anova_LPC_Start_300.400,table(Subject, Emotion,Electrode))
#Anova_LPC_Start_300.400$Subject = factor(paste(Anova_LPC_Start_300.400$Subject_type,Anova_LPC_Start_300.400$Subject))
#ezDesign(data=Anova_LPC_Start_300.400, y=Subject, x=Emotion, col=Subject_type)
#Anova_EPN_Start_350.400$Subject = factor(paste(Anova_EPN_Start_350.400$Subject_type,Anova_EPN_Start_350.400$Subject))

LPC4sAnova <- ezANOVA(Anova_LPC_Start_300.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within = .(Pic_type,Emotion,Gaze), 
                      detailed = TRUE)
LPC4sAnova

Anova_LPC_Start_300.400$Gaze         <- factor(Anova_LPC_Start_300.400$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_LPC_Start_300.400$Emotion      <- factor(Anova_LPC_Start_300.400$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_LPC_Start_300.400$Subject_type <- factor(Anova_LPC_Start_300.400$Subject_type, levels = 0:1, labels = c('Mother',"N-Mother"))
Anova_LPC_Start_300.400$Pic_type     <- factor(Anova_LPC_Start_300.400$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

options(max.print = 1000000)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_300.400$Amplitude~Anova_LPC_Start_300.400$Gaze:Anova_LPC_Start_300.400$Emotion:Anova_LPC_Start_300.400$Pic_type:Anova_LPC_Start_300.400$Subject_type)), data=Anova_pa_EPN_Start_200.400, conf.level=0.95)
print(TUKEYM1)

# i want to see the triple interaction i got after the aggregation of the EPN time windows wroks: Subject type & pic type & Emotion
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_300.400$Amplitude~Anova_LPC_Start_300.400$Emotion:Anova_LPC_Start_300.400)), data=Anova_pa_EPN_Start_200.400, conf.level=0.95)
print(TUKEYM1)

# To Look at the interaction Subject_type X Pic_type X Emotion X Gaze
Anova_LPC_Start_300.400_Mother = Anova_LPC_Start_300.400 [Anova_LPC_Start_300.400$Subject_type==0,] #Filtering based on the Subject_type Factor (mothers)
Anova_LPC_Start_300.400_NMother = Anova_LPC_Start_300.400 [Anova_LPC_Start_300.400$Subject_type==1,] #Filtering based on the Subject_type Factor (N-mothers)

table(Anova_LPC_Start_300.400_Mother$Subject_type)
table(Anova_LPC_Start_300.400_NMother)
head(Anova_LPC_Start_300.400_Mother)
head(Anova_LPC_Start_300.400_NMother)

#Mother
LPC4sAnova <- ezANOVA(Anova_LPC_Start_300.400_Mother, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Pic_type,Emotion,Gaze), 
                      detailed = TRUE)
LPC4sAnova

#N-Mother
LPC4sAnova <- ezANOVA(Anova_LPC_Start_300.400_NMother, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Pic_type,Emotion,Gaze), 
                      detailed = TRUE)
LPC4sAnova

#Mother
Anova_LPC_Start_300.400_Mother$Gaze         <- factor(Anova_LPC_Start_300.400_Mother$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_LPC_Start_300.400_Mother$Emotion      <- factor(Anova_LPC_Start_300.400_Mother$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_LPC_Start_300.400_Mother$Pic_type     <- factor(Anova_LPC_Start_300.400_Mother$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))
#N-Mother
Anova_LPC_Start_300.400_NMother$Gaze         <- factor(Anova_LPC_Start_300.400_NMother$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_LPC_Start_300.400_NMother$Emotion      <- factor(Anova_LPC_Start_300.400_NMother$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_LPC_Start_300.400_NMother$Pic_type     <- factor(Anova_LPC_Start_300.400_NMother$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

#Mother
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_300.400_Mother$Amplitude~Anova_LPC_Start_300.400_Mother$Gaze:Anova_LPC_Start_300.400_Mother$Emotion:Anova_LPC_Start_300.400_Mother$Pic_type)), data=Anova_LPC_Start_300.400_Mother, conf.level=0.95)
print(TUKEYM1)
#N-Mother
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_300.400_NMother$Amplitude~Anova_LPC_Start_300.400_NMother$Gaze:Anova_LPC_Start_300.400_NMother$Emotion:Anova_LPC_Start_300.400_NMother$Pic_type)), data=Anova_LPC_Start_300.400_NMother, conf.level=0.95)
print(TUKEYM1)

# Plotting
goggles_table <- apa.ezANOVA.table(LPC4sAnova,
                                   filename="Anova_LPC_Start_300.400.doc")

## Gaze
mean_D= mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Gaze==1])
mean_D
mean_a = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Emotion==1])
mean_Nu
mean_An = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Emotion==2])
mean_An
mean_Ha = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Emotion==3])
mean_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Subject_type==0])
mean_Mo
mean_Nm = mean(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Subject_type==1])
mean_Nm

SD_Mo= sd(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Subject_type==0])
SD_Mo
SD_Nm = sd(Anova_LPC_Start_300.400$Amplitude[Anova_LPC_Start_300.400$Subject_type==1])
SD_Nm


## Plots
ezPlot(Anova_LPC_Start_300.400, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="LPC Start Emotion")

ezPlot(Anova_LPC_Start_300.400, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="LPC Start Gaze")

ezPlot(Anova_LPC_Start_300.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="LPC Start Gaze x Emotion")

ezPlot(Anova_LPC_Start_300.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="LPC (300.400) Start Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_LPC_Start_300.400$Amplitude,Anova_LPC_Start_300.400$Emotion, p.adj="bonferroni", paired=T)

### 400-500 ###

Anova_LPC_Start_400.500$Subject      <- factor(Anova_LPC_Start_400.500$Subject,     levels = c( 1:114)) 
Anova_LPC_Start_400.500$Subject_type <- factor(Anova_LPC_Start_400.500$Subject_type,levels = c( 0:1)) 
Anova_LPC_Start_400.500$Gaze         <- factor(Anova_LPC_Start_400.500$Gaze,        levels = c( 1:2 )) 
Anova_LPC_Start_400.500$Emotion      <- factor(Anova_LPC_Start_400.500$Emotion,     levels = c( 1:3 )) 
Anova_LPC_Start_400.500$Pic_type     <- factor(Anova_LPC_Start_400.500$Pic_type,    levels = c( 1:2 )) 
Anova_LPC_Start_400.500$Electrode    <- factor(Anova_LPC_Start_400.500$Electrode,   levels = c( 22,23,27,28,29,18)) 

LPC1sAnova <- ezANOVA(Anova_LPC_Start_400.500, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
LPC1sAnova

LPC4sAnova <- ezANOVA(Anova_LPC_Start_400.500, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within = .(Pic_type,Emotion,Gaze), 
                      detailed = TRUE)
LPC4sAnova

Anova_LPC_Start_400.500$Gaze         <- factor(Anova_LPC_Start_400.500$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_LPC_Start_400.500$Emotion      <- factor(Anova_LPC_Start_400.500$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_LPC_Start_400.500$Subject_type <- factor(Anova_LPC_Start_400.500$Subject_type, levels = 0:1, labels = c('Mother',"N-Mother"))
Anova_LPC_Start_400.500$Pic_type     <- factor(Anova_LPC_Start_400.500$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

options(max.print = 1000000)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_400.500$Amplitude~Anova_LPC_Start_400.500$Gaze:Anova_LPC_Start_400.500$Emotion:Anova_LPC_Start_400.500$Pic_type:Anova_LPC_Start_400.500$Subject_type)), data=Anova_pa_EPN_Start_200.400, conf.level=0.95)
print(TUKEYM1)

# i want to see the triple interaction i got after the aggregation of the EPN time windows wroks: Subject type & pic type & Emotion
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_400.500$Amplitude~Anova_LPC_Start_400.500$Emotion:Anova_LPC_Start_400.500)), data=Anova_pa_EPN_Start_200.400, conf.level=0.95)
print(TUKEYM1)

# To Look at the interaction Subject_type X Pic_type X Emotion X Gaze
Anova_LPC_Start_400.500_Mother = Anova_LPC_Start_400.500 [Anova_LPC_Start_400.500$Subject_type==0,] #Filtering based on the Subject_type Factor (mothers)
Anova_LPC_Start_400.500_NMother = Anova_LPC_Start_400.500 [Anova_LPC_Start_400.500$Subject_type==1,] #Filtering based on the Subject_type Factor (N-mothers)

table(Anova_LPC_Start_400.500_Mother$Subject_type)
table(Anova_LPC_Start_400.500_NMother)
head(Anova_LPC_Start_400.500_Mother)
head(Anova_LPC_Start_400.500_NMother)

#Mother
LPC4sAnova <- ezANOVA(Anova_LPC_Start_400.500_Mother, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Pic_type,Emotion,Gaze), 
                      detailed = TRUE)
LPC4sAnova

#N-Mother
LPC4sAnova <- ezANOVA(Anova_LPC_Start_400.500_NMother, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Pic_type,Emotion,Gaze), 
                      detailed = TRUE)
LPC4sAnova

#Mother
Anova_LPC_Start_400.500_Mother$Gaze         <- factor(Anova_LPC_Start_400.500_Mother$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_LPC_Start_400.500_Mother$Emotion      <- factor(Anova_LPC_Start_400.500_Mother$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_LPC_Start_400.500_Mother$Pic_type     <- factor(Anova_LPC_Start_400.500_Mother$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))
#N-Mother
Anova_LPC_Start_400.500_NMother$Gaze         <- factor(Anova_LPC_Start_400.500_NMother$Gaze,         levels = 1:2, labels = c('Direct', 'Averted')) #'A-F', 'F_A'
Anova_LPC_Start_400.500_NMother$Emotion      <- factor(Anova_LPC_Start_400.500_NMother$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_LPC_Start_400.500_NMother$Pic_type     <- factor(Anova_LPC_Start_400.500_NMother$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))

#Mother
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_400.500_Mother$Amplitude~Anova_LPC_Start_400.500_Mother$Gaze:Anova_LPC_Start_400.500_Mother$Emotion:Anova_LPC_Start_400.500_Mother$Pic_type)), data=Anova_LPC_Start_400.500_Mother, conf.level=0.95)
print(TUKEYM1)
#N-Mother
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_LPC_Start_400.500_NMother$Amplitude~Anova_LPC_Start_400.500_NMother$Gaze:Anova_LPC_Start_400.500_NMother$Emotion:Anova_LPC_Start_400.500_NMother$Pic_type)), data=Anova_LPC_Start_400.500_NMother, conf.level=0.95)
print(TUKEYM1)






goggles_table <- apa.ezANOVA.table(LPC4sAnova,
                                   filename="Anova_LPC_Start_400.500.doc")

#with(Anova_LPC_Start_300.400,table(Subject, Emotion,Electrode))
Anova_LPC_Start_400.500$Subject = factor(paste(Anova_LPC_Start_400.500$Subject_type,Anova_LPC_Start_400.500$Subject))
#ezDesign(data=Anova_LPC_Start_300.400, y=Subject, x=Emotion, col=Subject_type)
#Anova_EPN_Start_350.400$Subject = factor(paste(Anova_EPN_Start_350.400$Subject_type,Anova_EPN_Start_350.400$Subject))




Anova_LPC_Start_400.500

goggles_table <- apa.ezANOVA.table(LPC1sAnova,
                                   filename="Anova_LPC_Start_400.500.doc")


## Gaze
mean_D= mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Gaze==1])
mean_D
mean_a = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Emotion==1])
mean_Nu
mean_An = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Emotion==2])
mean_An
mean_Ha = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Emotion==3])
mean_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Subject_type==0])
mean_Mo
mean_Nm = mean(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Subject_type==1])
mean_Nm

SD_Mo= sd(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Subject_type==0])
SD_Mo
SD_Nm = sd(Anova_LPC_Start_400.500$Amplitude[Anova_LPC_Start_400.500$Subject_type==1])
SD_Nm

## Plots
ezPlot(Anova_LPC_Start_400.500, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="LPC Start Emotion")

ezPlot(Anova_LPC_Start_400.500, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="LPC Start Gaze")

ezPlot(Anova_LPC_Start_400.500, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="LPC Start Gaze x Emotion")

ezPlot(Anova_LPC_Start_400.500, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="LPC (400.500) Start Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_LPC_Start_400.500$Amplitude,Anova_LPC_Start_400.500$Emotion, p.adj="bonferroni", paired=T)
