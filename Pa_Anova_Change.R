########################
### ezANOVA - Change ###
########################
# Parenthood Study Statistical Analysis
# 16.06.2020 extra edits 22.08.2021

# This scripts provides statistical analysis of START-conditions (first image)
# using .txt data created before with MATLAB and excel masks
# also plots are created to visualize the output

# here: without plots. Use the following format for plots:
# ezPlot(Anova_170_Start, 
# dv = Amplitude, 
# wid = Subject, 
# within = Hemisphere,
# x = Hemisphere, 
# x_lab="N170 Start Hemisphere")

# set work directory
setwd("~/Desktop/PA_DATA/")
setwd("~/Desktop/PA_DATA/EPN/NEW/") #all the corrected data are here! 2021
getwd()

library(ez)
library(apaTables)
library(car)
library(ggplot2)
#library(stargazer)
#library(apa)

# import data
Anova_170_Change_region     = read.table("Anova_pa_170_Change_region.txt",    header=TRUE)
Anova_170_Change_peak       = read.table("Anova_Pa_170_Change_peak.txt",      header=TRUE)
Anova_170_Change_hemisphere = read.table("Anova_pa_170_Change_hemisphere.txt",header=TRUE)
Anova_EPN_Change_200.250    = read.table("Anova_pa_EPN_Change_200-250.txt",   header=TRUE)
Anova_EPN_Change_250.300    = read.table("Anova_pa_EPN_Change_250-300.txt",   header=TRUE)
Anova_EPN_Change_300.350    = read.table("Anova_pa_EPN_Change_300-350.txt",   header=TRUE)
Anova_EPN_Change_350.400    = read.table("Anova_pa_EPN_Change_350-400.txt",   header=TRUE)
Anova_LPC_Change_300.400    = read.table("Anova_pa_LPC_Change_300-400.txt",   header=TRUE)
Anova_LPC_Change_400.500    = read.table("Anova_pa_LPC_Change_400-500.txt",   header=TRUE)

options(max.print=1000000)


################## Normalization Process ##################
# Data Primary Check
length(Anova_170_Change_hemisphere$Amplitude) # 5742
length(Anova_EPN_Change_200.250$Amplitude) #13680
length(Anova_EPN_Change_250.300$Amplitude) #13680
length(Anova_EPN_Change_300.350$Amplitude) #13680
length(Anova_EPN_Change_350.400$Amplitude) #13680
# Shapiro Test for normality 
# We can try to do the shapiro test using only the first 5.000 samples
shapiro.test(Anova_170_Change_hemisphere$Amplitude[0:5000]) 
qqnorm(Anova_170_Change_hemisphere$Amplitude);qqline(Anova_170_Change_hemisphere$Amplitude)
Anova_170_Change_hemisphere$Amplitude <- as.numeric(scale(Anova_170_Change_hemisphere$Amplitude)) 
#(Anova_170_Start_hemisphere$Amplitude - mean(Anova_170_Start_hemisphere$Amplitude))/sd(Anova_170_Start_hemisphere$Amplitude) # first of the scale output

# let's check
Anova_170_Change_hemisphere$Amplitude <- scale(Anova_170_Change_hemisphere$Amplitude) 

Anova_170_Change_hemisphere=data.frame(Anova_170_Change_hemisphere) #Change accordingly 
head(Anova_170_Change_hemisphere)
hist(Anova_170_Change_hemisphere$Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_170_Change_hemisphere$Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'MV', ylab='Frequency')
boxplot(Anova_170_Change_hemisphere$Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

# Levene's test for equality of variances
Anova_170_Change_hemisphere$Subject_type <-  as.factor(Anova_170_Change_hemisphere$Subject_type)
Anova_170_Change_hemisphere$Amplitude    <- as.numeric(Anova_170_Change_hemisphere$Amplitude)
result = leveneTest(weight ~ group, Anova_170_Start_hemisphere)
print (result)

################## N170-Start_region ################

Anova_170_Change_region$Subject      <- factor(Anova_170_Change_region$Subject,     levels = c( 1:114)) 
Anova_170_Change_region$Subject_type <- factor(Anova_170_Change_region$Subject_type,levels = c( 1:2)) 
Anova_170_Change_region$Gaze         <- factor(Anova_170_Change_region$Gaze,        levels = c( 1:2 )) 
Anova_170_Change_region$Emotion      <- factor(Anova_170_Change_region$Emotion,     levels = c( 1:3 )) 
Anova_170_Change_region$Pic_type     <- factor(Anova_170_Change_region$Pic_type,    levels = c( 1:2 )) 
Anova_170_Change_region$Electrode    <- factor(Anova_170_Change_region$Electrode,   levels = c( 30,31,35,34 )) 

Anova_170s <- ezANOVA (Anova_170_Change_region, 
                        dv = Amplitude, 
                        wid = Subject, 
                        within  = .(Pic_type, Emotion, Gaze, Electrode), 
                        detailed = TRUE)
Anova_170s

goggles_table <- apa.ezANOVA.table(Anova_170s,
                                   filename="Anova_pa_170_Start_region.doc")

Anova_170s <- ezANOVA (Anova_170_Change_region, 
                        dv = Amplitude, 
                        wid = Subject, 
                        between = Subject_type, 
                        within  = .(Pic_type, Emotion, Gaze, Electrode), 
                        detailed = TRUE)
Anova_170s

goggles_table <- apa.ezANOVA.table(Anova_170s,
                                   filename="Anova_pa_170_Start_region_between.doc")

# Gaze
mean_D= mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Gaze==1])
mean_D
mean_a = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Emotion==1])
mean_Nu
mean_An = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Emotion==2])
mean_An
mean_Ha = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Emotion==3])
mean_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Subject_type==2])
mean_Nm

mean_Mo= sd(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Subject_type==1])
mean_Mo
mean_Nm = sd(Anova_170_Change_region$Amplitude[Anova_170_Change_region$Subject_type==2])
mean_Nm

## Plots
ezPlot(Anova_170_Change_region, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="N170 Start Emotion")

ezPlot(Anova_170_Change_region, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="N170 Start Gaze")

ezPlot(Anova_170_Change_region, 
       dv = Amplitude, 
       wid = Subject, 
       within = Pic_type, 
       x = Pic_type, 
       x_lab="N170 Start Pic_type")

ezPlot(Anova_170_Change_region, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Pic_type,Emotion),
       x = Emotion, 
       split= Emotion,
       x_lab="N170 Start Pic_type x Emotion")

ezPlot(Anova_170_Change_region,  
       dv = Amplitude, 
       wid = Subject,
       within = .(Subject_type,Emotion),
       x = Emotion, 
       split= Emotion,
       x_lab="N170 Start Subj_type x Emotion")

###################``
# post-hoc t-test
pairwise.t.test(Anova_170_Change_region$Amplitude,Anova_170_Change_region$Emotion, p.adj="bonferroni", paired=T)

################## N170-Change_hemisphere ################

Anova_170_Change_hemisphere$Subject      <- factor(Anova_170_Change_hemisphere$Subject,     levels = c( 1:114)) 
Anova_170_Change_hemisphere$Subject_type <- factor(Anova_170_Change_hemisphere$Subject_type,levels = c( 1:2)) 
Anova_170_Change_hemisphere$Gaze         <- factor(Anova_170_Change_hemisphere$Gaze,        levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Emotion      <- factor(Anova_170_Change_hemisphere$Emotion,     levels = c( 1:3 )) 
Anova_170_Change_hemisphere$Pic_type     <- factor(Anova_170_Change_hemisphere$Pic_type,    levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Hemisphere   <- factor(Anova_170_Change_hemisphere$Hemisphere,  levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Electrode    <- factor(Anova_170_Change_hemisphere$Electrode,   levels = c( 30,26,33,34 )) 

summary(Anova_170_Change_hemisphere)

Anova_170c1 <- ezANOVA (Anova_170_Change_hemisphere, 
                       dv = Amplitude, 
                       wid = Subject, 
                       within  = .(Pic_type, Emotion, Gaze, Electrode), 
                       detailed = TRUE)
Anova_170c1

goggles_table <- apa.ezANOVA.table(Anova_170c,
                                   filename="Anova_170_Change_hemisphere.doc")

Anova_170c1 <- ezANOVA (Anova_170_Change_hemisphere, 
                       dv = Amplitude, 
                       wid = Subject, 
                       between = Subject_type, 
                       within  = .(Pic_type, Emotion, Gaze, Hemisphere), 
                       detailed = TRUE)
Anova_170c1

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_170_Change_hemisphere$Amplitude~Anova_170_Change_hemisphere$Emotion)), data=Anova_170_Change_hemisphere, conf.level=0.95)
print(TUKEYM1)

Anova_170_Change_hemisphere$Gaze         <- factor(Anova_170_Change_hemisphere$Gaze,         levels = 1:2, labels = c('Av_D', 'D_Av')) #'A-F', 'F_A'
Anova_170_Change_hemisphere$Emotion      <- factor(Anova_170_Change_hemisphere$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_170_Change_hemisphere$Subject_type <- factor(Anova_170_Change_hemisphere$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
Anova_170_Change_hemisphere$Pic_type     <- factor(Anova_170_Change_hemisphere$Pic_type,     levels = 1:2, labels = c('Adult',"Baby"))
Anova_170_Change_hemisphere$Hemisphere   <- factor(Anova_170_Change_hemisphere$Hemisphere,   levels = 1:2, labels = c('Left',"Right"))

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_170_Change_hemisphere$Amplitude~Anova_170_Change_hemisphere$Gaze:Anova_170_Change_hemisphere$Hemisphere)), data=Anova_170_Start_hemisphere, conf.level=0.95)
print(TUKEYM1)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_170_Change_hemisphere$Amplitude~Anova_170_Change_hemisphere$Pic_type:Anova_170_Change_hemisphere$Hemisphere)), data=Anova_170_Start_hemisphere, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(Anova_170c1,
                                   filename="Anova_170_Change_hemisphere.doc")

# Gaze
mean_D= mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Gaze==1])
mean_D
mean_a = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Gaze==2])
mean_a

SD_D= sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Gaze==1])
SD_D
SD_a = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Gaze==2])
SD_a

# Hemisphere
mean_L = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Hemisphere==1])
mean_L
mean_R = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Hemisphere==2])
mean_R

sd_L = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Hemisphere==1])
sd_L
sd_R = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Hemisphere==2])
sd_R

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Emotion==1])
mean_Nu
mean_An = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Emotion==2])
mean_An
mean_Ha = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Emotion==1])
SD_Nu
SD_An = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Emotion==2])
SD_An
SD_Ha = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Subject_type==1])
mean_Mo
SD_Nm = sd(Anova_170_Change_hemisphere$Amplitude[Anova_170_Change_hemisphere$Subject_type==2])
mean_Nm

##
filtered <- Anova_170_Change_hemisphere %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
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

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_170_Change_hemisphere, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
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
p1g <- ggplot(Anova_170_Change_hemisphere, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2g

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3g

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Averted to Direct", "Direct to Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Averted to Direct","Direct to Averted"))

#Emotion
p1e <- ggplot(Anova_170_Change_hemisphere, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy"))

#Hemisphere
p1h <- ggplot(Anova_170_Change_hemisphere, aes(x = Hemisphere, y = Amplitude, color = Subject_type)) +
  geom_boxplot()

p2h = p1h + labs(color = "Groups", x = "Hemisphere")
p2h

p3h = p2h + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3h

p3h = p2h + scale_fill_manual(x ="Hemisphere", labels = x("Left", "Right"))
p3h
p3h + scale_x_discrete(name ="Hemisphere", labels=c("Left", "Right"))

## Plots
ezPlot(Anova_170_Change_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="N170 Change Emotion")

ezPlot(Anova_170_Change_hemisphere, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="N170 Change Gaze")

ezPlot(Anova_170_Change_hemisphere, 
       dv = Amplitude, 
       wid = Subject, 
       within = Hemisphere,
       x = Hemisphere, 
       x_lab="N170 Change Hemisphere")

ezPlot(Anova_170_Change_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="N170 Change Gaze x Emotion")

ezPlot(Anova_170_Change_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Pic_type),
       x = Gaze, 
       split= Pic_type,
       x_lab="EPN Start Gze x Stimuli")

ezPlot(Anova_170_Change_hemisphere, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Hemisphere),
       x = Hemisphere, 
       split=Gaze,
       x_lab="N170 Change Gaze x Hemisphere")

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

### 200-250 ### 
Anova_EPN_Change_200.250 = data.frame(Anova_EPN_Change_200.250) #Change accordingly 
head(Anova_EPN_Change_200.250)
hist(Anova_EPN_Change_200.250 $Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Change_200.250 $Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
boxplot(Anova_EPN_Change_200.250 $Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Change_200.250$Subject      <- factor(Anova_EPN_Change_200.250$Subject,     levels = c( 1:114)) 
Anova_EPN_Change_200.250$Subject_type <- factor(Anova_EPN_Change_200.250$Subject_type,levels = c( 1:2)) 
Anova_EPN_Change_200.250$Gaze         <- factor(Anova_EPN_Change_200.250$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Change_200.250$Emotion      <- factor(Anova_EPN_Change_200.250$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Change_200.250$Pic_type     <- factor(Anova_EPN_Change_200.250$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Change_200.250$Electrode    <- factor(Anova_EPN_Change_200.250$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

summary(Anova_EPN_Change_200.250)

EPN1sAnova <- ezANOVA(Anova_EPN_Change_200.250, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

EPN1cAnova <- ezANOVA(Anova_EPN_Change_200.250, 
                       dv = Amplitude, 
                       wid = Subject, 
                       between = Subject_type, 
                       within  = .(Pic_type, Emotion, Gaze), 
                       detailed = TRUE)
EPN1cAnova


TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_200.250$Amplitude~Anova_EPN_Change_200.250$Emotion)), data=Anova_EPN_Change_200.250, conf.level=0.95)
print(TUKEYM1)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_200.250$Amplitude~Anova_EPN_Change_200.250$Pic_type:Anova_EPN_Change_200.250$Hemisphere)), data=Anova_EPN_Change_200.250, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(EPN1cAnova,
                                   filename="Anova_EPN_Change_200.250.doc")


#Labeling for Stimulus x Gaze interaction
Anova_EPN_Change_200.250$Gaze     <- factor(Anova_EPN_Change_200.250$Gaze,         levels = 1:2, labels = c('Av>D', 'D<AV')) #'A-F', 'F_A'
Anova_EPN_Change_200.250$Pic_type <- factor(Anova_EPN_Change_200.250$Pic_type,     levels = 1:2, labels = c('Adult','Baby'))

table(Anova_EPN_Change_200.250$Gaze)
table(Anova_EPN_Change_200.250$Pic_type)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_200.250$Amplitude~Anova_EPN_Change_200.250$Gaze:Anova_EPN_Change_200.250$Pic_type)), data=Anova_EPN_Change_200.250, conf.level=0.95)
print(TUKEYM1)

# Gaze 1, Averted to Direct 2, Direct to Averted
mean_D= mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Pic_type==2])
mean_Ba

SD_Ad=sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Pic_type==1])
SD_Ad
SD_Ba=sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Pic_type==2])
SD_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Subject_type==1])
SD_Mo
SD_Nm = sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Subject_type==2])
SD_Nm

Anova_EPN_Change_200.250$Subject_type <-  as.factor(Anova_EPN_Change_200.250$Subject_type)
Anova_EPN_Change_200.250$Amplitude    <- as.numeric(Anova_EPN_Change_200.250$Amplitude)
Anova_EPN_Change_200.250$Pic_type     <-  as.factor(Anova_EPN_Change_200.250$Pic_type)
Anova_EPN_Change_200.250$Emotion      <-  as.factor(Anova_EPN_Change_200.250$Emotion)
Anova_EPN_Change_200.250$Gaze         <-  as.factor(Anova_EPN_Change_200.250$Gaze)

filtered <- Anova_EPN_Change_200.250 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
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

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Change_200.250, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Change_200.250, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2g

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3g

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Averted to Direct","Direct to Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Averted to Direct","Direct to Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Change_200.250, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy"))

## Plots
ezPlot(Anova_EPN_Change_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Change Emotion")

ezPlot(Anova_EPN_Change_200.250, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Change Gaze")

ezPlot(Anova_EPN_Change_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Change Gaze x Emotion")

ezPlot(Anova_EPN_Change_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Pic_type),
       x = Gaze, 
       split= Pic_type,
       x_lab="EPN Change Gaze x Stimuli")

ezPlot(Anova_EPN_Change_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Pic_type),
       x = Emotion, 
       split= Pic_type,
       x_lab="EPN Change Emotion x Stimuli")

ezPlot(Anova_pa_EPN_Change_200.250, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(200-250) Change Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_pa_EPN_Start_200.250$Amplitude,Anova_pa_EPN_Start_200.250$Emotion, p.adj="bonferroni", paired=T)

### 250-300 ###
Anova_EPN_Change_250.300 = data.frame(Anova_EPN_Change_250.300) #Change accordingly 
head(Anova_EPN_Change_250.300)
hist(Anova_EPN_Change_250.300 $Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Change_250.300 $Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
boxplot(Anova_EPN_Change_250.300 $Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Change_250.300$Subject      <- factor(Anova_EPN_Change_250.300$Subject,     levels = c( 1:114)) 
Anova_EPN_Change_250.300$Subject_type <- factor(Anova_EPN_Change_250.300$Subject_type,levels = c( 1:2)) 
Anova_EPN_Change_250.300$Gaze         <- factor(Anova_EPN_Change_250.300$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Change_250.300$Emotion      <- factor(Anova_EPN_Change_250.300$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Change_250.300$Pic_type     <- factor(Anova_EPN_Change_250.300$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Change_250.300$Electrode    <- factor(Anova_EPN_Change_250.300$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

EPN1sAnova <- ezANOVA(Anova_EPN_Change_250.300, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

summary(Anova_EPN_Change_250.300)

EPN2cAnova <- ezANOVA(Anova_EPN_Change_250.300, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
EPN2cAnova

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_250.300$Amplitude~Anova_EPN_Change_250.300$Emotion)), data=Anova_EPN_Change_250.300, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(EPN2cAnova,
                                   filename="Anova_EPN_Change_250.300.doc")

# Gaze
mean_D= mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Pic_type==2])
mean_Ba

SD_Ad=sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Pic_type==1])
SD_Ad
SD_Ba=sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Pic_type==2])
SD_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Subject_type==2])
mean_Nm

mean_Mo= sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Subject_type==1])
mean_Mo
mean_Nm = sd(Anova_EPN_Change_250.300$Amplitude[Anova_EPN_Change_250.300$Subject_type==2])
mean_Nm

Anova_EPN_Change_250.300$Subject_type <-  as.factor(Anova_EPN_Change_250.300$Subject_type)
Anova_EPN_Change_250.300$Amplitude    <- as.numeric(Anova_EPN_Change_250.300$Amplitude)
Anova_EPN_Change_250.300$Pic_type     <-  as.factor(Anova_EPN_Change_250.300$Pic_type)
Anova_EPN_Change_250.300$Emotion      <-  as.factor(Anova_EPN_Change_250.300$Emotion)
Anova_EPN_Change_250.300$Gaze         <-  as.factor(Anova_EPN_Change_250.300$Gaze)

filtered <- Anova_EPN_Change_250.300 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
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

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Change_250.300, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Change_250.300, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2g

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3g

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Averted to Direct", "Direct to Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Averted to Direct", "Direct to Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Change_250.300, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy"))

## Plots
ezPlot(Anova_EPN_Change_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Change Emotion")

ezPlot(Anova_EPN_Change_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Pic_type),
       x = Gaze, 
       split= Pic_type,
       x_lab="EPN Change Gaze x Stimuli")

ezPlot(Anova_EPN_Change_250.300, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Change Gaze")

ezPlot(Anova_EPN_Change_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Change Gaze x Emotion")

ezPlot(Anova_EPN_Change_250.300, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(250-300) Change Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_EPN_Start_250.300$Amplitude,Anova_EPN_Start_250.300$Emotion, p.adj="bonferroni", paired=T)

### 300-350 ###
Anova_EPN_Change_300.350 = data.frame(Anova_EPN_Change_300.350) #Change accordingly 
head(Anova_EPN_Change_300.350)
hist(Anova_EPN_Change_300.350 $Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Change_300.350 $Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
boxplot(Anova_EPN_Change_300.350 $Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Change_300.350$Subject      <- factor(Anova_EPN_Change_300.350$Subject,     levels = c( 1:114)) 
Anova_EPN_Change_300.350$Subject_type <- factor(Anova_EPN_Change_300.350$Subject_type,levels = c( 1:2)) 
Anova_EPN_Change_300.350$Gaze         <- factor(Anova_EPN_Change_300.350$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Change_300.350$Emotion      <- factor(Anova_EPN_Change_300.350$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Change_300.350$Pic_type     <- factor(Anova_EPN_Change_300.350$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Change_300.350$Electrode    <- factor(Anova_EPN_Change_300.350$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

EPN1sAnova <- ezANOVA(Anova_EPN_Change_300.350, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

summary(Anova_EPN_Change_300.350)

EPN3cAnova <- ezANOVA(Anova_EPN_Change_300.350, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
EPN3cAnova

#Genral Tukeys
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_300.350$Amplitude~Anova_EPN_Change_300.350$Emotion)), data=Anova_EPN_Change_300.350, conf.level=0.95)
print(TUKEYM1)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_300.350$Amplitude~Anova_EPN_Change_300.350$Gaze:Anova_EPN_Change_300.350$Emotion:Anova_EPN_Change_300.350$Pic_type)), data=Anova_EPN_Change_300.350, conf.level=0.95)
print(TUKEYM1)

#Labeling for Stimulus x Emotion interaction
Anova_EPN_Change_300.350$Emotion  <- factor(Anova_EPN_Change_300.350$Emotion,      levels = 1:3, labels = c('Neutral','Angry','Happy'))
Anova_EPN_Change_300.350$Pic_type <- factor(Anova_EPN_Change_300.350$Pic_type,     levels = 1:2, labels = c('Adult','Baby'))

table(Anova_EPN_Change_300.350$Emotion)
table(Anova_EPN_Change_300.350$Pic_type)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_300.350$Amplitude~Anova_EPN_Change_300.350$Emotion:Anova_EPN_Change_300.350$Pic_type)), data=Anova_EPN_Change_300.350, conf.level=0.95)
print(TUKEYM1)

#Filtering
Anova_EPN_Change_300.350_Ad = Anova_EPN_Change_300.350 [Anova_EPN_Change_300.350$Pic_type==1,] #Filtering based on the Stimulus Type
Anova_EPN_Change_300.350_Ba = Anova_EPN_Change_300.350 [Anova_EPN_Change_300.350$Pic_type==2,] #Filtering based on the Stimulus Type

table(Anova_EPN_Change_300.350_Ad$Pic_type)
table(Anova_EPN_Change_300.350_Ad$Gaze)
table(Anova_EPN_Change_300.350_Ad$Emotion)

#Labeling
Anova_EPN_Change_300.350_Ad$Gaze         <- factor(Anova_EPN_Change_300.350_Ad$Gaze,         levels = 1:2, labels = c('Av>D', 'D<AV')) #'A-F', 'F_A'
Anova_EPN_Change_300.350_Ad$Emotion      <- factor(Anova_EPN_Change_300.350_Ad$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Change_300.350_Ad$Subject_type <- factor(Anova_EPN_Change_300.350_Ad$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
#Anova_EPN_Change_300.350_Ad$Pic_type     <- factor(Anova_EPN_Change_300.350_Ad$Pic_type,     levels = 1:1, labels = c('Adult'))

# Mini _Tukeys for the obtained interaction of E x S x G
#Adult
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_300.350_Ad$Amplitude~Anova_EPN_Change_300.350_Ad$Gaze:Anova_EPN_Change_300.350_Ad$Emotion)), data=Anova_EPN_Change_300.350_Ad, conf.level=0.95)
print(TUKEYM1)

#Baby
Anova_EPN_Change_300.350_Ba$Gaze         <- factor(Anova_EPN_Change_300.350_Ba$Gaze,         levels = 1:2, labels = c('Av>D', 'D<AV')) #'A-F', 'F_A'
Anova_EPN_Change_300.350_Ba$Emotion      <- factor(Anova_EPN_Change_300.350_Ba$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Change_300.350_Ba$Subject_type <- factor(Anova_EPN_Change_300.350_Ba$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_300.350_Ba$Amplitude~Anova_EPN_Change_300.350_Ba$Gaze:Anova_EPN_Change_300.350_Ba$Emotion)), data=Anova_EPN_Change_300.350_Ba, conf.level=0.95)
print(TUKEYM1)

goggles_table <- apa.ezANOVA.table(EPN3cAnova,
                                   filename="Anova_EPN_Change_300.350.doc")

# Gaze
mean_D= mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Pic_type==2])
mean_Ba

mean_Ad = sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Pic_type==1])
mean_Ad
mean_Ba = sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Pic_type==2])
mean_Ba

SD_Ad=sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Pic_type==1])
SD_Ad
SD_Ba=sd(Anova_EPN_Change_200.250$Amplitude[Anova_EPN_Change_200.250$Pic_type==2])
SD_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Subject_type==1])
SD_Mo
SD_Nm = sd(Anova_EPN_Change_300.350$Amplitude[Anova_EPN_Change_300.350$Subject_type==2])
SD_Nm

Anova_EPN_Change_300.350$Subject_type <-  as.factor(Anova_EPN_Change_300.350$Subject_type)
Anova_EPN_Change_300.350$Amplitude    <- as.numeric(Anova_EPN_Change_300.350$Amplitude)
Anova_EPN_Change_300.350$Pic_type     <-  as.factor(Anova_EPN_Change_300.350$Pic_type)
Anova_EPN_Change_300.350$Emotion      <-  as.factor(Anova_EPN_Change_300.350$Emotion)
Anova_EPN_Change_300.350$Gaze         <-  as.factor(Anova_EPN_Change_300.350$Gaze)

filtered <- Anova_EPN_Change_300.350 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
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

# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Change_300.350, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Change_300.350, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2g

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3g

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Averted to Direct","Direct to Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Averted to Direct","Direct to Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Change_300.350, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy"))


## Plots
ezPlot(Anova_EPN_Change_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Change Emotion")

ezPlot(Anova_EPN_Change_300.350, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Change Gaze")

ezPlot(Anova_EPN_Change_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Change Gaze x Emotion")

ezPlot(Anova_EPN_Change_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Pic_type),
       x = Gaze, 
       split= Pic_type,
       x_lab="EPN Change Gaze x Stimuli")

ezPlot(Anova_EPN_Change_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Pic_type),
       x = Emotion, 
       split= Pic_type,
       x_lab="EPN Change Emotion x Stimuli")

ezPlot(Anova_EPN_Change_300.350, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(300-350) Change Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_EPN_Start_300.350$Amplitude,Anova_EPN_Start_300.350$Emotion, p.adj="bonferroni", paired=T)

### 350-400 ###
Anova_EPN_Change_350.400 = data.frame(Anova_EPN_Change_350.400) #Change accordingly 
head(Anova_EPN_Change_350.400)
hist(Anova_EPN_Change_350.400 $Amplitude, main    = 'Histogram of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
plot(Anova_EPN_Change_350.400 $Amplitude, main    = 'Scatterplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')
boxplot(Anova_EPN_Change_350.400 $Amplitude, main = 'Boxplot of Amplitude', xlab = 'Amplitude (MV)', ylab='Frequency')

Anova_EPN_Change_350.400$Subject      <- factor(Anova_EPN_Change_350.400$Subject,     levels = c( 1:114)) 
Anova_EPN_Change_350.400$Subject_type <- factor(Anova_EPN_Change_350.400$Subject_type,levels = c( 1:2)) 
Anova_EPN_Change_350.400$Gaze         <- factor(Anova_EPN_Change_350.400$Gaze,        levels = c( 1:2 )) 
Anova_EPN_Change_350.400$Emotion      <- factor(Anova_EPN_Change_350.400$Emotion,     levels = c( 1:3 )) 
Anova_EPN_Change_350.400$Pic_type     <- factor(Anova_EPN_Change_350.400$Pic_type,    levels = c( 1:2 )) 
Anova_EPN_Change_350.400$Electrode    <- factor(Anova_EPN_Change_350.400$Electrode,   levels = c( 30,34,35,38,37,39,36,32,33,26 )) 

summary(Anova_EPN_Change_350.400)

EPN1sAnova <- ezANOVA(Anova_EPN_Change_350.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
EPN1sAnova

EPN4cAnova <- ezANOVA(Anova_EPN_Change_350.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
EPN4cAnova

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_350.400$Amplitude~Anova_EPN_Change_350.400$Emotion)), data=Anova_EPN_Change_350.400, conf.level=0.95)
print(TUKEYM1)

#Labeling for Stimulus x Emotion interaction
Anova_EPN_Change_350.400$Emotion  <- factor(Anova_EPN_Change_350.400$Emotion,      levels = 1:3, labels = c('Neutral','Angry','Happy'))
Anova_EPN_Change_350.400$Pic_type <- factor(Anova_EPN_Change_350.400$Pic_type,     levels = 1:2, labels = c('Adult','Baby'))

table(Anova_EPN_Change_350.400$Emotion)
table(Anova_EPN_Change_350.400$Pic_type)

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_350.400$Amplitude~Anova_EPN_Change_350.400$Emotion:Anova_EPN_Change_350.400$Pic_type)), data=Anova_EPN_Change_350.400, conf.level=0.95)
print(TUKEYM1)

#Filtering
Anova_EPN_Change_350.400_Ad = Anova_EPN_Change_350.400 [Anova_EPN_Change_350.400$Pic_type==1,] #Filtering based on the Stimulus Type

table(Anova_EPN_Change_350.400_Ad$Pic_type)
table(Anova_EPN_Change_350.400_Ad$Gaze)
table(Anova_EPN_Change_350.400_Ad$Emotion)

#Labeling
Anova_EPN_Change_350.400_Ad$Gaze         <- factor(Anova_EPN_Change_350.400_Ad$Gaze,         levels = 1:2, labels = c('Av>D', 'D<AV')) #'A-F', 'F_A'
Anova_EPN_Change_350.400_Ad$Emotion      <- factor(Anova_EPN_Change_350.400_Ad$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Change_350.400_Ad$Subject_type <- factor(Anova_EPN_Change_350.400_Ad$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))
#Anova_EPN_Change_300.350_Ad$Pic_type     <- factor(Anova_EPN_Change_300.350_Ad$Pic_type,     levels = 1:1, labels = c('Adult'))

# Mini _Tukeys for the obtained interaction of E x S x G
#Adult
TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_350.400_Ad$Amplitude~Anova_EPN_Change_350.400_Ad$Gaze:Anova_EPN_Change_350.400_Ad$Emotion)), data=Anova_EPN_Change_350.400_Ad, conf.level=0.95)
print(TUKEYM1)

#Baby

Anova_EPN_Change_350.400_Ba = Anova_EPN_Change_350.400 [Anova_EPN_Change_350.400$Pic_type==2,] #Filtering based on the Stimulus Type

Anova_EPN_Change_350.400_Ba$Gaze         <- factor(Anova_EPN_Change_350.400_Ba$Gaze,         levels = 1:2, labels = c('Av>D', 'D<AV')) #'A-F', 'F_A'
Anova_EPN_Change_350.400_Ba$Emotion      <- factor(Anova_EPN_Change_350.400_Ba$Emotion,      levels = 1:3, labels = c('Neutral',"Angry",'Happy'))
Anova_EPN_Change_350.400_Ba$Subject_type <- factor(Anova_EPN_Change_350.400_Ba$Subject_type, levels = 1:2, labels = c('Mother',"N-Mother"))

TUKEYM1 <- TukeyHSD(x=aov(lm(Anova_EPN_Change_350.400_Ba$Amplitude~Anova_EPN_Change_350.400_Ba$Gaze:Anova_EPN_Change_350.400_Ba$Emotion)), data=Anova_EPN_Change_300.350_Ba, conf.level=0.95)
print(TUKEYM1)

#Table Export
goggles_table <- apa.ezANOVA.table(EPN4cAnova,
                                   filename="Anova_EPN_Change_350.400.doc")

# Gaze
mean_D= mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Gaze==1])
mean_D
mean_a = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Emotion==1])
mean_Nu
mean_An = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Emotion==2])
mean_An
mean_Ha = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Emotion==3])
mean_Ha

SD_Nu = sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Emotion==1])
SD_Nu
SD_An = sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Emotion==2])
SD_An
SD_Ha = sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Emotion==3])
SD_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Pic_type==2])
mean_Ba

SD_Ad = sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Pic_type==1])
SD_Ad
SD_Ba = sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Pic_type==2])
SD_Ba

# SubjType
mean_Mo= mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Subject_type==1])
mean_Mo
mean_Nm = mean(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Subject_type==2])
mean_Nm

SD_Mo= sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Subject_type==1])
SD_Mo
SD_Nm = sd(Anova_EPN_Change_350.400$Amplitude[Anova_EPN_Change_350.400$Subject_type==2])
SD_Nm

Anova_EPN_Change_350.400$Subject_type <-  as.factor(Anova_EPN_Change_350.400$Subject_type)
Anova_EPN_Change_350.400$Amplitude    <- as.numeric(Anova_EPN_Change_350.400$Amplitude)
Anova_EPN_Change_350.400$Pic_type     <-  as.factor(Anova_EPN_Change_350.400$Pic_type)
Anova_EPN_Change_350.400$Emotion      <-  as.factor(Anova_EPN_Change_350.400$Emotion)
Anova_EPN_Change_350.400$Gaze         <-  as.factor(Anova_EPN_Change_350.400$Gaze)

filtered <- Anova_EPN_Change_350.400 %>% group_by(Emotion, Subject) %>% summarise(means=mean(Amplitude))
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


# Viuallization for better undrestanding of the data
# boxplot
#Pic_ype
p1p <- ggplot(Anova_EPN_Change_350.400, aes(x = Pic_type, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2p = p1p + labs(color = "Groups", x = "Simuli")
p2p

p3p = p2p + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3p

p3p = p2p + scale_fill_manual(x ="Stimuli", labels = x("Adult", "Baby"))
p3p
p3p + scale_x_discrete(name ="Simuli", labels=c("Adult","Baby"))

# Gaze
p1g <- ggplot(Anova_EPN_Change_350.400, aes(x = Gaze, y = Amplitude, color = Subject_type)) +
  geom_boxplot()
p2g = p1g + labs(color = "Groups", x = "Gaze")
p2g

p3g = p2g + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3g

p3g = p2g + scale_fill_manual(x = "Gaze", labels = x("Averted to Direct", "Direct to Averted"))
p3g
p3g + scale_x_discrete(name ="Gaze", labels=c("Averted to Direct", "Direct to Averted"))

#Emotion
p1e <- ggplot(Anova_EPN_Change_350.400, aes(x = Emotion, y = Amplitude, color = Subject_type )) +
  geom_boxplot() 

p2e = p1e + labs(color = "Groups", x = "Emotion")
p2e

p3e = p2e + scale_color_manual(labels = c("Mother", "Non-Mother"), values = c("red", "blue"))
p3e

p3e = p2e + scale_fill_manual(x = "Emotion", labels = x("Neutral", "Angry", "Happy"))
p3e
p3e + scale_x_discrete(name ="Emotion", labels=c("Neutral", "Angry", "Happy"))

## Plots
ezPlot(Anova_EPN_Change_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="EPN Change Emotion")

ezPlot(Anova_EPN_Change_350.400, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="EPN Change Gaze")

ezPlot(Anova_EPN_Change_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="EPN Change Gaze x Emotion")

ezPlot(Anova_EPN_Change_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Pic_type),
       x = Emotion, 
       split= Pic_type,
       x_lab="EPN Change Emotion x Stimuli")

ezPlot(Anova_EPN_Change_350.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="EPN(350.400) Change Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_EPN_Start_350.400$Amplitude,Anova_EPN_Start_350.400$Emotion, p.adj="bonferroni", paired=T)










#################################################################
############# LPC-Change #################

### 300-400 ###
Anova_LPC_Change_300.400$Subject      <- factor(Anova_LPC_Change_300.400$Subject,     levels = c( 1:114)) 
Anova_LPC_Change_300.400$Subject_type <- factor(Anova_LPC_Change_300.400$Subject_type,levels = c( 0:1)) 
Anova_LPC_Change_300.400$Gaze         <- factor(Anova_LPC_Change_300.400$Gaze,        levels = c( 1:2 )) 
Anova_LPC_Change_300.400$Emotion      <- factor(Anova_LPC_Change_300.400$Emotion,     levels = c( 1:3 )) 
Anova_LPC_Change_300.400$Pic_type     <- factor(Anova_LPC_Change_300.400$Pic_type,    levels = c( 1:2 )) 
Anova_LPC_Change_300.400$Electrode    <- factor(Anova_LPC_Change_300.400$Electrode,   levels = c( 22,23,27,28,29,18)) 

LPC1sAnova <- ezANOVA(Anova_LPC_Change_300.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
LPC1sAnova

LPC1sAnova <- ezANOVA(Anova_LPC_Change_300.400, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
LPC1sAnova

# To Look at the interaction Pic_type X Emotion X Gaze
Anova_LPC_Change_400.500_Mother = Anova_LPC_Change_400.500 [Anova_LPC_Change_400.500$Subject_type==0,] #Filtering based on the Subject_type Factor (mothers)
Anova_LPC_Change_400.500_NMother = Anova_LPC_Change_400.500 [Anova_LPC_Change_400.500$Subject_type==1,] #Filtering based on the Subject_type Factor (N-mothers)

table(Anova_LPC_Change_400.500_Mother$Subject_type)
table(Anova_LPC_Change_400.500_NMother)
head(Anova_LPC_Change_400.500_Mother)
head(Anova_LPC_Change_400.500_NMother)


goggles_table <- apa.ezANOVA.table(LPC1sAnova,
                                   filename="Anova_LPC_Change_300.400.doc")


## Gaze
mean_D= mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Gaze==1])
mean_D
mean_a = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Emotion==1])
mean_Nu
mean_An = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Emotion==2])
mean_An
mean_Ha = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Emotion==3])
mean_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Subject_type==0])
mean_Mo
mean_Nm = mean(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Subject_type==1])
mean_Nm

SD_Mo= sd(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Subject_type==0])
SD_Mo
SD_Nm = sd(Anova_LPC_Change_300.400$Amplitude[Anova_LPC_Change_300.400$Subject_type==1])
SD_Nm


## Plots
ezPlot(Anova_LPC_Change_300.400, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="LPC Change Emotion")

ezPlot(Anova_LPC_Change_300.400, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="LPC Change Gaze")

ezPlot(Anova_LPC_Change_300.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="LPC Change Gaze x Emotion")

ezPlot(Anova_LPC_Change_300.400, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="LPC (300.400) Change Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_LPC_Change_300.400$Amplitude,Anova_LPC_Change_300.400$Emotion, p.adj="bonferroni", paired=T)

### 400-500 ###

Anova_LPC_Change_400.500$Subject      <- factor(Anova_LPC_Change_400.500$Subject,     levels = c( 1:114)) 
Anova_LPC_Change_400.500$Subject_type <- factor(Anova_LPC_Change_400.500$Subject_type,levels = c( 0:1)) 
Anova_LPC_Change_400.500$Gaze         <- factor(Anova_LPC_Change_400.500$Gaze,        levels = c( 1:2 )) 
Anova_LPC_Change_400.500$Emotion      <- factor(Anova_LPC_Change_400.500$Emotion,     levels = c( 1:3 )) 
Anova_LPC_Change_400.500$Pic_type     <- factor(Anova_LPC_Change_400.500$Pic_type,    levels = c( 1:2 )) 
Anova_LPC_Change_400.500$Electrode    <- factor(Anova_LPC_Change_400.500$Electrode,   levels = c( 22,23,27,28,29,18)) 

LPC1sAnova <- ezANOVA(Anova_LPC_Change_400.500, 
                      dv = Amplitude, 
                      wid = Subject, 
                      within = .(Emotion, Gaze, Electrode), 
                      detailed = TRUE)
LPC1sAnova

LPC1sAnova <- ezANOVA(Anova_LPC_Change_400.500, 
                      dv = Amplitude, 
                      wid = Subject, 
                      between = Subject_type, 
                      within  = .(Pic_type, Emotion, Gaze), 
                      detailed = TRUE)
LPC1sAnova

goggles_table <- apa.ezANOVA.table(LPC1sAnova,
                                   filename="Anova_LPC_Change_400.500.doc")


## Gaze
mean_D= mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Gaze==1])
mean_D
mean_a = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Gaze==2])
mean_a

# Emotion 1, Neutral, 2, Angry, 3, Happy
mean_Nu = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Emotion==1])
mean_Nu
mean_An = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Emotion==2])
mean_An
mean_Ha = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Emotion==3])
mean_Ha

# PicType  1, Adult, 2, Baby
mean_Ad = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Pic_type==1])
mean_Ad
mean_Ba = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Pic_type==2])
mean_Ba

# SubjType
mean_Mo= mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Subject_type==0])
mean_Mo
mean_Nm = mean(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Subject_type==1])
mean_Nm

SD_Mo= sd(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Subject_type==0])
SD_Mo
SD_Nm = sd(Anova_LPC_Change_400.500$Amplitude[Anova_LPC_Change_400.500$Subject_type==1])
SD_Nm

## Plots
ezPlot(Anova_LPC_Change_400.500, 
       dv = Amplitude, 
       wid = Subject,
       within = Emotion,
       x = Emotion, 
       x_lab="LPC Change Emotion")

ezPlot(Anova_LPC_Change_400.500, 
       dv = Amplitude, 
       wid = Subject, 
       within = Gaze,
       x = Gaze, 
       x_lab="LPC Change Gaze")

ezPlot(Anova_LPC_Change_400.500, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Gaze,Emotion),
       x = Gaze, 
       split= Emotion,
       x_lab="LPC Change Gaze x Emotion")

ezPlot(Anova_LPC_Change_400.500, 
       dv = Amplitude, 
       wid = Subject,
       within = .(Emotion,Electrode),
       x = Electrode, 
       split=Emotion,
       x_lab="LPC (400.500) Change Emotion x Electrode")

# post-hoc t-test
pairwise.t.test(Anova_LPC_Change_400.500$Amplitude,Anova_LPC_Change_400.500$Emotion, p.adj="bonferroni", paired=T)
