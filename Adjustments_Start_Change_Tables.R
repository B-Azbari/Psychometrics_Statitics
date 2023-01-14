setwd("~/Desktop/PA_DATA/EPN/NEW/")
getwd()

rm(list = ls())

library(dplyr)
library(tidyverse)
library(tidyr)

#Start & Change
Anova_170_Start_hemisphere  = read.table("Anova_pa_170_Start_hemisphere.txt",header=TRUE)
Anova_170_Change_hemisphere = read.table("Anova_pa_170_Change_hemisphere.txt",header=TRUE)

############################### START #######################################################################
# the same as before
Anova_170_Start_hemisphere$Subject      <- factor(Anova_170_Start_hemisphere$Subject,     levels = c( 1:114)) 
Anova_170_Start_hemisphere$Subject_type <- factor(Anova_170_Start_hemisphere$Subject_type,levels = c( 1:2)) 
Anova_170_Start_hemisphere$Gaze         <- factor(Anova_170_Start_hemisphere$Gaze,        levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Emotion      <- factor(Anova_170_Start_hemisphere$Emotion,     levels = c( 1:3 )) 
Anova_170_Start_hemisphere$Pic_type     <- factor(Anova_170_Start_hemisphere$Pic_type,    levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Hemisphere   <- factor(Anova_170_Start_hemisphere$Hemisphere,  levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Electrode    <- factor(Anova_170_Start_hemisphere$Electrode,   levels = c( 30,26,33,34 )) 

Pairwise_D <- function(df, grouping_variables){
  f1 <- function(df, grouping_variables){
    nt <- levels(df[,grouping_variables[1]])
    np <- levels(df[,grouping_variables[2]])
    ne <- levels(df[,grouping_variables[3]])
    Em=c()
    for (i in 1:length(nt)) { # for Subject_type
      for (j in 1:length(np)) { # for Pic_type
        for (k in 1:length(ne)) {
          fdf <- df[
            df[,grouping_variables[1]]==nt[i] & 
              df[,grouping_variables[2]]==np[j] &
              df[,grouping_variables[3]]==ne[k], ]
          amplitudes <- fdf$Amplitude
          ndfm <- data.frame(nt[i],
                             np[j],
                             ne[k],
                             amplitudes=amplitudes)
          Em <- rbind.data.frame(Em, ndfm)
        }
      }
    }
    colnames(Em) <- c(grouping_variables, 'Amplitude')
    E <- as.data.frame(Em)
    return(data.frame(E))
  }
  s=f1(df=df, grouping_variables=grouping_variables)
  s[,1] <- as.factor(s[,1] )
  s[,2] <- as.factor(s[,2])
  s[,3] <- as.factor(s[,3])
  S <- s
  df1 <- S[S$Emotion==1,]
  df2 <- S[S$Emotion==2,]
  df3 <- S[S$Emotion==3,]
  e1 <- df1$Amplitude
  e2 <- df2$Amplitude
  e3 <- df3$Amplitude
  e21 <- df2$Amplitude-df1$Amplitude
  e31 <- df3$Amplitude-df1$Amplitude
  e32 <- df3$Amplitude-df2$Amplitude
  DF <- data.frame(S[,1], S[,2], Emotion1=e1, Emotion2=e2, Emotion3=e3, Emotion21=e21, Emotion31=e31, Emotion32=e32)
  colnames(DF)[1:2] <- grouping_variables[1:2]
  DF[,1] <- as.factor(DF[,1])
  DF[,2] <- as.factor(DF[,2])
  DF
}


SF <- Pairwise_D(df=Anova_EPN_Start_250.300, grouping_variables=c('Subject_type','Pic_type','Emotion'))
SF %>% head()
SF %>% dim()

# T-Test for Emotion 1 for different levels of Subject_type and Pic_type
d1 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion1)$statistic,
                   p_value=t.test(Emotion1)$p.value,
                   lower=t.test(Emotion1)$conf.int[1],
                   upper=t.test(Emotion1)$conf.int[2]) %>% 
  mutate(Emotion='Emotion1') %>% print()
# T-Test for Emotion 2 for different levels of Subject_type and Pic_type
d2 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion2)$statistic,
                   p_value=t.test(Emotion2)$p.value,
                   lower=t.test(Emotion2)$conf.int[1],
                   upper=t.test(Emotion2)$conf.int[2]) %>% 
  mutate(Emotion='Emotion2') %>% print()

# T-Test for Emotion 3 for different levels of Subject_type and Pic_type
d3 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion3)$statistic,
                   p_value=t.test(Emotion3)$p.value,
                   lower=t.test(Emotion3)$conf.int[1],
                   upper=t.test(Emotion3)$conf.int[2]) %>% 
  mutate(Emotion='Emotion3') %>% print()

# T-Test for Emotion 21 for different levels of Subject_type and Pic_type
d21 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion21)$statistic,
                   p_value=t.test(Emotion21)$p.value,
                   lower=t.test(Emotion21)$conf.int[1],
                   upper=t.test(Emotion21)$conf.int[2]) %>% 
  mutate(Emotion='Emotion21') %>% print()

# T-Test for Emotion 31 for different levels of Subject_type and Pic_type
d31 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion31)$statistic,
                   p_value=t.test(Emotion31)$p.value,
                   lower=t.test(Emotion31)$conf.int[1],
                   upper=t.test(Emotion31)$conf.int[2]) %>% 
  mutate(Emotion='Emotion31') %>% print()

# T-Test for Emotion 32 for different levels of Subject_type and Pic_type
d32 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion32)$statistic,
                   p_value=t.test(Emotion32)$p.value,
                   lower=t.test(Emotion32)$conf.int[1],
                   upper=t.test(Emotion32)$conf.int[2]) %>% 
  mutate(Emotion='Emotion32') %>% print()

# combine altogether
DM <- rbind.data.frame(d1,d2,d3,d21,d31,d32)
DM
write.csv(DM, file='Pa_170_Start_hemisphere')

############################### CHANGE #######################################################################

Anova_170_Change_hemisphere$Subject      <- factor(Anova_170_Change_hemisphere$Subject,     levels = c( 1:114)) 
Anova_170_Change_hemisphere$Subject_type <- factor(Anova_170_Change_hemisphere$Subject_type,levels = c( 1:2)) 
Anova_170_Change_hemisphere$Gaze         <- factor(Anova_170_Change_hemisphere$Gaze,        levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Emotion      <- factor(Anova_170_Change_hemisphere$Emotion,     levels = c( 1:3 )) 
Anova_170_Change_hemisphere$Pic_type     <- factor(Anova_170_Change_hemisphere$Pic_type,    levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Hemisphere   <- factor(Anova_170_Change_hemisphere$Hemisphere,  levels = c( 1:2 )) 
Anova_170_Change_hemisphere$Electrode    <- factor(Anova_170_Change_hemisphere$Electrode,   levels = c( 30,26,33,34 )) 

Anova_170_Start_hemisphere$Subject      <- factor(Anova_170_Start_hemisphere$Subject,     levels = c( 1:114)) 
Anova_170_Start_hemisphere$Subject_type <- factor(Anova_170_Start_hemisphere$Subject_type,levels = c( 1:2)) 
Anova_170_Start_hemisphere$Gaze         <- factor(Anova_170_Start_hemisphere$Gaze,        levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Emotion      <- factor(Anova_170_Start_hemisphere$Emotion,     levels = c( 1:3 )) 
Anova_170_Start_hemisphere$Pic_type     <- factor(Anova_170_Start_hemisphere$Pic_type,    levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Hemisphere   <- factor(Anova_170_Start_hemisphere$Hemisphere,  levels = c( 1:2 )) 
Anova_170_Start_hemisphere$Electrode    <- factor(Anova_170_Start_hemisphere$Electrode,   levels = c( 30,26,33,34 )) 

Pairwise_D <- function(df, grouping_variables){
  f1 <- function(df, grouping_variables){
    nt <- levels(df[,grouping_variables[1]])
    np <- levels(df[,grouping_variables[2]])
    ne <- levels(df[,grouping_variables[3]])
    Em=c()
    for (i in 1:length(nt)) { # for Subject_type
      for (j in 1:length(np)) { # for Pic_type
        for (k in 1:length(ne)) {
          fdf <- df[
            df[,grouping_variables[1]]==nt[i] & 
              df[,grouping_variables[2]]==np[j] &
              df[,grouping_variables[3]]==ne[k], ]
          amplitudes <- fdf$Amplitude
          ndfm <- data.frame(nt[i],
                             np[j],
                             ne[k],
                             amplitudes=amplitudes)
          Em <- rbind.data.frame(Em, ndfm)
        }
      }
    }
    colnames(Em) <- c(grouping_variables, 'Amplitude')
    E <- as.data.frame(Em)
    return(data.frame(E))
  }
  s=f1(df=df, grouping_variables=grouping_variables)
  s[,1] <- as.factor(s[,1] )
  s[,2] <- as.factor(s[,2])
  s[,3] <- as.factor(s[,3])
  S <- s
  df1 <- S[S$Emotion==1,]
  df2 <- S[S$Emotion==2,]
  df3 <- S[S$Emotion==3,]
  e1 <- df1$Amplitude
  e2 <- df2$Amplitude
  e3 <- df3$Amplitude
  e21 <- df2$Amplitude-df1$Amplitude
  e31 <- df3$Amplitude-df1$Amplitude
  e32 <- df3$Amplitude-df2$Amplitude
  DF <- data.frame(S[,1], S[,2], Emotion1=e1, Emotion2=e2, Emotion3=e3, Emotion21=e21, Emotion31=e31, Emotion32=e32)
  colnames(DF)[1:2] <- grouping_variables[1:2]
  DF[,1] <- as.factor(DF[,1])
  DF[,2] <- as.factor(DF[,2])
  DF
}


SF <- Pairwise_D(df=Anova_EPN_Start_250.300, grouping_variables=c('Subject_type','Pic_type','Emotion'))
SF %>% head()
SF %>% dim()

# T-Test for Emotion 1 for different levels of Subject_type and Pic_type
d1 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion1)$statistic,
                   p_value=t.test(Emotion1)$p.value,
                   lower=t.test(Emotion1)$conf.int[1],
                   upper=t.test(Emotion1)$conf.int[2]) %>% 
  mutate(Emotion='Emotion1') %>% print()
# T-Test for Emotion 2 for different levels of Subject_type and Pic_type
d2 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion2)$statistic,
                   p_value=t.test(Emotion2)$p.value,
                   lower=t.test(Emotion2)$conf.int[1],
                   upper=t.test(Emotion2)$conf.int[2]) %>% 
  mutate(Emotion='Emotion2') %>% print()

# T-Test for Emotion 3 for different levels of Subject_type and Pic_type
d3 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion3)$statistic,
                   p_value=t.test(Emotion3)$p.value,
                   lower=t.test(Emotion3)$conf.int[1],
                   upper=t.test(Emotion3)$conf.int[2]) %>% 
  mutate(Emotion='Emotion3') %>% print()

# T-Test for Emotion 21 for different levels of Subject_type and Pic_type
d21 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion21)$statistic,
                   p_value=t.test(Emotion21)$p.value,
                   lower=t.test(Emotion21)$conf.int[1],
                   upper=t.test(Emotion21)$conf.int[2]) %>% 
  mutate(Emotion='Emotion21') %>% print()

# T-Test for Emotion 31 for different levels of Subject_type and Pic_type
d31 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion31)$statistic,
                   p_value=t.test(Emotion31)$p.value,
                   lower=t.test(Emotion31)$conf.int[1],
                   upper=t.test(Emotion31)$conf.int[2]) %>% 
  mutate(Emotion='Emotion31') %>% print()

# T-Test for Emotion 32 for different levels of Subject_type and Pic_type
d32 <- SF %>%
  dplyr::group_by(Subject_type,Pic_type) %>% 
  dplyr::summarise(t_statistic=t.test(Emotion32)$statistic,
                   p_value=t.test(Emotion32)$p.value,
                   lower=t.test(Emotion32)$conf.int[1],
                   upper=t.test(Emotion32)$conf.int[2]) %>% 
  mutate(Emotion='Emotion32') %>% print()

# combine altogether
DM <- rbind.data.frame(d1,d2,d3,d21,d31,d32)
DM
write.csv(DM, file='Pa_170_Change_hemisphere.csv')


