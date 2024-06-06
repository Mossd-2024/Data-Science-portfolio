##################################################
#
# R code to data analysis 'Exploring affect regulation of marijuana use in everyday life using Specification Curve Analysis'
# written by Jonas Dora
# email: jdora@uw.edu
# please email me directly if you see any errors or have any questions
# last update: 3/25/2022
#
##################################################
#
# Outline:
# 
# 1) loading library packages
# 2) setting generic options, reading in data, calculating and standardizing variables
# 3) SCA
# 4) permutation test
# 5) plots
#
##################################################
#
# 1) loading library packages
#
library(tidyverse)
library(lme4)
library(tidyr)
library(cowplot)
#
##################################################
#
# 2) setting generic options, reading in data, calculating and standardizing variables
#
#
# penalize scientific notation
#
options(scipen = 999)
options(mc.cores = parallel::detectCores())

#
# reading in  data
#
data.averaged.averaged <- read.csv("/mmfs1/home/jdora/data.averaged.averaged.csv")
data.averaged.median  <- read.csv("/mmfs1/home/jdora/data.averaged.median.csv")
data.immediately.averaged <- read.csv("/mmfs1/home/jdora/data.immediately.averaged.csv")
data.immediately.median <- read.csv("/mmfs1/home/jdora/data.immediately.median.csv")
#
# turn PID, sex, and marijuana use into factors
#
factors <- c("PID", "sex")
data.averaged.averaged[factors] <- lapply(data.averaged.averaged[factors], factor)
data.averaged.median[factors] <- lapply(data.averaged.median[factors], factor)
data.immediately.averaged[factors] <- lapply(data.immediately.averaged[factors], factor)
data.immediately.median[factors] <- lapply(data.immediately.median[factors], factor)
data.averaged.averaged$mar.binary <- factor(data.averaged.averaged$mar.binary, levels = c("0", "1"))
data.averaged.median$mar.binary <- factor(data.averaged.median$mar.binary, levels = c("0", "1"))
data.immediately.averaged$mar.binary <- factor(data.immediately.averaged$mar.binary, levels = c("0", "1"))
data.immediately.median$mar.binary <- factor(data.immediately.median$mar.binary, levels = c("0", "1"))
#
# calculating and standardizing variables in each of the four datasets
#
mood.negative <- c("mood.angry", "mood.anxious", "mood.irritable", "mood.unhappy", "mood.bored")
mood.positive <- c("mood.cheerful", "mood.friendly", "mood.calm", "mood.happy", "mood.engaged")
data.averaged.averaged$mood.negative <- rowMeans(data.averaged.averaged[,mood.negative])
data.averaged.averaged$mood.positive <- rowMeans(data.averaged.averaged[,mood.positive])
data.averaged.averaged[is.na(data.averaged.averaged)]<-NA
#
mood.negative.no.boredom <- c("mood.angry", "mood.anxious", "mood.irritable", "mood.unhappy")
data.averaged.averaged$mood.negative.no.boredom <- rowMeans(data.averaged.averaged[,mood.negative.no.boredom])
data.averaged.averaged[is.na(mood.negative.no.boredom)]<-NA
#
data.averaged.averaged <- data.averaged.averaged %>%
  group_by(PID) %>%
  mutate(mood.angry = (mood.angry - mean(mood.angry, na.rm=T))/sd(mood.angry, na.rm=T)) %>%
  mutate(mood.anxious = (mood.anxious - mean(mood.anxious, na.rm=T))/sd(mood.anxious, na.rm=T)) %>%
  mutate(mood.irritable = (mood.irritable - mean(mood.irritable, na.rm=T))/sd(mood.irritable, na.rm=T)) %>%
  mutate(mood.unhappy = (mood.unhappy - mean(mood.unhappy, na.rm=T))/sd(mood.unhappy, na.rm=T)) %>%
  mutate(mood.bored = (mood.bored - mean(mood.bored, na.rm=T))/sd(mood.bored, na.rm=T)) %>%
  mutate(mood.cheerful = (mood.cheerful - mean(mood.cheerful, na.rm=T))/sd(mood.cheerful, na.rm=T)) %>%
  mutate(mood.friendly = (mood.friendly - mean(mood.friendly, na.rm=T))/sd(mood.friendly, na.rm=T)) %>%
  mutate(mood.calm = (mood.calm - mean(mood.calm, na.rm=T))/sd(mood.calm, na.rm=T)) %>%
  mutate(mood.happy = (mood.happy - mean(mood.happy, na.rm=T))/sd(mood.happy, na.rm=T)) %>%
  mutate(mood.engaged = (mood.engaged - mean(mood.engaged, na.rm=T))/sd(mood.engaged, na.rm=T)) %>%
  mutate(mood.negative = (mood.negative - mean(mood.negative, na.rm=T))/sd(mood.negative, na.rm=T)) %>%
  mutate(mood.positive = (mood.positive - mean(mood.positive, na.rm=T))/sd(mood.positive, na.rm=T)) %>%
  mutate(mood.negative.no.boredom = (mood.negative.no.boredom - mean(mood.negative.no.boredom, na.rm=T))/sd(mood.negative.no.boredom, na.rm=T))
#
gmc <- c("age", "MMQ.coping", "MMQ.enhancement", "MMQ.conformity", "MMQ.social", "MMQ.expansion", "UPPS.premeditation", "UPPS.negativeurgency",
                        "UPPS.sensationseeking", "UPPS.perseverance", "UPPS.positiveurgency")
data.averaged.averaged[gmc] <- lapply(data.averaged.averaged[gmc], scale, center = T, scale = T)
#
data.averaged.median$mood.negative <- rowMeans(data.averaged.median[,mood.negative])
data.averaged.median$mood.positive <- rowMeans(data.averaged.median[,mood.positive])
data.averaged.median[is.na(data.averaged.median)]<-NA
#
data.averaged.median$mood.negative.no.boredom <- rowMeans(data.averaged.median[,mood.negative.no.boredom])
data.averaged.median[is.na(mood.negative.no.boredom)]<-NA
#
data.averaged.median <- data.averaged.median %>%
  group_by(PID) %>%
  mutate(mood.angry = (mood.angry - mean(mood.angry, na.rm=T))/sd(mood.angry, na.rm=T)) %>%
  mutate(mood.anxious = (mood.anxious - mean(mood.anxious, na.rm=T))/sd(mood.anxious, na.rm=T)) %>%
  mutate(mood.irritable = (mood.irritable - mean(mood.irritable, na.rm=T))/sd(mood.irritable, na.rm=T)) %>%
  mutate(mood.unhappy = (mood.unhappy - mean(mood.unhappy, na.rm=T))/sd(mood.unhappy, na.rm=T)) %>%
  mutate(mood.bored = (mood.bored - mean(mood.bored, na.rm=T))/sd(mood.bored, na.rm=T)) %>%
  mutate(mood.cheerful = (mood.cheerful - mean(mood.cheerful, na.rm=T))/sd(mood.cheerful, na.rm=T)) %>%
  mutate(mood.friendly = (mood.friendly - mean(mood.friendly, na.rm=T))/sd(mood.friendly, na.rm=T)) %>%
  mutate(mood.calm = (mood.calm - mean(mood.calm, na.rm=T))/sd(mood.calm, na.rm=T)) %>%
  mutate(mood.happy = (mood.happy - mean(mood.happy, na.rm=T))/sd(mood.happy, na.rm=T)) %>%
  mutate(mood.engaged = (mood.engaged - mean(mood.engaged, na.rm=T))/sd(mood.engaged, na.rm=T)) %>%
  mutate(mood.negative = (mood.negative - mean(mood.negative, na.rm=T))/sd(mood.negative, na.rm=T)) %>%
  mutate(mood.positive = (mood.positive - mean(mood.positive, na.rm=T))/sd(mood.positive, na.rm=T)) %>%
  mutate(mood.negative.no.boredom = (mood.negative.no.boredom - mean(mood.negative.no.boredom, na.rm=T))/sd(mood.negative.no.boredom, na.rm=T)) 
#
gmc <- c("age", "MMQ.coping", "MMQ.enhancement", "MMQ.conformity", "MMQ.social", "MMQ.expansion", "UPPS.premeditation", "UPPS.negativeurgency",
         "UPPS.sensationseeking", "UPPS.perseverance", "UPPS.positiveurgency")
data.averaged.median[gmc] <- lapply(data.averaged.median[gmc], scale, center = T, scale = T)
#
data.immediately.averaged$mood.negative <- rowMeans(data.immediately.averaged[,mood.negative])
data.immediately.averaged$mood.positive <- rowMeans(data.immediately.averaged[,mood.positive])
data.immediately.averaged[is.na(data.immediately.averaged)]<-NA
#
data.immediately.averaged$mood.negative.no.boredom <- rowMeans(data.immediately.averaged[,mood.negative.no.boredom])
data.immediately.averaged[is.na(mood.negative.no.boredom)]<-NA
#
data.immediately.averaged <- data.immediately.averaged %>%
  group_by(PID) %>%
  mutate(mood.angry = (mood.angry - mean(mood.angry, na.rm=T))/sd(mood.angry, na.rm=T)) %>%
  mutate(mood.anxious = (mood.anxious - mean(mood.anxious, na.rm=T))/sd(mood.anxious, na.rm=T)) %>%
  mutate(mood.irritable = (mood.irritable - mean(mood.irritable, na.rm=T))/sd(mood.irritable, na.rm=T)) %>%
  mutate(mood.unhappy = (mood.unhappy - mean(mood.unhappy, na.rm=T))/sd(mood.unhappy, na.rm=T)) %>%
  mutate(mood.bored = (mood.bored - mean(mood.bored, na.rm=T))/sd(mood.bored, na.rm=T)) %>%
  mutate(mood.cheerful = (mood.cheerful - mean(mood.cheerful, na.rm=T))/sd(mood.cheerful, na.rm=T)) %>%
  mutate(mood.friendly = (mood.friendly - mean(mood.friendly, na.rm=T))/sd(mood.friendly, na.rm=T)) %>%
  mutate(mood.calm = (mood.calm - mean(mood.calm, na.rm=T))/sd(mood.calm, na.rm=T)) %>%
  mutate(mood.happy = (mood.happy - mean(mood.happy, na.rm=T))/sd(mood.happy, na.rm=T)) %>%
  mutate(mood.engaged = (mood.engaged - mean(mood.engaged, na.rm=T))/sd(mood.engaged, na.rm=T)) %>%
  mutate(mood.negative = (mood.negative - mean(mood.negative, na.rm=T))/sd(mood.negative, na.rm=T)) %>%
  mutate(mood.positive = (mood.positive - mean(mood.positive, na.rm=T))/sd(mood.positive, na.rm=T)) %>%
  mutate(mood.negative.no.boredom = (mood.negative.no.boredom - mean(mood.negative.no.boredom, na.rm=T))/sd(mood.negative.no.boredom, na.rm=T)) 
#
gmc <- c("age", "MMQ.coping", "MMQ.enhancement", "MMQ.conformity", "MMQ.social", "MMQ.expansion", "UPPS.premeditation", "UPPS.negativeurgency",
         "UPPS.sensationseeking", "UPPS.perseverance", "UPPS.positiveurgency")
data.immediately.averaged[gmc] <- lapply(data.immediately.averaged[gmc], scale, center = T, scale = T)
#
data.immediately.median$mood.negative <- rowMeans(data.immediately.median[,mood.negative])
data.immediately.median$mood.positive <- rowMeans(data.immediately.median[,mood.positive])
data.immediately.median[is.na(data.immediately.median)]<-NA
#
data.immediately.median$mood.negative.no.boredom <- rowMeans(data.immediately.median[,mood.negative.no.boredom])
data.immediately.median[is.na(mood.negative.no.boredom)]<-NA
#
data.immediately.median <- data.immediately.median %>%
  group_by(PID) %>%
  mutate(mood.angry = (mood.angry - mean(mood.angry, na.rm=T))/sd(mood.angry, na.rm=T)) %>%
  mutate(mood.anxious = (mood.anxious - mean(mood.anxious, na.rm=T))/sd(mood.anxious, na.rm=T)) %>%
  mutate(mood.irritable = (mood.irritable - mean(mood.irritable, na.rm=T))/sd(mood.irritable, na.rm=T)) %>%
  mutate(mood.unhappy = (mood.unhappy - mean(mood.unhappy, na.rm=T))/sd(mood.unhappy, na.rm=T)) %>%
  mutate(mood.bored = (mood.bored - mean(mood.bored, na.rm=T))/sd(mood.bored, na.rm=T)) %>%
  mutate(mood.cheerful = (mood.cheerful - mean(mood.cheerful, na.rm=T))/sd(mood.cheerful, na.rm=T)) %>%
  mutate(mood.friendly = (mood.friendly - mean(mood.friendly, na.rm=T))/sd(mood.friendly, na.rm=T)) %>%
  mutate(mood.calm = (mood.calm - mean(mood.calm, na.rm=T))/sd(mood.calm, na.rm=T)) %>%
  mutate(mood.happy = (mood.happy - mean(mood.happy, na.rm=T))/sd(mood.happy, na.rm=T)) %>%
  mutate(mood.engaged = (mood.engaged - mean(mood.engaged, na.rm=T))/sd(mood.engaged, na.rm=T)) %>%
  mutate(mood.negative = (mood.negative - mean(mood.negative, na.rm=T))/sd(mood.negative, na.rm=T)) %>%
  mutate(mood.positive = (mood.positive - mean(mood.positive, na.rm=T))/sd(mood.positive, na.rm=T)) %>%
  mutate(mood.negative.no.boredom = (mood.negative.no.boredom - mean(mood.negative.no.boredom, na.rm=T))/sd(mood.negative.no.boredom, na.rm=T)) 
#
gmc <- c("age", "MMQ.coping", "MMQ.enhancement", "MMQ.conformity", "MMQ.social", "MMQ.expansion", "UPPS.premeditation", "UPPS.negativeurgency",
         "UPPS.sensationseeking", "UPPS.perseverance", "UPPS.positiveurgency")
data.immediately.median[gmc] <- lapply(data.immediately.median[gmc], scale, center = T, scale = T)
#
##################################################
#
# 3) specification curve analysis
#
# create empty frame for results to be written in
#
resultsframe.rq1.na <- function(input) {
  ### SETUP SPECIFICATION NAMES
  levels.outcome <- input
  levels.affect.variable <- c("mood.negative", "mood.negative.no.boredom", "mood.angry", "mood.anxious", "mood.irritable", "mood.unhappy", "mood.bored")
  levels.inclusion.criterion <- c("prior.use.any", "prior.use.30.days", "prior.use.30.days.high")
  levels.control.variable <- c("none", "motives.coping", "motives.enhancement", "motives.social", "upps.premeditation", "upps.negativeurgency", 
                               "upps.sensationseeking", "upps.perseverance", "upps.positiveurgency", "sex")
  levels.timing.use <- c("averaged1", "immediate")
  levels.timing.nonuse <- c("averaged2", "median")
  ### CALCULATE NUMBER OF COMBINATIONS TO RUN
  combinations <- length(levels.outcome) * length(levels.affect.variable) * length(levels.inclusion.criterion) * length(levels.control.variable) * 
    length(levels.timing.use) * length(levels.timing.nonuse)
  ### SETUP RESULTS FRAME
  results.frame <- data.frame(matrix(NA, nrow = combinations, ncol = 8))
  colnames(results.frame) <- c("outcome", "affect.variable", "inclusion.criterion", "control.variable", "timing.use", "timing.nonuse", "effect", "p.value")
  ### WRITE COMBINATIONS INTO RESULTS FRAME
  results.frame$outcome <- rep(levels.outcome, each = nrow(results.frame)/length(levels.outcome))
  
  results.frame$affect.variable <- rep(rep(levels.affect.variable, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable))),
                                       times = length(levels.outcome))
  
  results.frame$inclusion.criterion <- rep(rep(rep(levels.inclusion.criterion, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) * 
                                                                                                             length(levels.inclusion.criterion))), times = length(levels.outcome)), times = length(levels.affect.variable))
  
  results.frame$control.variable <- rep(rep(rep(rep(levels.control.variable, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                                                                                           length(levels.inclusion.criterion) * length(levels.control.variable))), times = length(levels.outcome)), 
                                            times = length(levels.affect.variable)), times = length(levels.inclusion.criterion))
  results.frame$timing.use <- rep(rep(rep(rep(rep(levels.timing.use, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                                                                                   length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.timing.use))), times = length(levels.outcome)),
                                          times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), times = length(levels.control.variable))
  results.frame$timing.nonuse <- rep(rep(rep(rep(rep(rep(levels.timing.nonuse, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                                                                                             length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.timing.use) * length(levels.timing.nonuse))),
                                                     times = length(levels.outcome)), times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), 
                                         times = length(levels.control.variable)), times = length(levels.timing.use))
  
  return(results.frame)
}
#
# fill empty frame with results
#
curve.rq1.na <- function(input) {
  results.frame <- input
  affectvar <- NA
  model <- NA
  nullmodel <- NA
  data <- NA
  for (n in 1:nrow(results.frame)) {
    print(n) 
    ### DEFINE APPROPRIATE DATASET
    data <- data.frame(data)
    if (results.frame$timing.use[n] == "averaged1" & results.frame$timing.nonuse[n] == "averaged2") {
      data <- data.frame(data.averaged.averaged)
    }
    else if (results.frame$timing.use[n] == "averaged1" & results.frame$timing.nonuse[n] == "median") {
      data <- data.frame(data.averaged.median)
    }
    else if (results.frame$timing.use[n] == "immediate" & results.frame$timing.nonuse[n] == "averaged2") {
      data <- data.frame(data.immediately.averaged)
    }    
    else if (results.frame$timing.use[n] == "immediate" & results.frame$timing.nonuse[n] == "median") {
      data <- data.frame(data.immediately.median)
    }       
    if (results.frame$inclusion.criterion[n] == "prior.use.any") {
      data <- data.frame(data[data$DMU1 == 1,])
    }
    else if (results.frame$inclusion.criterion[n] == "prior.use.30.days") {
      data <- data.frame(data[data$DMU3 > 0,])
    }
    else if (results.frame$inclusion.criterion[n] == "prior.use.30.days.high") {
      data <- data.frame(data[data$DMU4 > 0,])
    }    
    ### DEFINE AFFECT VARIABLE
    affectvar <- results.frame$affect.variable[n]
    ### REMOVE MISSING, NECESSARY FOR anova(); multiple imputation not feasible due to computational costs
    if (results.frame$affect.variable[n] == "mood.negative") {
      data <- data %>%
        drop_na(mar.binary, mood.negative)
    }
    else if (results.frame$affect.variable[n] == "mood.negative.no.boredom") {
      data <- data %>%
        drop_na(mar.binary, mood.negative.no.boredom)
    }
    else if (results.frame$affect.variable[n] == "mood.angry") {
      data <- data %>%
        drop_na(mar.binary, mood.angry)
    }
    else if (results.frame$affect.variable[n] == "mood.anxious") {
      data <- data %>%
        drop_na(mar.binary, mood.anxious)
    }
    else if (results.frame$affect.variable[n] == "mood.irritable") {
      data <- data %>%
        drop_na(mar.binary, mood.irritable)
    }
    else if (results.frame$affect.variable[n] == "mood.unhappy") {
      data <- data %>%
        drop_na(mar.binary, mood.unhappy)
    }
    else if (results.frame$affect.variable[n] == "mood.bored") {
      data <- data %>%
        drop_na(mar.binary, mood.bored)
    }
    ### DEFINE CONTROL VARIABLE IN MODEL & RUN MODELS
    if (results.frame$control.variable[n] == "none") {
      model <- glmer(mar.binary ~ data[,affectvar] + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ 1 + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]
    }
    else if (results.frame$control.variable[n] == "motives.coping") {
      model <- glmer(mar.binary ~ data[,affectvar] + MMQ.coping + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ MMQ.coping + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "motives.enhancement") {
      model <- glmer(mar.binary ~ data[,affectvar] + MMQ.enhancement + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ MMQ.enhancement + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "motives.social") {
      model <- glmer(mar.binary ~ data[,affectvar] + MMQ.social + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ MMQ.social + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.premeditation") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.premeditation + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.premeditation + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.negativeurgency") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.negativeurgency + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.negativeurgency + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.sensationseeking") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.sensationseeking + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.sensationseeking + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.perseverance") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.perseverance + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.perseverance + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.positiveurgency") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.positiveurgency + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.positiveurgency + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "sex") {
      model <- glmer(mar.binary ~ data[,affectvar] + sex + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ sex + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }    
  }

  return(results.frame)

}
#
# run SCA
#
results.frame.rq1.na <- resultsframe.rq1.na(c("mar.binary"))
results.curve.rq1.na <- curve.rq1.na(results.frame.rq1.na)
#
sum(results.curve.rq1.na$p.value < .05)/length(results.curve.rq1.na$p.value) # % of significant specifications
1/median(exp(results.curve.rq1.na$effect)) # median effect size
#
##################################################
#
# 4) permutation test
#
repetitions <- 500
# Set up empty list of data sets
shuffled.averaged.averaged <- vector("list", repetitions)
shuffled.averaged.median <- vector("list", repetitions)
shuffled.immediately.averaged <- vector("list", repetitions)
shuffled.immediately.median <- vector("list", repetitions)
#
# create 500 shuffled datasets
#
for (n in 1:repetitions) {
  print(n)
  set.seed(n)
  temp.data.averaged.averaged <- data.averaged.averaged
  temp.data.averaged.median <- data.averaged.median
  temp.data.immediately.averaged <- data.immediately.averaged
  temp.data.immediately.median <- data.immediately.median
  temp.data.averaged.averaged <- temp.data.averaged.averaged %>%
    group_by(PID) %>%
    mutate(mar.binary = sample(mar.binary))
  temp.data.averaged.median <- temp.data.averaged.median %>%
    group_by(PID) %>%
    mutate(mar.binary = sample(mar.binary))
  temp.data.immediately.averaged <- temp.data.immediately.averaged %>%
    group_by(PID) %>%
    mutate(mar.binary = sample(mar.binary))  
  temp.data.immediately.median <- temp.data.immediately.median %>%
    group_by(PID) %>%
    mutate(mar.binary = sample(mar.binary))             
  shuffled.averaged.averaged[[n]] <- temp.data.averaged.averaged
  shuffled.averaged.median[[n]] <- temp.data.averaged.median
  shuffled.immediately.averaged[[n]] <- temp.data.immediately.averaged
  shuffled.immediately.median[[n]] <- temp.data.immediately.median
}
#
# create empty frame for results to be written in
#
resultsframe.shuffle.rq1.na <- function(input) {
  ### SETUP SPECIFICATION NAMES
  levels.outcome <- input
  levels.affect.variable <- c("mood.negative", "mood.negative.no.boredom", "mood.angry", "mood.anxious", "mood.irritable", "mood.unhappy", "mood.bored")
  levels.inclusion.criterion <- c("prior.use.any", "prior.use.30.days", "prior.use.30.days.high")
  levels.control.variable <- c("none", "motives.coping", "motives.enhancement", "motives.social", "upps.premeditation", "upps.negativeurgency", 
                               "upps.sensationseeking", "upps.perseverance", "upps.positiveurgency", "sex")
  levels.timing.use <- c("averaged1", "immediate")
  levels.timing.nonuse <- c("averaged2", "median")
  levels.shuffle <- c(1:500)
  ### CALCULATE NUMBER OF COMBINATIONS TO RUN
  combinations <- length(levels.outcome) * length(levels.affect.variable) * length(levels.inclusion.criterion) * length(levels.control.variable) * 
    length(levels.timing.use) * length(levels.timing.nonuse) * length(levels.shuffle)
  ### SETUP RESULTS FRAME
  results.frame <- data.frame(matrix(NA, nrow = combinations, ncol = 9))
  colnames(results.frame) <- c("outcome", "affect.variable", "inclusion.criterion", "control.variable", "timing.use", "timing.nonuse", "shuffle", "effect", "p.value")
  ### WRITE COMBINATIONS INTO RESULTS FRAME
  results.frame$outcome <- rep(levels.outcome, each = nrow(results.frame)/length(levels.outcome))
  
  results.frame$affect.variable <- rep(rep(levels.affect.variable, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable))),
                                       times = length(levels.outcome))
  
  results.frame$inclusion.criterion <- rep(rep(rep(levels.inclusion.criterion, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) * 
                                                                                                             length(levels.inclusion.criterion))), times = length(levels.outcome)), times = length(levels.affect.variable))
  
  results.frame$control.variable <- rep(rep(rep(rep(levels.control.variable, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                                                                                           length(levels.inclusion.criterion) * length(levels.control.variable))), times = length(levels.outcome)), 
                                            times = length(levels.affect.variable)), times = length(levels.inclusion.criterion))
  results.frame$timing.use <- rep(rep(rep(rep(rep(levels.timing.use, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                                                                                   length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.timing.use))), times = length(levels.outcome)),
                                          times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), times = length(levels.control.variable))
  results.frame$timing.nonuse <- rep(rep(rep(rep(rep(rep(levels.timing.nonuse, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                              length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.timing.use) * length(levels.timing.nonuse))),
                                              times = length(levels.outcome)), times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), 
                                         times = length(levels.control.variable)), times = length(levels.timing.use))
  results.frame$shuffle <- rep(rep(rep(rep(rep(rep(rep(levels.shuffle, each = nrow(results.frame)/(length(levels.outcome) * length(levels.affect.variable) *
                                              length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.timing.use) * length(levels.timing.nonuse) * 
                                              length(levels.shuffle))), times = length(levels.outcome)), times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)),
                                              times = length(levels.control.variable)), times = length(levels.timing.use)), times = length(levels.timing.nonuse))
  
  return(results.frame)
}
#
# fill empty frame with results
#
curve.shuffle.rq1.na <- function(input) {
  results.frame <- input
  affectvar <- NA
  model <- NA
  nullmodel <- NA
  data <- NA
  for (n in 1:nrow(results.frame)) {
    print(n) 
    ### DEFINE APPROPRIATE DATASET
    data <- data.frame(data)
    if (results.frame$timing.use[n] == "averaged1" & results.frame$timing.nonuse[n] == "averaged2") {
      data <- data.frame(shuffled.averaged.averaged[results.frame$shuffle[[n]]])
    }
    else if (results.frame$timing.use[n] == "averaged1" & results.frame$timing.nonuse[n] == "median") {
      data <- data.frame(shuffled.averaged.median[results.frame$shuffle[[n]]])
    }
    else if (results.frame$timing.use[n] == "immediate" & results.frame$timing.nonuse[n] == "averaged2") {
      data <- data.frame(shuffled.immediately.averaged[results.frame$shuffle[[n]]])
    }    
    else if (results.frame$timing.use[n] == "immediate" & results.frame$timing.nonuse[n] == "median") {
      data <- data.frame(shuffled.immediately.median[results.frame$shuffle[[n]]])
    }       
    if (results.frame$inclusion.criterion[n] == "prior.use.any") {
      data <- data.frame(data[data$DMU1 == 1,])
    }
    else if (results.frame$inclusion.criterion[n] == "prior.use.30.days") {
      data <- data.frame(data[data$DMU3 > 0,])
    }
    else if (results.frame$inclusion.criterion[n] == "prior.use.30.days.high") {
      data <- data.frame(data[data$DMU4 > 0,])
    }    
    ### DEFINE AFFECT VARIABLE
    affectvar <- results.frame$affect.variable[n]
    ### REMOVE MISSING, NECESSARY FOR anova(); multiple imputation not feasible due to computational costs
    if (results.frame$affect.variable[n] == "mood.negative") {
      data <- data %>%
        drop_na(mar.binary, mood.negative)
    }
    else if (results.frame$affect.variable[n] == "mood.negative.no.boredom") {
      data <- data %>%
        drop_na(mar.binary, mood.negative.no.boredom)
    }
    else if (results.frame$affect.variable[n] == "mood.angry") {
      data <- data %>%
        drop_na(mar.binary, mood.angry)
    }
    else if (results.frame$affect.variable[n] == "mood.anxious") {
      data <- data %>%
        drop_na(mar.binary, mood.anxious)
    }
    else if (results.frame$affect.variable[n] == "mood.irritable") {
      data <- data %>%
        drop_na(mar.binary, mood.irritable)
    }
    else if (results.frame$affect.variable[n] == "mood.unhappy") {
      data <- data %>%
        drop_na(mar.binary, mood.unhappy)
    }
    else if (results.frame$affect.variable[n] == "mood.bored") {
      data <- data %>%
        drop_na(mar.binary, mood.bored)
    }
    ### DEFINE CONTROL VARIABLE IN MODEL & RUN MODELS
    if (results.frame$control.variable[n] == "none") {
      model <- glmer(mar.binary ~ data[,affectvar] + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ 1 + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]
    }
    else if (results.frame$control.variable[n] == "motives.coping") {
      model <- glmer(mar.binary ~ data[,affectvar] + MMQ.coping + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ MMQ.coping + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "motives.enhancement") {
      model <- glmer(mar.binary ~ data[,affectvar] + MMQ.enhancement + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ MMQ.enhancement + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "motives.social") {
      model <- glmer(mar.binary ~ data[,affectvar] + MMQ.social + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ MMQ.social + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.premeditation") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.premeditation + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.premeditation + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.negativeurgency") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.negativeurgency + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.negativeurgency + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.sensationseeking") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.sensationseeking + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.sensationseeking + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.perseverance") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.perseverance + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.perseverance + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "upps.positiveurgency") {
      model <- glmer(mar.binary ~ data[,affectvar] + UPPS.positiveurgency + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ UPPS.positiveurgency + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$control.variable[n] == "sex") {
      model <- glmer(mar.binary ~ data[,affectvar] + sex + (1 | PID), family = binomial, data = data)
      nullmodel <- glmer(mar.binary ~ sex + (1 | PID), family = binomial, data = data)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }    
  }
  
  return(results.frame)
  
}
#
# run permutation test
#
results.frame.shuffle.rq1.na <- resultsframe.shuffle.rq1.na(c("mar.binary"))
results.curve.shuffle.rq1.na <- curve.shuffle.rq1.na(results.frame.shuffle.rq1.na)
#
extract_threshold <- function(x) { # extract % of p < .05 specification from shuffled
  fract <- c()
  for (i in levels(x$shuffle)) {
    temp <- subset(x, shuffle == i)
    ps <- c(temp$p.value)
    values <- ps[!is.na(ps)]
    fract <- append(fract,sum(values<.05)/length(values))
  }
  return(fract)
}
#
extract_fraction <- function(x) { # extract % of p < .05 from curve
  ps <- c(x$p.value)
  values <- ps[!is.na(ps)]
  fract <- sum(values<.05)/length(values)
  return(fract)
}
#
results.curve.shuffle.rq1.na$shuffle <- as.factor(as.character(results.curve.shuffle.rq1.na$shuffle))
sum(extract_fraction(results.curve.rq1.na)<=extract_threshold(results.curve.shuffle.rq1.na)) # count number of shuffled datasets with more sig specs
mean(extract_fraction(results.curve.rq1.na)<=extract_threshold(results.curve.shuffle.rq1.na)) # calculate p-value of permutation test
#
#save.image('rq1.na.RData', compress = "xz")
#
##################################################
#
# 5) plots
#
outcome <- c("curve.rq1.na")
#
temp.data <- NA
for (i in 1:length(outcome)) {
  temp.data <- get(eval(paste("results.", outcome[i], sep = "")))
  temp.data <- temp.data[order(temp.data$effect),]
  temp.data$p <- temp.data[,c("p.value")] # get the relevant p-value
  temp.data$sig <- 0
  temp.data$sig[temp.data$p < .05] <- 1
  temp.data$effect <- exp(temp.data$effect)
  assign(paste("results.", outcome[i], sep = ""), temp.data)
}
# Plot the results (upper graph)
#
temp.plot <- NA
for (i in 1:length(outcome)) {
  temp.data <- get(eval(paste("results.", outcome[i], sep = "")))
  temp.data$index[!is.na(temp.data$effect)] <- 1:nrow(temp.data[!is.na(temp.data$effect),])
  temp.data$p <- temp.data[,"p.value"]
  temp.data$color <- 1
  temp.data$color[temp.data$p < .05] <- 2
  temp.plot <- ggplot(data = temp.data[!is.na(temp.data$effect),], 
                      aes(x = index, y = effect, color = as.factor(sig), size = as.factor(sig))) +
    geom_point(shape = "|") +
    scale_color_manual(values = c("#000000", "#32527b")) +
    scale_size_manual(values = c(5, 7)) +
    geom_hline(yintercept = 1) +
    theme(legend.position = "none",
          plot_title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(50, max(temp.data$index, na.rm = TRUE), by = 50)) +
    scale_y_continuous(limits = c(0.50, 1.50), breaks = seq(0.50, 1.50, by = 0.05), labels = seq(0.50, 1.50, by = 0.05)) +
    xlab("Specification number") +
    ylab("Effect size") +
    theme_cowplot() + 
    theme(legend.position = "none", text = element_text(size = 16, family = "TT Times New Roman"))
  print(temp.plot)
}
# 
# remove control variables from graph to increase readability
#
scatter.frame <- function(results.frame) {
  results.frame <- results.frame[which(!is.na(results.frame$effect)),]
  results.frame <- results.frame[order(results.frame$effect),]
  results.frame$index <- 1:nrow(results.frame)
  results.frame.long <- gather(results.frame, condition, label, affect.variable:timing.nonuse, factor_key = TRUE)
  results.frame.long <- results.frame.long[order(results.frame.long$index),]
  results.frame.long <- results.frame.long[which(results.frame.long$label != "age" & results.frame.long$label != "sex" & results.frame.long$label != "upps.positiveurgency" & results.frame.long$label != "upps.perseverance" & results.frame.long$label != "upps.sensationseeking" & results.frame.long$label != "upps.negativeurgency" & results.frame.long$label != "upps.premeditation" & results.frame.long$label != "motives.social" & results.frame.long$label != "motives.coping" & results.frame.long$label != "motives.enhancement" & results.frame.long$label != "none"),]
  #results.frame.long <- results.frame.long[, c("index", "label")]
  results.frame.long$label <- ordered(results.frame.long$label,
                                      levels = c("median", "averaged2", 
                                                 "immediate", "averaged1",
                                                 "prior.use.30.days.high", "prior.use.30.days", "prior.use.any", 
                                                 "mood.bored", "mood.unhappy", "mood.irritable", "mood.anxious", "mood.angry", "mood.negative.no.boredom", "mood.negative")) 
  library(plyr)
  results.frame.long$label <- revalue(results.frame.long$label,
                                      c("mood.negative" = "Negative affect: average", "mood.negative.no.boredom" = "Negative affect: average no boredom", "mood.angry" = "Negative affect: angry", "mood.anxious" = "Negative affect: anxious", "mood.irritable" = "Negative affect: irritable", "mood.unhappy" = "Negative affect: unhappy", "mood.bored" = "Negative affect: bored",
                                        "prior.use.any" = "Inclusion criterion: any lifetime use", "prior.use.30.days" = "Inclusion criterion: past 30 days use", "prior.use.30.days.high" = "Inclusion criterion: past 30 days getting high",
                                        "averaged1" = "Affect averaged prior to use on use days", "immediate" = "Affect immediately prior to use on use days",
                                        "averaged2" = "Affect averaged over entire day on non-use days", "median" = "Affect averaged prior to median use on non-use days"))
  return(results.frame.long)
}
#
# plot the results (lower graph)
#
results.frame.long <- NA
temp.plot2 <- NA
outcomes.single <- c("curve.rq1.na")
#
for (i in 1:length(outcomes.single)) {
  results.frame.long <- scatter.frame(get(paste("results.", outcomes.single[i], sep = "")))
  temp.plot2 <- ggplot(data = results.frame.long, 
                       aes(x = index, y = label, color = as.factor(sig), size = as.factor(sig))) +
    geom_point(shape = "|") +
    scale_color_manual(values = c("#000000", "#32527b")) +
    scale_size_manual(values = c(3, 6)) +
    geom_hline(yintercept = 0) +
    theme(legend.position="none",
          axis.title.y = element_blank()) +
    scale_x_continuous(breaks = seq(50, max(temp.data$index, na.rm = TRUE), by = 50)) +
    xlab("Specification number") +
    theme_cowplot() + 
    theme(legend.position = "none", text = element_text(size = 16, family = "TT Times New Roman"), axis.title.y = element_blank())
  print(temp.plot2)
}
#
#
#
ggpubr::ggarrange(temp.plot, temp.plot2, nrow = 2, ncol = 1)
#ggsave(filename = "rq1.na.png", dpi = 600, height = 12, width = 18)

#
# transform data for lower graph in full SUPPLEMENTARY MATERIALS
#
scatter.frame <- function(results.frame) {
  results.frame <- results.frame[which(!is.na(results.frame$effect)),]
  results.frame <- results.frame[order(results.frame$effect),]
  results.frame$index <- 1:nrow(results.frame)
  results.frame.long <- gather(results.frame, condition, label, affect.variable:timing.nonuse, factor_key = TRUE)
  results.frame.long <- results.frame.long[order(results.frame.long$index),]
  #results.frame.long <- results.frame.long[, c("index", "label")]
  results.frame.long$label <- ordered(results.frame.long$label,
                                      levels = c("median", "averaged2", 
                                                 "immediate", "averaged1",
                                                 "age", "sex", "upps.positiveurgency", "upps.perseverance", "upps.sensationseeking", "upps.negativeurgency", "upps.premeditation", "motives.social", "motives.enhancement", "motives.coping", "none", 
                                                 "prior.use.30.days.high", "prior.use.30.days", "prior.use.any", 
                                                 "mood.bored", "mood.unhappy", "mood.irritable", "mood.anxious", "mood.angry", "mood.negative.no.boredom", "mood.negative")) 
  library(plyr)
  results.frame.long$label <- revalue(results.frame.long$label,
                                      c("mood.negative" = "Negative affect: average", "mood.negative.no.boredom" = "Negative affect: average no boredom", "mood.angry" = "Negative affect: angry", "mood.anxious" = "Negative affect: anxious", "mood.irritable" = "Negative affect: irritable", "mood.unhappy" = "Negative affect: unhappy", "mood.bored" = "Negative affect: bored",
                                        "prior.use.any" = "Inclusion criterion: any lifetime use", "prior.use.30.days" = "Inclusion criterion: past 30 days use", "prior.use.30.days.high" = "Inclusion criterion: past 30 days getting high",
                                        "none" = "Control variable: none", "motives.coping" = "Control variable: coping motives", "motives.enhancement" = "Control variable: enhancement motives", "motives.social" = "Control variable: social motives", "upps.premeditation" = "Control variable: Premeditation", "upps.negativeurgency" = "Control variable: negative urgency", "upps.sensationseeking" = "Control variable: sensation seeking", "upps.perseverance" = "Control variable: perseverance", "upps.positiveurgency" = "Control variable: positive urgency", "sex" = "Control variable: biological sex", "age" = "Control variable: age" ,
                                        "averaged1" = "Affect averaged prior to use on use days", "immediate" = "Affect immediately prior to use on use days",
                                        "averaged2" = "Affect averaged over entire day on non-use days", "median" = "Affect averaged prior to median use on non-use days"))
  return(results.frame.long)
}
#
# plot the results (lower graph in full)
#
results.frame.long <- NA
temp.plot2 <- NA
outcomes.single <- c("curve.rq1.na")
#
for (i in 1:length(outcomes.single)) {
  results.frame.long <- scatter.frame(get(paste("results.", outcomes.single[i], sep = "")))
  temp.plot2 <- ggplot(data = results.frame.long, 
                       aes(x = index, y = label, color = as.factor(sig), size = as.factor(sig))) +
    geom_point(shape = "|") +
    scale_color_manual(values = c("#000000", "#32527b")) +
    scale_size_manual(values = c(3, 6)) +
    geom_hline(yintercept = 0) +
    theme(legend.position="none",
          axis.title.y = element_blank()) +
    scale_x_continuous(breaks = seq(1, max(temp.data$index, na.rm = TRUE), by = 10), labels = NULL) +
    xlab("Specification number") +
    theme_cowplot() + 
    theme(legend.position = "none", text = element_text(size = 16, family = "TT Times New Roman"), axis.title.y = element_blank())
  print(temp.plot2)
}
#
ggpubr::ggarrange(temp.plot, temp.plot2, nrow = 2, ncol = 1)
#ggsave(filename = "rq1.na.full.png", dpi = 600, height = 12, width = 15)
