##################################################
#
# R code to data analysis 'Exploring affect regulation of marijuana use in everyday life using Specification Curve Analysis'
# written by Jonas Dora
# email: jdora@uw.edu
# please email me directly if you see any errors or have any questions
# last update: 3/25/2922
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
data.lead <- read.csv("/mmfs1/home/jdora/data.lead.csv")
data.lead.episode <- read.csv("/mmfs1/home/jdora/data.lead.episode.csv")
#
# turn PID, sex, and marijuana use into factors
#
factors <- c("PID", "sex")
data.lead[factors] <- lapply(data.lead[factors], factor)
data.lead.episode[factors] <- lapply(data.lead.episode[factors], factor)
data.lead$mar.binary <- factor(data.lead$mar.binary, levels = c("0", "1"))
data.lead.episode$mar.binary <- factor(data.lead.episode$mar.binary, levels = c("0", "1"))
#
# calculating and standardizing variables in each of the four datasets
#
mood.negative <- c("mood.angry", "mood.anxious", "mood.irritable", "mood.unhappy", "mood.bored")
mood.positive <- c("mood.cheerful", "mood.friendly", "mood.calm", "mood.happy", "mood.engaged")
mood.negative.no.boredom <- c("mood.angry", "mood.anxious", "mood.irritable", "mood.unhappy")
#
data.lead$mood.negative <- rowMeans(data.lead[,mood.negative])
data.lead$mood.positive <- rowMeans(data.lead[,mood.positive])
data.lead[is.na(data.lead)]<-NA
#
mood.negative.morning <- c("mood.angry.morning", "mood.anxious.morning", "mood.irritable.morning", "mood.unhappy.morning", "mood.bored.morning")
mood.positive.morning <- c("mood.cheerful.morning", "mood.friendly.morning", "mood.calm.morning", "mood.happy.morning", "mood.engaged.morning")
data.lead$mood.negative.morning <- rowMeans(data.lead[,mood.negative.morning])
data.lead$mood.positive.morning <- rowMeans(data.lead[,mood.positive.morning])
data.lead[is.na(data.lead)]<-NA
#
data.lead$mood.negative.no.boredom <- rowMeans(data.lead[,mood.negative.no.boredom])
data.lead[is.na(mood.negative.no.boredom)]<-NA
#
mood.negative.no.boredom.morning <- c("mood.angry.morning", "mood.anxious.morning", "mood.irritable.morning", "mood.unhappy.morning")
data.lead$mood.negative.no.boredom.morning <- rowMeans(data.lead[,mood.negative.no.boredom.morning])
data.lead[is.na(mood.negative.no.boredom.morning)]<-NA
#
gmc <- c("age", "MMQ.coping", "MMQ.enhancement", "MMQ.conformity", "MMQ.social", "MMQ.expansion", "UPPS.premeditation", "UPPS.negativeurgency",
         "UPPS.sensationseeking", "UPPS.perseverance", "UPPS.positiveurgency")
data.lead[gmc] <- lapply(data.lead[gmc], scale, center = T, scale = T)
#
data.lead <- data.lead %>%
  group_by(PID) %>%
  mutate(mood.angry.lead = lead(mood.angry)) %>%
  mutate(mood.anxious.lead = lead(mood.anxious)) %>%
  mutate(mood.irritable.lead = lead(mood.irritable)) %>%
  mutate(mood.unhappy.lead = lead(mood.unhappy)) %>%
  mutate(mood.bored.lead = lead(mood.bored)) %>%
  mutate(mood.cheerful.lead = lead(mood.cheerful)) %>%
  mutate(mood.friendly.lead = lead(mood.friendly)) %>%
  mutate(mood.calm.lead = lead(mood.calm)) %>%
  mutate(mood.happy.lead = lead(mood.happy)) %>%
  mutate(mood.engaged.lead = lead(mood.engaged)) %>%
  mutate(mood.negative.lead = lead(mood.negative)) %>%
  mutate(mood.positive.lead = lead(mood.positive)) %>%
  mutate(mood.negative.no.boredom.lead = lead(mood.negative.no.boredom)) %>%
  mutate(mood.angry.morning.lead = lead(mood.angry.morning)) %>%
  mutate(mood.anxious.morning.lead = lead(mood.anxious.morning)) %>%
  mutate(mood.irritable.morning.lead = lead(mood.irritable.morning)) %>%
  mutate(mood.unhappy.morning.lead = lead(mood.unhappy.morning)) %>%
  mutate(mood.bored.morning.lead = lead(mood.bored.morning)) %>%
  mutate(mood.cheerful.morning.lead = lead(mood.cheerful.morning)) %>%
  mutate(mood.friendly.morning.lead = lead(mood.friendly.morning)) %>%
  mutate(mood.calm.morning.lead = lead(mood.calm.morning)) %>%
  mutate(mood.happy.morning.lead = lead(mood.happy.morning)) %>%
  mutate(mood.engaged.morning.lead = lead(mood.engaged.morning)) %>%
  mutate(mood.negative.morning.lead = lead(mood.negative.morning)) %>%
  mutate(mood.positive.morning.lead = lead(mood.positive.morning)) %>%
  mutate(mood.negative.no.boredom.morning.lead = lead(mood.negative.no.boredom.morning)) 
#
data.lead.episode$mood.negative <- rowMeans(data.lead.episode[,mood.negative])
data.lead.episode$mood.positive <- rowMeans(data.lead.episode[,mood.positive])
data.lead.episode[is.na(data.lead.episode)]<-NA
#
data.lead.episode$mood.negative.morning <- rowMeans(data.lead.episode[,mood.negative.morning])
data.lead.episode$mood.positive.morning <- rowMeans(data.lead.episode[,mood.positive.morning])
data.lead.episode[is.na(data.lead.episode)]<-NA
#
data.lead.episode$mood.negative.no.boredom <- rowMeans(data.lead.episode[,mood.negative.no.boredom])
data.lead.episode[is.na(mood.negative.no.boredom)]<-NA
#
data.lead.episode$mood.negative.no.boredom.morning <- rowMeans(data.lead.episode[,mood.negative.no.boredom.morning])
data.lead.episode[is.na(mood.negative.no.boredom.morning)]<-NA
#
gmc <- c("age", "MMQ.coping", "MMQ.enhancement", "MMQ.conformity", "MMQ.social", "MMQ.expansion", "UPPS.premeditation", "UPPS.negativeurgency",
         "UPPS.sensationseeking", "UPPS.perseverance", "UPPS.positiveurgency")
data.lead.episode[gmc] <- lapply(data.lead.episode[gmc], scale, center = T, scale = T)
#
data.lead.episode <- data.lead.episode %>%
  group_by(PID) %>%
  mutate(mood.angry.lead = lead(mood.angry)) %>%
  mutate(mood.anxious.lead = lead(mood.anxious)) %>%
  mutate(mood.irritable.lead = lead(mood.irritable)) %>%
  mutate(mood.unhappy.lead = lead(mood.unhappy)) %>%
  mutate(mood.bored.lead = lead(mood.bored)) %>%
  mutate(mood.cheerful.lead = lead(mood.cheerful)) %>%
  mutate(mood.friendly.lead = lead(mood.friendly)) %>%
  mutate(mood.calm.lead = lead(mood.calm)) %>%
  mutate(mood.happy.lead = lead(mood.happy)) %>%
  mutate(mood.engaged.lead = lead(mood.engaged)) %>%
  mutate(mood.negative.lead = lead(mood.negative)) %>%
  mutate(mood.positive.lead = lead(mood.positive)) %>%
  mutate(mood.negative.no.boredom.lead = lead(mood.negative.no.boredom)) %>%
  mutate(mood.angry.morning.lead = lead(mood.angry.morning)) %>%
  mutate(mood.anxious.morning.lead = lead(mood.anxious.morning)) %>%
  mutate(mood.irritable.morning.lead = lead(mood.irritable.morning)) %>%
  mutate(mood.unhappy.morning.lead = lead(mood.unhappy.morning)) %>%
  mutate(mood.bored.morning.lead = lead(mood.bored.morning)) %>%
  mutate(mood.cheerful.morning.lead = lead(mood.cheerful.morning)) %>%
  mutate(mood.friendly.morning.lead = lead(mood.friendly.morning)) %>%
  mutate(mood.calm.morning.lead = lead(mood.calm.morning)) %>%
  mutate(mood.happy.morning.lead = lead(mood.happy.morning)) %>%
  mutate(mood.engaged.morning.lead = lead(mood.engaged.morning)) %>%
  mutate(mood.negative.morning.lead = lead(mood.negative.morning)) %>%
  mutate(mood.positive.morning.lead = lead(mood.positive.morning)) %>%
  mutate(mood.negative.no.boredom.morning.lead = lead(mood.negative.no.boredom.morning)) 
#
##################################################
#
# 3) specification curve analysis
#
# create empty frame for results to be written in
#
resultsframe.rq2.pa <- function(input) {
  ### SETUP SPECIFICATION NAMES
  levels.predictor <- input
  levels.affect.variable <- c("mood.positive.lead", "mood.cheerful.lead", "mood.friendly.lead", "mood.calm.lead", "mood.happy.lead", "mood.engaged.lead",
                              "mood.positive.morning.lead", "mood.cheerful.morning.lead", "mood.friendly.morning.lead", "mood.calm.morning.lead", "mood.happy.morning.lead", "mood.engaged.morning.lead")
  levels.inclusion.criterion <- c("prior.use.any", "prior.use.30.days", "prior.use.30.days.high")
  levels.control.variable <- c("none", "motives.coping", "motives.enhancement", "motives.social", "upps.premeditation", "upps.negativeurgency", 
                               "upps.sensationseeking", "upps.perseverance", "upps.positiveurgency", "sex", "age")
  levels.model <- c("lead", "lead.episode")
  ### CALCULATE NUMBER OF COMBINATIONS TO RUN
  combinations <- length(levels.predictor) * length(levels.affect.variable) * length(levels.inclusion.criterion) * length(levels.control.variable) * 
    length(levels.model)
  ### SETUP RESULTS FRAME
  results.frame <- data.frame(matrix(NA, nrow = combinations, ncol = 8))
  colnames(results.frame) <- c("predictor", "affect.variable", "inclusion.criterion", "control.variable", "model", "affect.control", "effect", "p.value")
  ### WRITE COMBINATIONS INTO RESULTS FRAME
  results.frame$predictor <- rep(levels.predictor, each = nrow(results.frame)/length(levels.predictor))
  
  results.frame$affect.variable <- rep(rep(levels.affect.variable, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable))),
                                       times = length(levels.predictor))
  
  results.frame$inclusion.criterion <- rep(rep(rep(levels.inclusion.criterion, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) * 
                                                                                                             length(levels.inclusion.criterion))), times = length(levels.predictor)), times = length(levels.affect.variable))
  
  results.frame$control.variable <- rep(rep(rep(rep(levels.control.variable, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) *
                                                                                                           length(levels.inclusion.criterion) * length(levels.control.variable))), times = length(levels.predictor)), 
                                            times = length(levels.affect.variable)), times = length(levels.inclusion.criterion))
  results.frame$model <- rep(rep(rep(rep(rep(levels.model, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) *
                                                                                         length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.model))), times = length(levels.predictor)),
                                     times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), times = length(levels.control.variable))
  results.frame$affect.control <- substr(results.frame$affect.variable, 1, nchar(results.frame$affect.variable)-5)
  
  return(results.frame)
}
#
# fill empty frame with results
#
curve.rq2.pa <- function(input) {
  results.frame <- input
  affectvar <- NA
  model <- NA
  nullmodel <- NA
  data <- NA
  for (n in 1:nrow(results.frame)) {
    print(n) 
    ### DEFINE APPROPRIATE DATASET
    data <- data.frame(data)
    if (results.frame$model[n] == "lead") {
      data <- data.frame(data.lead)
    }
    else if (results.frame$model[n] == "lead.episode") {
      data <- data.frame(data.lead.episode)
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
    affect.control <- results.frame$affect.control[n]
    ### REMOVE MISSING, NECESSARY FOR anova(); multiple imputation not feasible due to computational costs
    if (results.frame$affect.variable[n] == "mood.positive.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.positive.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.cheerful.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.cheerful.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.friendly.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.friendly.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.calm.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.calm.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.happy.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.happy.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.engaged.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.engaged.lead)
    }
    if (results.frame$affect.variable[n] == "mood.positive.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.positive.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.cheerful.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.cheerful.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.friendly.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.friendly.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.calm.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.calm.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.happy.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.happy.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.engaged.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.engaged.morning.lead)
    }
    ### DEFINE CONTROL VARIABLE IN MODEL & RUN MODELS
    if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "none") {
      model <- lmer(data[,affectvar] ~ 1 + mar.binary + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ 1 + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "motives.coping") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.coping + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.coping + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "motives.enhancement") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.enhancement + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.enhancement + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "motives.social") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.social + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.social + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.premeditation") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.premeditation + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.premeditation + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.negativeurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.negativeurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.negativeurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.sensationseeking") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.sensationseeking + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.sensationseeking + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.perseverance") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.perseverance + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.perseverance + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.positiveurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.positiveurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.positiveurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "sex") {
      model <- lmer(data[,affectvar] ~ mar.binary + sex + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ sex + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }   
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "none") {
      model <- lmer(data[,affectvar] ~ mar.binary + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ 1 + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "motives.coping") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.coping + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.coping + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "motives.enhancement") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.enhancement + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.enhancement + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "motives.social") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.social + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.social + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.premeditation") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.premeditation + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.premeditation + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.negativeurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.negativeurgency + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.negativeurgency + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.sensationseeking") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.sensationseeking + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.sensationseeking + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.perseverance") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.perseverance + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.perseverance + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.positiveurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.positiveurgency + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.positiveurgency + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "sex") {
      model <- lmer(data[,affectvar] ~ mar.binary + sex + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ sex + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }  
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "age") {
      model <- lmer(data[,affectvar] ~ mar.binary + age + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ age + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }  
  }
  
  return(results.frame)
  
}
#
# run SCA
#
results.frame.rq2.pa <- resultsframe.rq2.pa(c("mar.binary"))
results.curve.rq2.pa <- curve.rq2.pa(results.frame.rq2.pa)
#
sum(results.curve.rq2.pa$p.value < .05, na.rm=T)/length(results.curve.rq2.pa$p.value) # % of significant specifications
median(results.curve.rq2.pa$effect, na.rm=T) # median effect size
#
##################################################
#
# 4) permutation test
#
repetitions <- 500
# Set up empty list of data sets
shuffled.lead <- vector("list", repetitions)
shuffled.lead.episode <- vector("list", repetitions)
#
# create 500 shuffled datasets
#
for (n in 1:repetitions) {
  print(n)
  set.seed(n)
  temp.data.lead <- data.lead
  temp.data.lead.episode <- data.lead.episode
  temp.data.lead <- temp.data.lead %>%
    group_by(PID) %>%
    mutate(mar.binary = sample(mar.binary))
  temp.data.lead.episode <- temp.data.lead.episode %>%
    group_by(PID) %>%
    mutate(mar.binary = sample(mar.binary))             
  shuffled.lead[[n]] <- temp.data.lead
  shuffled.lead.episode[[n]] <- temp.data.lead.episode
}
#
# create empty frame for results to be written in
#
resultsframe.shuffle.rq2.pa <- function(input) {
  ### SETUP SPECIFICATION NAMES
  levels.predictor <- input
  levels.affect.variable <- c("mood.positive.lead", "mood.cheerful.lead", "mood.friendly.lead", "mood.calm.lead", "mood.happy.lead", "mood.engaged.lead",
                              "mood.positive.morning.lead", "mood.cheerful.morning.lead", "mood.friendly.morning.lead", "mood.calm.morning.lead", "mood.happy.morning.lead", "mood.engaged.morning.lead")
  levels.inclusion.criterion <- c("prior.use.any", "prior.use.30.days", "prior.use.30.days.high")
  levels.control.variable <- c("none", "motives.coping", "motives.enhancement", "motives.social", "upps.premeditation", "upps.positiveurgency", 
                               "upps.sensationseeking", "upps.perseverance", "upps.positiveurgency", "sex", "age")
  levels.model <- c("lead", "lead.episode")
  levels.shuffle <- c(1:500)
  ### CALCULATE NUMBER OF COMBINATIONS TO RUN
  combinations <- length(levels.predictor) * length(levels.affect.variable) * length(levels.inclusion.criterion) * length(levels.control.variable) * 
    length(levels.model) * length(levels.shuffle)
  ### SETUP RESULTS FRAME
  results.frame <- data.frame(matrix(NA, nrow = combinations, ncol = 9))
  colnames(results.frame) <- c("predictor", "affect.variable", "inclusion.criterion", "control.variable", "model", "affect.control", "shuffle", "effect", "p.value")
  ### WRITE COMBINATIONS INTO RESULTS FRAME
  results.frame$predictor <- rep(levels.predictor, each = nrow(results.frame)/length(levels.predictor))
  
  results.frame$affect.variable <- rep(rep(levels.affect.variable, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable))),
                                       times = length(levels.predictor))
  
  results.frame$inclusion.criterion <- rep(rep(rep(levels.inclusion.criterion, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) * 
                                                                                                             length(levels.inclusion.criterion))), times = length(levels.predictor)), times = length(levels.affect.variable))
  
  results.frame$control.variable <- rep(rep(rep(rep(levels.control.variable, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) *
                                                                                                           length(levels.inclusion.criterion) * length(levels.control.variable))), times = length(levels.predictor)), 
                                            times = length(levels.affect.variable)), times = length(levels.inclusion.criterion))
  results.frame$model <- rep(rep(rep(rep(rep(levels.model, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) *
                                                                                         length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.model))), times = length(levels.predictor)),
                                     times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), times = length(levels.control.variable))
  results.frame$affect.control <- substr(results.frame$affect.variable, 1, nchar(results.frame$affect.variable)-5)
  results.frame$shuffle <- rep(rep(rep(rep(rep(rep(levels.shuffle, each = nrow(results.frame)/(length(levels.predictor) * length(levels.affect.variable) *
                                                                                                 length(levels.inclusion.criterion) * length(levels.control.variable) * length(levels.model) * length(levels.shuffle))), times = length(levels.predictor)),
                                           times = length(levels.affect.variable)), times = length(levels.inclusion.criterion)), times = length(levels.control.variable)), times = length(levels.model))
  
  return(results.frame)
}
#
# fill empty frame with results
#
curve.shuffle.rq2.pa <- function(input) {
  results.frame <- input
  affectvar <- NA
  model <- NA
  nullmodel <- NA
  data <- NA
  for (n in 1:nrow(results.frame)) {
    print(n) 
    ### DEFINE APPROPRIATE DATASET
    data <- data.frame(data)
    if (results.frame$model[n] == "lead") {
      data <- data.frame(shuffled.lead[results.frame$shuffle[[n]]])
    }
    else if (results.frame$model[n] == "lead.episode") {
      data <- data.frame(shuffled.lead.episode[results.frame$shuffle[[n]]])
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
    affect.control <- results.frame$affect.control[n]
    ### REMOVE MISSING, NECESSARY FOR anova(); multiple imputation not feasible due to computational costs
    if (results.frame$affect.variable[n] == "mood.positive.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.positive.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.cheerful.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.angry.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.friendly.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.anxious.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.calm.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.irritable.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.happy.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.unhappy.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.engaged.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.bored.lead)
    }
    if (results.frame$affect.variable[n] == "mood.positive.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.positive.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.cheerful.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.angry.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.friendly.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.anxious.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.calm.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.irritable.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.happy.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.unhappy.morning.lead)
    }
    else if (results.frame$affect.variable[n] == "mood.engaged.morning.lead") {
      data <- data %>%
        drop_na(mar.binary, mood.bored.morning.lead)
    }
    ### DEFINE CONTROL VARIABLE IN MODEL & RUN MODELS
    if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "none") {
      model <- lmer(data[,affectvar] ~ 1 + mar.binary + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ 1 + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "motives.coping") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.coping + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.coping + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "motives.enhancement") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.enhancement + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.enhancement + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "motives.social") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.social + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.social + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.premeditation") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.premeditation + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.premeditation + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.negativeurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.negativeurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.negativeurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.sensationseeking") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.sensationseeking + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.sensationseeking + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.perseverance") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.perseverance + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.perseverance + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "upps.positiveurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.positiveurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.positiveurgency + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead" & results.frame$control.variable[n] == "sex") {
      model <- lmer(data[,affectvar] ~ mar.binary + sex + data[,affect.control] + (1 | PID), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ sex + data[,affect.control] + (1 | PID), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }   
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "none") {
      model <- lmer(data[,affectvar] ~ mar.binary + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ 1 + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "motives.coping") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.coping + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.coping + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "motives.enhancement") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.enhancement + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.enhancement + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "motives.social") {
      model <- lmer(data[,affectvar] ~ mar.binary + MMQ.social + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ MMQ.social + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.premeditation") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.premeditation + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.premeditation + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.negativeurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.negativeurgency + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.negativeurgency + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.sensationseeking") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.sensationseeking + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.sensationseeking + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.perseverance") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.perseverance + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.perseverance + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "upps.positiveurgency") {
      model <- lmer(data[,affectvar] ~ mar.binary + UPPS.positiveurgency + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ UPPS.positiveurgency + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "sex") {
      model <- lmer(data[,affectvar] ~ mar.binary + sex + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ sex + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    }  
    else if (results.frame$model[n] == "lead.episode" & results.frame$control.variable[n] == "age") {
      model <- lmer(data[,affectvar] ~ mar.binary + age + (1 | PID/episode), data = data, REML = F)
      nullmodel <- lmer(data[,affectvar] ~ age + (1 | PID/episode), data = data, REML = F)
      results.frame$effect[n] <- summary(model)$coefficients[2,1]
      results.frame$p.value[n] <- anova(model, nullmodel)[2,"Pr(>Chisq)"]      
    } 
  }
  
  return(results.frame)
  
}
#
# run permutation test
#
results.frame.shuffle.rq2.pa <- resultsframe.shuffle.rq2.pa(c("mar.binary"))
results.curve.shuffle.rq2.pa <- curve.shuffle.rq2.pa(results.frame.shuffle.rq2.pa)
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
results.curve.shuffle.rq2.pa$shuffle <- as.factor(as.character(results.curve.shuffle.rq2.pa$shuffle))
sum(extract_fraction(results.curve.rq2.pa)<=extract_threshold(results.curve.shuffle.rq2.pa)) # count number of shuffled datasets with more sig specs
mean(extract_fraction(results.curve.rq2.pa)<=extract_threshold(results.curve.shuffle.rq2.pa)) # calculate p-value of permutation test
#
save.image('rq2.pa.RData', compress = "xz")
#
##################################################
#
# 5) plots
#
outcome <- c("curve.rq2.pa")
#
temp.data <- NA
for (i in 1:length(outcome)) {
  temp.data <- get(eval(paste("results.", outcome[i], sep = "")))
  temp.data <- temp.data[order(temp.data$effect),]
  temp.data$p <- temp.data[,c("p.value")] # get the relevant p-value
  temp.data$sig <- 0
  temp.data$sig[temp.data$p < .05] <- 1
  assign(paste("results.", outcome[i], sep = ""), temp.data)
}
# Plot the results
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
    scale_color_manual(values = c("#000000", "#da680f")) +
    scale_size_manual(values = c(5, 7)) +
    geom_hline(yintercept = 0) +
    theme(legend.position = "none",
          plot_title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(50, max(temp.data$index, na.rm = TRUE), by = 50)) +
    scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, by = 1), labels = seq(-10, 10, by = 1)) +
    xlab("Specification number") +
    ylab("Effect size") +
    theme_cowplot() + 
    theme(legend.position = "none", text = element_text(size = 16, family = "TT Times New Roman"))
  print(temp.plot)
}
#
scatter.frame <- function(results.frame) {
  results.frame <- results.frame[which(!is.na(results.frame$effect)),]
  results.frame <- results.frame[order(results.frame$effect),]
  results.frame$index <- 1:nrow(results.frame)
  results.frame <- results.frame %>%
    mutate(timing = if_else(str_detect(affect.variable, "morning"), "morning", "averaged")) %>%
    relocate(timing, .after = model)
  results.frame.long <- gather(results.frame, condition, label, affect.variable:timing, factor_key = TRUE)
  results.frame.long <- results.frame.long[order(results.frame.long$index),]
  results.frame.long <- results.frame.long[which(results.frame.long$label != "age" & results.frame.long$label != "sex" & results.frame.long$label != "upps.positiveurgency" & results.frame.long$label != "upps.perseverance" & results.frame.long$label != "upps.sensationseeking" & results.frame.long$label != "upps.negativeurgency" & results.frame.long$label != "upps.premeditation" & results.frame.long$label != "motives.social" & results.frame.long$label != "motives.coping" & results.frame.long$label != "motives.enhancement" & results.frame.long$label != "none"),]
  #results.frame.long <- results.frame.long[, c("index", "label")]
  results.frame.long$label <- ordered(results.frame.long$label,
                                      levels = c("prior.use.30.days.high", "prior.use.30.days", "prior.use.any", 
                                                 "lead.episode", "lead",
                                                 "morning", "averaged",
                                                 "mood.engaged.morning.lead", "mood.happy.morning.lead", "mood.calm.morning.lead", "mood.friendly.morning.lead", "mood.cheerful.morning.lead", "mood.positive.morning.lead", "mood.engaged.lead", "mood.happy.lead", "mood.calm.lead", "mood.friendly.lead", "mood.cheerful.lead", "mood.positive.lead")) 
  library(plyr)
  results.frame.long$label <- revalue(results.frame.long$label,
                                      c("mood.positive.lead" = "Positive affect: average", "mood.cheerful.lead" = "Positive affect: cheerful", "mood.friendly.lead" = "Positive affect: friendly", "mood.calm.lead" = "Positive affect: calm", "mood.happy.lead" = "Positive affect: happy", "mood.engaged.lead" = "Positive affect: engaged", "mood.positive.morning.lead" = "Positive affect: average", "mood.cheerful.morning.lead" = "Positive affect: cheerful", "mood.friendly.morning.lead" = "Positive affect: friendly", "mood.calm.morning.lead" = "Positive affect: calm", "mood.happy.morning.lead" = "Positive affect: happy", "mood.engaged.morning.lead" = "Positive affect: engaged",
                                        "morning" = "Affect reported in the morning", "averaged" = "Affect averaged over day",
                                        "lead" = "Comparing affect following use to non-use", "lead.episode" = "Comparing affect pre- to post use",
                                        "prior.use.any" = "Inclusion criterion: any lifetime use", "prior.use.30.days" = "Inclusion criterion: past 30 days use", "prior.use.30.days.high" = "Inclusion criterion: past 30 days getting high"))
  return(results.frame.long)
}
#
results.frame.long <- NA
temp.plot2 <- NA
outcomes.single <- c("curve.rq2.pa")
#
for (i in 1:length(outcomes.single)) {
  results.frame.long <- scatter.frame(get(paste("results.", outcomes.single[i], sep = "")))
  temp.plot2 <- ggplot(data = results.frame.long, 
                       aes(x = index, y = label, color = as.factor(sig), size = as.factor(sig))) +
    geom_point(shape = "|") +
    scale_color_manual(values = c("#000000", "#da680f")) +
    scale_size_manual(values = c(3, 6)) +
    geom_hline(yintercept = 0) +
    theme(legend.position="none",
          axis.title.y = element_blank()) +
    scale_x_continuous(breaks = seq(1, max(temp.data$index, na.rm = TRUE), by = 10), labels = NULL) +
    xlab("Specification") +
    theme_cowplot() + 
    theme(legend.position = "none", text = element_text(size = 16, family = "TT Times New Roman"), axis.title.y = element_blank())
  print(temp.plot2)
}
#
#ggpubr::ggarrange(temp.plot, temp.plot2, nrow = 2, ncol = 1)
#ggsave(filename = "rq2.pa.png", dpi = 600, height = 12, width = 18)
#
scatter.frame <- function(results.frame) {
  results.frame <- results.frame[which(!is.na(results.frame$effect)),]
  results.frame <- results.frame[order(results.frame$effect),]
  results.frame$index <- 1:nrow(results.frame)
  results.frame <- results.frame %>%
    mutate(timing = if_else(str_detect(affect.variable, "morning"), "morning", "averaged")) %>%
    relocate(timing, .after = model)
  results.frame.long <- gather(results.frame, condition, label, affect.variable:timing, factor_key = TRUE)
  results.frame.long <- results.frame.long[order(results.frame.long$index),]
  #results.frame.long <- results.frame.long[which(results.frame.long$label != "age" & results.frame.long$label != "sex" & results.frame.long$label != "upps.positiveurgency" & results.frame.long$label != "upps.perseverance" & results.frame.long$label != "upps.sensationseeking" & results.frame.long$label != "upps.negativeurgency" & results.frame.long$label != "upps.premeditation" & results.frame.long$label != "motives.social" & results.frame.long$label != "motives.coping" & results.frame.long$label != "motives.enhancement" & results.frame.long$label != "none"),]
  #results.frame.long <- results.frame.long[, c("index", "label")]
  results.frame.long$label <- ordered(results.frame.long$label,
                                      levels = c("age", "sex", "upps.positiveurgency", "upps.perseverance", "upps.sensationseeking", "upps.negativeurgency", "upps.premeditation", "motives.social", "motives.enhancement", "motives.coping", "none",
                                                 "prior.use.30.days.high", "prior.use.30.days", "prior.use.any", 
                                                 "lead.episode", "lead",
                                                 "morning", "averaged",
                                                 "mood.engaged.morning.lead", "mood.happy.morning.lead", "mood.calm.morning.lead", "mood.friendly.morning.lead", "mood.cheerful.morning.lead", "mood.positive.morning.lead", "mood.engaged.lead", "mood.happy.lead", "mood.calm.lead", "mood.friendly.lead", "mood.cheerful.lead", "mood.positive.lead")) 
  library(plyr)
  results.frame.long$label <- revalue(results.frame.long$label,
                                      c("mood.positive.lead" = "Positive affect: average", "mood.cheerful.lead" = "Positive affect: cheerful", "mood.friendly.lead" = "Positive affect: friendly", "mood.calm.lead" = "Positive affect: calm", "mood.happy.lead" = "Positive affect: happy", "mood.engaged.lead" = "Positive affect: engaged", "mood.positive.morning.lead" = "Positive affect: average", "mood.cheerful.morning.lead" = "Positive affect: cheerful", "mood.friendly.morning.lead" = "Positive affect: friendly", "mood.calm.morning.lead" = "Positive affect: calm", "mood.happy.morning.lead" = "Positive affect: happy", "mood.engaged.morning.lead" = "Positive affect: engaged",
                                        "morning" = "Affect reported in the morning", "averaged" = "Affect averaged over day",
                                        "lead" = "Comparing affect following use to non-use", "lead.episode" = "Comparing affect pre- to post use",
                                        "prior.use.any" = "Inclusion criterion: any lifetime use", "prior.use.30.days" = "Inclusion criterion: past 30 days use", "prior.use.30.days.high" = "Inclusion criterion: past 30 days getting high",
                                        "none" = "Control variable: none", "motives.coping" = "Control variable: coping motives", "motives.enhancement" = "Control variable: enhancement motives", "motives.social" = "Control variable: social motives", "upps.premeditation" = "Control variable: Premeditation", "upps.negativeurgency" = "Control variable: negative urgency", "upps.sensationseeking" = "Control variable: sensation seeking", "upps.perseverance" = "Control variable: perseverance", "upps.positiveurgency" = "Control variable: positive urgency", "sex" = "Control variable: biological sex", "age" = "Control variable: age"))
  return(results.frame.long)
}
#
results.frame.long <- NA
temp.plot2 <- NA
outcomes.single <- c("curve.rq2.pa")
#
for (i in 1:length(outcomes.single)) {
  results.frame.long <- scatter.frame(get(paste("results.", outcomes.single[i], sep = "")))
  temp.plot2 <- ggplot(data = results.frame.long, 
                       aes(x = index, y = label, color = as.factor(sig), size = as.factor(sig))) +
    geom_point(shape = "|") +
    scale_color_manual(values = c("#000000", "#da680f")) +
    scale_size_manual(values = c(3, 6)) +
    geom_hline(yintercept = 0) +
    theme(legend.position="none",
          axis.title.y = element_blank()) +
    scale_x_continuous(breaks = seq(1, max(temp.data$index, na.rm = TRUE), by = 10), labels = NULL) +
    xlab("Specification") +
    theme_cowplot() + 
    theme(legend.position = "none", text = element_text(size = 16, family = "TT Times New Roman"), axis.title.y = element_blank())
  print(temp.plot2)
}

ggpubr::ggarrange(temp.plot, temp.plot2, nrow = 2, ncol = 1)
ggsave(filename = "rq2.pa.full.png", dpi = 600, height = 12, width = 15)
