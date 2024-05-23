library(sjstats)  # For anova_stats() function
library(dplyr)
library(tidyverse)
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models
library(car)

# Set contrasts and working directory
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryPitch/E1/data")

###
# Figure 1 Analysis
###

# Load processed scores
data <- read.csv("scores.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% c(15, 22))

# Convert independent variables to factors
data$subject <- factor(data$subject, ordered=F)
data$octave <- factor(data$octave, ordered=F)
data$offset <- factor(data$offset, ordered=F)

# Repeated measures ANOVA (C)
model <- aov(C ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)

# Repeated measures ANOVA (d')
model <- aov(dprime ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)

# Pairwise t-tests for main effect of timing offset on C
pairwise.t.test(data$C, data$offset, p.adj="holm", paired=T, alternative="two.sided")
# Early perceived as higher pitched than on time and late; on time perceived as higher pitched than late

###
# Figure 2 Analysis
###

data <- read.csv("response_data.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% c(15, 22) & rt < 10000)
data$subject <- factor(data$subject, ordered=F)
data$offset <- factor(data$offset, ordered=F)
data$pitch_shift <- factor(data$pitch_shift, ordered=F)
data$correct <- factor(data$correct, ordered=F)

model <- lmer(rt ~ 1 + offset * pitch_shift * correct + (1 + correct | subject), data=data, REML=FALSE)
anova(model)
summary(model)

###
# Figure 3 Analysis
###

data <- read.csv("subj_scores.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% c(15, 22))

cor.test(data$dprime, data$C_slope)