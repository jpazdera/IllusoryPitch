library(sjstats)  # For anova_stats() function

# Set contrasts and working directory
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/BidirectionalInteractions/E2/data")

# Load processed scores
data <- read.csv("scores.csv", fileEncoding="UTF-8-BOM")

# Convert independent variables to factors
data$subject <- factor(data$subject, ordered=F)
data$octave <- factor(data$octave, ordered=F)
data$offset <- factor(data$offset, ordered=F)

# Repeated measures ANOVA (d')
model <- aov(dprime ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)

# Repeated measures ANOVA (C)
model <- aov(C ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)

# Pairwise t-tests for main effect of timing offset on C
pairwise.t.test(data$C, data$offset, p.adj="bonferroni", paired=T, alternative="two.sided")
# Early perceived as higher pitched than on time and late; on time perceived as higher pitched than late
