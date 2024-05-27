library(tidyverse)
library(sjstats)  # For anova_stats
library(sjPlot)  # For plot_model
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models

#setwd("/Users/oliverinaldi/Documents/GitHub/IllusoryPitch/E2/data")
setwd("~/git/IllusoryPitch/E2/data")
options(contrasts=c("contr.sum", "contr.poly"))

###
# Figure 4 & 5 Analysis
###

dat <- read.csv("scores.csv")
dat2 <- dat %>% filter(!subject %in% c(21) & version >= 1.1)
dat2$subject <- as.factor(dat2$subject)
dat2$shift_size <- as.factor(dat2$shift_size)
dat2$offset <- as.factor(dat2$offset)

model <- aov(dprime ~ 1 + offset * shift_size + Error(subject / (offset * shift_size)), data=dat2)
anova_stats(model)
# stratum                   |              term | df |  sumsq | meansq | statistic | p.value | etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f | power
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# subject                   |         Residuals | 26 | 78.324 |  3.012 |           |         | 0.533 |         0.875 |   0.494 |           0.676 |     0.495 |    2.647 | 1.000
# subject:offset            |            offset |  2 |  0.595 |  0.298 |     1.588 |   0.214 | 0.004 |         0.051 |   0.001 |           0.005 |     0.001 |    0.231 | 0.299
# subject:offset            |         Residuals | 52 |  9.743 |  0.187 |           |         | 0.066 |         0.466 |  -0.010 |          -0.043 |    -0.010 |    0.934 | 0.989
# subject:shift_size        |        shift_size |  1 | 31.003 | 31.003 |    49.971 |  < .001 | 0.211 |         0.735 |   0.209 |           0.469 |     0.209 |    1.665 | 1.000
# subject:shift_size        |         Residuals | 26 | 16.131 |  0.620 |           |         | 0.110 |         0.591 |   0.072 |           0.232 |     0.072 |    1.201 | 1.000
# subject:offset:shift_size | offset:shift_size |  2 |  0.077 |  0.039 |     0.180 |   0.836 | 0.001 |         0.007 |  -0.002 |          -0.010 |    -0.002 |    0.083 | 0.078
# subject:offset:shift_size |         Residuals | 52 | 11.177 |  0.215 |           |         |       |               |         |                 |           |          |

model <- aov(C ~ 1 + offset * shift_size + Error(subject / (offset * shift_size)), data=dat2)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                     group |              term |  sumsq | df | meansq | statistic | p.value | power
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.465 |         0.812 |   0.411 |           0.551 |     0.412 |    2.079 |                   subject |         Residuals | 13.108 | 26 |  0.504 |           |         | 1.000
# 0.028 |         0.208 |   0.024 |           0.067 |     0.024 |    0.513 |            subject:offset |            offset |  0.797 |  2 |  0.398 |     4.672 |   0.014 | 0.921
# 0.157 |         0.594 |   0.050 |           0.129 |     0.050 |    1.209 |            subject:offset |         Residuals |  4.435 | 52 |  0.085 |           |         | 1.000
# 0.062 |         0.366 |   0.060 |           0.152 |     0.060 |    0.760 |        subject:shift_size |        shift_size |  1.751 |  1 |  1.751 |     9.882 |   0.004 | 1.000
# 0.164 |         0.603 |   0.109 |           0.246 |     0.110 |    1.232 |        subject:shift_size |         Residuals |  4.607 | 26 |  0.177 |           |         | 1.000
# 0.015 |         0.125 |   0.011 |           0.032 |     0.011 |    0.378 | subject:offset:shift_size | offset:shift_size |  0.433 |  2 |  0.217 |     3.715 |   0.031 | 0.682
#       |               |         |                 |           |          | subject:offset:shift_size |         Residuals |  3.033 | 52 |  0.058 |           |         |

# Pairwise t-tests for main effect of timing offset on C
pairwise.t.test(dat2$C, dat2$offset, p.adj="holm", paired=T, alternative="two.sided")
#    -15    0
# 0  0.9516 -
# 15 0.0194 0.0053
# P value adjustment method: holm

# Analysis of the offset x shift size interaction
fits <- read.csv("fits.csv")
fits <- fits %>% filter(!subject %in% c(21) & version >= 1.1)
t.test(fits[fits$shift_size==1, 'C_slope'], fits[fits$shift_size==0.5, 'C_slope'], paired=TRUE)
# t = 0.17829, df = 26, p-value = 0.8599
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
# -0.006023409  0.007167562
# sample estimates:
# mean difference
# 0.0005720765

###
# Figure 6 Analysis
###

data <- read.csv("fits.csv")
data <- data %>% filter(!subject %in% c(21) & version >= 1.1 & shift_size == 'pooled')
cor.test(data$jnd, data$C_slope)
# t = 0.13981, df = 25, p-value = 0.8899
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.3558430  0.4036769
# sample estimates:
# cor
# 0.02795063