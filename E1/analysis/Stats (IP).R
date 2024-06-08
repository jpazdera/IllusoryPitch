library(sjstats)  # For anova_stats() function
library(dplyr)
library(tidyverse)
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models
library(car)

# Set contrasts and working directory
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryPitch/E1/data")

EXCLUDED <- c(13, 15, 22, 31, 35)

###
# Bias and Sensitivity Analysis
###

# Load processed scores
data <- read.csv("scores.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% EXCLUDED)

# Convert independent variables to factors
data$subject <- factor(data$subject, ordered=F)
data$octave <- factor(data$octave, ordered=F)
data$offset <- factor(data$offset, ordered=F)

# Repeated measures ANOVA (C)
model <- aov(C ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                 group |          term |  sumsq | df | meansq | statistic | p.value | power
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.168 |         0.592 |   0.110 |           0.233 |     0.110 |    1.203 |               subject |     Residuals |  5.091 | 24 |  0.212 |           |         | 1.000
# 0.153 |         0.568 |   0.147 |           0.290 |     0.148 |    1.147 |        subject:offset |        offset |  4.623 |  2 |  2.311 |    10.513 |  < .001 | 1.000
# 0.348 |         0.750 |   0.232 |           0.390 |     0.232 |    1.733 |        subject:offset |     Residuals | 10.553 | 48 |  0.220 |           |         | 1.000
# 0.058 |         0.333 |   0.055 |           0.133 |     0.056 |    0.707 |        subject:octave |        octave |  1.757 |  1 |  1.757 |     8.986 |   0.006 | 0.998
# 0.155 |         0.572 |   0.097 |           0.211 |     0.097 |    1.156 |        subject:octave |     Residuals |  4.694 | 24 |  0.196 |           |         | 1.000
# 0.002 |         0.017 |  -0.003 |          -0.008 |    -0.003 |    0.132 | subject:offset:octave | offset:octave |  0.061 |  2 |  0.031 |     0.417 |   0.661 | 0.118
#       |               |         |                 |           |          | subject:offset:octave |     Residuals |  3.515 | 48 |  0.073 |           |         |

# Repeated measures ANOVA (d')
model <- aov(dprime ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                 group |          term |   sumsq | df | meansq | statistic | p.value | power
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.798 |         0.937 |   0.770 |           0.821 |     0.771 |    3.856 |               subject |     Residuals | 153.438 | 24 |  6.393 |           |         | 1.000
# 0.001 |         0.011 |  -0.002 |          -0.010 |    -0.002 |    0.106 |        subject:offset |        offset |   0.115 |  2 |  0.058 |     0.260 |   0.772 | 0.093
# 0.055 |         0.508 |   0.002 |           0.011 |     0.002 |    1.017 |        subject:offset |     Residuals |  10.665 | 48 |  0.222 |           |         | 0.996
# 0.003 |         0.049 |   0.002 |           0.010 |     0.002 |    0.228 |        subject:octave |        octave |   0.535 |  1 |  0.535 |     0.744 |   0.397 | 0.352
# 0.090 |         0.626 |   0.063 |           0.273 |     0.063 |    1.293 |        subject:octave |     Residuals |  17.260 | 24 |  0.719 |           |         | 1.000
# 0.000 |         0.000 |  -0.002 |          -0.014 |    -0.002 |    0.002 | subject:offset:octave | offset:octave |   0.000 |  2 |  0.000 |     0.000 |  > .999 | 0.050
#       |               |         |                 |           |          | subject:offset:octave |     Residuals |  10.318 | 48 |  0.215 |           |         |

# Pairwise t-tests for main effect of timing offset on C
pairwise.t.test(data$C, data$offset, p.adj="holm", paired=T, alternative="two.sided")
#     -15     0
# 0  0.00501 -
# 15 8.3e-05 0.00043
# P value adjustment method: holm

###
# Analysis of Percent "Higher"
###

data <- read.csv("response_data.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% EXCLUDED)
data$subject <- factor(data$subject, ordered=F)
data$offset <- factor(data$offset, ordered=F)
data$pitch_shift <- factor(data$pitch_shift, ordered=F)
data$response <- as.logical(data$response, ordered=F)

subj_means <- group_by(data, subject, offset, pitch_shift) %>%
  summarize(response=mean(response))

model <- aov(response ~ 1 + offset * pitch_shift + Error(subject / (offset * pitch_shift)), data=subj_means)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                      group |               term |  sumsq | df | meansq | statistic | p.value | power
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.021 |         0.718 |   0.017 |           0.395 |     0.017 |    1.594 |                    subject |          Residuals |  0.364 | 24 |  0.015 |           |         | 1.000
# 0.022 |         0.727 |   0.022 |           0.456 |     0.022 |    1.632 |             subject:offset |             offset |  0.381 |  2 |  0.191 |     8.629 |   0.001 | 1.000
# 0.061 |         0.881 |   0.053 |           0.672 |     0.053 |    2.722 |             subject:offset |          Residuals |  1.061 | 48 |  0.022 |           |         | 1.000
# 0.727 |         0.989 |   0.727 |           0.966 |     0.727 |    9.371 |        subject:pitch_shift |        pitch_shift | 12.572 |  1 | 12.572 |   109.078 |  < .001 | 1.000
# 0.160 |         0.951 |   0.156 |           0.858 |     0.156 |    4.396 |        subject:pitch_shift |          Residuals |  2.766 | 24 |  0.115 |           |         | 1.000
# 0.000 |         0.001 |   0.000 |          -0.013 |     0.000 |    0.035 | subject:offset:pitch_shift | offset:pitch_shift |  0.000 |  2 |  0.000 |     0.029 |   0.971 | 0.054
#       |               |         |                 |           |          | subject:offset:pitch_shift |          Residuals |  0.143 | 48 |  0.003 |           |         |

###
# Sensitivity/Bias Correlation Analysis
###

data <- read.csv("subj_scores.csv", fileEncoding="UTF-8-BOM")
# With poor performers included
cor.test(data$dprime, data$C_slope)
# t = -2.1693, df = 28, p-value = 0.0387
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.65067489 -0.02207416
# sample estimates:
# cor
# -0.3793267
data <- data %>% filter(!subject %in% EXCLUDED)
# With poor performers excluded
cor.test(data$dprime, data$C_slope)
# t = -1.6778, df = 23, p-value = 0.1069
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.6416295  0.0746537
# sample estimates:
# cor
# -0.330218

###
# Reaction Time Analysis
###

data <- read.csv("response_data.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% EXCLUDED & rt <= 5000 & correct=='True')
data$subject <- factor(data$subject, ordered=F)
data$offset <- factor(data$offset, ordered=F)
data$pitch_shift <- factor(data$pitch_shift, ordered=F)

subj_means <- group_by(data, subject, offset, pitch_shift) %>%
  summarize(rt=mean(rt))

model <- aov(rt ~ 1 + offset * pitch_shift + Error(subject / (offset * pitch_shift)), data=subj_means)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                      group |               term |     sumsq | df |    meansq | statistic | p.value | power
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.706 |         0.915 |   0.672 |           0.766 |     0.673 |    3.272 |                    subject |          Residuals | 8.236e+06 | 24 | 3.431e+05 |           |         | 1.000
# 0.003 |         0.048 |   0.001 |           0.003 |     0.001 |    0.225 |             subject:offset |             offset | 39058.527 |  2 | 19529.263 |     1.322 |   0.276 | 0.267
# 0.061 |         0.480 |  -0.005 |          -0.026 |    -0.005 |    0.960 |             subject:offset |          Residuals | 7.090e+05 | 48 | 14770.404 |           |         | 0.989
# 0.009 |         0.117 |   0.007 |           0.035 |     0.007 |    0.364 |        subject:pitch_shift |        pitch_shift | 1.021e+05 |  1 | 1.021e+05 |     1.782 |   0.194 | 0.714
# 0.118 |         0.641 |   0.085 |           0.292 |     0.085 |    1.337 |        subject:pitch_shift |          Residuals | 1.376e+06 | 24 | 57325.770 |           |         | 1.000
# 0.037 |         0.358 |   0.034 |           0.142 |     0.034 |    0.747 | subject:offset:pitch_shift | offset:pitch_shift | 4.289e+05 |  2 | 2.145e+05 |    13.379 |  < .001 | 0.998
#       |               |         |                 |           |          | subject:offset:pitch_shift |          Residuals | 7.694e+05 | 48 | 16029.586 |           |         |

###
# Analysis to explain the interaction
###

# Now for some fun programmer magic --- if you label the conditions with the following numerical values:
# Early = 1, Late = 0; Up = 1, Down = 0
# When summed, any even outcome is a bias-conforming response and any odd outcome is a bias-opposing response
# For example, an early/down/incorrect response sums to 2, and is bias-conforming because they rated an early tone as high
# The combinations that are categorized as bias conforming, bias opposing, and neutral are as follows:
# BC = EU, LD = 1+1, 0+0
# BO = ED, LU = 1+0, 0+1
# BN = OU, OD
data$response_type <- NA
bias_conforming_mask <- ((data$offset == -15) + (data$pitch_shift=='+')) %% 2 == 0
data[bias_conforming_mask, 'response_type'] <- 'conforming'
data[!bias_conforming_mask, 'response_type'] <- 'opposing'
# All trials with an offset of 0 get their labels overwritten as bias neutral (the above process would have mislabeled them, but we correct that now)
neutral_mask <- data$offset == 0
data[neutral_mask, 'response_type'] <- 'neutral'
data$response_type <- factor(data$response_type)

# Average reaction times for each subject and response type
subj_means <- group_by(data, subject, response_type) %>%
  summarize(rt=mean(rt))

subj_means %>% group_by(response_type) %>% summarize(m=mean(rt), sd=sd(rt))
# response_type     m    sd
# 1 conforming     850.  241.
# 2 neutral        949.  255.
# 3 opposing       980.  250.

# Pairwise t-tests for reaction time differences by response type
t.test(subj_means[subj_means$response_type=='conforming', 'rt'] - subj_means[subj_means$response_type=='neutral', 'rt'], mu=0)
# t = -3.8821, df = 24, p-value = 0.0007093 (p_adj = .001)
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# -150.71627  -46.08764
# sample estimates:
# mean of x
# -98.40196
t.test(subj_means[subj_means$response_type=='conforming', 'rt'] - subj_means[subj_means$response_type=='opposing', 'rt'], mu=0)
# t = -4.4241, df = 24, p-value = 0.0001796 (p_adj < .001)
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# -190.00256  -69.11938
# sample estimates:
# mean of x
# -129.561
t.test(subj_means[subj_means$response_type=='neutral', 'rt'] - subj_means[subj_means$response_type=='opposing', 'rt'], mu=0)
# t = -1.224, df = 24, p-value = 0.2328 (p_adj = .233)
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# -83.69851  21.38048
# sample estimates:
# mean of x
# -31.15902