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
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                 group |          term |  sumsq | df | meansq | statistic | p.value | power
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   0.195 |         0.639 |   0.140 |           0.290 |     0.140 |    1.331 |               subject |     Residuals |  9.045 | 27 |  0.335 |           |         | 1.000
#   0.175 |         0.613 |   0.170 |           0.333 |     0.171 |    1.260 |        subject:offset |        offset |  8.101 |  2 |  4.050 |    12.709 |  < .001 | 1.000
#   0.371 |         0.771 |   0.261 |           0.433 |     0.261 |    1.836 |        subject:offset |     Residuals | 17.210 | 54 |  0.319 |           |         | 1.000
#   0.045 |         0.290 |   0.043 |           0.111 |     0.043 |    0.639 |        subject:octave |        octave |  2.084 |  1 |  2.084 |    11.788 |   0.002 | 0.997
#   0.103 |         0.483 |   0.048 |           0.123 |     0.048 |    0.967 |        subject:octave |     Residuals |  4.773 | 27 |  0.177 |           |         | 0.999
#   0.001 |         0.006 |  -0.003 |          -0.010 |    -0.003 |    0.076 | subject:offset:octave | offset:octave |  0.029 |  2 |  0.015 |     0.154 |   0.857 | 0.074
#         |               |         |                 |           |          | subject:offset:octave |     Residuals |  5.104 | 54 |  0.095 |           |         |

# Repeated measures ANOVA (d')
model <- aov(dprime ~ 1 + offset * octave + Error(subject / (offset * octave)), data=data)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                 group |          term |   sumsq | df | meansq | statistic | p.value | power
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   0.820 |         0.932 |   0.789 |           0.809 |     0.790 |    3.697 |               subject |     Residuals | 201.878 | 27 |  7.477 |           |         | 1.000
#   0.000 |         0.006 |  -0.002 |          -0.010 |    -0.002 |    0.080 |        subject:offset |        offset |   0.095 |  2 |  0.048 |     0.229 |   0.796 | 0.077
#   0.046 |         0.432 |  -0.014 |          -0.084 |    -0.014 |    0.872 |        subject:offset |     Residuals |  11.223 | 54 |  0.208 |           |         | 0.976
#   0.003 |         0.043 |   0.002 |           0.008 |     0.002 |    0.211 |        subject:octave |        octave |   0.659 |  1 |  0.659 |     1.023 |   0.321 | 0.342
#   0.071 |         0.541 |   0.041 |           0.179 |     0.041 |    1.085 |        subject:octave |     Residuals |  17.396 | 27 |  0.644 |           |         | 1.000
#   0.000 |         0.007 |  -0.002 |          -0.010 |    -0.002 |    0.084 | subject:offset:octave | offset:octave |   0.103 |  2 |  0.052 |     0.189 |   0.829 | 0.079
#         |               |         |                 |           |          | subject:offset:octave |     Residuals |  14.769 | 54 |  0.274 |           |         |

# Pairwise t-tests for main effect of timing offset on C
pairwise.t.test(data$C, data$offset, p.adj="holm", paired=T, alternative="two.sided")
#     -15     0
# 0  0.00138 -
# 15 1.6e-05 0.00027
# P value adjustment method: holm

###
# Analysis of Percent "Higher"
###

data <- read.csv("response_data.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% c(15, 22))
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
#   0.041 |         0.837 |   0.037 |           0.598 |     0.037 |    2.266 |                    subject |          Residuals |  0.773 | 27 |  0.029 |           |         | 1.000
#   0.037 |         0.822 |   0.037 |           0.595 |     0.037 |    2.146 |             subject:offset |             offset |  0.693 |  2 |  0.347 |    11.231 |  < .001 | 1.000
#   0.089 |         0.917 |   0.081 |           0.764 |     0.081 |    3.327 |             subject:offset |          Residuals |  1.667 | 54 |  0.031 |           |         | 1.000
#   0.620 |         0.987 |   0.620 |           0.961 |     0.620 |    8.763 |        subject:pitch_shift |        pitch_shift | 11.563 |  1 | 11.563 |    82.139 |  < .001 | 1.000
#   0.204 |         0.962 |   0.200 |           0.888 |     0.200 |    5.024 |        subject:pitch_shift |          Residuals |  3.801 | 27 |  0.141 |           |         | 1.000
#   0.000 |         0.000 |   0.000 |          -0.012 |     0.000 |    0.019 | subject:offset:pitch_shift | offset:pitch_shift |  0.000 |  2 |  0.000 |     0.009 |   0.991 | 0.051
#         |               |         |                 |           |          | subject:offset:pitch_shift |          Residuals |  0.151 | 54 |  0.003 |           |         |

###
# Figure 2 Analysis
###

data <- read.csv("subj_scores.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% c(15, 22))

cor.test(data$dprime, data$C_slope)
# t = -2.2667, df = 26, p-value = 0.03196
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.6767316 -0.0390539
# sample estimates:
# cor
# -0.4062123

###
# Figure 3 Analysis
###

data <- read.csv("response_data.csv", fileEncoding="UTF-8-BOM")
data <- data %>% filter(!subject %in% c(15, 22) & rt < 5000)
data$subject <- factor(data$subject, ordered=F)
data$offset <- factor(data$offset, ordered=F)
data$pitch_shift <- factor(data$pitch_shift, ordered=F)
data$correct <- factor(data$correct, ordered=F)

model <- lmer(rt ~ 1 + offset * pitch_shift * correct + (1 + correct | subject), data=data, REML=FALSE)
anova(model)
# Type III Analysis of Variance Table with Satterthwaite's method
#                              Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# offset                      7972994 3986497     2 6502.4  8.2272 0.0002701 ***
# pitch_shift                 1036720 1036720     1 6515.4  2.1396 0.1435915
# correct                     8823866 8823866     1   25.9 18.2105 0.0002330 ***
# offset:pitch_shift           316684  158342     2 6511.7  0.3268 0.7212524
# offset:correct              4143980 2071990     2 6501.4  4.2761 0.0139356 *
# pitch_shift:correct          564768  564768     1 6510.8  1.1656 0.2803567
# offset:pitch_shift:correct 10798310 5399155     2 6507.0 11.1426 1.476e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###
# Analysis to explain the 3-way interaction
###

# Now for some fun programmer magic --- if you label the conditions with the following numerical values:
# Early = 1, Late = 0; Up = 1, Down = 0; Incorrect = 1, Correct = 0
# When summed, any even outcome is a bias-conforming response and any odd outcome is a bias-opposing response
# For example, an early/down/incorrect response sums to 2, and is bias-conforming because they rated an early tone as high
# The 3-factor combinations that are categorized as bias conforming, bias opposing, and neutral are as follows:
# BC = EUC, LDC, EDI, LUI = 1+1+0, 0+0+0, 1+0+1, 0+1+1
# BO = EDC, LUC, EUI, LDI = 1+0+0, 0+1+0, 1+1+1, 0+0+1
# BN = OUC, ODC, OUI, ODI
data$response_type <- NA
bias_conforming_mask <- ((data$offset == -15) + (data$pitch_shift=='+')) + (data$correct == 'False') %% 2 == 0
data[bias_conforming_mask, 'response_type'] <- 'conforming'
data[!bias_conforming_mask, 'response_type'] <- 'opposing'
# All trials with an offset of 0 get their labels overwritten as bias neutral (the above process would have mislabeled them, but we correct that now)
neutral_mask <- data$offset == 0
data[neutral_mask, 'response_type'] <- 'neutral'
data$response_type <- factor(data$response_type)

subj_means <- group_by(data, subject, response_type) %>%
  summarize(rt=mean(rt))
model <- aov(rt ~ 1 + response_type + Error(subject / response_type), data=subj_means)
anova_stats(model)
# etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f |                 group |          term |     sumsq | df |    meansq | statistic | p.value | power
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   0.825 |         0.906 |   0.781 |           0.855 |     0.783 |    3.105 |               subject |     Residuals | 5.216e+06 | 27 | 1.932e+05 |           |         |     1
#   0.089 |         0.510 |   0.086 |           0.392 |     0.086 |    1.020 | subject:response_type | response_type | 5.632e+05 |  2 | 2.816e+05 |    28.116 |  < .001 |     1
#         |               |         |                 |           |          | subject:response_type |     Residuals | 5.409e+05 | 54 | 10015.853 |           |         |

# Pairwise t-tests for reaction time differences by response type
t.test(subj_means[subj_means$response_type=='conforming', 'rt'] - subj_means[subj_means$response_type=='neutral', 'rt'], mu=0)
# t = -5.9306, df = 27, p-value = 2.541e-06 (p_adj<.001)
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# -253.7131 -123.2823
# sample estimates:
# mean of x
# -188.4977
t.test(subj_means[subj_means$response_type=='conforming', 'rt'] - subj_means[subj_means$response_type=='opposing', 'rt'], mu=0)
# t = -5.6893, df = 27, p-value = 4.816e-06 (p_adj<.001)
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# -209.00127  -98.20726
# sample estimates:
# mean of x
# -153.6043
t.test(subj_means[subj_means$response_type=='neutral', 'rt'] - subj_means[subj_means$response_type=='opposing', 'rt'], mu=0)
# t = 1.7294, df = 27, p-value = 0.09516 (p_adj=.095)
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# -6.505535 76.292373
# sample estimates:
# mean of x
# 34.89342

subj_means %>% group_by(response_type) %>% summarize(m=mean(rt), sd=sd(rt))
# response_type     m    sd
# 1 conforming     825.  247.
# 2 neutral       1013.  272.
# 3 opposing       978.  279.