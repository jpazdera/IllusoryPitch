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
data <- data %>% filter(!subject %in% c(15, 22) & rt < 10000)
data$subject <- factor(data$subject, ordered=F)
data$offset <- factor(data$offset, ordered=F)
data$pitch_shift <- factor(data$pitch_shift, ordered=F)
data$correct <- factor(data$correct, ordered=F)

model <- lmer(rt ~ 1 + offset * pitch_shift * correct + (1 + correct | subject), data=data, REML=FALSE)
anova(model)
# Type III Analysis of Variance Table with Satterthwaite's method
#                              Sum Sq  Mean Sq NumDF  DenDF F value    Pr(>F)
# offset                      9363030  4681515     2 6639.3  4.5539 0.0105590 *
# pitch_shift                 3609689  3609689     1 6653.0  3.5113 0.0609958 .
# correct                    19368789 19368789     1   25.6 18.8408 0.0001972 ***
# offset:pitch_shift          1197167   598584     2 6648.9  0.5823 0.5586595
# offset:correct              2645128  1322564     2 6637.5  1.2865 0.2763018
# pitch_shift:correct           11028    11028     1 6646.2  0.0107 0.9175107
# offset:pitch_shift:correct 16504173  8252087     2 6641.6  8.0271 0.0003297 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###
# Analysis to explain the 3-way interaction
###

# Now for some fun programmer magic --- if you label the conditions with the following numerical values:
# Early = 1, Late = 0; Up = 1, Down = 0; Incorrect = 1, Correct = 0
# When summed, any even outcome is a bias-affirming response and any odd outcome is a bias-opposing response
# For example, an early/down/incorrect response sums to 2, and is bias-affirming because they rated an early tone as high
# The 3-factor combinations that are categorized as bias affirming, bias opposing, and neutral are as follows:
# BA = EUC, LDC, EDI, LUI = 1+1+0, 0+0+0, 1+0+1, 0+1+1
# BO = EDC, LUC, EUI, LDI = 1+0+0, 0+1+0, 1+1+1, 0+0+1
# N = OUC, ODC, OUI, ODI
data$response_type <- NA
bias_affirming_mask <- ((data$offset == -15) + (data$pitch_shift=='+')) + (data$correct == 'False') %% 2 == 0
data[bias_affirming_mask, 'response_type'] <- 'affirming'
data[!bias_affirming_mask, 'response_type'] <- 'opposing'
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
#   0.813 |         0.896 |   0.765 |           0.840 |     0.766 |    2.941 |               subject |     Residuals | 1.038e+07 | 27 | 3.846e+05 |           |         |     1
#   0.092 |         0.496 |   0.089 |           0.378 |     0.089 |    0.991 | subject:response_type | response_type | 1.180e+06 |  2 | 5.900e+05 |    26.531 |  < .001 |     1
#         |               |         |                 |           |          | subject:response_type |     Residuals | 1.201e+06 | 54 | 22237.606 |           |         |

# Pairwise t-tests for reaction time differences by response type
pairwise.t.test(subj_means$rt, subj_means$response_type, p.adj="holm", paired=T, alternative="two.sided")
#          affirming neutral
# neutral  1.6e-05   -
# opposing 1.6e-05   0.25
group_by(subj_means, response_type) %>%
  summarize(m=mean(rt), sd=sd(rt))
# response_type     m    sd
# <fct>         <dbl> <dbl>
# 1 affirming      871.  281.
# 2 neutral       1137.  400.
# 3 opposing      1104.  436.