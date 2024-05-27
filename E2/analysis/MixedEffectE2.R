library(sjstats)
library(dplyr)
library(tidyverse)
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models
library(car)

#setwd("/Users/oliverinaldi/Documents/GitHub/IllusoryPitch/E2/data")
setwd("/Users/jpazd/Documents/git/IllusoryPitch/E2/data")
options(contrasts=c("contr.sum", "contr.poly"))

###
# Figure 4 Analysis
###

data <- read.csv("fits.csv")
data <- data %>% filter(!subject %in% c(21) & version >= 1.1 & shift_size == 1)
cor.test(data$jnd, data$C_slope)
# t = -0.75802, df = 25, p-value = 0.4555
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.5013479  0.2440226
# sample estimates:
# cor
# -0.149891

###
# Figure 5 & 6 Analysis
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

data <- read.csv("fits.csv")
data <- data %>% filter(!subject %in% c(21) & version >= 1.1)
t.test(data[data$shift_size==1, 'C_slope'], data[data$shift_size==0.5, 'C_slope'], paired=TRUE)
# t = 0.17829, df = 26, p-value = 0.8599
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
# -0.006023409  0.007167562
# sample estimates:
# mean difference
# 0.0005720765

t.test(data$C_slope, mu=0)
# t = 2.6844, df = 53, p-value = 0.009676
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
# 0.001267931 0.008762749
# sample estimates:
# mean of x
# 0.00501534

dat2$offset <- as.factor(dat2$offset)
modelC <- lmer(C ~ 1 + poly(offset, 1) * shift_size + (1 + poly(offset, 1) + shift_size | subject),
              data=dat2, REML=FALSE)
anova(modelC)
library(sjPlot)

# Plot the random slopes
plot_model(modelC, type = "est")

poly(grp.means <- with(dat2,
                   tapply(C, list(interval, difficulty), mean)))
with(dat2,
     interaction.plot(x.factor=interval,
                      trace.factor=difficulty,
                      response=C))
#C
# Type III Analysis of Variance Table with Satterthwaite's method
#                              Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
# poly(interval, 1)            1.0393  1.0393     1 145.02 12.3793 0.0005801 ***
# difficulty                   1.2791  1.2791     1 160.84 15.2345 0.0001394 ***
# poly(interval, 1):difficulty 0.0015  0.0015     1 145.02  0.0179 0.8937508 

dat2$interval <- as.factor(dat2$interval)
modeldprime <- lmer(dprime ~ 1 + interval * difficulty + (1 | subject),
              data=dat2, REML=FALSE)
anova(modeldprime)

poly(grp.means <- with(dat2,
                       tapply(dprime, list(interval, difficulty), mean)))
with(dat2,
     interaction.plot(x.factor=interval,
                      trace.factor=difficulty,
                      response=dprime))
# Dprime
# Type III Analysis of Variance Table with Satterthwaite's method
#                      Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)    
# interval             0.7751  0.3875     2 145.37  1.4204 0.2449    
# difficulty          27.2652 27.2652     1 152.50 99.9328 <2e-16 ***
# interval:difficulty  0.2794  0.1397     2 145.37  0.5120 0.6004    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
