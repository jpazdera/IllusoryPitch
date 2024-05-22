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

data <- read.csv("fits.csv")
data <- data %>% filter(!subject %in% c(21) & version >= 1.1)
t.test(data[data$shift_size==1, 'C_slope'], data[data$shift_size==0.5, 'C_slope'])

modelC <- lmer(C ~ 1 + poly(interval, 1) * difficulty + (1 + poly(interval, 1) | subject),
              data=dat2, REML=FALSE)

library(sjPlot)

# Plot the random slopes
plot_model(modelC, type = "re")

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
