setwd("/Users/oliverinaldi/Documents/GitHub/IllusoryPitch/E2/data")
options(contrasts=c("contr.sum", "contr.poly"))
dat <- read.csv("scores.csv")
library(dplyr)
library(tidyverse)
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models
library(car)
dat2 <- dat %>% filter(!row_number() %in% c(55, 56, 57, 58, 59, 60))
dat2$subject <- as.factor(dat2$subject)
dat2$difficulty <- as.factor(dat2$difficulty)
modelC <- lmer(C ~ 1 + poly(interval, 1) * difficulty + (1 | subject),
              data=dat, REML=FALSE)
anova(modelC)

poly(grp.means <- with(dat,
                   tapply(C, list(interval, difficulty), mean)))
with(dat,
     interaction.plot(x.factor=interval,
                      trace.factor=difficulty,
                      response=C))
#C
# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
# poly(interval, 1)            0.11331 0.11331     1 150.09  1.3483 0.2474178    
# difficulty                   1.14513 1.14513     1 166.15 13.6261 0.0003021 ***
# poly(interval, 1):difficulty 0.00065 0.00065     1 150.09  0.0077 0.9301180    

modeldprime <- lmer(dprime ~ 1 + poly(interval, 1) * difficulty + (1 | subject),
              data=dat, REML=FALSE)
anova(modeldprime)

poly(grp.means <- with(dat,
                       tapply(dprime, list(interval, difficulty), mean)))
with(dat,
     interaction.plot(x.factor=interval,
                      trace.factor=difficulty,
                      response=dprime))

# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq Mean Sq NumDF  DenDF  F value Pr(>F)    
# poly(interval, 1)             0.1463  0.1463     1 150.64   0.5326 0.4666    
# difficulty                   30.4797 30.4797     1 156.99 110.9603 <2e-16 ***
# poly(interval, 1):difficulty  0.0214  0.0214     1 150.64   0.0779 0.7806    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1