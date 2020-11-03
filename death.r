#### Preliminaries: Load packages ####

setwd("C:/Users/hichem/Documents/Projects/Covid-transmisson project/code/Codes-Covid19_paper")

rm(list=ls())
library(magrittr)
library(foreign)
library(RColorBrewer)
library(sp)
library(lattice)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(nlme)
library(mgcv)
require(foreign)
require(ggplot2)
require(MASS)
library(car)
library(lme4)
library(gamm4)
library(psych)
library(foreign)
library(MASS)
library(sandwich)
library(compactr)
library(glmmADMB)
library(xtable)
library(DHARMa)
library(nlme)
library(lme4)
library(car)
library(glmmTMB)

eud <- read.csv("COVID19_European data_CLEAN_DEATHS_CSV.csv", sep=",", dec = ".", header = TRUE)

source("death_exploration.r")
source("death_categorizing_Lockdown.r")
# source("death_scaling.r")

eud_s <- eud

eud_s$POP_DENS <- quantcut( eud$POP_DENS, 4)

str(eud)
dim(eud)

####### GLMM using lme4 ########

# eud_s<-eud_s[eud_s$COVID19_D>0 ,] ## This is for the logNormal distribution

hist(eud_s$COVID19_D)
shapiro.test(log(eud_s$COVID19_D))

#################################################
death.glmer.nb = glmer.nb(COVID19_D ~ # COVID19_CCONF + 
                            POP_DENS + 
                            # log(POPULATION) + 
                            P_POP_60 + 
                            # P_POP_0_14   + 
                            # P_POP_15_29  + 
                            # P_POP_30_44  + 
                            # P_POP_45_59  +  
                            P_MALES + 
                            NO2 + 
                            # PM25 +
                            NTI + 
                            PRESSURE +
                            # TEMP + 
                            SOLAR_RAD + 
                            PRECIPITATION + 
                            # WIND +
                            LAI +
                            # DURATION_LD_CAT +
                            # LAG_LD_CAT +
                            (1+COVID19_D|CODE) , nAGQ=0,
                          data = eud_s, control = glmerControl(optimizer ="bobyqa"))

summary(death.glmer.nb)
par(mfrow=c(1,2))
plot(fitted(death.glmer.nb),residuals(death.glmer.nb)) 
qqnorm(residuals(death.glmer.nb))

vif(death.glmer.nb) 
exp(death.glm.nb$coefficients) 

print(death.glm.nb, correlation=TRUE)

xtable(death.glm.nb)

library(DHARMa)      
simulationOutput <- simulateResiduals(fittedModel = death.glmer.nb, plot = T)
testUniformity(simulationOutput)
testOutliers(simulationOutput)
testDispersion(simulationOutput)

# % latex table generated in R 4.0.2 by xtable 1.8-4 package
# % Fri Oct 30 21:38:30 2020
# \begin{table}[ht]
# \centering
# \begin{tabular}{rrrrr}
# \hline
# & Estimate & Std. Error & z value & Pr($>$$|$z$|$) \\ 
# \hline
# (Intercept) & 1.6075 & 3.1372 & 0.51 & 0.6084 \\ 
# COVID19\_CCONF & 0.0003 & 0.0000 & 11.41 & 0.0000 \\ 
# POP\_DENS(93,158] & 0.1291 & 0.1046 & 1.23 & 0.2173 \\ 
# POP\_DENS(158,334] & 0.2068 & 0.1192 & 1.73 & 0.0829 \\ 
# POP\_DENS(334,1.1e+03] & 0.1325 & 0.1529 & 0.87 & 0.3860 \\ 
# POP\_DENS(1.1e+03,2.03e+04] & -0.1464 & 0.2336 & -0.63 & 0.5307 \\ 
# POPULATION & 0.0000 & 0.0000 & 2.83 & 0.0047 \\ 
# P\_POP\_60 & -0.0214 & 0.0101 & -2.11 & 0.0347 \\ 
# P\_MALES & 0.0472 & 0.0532 & 0.89 & 0.3748 \\ 
# NO2 & 0.1381 & 0.0283 & 4.88 & 0.0000 \\ 
# PM25 & 0.0418 & 0.2987 & 0.14 & 0.8887 \\ 
# NLI & 0.0053 & 0.0051 & 1.04 & 0.2969 \\ 
# PRESSURE & 4.8468 & 1.6017 & 3.03 & 0.0025 \\ 
# TEMP & -0.4699 & 0.3207 & -1.47 & 0.1429 \\ 
# SOLAR\_RAD & 0.0000 & 0.0001 & 0.73 & 0.4663 \\ 
# PRECIPITATION & 0.0067 & 0.0026 & 2.54 & 0.0111 \\ 
# WIND & -0.3110 & 0.0551 & -5.64 & 0.0000 \\ 
# LAI & -0.0627 & 0.0782 & -0.80 & 0.4226 \\ 
# DURATION\_LD\_CATLow & -2.0374 & 0.1611 & -12.65 & 0.0000 \\ 
# DURATION\_LD\_CATMedium & 0.2421 & 0.1617 & 1.50 & 0.1343 \\ 
# LAG\_LD\_CATLow & -1.2069 & 0.1528 & -7.90 & 0.0000 \\ 
# \hline
# \end{tabular}
# \end{table}


################################################# COVID19_CCONF

death.glmer.nb1 = glmer.nb(COVID19_CCONF ~ # COVID19_CCONF + 
                            POP_DENS + 
                            # log(POPULATION) + 
                            P_POP_60 + 
                            # P_POP_0_14   + 
                            # P_POP_15_29  + 
                            # P_POP_30_44  + 
                            # P_POP_45_59  +  
                            P_MALES + 
                            NO2 + 
                            # PM25 +
                            # NTI + 
                            PRESSURE +
                            # TEMP + 
                            SOLAR_RAD + 
                            PRECIPITATION + 
                            # WIND +
                            LAI +
                            # DURATION_LD_CAT +
                            # LAG_LD_CAT +
                            (1+COVID19_D|CODE) , nAGQ=0,
                          data = eud_s, control = glmerControl(optimizer ="bobyqa"))

summary(death.glmer.nb1)
par(mfrow=c(1,2))
plot(fitted(death.glmer.nb),residuals(death.glmer.nb1))
qqnorm(residuals(death.glmer.nb1))

vif(death.glmer.nb1)
exp(death.glm.nb1$coefficients)

print(death.glm.nb1, correlation=TRUE)

xtable(death.glm.nb1)



coef = death.glmer.nb@beta                ### new line code to extract coefficients 

### 
library(DHARMa)

as.data.frame(exp(death.glmer.nb$beta))
# as.data.frame(exp(death.glmer.nb$beta$fixed))


exp(as.data.frame(death.glmer.nb1$coefficients$random$CODE)) # this syntax is not correct !!!

simulationOutput <- simulateResiduals(fittedModel = death.glmer.nb, plot = T)

testUniformity(simulationOutput)   # error should be normally distributed 
testOutliers(simulationOutput)     # 
testDispersion(simulationOutput)   # 

####### GLM model with MASS ####### 

summary(fit) # display results

death.glm.nb = glm.nb(COVID19_D ~ COVID19_CCONF + # I have included: COVID19_CCONF!
                     POP_DENS + 
                     POPULATION + 
                     P_POP_60 + 
                     P_MALES + 
                     NO2 + 
                     PM25 +
                     NTI + 
                     PRESSURE +
                     TEMP + 
                     SOLAR_RAD + 
                     PRECIPITATION + 
                     WIND +
                     LAI  +
                     DURATION_LD_CAT + 
                     LAG_LD_CAT + 
                     (1 + COVID19_D | CODE), (1 | state ),
                 data = eud_s, 
                 na.action = na.exclude, 
                 link="log",
                 init.theta = 1.2, control = glm.control(epsilon = 1e-2, maxit = 1000, trace = FALSE))



### note: There is not enough variation in dependent variable 
### with only one value. So, you need to drop that variable, 
### irrespective of whether that is numeric or character or factor variable.

summary(death.glm.nb)

vif(death.glm.nb)

xtable(death.glm.nb)

exp(death.glm.nb$coefficients)

simulationOutput <- simulateResiduals(fittedModel = death.glm.nb, plot = T)

testUniformity(simulationOutput)
testOutliers(simulationOutput)
testDispersion(simulationOutput)


############# glmmTMB function (glmmTMB package) ###############

death.glmmTMB = glmmTMB(COVID19_D ~ 
                     POP_DENS + 
                     POPULATION + 
                     P_POP_60 + 
                     P_MALES + 
                     NO2 + 
                     PM25 +
                     NLI + 
                     PRESSURE +
                     TEMP + 
                     SOLAR_RAD + 
                     PRECIPITATION + 
                     WIND +
                     LAI +
                     (1|CODE),
                  data = eud_s, 
                  family=nbinom1(link = "log"))

summary(death.glmmTMB) #### Suuuper bad fitting 

vif(death.glmmTMB)

simulationOutput <- simulateResiduals(fittedModel = death.glmmTMB, plot = T)

testUniformity(simulationOutput)
testOutliers(simulationOutput)
testDispersion(simulationOutput)
testQuantiles(simulationOutput)

eud_s$COVID19_CCONF

