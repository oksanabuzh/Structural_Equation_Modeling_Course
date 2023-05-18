# Solutions to the tasks in the lecture
#
# 
# ==============================================#
# ------- Structural equation & mixed model course
# 24.02.2023
# ==============================================#
# Day 05  --------------------------------------#
# Teaching and code:
# Oksana Buzhdygan (oksana.buzh@fu-berlin.de)
#
# ==============================================#

rm(list = ls()) # clears working environment

# Required packages----
library(tidyverse)
library(here)
# install.packages ("lavaan")
library(lavaan)

# Set up a local working directory
# setwd("C:/Users/Oksana/Nextcloud/SEM_final/Day 5/Data")
library(here)
path <- here::here()
path

# -------#
# Task 1-------
# -------#
data <- read_csv("Data/Grassl_data.csv")
str(data)

#1)

sem_mod <- '
    Grazing  ~  Elevation
    BareSoil  ~  a2*Grazing 
    Diversity ~ Elevation + a1*Grazing + a3*BareSoil
    
    # define direct, indirect and total effects  
          direct   := a1 
          indirect := a2*a3
          total    := direct + indirect
'
sem.fit <- sem(sem_mod, data=data)
#
summary(sem.fit, standardize = T)
#
# Defined Parameters:
#                 Estimate   Std.Err  z-value  P(>|z|)   Std.lv   Std.all
# direct           -37.259   11.739   -3.174    0.002   -37.259   -0.331
# indirect         -10.113    5.792   -1.746    0.081   -10.113   -0.090
# total            -47.372   10.583   -4.476    0.000   -47.372   -0.421
#
#
library(lavaanPlot)
#
lavaanPlot(model = sem.fit, coefs = TRUE,
           stand=TRUE)

#2)
varTable(sem.fit)
# exogenous variables: Grazing, BareSoil, Diversity
# endogenous variables: Elevation
#
#3)
#3.1. The variance explained by the model (R-Square):

summary(sem.fit, standardize = T, rsq = T)
# R-Square:
#                   Estimate 
# Grazing            0.064
# BareSoil           0.227
# Diversity          0.281
#
#3.2. The error variances (1 - R-Square): 
summary(sem.fit, standardize = T)
# Variances:
#                  Estimate   Std.Err  z-value  P(>|z|)   Std.lv   Std.all
# .Grazing            0.016    0.002    6.708    0.000     0.016    0.936
# .BareSoil           2.077    0.310    6.708    0.000     2.077    0.773
# .Diversity        155.967   23.250    6.708    0.000   155.967    0.719



library(semPlot)
semPaths(sem.fit, what='std', nCharNodes=5, sizeMan=12,
         edge.label.cex=1.7, curvePivot = TRUE, fade=FALSE)
#

# 3.3. The effects of the error: the path coefficients with the error variances (sqrt( - R-Square)).
#
R2<-lavInspect(sem.fit, what="rsquare")
sqrt(1-R2)

# or

PE <- parameterEstimates(sem.fit, stand=TRUE, rsq = T) # the error variances are in rows where "op" is "~~"
Er_Var <- PE$std.all[PE$op == "~~"]
sqrt(Er_Var)
#[1] 0.9676183 0.8793914 0.8477890 1.0000000
# or
PE <- parameterEstimates(sem.fit, stand=TRUE, rsq = TRUE) # the R-squared values are in rows where "op" is "r2"
R2 <- PE$est[PE$op == "r2"]
sqrt(1-R2)
# [1] 0.9676183 0.8793914 0.8477890
#
#
# -------#
# Task 2 ----
# -------#
library(piecewiseSEM)
data(keeley)
str(keeley)
#
# The task:
# The authors found relation among cover ~  age:
mod_lm <-lm(cover ~  age, data=keeley)


ggplot(keeley, aes(x = age, y = cover))+ 	
  geom_point()+ 	
  geom_smooth(method=lm)+ 	
theme_classic()


# But it might be indirect effect through the fire severity:
#
ggplot(keeley, aes(x = age, y = firesev))+ 	
  geom_point()+ 	
  theme_classic()

ggplot(keeley, aes(x =firesev , y =cover))+ 	
  geom_point()+ 	
  theme_classic()


### Solutions:
#
# model 1 in lavaan
sem_mod1 <- '
      firesev ~ age
      cover ~ firesev
'
#
# 1) Model identifability status:
# 1.1. t-rule
s=3  # number of observed parameters
# t_max - maximum number of parameters that can be estimated given s
t_max=s*(s+1)/2 
t_max
# t - number of parameters to be estimated by our model
t=2+3 # 2 regression coefficients for each effect + 3 variances to derive these coeficients  
t # number of parameters to be estimated by the model
t<t_max  # Model is overidentified (unsaturated)
#


#1.2. Model is recursive: all causal effects are unidirectional (arrows going in one direction)
#
#2) Is sample size (n) enough to fit this model?
# n=p+5 is minimum requirement
# p - number of path coefficients to be estimated
p=2 # regression coefficients in our model
n=p*5
n
str(keeley) # we have 90 observations, which is definitely sufficient for this model


# -------#
# Task 3 ----
# -------#

# model 1 in lavaan
sem_mod1 <- '
      firesev ~ age
      cover ~ firesev
'

#1) Fit the model in lavaan, get  path coeficients
sem_fit1 <- sem(sem_mod1, data=keeley)
#
library(lavaanPlot)
plot <- lavaanPlot(model = sem_fit1, 
                   coefs = TRUE, stand=TRUE,
                   sig = 0.05) # shows only significat effcets
plot
save_png(plot, "plot1.png")

#2) Get the fit indices. 
summary(sem_fit1, standardize = T, fit.measures=T)
fitMeasures(sem_fit1)
#
# 3) Test if link from "age" to "cover" is missing using a Likelihood Ratio Test (Chi-square difference test) 
# model 2 in lavaan
sem_mod2 <- '
      firesev ~ age
      cover ~ firesev + age
'
sem_fit2 <- sem(sem_mod2,  data=keeley)
#
library(lavaanPlot)
plot <- lavaanPlot(model = sem_fit2, 
                   coefs = TRUE, stand=TRUE,
                   stars = "regress", # shows stars for regr coef
                   digits = 1) 
plot
save_png(plot, "plot2.png")
#
# compare two models
anova(sem_fit1, sem_fit2)
#
# 4 indirect effects for the final model

sem_mod1 <- '
      firesev ~ a1*age
      cover ~ a2*firesev

indirect := a1*a2
'

#1) Fit the model in lavaan, get  path coeficients
sem_fit1 <- sem(sem_mod1, data=keeley)

summary(sem_fit1, standardize = T, fit.measures=T)

