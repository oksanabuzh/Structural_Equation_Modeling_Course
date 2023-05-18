# Script from the lecture
# 27.02.2023
#==============================================#
# ------- Structural equation & mixed model course
# 
# ==============================================#
# Day 06  --------------------------------------
# Teaching & code: 
# Oksana Buzhdygan 
# oksana.buzh@fu-berlin.de
#
# ==============================================#

rm(list = ls()) # clears working environment

# Required packages----
library(tidyverse)
library(here)
library(lavaan)
library(performance)
library(mvnormtest)
library(MVN)
library(scatterplot3d)
library(car)
library(AICcmodavg)




# Set up a local working directory
library(here)
path <- here::here()
path

# Data----
library(tidyverse)
data1 <- read_tsv("Data/SEMdata1.txt")
# or
data1 <- read_delim(file="Data/SEMdata1.txt", delim  = "\t") 

names(data1)
str(data1)


# Part 1-----


#---------------------------------------------------#
# Testing Assumptions of Covariance-Based Estimation---- 
#---------------------------------------------------#
#
## Normality of Residual Distribution----
# ------------------------------------#

library(lavaan)

sem_mod1 <- '
    y1 ~ x1 
    y2 ~  y1 
'

sem.fit1 <- sem(sem_mod1, data=data1)


# specify regression models for each response variable from SEM model
mod1 <- lm(y1 ~ x1, data1)
mod2 <- lm(y2 ~ y1, data1)


# get residuals for each variable
res_y1 <- resid(mod1) 
res_y2 <- resid(mod2) 


# Plot the Q-Q plots and examine
par(mfrow=c(1,2))
qqnorm(res_y1)
qqline(res_y1)
qqnorm(res_y2)
qqline(res_y2)
par(mfrow=c(1,1))

# or use plot() function instead
par(mfrow=c(2,2))
plot(mod1)
plot(mod2)
par(mfrow=c(1,1))

# or
library(performance)

x11(height=20,width=20)
check_model(mod1)
check_model(mod2)


# Additional options:

### 1) Multivariate Test with Shapiro-Wilks----
library(mvnormtest)
res <- cbind(res_y1, res_y2)
mshapiro.test(t(res))
# the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# In other words, we can assume the normality.

### 2) Multivariate Test with MVN package ----
library(MVN)


mvn(res, mvnTest="mardia")

# you can set univariate normality test to  Shapiro-Wilk test
mvn(res,mvnTest="mardia", univariateTest="SW")  

?mvn() # see more

# multivariate plots
par(mfrow=c(1,1))
mvn(res, multivariatePlot="qq")
mvn(res, multivariateOutlierMethod="quan") # outliers

# univariate plots 
par(mfrow=c(1,1))
mvn(res,univariatePlot="qqplot") # same what we did manually


# ---------------------------------#
## Multivariate normality of data----
# ---------------------------------#
#
# Scatterplot of our three variables
library(scatterplot3d)
par(mfrow=c(1,1))
scatterplot3d(data1, highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              pch=20)
?scatterplot3d


####Multivariate Mardia's Test----
mvn(data1,mvnTest="mardia")  
# you can set univariate normality test to  Shapiro-Wilk test
mvn(data1,mvnTest="mardia", univariateTest="SW")  
?mvn() # see more

# plots for Multivariate Normality
par(mfrow=c(1,1))
mvn(data1,multivariatePlot="qq")
mvn(data1, multivariateOutlierMethod="quan") # outliers
#
# univariate plots 
mvn(data1,univariatePlot="qqplot")
par(mfrow=c(1,1))
#
#
# Adjusting for non-normality of data----
#------------------------------------#
#
## 1) The Satorra-Bentler Chi Square----
#     MLM estimation with robust standard errors
# our model:

sem_mod1 <- '
    y1 ~ x1 
    y2 ~  y1
'
# fit the model with estimator="MLM",se="robust"
sem.fit1 <- sem(sem_mod1, data=data1,
                estimator="MLM",
                se="robust")

summary(sem.fit1, standardize = T)

# or instead use test="Satorra-Bentler"
sem.fit1 <- sem(sem_mod1, data=data1, 
                test="Satorra-Bentler")

summary(sem.fit1, standardize = T)

summary(sem.fit1, standardize = T,
        fit.measures=TRUE)

## 2) Bollen-Stine Bootstrap----

sem.fit1 <- sem(sem_mod1, data=data1,
                test="bollen.stine", se="bootstrap", 
                bootstrap=1000)

summary(sem.fit1, standardize = T)


# Checking for missing data----
is.na(data1) 
which(is.na(data1)) # no missing data

# Adjusting for incomplete data
# If we have NA in a data 
# Full-information maximum likelihood (FIML) estimation
sem.fit1 <- sem(sem_mod1, data=data1, 
                missing="fiml")

summary(sem.fit1, standardize = T)

# If data is incomplate and nonnormal:
sem.fit1 <- sem(sem_mod1, data=data1, 
                estimator="MLR", 
                missing="fiml")

summary(sem.fit1, standardize = T)



# Checking for singular determinants----
#-----------------------------------#
# Check for multicolinearity between predictors if you get Error-message 
# that there are  non-positive definite elements in the matrices! 
# for this you need at least 2 or more predictor variables
m3 <- lm(y2 ~ x1 + y1, data1)
library(car)
vif(m3)


# Part 2-----


#---------------------------#
#Model Comparison in SEM----
#----------------------------#
library(piecewiseSEM)
data(keeley)
str(keeley)

sem_m1 <- '
  rich ~ abiotic + hetero
  hetero ~ distance
  abiotic ~ distance'

sem_fit1 <- sem(sem_m1, data=keeley)
summary(sem_fit1, standardize = T)

resid(sem_fit1, "cor")$cov

# Request modification indices 
summary(sem_fit1, modindices=T)
modificationIndices(sem_fit1, standardized=F)
modificationIndices(sem_fit1, standardized=F, minimum.value=3.8)

# Specify the model 2  in lavaan 
sem_m2 <- '
  rich ~ abiotic + hetero + distance
  hetero ~ distance
  abiotic ~ distance'

# Fit the model 
sem_fit2 <- sem(sem_m2, data=keeley)
summary(sem_fit2, standardize = T)
# compare two models
anova(sem_fit1, sem_fit2)



## Model Comparison with AICc----
library(AICcmodavg)
aictab(cand.set = list(sem_fit1, sem_fit2),
       modnames = c("Full", "Partial"))
#
## Model Comparison with AIC----
aic <- AIC(sem_fit1, sem_fit2)
aic
d_aic <- aic[2] - min(aic[2])
d_aic
library(dplyr)
arrange(d_aic, +AIC)

bic <- BIC(sem_fit1, sem_fit2) 
bic[2] - min(bic[2])
