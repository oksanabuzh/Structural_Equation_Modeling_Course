# Solutions to the tasks in the lecture
#
#  27.02.2023
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
dev.off() # clear graphical settings

# Required packages----
library(lavaan)
library(piecewiseSEM)
library(mvnormtest)
library(MVN)
library(scatterplot3d)
library(car)
library(lavaanPlot)
library(AICcmodavg)

library(here)
library(tidyverse)
# -------#
# Task 1----
# -------#

library(piecewiseSEM)
data(keeley)
str(keeley)
#
# 1) Specify the model in lavaan 
SemModel <- '
  rich ~ abiotic + hetero + distance
  hetero ~ distance
  abiotic ~ distance'
#
# 2) Assumptions for covariance-based SEM----
#
# 2.1 Testing Normality of residuals----
#
mod1 <-lm(rich ~ abiotic + hetero + distance, data=keeley)
mod2 <- lm (hetero ~ distance, data=keeley)
mod3 <- lm (abiotic ~ distance, data=keeley)


lavCor(SemFit) # Observed covariance matrix (correlation)


#get residuals
res_rich <- resid(mod1)
res_hetero <- resid(mod2)
res_abiotic <- resid(mod3)
#
#Plot all the residuals
par(mfrow=c(2,2))
qqnorm(res_rich)
qqline(res_rich)
qqnorm(res_hetero)
qqline(res_hetero)
qqnorm(res_abiotic)
qqline(res_abiotic)
par(mfrow=c(1,1))
# 
# Alternatively:
par(mfrow=c(2,2))
plot(mod1)
plot(mod2)
plot(mod3)
par(mfrow=c(1,1))
#
#
res <- cbind(res_rich, res_hetero, res_abiotic)
#
# Multivariate Test with Shapiro-Wilks
library(mvnormtest)
mshapiro.test(t(res)) # t() command transposes rows and columns in matrix
#
# Multivariate Test with MVN package
library(MVN)
#plot it and evaluate
mvn(res, mvnTest="mardia")
#
par(mfrow=c(1,1))
mvn(res,multivariatePlot="qq")
#
# univariate plots 
par(mfrow=c(1,1))
mvn(res,univariatePlot="qqplot")
#
#
# 2.2  Multivariate normality of data----
# 
library(scatterplot3d)

keeley_1 <- keeley[c("rich", "abiotic","hetero","distance")]
par(mfrow=c(1,1))

scatterplot3d(keeley_1,  color="distance",
              col.axis="blue", col.grid="lightblue",
              pch=20)


# Multivariate Mardia's Test:----
library(MVN)
mvn(keeley_1,mvnTest="mardia")  

# plots for Multivariate Normality
par(mfrow=c(1,1))
mvn(keeley_1,multivariatePlot="qq")

# univariate plots 
mvn(keeley_1,univariatePlot="qqplot")
par(mfrow=c(1,1))

# 2.3 Checking for singular predictors----
library(car)
vif(mod1) # only for mod1, mod2 and mod3 have only 1 predictor

# 3) Fit the model using data  data(keeley)----
library(lavaan)
SemFit <- sem(SemModel, data=keeley)
summary(SemFit, standardize = T)

# Check the WARNING
lavInspect(SemFit, "obs")$cov # call the model-implied covariance matrix
# Check variances
varTable(SemFit)
# Recode vars to roughly same scale
rich <- keeley$rich/100
abiotic <- keeley$abiotic/100
hetero <- keeley$hetero
distance <- keeley$distance/100
# Create Transformed Dataset
# overwrite file with recoded data
t.keeley <- data.frame(rich, abiotic, hetero, distance)
summary(t.keeley)

# Repeat model estimation using transformed data
SemFit1 <- sem(SemModel, data=t.keeley)
varTable(SemFit1)
summary(SemFit1, standardize = T)


#4) Get the fit indices----
summary(SemFit1, standardize = T,
        fit.measures=TRUE)

# 5) Fill in Standardized Coefficients and R2 for the model----
summary(SemFit1, standardize = T, rsq= T)
#
library(lavaanPlot)
plot <- lavaanPlot(model = SemFit1, 
                   coefs = TRUE, stand=TRUE,
                   graph_options = list(layout = "circo"),
                   stars = 'regress', # shows stars for regr coef
                   digits = 1) 
plot
library(DiagrammeRsvg)
library(rsvg)
save_png(plot, "plot3.png")


# 6) Calculate indirect and total effects of distance
                          # on plant species richness
SemModel2 <- '
  rich ~ b1*abiotic + b2*hetero + b3*distance
  hetero ~ b4*distance
  abiotic ~ b5*distance
           # define direct, indirect and total effects  
        direct   := b3 
        indirect_hetero := b4*b2 
        indirect_abiotic :=  b5*b1
        indirect := indirect_hetero + indirect_abiotic
        total := direct + indirect
'
# Fit the model
SemFit2 <- sem(SemModel2, data=t.keeley)
summary(SemFit2, standardize = T)



# -------#
# Task 2----
# -------#
library(piecewiseSEM)
data(keeley)
str(keeley)
#
# Specify the model 1  
sem_m1 <- '
  rich ~ abiotic + hetero
  hetero ~ distance
  abiotic ~ distance'
#
sem_fit1 <- sem(sem_m1, data=keeley)
summary(sem_fit1, standardize = T)


# Specify the model 2 
sem_m2 <- '
  rich ~ abiotic + hetero + distance
  hetero ~ distance
  abiotic ~ distance'

sem_fit2 <- sem(sem_m2, data=keeley)
summary(sem_fit2, standardize = T)

# Specify the model 3  
sem_m3 <- '
  rich ~ abiotic + hetero 
  hetero ~ distance + abiotic
  abiotic ~ distance'

sem_fit3 <- sem(sem_m3, data=keeley)
summary(sem_fit3, standardize = T)

# Specify the model 4   
sem_m4 <- '
  rich ~ abiotic + hetero + distance
  hetero ~ distance + abiotic
  abiotic ~ distance'

sem_fit4 <- sem(sem_m4, data=keeley)
summary(sem_fit4, standardize = T)


# Specify the model 1  
sem_m5 <- '
  rich ~ abiotic + hetero
  hetero ~ distance
  abiotic ~ distance
   hetero ~~ abiotic
'

sem_fit5 <- sem(sem_m5, data=keeley)
summary(sem_fit5, standardize = T)


# Model Comparison with AICc ----
library(AICcmodavg)
aictab(cand.set = list(sem_fit1, sem_fit2, sem_fit3, sem_fit4, sem_fit5),
       modnames = c("mod1", "mod2", "mod3", "mod4", "mod5"))

# -------#
# Task 3----
# -------#
library(here)
path <- here::here()
path

library(tidyverse)
grassl_data <- read_csv("Data/Grassl_data_2.csv")
str(grassl_data)

# Specify the SEM model
sem_mod <- '
    Grazing  ~  Elevation
    BareSoil  ~  Grazing 
    Diversity ~ Elevation + BareSoil
'

# estimate the parameters and fit the model
sem.fit <- sem(sem_mod, data=grassl_data)

plot <- lavaanPlot(model = sem.fit, 
                   coefs = TRUE, stand=TRUE, graph_options = list(layout = "circo"), 
                   sig = 0.05)
plot


#Model assumptions----

### Normality of residuals----
# specify regression models for each response variable from SEM model
#
grassl_data_1 <- na.omit(grassl_data) 
#grassl_data_1 <- grassl_data

grassl_data_1$Grazing_  <-  log(grassl_data_1$Grazing)
grassl_data_1$BareSoil_  <-  log(grassl_data_1$BareSoil)
grassl_data_1$Diversity_  <-  log(grassl_data_1$Diversity)

mod1 <- lm(Grazing_ ~ Elevation, grassl_data_1)
mod2 <- lm(BareSoil_ ~ Grazing_, grassl_data_1)
mod3 <- lm(Diversity ~ Elevation + BareSoil_, grassl_data_1)
#
# get residuals for each variable
res_y1 <- resid(mod1) 
res_y2 <- resid(mod2) 
res_y3 <- resid(mod3) 





# Plot the Q-Q plots and examine
par(mfrow=c(2,2))
qqnorm(res_y1)
qqline(res_y1)
qqnorm(res_y2)
qqline(res_y2)
qqnorm(res_y3)
qqline(res_y3)
par(mfrow=c(1,1))




par(mfrow=c(2,2))
plot(mod1)
plot(mod2)
plot(mod3)
par(mfrow=c(1,1))

# Additional options:

# 1) Multivariate Test with Shapiro-Wilks
library(mvnormtest)
res <- cbind(res_y1, res_y2)
res <- cbind(res, res_y3)
mshapiro.test(t(res))

# Multivariate Mardia's Test:
library(MVN)
mvn(res, mvnTest="mardia")

### Multivariate normality of data----
# ---------------------------------#

# Scatterplot of our three variables
library(scatterplot3d)
par(mfrow=c(1,1))
scatterplot3d(grassl_data)

##Multivariate Mardia's Test:----
mvn(grassl_data, mvnTest="mardia")  
# you can set univariate normality test to  Shapiro-Wilk test
mvn(grassl_data,mvnTest="mardia", univariateTest="SW")  

# plots for Multivariate Normality
par(mfrow=c(1,1))
mvn(grassl_data,multivariatePlot="qq")
mvn(grassl_data, multivariateOutlierMethod="quan") # outliers

# univariate plots 
mvn(grassl_data,univariatePlot="qqplot")
par(mfrow=c(1,1))

### Missing data----

is.na(grassl_data)
which(is.na(grassl_data))

# We have missing data and nonnormal data
#Adjust for the violations ----

grassl_data$Grazing_  <-  log(grassl_data$Grazing)
grassl_data$BareSoil_  <-  log(grassl_data$BareSoil)
grassl_data$Diversity  <-  log(grassl_data$Diversity)

sem_mod <- '    
    Grazing  ~  Elevation
    BareSoil  ~  Grazing
    Diversity ~ Elevation + BareSoil
'

sem.fit2 <- sem(sem_mod, data=grassl_data_1, missing="fiml", 
               estimator="MLR")              
        
                
                
summary(sem.fit2, standardize=T)

# Get fit indices----
summary(sem.fit2, standardize = T, fit.measures=T)

# Are we ignoring important links?----

resid(sem.fit2, "cor")$cov

summary(sem.fit2, standardize=T, modindices=T)
# add link to the model
sem_mod3 <- 'BareSoil ~ Grazing
              Diversity ~ BareSoil + Elevation + Grazing
              Grazing ~ Elevation'
sem.fit3 <- sem(sem_mod3, data=grassl_data, missing="fiml", estimator="MLR")
summary(sem.fit3, standardize=T)
# we have a good fit
 
anova(sem.fit2, sem.fit3)
# more complex model (sem_fit3) is significantly more accurate than the simpler model sem_fit2

# Are all the included links supported by the data? -----

# Model Comparison with AIC
aic <- AIC(sem.fit2, sem.fit3)
aic
d_aic <- aic[2] - min(aic[2])
d_aic
library(dplyr)
arrange(d_aic, +AIC)
# models are significantly different with sem.fit3 having significantly more accurate fit

