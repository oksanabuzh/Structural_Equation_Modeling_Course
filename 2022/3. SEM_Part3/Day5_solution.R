# Code for the course:
# Introduction to structural equation modeling and mixed models in R

# Day 5

# Oksana Buzhdygan
# oksana.buzh@fu-berlin.de
#
#  Solutions
#
#
library(lavaan)
#
#-------
# Task 1
#-------
fw_data <- read.csv("Food_web_data.csv")
str(fw_data) 
names(fw_data)
#
fw_data$grazing <- factor(fw_data$grazing, levels= c("low", "medium", "high"))
fw_data$grazing
fw_data$grazing_n <- unclass(fw_data$grazing)                         # Convert categories to numeric
fw_data$grazing_n
# or
fw_data$grazing_n <- as.numeric(fw_data$grazing)
#
# or code by Julian Winston Zeller (Thanks Julian)
fw <- fw_data %>% 
  mutate(graz_int = case_when(grazing == "low" ~ 1,
                              grazing == "medium" ~ 2,
                              grazing == "high" ~ 3)
  ) %>% 
  select(- c(grazing, food_web))
#
# In situations in which the assumption of
# multivariate normality is severely violated and/or
# data are ordinal, the diagonally weighted least
# squares (DWLS) method provides more accurate
# parameter estimates.
#
sem.mod1 <- ' food_web ~  plant_sr + plant_biom + grazing_n
                 plant_sr ~ grazing_n
                 plant_biom ~  grazing_n + plant_sr +  soil_C
                 soil_C ~ grazing_n
'
fit2 <- sem(sem.mod1, data=fw_data, ordered = c("food_web"))
summary(fit2, standardize = T, fit.measures = T)
# you get the robust statistics (corrected for non-normality and ordered data)
#                                             Standard      Robust
# Test Statistic                                 1.385       1.755
# Degrees of freedom                                 2           2
# P-value (Chi-square)                           0.500       0.416
# Scaling correction factor                                  0.842
#Plot the model
#
# But RMSA, CFI, SRMR are not calculated; as no clear suggestions exist regarding the application of these fit indices for for non-ML estimators. 
# report 'scaled' RMSA, CFI, SRMR
library(lavaanPlot)
lavaanPlot(model = fit2, coefs = T, stand = T)
#
#-------
# Task 2
#-------
bdsize_data <- read.csv("Bodysize_data.csv")
str(fw_data) 
#
# Confirmatory factor analysis (CFA)
#
round(cor(bdsize_data[, 1:4]),2)
#
# specify measurement model
cfa <- '
    body_size =~ vol + mass + ln + wdth
'
#
cfa.fit <- sem(cfa, data = bdsize_data)
# or
# cfa.fit <- cfa(cfa, data = bdsize_data)
#
#Confirmatory factor analysis:
summary(cfa.fit)
#
modindices(cfa.fit)
# Add only one link!
# We adapt the model by adding a correlation
cfa2 <- '
    body_size =~ vol + mass + ln + wdth
    ln ~~ wdth
'
#
cfa.fit2 <- sem(cfa2, data = bdsize_data)
summary(cfa.fit2)
#
modindices(cfa.fit2)
#
# Add the CFA to a structural model:
sem.mod <- '
    body_size =~ vol + mass + ln + wdth
    ln ~~ wdth
    body_size ~ HI
'
sem.fit <- sem(sem.mod, data = bdsize_data)
summary(sem.fit, standardize = T, rsq = T, fit.measures=T)
# Plot
lavaanPlot(model = sem.fit, coefs = T, stand = T, stars = 'regress', digits = 2)
# Interpretation of results:
# The human impact is positive and significant, this could be due to eutrophication due to fertilizers
# that increase biomass producation and therefore increase body size.


