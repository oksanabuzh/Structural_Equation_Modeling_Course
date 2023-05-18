#
#  Solutions
#
#
# Script from the lecture
# 28.02.2023
#==============================================#
# ------- Structural equation & mixed model course
# 
# ==============================================#
# Day 07  --------------------------------------
# Teaching & code: 
# Oksana Buzhdygan 
# oksana.buzh@fu-berlin.de
#
# ==============================================#

rm(list = ls()) # clears working environment
graphics.off() # shuts down all open graphics 


# Required packages----

library(lavaan)
#
# Set up a local working directory
library(here)
path <- here::here()
path

#-------#
# Task 1----
#-------#
fw_data <- read_csv("Data/Food_web_data.csv")
str(fw_data) 
names(fw_data)

foodweb$food_web <- ifelse(foodweb$food_web == "1_level", 1,
                           ifelse(foodweb$food_web == "2_levels", 2,
                                  ifelse(foodweb$food_web == "3_levels", 3, NA)))
str(foodweb) 

# recode the categorical variable
fw_data <- fw_data %>% 
  mutate(graz_int = case_when(grazing == "low" ~ 1,
                              grazing == "medium" ~ 2,
                              grazing == "high" ~ 3)) 
#
# In situations in which the assumption of
# multivariate normality is severely violated and/or
# data are ordinal, the diagonally weighted least
# squares (DWLS) method provides more accurate
# parameter estimates.


sem.mod1 <- ' food_web ~  plant_sr + plant_biom + grazing_n
                 plant_sr ~ graz_int
                 plant_biom ~  graz_int + plant_sr +  soil_C
                 soil_C ~ graz_int
'

# Normality of residuals
mod1 <- lm(plant_sr ~ graz_int, fw_data)

mod2 <- lm(plant_biom ~  graz_int + plant_sr +  soil_C, fw_data)
car::vif(mod2) # check for correlation among predictors

mod3 <- lm(soil_C ~ graz_int, fw_data)

par(mfrow=c(2,2))
plot(mod1)
plot(mod2)
plot(mod3)
plot(mod4)
par(mfrow=c(1,1))

# Normality of data
library(MVN)
mvn(fw_data %>% 
      select(- c(grazing, food_web)),
    mvnTest="mardia", univariateTest="SW")

# Checking for missing data
which(is.na(fw_data))


sem.mod1 <- ' food_web ~  plant_sr + plant_biom + graz_int
                 plant_sr ~ graz_int
                 plant_biom ~  graz_int + plant_sr +  soil_C
                 soil_C ~ graz_int
'
fit2 <- sem(sem.mod1, data=fw_data, 
            ordered = c("food_web"))

summary(fit2, standardize = T, fit.measures = T)

# you get the robust statistics (corrected for non-normality and ordered data)
#                                             Standard      Scaled
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



#-------#
# Task 2----
#-------#

bdsize_data <- read_csv("Data/Bodysize_data.csv")
str(fw_data) 

# Confirmatory factor analysis (CFA)

round(cor(bdsize_data[, 1:4]),2)

# specify measurement model
cfa <- '
    body_size =~ vol + mass + ln + wdth
'

cfa.fit <- sem(cfa, data = bdsize_data)

library(lavaanPlot)
lavaanPlot(model = cfa.fit, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)


# Normality of data
library(MVN)
mvn(bdsize_data %>% 
      select(-HI),
    mvnTest="mardia")

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

# Normality of data
library(MVN)
mvn(bdsize_data ,
    mvnTest="mardia")

sem.fit <- sem(sem.mod, data = bdsize_data)
summary(sem.fit, standardize = T, rsq = T, fit.measures=T)
# Plot
library(lavaanPlot)
lavaanPlot(model = sem.fit, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)
# Interpretation of results:
# The human impact is positive and significant, this could be due to eutrophication due to fertilizers
# that increase biomass producation and therefore increase body size.


#-------#
# Task 3----
#-------#

tree_holes <- read_csv("Data/Tree_holes.csv")
str(tree_holes) 

# specify measurement model
cfa_3 <- '
    forest_manage =~ dead_wood + non_nat_sp + harv_biom + tree_gaps
'

cfa.fit_3 <- sem(cfa_3, data = tree_holes)

library(lavaanPlot)
lavaanPlot(model = cfa.fit_3, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)

# Normality of data
library(MVN)
mvn(tree_holes %>% 
      select(-c(predat, detritus, decomposition)),
    mvnTest="mardia")

# Confirmatory factor analysis (CFA)

cfa.fit_3 <- sem(cfa_3, data = tree_holes)
summary(cfa.fit_3, standardize = T, rsq = T, fit.measures=T)

#bad model fit (Chisq,p-value<0.05) -> add correlation between dead_wood & harv_biom due to high correlation

#refit latent variable approximation with correlation

round(cor(tree_holes[, 1:4]),2)

modindices(cfa.fit_3)

cfa_4 <- '
    forest_manage =~ dead_wood + non_nat_sp + harv_biom + tree_gaps
    dead_wood ~~ harv_biom
'

cfa.fit_4 <- sem(cfa_4, data = tree_holes)
summary(cfa.fit_4, standardize = T, rsq = T, fit.measures=T)

library(lavaanPlot)
lavaanPlot(model = cfa.fit_4, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)

# Add the CFA to a structural model:

cfa_5 <- '
    forest_manage =~ dead_wood + non_nat_sp + harv_biom + tree_gaps
    dead_wood ~~ harv_biom
    decomposition ~ predat + detritus + forest_manage
    predat ~ forest_manage
'

cfa.fit_5 <- sem(cfa_5,
                 ordered = c("predat"),
                 data = tree_holes)
summary(cfa.fit_5, standardize = T, rsq = T, fit.measures=T)

library(lavaanPlot)
lavaanPlot(model = cfa.fit_5, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)


#get modification indices to improve model fit
modificationIndices(cfa.fit_5,standardize=T)


cfa_6 <- '
    forest_manage =~ dead_wood + non_nat_sp + harv_biom + tree_gaps
    dead_wood ~~ harv_biom
    decomposition ~ predat + detritus + forest_manage
    predat ~ forest_manage
    detritus ~ forest_manage
'

cfa.fit_6 <- sem(cfa_6,
                 ordered = c("predat"),
                 data = tree_holes)
summary(cfa.fit_6, standardize = T, rsq = T, fit.measures=T)

library(lavaanPlot)
lavaanPlot(model = cfa.fit_6, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)


#fit model with added definitions of direct and indirect effects

cfa_7 <- "forest_manage =~ tree_gaps + dead_wood + non_nat_sp + harv_biom
dead_wood ~~ harv_biom
decomposition ~ e4*predat + e5*detritus + e1*forest_manage
predat ~ e2*forest_manage
detritus ~ e3*forest_manage
direct := e1
indirect := (e2*e4) + (e3*e5)
total := direct + indirect"

cfa.fit_7 <- sem(cfa_7,
                 ordered = c("predat"),
                 data = tree_holes)

summary(cfa.fit_7, standardize = T, rsq = T, fit.measures=T)

library(lavaanPlot)
lavaanPlot(model = cfa.fit_7, coefs = T, stand = T, stars = 'regress', digits = 2, cov=T)
