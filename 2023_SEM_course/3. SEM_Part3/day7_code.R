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

# Data----
library(tidyverse)
aphid_data <- read_csv("Data/Aphid_data.csv")
str(aphid_data)

# Create dummy variables-----
# convert "enemy" in 3 binary dummy variables 
# and convert garlic_ef into 1 binary variable called garlic

aphid_data<- aphid_data %>%
   mutate(enemy_cat = enemy) %>% 
   mutate(n = 1) %>% 
   pivot_wider(names_from = enemy, values_from = n, values_fill = list(n = 0)) %>% 
   mutate(garlic = case_when(garlic_ef == "present" ~ 1,
                            garlic_ef == "absent" ~ 0)) 

str(aphid_data) 
names(aphid_data)

#Categorical  Exogenous  Variable----
#---------------------------------#
# create plot

#Check the assumptions:
# Normality of residuals
mod1 <- lm(aphid ~ host_plant + garlic + predator + parasite, aphid_data)
car::vif(mod1) # check for correlation among predictors
mod2 <- lm(host_plant ~ garlic, aphid_data)
# or use plot() function instead
par(mfrow=c(2,2))
plot(mod1)
plot(mod2)
par(mfrow=c(1,1))

# Normality of data
library(MVN)
mvn(aphid_data %>% 
      select(-enemy_cat, -garlic_ef, -no_enemy),
    mvnTest="mardia", univariateTest="SW")

# Checking for missing data
which(is.na(aphid_data))

library(lavaan)
sem_mod <- ' aphid ~ host_plant + garlic + predator + parasite
             host_plant ~ garlic
'
fit <- sem(sem_mod, 
           test="Satorra-Bentler", 
           data=aphid_data)

d


# calculate indirect effects
sem_mod <- ' aphid ~ a1*host_plant + a2*garlic + predator + parasite
             host_plant ~ a3*garlic
                # define indirect and total effect
                  direct := a2
                  indirect := a3*a1
                  total := direct + indirect
'
fit <- sem(sem_mod, 
           test="Satorra-Bentler", 
           data=aphid_data)
summary(fit, standardize = T, rsq = T, fit.measures=T)



# What we see from the linear regression:
mod1 <- lm(aphid ~ host_plant + garlic + enemy_cat , aphid_data)
library(car)
Anova(mod1)
summary(mod1)

new_data <- expand_grid(
  host_plant = min(aphid_data$host_plant):max(aphid_data$host_plant),
  garlic=mean(aphid_data$garlic),
  enemy_cat = unique(aphid_data$enemy_cat))

# predict for range of canopy_closure values (numerical) for each tree species (factor levels)
new_data$pred_aphid <- predict(mod1, new_data, type = "response")

library(ggplot2)

ggplot(aphid_data, aes(host_plant, aphid, color = enemy_cat)) +
  geom_point() +
  geom_line(aes(y = pred_aphid), data = new_data)+
  geom_point()+
  labs(y="Aphid density", x="Host-plant biomass")+
  theme_bw()


#Categorical  Endogenous Variable----
#---------------------------------#
#
fish_data <- read_csv("Data/Fish_data.csv")
str(fish_data) 
names(fish_data)
#

sem_mod2 <- ' inv_fish ~  HII
                native_fish ~ plant_div + HII
                plant_div ~ HII
                native_fish ~~ inv_fish
'

#Check the assumptions:

# In situations in which the assumption of
# multivariate normality is severely violated and/or
# data are ordinal, the diagonally weighted least
# squares (DWLS) method provides more accurate
# parameter estimates.


# Normality of residuals
# we dont check the 1st submodel as it is categorical
mod2 <- lm(native_fish ~ plant_div + HII, fish_data)
car::vif(mod2) # check for correlation among predictors
mod3 <- lm(plant_div ~ HII, fish_data)

par(mfrow=c(2,2))
plot(mod1)
plot(mod2)
plot(mod3)
par(mfrow=c(1,1))

# Normality of data
library(MVN)
mvn(fish_data %>% 
      select(-inv_fish, -inv_fish_),
    mvnTest="mardia")


# Fit model
sem_mod2 <- ' inv_fish ~  HII
                native_fish ~ plant_div + HII
                plant_div ~ HII
                native_fish ~~ inv_fish
'
fit2 <- sem(sem_mod2, data=fish_data, 
            ordered = c("inv_fish")) # account for the endogenous categorical variable
summary(fit2, standardize = T, rsq = T, fit.measures = T)


# But RMSA, CFI, SRMR are not calculated; as no clear suggestions exist regarding the application of these fit indices for for non-ML estimators. 
# report 'scaled' RMSA, CFI, SRMR


# Latent Variable----
#--------------------------------- #
#
# Confirmatory factor analysis----
library(tidyverse)
travis <- read_csv("Data/Travis_data.csv")
str(travis)

cor(travis[, 4:8])
round(cor(travis[, 4:8]),2)

# Normality of data
library(MVN)
mvn(travis %>% 
      select(-c(geneticdist, latitude, siteno)),
    mvnTest="mardia")

# confirmatory analysis
cfa_mod <- '
performance =~ stems + infls + clonediam + leafht + leafwdth
'
cfa_fit <- sem(cfa_mod, travis, test="Satorra-Bentler")

summary(cfa_fit, standardize = T, fit.measures = T, rsq=T)#
modindices(cfa_fit)
#
cfa_mod2 <- '
performance =~ stems + infls + clonediam + leafht + leafwdth
leafht ~~ leafwdth
'
cfa_fit2 <- sem(cfa_mod2,  
               test="Satorra-Bentler",
                travis)

summary(cfa_fit2, standardize = T, fit.measures = T, rsq=T)#

# Also try:
anova(cfa_fit, cfa_fit2)
AIC(cfa_fit, cfa_fit2)

# CFA as a part of structural model
SEM_latent_mod <- '
            # latent
performance =~ stems + infls + clonediam + leafht + leafwdth
           # correlated errors
leafht ~~ leafwdth
         # structural paths
performance ~ geneticdist
         
'
# Normality of data
library(MVN)
mvn(travis %>% 
      select(-c(latitude, siteno)), 
    mvnTest="mardia")

SEM_latent_fit <- sem(SEM_latent_mod , travis)

summary(SEM_latent_fit, standardize = T, rsq = T, fit.measures=T)

library(lavaanPlot)
lavaanPlot(model = SEM_latent_fit, 
           coefs = TRUE, stand=TRUE,
           # graph_options = list(layout = "circo"),
           # stars = 'regress', # shows stars for regr coef
           digits = 2, cov=T) 
