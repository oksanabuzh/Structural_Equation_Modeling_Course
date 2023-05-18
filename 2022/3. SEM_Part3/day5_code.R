# Code for the course:
# Introduction to structural equation modeling and mixed models in R

# Day 5

# Oksana Buzhdygan
# oksana.buzh@fu-berlin.de
#
#
library(lavaan)
#
#

setwd("C:/Users/Oksana/Nextcloud/SEM_final/Day 5/Data")

aphid_data <- read.csv("Aphid_data.csv")

aphid_data$garlic_ef<-factor(aphid_data$garlic_ef)
aphid_data$enemy<-factor(aphid_data$enemy)
#
str(aphid_data) 
names(aphid_data)
aphid_data$enemy
#
# create plot
library(ggplot2)
graphics.off() # shuts down all open graphics 
ggplot(aphid_data, aes(x=host_plant, y=aphid, color = enemy)) +
  geom_point()+
  labs(y="Aphid density", x="Host-plant biomass")+
theme_bw()
#
#
mod1 <- lm(aphid ~ host_plant + garlic + enemy , aphid_data)
library(car)
Anova(mod1)
summary(mod1)
#
pred_dat <- expand.grid(host_plant = seq(min(aphid_data$host_plant),
                                             max(aphid_data$host_plant),
                                             length = 100),
                        garlic = mean(aphid_data$garlic),
                        enemy = unique(aphid_data$enemy))
# predict for range of canopy_closure values (numerical) for each tree species (factor levels)
pred_dat$pred_aphid <- predict(mod1, pred_dat, type = "response")
#
ggplot(aphid_data, aes(host_plant, aphid, color = enemy)) +
  geom_point() +
  geom_line(aes(y = pred_aphid), data = pred_dat)+
geom_point()+
  labs(y="Aphid density", x="Host-plant biomass")+
  theme_bw()
#
#
#
library(lavaan)
sem_mod <- ' aphid ~ host_plant + garlic + predator + parasite
             host_plant ~ garlic
'
fit <- sem(sem_mod, data=aphid_data)
library(lavaanPlot)
lavaanPlot(model = fit, 
                   coefs = TRUE, stand=TRUE,
                   # graph_options = list(layout = "circo"),
                   # stars = 'regress', # shows stars for regr coef
                   digits = 2) 
#
#
summary(fit, standardize = T, rsq = T, fit.measures=T)
#
# calculate indirect effects
sem_mod <- ' aphid ~ a1*host_plant + a2*garlic + predator + parasite
             host_plant ~ a3*garlic
                # define indirect and total effect
                  direct := a2
                  indirect := a3*a1
                  total := direct + indirect
'
fit <- sem(sem_mod, data=aphid_data)
summary(fit, standardize = T, rsq = T, fit.measures=T)
#
#
#Categorical  Endogenous Variable
#---------------------------------
#
fish_data <- read.csv("Fish_data.csv")
str(fish_data) 
names(fish_data)
#
sem_mod2 <- ' inv_fish ~  HII
                native_fish ~ plant_div + HII
                plant_div ~ HII
                native_fish ~~ inv_fish
'
fit2 <- sem(sem_mod2, data=fish_data, ordered = c("inv_fish"))
summary(fit2, standardize = T, rsq = T, fit.measures = T)

#
# Latent Variable
#---------------------------------
#
# Confirmatory factor analysis
#
travis <- read.csv("Travis_data.csv")
str(travis)

cor(travis[, 4:8])
round(cor(travis[, 4:8]),2)

cfa_mod <- '
performance =~ stems + infls + clonediam + leafht + leafwdth
'
cfa_fit <- sem(cfa_mod, travis)
summary(cfa_fit)
#
modindices(cfa_fit)
#
cfa_mod2 <- '
performance =~ stems + infls + clonediam + leafht + leafwdth
leafht ~~ leafwdth
'
cfa_fit2 <- sem(cfa_mod2, travis)
summary(cfa_fit2)
#
# CFA as a part of structural model
SEM_latent_mod <- '
            # latent
performance =~ stems + infls + clonediam + leafht + leafwdth
         # structural paths
performance ~ geneticdist
          # correlated errors
leafht ~~ leafwdth
'
#
SEM_latent_fit <- sem(SEM_latent_mod , travis)
#
summary(SEM_latent_fit, standardize = T, rsq = T, fit.measures=T)
#
library(lavaanPlot)
lavaanPlot(model = SEM_latent_fit, 
           coefs = TRUE, stand=TRUE,
           # graph_options = list(layout = "circo"),
           # stars = 'regress', # shows stars for regr coef
           digits = 2) 
#