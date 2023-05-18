# Code for the course:
# Introduction to structural equation modeling and mixed models in R

# Day 9

# Oksana Buzhdygan
# oksana.buzh@fu-berlin.de
#
# Solutions


library(piecewiseSEM)
#
setwd("C:/Users/Oksana/Nextcloud/SEM_final/Day 9")

#task 1
#
foodweb <- read.csv("Data/Food_web_data_2.csv")
str(foodweb)
#
# Check the assumptions for the sub-models:
m1 <- lm(soil_C ~ Gr_type, data = foodweb)
par(mfrow = c(2, 2))
plot(m1) # is OK
par(mfrow = c(1, 1))
#
m2 <- lm(plant_sr ~ Gr_type, data = foodweb)
par(mfrow = c(2, 2))
plot(m2) # is OK
par(mfrow = c(1, 1))
#
m3 <- lm(plant_biom ~ Gr_type + plant_sr + soil_C, data = foodweb)
par(mfrow = c(2, 2))
plot(m3) # is OK
par(mfrow = c(1, 1))
#
m4 <- lm(FW.length ~ plant_biom + plant_sr + Gr_type, data = foodweb)
par(mfrow = c(2, 2))
plot(m4) # is Ok (1-st sub-plot weird because the responce variable was categorical and converted to numeric)
par(mfrow = c(1, 1))
#
psem1 <- psem(m1, m2, m3, m4)
psem1
#
coefs(psem1)
#
#-------------
# Task2
#-------------
library(piecewiseSEM)
data(meadows)
str(meadows)
#
#
# Check the assumptions for the sub-models:
mod1 <- lm(rich ~ elev*grazed + mass*grazed, meadows)
drop1(mod1, test = "F")
mod1c <- lm(rich ~ elev + mass +  mass:grazed, meadows)
drop1(mod1c, test = "F")
par(mfrow = c(2, 2))
plot(mod1c) # 1st subplot is not perfect
# we could use poisson (as the responce is a count variable)
par(mfrow = c(1, 1))
#
mod2 <- lm(mass ~ elev*grazed, meadows)
drop1(mod2, test = "F")
par(mfrow = c(2, 2))
plot(mod2) # 1st subplot is not good, QQ-plot is not best
par(mfrow = c(1, 1))
# I would log() or sqrt() transform data 
# for now lets do it without any transformation
psem2 <- psem(
  lm(rich ~ elev + mass, meadows),
  lm(mass ~ elev, meadows)
)
#
multigroup(psem2, group = "grazed")
#
#---
#
mod2c <- lm(log(mass) ~ elev*(grazed), meadows)
drop1(mod2c, test = "F")
par(mfrow = c(2, 2))
plot(mod2c) # 1st subplot is not good, QQ-plot is not best
par(mfrow = c(1, 1))
#
gmod1c <- glm(rich ~ elev + log(mass) +  log(mass):grazed, family = "poisson", meadows)
summary(gmod1c)
524.30/350  # close to 1.5 (still acceptable)
#
#
# If we transform variables we have to transform the same variable across all submodels
meadows$mass_log <- log(meadows$mass) # need to create separate variable before you put it in the psem
#
library(MASS) # for glm.nb()
#
psem3 <- psem(
  lm(mass_log ~ elev, meadows),
  glm(rich ~ elev + mass_log, family = "poisson", meadows)
       # # quasipoisson and negative binomial also possible, but they do not return the st coef (only raw estimates can be used)
      # try:
  # glm(rich ~ elev + mass_log, family = "quasipoisson", meadows)
      # or
  # glm.nb(rich ~elev + mass_log,  data = meadows)
)
multigroup(psem3, group = "grazed")
summary(psem3)
#
#
#-------
# Task 3
#-------
#
library(piecewiseSEM)
data(shipley)
str(shipley)
#
library(nlme)
library(lme4)
#
#
psem4 <- psem(
  
  lme(DD ~ lat, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  
  lme(Date ~ DD, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  
  lme(Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  
  glmer(Live ~ Growth + (1 | site) + (1 | tree),
        family = binomial(link = "logit"), data = shipley)
  
)
#
summary(shipley_psem)