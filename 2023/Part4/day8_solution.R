# Solutions



# Script from the lecture
# 01.03.2023data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
#==============================================#
# ------- Structural equation & mixed model course
# 
# ==============================================#
# Day 08  --------------------------------------
# Teaching & code: 
# Oksana Buzhdygan 
# oksana.buzh@fu-berlin.de
#
# ==============================================#

rm(list = ls()) # clears working environment
graphics.off() # shuts down all open graphics 


# Required packages----

# install the development version of piecewiseSEM
devtools::install_github("jslefche/piecewiseSEM@devel")
library(piecewiseSEM)



# -------#
# Task 1----
# -------#
library(piecewiseSEM)
data(keeley)
str(keeley)
#
 
sem_m1 <- psem(lm(rich ~ abiotic + hetero, keeley),
               lm(hetero ~ distance, keeley),
               lm(abiotic ~ distance, keeley))

summary(sem_m1)


sem_m2 <- psem(lm(rich ~ abiotic + hetero + distance, keeley),
               lm(hetero ~ distance, keeley),
               lm(abiotic ~ distance, keeley))

summary(sem_m2)



sem_m5 <- psem(lm(rich ~ abiotic + hetero, keeley),
                  lm(hetero ~ distance, keeley),
                     lm(abiotic ~ distance, keeley),
               hetero %~~% abiotic)

summary(sem_m5)

AIC (sem_m1, sem_m2,sem_m5, AIC.type = "dsep")

anova(sem_m1, sem_m2, sem_m5)


aic <- AIC(sem_m1, sem_m2,sem_m5, AIC.type = "dsep", aicc = TRUE)
aic
d_aic <- aic[1] - min(aic[1])
d_aic


#----#
#task 2----
#----#
# Set up a local working directory
library(here)
path <- here::here()
path

# Data----
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

psem1 <- psem(m1, m2, m3, m4)
psem1
summary(psem1)
coefs(psem1)

coefs(psem1, standardize.type = "Menard.OE")
coefs(psem1, standardize.type = "latent.linear")
coefs(psem1, standardize = "range")
coefs(psem1, standardize = "scale")

#------------- # 
# Task 3----#
#------------- #

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
  lm(rich ~ elev*grazed + mass*grazed, meadows),
  lm(mass ~ elev *grazed, meadows)
)



psem3 <- psem(
  lm(rich ~ elev+ grazed + mass*grazed, meadows),
  lm(mass ~ elev *grazed, meadows)
)

AIC(psem2, psem3)


summary(psem3)

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



#-------#
# Task 4----
#-------#



data <- read_csv("Data/dataset4_grassland_abandonment.csv")
str(data)



mod1 <- lm(SR   ~  LU_type + Elev+Encroch_rate + Soil_C + Encroch_rate:Soil_C, data = data)

with(data, cor(Encroch_rate, Encroch_rate*Soil_C))
with(data, cor(Soil_C, Encroch_rate*Soil_C))

#
# Centering for Interactions 
data1 <- data %>% 
  mutate(Encroch_rate_c = as.vector(scale(Encroch_rate, scale=FALSE)),
                         Soil_C_c = as.vector(scale(Soil_C, scale=FALSE)))

names(data1)
str(data1)

with(data1, cor(Soil_C_c, Encroch_rate_c*Soil_C_c))
with(data1, cor(Encroch_rate_c, Encroch_rate_c*Soil_C_c))

mod1 <- lm(SR   ~  LU_type + Elev+Encroch_rate_c + Soil_C_c + Encroch_rate_c:Soil_C_c, data = data1)
summary(mod1)

par(mfrow = c(2, 2))
plot(mod1) 
par(mfrow = c(1, 1))


mod2 <- lm(Encroch_rate_c   ~  Elev
           # + LU_type
           , data = data1)
summary(mod2)
par(mfrow = c(2, 2))
plot(mod2) 
par(mfrow = c(1, 1))

mod3 <- lm(Soil_C_c   ~  LU_type  
           # +  Elev
           , data = data1)
summary(mod3)
par(mfrow = c(2, 2))
plot(mod3) 
par(mfrow = c(1, 1))


library(piecewiseSEM)
psem1 <- psem(mod1, mod2, mod3)

coefs(psem1)
summary(psem1)
fisherC(psem1)
LLchisq(psem1)



mod2_c <- lm(Encroch_rate_c   ~   LU_type +
               Elev, data = data1)
summary(mod2_c)
par(mfrow = c(2, 2))
plot(mod2_c) 
par(mfrow = c(1, 1))


psem2 <- psem(mod1, mod2_c, mod3)
coefs(psem2)
summary(psem2)
fisherC(psem2)
LLchisq(psem2)

mod3_c <- lm(Soil_C_c   ~  LU_type  
             +  Elev, data = data1)
summary(mod3_c)
par(mfrow = c(2, 2))
plot(mod3_c) 
par(mfrow = c(1, 1))
#
#
psem3 <- psem(mod1, mod2_c, mod3_c)
coefs(psem3)

summary(psem3)
fisherC(psem3)
LLchisq(psem3)

aic <- AIC(psem1, psem2, psem3, aicc=TRUE)

d_aic <- aic[1] - min(aic[1])
aic
d_aic



###### Try GLM instead----
mod1_glm <- glm(SR   ~  LU_type + Elev+Encroch_rate_c + Soil_C_c + Encroch_rate_c:Soil_C_c,
            family = "poisson", data = data1)
summary(mod1_glm)
84.034/88 # no overdispersion


#######
psem4 <- psem(mod1_glm, mod2_c, mod3_c)
coefs(psem4)
summary(psem4)
fisherC(psem4)
LLchisq(psem4)





#-------#
# Task 5----
#-------#
#
library(piecewiseSEM)
data(shipley)
str(shipley)

library(lme4)

m1 <- lm(DD ~ lat, data = shipley)
summary(mod2_c)
par(mfrow = c(2, 2))
plot(m1) 
par(mfrow = c(1, 1))

m2 <- lm(Growth ~ DD, data = shipley)
summary(mod2_c)
par(mfrow = c(2, 2))
plot(m2) 
par(mfrow = c(1, 1))

m3 <- glm(Live ~ Growth+DD, family = binomial(link = "logit"), data = shipley)
summary(mod2_c)

psem4 <- psem(m1, m2, m3)

summary(psem4, .progressBar = FALSE)


#-------#
# Task 6----
#-------#

m1_mm <- lmer(DD ~ lat+ (1 | site), data = shipley)
summary(m1_mm)
par(mfrow = c(2, 2))
plot(m1_mm) 
par(mfrow = c(1, 1))

m2_mm <- lmer(Growth ~ DD + (1 |site/ tree), data = shipley)
summary(m2_mm)
par(mfrow = c(2, 2))
plot(m2_mm) 
par(mfrow = c(1, 1))

m3_mm <- glmer(Live ~ Growth+DD + (1 | site/ tree) , family = binomial(link = "logit"), data = shipley)
summary(m3_mm)


psem6 <- psem(m1_mm, m2_mm, m3_mm)

summary(psem6, .progressBar = FALSE)
