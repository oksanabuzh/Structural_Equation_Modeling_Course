#  Solutions


# Script from the lecture
# 02.02.2023
#==============================================#
# ------- Structural equation & mixed model course
# 
# ==============================================#
# Day 09  --------------------------------------
# Teaching & code: 
# Oksana Buzhdygan 
# oksana.buzh@fu-berlin.de
#
# ==============================================#

rm(list = ls()) # clears working environment
graphics.off() # shuts down all open graphics 


#
# Set up a local working directory
library(here)
path <- here::here()
path




#-----------------#
# Task 1----
#-----------------#

# Data----
library(tidyverse)

jena <- read_csv("Data/Jena.csv")
str(jena)
    

# Confirmatory factor analysis

round(cor(jena[, 4:8]),2)

library(lavaan)

cfa_mod1 <- '
ecos_func =~  C_stor + decomposition + pollination + predation + herbivory
  '
# Normality of data
library(MVN)
mvn(jena %>% 
      select(C_stor, decomposition, pollination,predation, herbivory),
    mvnTest="mardia")

cfa_fit1 <- sem(cfa_mod1, jena, test="Satorra-Bentler")
summary(cfa_fit1)
#
modindices(cfa_fit1)
# include correlation

cfa_mod2 <- '
  ecos_func =~  C_stor + decomposition + pollination + predation +herbivory
  predation~~herbivory'

cfa_fit2 <- sem(cfa_mod2, jena, test="Satorra-Bentler")
summary(cfa_fit2, standardize = T, fit.measures = T, rsq=T)
# improved fit

modindices(cfa_fit2)


cfa_mod3 <- '
  ecos_func =~  C_stor + decomposition + pollination + predation +herbivory
  predation~~herbivory
  C_stor ~~ decomposition
'

cfa_fit3 <- sem(cfa_mod3, jena, test="Satorra-Bentler")
summary(cfa_fit3, standardize = T, fit.measures = T, rsq=T)

# improved fit
# not perfect but ok to go

modindices(cfa_fit3)
# no more changes can be made to improve the latent variable


# CFA as a part of structural model
SEM_latent_mod <- ' ecos_func =~  C_stor + decomposition + pollination + predation +herbivory
                    predation ~~ herbivory
                    C_stor ~~ decomposition
                 
                 ecos_func ~  plant_biom 
                 legumes ~  plant_sr
                 plant_biom ~  plant_sr  
                  
'
# Check assumptions:
# we will use argument ordered = c("legumes") 
# that is why we do not test for the normality of data as this accounts for the non-normal data

# normality of residuals:

m1 <- lm(plant_biom ~  plant_sr, jena)

par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

# transform the plant_sr

m1 <- lm(plant_biom ~  log2(plant_sr), jena)

par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

# create a transformed variable

jena <- jena %>% 
  mutate (plant_sr_log2 = log2(plant_sr))


SEM_latent_mod1 <- ' ecos_func =~  C_stor + decomposition + pollination + predation +herbivory
                    predation ~~ herbivory
                    C_stor ~~ decomposition
                 
                 ecos_func ~  plant_biom + legumes
                 legumes ~  plant_sr_log2
                 plant_biom ~  plant_sr_log2  
                  
'

sem_fit1 <- sem(SEM_latent_mod1, jena, ordered = c("legumes"))

varTable(sem_fit1)

summary(sem_fit1, standardize = T, fit.measures = T, rsq=T)

library(lavaanPlot)
lavaanPlot(model = sem_fit1, 
           coefs = TRUE, stand=TRUE,
           # graph_options = list(layout = "circo"),
           # stars = 'regress', # shows stars for regr coef
           digits = 2, cov=T) 

# not good fit
# Model is missing the links


modindices(sem_fit1)


SEM_latent_mod2 <- ' ecos_func =~  C_stor + decomposition + pollination + predation +herbivory
                    predation ~~ herbivory
                    C_stor ~~ decomposition
                 
                 ecos_func ~  plant_biom + legumes +plant_sr_log2
                 legumes ~  plant_sr_log2
                 plant_biom ~  plant_sr_log2 
                  
'
sem_fit2 <- sem(SEM_latent_mod2, jena, ordered = c("legumes"))
summary(sem_fit2, standardize = T, fit.measures = T, rsq=T)

modindices(sem_fit2)


# good fit

library(lavaanPlot)
lavaanPlot(model = sem_fit2, 
           coefs = TRUE, stand=TRUE,
           # graph_options = list(layout = "circo"),
           # stars = 'regress', # shows stars for regr coef
           digits = 2, cov=T) 


# Calculate direct, indirect, and total effects of 
# “Plant species richness” on “Ecosystem Functioning”


SEM_latent_mod3 <- ' ecos_func =~  C_stor + decomposition + pollination + predation +herbivory
                    predation ~~ herbivory
                    C_stor ~~ decomposition
                 
                 ecos_func ~  b4*plant_biom + b3*legumes + b1* plant_sr_log2
                 legumes ~ b2* plant_sr_log2
                 plant_biom ~ b5* plant_sr_log2
# define direct, indirect and total effects  
    direct   := b1 
    indirect_legumes := b2*b3
    indirect_biom:= b4*b5
indirect:= indirect_legumes + indirect_biom
total   := direct + indirect
'
fit1 <- sem(SEM_latent_mod3, jena, ordered = c("legumes"))
summary(fit1, standardize = T, rsq = T)


#-----------------#
# Task 2----
#-----------------#
pond <- read_csv("Data/pond.csv")
str(pond)
#
pond$Inv_fish_prc <- pond$Invas_fish/pond$Tot_fish
#
mod1 <- glm(Inv_fish_prc ~ Macr + water_T +  HII + Connect + HII:Connect ,
            family = "binomial", weights = Tot_fish, data = pond)
summary(mod1)
63.826/114
drop1(mod1, test = "Chi")
#
mod2 <- glm(Macr ~ water_T +  HII ,
            family = "poisson", data = pond)
summary(mod2)
71.657/117
#
mod3 <- lm(water_T ~ HII, data = pond)

par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))

library(piecewiseSEM)
psem1 <- psem(mod1, mod2, mod3)
summary(psem1)
plot(psem1)

mod1_b <- glm(Inv_fish_prc ~ Macr + water_T +  HII + Connect  ,
            family = "binomial", weights = Tot_fish, data = pond)
summary(mod1_b)
64.357/115
#
psem2 <- psem(mod1_b, mod2, mod3)
summary(psem2)
plot(psem2)
#
aic <- AIC(psem1, psem2, aicc = TRUE)
aic
#AICc is used when n/K < 40 
120/11
aic[2] - min(aic[2])
#
# to calculate the indirect effect you can use use:
coefs(psem2)
# for example:
a1<- coefs(psem2)[1, 8]
a1
a2<- coefs(psem2)[2, 8]
a2
indirect<- a1*a2
indirect


