# Code for the course:
# Introduction to structural equation modeling and mixed models in R

# Day 10

# Oksana Buzhdygan
# oksana.buzh@fu-berlin.de
#
#  Solutions
#
#
jena <- read.csv("Data/Jena.csv")
str(jena)
#
# Confirmatory factor analysis
#
cor(jena[, 4:7])
round(cor(jena[, 4:7]),2)

cfa_mod1 <- '
ecos_func =~  C_stor + decomposition + pollination + predation
  '
cfa_fit1 <- sem(cfa_mod1, jena)
summary(cfa_fit1)
#
modindices(cfa_fit1)
# include correlation
cfa_mod2 <- '
ecos_func =~  C_stor + decomposition + pollination + predation
  C_stor ~~ decomposition'
cfa_fit2 <- sem(cfa_mod2, jena)
summary(cfa_fit2)
# improved fit
#
modindices(cfa_fit2)
#
#
m1 <- lm(plant_biom ~  plant_sr + legumes, jena)
#
res_y1 <- resid(m1) 

par(mfrow=c(1,1))
qqnorm(res_y1)
qqline(res_y1)
#
# CFA as a part of structural model
SEM_latent_mod <- '    ecos_func =~  C_stor + decomposition + pollination + predation
                 ecos_func ~ plant_sr + plant_biom +legumes
                 legumes ~  plant_sr
                 plant_biom ~  plant_sr + legumes
                  C_stor ~~ decomposition
'
fit1 <- sem(SEM_latent_mod, jena, ordered = c("legumes"))
summary(fit1, standardize = T, rsq = T)
#
#
SEM_latent_mod <- '    ecos_func =~  C_stor + decomposition + pollination + predation
                       ecos_func ~ b1*plant_sr + b5*plant_biom +b3*legumes
                       legumes ~  b2*plant_sr
                       plant_biom ~  b6*plant_sr + b4*legumes
                       C_stor ~~ decomposition
# define direct, indirect and total effects  
direct   := b1 
    indirect_legumes_1 := b2*b3
    indirect_legumes_2 := b2*b4*b5
 indirect_legumes_sum := indirect_legumes_1 + indirect_legumes_2
   indirect_biom:= b6*b5
indirect:= indirect_legumes_sum + indirect_biom
total   := direct + indirect
'
fit1 <- sem(SEM_latent_mod, jena, ordered = c("legumes"))
summary(fit1, standardize = T, rsq = T)
#
#
#-----------------
# Task 2
#-----------------
pond <- read.csv("Data/pond.csv")
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
#
#
library(piecewiseSEM)
psem1 <- psem(mod1, mod2, mod3)
summary(psem1)
plot(psem1)

mod1_b <- glm(Inv_fish_prc ~ Macr + water_T +  HII + Connect  ,
            family = "binomial", weights = Tot_fish, data = pond)
summary(mod1)
63.826/114
#
psem2 <- psem(mod1_b, mod2, mod3)
summary(psem2)
plot(psem2)
#
aic <- AIC(psem1, psem2)
aic[1] - min(aic[1])
#
# to calculate the indirect effect you can use use:
coefs(psem2)
# for example:
a1<- coefs(psem2)[1, 8]
a2<- coefs(psem2)[2, 8]
indirect<- a1*a2
#

