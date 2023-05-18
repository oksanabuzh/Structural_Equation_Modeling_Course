# Script from the lecture
# 01.03.2023
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

library(lavaan)
# install the development version of piecewiseSEM
devtools::install_github("jslefche/piecewiseSEM@devel")
#
library(piecewiseSEM)


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

# Global vs local estimator procedure----

# in lavaan (Global) ----

library(lavaan)
lav.mod <- 'y1 ~ x1 
             y2 ~  y1'

lav.fit <- sem(lav.mod, data=data1)

summary(lav.fit, standardize = T, rsq=T,
        fit.measures=TRUE)

library(lavaanPlot)
lavaanPlot(model = lav.fit, 
           coefs = TRUE, stand=TRUE,
           # graph_options = list(layout = "circo"),
            stars = 'regress', # shows stars for regr coef
           digits = 2) 

# in piecewiseSEM (Local)----

library(piecewiseSEM)

# Specify the model
m1 <- lm(y1 ~ x1, data = data1)
m2 <- lm(y2 ~ y1, data = data1)

# Fit the model
psem_mod <- psem(m1, m2)
#or
psem_mod <- psem(
  lm(y1 ~ x1, data = data1),
  lm(y2 ~ y1, data = data1),
  x1%~~%y1)

# Extract results
summary(psem_mod)
# see also coefficients
coefs(psem_mod, #
      standardize = "none", 
      intercepts = TRUE)
# get R square:
rsquared(psem_mod)


# Plot the model
plot(psem_mod)
# see
?plot.psem

plot(psem_mod, node_attrs = list(shape = "rectangle",
                                fillcolor = "white"))

plot(psem_mod, node_attrs = list(
  shape = "rectangle", color = "gray",
  fillcolor = "orange"),
  show = "std", digits = 2) # show = "unstd" - for unstandardised coeficients

?plot.psem
#

# Call model
psem_mod


# Model fit ----
# in lavaan
summary(lav.fit, standardize=T, rsq=T, fit.measures=TRUE)
# or call the fit measures:

fitMeasures(lav.fit)


# in piecewiseSEM:

# Extract results
summary(psem_mod)
# -- Global goodness-of-fit:
# Chi-Squared = 1.064 with P-value = 0.302 and on 1 degrees of freedom
# Fisher's C = 2.336 with P-value = 0.311 and on 2 degrees of freedom

# or call the fit measures:
# 1) Fisher’s C statistic
fisherC(psem_mod)
# 2) χ2 statistic
LLchisq(psem_mod)

#------------------#
# Missing links?----

# in lavaan:

# Request modification indices 
summary(lav.fit, standardize = T,  
        modindices=T)
# or
modificationIndices(lav.fit, standardized=F)

# in piecewiseSEM:
summary(psem_mod)
# or
# Tests of directed separation
dSep(psem_mod, .progressBar = FALSE)
# tests if the missing effect  is not different from 0

# check manually what the dSep() function does
summary(lm(y2 ~ y1+x1, data = data1))$coefficients[3, ]


# Derive the basis set
# basis set is the minimum number of independence claims 
# derived from a path diagram. 
basisSet(psem_mod)


# Model Fit in piecewiseSEM ----
# understanding of fit measures

## Fisher's C statistic----

fisherC(psem_mod)

# check manually what the function does:
C <- -2 * log(summary(lm(y2 ~ y1+x1, data = data1))$coefficients[3, 4])
C
1-pchisq(C, 2) # 2 DF (C is Chi-square distributed with 2k degrees of freedom, where k - number of independence claims)

# AIC for Fisher's C----
# AIC value based on the Fisher's C statistic and the d-sep tests
## The default is based on the log-likelihood (see below)
AIC(psem_mod, AIC.type = "dsep", aicc = TRUE) 

## ChiSq statistic ----
# Log-likelihood based statistic
#
LLchisq(psem_mod)

# check manually what the function does:
LL1 <- logLik(lm(y2 ~ y1,  data = data1)) - logLik(lm(y2 ~ y1+x1,  data = data1))
LL2 <- logLik(lm(y1 ~ x1, data = data1)) - logLik(lm(y1 ~ x1, data = data1))
#
ChiSq <- -2*sum(as.numeric(LL1), as.numeric(LL2))
ChiSq
#
DF <- 1 # one additional parameter estimated in the saturated model
1 - pchisq(ChiSq, DF)


#AIC for ChiSq
## The default is based on the log-likelihood (see below)
AIC(psem_mod, aicc = TRUE) 




# compare with lavaan:
# For models assuming multivariate normality (as we have here), the chi-square statistic and P-value are actually the same as we obtain from lavaan
library(lavaan)
sem_mod1 <- '
                 y1 ~ x1
                y2 ~ y1
'
sem_fit1 <- sem(sem_mod1, data = data1)
fit <- lavInspect(sem_fit1, "fit")
fit["chisq"]; fit["pvalue"]


# Model comparison----
psem_mod2 <- psem(
  lm(y1 ~ x1, data = data1),
  lm(y2 ~ y1+x1, data = data1))
#
# Chi-square difference test
#
anova(psem_mod2, psem_mod)
#
# AIC comparison 

AIC(psem_mod2, AIC.type = "dsep", )

aic <- AIC(psem_mod, psem_mod2, aicc = TRUE)
aic
d_aic <- aic[2] - min(aic[2])
d_aic


#----------------------#
# Categorical variables----
#---------------------#
data3 <- read_csv("Data/SEMdata2.csv")
str(data3)
#
model1 <- lm(y ~ Group, data3)
summary(model1)
predict(model1, data.frame(Group = "A")) # intercept is the mean of y in group "A"
mean(subset(data3, Group == "A")$y)

predict(model1, data.frame(Group = "B"))
mean(subset(data3, Group == "B")$y)


# 
# If we add a continuous covariate:
model2 <- lm(y ~ x+Group, data3)
# the marginal mean is evaluated while holding the covariate x at its mean value:
predict(model2, data.frame(Group = "A", x = mean(data3$x))) # it controls for the effect of x on y.
predict(model2, data.frame(Group = "B", x = mean(data3$x))) 
#
library(emmeans)
emmeans(model2, specs = "Group") # where specs is the variable or list of variables whose means are to be estimated
# Post-hoc tests of differences among the means of each factor level
emmeans(model2, list(pairwise ~ Group))
#
library(piecewiseSEM)

psem_model <- psem(model2)
#
coefs(psem_model)
summary(psem_model)
#
# for task 1
fw_data <- read.csv("Data/Food_web_data_2.csv")
str(fw_data)
#
#----------------#
# Interactions----
#----------------#
#
data(keeley)
#
psem_m1 <- psem(
  lm(cover ~ age*firesev, data = keeley),
  lm(firesev ~ age, data = keeley))
#
plot(psem_m1)
#
summary(psem_m1)
#
fisherC(psem_m1)# This is saturated model (no DF)
LLchisq(psem_m1)#
#
coefs(psem_m1)
#
with(keeley, cor(age, age*firesev))
#
# Centering for Interactions 
data2 <- data.frame(age_c=scale(keeley$age, scale=FALSE),
                    firesev_c=scale(keeley$firesev, scale=FALSE),
                    cover=keeley$cover)
#
with(data2, cor(age_c, age_c*firesev_c))
#
psem_m2 <- psem(
  lm(cover ~ age_c*firesev_c, data = data2),
  lm(firesev_c ~ age_c, data = data2))
#
plot(psem_m2)
#
summary(psem_m1)
#
fisherC(psem_m2)# This is saturated model (no DF)
LLchisq(psem_m2)#
#
coefs(psem_m2)
#
# Interactions using lavaan
library(lavaan)
sem_m1 <-' 
  firesev ~ age
  cover ~ firesev + age + firesev:age
'
sem_fit1 <- sem(sem_m1, data=keeley)
summary(sem_fit1, standardize = T)
#
library(lavaanPlot)
plot <- lavaanPlot(model = sem_fit1, 
                   coefs = TRUE, stand=TRUE,
                   graph_options = list(layout = "circo"),
                   stars = 'regress', # shows stars for regr coef
                   digits = 2) 
plot
#
modificationIndices(sem_fit1)
#
#
sem_m2<-' 
  firesev_c ~ age_c
  cover ~ firesev_c + age_c + firesev_c:age_c
'
sem_fit2 <- sem(sem_m2, data=data2)
summary(sem_fit2, standardize = T)
#
library(car)
m1<-lm(firesev ~ age, data = keeley)
Anova(m1)
m2<-lm(cover ~ age*firesev, data = keeley)
Anova(m2)
#
#--------------------
# Multigroup Analysis
#--------------------
#
data3 <- read_csv("Data/SEMdata2.csv")
str(data3)
#
anova(lm(y ~ x * Group, data3))
anova(lm(z ~ y * Group, data3))
#
library(piecewiseSEM)
#
psem_model <- psem(
  lm(y ~ x, data3),
  lm(z ~ y, data3)
)
#
#
multigroup(psem_model, group = "Group")
#
# for task 2
library(piecewiseSEM)
data(meadows)
str(meadows)
#
#
#
#--------------
# GLM
#--------------
#
anderson <- read_csv("Data/Anderson.csv")
str(anderson)

library(ggplot2)
qplot(biomass.kg, hotspotYN, data=anderson) +
  theme_bw(base_size=17) +
  stat_smooth(method="glm", method.args=list(family=binomial), color="red", lwd=2)
#
m1 <- lm(leafN ~ biomass.kg, anderson)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))
summary(m1)
#
m2 <- glm(hotspotYN ~ leafN + biomass.kg + landscape, family = "binomial", anderson)
summary(m2)
#
anderson.sem <- psem( m1,m2)
summary(anderson.sem)
# glm.nb and glmmPQL are used if data are overdispersed
