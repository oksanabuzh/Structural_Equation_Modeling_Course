# Code for the course:
# Introduction to structural equation modeling and mixed models in R

# Day 9

# Oksana Buzhdygan
# oksana.buzh@fu-berlin.de
#
#

# install the development version of piecewiseSEM
devtools::install_github("jslefche/piecewiseSEM@devel")
#
library(piecewiseSEM)
#
data1 <- read.table("Data/SEMdata1.txt", header = T)
str(data1) 
#
# Check assumptions (reidual normality, constant residuals)
m1 <- lm(y1 ~ x1, data = data1)
par(mfrow=c(2,2))
plot(m1)
m2 <- lm(y2 ~ y1, data = data1)
plot(m2)
par(mfrow=c(1,1))
#
# Model specification in piecewiseSEM
psem_mod1 <- psem(
  lm(y1 ~ x1, data = data1),
  lm(y2 ~ y1, data = data1))
# 
# or
psem_mod1 <- psem(m1, m2)
#
psem_mod1
plot(psem_mod1)
plot(psem_mod1, node_attrs = list(shape = "rectangle",
                                  fillcolor = "white"))
#
# derive the basis set
basisSet(psem_mod1)
#
# Tests of directed separation
dSep(psem_mod1, .progressBar = FALSE)
#
summary(lm(y2 ~ y1+x1, data = data1))$coefficients[3, ]
#
#---------------
# Model fit
#---------------
#
# Fisher's C statistic
#
fisherC(psem_mod1)
#
C <- -2 * log(summary(lm(y2 ~ y1+x1, data = data1))$coefficients[3, 4])
C
1-pchisq(C, 2) # 2 DF (C is Chi-square distributed with 2k degrees of freedom, where k - number of independence claims)
# 
# AIC value based on the Fisher's C statistic and the d-sep tests
## The default is based on the log-likelihood (see below)
AIC(psem_mod1, AIC.type = "dsep", aicc = TRUE) 
#
# Log-likelihood based ??2 statistic
# 
LLchisq(psem_mod1)
#
LL1 <- logLik(lm(y2 ~ y1,  data = data1)) - logLik(lm(y2 ~ y1+x1,  data = data1))
LL2 <- logLik(lm(y1 ~ x1, data = data1)) - logLik(lm(y1 ~ x1, data = data1))
#
ChiSq <- -2*sum(as.numeric(LL1), as.numeric(LL2))
ChiSq
#
DF <- 1 # one additional parameter estimated in the saturated model
1 - pchisq(ChiSq, DF)
#AIC, AICc
## The default is based on the log-likelihood (see below)
AIC(psem_mod1, aicc = TRUE) 
#
rsquared(psem_mod1)
#
# For models assuming multivariate normality (as we have here), the chi-square statistic and P-value are actually the same as we obtain from lavaan
library(lavaan)
sem_mod1 <- '
                 y1 ~ x1
                y2 ~ y1
'
sem_fit1 <- sem(sem_mod1, data = data1)
fit <- lavInspect(sem_fit1, "fit")
fit["chisq"]; fit["pvalue"]
#
# All the above can be executed simultaneously using the summary function
summary(psem_mod1)
#
#
# Model comparison
psem_mod2 <- psem(
  lm(y1 ~ x1, data = data1),
  lm(y2 ~ y1+x1, data = data1))
#
# Chi-square difference test
#
anova(psem_mod2, psem_mod1)
#
# AIC comparison 
#
aic <- AIC(psem_mod1, psem_mod2)
aic
d_aic <- aic[1] - min(aic[1])
d_aic
#
#
#----------------------
# Categorical variables
#---------------------
data3 <- read.csv("Data/SEMdata2.csv")
str(data3)
#
model1 <- lm(y ~ Group, data3)
summary(model1)
predict(model1, data.frame(Group = "A")) # intercept is the mean of y in group "A"
mean(subset(data3, Group == "A")$y)
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
#----------------
# Interactions
#----------------
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
data3 <- read.csv("Data/SEMdata2.csv")
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
anderson <- read.csv("Data/Anderson.csv")
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
#
# -----------------------
# for task 3
# library(piecewiseSEM)
data(shipley)
str(shipley)
#
library(nlme)
library(lme4)
#
lme(DD ~ lat, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley)
#  
lme(Date ~ DD, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley)
#  
lme(Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley)
# 
glmer(Live ~ Growth + (1 | site) + (1 | tree),
        family = binomial(link = "logit"), data = shipley)



lmer(Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit,
    data = shipley),