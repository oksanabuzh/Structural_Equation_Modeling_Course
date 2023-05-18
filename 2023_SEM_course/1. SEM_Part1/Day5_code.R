# Script from the lecture
# 24.02.2023
#==============================================#
# ------- Structural equation & mixed model course
# 
# ==============================================#
# Day 05  --------------------------------------
# Teaching & code: 
# Oksana Buzhdygan 
# oksana.buzh@fu-berlin.de
#
# ==============================================#

rm(list = ls()) # clears working environment

# Required packages----
library(tidyverse)
library(here)
install.packages ("lavaan")
library(lavaan)
install.packages ("lavaanPlot")
library(lavaanPlot)
install.packages ("DiagrammeRsvg")
library(DiagrammeRsvg)
install.packages ("rsvg")
library(rsvg)
install.packages ("semPlot")
library(semPlot)
library(ggcorrplot)


# Set up a local working directory
library(here)
path <- here::here()
path

# Data----
library(tidyverse)
data <- read_csv("Data/Grassl_data.csv")
names(data)
str(data)
#
# view the data
pairs(data)
#
# ==============================================#
# Part 1 ----
# ==============================================#
# First impression of lavaan
#
library(lavaan)
#
# Specify the SEM model
sem_mod <- '
    Grazing  ~  Elevation
    BareSoil  ~  Grazing 
    Diversity ~ Elevation + Grazing + BareSoil
'
# estimate the parameters and fit the model
sem.fit <- sem(sem_mod, data=data)
#
# Address the WARNING
# Call the model-implied covariance matrix
lavInspect(sem.fit, "obs")$cov
# Check the data scales
varTable(sem.fit)
#
# Transform the data: recode vars to roughly same scale
data$Diversity    <- data$Diversity/10
data$Elevation <- data$Elevation/100
#
# Repeat model estimation using transformed data
sem_mod <- '
    Grazing  ~  Elevation
    BareSoil  ~  Grazing 
    Diversity ~ Elevation + Grazing + BareSoil
'
# estimate the parameters and fit the model
sem.fit <- sem(sem_mod, data=data)
#
# extract results
summary(sem.fit, standardize = T)
#
# Results in graphical form
library(lavaanPlot)
# plot
lavaanPlot(model = sem.fit)

lavaanPlot(model = sem.fit, coefs = TRUE,
           stand=TRUE)
#
?lavaanPlot()
#
plot <- lavaanPlot(model = sem.fit, 
                   coefs = TRUE, stand=TRUE, graph_options = list(layout = "circo"), 
                   sig = 0.05)
plot
library(DiagrammeRsvg)
library(rsvg)

save_png(plot, "SEM_plot.png")
#
#
library(semPlot)
semPaths(sem.fit)
#
semPaths(sem.fit, what='std', nCharNodes=5, sizeMan=12,
         edge.label.cex=1.7, curvePivot = TRUE, fade=FALSE)
#
# ==============================================#
# Part 2 ----
# ==============================================#
# Variance, covariance ----
x <- c(1, 2, 3, 4) 
var(x)
#
y <- c(70, 30, 10, 90)
var(y)
#
cov(x,y)
mean(x)
mean(y)
#
cor(x,y)
# calculate by hand <- standardised covariation (i.e. correlation)
cov(x, y)/(sd(x)*sd(y))

#
# Path Coefficients----
#------------------#
data1 <- read_table("Data/SEMdata1.txt")
str(data1) 
#
# Specify the model in lavaan
sem_mod1 <- ' y1 ~ x1 
              y2 ~  x1 + y1
'
# Fit the model
sem.fit1 <- sem(sem_mod1, data=data1)
#
# Extract results
summary(sem.fit1, standardize = T)

# parameterEstimates() returns a data.frame containing all the model parameters in the rows
parameterEstimates(sem.fit1)
# see also
coef(sem.fit1)

# standardized coefficients
standardizedSolution(sem.fit1)

# Unstandardized vs standardized coefficients
# Standardization is generally implemented by 
# multiplying the coefficient Beta by the ratio of the standard deviation of x 
# over the standard deviation in y

# standardize the effect:
coef(sem.fit1)[1] *sd(data1$x1)/sd(data1$y1)
# check with the result table from lavaan
standardizedSolution(sem.fit1)[1 ,  "est.std"]

#convert back to the unstandardized effcets:
standardizedSolution(sem.fit1)[1 ,  "est.std"]* sd(data1$y1)/sd(data1$x1)
coef(sem.fit1)[1]


# Indirect effects----
# Naming the coefficients in lavaan
sem_mod1 <- '
     y2 ~ b1*x1 + b3*y1
     y1 ~ b2*x1
     # define direct, indirect and total effects  
     direct   := b1 
     indirect := b2*b3
     total    := b1 + (b2*b3)
     # or
     # total   := direct + indirect

'
sem.fit1 <- sem(sem_mod1, data=data1)
summary(sem.fit1, standardize = T)
#
# Get R-square----
summary(sem.fit1, standardize = T, rsq = T)
#
#
#
# ==============================================#
# Part 3 ----
# ==============================================#

# Covariance-based SEM----

order <- c("x1","y1","y2") 
round(cov(data1), 2)[order,order] # reorder columns and raw, round off the values

# Assessing model fit ----

# Specify the model in lavaan
sem_mod1 <- '
    y1 ~ x1 
    y2 ~ y1
'
# Fit the model
sem.fit1 <- sem(sem_mod1, data=data1)
# Extract results
summary(sem.fit1, standardize = T)
#
# Model-implied correlation matrix
Cor_M <- lavInspect(sem.fit1, what="cor.all")
Cor_M
order <- c("x1","y1","y2") 
round(Cor_M, 2)[order,order] # reorder columns and raw, round off the values
#
# Observed (data) correlation matrix
Cor_D <- lavCor(sem.fit1)
Cor_D
round(Cor_D, 2)[order,order]
#
# Residual correlations
Resid <- resid(sem.fit1, "cor")$cov
Resid
Resid<-round(Resid, 3)[order,order]
Resid
#
# Plot residual correlations
round(resid(sem.fit1, type="cor")$cov, 3)[order,order]
library(ggcorrplot)
ggcorrplot(Resid, type="lower")
#
#
## Chi-Square difference test
#
# Specify model 2 with link y2 ~ x1
sem_mod2 <- ' y1 ~ x1 
              y2 ~  y1 + x1
'
# Fit the model
sem.fit2 <- sem(sem_mod2, data=data1)
summary(sem.fit1, standardize = T)

# compare two models
anova(sem.fit1, sem.fit2)
#
# Extract additional fit measures for model 1
summary(sem.fit1, standardize = T,
                  fit.measures=T)
# or use
fitMeasures(sem.fit1)
#
# Example of how to present the fit statistics:
sem_mod1 <- '
    y1 ~ b1*x1 
    y2 ~ b2*y1
    # define indirect effect
    indirect := b1*b2
'
sem.fit1 <- sem(sem_mod1, data=data1)
summary(sem.fit1, standardize = T, rsq=T,
        fit.measures=T)
#
library(lavaanPlot)
plot <- lavaanPlot(model = sem.fit1, 
                   coefs = TRUE, stand=TRUE,
                   # graph_options = list(layout = "circo"),
                   stars = 'regress', # shows stars for regr coef
                   digits = 2) 
plot
save_png(plot, "plot1.png")
#
