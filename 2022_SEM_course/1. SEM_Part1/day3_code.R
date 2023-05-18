# Code for the course:
# Introduction to structural equation modeling and mixed models in R

# Day 3

# Oksana Buzhdygan
# oksana.buzh@fu-berlin.de
#
#
data <- read.csv("Data/Grassl_data.csv")
names(data)
str(data)
#
# view the data
pairs(data)
#
#-------------
# Day 3 part 1
#-------------
# First impression of 'lavaan'
#----------------------------
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
save_png(plot, "plot.png")
#
#
library(semPlot)
semPaths(sem.fit)
#
semPaths(sem.fit, what='std', nCharNodes=5, sizeMan=12,
         edge.label.cex=1.7, curvePivot = TRUE, fade=FALSE)
#
#
#
#-------------
# Day 3 part 2
#-------------
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
# calculate by hand
cov(x, y)/(sd(x)*sd(y))
#
#
#
# Path Coefficients
#------------------
data1 <- read.table("Data/SEMdata1.txt", header = T)
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
#
# Indirect effects
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
# Get R-square
summary(sem.fit1, standardize = T, rsq = T)
#
#
#
#-------------
# Day 3 part 3
#-------------
#
cov(data1)
#
# Assessing model fit
#---------------------
#
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
# compare two models
anova(sem.fit1, sem.fit2)
#
# Extract additional fit measures for model 1
summary(sem.fit1, standardize = T,
                  fit.measures=T)
# or use
fitMeasures(sem.fit1)
#
# Indirect effect
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
