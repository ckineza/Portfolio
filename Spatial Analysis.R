install.packages("MuMIn")


library(dplyr)
library(downloader)
library(readxl)
library(MuMIn)

set = setwd("~/Desktop/Work/Asthma Research ")

Baltimore.Health = read.csv("~/Desktop/Work/Asthma Research /BaltimoreDataUpdated.csv", header=TRUE, sep=",")

Baltimore.Health = as.data.frame(Baltimore.Health)



Baltimore.Health = Baltimore.Health[apply(Baltimore.Health,1,function(x)any(!is.na(x))),] # delete the rows that are empty 
Baltimore.Health = Baltimore.Health[, colSums(is.na(Baltimore.Health)) != nrow(Baltimore.Health)]# delete columns with all Nas
Baltimore.Health = Baltimore.Health[ , colSums(is.na(Baltimore.Health)) == 0] #delete columns with at least One Na


lower.income = subset(Baltimore.Health,(Baltimore.Health$income < 33224 & Baltimore.Health$income  > 25049))
higher.income = subset(Baltimore.Health,(Baltimore.Health$income < 60164  & Baltimore.Health$income  > 41218))

Baltimore.Health$income.ind = ifelse((Baltimore.Health$income < 33224 & Baltimore.Health$income  > 25049),1,
                                     ifelse((Baltimore.Health$income < 60164  & Baltimore.Health$income  > 41218),2,3))


plot(Baltimore.Health$income,Baltimore.Health$Tobacco, col = Baltimore.Health$income.ind)
legend("topright", levels(Baltimore.Health$income.ind), fill = Baltimore.Health$income.ind)


Baltimore.Health.subset = subset(Baltimore.Health,Baltimore.Health$income.ind == 1 | Baltimore.Health$income.ind == 2)
library(ggplot2)
#reg1 <- lm(data = Baltimore.Health.subset, Tobacco~income)
ggplot(Baltimore.Health.subset,aes(x=income,y=Tobacco)) + 
  geom_point(aes(colour = factor(income.ind))) +
  xlab("Income") + ylab("Tobacco Density") +
  #abline(reg1)
  
  scale_color_discrete(name = "Income Levels",
                       labels = c("lower","Higher")) +
  #scale_color_manual(values = c("orange",
  #                             "purple"))
  
  
  ggplot(Baltimore.Health,aes(x=income,y=LifeExp)) + 
  geom_point(aes(colour = factor(income.ind))) +
  xlab("Income") + ylab("Life Expectancy")


Baltimore.lower= subset(Baltimore.Health,Baltimore.Health$income.ind == 1)
reg1 <- lm(data = Baltimore.lower, Tobacco~income)
plot(Baltimore.lower$income,Baltimore.lower$Tobacco)
abline(reg1)

#+ 

#  geom_smooth(method = lm)


#Run Multiple Linear Regression

dat = lm(LifeExp ~ income + Poverty + Tobacco, data = Baltimore.Health)


#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.921e+01  1.957e+00  35.360  < 2e-16 ***
# income       1.492e-04  3.094e-05   4.823 1.32e-05 ***
#  Poverty     -1.003e+01  5.195e+00  -1.930   0.0592 .  
#Tobacco     -7.914e-02  1.637e-02  -4.834 1.27e-05 ***

dat.1 = lm(LifeExp ~ income + Tobacco, data = Baltimore.Health)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.61e+01   1.11e+00   59.64  < 2e-16 ***
# income       1.94e-04   2.08e-05    9.34  1.0e-12 ***
# Tobacco     -8.83e-02   1.61e-02   -5.49  1.2e-06 ***
# ---
dat.6 = lm(LifeExp ~ income , data = Baltimore.Health)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 6.22e+01   1.07e+00   58.16  < 2e-16 ***
#income      2.30e-04   2.46e-05    9.35  8.4e-13 ***

dat.2 = lm(LifeExp ~ income * Tobacco + Poverty, data = Baltimore.Health)


#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     6.906e+01  2.216e+00  31.165  < 2e-16 ***
#  income          1.530e-04  4.025e-05   3.801 0.000392 ***
#  Tobacco        -7.191e-02  5.138e-02  -1.399 0.167840    
#Poverty        -1.011e+01  5.279e+00  -1.916 0.061129 .  
#income:Tobacco -1.875e-07  1.263e-06  -0.149 0.882541 


dat.3 = lm(LifeExp ~ income + Tobacco * Poverty, data = Baltimore.Health)

# Models with interaction should also include the main effects even if the main effects are not significant.
#Otherwise main effects and interaction effects can be confounded.
dat.4 = lm(LifeExp ~ income + Tobacco + Poverty + Tobacco * Poverty, data = Baltimore.Health)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      6.942e+01  2.227e+00  31.177  < 2e-16 ***
#  income           1.483e-04  3.157e-05   4.696 2.11e-05 ***
#  Tobacco         -8.700e-02  4.100e-02  -2.122   0.0388 *  
#  Poverty         -1.112e+01  7.388e+00  -1.505   0.1387    
#Tobacco:Poverty  4.046e-02  1.931e-01   0.210   0.8349  

# See all combinations of linear models


globalmodel = lm(LifeExp ~ income + Poverty + Tobacco, data = Baltimore.Health, na.action = "na.fail")
combinations <- dredge(globalmodel)
print(combinations)

reg1 <- lm(data = Baltimore.Health, LifeExp~income)
plot(Baltimore.Health$income,Baltimore.Health$LifeExp)

ggplot(Baltimore.Health, aes(x = income,
                             y = LifeExp)
)+
  geom_point() +
  geom_smooth(method = lm)




ggplot(Baltimore.Health, aes(x = Tobacco,
                             y = LifeExp)
)+
  geom_point() +
  geom_smooth(method = lm)



xDen <- ggplot(Baltimore.Health, aes(x=Tobacco, fill = LifeExp, col=LifeExp))+
  geom_density(alpha=0.1)+
  theme_classic()+
  xlab("")


baltimore.health.subset = Baltimore.Health[,c("income","Tobacco","LifeExp")]
pairs(baltimore.health.subset )

# Determine correlation coefficient

cor(Baltimore.Health$LifeExp, Baltimore.Health$income) #0.789
cor(Baltimore.Health$LifeExp, Baltimore.Health$Tobacco) # -0.6


#From these outputs our suspicion is confirmed x and y have a high positive correlation,
#but as always in statistics we can test if this coefficient is significant.
#Using parametric assumptions (Pearson, dividing the coefficient by its standard error, 
#giving a value that follow a t-distribution) or
#when data violate parametric assumptions using Spearman rank coefficient.

cor.test(Baltimore.Health$LifeExp, Baltimore.Health$income,method="pearson")
cor.test(Baltimore.Health$LifeExp, Baltimore.Health$Tobacco,method="pearson")



plot(LifeExp ~ income, data=Baltimore.Health)

### the lowess() function will take a formula interface but will not use
### the data= option; an oversight in the programming, I suspect

### the other method is (close the graphics device first)...

#scatter.smooth(Baltimore.Health$LifeExp ~ Baltimore.Health$income)       # same programming flaw here
scatter.smooth(Baltimore.Health$LifeExp ~ Baltimore.Health$income, pch=16, cex=.6)
lines(lowess(Baltimore.Health$LifeExp ~ Baltimore.Health$income), col="red")



#Residuals against Fitted

#When conducting any statistical analysis it is important to evaluate how well
#the model fits the data and that the data meet the assumptions of the model.
#There are numerous ways to do this and a variety of statistical tests
#to evaluate deviations from model assumptions


#When you run a regression analysis, the variance of the error terms must be constant, and they must have a mean of zero. 
#If this isn't the case, your model may not be valid.
#To check these assumptions, you should use a residuals
#versus fitted values plot. Below is the plot from the regression analysis 


plot(resid(dat) ~ fitted(dat),xlab = "Fitted Values",ylab = "Residuals")
abline(0,0)

#t test would be between two groups 


#Subjects are not independent,account for heterogeneity. 
#independent of each other: relationship of tobacco, t test , 
#multiple linear regression, find if its significant
#test for independence, variance are same, errors are normally distributed, 
#square regression base R for proper estimate of variance, 
#plot errors vs residuals




. 
#fits plot and what they suggest about the appropriateness of the simple linear regression model:

# The residuals "bounce randomly" around the 0 line. 
#This suggests that the assumption that the relationship is linear is reasonable.
#The residuals roughly form a "horizontal band" around the 0 line.
#This suggests that the variances of the error terms are equal.




#b. Plotting residuals against PM10. Add a smooth curve to help identify any patterns
#in the level of the residuals that would require a change in the model specification

plot(Baltimore.Health$income, residuals(dat), pch = 16, col = rgb(0.3, 0.3, 0.3, 0.5))
points(smooth.spline(Baltimore.Health$income, residuals(dat)), type = "l", col = "red", 
       lwd = 3) #non linear 

#plot(Baltimore.Health$income, residuals(dat.6), pch = 16, col = rgb(0.3, 0.3, 0.3, 0.5))
#points(smooth.spline(Baltimore.Health$income, residuals(dat.6)), type = "l", col = "red", 
#      lwd = 3)


#non-linear trend of the residuals was found, severe enough to change model specification 

#c.plotting squared residuals against predicted values and add a smooth regression curve to assess
#the constant variance assumption

plot(fitted.values(dat.1), residuals(dat.1)^2, pch = 16, col = rgb(0.3, 
                                                                   0.3, 0.3, 0.5))
points(smooth.spline(fitted.values(dat.1), residuals(dat.1)^2), type = "l", 
       col = "red", lwd = 3)

#plot(fitted.values(dat.6), residuals(dat.6)^2, pch = 16, col = rgb(0.3, 
#                                                                  0.3, 0.3, 0.5))
#points(smooth.spline(fitted.values(dat.6), residuals(dat.6)^2), type = "l", 
#      col = "red", lwd = 3)


#d. making a Q-Q plot of the residuals against the Gaussian (normal) distribution
qqnorm(residuals(dat), ylab = "Residuals", pch = 16, col = rgb(0.3, 0.3, 
                                                               0.3, 0.5))

logexp = log(Baltimore.Health$LifeExp)
hist(Baltimore.Health$LifeExp)
hist(  logexp )


#The Q-Q plot displays a bad straight line, even at both tails


#e.plotting the DFBETAs for PM10 against the predicted values to identify any 
#highly influential observations. If there is one or a few days that largely determine 
#the PM10 coefficient, drop them and refit the model.

dfbetas_3.9 = as.data.frame(dfbeta(dat))
dfbetas_3.9.income=Baltimore.Health$income
plot(fitted.values(dat), dfbetas_3.9.income, pch = 16, col = rgb(0.3, 0.3, 
                                                                 0.3, 0.5))


#If the number of data points is small, it may be difficult to detect assumption violations. 
#With small samples, violation assumptions such as nonnormality or heteroscedasticity of variances
#are difficult to detect even when they are present. With a small number of data points 
#linear regression offers less protection against violation of assumptions. With few data points,
#it may be hard to determine how well the fitted line matches the data, 
#or whether a nonlinear function would be more appropriate.

#Even if none of the test assumptions are violated, 
#a linear regression on a small number of data points may not have 
#sufficient power to detect a significant difference between the slope and 0,
#even if the slope is non-zero. The power depends on the residual error, 
#the observed variation in X, the selected significance (alpha-) level of the test,
#and the number of data points. Power decreases as the residual variance increases,
#decreases as the significance level is decreased (i.e., as the test is made more stringent),
#increases as the variation in observed X increases, and increases as the number of data points increases.
#If a statistical significance test with a small number of data values produces 
#a surprisingly non-significant P value, then lack of power may be the reason.





# Exploratory Analysis Taking care of Spatial autocorrelation
install.packages("ctv")
install.packages("maptools")
install.packages("rgdal")
install.packages("spdep")
install.views("Spatial")
library(ctv)
library(maptools)
library(rgdal)
library(spdep)
library(spatial)
library(maptools)

#Importing a shapefile
getinfo.shape("~/Desktop/Census_Info/VS14_Census.shp") #Shapefile type: Polygon, (5), # of Shapes: 100
#Shapefile type: Polygon, (5), # of Shapes: 55
sids <- readShapePoly ("~/Desktop/Census_Info/VS14_Census.shp")
class(sids)

#Load a shapefile.

shape2 = readOGR(dsn=path.expand("~/Desktop/Census_Info"), layer="VS14_Census")
shape2 <- sp::spTransform(shape2, CRS("+proj=longlat +datum=WGS84")) #convert coord to long/lat # not sure why it was gor???


head(shape2@polygons[[1]]@Polygons[[1]]@coords, 3)#The head function in the first line of the code above simply means “show the first few lines of data”
plot(shape2@polygons[[1]]@Polygons[[1]]@coords) #Raw coordinate data displayed and plotted in R.

plot(shape2)


# using the Children and Family Health shapefile

shape3 = readOGR(dsn=path.expand("~/Desktop/VS 14 Children & Family Health"), layer="VS14_Health")
#shape3 <- sp::spTransform(shape3, CRS("+proj=longlat +datum=WGS84"))
head(shape3@data)
mean(shape3$LifeExp11)#2010 life expectancy
head(shape3@polygons[[1]]@Polygons[[1]]@coords, 3)
plot(shape3@polygons[[1]]@Polygons[[1]]@coords)
plot(shape3) 
lifeef = shape3@data[shape3$LifeExp11 < 70.0, ] # select rows where life expectancy is the lowest


# Places where life expectancy is less than 70 years old
plot(shape3, col = "lightgrey") # plot the london_sport object
sel <- shape3$LifeExp11 < 70.0
plot(shape3[sel, ], col = "turquoise", add = TRUE) # add selected zones to map 


library(rgdal)
# create you can use new shapefiles. 
#stations <- readOGR(dsn = "data", layer = "lnd-stns") 
proj4string(shape3) # this is the full geographical detail. 
proj4string(shape3) # what's the coordinate reference system (CRS) 
bbox(shape3) # the extent, 'bounding box' of stations 
bbox(shape3) # return the bounding box of the lnd object


plot(shape2) # Baltimore Map 
head(shape2@data)
str(shape2)

sp::coordinates(shape2)
library(geosphere)

csa.dists <- geosphere::distm(sp::coordinates(shape2), fun = distVincentyEllipsoid) #dist(cbind(mtdata$lon.med, mtdata$lat.med), method = "euclidean") #gets the distance between coordiantes in shapefile 
csa.dists <- as.dist(csa.dists) 

#Outcome
le11.dists <- dist(Baltimore.Health$LifeExp, method = "euclidean")
ade4::mantel.randtest(csa.dists, le11.dists, nrepet = 9999) 
# functions that calculates mantel test you need a distance matrix.

#Monte-Carlo test
#Call: ade4::mantel.randtest(m1 = csa.dists, m2 = le11.dists, nrepet = 9999)

#Observation: 0.03098254 

#Based on 9999 replicates
#Simulated p-value: 0.2778 
#Alternative hypothesis: greater 

#Std.Obs   Expectation      Variance 
#0.5721989656 -0.0008173283  0.0030885636 

#The Mantel test tests the correlation between two distance matrices. It is non-parametric and computes the significance of the
#correlation through permutations of the rows and columns of one of the input distance matrices
#The p-value of  0.2778  indicates that our results are not statistically significant at an alpha of 0.05. We determined the p-value by specifying 999 permutations with the -n option.
#By default, the p-value is calculated using a two-tailed test, though this can be changed using the -t option.
# Since the mantel statistic is unsignificant we fail to reject the null hypothesis, the distance among points in matrix B are not linearly
#correlated to the corresponding distances 
#Moran's I


#Moran's I is a measure of spatial autocorrelation--how related the values of a variable are based on the locations where they were measured.
csa.dists <- as.matrix(csa.dists)

csa.dists.inv <- 1/csa.dists
diag(csa.dists.inv) <- 0  

ape::Moran.I(Baltimore.Health$LifeExp, csa.dists.inv) # 2 sided test

#$observed
#[1] 0.03114631

#$expected
#[1] -0.01851852

#$sd
#[1] 0.01676511

#$p.value
# [1] 0.003052587 #We can now conclude for a slighly significant positive correlation

#Based on these results, we can reject the null hypothesis that there is zero
#spatial autocorrelation present in the variable Av8top at alpha = .05.

#Based on these results, we can reject the null hypothesis that there is zero spatial autocorrelation present in the variable LifeExp at alpha = .05.

# Moran_I  Test




## Non-Contiguity Neighbors (snap distance = 100,000km)

# W_cont_s <- poly2nb(csa.shp, queen=F, snap= km2d(100000))
#W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)

shape = readOGR(dsn=path.expand("~/Desktop/Census_Info"), layer="VS14_Census")
class(shape)
Baltimore.Health$LogLifeExp = log(Baltimore.Health$LifeExp)
coords = coordinates(shape)
IDs = row.names(as(shape, "data.frame"))
bost_kd1 = dnearneigh(coords, d1 = 0, d2 = 1000, row.names = IDs) # used to find neighbours with an interpoint distance
plot(shape)
plot( bost_kd1,coords, add = T) #These two plots have to run together at the same time 

#bost_kd1_w = nb2listw(bost_kd1, zero.policy = TRUE)
spdep::poly2nb(shape) -> shape.nb
nb2listw(shape.nb, zero.policy = TRUE) -> result
print(result, zero.policy = T)


moran.test(Baltimore.Health$LogLifeExp,listw = result,zero.policy = TRUE)


#Moran I test under randomization

#data:  Baltimore.Health$LogLifeExp  
#weights: result  

# Moran I statistic standard deviate = 2.2169, p-value = 0.01331
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.168582233      -0.018867925       0.007149311

# pretty small and do the same thing for residuals, worry standard error of coeficcients,
#sandwich type estimator



#we can reject the null hypothesis that there
#is zero spatial autocorrelation present in the variable LifeExp at alpha = .05.

#Get the IDs
Baltimore.Health$ID = IDs
moran.plot(Baltimore.Health$LogLifeExp,result,labels = as.character(Baltimore.Health$ID))




#Moran's I on the DV

# So the above didnt work so ignore that for now, we are going to select a model first
# then after use OLS and check autocorrelation in residuals, if there is autocorrelation did what they did in the
#slides




# None of the interactions terms are significant 

#OLS Regression

bostlm = lm(LogLifeExp ~ Tobacco + income + Poverty+ income, data = Baltimore.Health)

#Call:
# lm(formula = LogLifeExp ~ Tobacco + income + Poverty + income, 
#   data = Baltimore.Health)

# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.068377 -0.024799 -0.001393  0.018527  0.081291 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.244e+00  2.719e-02 156.085  < 2e-16 ***
#   Tobacco     -1.125e-03  2.274e-04  -4.949 8.55e-06 ***
#   income       1.949e-06  4.299e-07   4.533 3.54e-05 ***
#   Poverty     -1.574e-01  7.217e-02  -2.181   0.0338 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.03227 on 51 degrees of freedom
# Multiple R-squared:  0.7783,	Adjusted R-squared:  0.7652 
# F-statistic: 59.67 on 3 and 51 DF,  p-value: < 2.2e-16

Baltimore.Health$lmresid = residuals(bostlm)
lm.morantest(bostlm, result,zero.policy = TRUE)

#Moran's I test for spatial autocorrelation in residuals from an estimated linear model (lm()).

#Value


#A list with class htest containing the following components:

# statistic	: the value of the standard deviate of Moran's I.
#p.value	: the p-value of the test.
#estimate :	the value of the observed Moran's I, its expectation and variance under the method assumption.
#alternative: a character string describing the alternative hypothesis.
#method	:a character string giving the method used.
#data.name	:a character string giving the name(s) of the data




#Global Moran I for regression residuals

# data:  
#  model: lm(formula = LogLifeExp ~ Tobacco + Poverty + income, data = Baltimore.Health)
#weights: result

#Moran I statistic standard deviate = 2.4701, p-value = 0.006755
#alternative hypothesis: greater
#sample estimates:
#   Observed Moran I      Expectation         Variance 
# 0.181504350     -0.023025259      0.006856474 


#linear mixed model with neighborhood effect run a g on that, linear mixed effect



#Determining the Type of Independence


lm.LMtests(bostlm,result,zero.policy = TRUE,test="all")

#Lagrange multiplier diagnostics for spatial dependence

#data:  
#   model: lm(formula = LogLifeExp ~ Tobacco + income + Poverty + income, data = Baltimore.Health)
# weights: result

# LMerr = 4.357, df = 1, p-value = 0.03686


# Lagrange multiplier diagnostics for spatial dependence

# data:  
#   model: lm(formula = LogLifeExp ~ Tobacco + income + Poverty + income, data = Baltimore.Health)
# weights: result

# LMlag = 0.12799, df = 1, p-value = 0.7205


# Lagrange multiplier diagnostics for spatial dependence

# data:  
#   model: lm(formula = LogLifeExp ~ Tobacco + income + Poverty + income, data = Baltimore.Health)
# weights: result

# RLMerr = 4.3099, df = 1, p-value = 0.03789


# Lagrange multiplier diagnostics for spatial dependence

#data:  
#   model: lm(formula = LogLifeExp ~ Tobacco + income + Poverty + income, data = Baltimore.Health)
# weights: result

# RLMlag = 0.080884, df = 1, p-value = 0.7761


# Lagrange multiplier diagnostics for spatial dependence

# data:  
#   model: lm(formula = LogLifeExp ~ Tobacco + income + Poverty + income, data = Baltimore.Health)
# weights: result

# SARMA = 4.4378, df = 2, p-value = 0.1087


# One more diagnostic

install.packages("lmtest") 
library(lmtest)

bptest(bostlm)


# studentized Breusch-Pagan test


# data:  bostlm
# BP = 0.94793, df = 3, p-value = 0.8138



#Running a spatial lag model

bostlag = lagsarlm(LogLifeExp ~ Tobacco + Poverty+ income, data = Baltimore.Health, result, zero.policy = TRUE)

#Call:lagsarlm(formula = LogLifeExp ~ Tobacco + Poverty + income, data = Baltimore.Health, 
#               listw = result, zero.policy = TRUE)

# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0687882 -0.0250253  0.0023627  0.0180840  0.0809759 

# Type: lag 
# Regions with no neighbours included:
#   3 
# Coefficients: (numerical Hessian approximate standard errors) 
# Estimate  Std. Error  z value  Pr(>|z|)
# (Intercept)  4.2335e+00  3.9826e-02 106.2989 < 2.2e-16
# Tobacco     -1.1297e-03  2.1906e-04  -5.1572 2.507e-07
# Poverty     -1.5685e-01  6.9400e-02  -2.2600   0.02382
# income       1.9421e-06  4.1371e-07   4.6944 2.674e-06

#Rho: 0.0026393, LR test value: 0.12835, p-value: 0.72015
#Approximate (numerical Hessian) standard error: 0.0073699
#z-value: 0.35813, p-value: 0.72025
#Wald statistic: 0.12825, p-value: 0.72025

#Log likelihood: 112.9501 for lag model
#ML residual variance (sigma squared): 0.00096329, (sigma: 0.031037)
#Number of observations: 55 
#Number of parameters estimated: 6 
#AIC: -213.9, (AIC for lm: -215.77)

# Checking if there is any improvement with lag, here we are using log 
# 
bostlag_1 = lagsarlm(LifeExp ~ Tobacco + Poverty+ income, data = Baltimore.Health, result, zero.policy = TRUE)

anova(bostlag_1, dat)

#Model df    AIC  logLik Test L.Ratio p-value
#bostlag_1     1  6 256.08 -122.04    1                
# dat           2  5 254.63 -122.31    2 0.55395 0.45671
# Lag here does not improve the model

## Checking Other Model requirements

dat = lm(LifeExp ~ income + Poverty + Tobacco, data = Baltimore.Health)

par(mfrow=c(3,1))
plot(Baltimore.Health$income,residuals(dat),xlab="income")
abline(0,0)
plot(Baltimore.Health$Poverty,residuals(dat),xlab="Poverty")
abline(0,0)
plot(Baltimore.Health$Tobacco,residuals(dat),xlab="Tobacco")
abline(0,0)
plot(Baltimore.Health$income,Baltimore.Health$LifeExp)


plot(dat$fitted.values,dat$residuals) #The first plot shows a random pattern, indicating a good fit for a linear model.
abline(0,0)

# checking normality 
qqnorm(residuals(dat), ylab = "Residuals", pch = 16, col = rgb(0.3, 0.3, 
                                                               0.3, 0.5))
#plotting squared residuals against predicted values and add a smooth regression curve to assess the constant variance assumption

plot(fitted.values(dat), residuals(dat), pch = 16, col = rgb(0.3, 
                                                             0.3, 0.3, 0.5))
points(smooth.spline(fitted.values(dat), residuals(dat)), type = "l", 
       col = "red", lwd = 3) # there is no pattern 
abline(0,0)
hist(Baltimore.Health$LifeExp, breaks = 20)

qqnorm(residuals(dat))
qqline(residuals(dat), col = "red") # normally distributed   

# Choose model First 

fit.1 = lm(LifeExp ~ income + Poverty + Tobacco, data = Baltimore.Health)
fit.2 = lm(LifeExp ~ income + Tobacco, data = Baltimore.Health)
fit.3 = lm(LifeExp ~ income , data = Baltimore.Health)
fit.4 = lm(LifeExp ~ Tobacco, data = Baltimore.Health)
fit.5 = lm(LifeExp ~ income + Poverty, data = Baltimore.Health)
fit.6 = lm(LifeExp ~ income * Tobacco+ Poverty , data = Baltimore.Health)
fit.7 = lm(LifeExp ~ income * Poverty+ Tobacco , data = Baltimore.Health)
fit.8 = lm(LifeExp ~ income * Tobacco, data = Baltimore.Health)

print(compare.1 <- anova(fit.1, fit.2, test = "LRT"))
pval_6vs4 = compare.1$"Pr(>Chi)"[2] # extended model doesnt improve fit

print(compare.2 <- anova(fit.1, fit.6, test = "LRT"))
pval_7vs4 = compare.2$"Pr(>Chi)"[2] # we dont need the interaction, p value not significant

print(compare.3 <- anova(fit.1, fit.7, test = "LRT"))
pval_8vs4 = compare.3$"Pr(>Chi)"[2]  #  we dont need the interaction, p value not significant


print(compare.4 <- anova(fit.1, fit.3, test = "LRT"))
pval_9vs4 = compare.4$"Pr(>Chi)"[2]  # p value signicant so we need additional values , it improves the fit


print(compare.5 <- anova(fit.2, fit.3, test = "LRT"))
pval_10vs4 = compare.5$"Pr(>Chi)"[2] # we need the additional term Tobacco , p is significant


print(compare.6 <- anova(fit.2, fit.8, test = "LRT"))
pval_11vs4 = compare.6$"Pr(>Chi)"[2] # extended model does not improve fit


print(compare.7 <- anova(fit.2, fit.6, test = "LRT"))
pval_12vs4 = compare.7$"Pr(>Chi)"[2] # no need for additional terms





