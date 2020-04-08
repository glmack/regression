# Load libraries
# install.packages('tidyverse', dependencies = TRUE)
# install.packages('rmarkdown', dependencies = TRUE)
# install.packages("tinytex", dependencies=TRUE)

# Import libraries
library('car')
library('alr3')
library('faraway')
library('lmtest')
library('sandwich')

# I/O
# read data from url csv into data.frame object
d <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv')
class(d)  # class of object d, returns data.frame

# Exploratory Data Analysis

# inspect summary stats, structure and names of vars in df
summary(d)
str(d)
dim(d) 
names(d)

# Model

# fit simple linear model, regress api00 on enroll, using lm module
m1 <- lm(formula = api00 ~ enroll, data = d)
class(m1) # return object of lm class

# coefficient
print(m1)
# constant is 744.2514
# coefficient for enroll is -.1999 
# for a one unit increase in enroll, we would expect a .1999 unit decrease in api00. 
# in other words, a school with 1000 students would be expected (on average) to 
# have an api score 20 units more than a school with 1100 students.

summary(m1) # summarize model

# call shows model specification.
# summary statistics of residuals estimated which is:
# TODO convert to Latex ei=yi−ŷ 

# coefficients table of estimated coefficients
# standard error, t-value, and p-value of hyp test of coefficients equal to zero

# p-value < 0.001 then 3 stars, strong significant evidence that coefficient is not zero 
# p-value less than 0.01 and greater than 0.001 we have 2 star, 
# p-value between 0.01 and 0.05 we get “*" and between 0.05 and 0.1 we have “.”.

# both intercept and slope are significant.

# residual standard error is 135 and its degree of freedom is 398
# multiple R-squared, R-squared of the model, is 0.1012
# adjusted R-squared, adjusted for number of predictors, is 0.09898

# in simple linear regression model, R-squared equal to square of
# correlation between response and predicted variable. 

# calculate corr coefficient
r <- cor(d$api00, d$enroll)
print(r)
# calculate r-squared
r^2

# overal significance of the model against null model, model with only intercept
# F-statistic is 44.83 with 1 and 398 degrees of freedom, p-value equal to 7.339e-11.
# p-value in a simple regression model is exactly equal to p-value of the slope.

ls(m1) # list components of lm class

m1$coefficients # coefficients

confint(m1) # return matrix of confidence interval for coefficients

# scatterplot of predicted and outcome vars with regression line
plot(api00 ~ enroll, data=d)

# results suggest outliers
# add text with labels = d$snum on plot to label school number for each point
text(d$enroll, d$api00+20, labels = d$snum, cex = 0.7)
abline(m1, col='blue')

# If we use only intercept to model the response variable the regression line
# will be horizontal line at mean of the response variable
#  mean of api00 which is the line  y=647.6225
mean(d$api00)

# residuals for line will be TODO Latex yi−y¯

# break down this error to two part using the predicted value from regression of api00 by predicted variable enroll.

$latex y
# yi−y¯=yi−yi^+yi^−y¯

# where y¯ is the mean of response, yi^ is the fitted value from the regression model including predictor variable and yi is the observed response.

# SST=RSS+SSreg
# Sum of Square Total (SST)
# Sum of Square Regression (SSreg)
# Residual Sum of Square (RSS)

# ratio of SSreg to SST is called R-squared
# R-square is percentage of error that can be explained by the model

# observe sum of squares of model
anova(m1) #anova table
# sum of square of regression model in row named enrol
# sum of square of residuals with their degrees of freedom
# F statistics
# Mean squares is sum of squares divided by degrees of freedom.

# multiple regression - regress api00 on enroll, meals, full
m2 <- lm(api00 ~ enroll + meals + full, data = d)
print(m2) # coefficients
summary(m2) #summarize model

# results show R-square is 0.8308 - approximately 83% of the variability of api00
# is accounted for by the variables in the model. 
# adjusted R-squared of .8295 - after taking the account of number of predictors
# in model R_square is still about 0.83.

# coefficients for each of the variables indicates the amount of change one could
# expect in api00 given a one-unit change in the value of that variable, 
# given that all other variables in the model are held constant. 

# we expect a decrease of 3.66 in dependent variable, api00 score, for every one unit increase
# in percent free meals, holding all other variables in the model constant

anova(m2)

sum(anova(m2)$Sum) # sum of RSS and SSreg # Lee TODO syntax
(400 -1) * var(d$api00) # total Sum of Squares

# anova table shows sum of squares that will be explain by adding 
# each variable at a time to the model. Or it is the reduced amount in sum of square
# residuals by an additional variable

# var enroll reduces total error by 817326. By adding var meals we reduce 
# additional 5820066 from the residuals, and by adding variable full we reduce 
# the error by 70313. Finally we have 1365967 left as unexplained error. Total
# error is all of those sum of squares added together. To get the total sum of square 
# of variable api00 we can multiply its’ variance by (n−1).

# standardized regression
m2.sd <- lm(scale(api00) ~ scale(enroll) + scale(meals) + scale(full), data=d)
print(m2.sd) # coefficients
summary(m2.sd) # standardized coefficients

# scatter plot using car module
scatterplotMatrix(~ api00 + enroll + meals + full, data=d)

# influence measures -individual obs that exert undue influence on coefficients

# calculate vector of studentized residuals using rstandard()
res.std <- rstandard(m2)
res.std

# plot studentized residuals
plot(er.std, ylab='Standardized Residual', ylim = c(-3.5, 3.5))

# add horizontal lines 3 and -3 to identify cutoff/threshhold of outliers/extreme values
abline(h = c(-3,0,3), lty=2)

# determine rows that are outliers
index <- which(er.std > 3 | er.std < -3 )
index # outliers

# label
text(index-20, er.std[index], labels = d$snum[index])
d$snum[index] # school name outliers

#Bonferroni p-values for testing outliers
outlierTest(m2)

# vector containing diagonal of hat matrix
h <- influence(m2)$hat
h

# half normal plot of leverage using faraway
halfnorm(h, ylab= "leverage")

# cutoff value for cook's distance
cutoff <- 4/((nrow(d)-length(m2$coefficients)-2))

# plot cook's distance
plot(m2, which=4, cook.levels = cutoff)

# regression influence plot - bubble plot of studentized residual vs hat vals
# areas of circles representing obs proportional to value Cook's distance
influencePlot(m2, main = "Influence Plot", sub = "Circle size is proportional
               to Cook's distance")

# inspect influence with diagnostic plots
infIndexPlot(m2)

# Check for homoskedasticity
# check for homoskedasticity using scatter plot of residuals vs fitted vals
plot(m2$residuals ~ m2$fitted.values)
abline(h=0, lty = 2) # add horizontal line at 0

# calculate robust standard errors with lmtest and sandwich
coeftest(m2, vcov = vcovHC(m2, type="HC1"))

# Check for model specification
# construct added variable (partial regression) plots
avPlots(m2)

# Check for linearity
# plot residual vs fitted val and all predictors using car package
residualPlots(m2)

# test for curvature

# Check for independence
# scatter plot of residuals vs school id
plot(m2$resid ~ d$snum)
# plot(m2$resid[-1] ~ m2$resid[-400])

# Check for normality
# normal quantile to quantile plot
qqnorm(m2$resid)
qqline(m2$resid)

# Check for collinearity
# use function vif from package car 
# calculate variance inflation factor
car::vif(m2)

class(d$api00)
class(d$yr_rnd)
class(d$mealcat)

summary(d$api00)
summary(d$yr_rnd)
summary(d$mealcat)

# frequency table
table(d$api00)
table(d$yr_rnd)
table(d$mealcat)

# encode vector as a factor
d$yr_rnd_F <- factor(d$yr_rnd)
d$mealcat_F <- factor(d$mealcat)

# factor returns object of class "factor"
class(d$yr_rnd_F)
class(d$mealcat_F)

# fit simpple regression model of api00 on yr_rnd_F
m3 <- lm(api00 ~ yr_rnd_F, data = d)
summary(m3)

# scatter plot of api00 vs yr_rnd 
plot(api00 ~yr_rnd, data=d)
# add regression line
abline(m3)

mean(api00[yr_rnd_F == "0"])

aggregate(api00 ~ yr_rnd_F, FUN=mean, data=d)

mean(d$api00[d$yr_rnd_F == "1"])

t.test(api00 ~ yr_rnd_F, data=d, var.equal = TRUE)

anova(m3)

# square of t val from t test == anova F value
10.7815^2

# fit model 4 simple regression, api00 on mealcat with 4 levels
m4 <- lm(api00 ~ mealcat_F, data=d)
summary(m4)

# 'aggregate' subsets and summarizes
# aggregate mean of api00 for each category in mealcat_F
aggregate(api00 ~ mealcat_F, FUN=mean, data=d)

# relevel factor mealcat_F and make group "3" as the reference group
d$mealcat_F <- relevel(d$mealcat_F, ref='3')

# fit model 5
m5 <- lm(api00 ~ mealcat_F, data=d)
summary(m5)


