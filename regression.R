# read data from csv
library('car')
library('alr3')
library('faraway')

d <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv')

class(d)  # class of object d, returns data.frame
names(d)  # inspec col names
dim(d)    # inspect dimensions of df

class(d) # inspect class
names(d) # inspect col names
dim(d) # inspect df dimensions

str(d) # inspect vars in df
summary(d) # summarize statistics of vars in df

help(lm)

# simple regression - regress api00 on enroll
m1 <- lm(api00 ~ enroll, data=d)

# coefficient
print(m1)

summary(m1) # summarize model

# corr coefficient
r <- cor(d$api00, d$enroll)
print(r)

r^2 # r-squared

ls(m1) # list components of lm class

m1$coefficients # coefficients

confint(m1) # return matrix of confidence interval for coefficients

# plot
plot(api00 ~ enroll, data=d)
text(d$enroll, d$api00+20, labels = d$snum, cex = 0.7)
abline(m1, col='blue')

anova(m1) #anova table

# multiple regression - regress api00 on enroll, meals, full
m2 <- lm(api00 ~ enroll + meals + full, data = d)
print(m2) # coefficients
summary(m2) #summarize model
anova(m2)

sum(anova(m2)$Sum) # sum of RSS and SSreg # Lee TODO syntax
(400 -1) * var(d$api00) # total Sum of Squares

# standardized regression
m2.sd <- lm(scale(api00) ~ scale(enroll) + scale(meals) + scale(full), data=d)
print(m2.sd) # coefficients
summary(m2.sd) # standardized coefficients

# scatter plot using car module
scatterplotMatrix(~ api00 + enroll + meals + full, data=d)

# studentized residuals
er.std <- rstandard(m2)

# plot studentized residuals
plot(er.std, ylab='Standardized Residual', ylim = c(-3.5, 3.5))

# add horizontal lines for cut of value of outliers
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

# halfnormal plot of leverage using faraway package
halfnorm(h, ylab= "leverage")

