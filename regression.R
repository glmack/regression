# read data from csv
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

# regress api00 on enroll
m1 <- lm(api00 ~ enroll, data=d)

# coefficient
print(m1)

# summarize model
summary(m1)

# corr coefficient
r <- cor(d$api00, d$enroll)
print(r)

# r-squared
r^2

# list components of lm class
ls(m1)

# coefficients
m1$coefficients

# return matrix of confidence interval for coefficients
confint(m1)

# plot
plot(api00 ~ enroll, data=d)
text(d$enroll, d$api00+20, labels = d$snum, cex = 0.7)
abline(m1, col='blue')
