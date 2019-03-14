library(tidyverse)
library(stargazer)
library(lattice)
library(mice)
library(gdata)
library(Rcpp)
library(tibble)


df <- read.csv(file="C:/Users/james/Desktop/5253/wages.csv")
#drop missing value in hgc and tenured
df1 <- df %>% drop_na(hgc,tenure)

#use only data without na
df2 <- df[complete.cases(df1), ]
#table
stargazer(df2)
summary(df1)
#model
df2 %>% mutate(tenure.sqUared = tenure^2)
est <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married , data=df2)


df1.1 <- df1
#replace NA with mean
df1.mean <- df1.1$logwage[which(is.na(df1.1$logwage))] <- mean(df1.1$logwage, na.rm = TRUE)
#regression with mean in logwage
est.mean <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=df1.1)
stargazer(df1.1)

test <- function(t)
    {x <- dim(length(t))
   x[which(!is.na(t))] = 1
   x[which(is.na(t))] = 0
   return(x)}
df1.2 <- df1
df1.2$test.logwage <- test(df1.2$logwage)

df2.1 <- df2
df2.1$logwage[is.na(df2.1$logwage)] <- predict(est, newdata=df2[is.na(df2.1$logwage),])


est1 <-lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=df2.1)

stargazer(est, est.mean, est1)


#use mice 
df1_mice <- df1
df1_mice <- mice(df1, logwage = 12345)
fit <- with(df1_mice, lm(logwage ~ hgc + college + tenure+ tenure^2 + age + married))

round(summary(pool(fit)),2)

