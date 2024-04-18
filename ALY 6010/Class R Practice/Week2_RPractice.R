library(ggplot2)
age <- c(1:10)
height <- c(60:69)
df <- data.frame(age, height)
ggplot(df)+
  geom_point(mapping=aes(age, height))


##7.1.19 Find the confidence intervals
mean <- 61.2
n <- 84
sd <- 7.9
alpha <- 1-0.95
z <- abs(qnorm(alpha/2))
UL <- mean + z * sd/sqrt(n)
LL <- mean - z * sd/sqrt(n)

rm(list=ls())

##7.1.25 Find the sample size
var <- 28
error <- 3
z <- 2.58
samp <- (z * (sqrt(28)/error))^2

##7.2.11 
rm(list=ls())
n <- 8
xbar <- 12300
s <- 22
conf <- 0.95

alpha <- 1-conf
half_alf <- alpha/2
df <- n-1
t <- abs(qt(half_alf, df))
estimator <- t*s/sqrt(n)
upper <- xbar + estimator
lower <- xbar - estimator




##7.3.12
p <- 0.204
n <- 400
x <- 95
confidence <- 0.98

q <- 1-p
phat<- x/n
qhat <- 1-phat
#ensure np and nq >=5 to use this equation
np <- n*phat
nq <- n*qhat

alpha <- 1-confidence
half <- alpha/2
z <- abs(qnorm(half))
estimator <- z*sqrt(phat*qhat/n)
lower <- phat-estimator
upper <- phat+estimator


