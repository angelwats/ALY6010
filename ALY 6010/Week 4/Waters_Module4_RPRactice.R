library(pacman)
p_load(MASS)
library(tidyverse)
library(ggplot2)
library(nhstplot)


##Do male and female cats have the same body weight?
#Creating vectors of male and female cats
cats
female <- cats %>% filter(Sex=="F")
head(female)
female
summary(female)
male <- cats %>% filter(Sex=="M")
head(male)
male
summary(male)

#Identifying the null and alternative hypothesis and state the claim
Null <- "Average male body weight is equal to average female body weight"
Alt <- "Average male body weight is not equal to the average female body weight"
Claim <- "The average body weight of male cats is not equivalent to the average body weight of female cats."

#Obtaining the necessary sample statistics for the t test analysis
nm <- nrow(male)
nf <- nrow(female)
xbarm <- mean(male$Bwt)
xbarf <- mean(female$Bwt)
sm <- sd(male$Bwt)
sf <- sd(female$Bwt)
alpha <- 0.05
dfm <- nm-1
dff <- nf-1
df <- min(dfm, dff) #df to be used 

#Calculating the critical value
halpha <- alpha/2
cv <- qt(p=1-halpha, lower.tail = TRUE, df=df)
cv
#draw curve set to default as a two tailed t test
plotttest(cv, df=df)

##Manual calculation of the t statistic
t <- ((xbarm-xbarf)-(0))/sqrt(((sm^2)/nm)+((sf^2)/nf))
t
##using the R tool to calculate the t statistic
tsummary <- t.test(female$Bwt, male$Bwt, alternative="two.sided", var.equal = FALSE, mu=0, conf.level = 0.95)
tsummary

#Making the decision
decision <- if(abs(t)-abs(cv)>0){
  "Reject the Null Hypothesis"
} else {
  "Do not reject the Null Hypothesis"
}
decision
#Summarizing results
conclusion <- if(decision=="Reject the Null Hypothesis"){
  "There is enough evidence to support the claim:"
} else{
  "There is not enough evidence to support the claim:"
}
cat(conclusion, Claim)







##Meditation analysis
#The data
before <- c(4.6,7.8,9.1,5.6,6.9,8.5,5.3,7.1,3.2,4.4)
after <- c(6.6,7.7,9.0,6.2,7.8,8.3,5.9,6.5,5.8,4.9)

#State the null hypothesis, alternative hypothesis and claim
Null <- "uD = 0"
Alt <- "uD < 0"
Claim <- "Meditation increases the average sleep score"

#Critical values
n <- nrow(Meditation)
alpha <- 0.05
df <- n-1
cv <- qt(p=1-alpha, df=df, lower.tail = FALSE )

#calculating individual differences
Meditation <- data.frame(before, after)
Meditation <- Meditation %>% mutate(D = before-after, D2= D^2)
Meditation

#calculate t test statistics
dbar <- mean(Meditation$D)
sumd <- sum(Meditation$D)
sumd2 <- sum(Meditation$D2)
sD <- sqrt(((n*sumd2)-(sumd)^2)/(n*(n-1)))
t <- dbar/(sD/sqrt(n))

#Make decision
decision <- if(abs(t)-abs(cv)>0){
  "Reject the Null Hypothesis"
} else {
  "Do not reject the Null Hypothesis"
}
decision

#Summarize results
conclusion <- if(decision=="Reject the Null Hypothesis"){
  "There is enough evidence to support the claim:"
} else{
  "There is not enough evidence to support the claim:"
}
cat(conclusion, Claim)


