library(tidyverse)
library(ggplot2)
library(Hmisc)
library(skimr)
library(psych)
library(pastecs)
library(doBy)
library(ggpubr)

#Reading in the table for this script to analyze
lung <- read_csv("LungCapDataCSV.csv")
lung <- mutate(lung, Smoke = as.logical(ifelse(Smoke=="no", FALSE, TRUE)),
               Caesarean = as.logical(ifelse(Caesarean=="no", FALSE, TRUE)),
               Gender = ifelse(Gender=="male", "Male", "Female"))
names(lung)
str(lung)
view(lung)


#Creating a "random" sample of 100 entries from the lung cap data
set.seed(23)
sample1 <- sample(nrow(lung), 100)
sample1_set <- lung[sample1,]
view(sample1_set)
#viewing the spread of the continuous variables
sample1_height <- ggplot(sample1_set)+
  geom_histogram(mapping=aes(Height), fill="dark red", color="black")+
  theme_classic()+
  labs(title="Spread of Sample Heights", y="Count", x="Height (cm)")
sample1_height
sample1_age <- ggplot(sample1_set)+
  geom_histogram(mapping=aes(Age), fill="dark green", color="black", binwidth = 1)+
  theme_classic()+
  labs(title="Spread of Sample Age", y="Count", x="Age (years)")
sample1_age
sample1_lc <- ggplot(sample1_set)+
  geom_histogram(mapping=aes(LungCap), fill="dark blue", color="black")+
  theme_classic()+
  labs(title="Spread of Sample Lung Capacity", y="Count", x="Lung Capacity")
sample1_lc

#viewing categorical variables
sample1_caes <- ggplot(sample1_set)+
  geom_bar(mapping=aes(Caesarean), fill="blue")+
  theme_classic()+
  labs(title="Spread of Sample Caesarean Cases", y="Count")
sample1_caes
sample1_gender <- ggplot(sample1_set)+
  geom_bar(mapping=aes(Gender), fill="red")+
  theme_classic()+
  labs(title="Spread of Sample Gender", y="Count")
sample1_gender
sample1_smoke <- ggplot(sample1_set)+
  geom_bar(mapping=aes(Smoke), fill="green")+
  theme_classic()+
  labs(title="Spread of Sample Smoke Cases", y="Count")
sample1_smoke


#Sample statistics
#creating a function to capture the summary stats for the samples
sumstats_ttest <- function(x){
  xbar <- mean(x)
  sd <- sd(x)
  n <- length(x)
  df <- n-1
  return(c(xbar=xbar, std.dev=sd, n=n, df=df))
}



#Provide a one sample t test. State the null and alternative. Provide a 
#conclusion and discussion of the conclusion.
#Sample statistical values
LC_sampstats <- sumstats_ttest(sample1_set$LungCap)
LC_sampstats
alpha <- 0.05
mu <- mean(lung$LungCap)
mu

#step 1: State the null and alternative hypothesis, identify the claim
null <- "Population mean is equal to 7.86"
alt <- "Population mean is not equal to 7.86"
claim <- "There is a difference between the population mean and sample mean lung capacity data"

#step 2: Compute the critical value using the t.test()
CV <- qt(p=alpha, df=LC_sampstats[4], lower.tail=TRUE)
CV
CV2 <- qt(p=alpha, df=LC_sampstats[4], lower.tail=FALSE)
CV2

#step 3: Compute the test value
samplet.test <- t.test(sample1_set$LungCap, mu=mean(lung$LungCap), alternative = "two.sided")
samplet.test

#Step 4: Make decision around the null and alternative hypothesis
conclusion <- if(CV2>abs(samplet.test$statistic)){
  ("Do not reject the null hypothesis")
  }  else {"Reject Null hypothesis"
}
conclusion

#step 5: Summarize results
summary <- if(conclusion=="Reject Null hypothesis"){
  "There is not enough evidence to support the claim"
} else {
  "There is sufficient evidence to support the claim"
}
summary


#Conduct hypothesis testing for proportion using an appropriate variable from 
#the data sets

csec <- lung %>% filter(Caesarean==TRUE) %>% nrow()
total <- nrow(lung)
p <- csec/total
pt
q <- 1-p
np <- total*p
np
nq <- total*q
nq

#Step 1: state the null, alternative, and claim
null <- "Population proportion is equal to 22.6%"
alt <- "Population proportion is greater than 22.6%"
claim <- "The proportion of people born caesarean is greater than 22.6%"

#Step 2: Find the critical values.
CV <- qnorm(p, lower.tail = FALSE)
CV

#Step 3: Compute test values.
csec_samp <- sample1_set%>% filter(Caesarean==TRUE) %>% nrow()
n <- nrow(sample1_set)
phat <- csec_samp/n
z <- (phat - p)/ sqrt((p*q)/n)

#Step 4: Make decision
conclusion <- if(CV>z){
  ("Do not reject the null hypothesis")
}  else {"Reject Null hypothesis"
}
conclusion

#Step 5: Summarize results
summary <- if(conclusion=="Do not reject the null hypothesis"){
  "There is not enough evidence to support the claim"
} else {
  "There is sufficient evidence to support the claim"
}
summary

