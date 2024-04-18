library(tidyverse)
library(ggplot2)
library(Hmisc)
library(skimr)
library(pacman)
library(psych)
library(pastecs)
library(doBy)


##Loading and understanding the data
lung <- read_csv("LungCapDataCSV.csv")
lung <- mutate(lung, Smoke = as.logical(ifelse(Smoke=="no", FALSE, TRUE)),
                   Caesarean = as.logical(ifelse(Caesarean=="no", FALSE, TRUE)),
                   Gender = ifelse(Gender=="male", "Male", "Female"))
names(lung)
str(lung)
view(lung)

##Summary statistics into table formats
summary(lung)
skim(lung)
describe(lung)
##Cannot find the describeBy() function
##describeBy(lung, group=Gender)
##Grouped the data separately and stored them for later use
descr_fem <- lung %>% filter(Gender=="Female") %>% describe()
descr_mal <- lung %>% filter(Gender=="Male") %>% describe()

#Creating a table for the summary statistics of the numerical values.
mu_lc <- mean(lung$LungCap)
sigma_lc <- sd(lung$LungCap)
max_lc <- max(lung$LungCap)
min_lc <- min(lung$LungCap)
med_lc <- median(lung$LungCap)

mu_a <- mean(lung$Age)
sigma_a <- sd(lung$Age)
max_a <- max(lung$Age)
min_a <- min(lung$Age)
med_a <- median(lung$Age)

mu_h <- mean(lung$Height)
sigma_h <- sd(lung$Height)
max_h <- max(lung$Height)
min_h <- min(lung$Height)
med_h <- median(lung$Height)

Column <- c("LungCap", "Age", "Height")
mu <- c(mu_lc, mu_a, mu_h)
sigma <- c(sigma_lc, sigma_a, sigma_h)
Maximum <- c(max_lc, max_a, max_h)
Minimum <- c(min_lc, min_a, min_h)
Median <- c(med_lc, med_a, med_h)
DescStat_Num <- data.frame(Column,mu, sigma, Maximum, Minimum, Median)
view(DescStat_Num)

##Categorical Descriptive Statistics
table(lung$Smoke)
table(lung$Gender)
table(lung$Caesarean)

DescStat_Gender <- data.frame(Gender=table(lung$Gender))
DescStat_Smoke <- data.frame(Smokers=table(lung$Smoke))
DescStat_Caesarean <- data.frame(Caesarean=table(lung$Caesarean))


##three line table
DescStat_Gender


##Data Visualization
#Spread of the numerical data
hist_lc <- ggplot(lung)+
  geom_histogram(mapping=aes(LungCap), fill="Navy", color="Black")+
  theme_classic()+
  labs(title= "Histogram of Lung Capacity", x= "Lung Capacity", y="Count")
hist_a <- ggplot(lung)+
  geom_histogram(mapping=aes(Age), fill="Maroon", color="Black", binwidth = 1)+
  theme_classic()+
  labs(title= "Histogram of Age", x= "Age (years)", y="Count")
hist_h <- ggplot(lung)+
  geom_histogram(mapping=aes(Height), fill="Dark green", color="Black")+
  theme_classic()+
  labs(title= "Histogram of Age", x= "Height (cm)", y="Count")

#Categorical variables
bar_gen <- ggplot(lung)+
  geom_bar(mapping=aes(Gender, fill=Gender))+
  theme_classic()+
  labs(title = "Gender Frequencies", y="Frequency")
bar_gen
bar_smo <- ggplot(lung)+
  geom_bar(mapping=aes(Smoke), fill="dark red")+
  theme_classic()+
  labs(title = "Smoker Frequencies", y="Frequency", x="Smoke?")
bar_smo
bar_cae <- ggplot(lung)+
  geom_bar(mapping=aes(Caesarean), fill="orange")+
  theme_classic()+
  labs(title = "Caesarean Frequencies", y="Frequency")
bar_cae

#Age vs Lung Capacity
scatter_avlc <- ggplot(lung, mapping=aes(Age, LungCap))+
  geom_point(mapping=aes(color=Caesarean))+
  geom_abline()+
  geom_smooth(method=lm, formula=y~x)+
  xlim(0,20)+
  ylim(0,20)+
  theme_classic()+
  labs(title= "Relationship between Age and Lung Capacity", y= "Lung Capacity",
       x= "Age (years)")
#Calculating the correlation coefficient values for the age vs lung capacity data
r_avlc <- cor(lung$Age, lung$LungCap)
r2_avlc <- r_avlc^2


#male vs female heights
boxplot_gvsh <- ggplot(lung)+
  geom_boxplot(mapping=aes(y=Gender, x=Height, color=Gender))+
  geom_boxplot(mapping=aes(x=Height))+
  theme_classic()+
  labs(title="Height vs Gender", x="Height (cm)")
boxplot_gvsh

#smokers vs lung capacity
violin_lcvS <- ggplot(lung)+
  geom_violin(mapping=aes(x=LungCap, y=Smoke))+
  theme_bw()+
  labs(title= "Smokers vs Lung Capacity", x="Lung Capacity", y="Did they smoke?")
violin_lcvS

#Caesarean vs Gender
jitter_cvsg <- ggplot(lung)+
  geom_jitter(mapping=aes(Caesarean, Gender))+
  theme_bw()+
  labs(title= "Caesarean by Gender", x="Born via Caesarean", y="Gender")
jitter_cvsg

##Lung capacity by gender
density_lcvg <- ggplot(lung)+
  geom_density(mapping=aes(LungCap, color=Gender))+
  theme_classic()+
  labs(title="Density of Lung Capacity by Gender", y="Density", x="Lung Capacity")
density_lcvg
