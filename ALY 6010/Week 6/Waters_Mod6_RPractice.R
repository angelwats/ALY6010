library(tidyverse)
library(skimr)
library(dplyr)
library(corrplot)
library(ggplot2)

rm(list=ls())
dev.off()

# Setting up data for easy manipulation
view(mtcars)
data <- mtcars

#creating a dummy variable (aka changing cyl to a factor)
data <- data %>% mutate("Cyl.Dummy"=ifelse(cyl==4, "4", ifelse(cyl==6, "6", "8")))
str(data)

#view the relationships plotted by strength
data_num <- data %>% select(mpg, disp, hp, drat, wt, qsec)
correlations <- cor(data_num)
corrplot(correlations, method = "circle", main="Figure 1: Initial Regression Analysis")

#########################
# Test For Significance #
#########################
# significance of the correlation was tested
ggplot(data=data, mapping = aes(x=wt, y=disp))+
  geom_point()+
  theme_classic()+
  labs(title = "Figure 2: Displacement by Weight", x="Weight (1000lbs)", y="Displacement (cu.in)")
cor.test(data$wt, data$disp, conf.level = 0.95)

###################
# Dummy Variables #
###################
#Plotting a relationship for disp by wt, analysis through dummy variables
plot(y=data$disp, x=data$wt, col=data$Cyl.Dummy, main="Displacement by Weight: A Dummy's Tale", sub = "Dummy Variable Analysis", xlab = "Weight (1000lbs)", ylab = "Displacement (cu.in)")
# legend("topleft", 
#        title="Cylinders", 
#        legend = levels(data$Cyl.Dummy), 
#        text.col = c(1:3))
# levels(data$Cyl.Dummy)
###Code commented out because my legend argument was giving NULL so turning column into a factor
cylVec <- data$Cyl.Dummy
cylVec <- factor(cylVec)
levels(cylVec)
legend("topleft", 
       title="Cylinders",
       legend = levels(cylVec),
       text.col = c(1:3))
#Calculating the regression
regression <- lm(disp~wt+Cyl.Dummy, data=data)
regression
#Calculating the dummy variables regressions
abline(a=regression$coefficients[1], b=regression$coefficients[2], col=1)
abline(a=regression$coefficients[1]+regression$coefficients[3], b=regression$coefficients[2], col=2)
abline(a=regression$coefficients[1]+regression$coefficients[4], b=regression$coefficients[2], col=3)




################
# Data Subsets #
################
# Plotting a relationship for disp by wt, analysis through subset method
ggplot(data, mapping=aes(wt, disp, col=Cyl.Dummy))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  labs(title = "Displacement by Weight: A subset story", x="Weight (1000lbs)", y="Displacement (cu.in)", subtitle = "Subset Analysis")
