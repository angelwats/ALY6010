library(ggplot2)
library(tidyverse)
library(dplyr)
library(corrplot)
library(Hmisc)
library(psych)
library(skimr)

#pre work for this analysis
rm(list=ls())
dev.off()

#loading the dataset and understanding the metadata
data <- datasets::iris
?datasets::iris
#Exploratory data analysis
summary(data)
headtail(data)
#Data visualization
hist_sl <- ggplot(data)+
  geom_histogram(mapping=aes(Sepal.Length), binwidth = 0.25, fill="dark green", color="black")+
  theme_classic()+
  labs(x="Sepal Length", y="Count", title = "Histogram of Sepal Length")
hist_sl
hist_sw <- ggplot(data)+
  geom_histogram(mapping=aes(Sepal.Width), binwidth = 0.15, fill="dark blue", color="black")+
  theme_classic()+
  labs(x="Sepal Width", y="Count", title = "Histogram of Sepal Width")
hist_sw
hist_pl <- ggplot(data)+
  geom_histogram(mapping=aes(Petal.Length), binwidth = 0.5, fill="dark red", color="black")+
  theme_classic()+
  labs(x="Petal Length", y="Count", title = "Histogram of Petal Length")
hist_pl
hist_pw <- ggplot(data)+
  geom_histogram(mapping=aes(Petal.Width), binwidth = 0.25, fill="purple", color="black")+
  theme_classic()+
  labs(x="Petal Width", y="Count", title = "Histogram of Petal Width")
hist_pw

#Correlation analysis
#visualizing the data to see if there are any relationships
ggplot(data)+
  geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width, color=Species))
x <- data$Sepal.Length
y <- data$Sepal.Width
cor_table_sep <- data.frame(x, y, data$Species)
cor_table_sep <- cor_table_sep %>% mutate(xy=x*y, x2=x^2, y2=y^2)
#cor_table_sep %>% group_by(data.Species) %>% summarize(cor=cor(x,y))
cor(x,y) #weak negative relationship but there seems to be species specific relationships

#alternative correlation calculation
ggplot()+
  geom_point(data, mapping=aes(x=Petal.Length, y=Petal.Width, color=Species))
x <- data$Petal.Length
y <- data$Petal.Width
cor_table <- data.frame(x, y, data$Species)
cor_table %>% mutate(xy=x*y, x2=x^2, y2=y^2)
#cor_table %>% group_by(data.Species) %>% summarize(cor=cor(x,y))
cor_table
cor(x,y)

#creating a table of the individual correlations
data_num <- data %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
correlations <- cor(data_num)
correlations

corrplot(correlations, method="shade")

#Regression analysis
#going to calculate for the petal length vs petal width relationship

#hypothesis testing at alpha 0.05
alpha <- 0.05
null <- "rho = 0"
alt <- "rho neq 0"
#determine significance
cor.test(x=x, y=y, 
         alternative="two.sided",
         method="pearson", 
         conf.level =0.95)
#Regression analysis
##Formula for the regression line
reg_equation <- lm(data = data, Petal.Width~Petal.Length)
reg_plot <- ggplot(data, mapping=aes(x=Petal.Length, y=Petal.Width))+
  geom_point(mapping=aes(color=Species))+
  geom_smooth(method=lm)+
  theme_classic()+
  labs(x="Petal Length", y="Petal Width", title = "Scatter Plot")
reg_plot

#Predict the Petal width given a specific Petal Length
petLength_future <- data.frame("Petal.Length"=c(1,3,4,5))
petLength_future
petWidth_pred <- predict(newdata=petLength_future, reg_equation)
petWidth_pred

reg_prediction_df <- data.frame(petLength_future, "Petal.Width"=petWidth_pred)
reg_prediction_df
regression_finalplot <- ggplot()+
  geom_point(data, mapping=aes(x=Petal.Length, y=Petal.Width, color=Species))+
  geom_smooth(data, mapping=aes(x=Petal.Length, y=Petal.Width), method=lm)+
  geom_point(reg_prediction_df, mapping=aes(x=Petal.Length, y=Petal.Width), color="black")+
  theme_classic()+
  labs(x="Petal Length", y="Petal Width", title = "Scatter Plot")
regression_finalplot
