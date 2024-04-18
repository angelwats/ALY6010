# Lesson 5-1 Example
x <- c(1,2,3,4,5,6)
y <- c(1.8,3.2,4.1,6.5,5.8,7.2)
cov <- cov(x,y)
var <- var(x)
slope <- cov/var 

# Section 10-1: 8, 10, 11, 16, 19
# 8 What is the name of the correlation coefficient used in  this section? 
# correlation coefficient is known as the PPMC Pearson product moment correlation coefficient

# 10 When two variables are correlated, can the researcher  be sure that one 
# variable causes the other? Why or  why not? 
# You cannot be 100% sure of the causal mechanisms for two variable interactions.

# 11 The number of murders and robberies per  100,000 population for a random 
# selection of states is  shown. Is there a linear relationship between the 
# variables? 

murders <- c(2.4,2.7,5.6,2.6,2.1,3.3,6.6,5.7)
robberies <- c(25.3,14.3,151.6,91.1,80,49,173,95.8)

Null <- "rho = 0"
Alt <- "rho neq 0"

plot(murders, robberies)

xy <- murders*robberies
x2 <- murders^2
y2 <- robberies^2
data <- data.frame(murders, robberies, xy, x2,y2)

n <- nrow(data)
r <- ((n*sum(data$xy))-sum(data$murders)*sum(data$robberies))/sqrt(
  ((n*sum(data$x2))-sum(data$murders)^2)*((n*sum(data$y2))-sum(data$robberies)^2)
)

alpha <- 0.05
cv <- 0.707
t <- r*sqrt((n-2)/1-r^2)

##Positive linear relationship that is significant (t>cv)

# 16 An economics student wishes to see if there is a relationship between the  
# amount of state debt per capita and the amount of tax  per capita at the 
# state level. Based on the following data,  can she or he conclude that per 
# capita state debt and per  capita state taxes are related? Both amounts are 
# in dollars and represent five randomly selected states. 
rm(list=ls())
dev.off()
x <- c(1924,907,1445,1608,661)
y <- c(1685,1838,1734,1842,1317)

plot(x,y)

xy <- x*y
x2 <- x^2
y2 <- y^2

data <- data.frame(x,y,xy,x2,y2)
n <- nrow(data)
r <- ((n*sum(data$xy))-sum(data$x)*sum(data$y))/sqrt(
  ((n*sum(data$x2))-sum(data$x)^2)*((n*sum(data$y2))-sum(data$y)^2)
)

Null <- "rho = 0"
Alt <- "rho neq 0"

alpha <- 0.05
cv <- 0.878
t <- r*sqrt((n-2)/1-r^2)

## There is a weak positive relationship that is possibly due to chance because there is no statistical difference from 0

# 19 Is the average age  of armed services personnel related to the average 
# amount  of length of service in months? The data are shown. 
rm(list=ls())
dev.off()
x <- c(24.9,25.6,26.1,27.3,27)
y <- c(66.5,70,74.8,89.6,82.6)

plot(x,y)

xy <- x*y
x2 <- x^2
y2 <- y^2

data <- data.frame(x,y,xy,x2,y2)
n <- nrow(data)
r <- ((n*sum(data$xy))-sum(data$x)*sum(data$y))/sqrt(
  ((n*sum(data$x2))-sum(data$x)^2)*((n*sum(data$y2))-sum(data$y)^2)
)

Null <- "rho = 0"
Alt <- "rho neq 0"

alpha <- 0.05
cv <- 0.878
t <- r*sqrt((n-2)/1-r^2)

## This is a strong positive relationship that is statistically significant


# Section 10-2: 3, 8, 11, 16, 19
# 3 What is the general form for the regression line used in  statistics? 
# y' = a + bx where a is the intercept and b is the slope

# 8 As the value of the correlation coefficient increases  from 0 to 1, or 
# decreases from 0 to ???1, how do the  points of the scatter plot fit the 
# regression line? 
# they would be clustered closer to the line

# 11 The number of murders and robberies per 100,000  population for a random 
# selection of states are shown. Find y' when x = 4.5 murders. 
rm(list=ls())
dev.off()
x <- c(2.4,2.7,5.6,2.6,2.1,3.3,6.6,5.7)
y <- c(25.3,14.3,151.6,91.1,80,49,173,95.8)
xy <- x*y
x2 <- x^2
y2 <- y^2

data <- data.frame(x,y,xy,x2,y2)
n <- nrow(data)
r <- ((n*sum(data$xy))-sum(data$x)*sum(data$y))/sqrt(
  ((n*sum(data$x2))-sum(data$x)^2)*((n*sum(data$y2))-sum(data$y)^2)
)
a <- round(((sum(data$y)*sum(data$x2))-(sum(data$x)*sum(xy)))/
  ((n*sum(data$x2))-(sum(data$x))^2), 3)
b <- round(((n*sum(data$xy))-(sum(data$x)*sum(data$y)))/
  ((n*sum(data$x2))-(sum(data$x))^2),3)
yprime <- a+b*4.5

# 16 Data for per capita  state debt and per capita state tax are as follows. 
# Find y' when x = $1500 in per capita debt. 
# Weak positive and not significant means there is no regression formulat

# 19 The data show  the average ages and lengths of service in months. If a 
# service person is 26.8 years old, predict the time that  the person will 
# serve. 
rm(list=ls())
dev.off()
x <- c(24.9,25.6,26.1,27.3,27)
y <- c(66.5,70,74.8,89.6,82.6)
xy <- x*y
x2 <- x^2
y2 <- y^2

data <- data.frame(x,y,xy,x2,y2)
n <- nrow(data)
r <- ((n*sum(data$xy))-sum(data$x)*sum(data$y))/sqrt(
  ((n*sum(data$x2))-sum(data$x)^2)*((n*sum(data$y2))-sum(data$y)^2)
)
a <- ((sum(data$y)*sum(data$x2))-(sum(data$x)*sum(xy)))/
  ((n*sum(data$x2))-(sum(data$x))^2)
b <- ((n*sum(data$xy))-(sum(data$x)*sum(data$y)))/
  ((n*sum(data$x2))-(sum(data$x))^2)
yprime <- a+b*26.8


# Quiz
# The average gasoline price per gallon in a city and the cost of a barrel of 
# oil are shown for a selection of weeks. Based on this data, decide if the 
# correlation between the oil price and the gasoline price is significant at 
# ?? = 0.05
rm(list=ls())
dev.off()
x <- c(58.46,52.91,46.99,48.47,53.65,54.02)
y <- c(1.98,2.01,2.07,2.02,2.09,2.07)
plot(x,y)

xy <- x*y
x2 <- x^2
y2 <- y^2

data <- data.frame(x,y,xy,x2,y2)
n <- nrow(data)
r <- ((n*sum(data$xy))-sum(data$x)*sum(data$y))/sqrt(
  ((n*sum(data$x2))-sum(data$x)^2)*((n*sum(data$y2))-sum(data$y)^2)
)
cv <- 0.811

t <- r*sqrt((n-2)/1-r^2)
