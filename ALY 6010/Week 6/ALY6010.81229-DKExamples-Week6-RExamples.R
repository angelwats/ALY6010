
###########################################
#   ALY6010 -- Professor Dan Koloski      #
#   Week 6 Examples for R                 #
#   Using R as a calculator               #
#   Date:   April 5, 2021                #
###########################################

##### Setup ##########
#print name
print("Dan Koloski")



######################Bluman Example from Section 10-3 page 580 #####################

# clear everything out
rm(list=ls())
dev.off()



x <- c(1,2,3,4,5)
y <- c(10,8,12,16,20)

# calculate r
round(cor(x,y), digits=3)

# find the linear model
lm(y~x)

# create a data frame and add the y' predictions

bluman10.3 <- data.frame(x,y)

library(dplyr)
bluman10.3 <- mutate(bluman10.3, yprime=predict(lm(y~x), newdata=data.frame(x)))

bluman10.3

# find the mean of the y values and add to the table

bluman10.3 <- mutate(bluman10.3, 
                     ybar = mean(y), 
                     totvar=(y-ybar)^2, 
                     explvar=(yprime-ybar)^2,
                     unexplvar=(y-yprime)^2
                     )
bluman10.3

cat("Total Var =","Explained var + ", "Unexplained var")
cat("Total Var", sum(bluman10.3$totvar), "=","Explained var",sum(bluman10.3$explvar), "+ ", "Unexplained var", sum(bluman10.3$unexplvar))

bluman10.3 <- mutate(bluman10.3, 
                     residual=(y-yprime)
)
bluman10.3

plot(bluman10.3$x,bluman10.3$residual, main="Bluman 10-3 Example", xlab="",ylab="", pch = 19, col="blue")
abline(h=0)

# Calculate the Coefficient of Determination r^2

library(scales) #for percent function
percent(sum(bluman10.3$explvar)/sum(bluman10.3$totvar), accuracy=0.1)

# double check it manually by squaring r from the original x, y

percent(cor(x,y)^2, accuracy=0.1)



################BLUMAN EXAMPLE 10-12 and 10-14 ###############
# Given certain copy machine data about age and cost, find
# standard error of the estimate and the prediction interval about
# the predicted values for 95% confidence
# plot them

# clear everything out
rm(list=ls())
dev.off()


x <- c(1,2,3,4,4,6)
y <- c(62,78,70,90,93,103)

# find the linear model
lm(y~x)

# create a data frame and add the y' predictions

bluman10.12 <- data.frame(x,y)

library(dplyr)
bluman10.12 <- mutate(bluman10.12, 
                      yprime=round(
                                    predict(lm(y~x), 
                                            newdata=data.frame(x), 
                                            ),
                              digits=2))
bluman10.12

# find the mean of the y values and add to the table

bluman10.12 <- mutate(bluman10.12, 
                     yminusyprime=y-yprime, 
                     unexplvar=(y-yprime)^2
)
bluman10.12

n=nrow(bluman10.12)

sest <- sqrt(
              sum(bluman10.12$unexplvar)/(n-2)
              )
sest

#Bluman 10-14, find 95% CI for the monthly maintenance costs
# of a machine that is 3 years old

CI <-  0.95 #given
alpha <- 1-CI
t <- abs(qt(alpha/2,df=n-2))
xfuture=3 # given

interval <- t * sest * sqrt(1+(1/n)+
                        ((n*(xfuture-mean(x))^2/(n*sum(x^2)-sum(x)^2))))

yprimefuture <- bluman10.12$yprime[x=3]

lowerCI <- yprimefuture - interval
upperCI <- yprimefuture + interval

cat(lowerCI,"<y<",upperCI)

# calculate the interval for each point on the table and add to the df
bluman10.12 <- mutate (bluman10.12, 
                       interval=t * sest * sqrt(1+(1/n)+
                            ((n*(x-mean(x))^2/(n*sum(x^2)-sum(x)^2))))
                         )
bluman10.12

plot(bluman10.12$x,bluman10.12$y,
     main="Bluman 10-12 and 10-14",
     xlab="Copier Age",
     ylab="Copier Monthly Maintenance",
     xlim=c(0,max(bluman10.12$x)*1.5),
     ylim=c(min(bluman10.12$y)/1.5,max(bluman10.12$y)*1.5)
     )
lines(bluman10.12$x, bluman10.12$yprime)
lines(bluman10.12$x, bluman10.12$yprime-bluman10.12$interval)
lines(bluman10.12$x, bluman10.12$yprime+bluman10.12$interval)

# or have r do all the work for you
model <- lm(bluman10.12$y~bluman10.12$x)
model
futurex = seq(min(bluman10.12$x),max(bluman10.12$x),by = 1)
conf_interval <- predict(model, newdata=data.frame(x=futurex), 
                         interval="confidence",
                         level = 0.95)
plot(bluman10.12$x,bluman10.12$y,
     main="Bluman 10-12 and 10-14",
     xlab="Copier Age",
     ylab="Copier Monthly Maintenance",
     xlim=c(0,max(bluman10.12$x)*1.5),
     ylim=c(min(bluman10.12$y)/1.5,max(bluman10.12$y)*1.5)
)
conf_interval
abline(model, col="lightblue")
lines(futurex, conf_interval[,2], col="blue", lty=2)
lines(futurex, conf_interval[,3], col="blue", lty=2)

# or do it in ggplot with just a few lines
# use se=TRUE in the geom_smooth function to plot the CI

library(ggplot2)

ggplot(data = bluman10.12, 
         aes(x = x, 
             y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Bluman 10-12 and 10-14") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Copier Age") + 
  ylab("Copier Monthly Maintenance")

dev.off()


################CODE FOR SECTION 2 OF WEEK 6 LECTURE###############

# clear everything out
rm(list=ls())
dev.off()


############# Petal and Sepal Lengths Example ####################

require("datasets")
data("iris")
str(iris)

# plot overall data set and plot the overall regression line
plot(iris$Petal.Length ~ iris$Sepal.Length, 
     main="Iris Sepal vs Petal Lengths",
     xlab="Sepal Length",
     ylab="Petal Length")
abline(lm(Petal.Length ~ Sepal.Length, data=iris), col="blue")

# this is not a good model given dataset variability
dev.off()


# plot again and include species differentiators
plot(iris$Petal.Length ~ iris$Sepal.Length, 
     main="Iris Sepal vs Petal Lengths",
     xlab="Sepal Length",
     ylab="Petal Length",
     col=iris$Species
)
legend("topleft", 
       title = "Species",
       legend = levels(iris$Species),
       text.col = c(1:3),
)

# calculate and plot the linear regression model for 
# the "Excluded Category" (or base category) sestosa and other variants

reglineD0 <- lm(Petal.Length  ~ Sepal.Length + Species, data=iris)
reglineD0
#plot the 3 lines
abline(a=reglineD0$coefficients[1],b=reglineD0$coefficients[2],col=1)
abline(a=reglineD0$coefficients[1]+reglineD0$coefficients[3],b=reglineD0$coefficients[2],col=2)
abline(a=reglineD0$coefficients[1]+reglineD0$coefficients[4],b=reglineD0$coefficients[2],col=3)

dev.off()



############# Petal and Sepal Lengths Example 2 - Subsets ####################

require("datasets")
data("iris")
str(iris)

# plot again and include species differentiators
plot(iris$Petal.Length ~ iris$Sepal.Length, 
     main="Iris Sepal vs Petal Lengths",
     xlab="Sepal Length",
     ylab="Petal Length",
     col=iris$Species
)
legend("topleft", 
       title = "Species",
       legend = levels(iris$Species),
       text.col = c(1:3),
)

# calculate and plot the linear regression model for each subset
# this not only has a different y-intercept but has a separate slope
# for each category

levels(iris$Species)

#plot the 3 lines
abline(lm(Petal.Length  ~ Sepal.Length, data=subset(iris,Species=="setosa")),col=1)
abline(lm(Petal.Length  ~ Sepal.Length, data=subset(iris,Species=="versicolor")),col=2)
abline(lm(Petal.Length  ~ Sepal.Length, data=subset(iris,Species=="virginica")),col=3)


# plotting subset linear regressions using ggplot
library(ggplot2)

myplot2 <- 
  ggplot(data = iris, 
         aes(x = Sepal.Length, 
             y = Petal.Length, 
             col = Species)) +
  geom_point()
myplot2

myplot2 +
 geom_smooth(method = "lm", se = FALSE)  

dev.off()




###########################Regression Diagnostics###############

#PRE WORK -- delete any previous variables and plots
rm(list=ls())
dev.off()
opar <- par()


################ REGRESSION DIAGNOSTICS WITH scatterplotMatrix()##################

library(car)
scatterplotMatrix(iris)



data("iris")

fit2 <- lm(Petal.Length  ~ Sepal.Length, data=iris)
opar <- par()
par(mfrow=c(2,2))
plot(fit2)

par(opar)
dev.off()


############### Kabakoff p 183 Regression Diagnostics example#############
require("datasets")
data("women")
str(women)
women

par(opar)

# plot overall data set and plot the overall regression line
plot(women$weight ~ women$height, 
     main="Kabakoff page 183",
     xlab="Height",
     ylab="Weight")
fit <- lm(weight ~ height, data=women)
abline(fit, col="blue")


opar <- par()
par(mfrow=c(2,2))
plot(fit)

par(opar)
dev.off()




###############MULTIVARIATE REGRESSION######################

fit3 <- lm(Petal.Length  ~ Sepal.Length + Petal.Width, data=iris)
coefficients(fit3)
coefficients(fit2)
cat("y'=",coefficients(fit3)[1],"+",coefficients(fit3)[2],"x(Sepal.Length)","+", coefficients(fit3)[3],"x(Petal.Width)"," is the Linear Regression Equation.",sep=" ")

library(car)


scatter3d(x=iris$Petal.Length,
          y=iris$Sepal.Length, 
          z=iris$Petal.Width,
          groups=iris$Species,
          surface=TRUE)



######### R example scatterplots with CATS data #######

#PRE WORK -- delete any previous variables and plots
rm(list=ls())
dev.off()

library(MASS)
data("cats")

plot(cats$Bwt~cats$Hwt, 
     col=cats$Sex, 
     main="Scatter Plot of Cats Data",
     xlab="Body Weight (g)",
     ylab="Heart Weight (g)")
legend("topright", 
       title = "Gender",
       legend = c("Female","Male"),
       pch=1, 
       text.col = c(1,2),
       col = c(1,2)
)
covcats<-round(cov(cats$Bwt,cats$Hwt),digits=3)
corcats<-round(cor(cats$Bwt,cats$Hwt),digits=3)
text(x=17,y=2.5, labels=paste("Covariance (all cats):",covcats,sep=" "))
text(x=17,y=2.25, labels=paste("Correlation (all cats):",corcats,sep=" "))

# calculate r using cor.test

# use cor.test to hypothesis test the significance of the correlation
cor.test(x=cats$Hwt,y=cats$Bwt,
         alternative="two.sided",
         method="pearson",
         conf.level=.95
)

# make a linear model
reglinecatsoverall <- lm(cats$Bwt~cats$Hwt)


# run regression diagnostics  -- this is week 6, but a preview here!
summary(reglinecatsoverall)
par(mfrow = c(2,2))
plot(reglinecatsoverall)

dev.off()
par(mfrow = c(1,1))

# re-plot the cats data
plot(cats$Bwt~cats$Hwt, 
     col=cats$Sex, 
     main="Scatter Plot of Cats Data",
     xlab="Body Weight (g)",
     ylab="Heart Weight (g)")
legend("topright", 
       title = "Gender",
       legend = c("Female","Male"),
       pch=1, 
       text.col = c(1,2),
       col = c(1,2)
)
covcats<-round(cov(cats$Bwt,cats$Hwt),digits=3)
corcats<-round(cor(cats$Bwt,cats$Hwt),digits=3)
text(x=17,y=2.5, labels=paste("Covariance (all cats):",covcats,sep=" "))
text(x=17,y=2.25, labels=paste("Correlation (all cats):",corcats,sep=" "))


#calculate a regression line for the overall data set

reglinecatsoverall <- lm(Bwt~Hwt, data=cats)
reglinecatsoverall
summary(reglinecatsoverall)
abline(reglinecatsoverall, col="blue")

# what if we want to break out by gender?
dev.off()

plot(cats$Bwt~cats$Hwt, 
     col=cats$Sex, 
     main="Scatter Plot of Cats Data",
     xlab="Body Weight (g)",
     ylab="Heart Weight (g)")
legend("topright", 
       title = "Gender",
       legend = c("Female","Male"),
       pch=1, 
       text.col = c(1,2),
       col = c(1,2)
)
covcats<-round(cov(cats$Bwt,cats$Hwt),digits=3)
corcats<-round(cor(cats$Bwt,cats$Hwt),digits=3)
text(x=17,y=2.5, labels=paste("Covariance (all cats):",covcats,sep=" "))
text(x=17,y=2.25, labels=paste("Correlation (all cats):",corcats,sep=" "))


# now re-plot lines with dummy variable for Gender

# use the + Sex attribute in the linear model to create dummy variables
# r will assign the first dummy variable to the first factor value (F)
# then it will add an additional attribute for the second factor value (M)
reglinecatsbygender <- lm(Bwt~Hwt + Sex, data=cats)
reglinecatsbygender
reglinecatsoverall
summary(reglinecatsoverall)
summary(reglinecatsbygender)

# plot the original line that corresponds with D0 (this is the regular model)
abline(reglinecatsbygender, col="black")

# plot the second categorical line that corresponds with D1 (add the new coefficient to D0)
abline(a=reglinecatsbygender$coefficients[1]+reglinecatsbygender$coefficients[3],
       b=reglinecatsbygender$coefficients[2], col="red")

dev.off()


