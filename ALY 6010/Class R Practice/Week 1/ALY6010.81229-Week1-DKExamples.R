###########################################
#   ALY6010 -- Professor Dan Koloski      #
#   Week 1 In-Class Examples for R        #
#   Using Bluman Problems                #
#   Date:   April 24, 2022                 #
###########################################

#print name
print("Dan Koloski")

# clear everything
rm(list = ls())
dev.off()

# set working directory (replace with your directory)
setwd("~/teaching/ALY6010.81229/slides-and-in-class-materials")

# suppress scientific notation
options(scipen = 99)

################## R commands for standard normal dist ##########
# pnorm() and qnorm()

#### Graphing Normal Distribution in R (h/t Prof. Joy-EL Talbot)

# hat-tip to J.R. for their answer on Stackoverflow:
# https://stackoverflow.com/questions/27898931/how-to-shade-a-graph-using-curve-in-r

# 1) COOL IDEA:  define a color area function to fill area under the curve for us
colorArea <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens)
}

# 2) THEN draw a blank normal curve
curve(dnorm(x), from=-4, to=4, 
      main = "The Standard Normal Distibution", 
      ylab = "Probability Density",
      xlab = "Z")

# 3) fill in the drawing with different colors
colorArea(from=-4, to=qnorm(0.025), dnorm) # plot to left shading
colorArea(from=qnorm(0.975), to=4, dnorm, mean=0, sd=1, col=2, dens=20) # plot to right shading
colorArea(from=qnorm(0.4), to=qnorm(0.6), dnorm, col=5) # plot to middle shading





# Examples of using R to calculate Z and P values

# Problem 6.1.36
# Given Z, calculate p  
#  Find probability using the standard normal distribution
#>  P(-0.20 < z < 1.56)

# Option 1: just calculate
PvalHi <- pnorm(1.56, 0,1,TRUE)
PvalLo <- pnorm(-0.2, 0,1,TRUE)
pVal <- PvalHi-PvalLo
round(pVal,4)


# Option 2) draw a blank normal curve
curve(dnorm(x), from=-4, to=4, 
      main = "The Standard Normal Distibution", 
      ylab = "Probability Density",
      xlab = "Z")

# 3) fill in the drawing with different colors
colorArea(from=-0.2,
          to=1.56, 
          dnorm) # plot to middle shading
text(0,0.2,paste("p-val:",round(pVal,4)), col="green") # add the calcualted answer


# Problem 6.1.44 (Bluman, pg 323)
# Find the z-value for given area (aka p-value)
# P(X > z) = 0.0239

#approximate plot
curve(dnorm(x), from=-4, to=4, 
      main = "P(X > z) = 0.0239 Approximate Sketch", 
      ylab = "Probability Density",
      xlab = "Z")

colorArea(from=qnorm(0.99), # because the probability is very small
          to=4, # because it is a greater than
          dnorm)

#Or, given p, calculate z
z <- qnorm(p = 0.0239,
           mean = 0,
           sd = 1,
           lower.tail = FALSE) # because we are giving area to RIGHT of z
round(z,4)

# real plot
curve(dnorm(x), from=-4, to=4, 
      main = paste0("P(X > ", round(z, 2),") = 0.0239"), 
      ylab = "Probability Density",
      xlab = "Z")

colorArea(from=z, 
          to=4, # because it is a greater than problem
          dnorm)


#> Problem 6.2.2a (Bluman, pg 337)
#>  Teachers' Salaries
#>   The average annual salary for all US teachers is $47,750. Assume that the distribution is normal and the standard deviation is $5,680. Find the probability that a randomly selected teacher earns
#>   a. Between $35,000 and $45,000 a year
#
#Step 1: Draw normal distribution curve and shade desired area.

curve(dnorm(x), from=-4, to=4, 
  main = "Approximate Sketch", 
  ylab = "Probability Density",
  xlab = "Z")

colorArea(from=qnorm(0.1),
          to=qnorm(0.4),
          dnorm)

#Step 2: Find the z values

mu <- 47750 # mean
sigma <- 5680 # stdev
# P(35000 < X < 45000) = ???  OR
xLo <- 35000
xHi <- 45000
zLo <- (xLo - mu) / sigma
zHi <- (xHi - mu) / sigma

# alternatively - we could write a function!
getZScore <- function(x, mu, sigma){
  return((x - mu) / sigma)
}

print("Comparing the calculation to the function, we get the same results.")
print(paste("Original zLo: ", zLo, "  Function zLo: ", getZScore(xLo, mu, sigma)))

print("However, order matters - getZScore(xLo, sigma, mu) gives different result")
print(paste("Original zLo: ", zLo, "  Function zLo: ", getZScore(xLo, sigma, mu)))

print("Unless you specify the parameter to argument matches:")
print("getZScore(xbar = xLo, sigma = sigma, mu = mu)")
print(paste("Original zLo: ", zLo, "  Function zLo: ", getZScore(x = xLo, sigma = sigma, mu = mu)))

#Step 3: Find the p-value given the z-scores
# now like example 6.1.36 - given z get p
PvalLo <- pnorm(q = zLo)
PvalHi <- pnorm(q = zHi)
Pval <- PvalHi - PvalLo

# and plot
curve(dnorm(x), from=-4, to=4, 
      main = paste0("P($35k < X < $45k) = ", round(Pval, 4)),
      sub = "given mu=$47.75k & sigma = $5.68k", 
      ylab = "Probability Density",
      xlab = "Z")

colorArea(from=zLo, 
          to=zHi, # because it is a greater than problem
          dnorm)

# Problem 6.3.22
#>  Systolic Blood Pressure
#>   Assume that the mean systolic blood pressure of normal adults is 120 millimeters of mercury (mm Hg) and the standard deviation is 5.6. Assume the variable is normally distributed.
#>   a. If an individual is selected, find the probability that the individual's pressure will be between 120 and 121.8 mmHg.
#>   b. If a sample of 30 adults is randomly selected, find the probability that the sample mean [(xbar)] will be between 120 and 121.8 mmHg.


pnorm(121.8,120,5.6,TRUE)-pnorm(120,120,5.6,TRUE)
pValIndividual <- pnorm(121.8,120,5.6,TRUE)-pnorm(0)   # same thing since low value is mu

# For sample mean calc, we can  define a function
getSampleMeanZScore <- function(xbar, mu, sigma, n) {
  return((xbar - mu) / (sigma/sqrt(n)))
}

sLo <- getSampleMeanZScore(xbar=120, mu=120, sigma=5.6, n=30)
sHi <- getSampleMeanZScore(xbar=121.8, mu=120, sigma=5.6, n=30)
pValSample <- pnorm(sHi) - pnorm(sLo)

print(paste0("p-val for individual: ", round(pValIndividual, 4)))
print(paste0("p-val for sample: ", round(pValSample, 4)))




