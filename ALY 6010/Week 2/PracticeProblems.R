#Sections 7-1
##Question 7a find z for the 99% confidence interval
##using table E
2.58

##Question 7d find z for the 90% confidence interval
##using table E
1.65

##Question 18 A random sample of 50 four-year olds attending day care centers 
##provided a yearly tuition  average of $3987 and the population standard 
##deviation  of $630. Find the 90% confidence interval of the true  mean. 
##If a day care center were starting up and wanted  to keep tuition low, 
##what would be a reasonable amount  to charge? 
n <- 50
xbar <- 3987
sigma <- 630
conf_level <- 0.9
#calculations
alpha <- 1-conf_level
halpha <- alpha/2
z <- abs(qnorm(halpha))
upper <- xbar + z* sigma/sqrt(n)
lower <- xbar - z*sigma/sqrt(n)

##Question 24 A pizza shop owner wishes to find the  95% confidence interval of 
##the true mean cost of a large  cheese pizza. How large should the sample be 
##if she  wishes to be accurate to within $0.15? A previous study  showed that 
##the standard deviation of the price was  $0.26. 
conf_level <- 0.95
E <- 0.15
sigma <- 0.26
#Calculation
alpha <- 1-conf_level
halpha <- alpha/2
z <- abs(qnorm(halpha))
n <- round(((z*sigma)/E)^2,0)




#Section 7-2
##Question 6 The prices (in dollars) for a  particular model of digital camera 
##with 18.0 megapixels  and a f/3.5−5.6 zoom lens are shown here for 10  
##randomly selected online retailers. Estimate the  true mean price for this 
##particular model with 95%  confidence.  
##999 1499 1997 398 591 498 798 849 449 348 
camera <- c(999, 1499, 1997, 398, 591, 498, 798, 849, 449, 348)
s <- sd(camera)
xbar <- mean(camera)
n <- 10
conf_level <- 0.95
alpha <- 1-0.95
halpha <- alpha/2
df <- n-1
t <- 2.262
upper <- xbar+t*(s/sqrt(n))
lower <- xbar-t*(s/sqrt(n))

##Question 10 The number of students  who belong to the dance company at each 
##of several  randomly selected small universities is shown here.  Estimate the 
##true population mean size of a university  dance company with 99% confidence. 
##21 25 32 22 28 30 29 30  47 26 35 26 35 26 28 28  32 27 40  11. 
student <- c(21, 25, 32, 22, 28, 30, 29, 30,47, 26, 35, 26, 35, 26, 28, 28,32, 27, 40,  11)
xbar <- mean(student)
s <- sd(student)
n <- 20
conf_level <- 0.99
alpha <- 1-conf_level
halpha <- alpha/2
df <- n-1
t <- 2.861
upper <- xbar+t*(s/sqrt(n))
lower <- xbar-t*(s/sqrt(n))

##Question 12 A meteorologist who sampled  13 randomly selected thunderstorms 
##found that the  average speed at which they traveled across a certain  state 
##was 15.0 miles per hour. The standard deviation  of the sample was 1.7 miles 
##per hour. Find the 99%  confidence interval of the mean. If a meteorologist  
##wanted to use the highest speed to predict the times  it would take storms to
##travel across the state in  order to issue warnings, what figure would she 
##likely  use? 
n <- 13
xbar <- 15
s <- 1.7
conf_level <- 0.99
alpha <- 1-0.99
df <- n-1
t <- 3.055
upper <- xbar+t*(s/sqrt(n))
lower <- xbar-t*(s/sqrt(n))



#Section 7-3
##Question 11 A survey of 50 students in grades 4 through  12 found 68% have 
##classroom Wi-Fi access. Find the 99%  confidence interval of the
##population proportion. 
n <- 50
phat <- 0.68
qhat <- 1-phat
conf_level <- 0.99
alpha <- 1-conf_level
halpha <- alpha/2
z <- abs(qnorm(halpha))
check1 <- n*phat
check2 <- n*qhat
upper <- phat+z*sqrt(phat*qhat/n)
lower <- phat-z*sqrt(phat*qhat/n)

##Question 17 It is believed that 25%  of U.S. homes have a direct satellite 
##television receiver. How large a sample is necessary to estimate  the true 
##population of homes that do with 95% confidence and within 3 percentage 
##points? How large  a sample is necessary if nothing is known about the  
##proportion? 
phat <- 0.25
conf_level <- 0.95
E <- 0.03
qhat <- 1-phat
alpha <- 1-conf_level
halpha <- alpha/2
z <- abs(qnorm(halpha))
n <- phat*qhat*(z/E)^2
phat2 <- 0.5
qhat2 <- 0.5
n2 <- phat2*qhat2*(z/E)^2

##Question 18 Obesity is defined as a body mass index (BMI)  of 30 kg/m2 or 
##more. A 95% confidence interval for the  percentage of U.S. adults aged 20 
##years and over who  were obese was found to be 22.4 to 23.5%. What was  the 
##sample size? 
conf_level <- 0.95
alpha <- 1-0.95
halpha <- alpha/2
phat <- 0.5
qhat <- 0.5
z <- abs(qnorm(halpha))
E <- (0.235-0.224)/2
n <- phat*qhat*(z/E)^2



#Section 7-4
##Question 6 A study of generation related carbon monoxide deaths showed that a  
##random sample of 6 recent years had a standard  deviation of 4.1 deaths per 
##year. Find the 99% confidence interval of the variance and standard deviation.
##Assume the variable is normally distributed. 
n <- 6
s <- 4.1
conf_level <- 0.99
alpha <- 1-conf_level
halpha <- alpha/2
df <- n-1
right <- qchisq(halpha, df, lower.tail = TRUE)
left <- qchisq(halpha, df, lower.tail = FALSE)
upper_var <- (n-1)*s^2/right
lower_var <- (n-1)*s^2/left
lower_sd <- sqrt(lower_var)
upper_sd <- sqrt(upper_var)

##Question 8 Find the 90% confidence  interval for the variance and standard 
##deviation of the  ages of seniors at Oak Park College if a random sample  of 
##24 students has a standard deviation of 2.3 years.  Assume the variable is 
##normally distributed.
conf_level <- 0.9
alpha <- 1-conf_level
halpha <- alpha/2
n <- 24
df <- n-1
s <- 2.3
right <- qchisq(halpha, df, lower.tail = TRUE)
left <- qchisq(halpha, df, lower.tail = FALSE)
upper_var <- (n-1)*s^2/right
lower_var <- (n-1)*s^2/left
lower_sd <- sqrt(lower_var)
upper_sd <- sqrt(upper_var)

##Question 11 A medical researcher surveyed  11 hospitals and found that the 
##standard deviation for  the cost for removing a person’s gall bladder was $53.
##Assume the variable is normally distributed. Based on  this, find the 99% 
##confidence interval of the population  variance and standard deviation. 
n <- 11
s <- 53
conf_level <- 0.99
df <- n-1
alpha <- 1-conf_level
halpha <- alpha/2
right <- qchisq(halpha, df, lower.tail = TRUE)
left <- qchisq(halpha, df, lower.tail = FALSE)
upper_var <- (n-1)*s^2/right
lower_var <- (n-1)*s^2/left
lower_sd <- sqrt(lower_var)
upper_sd <- sqrt(upper_var)



#####Quiz
weights <- c(22.08,22.25,21.95,22.39,22.08,22.11,21.89,21.60,22.34,22.03,22.06,22.32)
n <- 12
conf_level <- 0.9
xbar <- mean(weights)
s <- sd(weights)
alpha <- 1-conf_level
halpha <- alpha/2
t <- 1.796
E <- t*(s/sqrt(n))
lower <- xbar-E
upper <- xbar+E


n <- 300
phat <- 20/n
qhat <- 1-phat
E <- 0.025
conf_level <- 0.99
alpha <- 1-conf_level
halpha <- alpha/2
z <- 2.58
n2 <- phat*qhat*(z/E)^2
