#Section 9-1
#9 The average length of "short  hospital stays" for men is slightly longer 
#than that for  women, 5.2 days versus 4.5 days. A random sample of  recent 
#hospital stays for both men and women revealed  the following. At ?? = 0.01, is 
#there sufficient evidence  to conclude that the average hospital stay for men 
#is  longer than the average hospital stay for women?  Men Women  
#Sample size 32 30  Sample mean 5.5 days 4.2 days  Population standard 
#deviation 1.2 days 1.5 days 
#Given
n1 <- 32
n2 <- 30
xbar1 <- 5.5
xbar2 <- 4.2
sigma1 <- 1.2
sigma2 <- 1.5

#Step 1: state the hypotheses
null <- "Men average day is equal to women average day"
alt <- "Men average day is greater than women average day"
claim <- "Men have longer hospital stays than women"

#Step 2: find critical value
alpha <- 0.01
cv <- qnorm(alpha, lower.tail = FALSE)
library(nhstplot)
plotztest(cv, tails="one")

#step 3: find test statistic
z <- (xbar1-xbar2)/sqrt(sigma1^2/n1+sigma2^2/n2)

#step 4: make decision
decision <- if(z<cv){
  "Fail to reject null"
} else {"Reject Null"}
decision

#step 5: summmarize the results
sumResults <- if(decision=="Fail to reject null"){
  "There is not enough evidence to support the claim"
} else{"There is enough evidence to support the claim"}
sumResults

#13 A real estate agent compares the selling  prices of randomly selected homes 
#in two municipalities in southwestern Pennsylvania to see if there is a  
#difference. The results of the study are shown. Is there enough evidence to 
#reject the claim that the average  cost of a home in both locations is the 
#same? Use  ?? = 0.01. 
#given
n1 <- 35
n2 <- 40
xbar1 <- 93430
xbar2 <- 98043
sigma1 <- 5602
sigma2 <- 4731
alpha <- 0.01

null <- "Cost of homes is the same (claim)"
alt <- "Cost of homes is not the same"

halpha <- alpha/2
cv <- qnorm(p=1-halpha, lower.tail = TRUE)

#23

#Section 9-2
#4
#8
#10

#Section 9-3
#5
#10
#12

#Section 9-4
#14
#20
#24

#Section 9-5
#11
#17
#21
