##Loading the necessary packages for data analysis and visualization
library(ggplot2)
library(tidyverse)
library(ggQC)
library(dplyr)
library(corrplot)
library(Hmisc)
library(psych)
library(skimr)

setwd("C:/Users/12072/OneDrive/Desktop/ALY 6010/Final Project")
##Loading the DK raw data subset for further cleaning and visualizing the data.
rawData <- 
  read_csv("SWANBaselineData_ProfessorKSubset (1).csv")
str(rawData)
headTail(rawData)
view(rawData)

###############
# Milestone 1 #
###############

##Further subsets the data to meet the requirements for the final
# milestone1_subset <- subset(rawData, select = c(
#   SWANID,
#   AGE0,
#   ANEMIA0,
#   SMOKERE0,
#   PULSE0,
#   HEIGHT0,
#   WEIGHT0,
#   RACE)
#   )
# view(milestone1_subset)
# str(milestone1_subset)
##Commenting because additional data was added and explored in other milestones

# ##Adjusting the values in the character columns to cleaner versions
# milestone1_subset <- mutate(milestone1_subset, ANEMIA0=ifelse(ANEMIA0=="(1) No",
#                                                               "No", ifelse(ANEMIA0=="(2) Yes", "Yes", NA)),
#        SMOKERE0=ifelse(SMOKERE0=="(1) No", "No", ifelse(SMOKERE0=="(2) Yes", "Yes", NA)),
#        RACE=ifelse(RACE=="(1) Black/African American", "Black/African American",
#                    ifelse(RACE=="(2) Chinese/Chinese American", 
#                           "Chinese/Chinese American",
#                           ifelse(RACE=="(3) Japanese/Japanese American",
#                                  "Japanese/Japanese American",
#                                  ifelse(RACE=="(4) Caucasian/White Non-Hispanic", 
#                                         "Caucasian/ White Non-Hispanic", 
#                                         ifelse(RACE=="(5) Hispanic", "Hispanic", NA)
#                                         )))))

##collecting descriptive statistics
# summary(milestone1_subset)
# age_sd <- sd(milestone1_subset$AGE0, na.rm = TRUE)
# age_mean <- mean(milestone1_subset$AGE0, na.rm = TRUE)
# pulse_sd <- sd(milestone1_subset$PULSE0, na.rm = TRUE)
# pulse_mean <- mean(milestone1_subset$PULSE0, na.rm = TRUE)
# height_sd <- sd(milestone1_subset$HEIGHT0, na.rm = TRUE)
# height_mean <- mean(milestone1_subset$HEIGHT0, na.rm=TRUE)
# weight_sd <- sd(milestone1_subset$WEIGHT0, na.rm = TRUE)
# weight_mean <- mean(milestone1_subset$WEIGHT0, na.rm=TRUE)
# age_med <- median(milestone1_subset$AGE0, na.rm=TRUE)
# pulse_med <- median(milestone1_subset$PULSE0, na.rm=TRUE)
# height_med <- median(milestone1_subset$HEIGHT0, na.rm=TRUE)
# weight_med <- median(milestone1_subset$WEIGHT0, na.rm=TRUE)

##Turning the means and sd's into a table format as a data frame
# clmn_names <- c("Age", "Pulse", "Height", "Weight")
# sds <- c(age_sd, pulse_sd, height_sd, weight_sd)
# means <- c(age_mean, pulse_mean, height_mean, weight_mean)
# medians <- c(age_med, pulse_med, height_med, weight_med)
# desc_table <- data.frame("Continuous Data"=clmn_names, "Standard Deviation"=sds, "Mean"=means, "Medians"=medians)
# desc_table
# #write the table into a format that can be copied over into the report
# write.csv(desc_table, "C:/Users/12072/OneDrive/Desktop/ALY 6010/Final Project\\Milestone1_Descriptive.csv")
# 
# ##visualization of the data spread for continuous plots through histograms
# age_hist <- ggplot(milestone1_subset)+
#   geom_histogram(mapping=aes(AGE0), na.rm = TRUE, binwidth = 1, color="black")+
#   theme_classic()+
#   labs(title="Spread of Age", x="Age (years)", y="Count")
# age_hist
# ##put a box plot in to highlight the outliers in this chart
# pulse_hist <- ggplot(milestone1_subset,mapping=aes(PULSE0), na.rm = TRUE)+
#   geom_histogram(color="black")+
#   geom_boxplot()+
#   theme_classic()+
#   labs(title="Spread of Pulse", x="Pulse (beats/30 sec)", y="Count")
# pulse_hist
# height_hist <- ggplot(milestone1_subset)+
#   geom_histogram(mapping=aes(HEIGHT0), na.rm = TRUE, color="black")+
#   theme_classic()+
#   labs(title="Spread of Height", x="Height (cm)", y="Count")
# height_hist
# weight_hist <- ggplot(milestone1_subset)+
#   geom_histogram(mapping=aes(WEIGHT0), na.rm = TRUE, color="black")+
#   theme_classic()+
#   labs(title="Spread of Weight", x="Weight (kg)", y="Count")
# weight_hist
# 
# ##Frequencies of the categorical data
# ##NA's were not being removed with na.rm=TRUE, therefore they were removed before calling the ggplot functions
# anem_freq <- milestone1_subset %>% drop_na() %>% 
#   ggplot()+
#   geom_bar(mapping=aes(ANEMIA0))+
#   theme_classic()+
#   labs(title= "Frequency of Patients with Anemia", y= "Count", 
#        x="Patients has Anemia")
# anem_freq
# 
# smoke_freq <- milestone1_subset %>% drop_na() %>% 
#   ggplot()+
#   geom_bar(mapping=aes(SMOKERE0))+
#   theme_classic()+
#   labs(title= "Frequency of Patients who Smoke", y= "Count", 
#        x="Patients who Smoke")
# smoke_freq
# ##Analysis of RACE
# ggplot(milestone1_subset)+
#   geom_bar(mapping=aes(RACE))+
#   labs(title="Patient Race")+
#   theme(axis.text.x=element_text(size=13, angle=45, hjust=1, vjust=1))
# ##creating frequency and pareto charts to plot the race data
# ##Creates a tibble with the total counts for each race identified
# race <- milestone1_subset %>% group_by(RACE) %>% summarise(Total=n())
# ##Calculates the frequency of each race
# race <- mutate(race, Frequency=Total/sum(Total))
# race
# race_pareto <- ggplot(race, mapping=aes(x=RACE, y=Total))+
#   stat_pareto(bars.fill="gray")+
#   labs(title="Patient Race Pareto", x="Race")+
#   theme_classic()+
#   theme(axis.text.x=element_text(size=10, angle=75, hjust=1, vjust=1))
# race_pareto


##Data analysis for relationships between data
#Relationship between height and weight
# hvw_scatter <- ggplot(milestone1_subset)+
#   geom_point(mapping=aes(HEIGHT0, WEIGHT0), na.rm=TRUE, size=0.5)+
#   geom_smooth(mapping=aes(HEIGHT0, WEIGHT0), method=lm, formula=y~x, na.rm=TRUE)+
#   theme_classic()
# hvw_scatter
# 
# #understanding the trends in patients identified with anemia
# avr_jitter <- milestone1_subset %>% drop_na() %>% ggplot()+
#   geom_jitter(mapping=aes(RACE, ANEMIA0), na.rm=TRUE, size=0.5)+
#   labs(title="Anemia by Race", x="Race", y="Patient had Anemia Diagnosis?")+
#   theme_classic()+
#   theme(axis.text.x=element_text(size=10, angle=75, hjust=1, vjust=1))
# avr_jitter
# 
# pulse_density <- milestone1_subset %>% drop_na() %>% ggplot()+
#   geom_density(mapping=aes(PULSE0, color=SMOKERE0), size=1)+
#   theme_classic()+
#   labs(title="Density of Pulse by Smoking Status", x= "Pulse (beats/30 sec)", y="Density")
# pulse_density





###############
# MILESTONE 2 #
###############

#loading the additional R Libraries for this analysis
library(nhstplot)

#Adding additional columns to the data table
milestone2_subset <- subset(rawData, select = c(
  SWANID,
  AGE0,
  ANEMIA0,
  LISTEN0,
  TAKETOM0,
  CONFIDE0,
  SYSBP10,
  DIABP10,
  HELPSIC0,
  SMOKERE0,
  PULSE0,
  HEIGHT0,
  WEIGHT0,
  RACE)
)

milestone2_subset <- mutate(milestone2_subset, ANEMIA0=ifelse(ANEMIA0=="(1) No",
                                                              "No", ifelse(ANEMIA0=="(2) Yes", "Yes", NA)),
                          SMOKERE0=ifelse(SMOKERE0=="(1) No", "No", ifelse(SMOKERE0=="(2) Yes", "Yes", NA)),
                            RACE=ifelse(RACE=="(1) Black/African American", "Black/African American",
                                        ifelse(RACE=="(2) Chinese/Chinese American", 
                                               "Chinese/Chinese American",
                                               ifelse(RACE=="(3) Japanese/Japanese American",
                                                      "Japanese/Japanese American",
                                                      ifelse(RACE=="(4) Caucasian/White Non-Hispanic", 
                                                             "Caucasian/ White Non-Hispanic", 
                                                             ifelse(RACE=="(5) Hispanic", "Hispanic", NA)
                                                      )))))
milestone2_subset <- mutate(milestone2_subset, LISTEN0=as.numeric(substr(LISTEN0, 2,2)), TAKETOM0=as.numeric(substr(TAKETOM0,2,2)),
       CONFIDE0=as.numeric(substr(CONFIDE0,2,2)), HELPSIC0=as.numeric(substr(HELPSIC0,2,2)))


str(milestone2_subset)
summary(milestone2_subset)

##creating frequency and pareto charts to plot the race data
##Creates a tibble with the total counts for each race identified
race <- milestone2_subset %>% group_by(RACE) %>% summarise(Total=n())
##Calculates the frequency of each race to identify minorities
race <- mutate(race, Frequency=Total/sum(Total))
race
race_pareto <- ggplot(race, mapping=aes(x=RACE, y=Total))+
  stat_pareto(bars.fill="gray")+
  labs(title="Patient Race Pareto", x="Race")+
  theme_classic()+
  theme(axis.text.x=element_text(size=10, angle=75, hjust=1, vjust=1))
race_pareto
#a minority will be any group that has a frequency less than 0.2 (5 races, means each should have a fifth of the population if there was even)
#SupportScore is to assess whether the patient feels supported. It combines the numeric value for each of the questions:
##Have someone who listens? Have someone to take to the doctor? Have someone to confide in? Have someone to do chores when sick?
milestone2_subset <- mutate(milestone2_subset, Subdivision=ifelse(RACE=="Black/African American"|RACE=="Caucasian/ White Non-Hispanic", "Majority", "Minority"),
                            SupportScore=LISTEN0+TAKETOM0+CONFIDE0+HELPSIC0, SupportAvg=SupportScore/4)
view(milestone2_subset)

#Repeated analysis from milestone1 with the full subset
age_sd <- sd(milestone2_subset$AGE0, na.rm = TRUE)
age_mean <- mean(milestone2_subset$AGE0, na.rm = TRUE)
pulse_sd <- sd(milestone2_subset$PULSE0, na.rm = TRUE)
pulse_mean <- mean(milestone2_subset$PULSE0, na.rm = TRUE)
height_sd <- sd(milestone2_subset$HEIGHT0, na.rm = TRUE)
height_mean <- mean(milestone2_subset$HEIGHT0, na.rm=TRUE)
weight_sd <- sd(milestone2_subset$WEIGHT0, na.rm = TRUE)
weight_mean <- mean(milestone2_subset$WEIGHT0, na.rm=TRUE)
age_med <- median(milestone2_subset$AGE0, na.rm=TRUE)
pulse_med <- median(milestone2_subset$PULSE0, na.rm=TRUE)
height_med <- median(milestone2_subset$HEIGHT0, na.rm=TRUE)
weight_med <- median(milestone2_subset$WEIGHT0, na.rm=TRUE)

rows <- c("Age", "Pulse", "Height", "Weight")
means <- c(age_mean, pulse_mean, height_mean, weight_mean)
sds <- c(age_sd, pulse_sd, height_sd, weight_sd)
medians <- c(age_med, pulse_med, height_med, weight_med)

sumStats <- data.frame(Variable=rows, Mean=means, Standard.Deviation=sds, Median=medians)
sumStats

##visualization of the data spread for continuous plots through histograms
age_hist <- ggplot(milestone2_subset)+
  geom_histogram(mapping=aes(AGE0), na.rm = TRUE, binwidth = 1, color="black")+
  theme_classic()+
  labs(title="Spread of Age", x="Age (years)", y="Count")
age_hist
##put a box plot in to highlight the outliers in this chart
pulse_hist <- ggplot(milestone2_subset,mapping=aes(PULSE0), na.rm = TRUE)+
  geom_histogram(color="black")+
  geom_boxplot()+
  theme_classic()+
  labs(title="Spread of Pulse", x="Pulse (beats/30 sec)", y="Count")
pulse_hist
height_hist <- ggplot(milestone2_subset)+
  geom_histogram(mapping=aes(HEIGHT0), na.rm = TRUE, color="black")+
  theme_classic()+
  labs(title="Spread of Height", x="Height (cm)", y="Count")
height_hist
weight_hist <- ggplot(milestone2_subset)+
  geom_histogram(mapping=aes(WEIGHT0), na.rm = TRUE, color="black")+
  theme_classic()+
  labs(title="Spread of Weight", x="Weight (kg)", y="Count")
weight_hist
support_hist <- ggplot(milestone2_subset)+
  geom_histogram(mapping=aes(SupportScore), na.rm=TRUE, color="black", binwidth = 1)+
  theme_classic()+
  labs(title="Spread of Support Score", x="Total Support", y="Count")
support_hist

##Frequencies of the categorical data
##NA's were not being removed with na.rm=TRUE, therefore they were removed before calling the ggplot functions
anem_freq <- milestone2_subset %>% na.omit() %>% 
  ggplot()+
  geom_bar(mapping=aes(ANEMIA0))+
  theme_classic()+
  labs(title= "Frequency of Patients with Anemia", y= "Count", 
       x="Patients has Anemia")
anem_freq

smoke_freq <- milestone2_subset %>% na.omit() %>% 
  ggplot()+
  geom_bar(mapping=aes(SMOKERE0))+
  theme_classic()+
  labs(title= "Frequency of Patients who Smoke", y= "Count", 
       x="Patients who Smoke")
smoke_freq

##Data analysis for relationships between data
#Relationship between height and weight
hvw_scatter <- ggplot(milestone2_subset)+
  geom_point(mapping=aes(HEIGHT0, WEIGHT0), na.rm=TRUE, size=0.5)+
  geom_smooth(mapping=aes(HEIGHT0, WEIGHT0), method=lm, formula=y~x, na.rm=TRUE)+
  theme_classic()
hvw_scatter

#understanding the trends in patients identified with anemia
avr_jitter <- milestone2_subset %>% na.omit() %>% ggplot()+
  geom_jitter(mapping=aes(RACE, ANEMIA0), na.rm=TRUE, size=0.5)+
  labs(title="Anemia by Race", x="Race", y="Patient had Anemia Diagnosis?")+
  theme_classic()+
  theme(axis.text.x=element_text(size=10, angle=75, hjust=1, vjust=1))
avr_jitter

pulse_density <- milestone2_subset %>% na.omit() %>% ggplot()+
  geom_density(mapping=aes(PULSE0, color=SMOKERE0), size=1)+
  theme_classic()+
  labs(title="Density of Pulse by Smoking Status", x= "Pulse (beats/30 sec)", y="Density")
pulse_density

# #Question 1: Are minority groups as supported as majority groups?
# minority <- filter(milestone2_subset, Subdivision=="Minority")
# minority
# majority <- filter(milestone2_subset, Subdivision=="Majority")
# majority
# 
# #State the null hypothesis, alternative hypothesis and the claim
# Null <- "Mean Minority = Mean Majority"
# Alt <- "Mean Minority NEQ Mean Majority"
# Claim <- "Minority groups do not feel as supported as majority groups"
# 
# #Calculate the sample statistics for a two sample t test
# minority <- na.exclude(minority)
# majority <- na.exclude(majority)
# min_n <- nrow(minority)
# maj_n <- nrow(majority)
# min_xbar <- mean(minority$SupportScore)
# maj_xbar <- mean(majority$SupportScore)
# min_s <- sd(minority$SupportScore)
# maj_s <- sd(majority$SupportScore)
# alpha <- 0.05
# min_df <- min_n-1
# maj_df <- maj_n-1
# df <- min(min_df, maj_df)
# 
# #Calculate the critical value
# halpha <- alpha/2
# cv <- qt(p=1-halpha, df=df, lower.tail=TRUE)
# plotttest(cv, df=df)
# 
# #calculate t statistic
# t <- ((min_xbar-maj_xbar)-0)/sqrt(((min_s^2)/min_n)+((maj_xbar^2)/maj_n))
# t.test(minority$SupportScore, majority$SupportScore,alternative = 'two.sided', var.equal = FALSE)
#
# #I realized after doing this test that the data for support score and support score average might not be normally distributed
# ggplot(milestone2_subset)+
#   geom_histogram(mapping=aes(SupportScore))


#Commented this code because I realized the data is negatively skewed for the 
#support score so a different investigation needs to be had for that particular 
#question





#Question 1: Do women with anemia have the same pulse as women without anemia?
with <- filter(milestone2_subset, ANEMIA0=="Yes", !is.na(PULSE0))
summary(with)
without <- filter(milestone2_subset, ANEMIA0=="No", !is.na(PULSE0))
summary(without)

#State the null hypothesis, alternative hypothesis and claim
Null <- "mu1 is equal to mu2"
ALT <- "mu1 does not equal mu2"
Claim <- "Women with anemia have a different average pulse than women without it"

#creating a random sample
set.seed(78)
with_samp <- sample(nrow(with), 100)
with_samp <- with[with_samp,]
with_samp

set.seed(34)
without_samp <- sample(nrow(without), 100)
without_samp <- without[without_samp,]
without_samp

#Calculate critical values
alpha <- 0.05
tails <- 2
halpha <- alpha/tails
cv <- qt(p=1-halpha, df=99, lower.tail = TRUE)
plotttest(cv, df=99, tails= "two")

#calculate the test statistic
ttest <- t.test(with_samp$PULSE0, without_samp$PULSE0, alternative = "two.sided", var.equal = FALSE)
t <- ttest$statistic
t

#Make decision
decision <- if(abs(cv)>abs(t)){
  "Do not reject Null Hypothesis"
}else{
  "Reject Null Hypothesis"
}
decision

#Summarize results
conclusion <- if(decision=="Do not reject Null Hypothesis"){
  "There is not enough evidence to support the claim:"
}else{
  "There is enough evidence to support the claim:"
}

cat(conclusion, Claim, sep=" ")





#Question 2: Is the proportion of women who smoke at age 45 equivalent to the proportion of all women who smoke?
smokers <- milestone2_subset %>% filter(SMOKERE0=="Yes") %>% nrow()
Total <- filter(milestone2_subset, !is.na(SMOKERE0)) %>% nrow()
pop_prop <- smokers/Total
fortyfivers <- filter(milestone2_subset, AGE0==45)

#State the null hypothesis, alternative hypothesis, and claim
cat("Null: p =", as.character(round(pop_prop*100)), "%", sep = " ")
cat("Alternative: p neq", as.character(round(pop_prop*100)), "%", sep = " ")
Claim <- "The proportion of smokers at age 45 is equal to the proportion of smokers in the SWAN dataset"

#Find proportion statistics
p <- pop_prop
q <- 1-p
smoker_45 <- fortyfivers %>% filter(SMOKERE0=="Yes") %>% nrow()
n <- filter(fortyfivers, !is.na(SMOKERE0)) %>% nrow()
phat <- smoker_45/n

#Calculating the critical value
alpha <- 0.05
tails <- 2
halpha <- alpha/2
cv <- qnorm(halpha, lower.tail = TRUE)
plotztest(cv, tails="two")

#Calculate z
z <- (phat-p)/sqrt(p*q/n)

#Make decision
decision <- if(abs(cv)>abs(z)){
  "Do not reject Null Hypothesis"
}else{
  "Reject Null Hypothesis"
}
decision

#Summarize the results
conclusion <- if(decision=="Reject Null Hypothesis"){
  "There is not enough evidence to support the claim:"
}else{
  "There is enough evidence to support the claim:"
}

cat(conclusion, Claim, sep=" ")

###############
# MILESTONE 3 #
###############
library(car)

#viewing all numerical relationships
scatterplotMatrix(data=milestone2_subset, ~HEIGHT0 + WEIGHT0 + PULSE0 + DIABP10 + SYSBP10, main="Fig. 14 Scatterplot Matrix")

# Is there a significant positive linear relationship between height and weight?
##Scatterplot made from earlier
hvw_scatter <- hvw_scatter+
  theme_classic()+
  labs(title = "Height vs Weight Scatterplot", x="Height", y="Weight")
hvw_scatter

library(hexbin)
#Viewing the data in an easier fashion due to the high density of the chart
with(milestone2_subset, {
  bin <- hexbin(HEIGHT0, WEIGHT0, xbins = 50)
  plot(bin, main="Hexagonal Binning", xlab="Height", ylab="Weight")
})

cor(milestone2_subset$HEIGHT0, milestone2_subset$WEIGHT0, use = "complete.obs", method="pearson")

cor.test(x=milestone2_subset$HEIGHT0,
         y=milestone2_subset$WEIGHT0,
         method = "pearson",
         alternative="two.sided",
         conf.level = 0.95)
#This is a positive significant relationship because p<0.05
hvw_scatter


# Can Diastolic blood pressure predict systolic blood pressure?
#visualize the data
ggplot(milestone2_subset, mapping=aes(DIABP10, SYSBP10))+
  geom_point(na.rm = TRUE)

cor.test(x=milestone2_subset$DIABP10,
         y=milestone2_subset$SYSBP10,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#statistically significant - reject null P<0.05

#Linear model
bp_lm <- lm(data=milestone2_subset, SYSBP10~DIABP10)
bp_lm
bp_regplot <- ggplot(milestone2_subset, mapping=aes(DIABP10, SYSBP10))+
  geom_point(na.rm = TRUE)+
  geom_smooth(method = lm, se=FALSE)+
  theme_classic()+
  labs(title="Blood Pressure Regression", x="Diastolic Blood Pressure", y="Systolic Blood Pressure")
bp_regplot

#diagnostics of the linear fit
par(mfrow=c(2,2))
plot(bp_lm, main="Figure 16 Linear Model Diagnostics")

#predictive modeling
diabp_future <- data.frame("DIABP10"=seq(60,100,10))
diabp_future
sys_pred <- predict(newdata=diabp_future, bp_lm)
sys_pred

prediction_data <- data.frame(diabp_future, "SYSBP10"=sys_pred)
prediction_data
bp_regFinal <- ggplot(na.rm = TRUE)+
  geom_point(milestone2_subset, mapping = aes(DIABP10, SYSBP10))+
  geom_smooth(milestone2_subset, mapping = aes(DIABP10, SYSBP10), method = lm, se=FALSE)+
  geom_point(prediction_data, mapping = aes(DIABP10, SYSBP10), color="red", size=2)+
  theme_classic()+
  labs(title="Blood Pressure Regression", x="Diastolic Blood Pressure", y="Systolic Blood Pressure")
bp_regFinal


### Does weight relate to blood pressure?
wvbp_scatter <- ggplot(milestone2_subset, mapping=aes(WEIGHT0, DIABP10))+
  geom_point(na.rm = TRUE)+
  geom_smooth(method = lm)+
  theme_classic()+
  labs(title="Weight vs Blood Pressure", x="Weight", y="Diastolic Blood Pressure")

#Looking at this chart in density space
with(milestone2_subset, {
  bin <- hexbin(WEIGHT0, DIABP10, xbins = 50)
  plot(bin, main="Hexagonal Binning", xlab="Weight", ylab="Diastolic Blood Pressure")
})

cor(milestone2_subset$WEIGHT0, milestone2_subset$DIABP10, use="complete.obs", method = "pearson")
 #very weak positive relationship

cor.test(x=milestone2_subset$WEIGHT0,
         y=milestone2_subset$DIABP10,
         method="pearson",
         alternative = "two.sided",
         conf.level = 0.95)
# Are there stronger relationships within racial subdivisions?
minority <- milestone2_subset %>% filter(Subdivision=="Minority")
majority <- milestone2_subset %>% filter(Subdivision=="Majority")
cor.test(x=minority$WEIGHT0,
         y=minority$DIABP10,
         method="pearson",
         alternative = "two.sided",
         conf.level = 0.95)
cor.test(x=majority$WEIGHT0,
         y=majority$DIABP10,
         method="pearson",
         alternative = "two.sided",
         conf.level = 0.95)
wvbp_scatter_sub <- ggplot(milestone2_subset, mapping=aes(WEIGHT0, DIABP10, color=Subdivision))+
  geom_point(na.rm = TRUE)+
  geom_smooth(method = lm, se=FALSE)+
  theme_classic()+
  labs(title="Weight vs Blood Pressure", x="Weight", y="Diastolic Blood Pressure")
wvbp_scatter_sub 
