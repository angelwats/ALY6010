library(ggplot2)
library(tidyverse)
lung_cap <- read.csv("LungCapDataCSV.csv", header = TRUE)
str(lung_cap)
view(lung_cap)
lung_cap <- mutate(lung_cap, Smoke = as.logical(ifelse(Smoke=="no", FALSE, TRUE)),
       Caesarean = as.logical(ifelse(Caesarean=="no", FALSE, TRUE)),
       Gender = ifelse(Gender=="male", "Male", "Female"))

lung_cap <- lung_cap %>% transmute(Gender, Age, Height, LungCap, Smoke, Caesarean)

summary(lung_cap)
sd(lung_cap$Age)
sd(lung_cap$Height)
sd(lung_cap$LungCap)

age_hist <- ggplot(lung_cap)+
  geom_histogram(mapping=aes(Age), fill="navy")+
  facet_wrap(vars(Gender), nrow=2)+
  theme_classic()+
  labs(x= "Age (years)", y="Count", title="Histogram of Patient Age")
age_hist

sd <- sd(lung_cap$Age)
avg <- mean(lung_cap$Age)

lung_cap <- lung_cap %>% 
  mutate(AgeBuckets = ifelse(Age<=7, "B1", 
                             ifelse(between(Age, 8, 11), "B2", 
                                               ifelse(between(Age, 12, 15), "B3", "B4"))))

gen_Age <- lung_cap %>% group_by(Gender, AgeBuckets) %>% summarise(Total = n())
gen_Age <- gen_Age %>% mutate(Frequency = Total / sum(Total))
female_freq <- gen_Age %>% filter(Gender=="Female") %>%  arrange(desc(Frequency),by_group= Gender)
male_freq <- gen_Age %>% filter(Gender=="Male") %>%  arrange(desc(Frequency),by_group= Gender)

LCvH_Scatter <- ggplot(lung_cap, mapping=aes(x=Height, y=LungCap, color = Gender))+
  geom_point()+
  geom_smooth(method = lm, formula=y~x)+
  theme_classic()+
  labs(x="Height (in)", y="Lung Capacity (Unit)", title="Lung Capacity by Height")

HvA <- ggplot(lung_cap, mapping=aes(x=Age, y=Height, color = Gender))+
  geom_point()+
  geom_smooth(method = lm, formula=y~x)+
  theme_classic()+
  labs(x="Age (years)", y="Height (in)", title="Lung Capacity by Age")

LCvA_Scatter <- ggplot(lung_cap, mapping=aes(x=Age, y=LungCap, color = Gender))+
  geom_point()+
  geom_smooth(method = lm, formula=y~x)+
  theme_classic()+
  labs(x="Age (years)", y="Lung Capacity (Unit)", title="Lung Capacity by Height")

height_boxplot <- ggplot(lung_cap)+
  geom_boxplot(mapping=aes(Height, Gender, color=Gender))+
  theme_classic()+
  labs(x="Height (in)", y="Gender", title = "Boxplot of Height by Gender")

smokers <- filter(lung_cap, Smoke==TRUE)
nonsmokers <- filter(lung_cap, Smoke==FALSE)

LCvA_smokers <- ggplot()+
  geom_point(nonsmokers, mapping=aes(Age, LungCap), alpha=0.5)+
  geom_point(smokers, mapping=aes(Age, LungCap), color="red", size=4)+
  theme_classic()+
  labs(x="Age (years)", y="Lung Capacity (Unit)", title="Lung Capacity by Height (smoking status)")

smokers_MF <- ggplot(smokers)+
  geom_histogram(mapping=aes(Gender), stat="count", fill = "Dark Green")+
  theme_classic()+
  labs(y="Count", title = "Genders who smoke")
  
smokers_Age <- ggplot(smokers)+
  geom_histogram(mapping=aes(Age), fill="dark red")+
  xlim(3, 19)+
  geom_vline(xintercept = 7)+
  geom_vline(xintercept= 11)+
  geom_vline(xintercept=15)+
  theme_classic()+
  labs(x= "Age (years)", y="Count", title="Distribution of Age for Patients who Smoke")
