###########################################
#   ALY6010 -- Professor Dan Koloski      #
#   Week 1 In-Class Examples for R        #
#   Using LungCapDataCSV.csv data         #
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



################## R examples for Module 1 project ##########



#Library up some helpful packages
library(tidyverse)
library(data.table)  # for frequency tables
library(FSA)   #to get headtail
library(ggplot2)
library(skimr)
library(reshape2)
library(gmodels) # for CrossTable
library(DT) # for datatable


# Import the DataSet -- make the strings factors
LungCap <- read.csv("LungCapDataCSV.csv", stringsAsFactors = TRUE)

# Learn about the data set - traditional way
headtail(LungCap)
str(LungCap)
summary(LungCap)

#Calculating some Z-scores for LungCap
LungCapPopMean <- mean(LungCap$LungCap)
LungCapPopMean
LungCapPopStDev <- sd(LungCap$LungCap)
LungCapPopStDev
hist(LungCap$LungCap, main="Histogram of Lung Capacity", xlab="Lung Capacity", col="light blue")


# We can turn the entire column into a vector of Z values using scale
LungCapVector <- LungCap$LungCap
LungCapVectorZ <- scale(LungCap$LungCap)
LungCapVectorComparison <- data.frame(LungCapVector,LungCapVectorZ)
headtail(LungCapVectorComparison, n=10)
mean(LungCapVector)
sd(LungCapVector)
mean(LungCapVectorZ)
sd(LungCapVectorZ)
hist(LungCapVector)
hist(LungCapVectorZ)

# Learn about the data set with skimr
skim(LungCap)
WhatIsSkimOutput <- skim(LungCap)
class(WhatIsSkimOutput)
str(WhatIsSkimOutput)

# make some Pivot tables using pivottabler
# https://cran.r-project.org/web/packages/pivottabler/vignettes/v00-vignettes.html
library(pivottabler)
LungCapPivotSmokersByAgeAndGender <- PivotTable$new()
LungCapPivotSmokersByAgeAndGender$addData(LungCap)
LungCapPivotSmokersByAgeAndGender$addColumnDataGroups("Gender")
LungCapPivotSmokersByAgeAndGender$addRowDataGroups("Age")
LungCapPivotSmokersByAgeAndGender$defineCalculation(calculationName="Smoke", summariseExpression="n()")
LungCapPivotSmokersByAgeAndGender$renderPivot()
LungCapPivotSmokersByAgeAndGender

LungCapPivotMeanLungCapByByAgeAndGenderSmokers <- PivotTable$new()
LungCapPivotMeanLungCapByByAgeAndGenderSmokers$addData(LungCap)
LungCapPivotMeanLungCapByByAgeAndGenderSmokers$addColumnDataGroups("Gender")
LungCapPivotMeanLungCapByByAgeAndGenderSmokers$addColumnDataGroups("Smoke")
LungCapPivotMeanLungCapByByAgeAndGenderSmokers$addRowDataGroups("Age")
LungCapPivotMeanLungCapByByAgeAndGenderSmokers$defineCalculation(calculationName="AvgLungCap", summariseExpression="mean(LungCap)")
LungCapPivotMeanLungCapByByAgeAndGenderSmokers$renderPivot()
LungCapPivotMeanLungCapByByAgeAndGenderSmokers

# generate the same table in condensed format
LungCapPivotMeanLungCapByByAgeAndGenderSmokers <- qhpvt(dataFrame = LungCap, 
      rows=c("Age"),
      columns=c("Gender","Smoke"),
      calculations=c("mean(LungCap)"),
      totals=list("")
)
LungCapPivotMeanLungCapByByAgeAndGenderSmokers
 
# re-generate the same table in condensed format with nummber formatting
LungCapPivotMeanLungCapByByAgeAndGenderSmokers <- qhpvt(dataFrame = LungCap, 
                                                        rows=c("Age"),
                                                        columns=c("Gender","Smoke"),
                                                        calculations=c("mean(LungCap)"),
                                                        format=c("%#.3f"),
                                                        totals=list("")
)
LungCapPivotMeanLungCapByByAgeAndGenderSmokers

# Generate various kinds of tables using data.table and other .table commands

#Subset the data to only get teens
LungCapTeens = dplyr::filter(LungCap, Age>10)
summary(LungCapTeens)


attach(LungCapTeens)
#table of counts
LungCapandSmoke <- table(Age,Smoke) # A will be rows, B will be columns
LungCapandSmoke

# table of relative counts 
margin.table(LungCapandSmoke, 1) # row (Age) counts
margin.table(LungCapandSmoke, 2) # column (Smoke) counts

# table of proportions 
prop.table(LungCapandSmoke) # cell percentages (total cells across all columns = 100%)
prop.table(LungCapandSmoke, 1) # row percentages (each row = 100%)
prop.table(LungCapandSmoke, 2) # column percentages (each column = 100%)

# Frequency Table of Gender
xtabs(~Age+Gender, data=LungCapTeens)

# Frequency Table of Gender by whether they smoke
xtabs(~Age+Gender+Smoke, data=LungCapTeens)

# Frequency Table of Smoke No/Yes by age
ftable(LungCapandSmoke) # print table

# More cool ways to get Frequency Tables
headtail(LungCapTeens)
ftable(LungCapTeens, row.vars = c("Age","Gender"), col.vars = "Smoke") # print table selecting pivot fields by name
ftable(LungCapTeens, row.vars = c("Age","Gender","Caesarean"), col.vars = "Smoke") # print table selecting pivot fields by name
ftable(LungCapTeens, row.vars = c(2,5), col.vars = "Smoke") # print table selecting pivot fields by column number

# Generate a CrossTab Table using CrossTable
library(gmodels)
CrossTable(LungCap$Age, LungCap$Smoke, dnn = c("Age", "Smoker?")) 

#using DT package (datatable) to create interactive tables
library(DT)

datatable(LungCapTeens)


# Using gsub and mutate with piping to subset and change data simultaneously
headtail(LungCapTeens)
# Drop the caesarian column (take only columns 1-5 and change gender from "female/male" to "F/M")
LungCapTeens2 <- LungCapTeens %>% 
                     select(1:5)  %>% 
                     mutate(Gender=gsub(c("female"),c("F"),Gender, ignore.case=T)) %>% 
                     mutate(Gender=gsub(c("male"),c("M"),Gender, ignore.case=T))
headtail(LungCapTeens2)
str(LungCapTeens2)





