# .	Understand the basics of the R Studio interface
# .	Know how to find and download R packages (e.g., epi tools and ggplot2)
# .	Know how to import an excel or text file into R
# .	Understand the basics of some easier data cleaning techniques
# .	Understand how to merge two files together
# .	Know where to find some resources to help build additional skills
# .	Know how to calculate some basic descriptive measures (prevalence and crude rate)
# .	Basic data visualization in R



# How to install packages
# install.packages("readr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("epitools")
# install.packages("ggplot2")

# Library calls in packages once they are installed
#reads in data
library(readr)
# can be used to make variables
library(tidyverse)
# used to transpose data 
library(tidyr)
#used for some basic epidemiology
library(epitools)
#Basic data visualization
library(ggplot2)

#how to read in data
ecig <- read_csv("C:/Users/samuell.swift/OneDrive - New Mexico Department of Health/NMPHA/workshop/ecig.csv")

#WANT TO CLEAN OUT THE MISSING--
# change 10s to NAs
ecig$ecig1[ecig$ecig1 == 10] <- NA
ecig$famhx[ecig$famhx == 9] <- NA

ecig$ecig1 <- as.factor(ecig$ecig1)
ecig$famhx <- as.factor(ecig$famhx)


####You can see I removed the NAs by making a table
table(ecig$ecig1)

### Do some data cleaning and Simulate some physical activity data to join with the ECIG data 




####################Bonus Code-- Just if we have time (must be run for other code to work) ##############################

#create another dataset from ecig, using subset function to remove all variables but e cig and ID
active = subset(ecig, select = -c(survey1,first,middle,last,monthuse,tob1,gender,hisp,race,dob,famhx))


### Make up a "random" dataset of minutes for physical activity per day
### This dataset is pulled from uniform distribution (runif) and conditional on ecig use using "case when"
set.seed(456)

active <-active %>% 
  mutate(active= case_when(ecig1 == 0 ~ runif(825,5,70),
                               ecig1 == 1 ~ runif(825,5,20)))

#take out ecig1 so its not on both datasets 
active <- subset(active, select = -c(ecig1))

###################Join data using an inner join##########################

Joined_data <- inner_join(ecig, active, by=c("id" = "id"))


#########################Finding Help#########################################

#where to find help with packages 
help(ggplot2)
help(epitools)
#Oh no-- no help for epitools! Try online:

#this also works: 
?ggplot2



############### Epi tools #################################

table(ecig$ecig1)
table(ecig$famhx)

#Note these are not adding up to 825 because the missings are dropped


#table in this case will make a 2X2 that goes exposure(rows) X outcome(Columns)
#We add useNA = "always" to see missing

twobytwo_NA<-table(ecig$ecig1,ecig$famhx,useNA = "always")

#without missing
twobytwo<-table(ecig$ecig1,ecig$famhx)


#Table without labels 
twobytwo

#Lets add row and column names 
colnames(twobytwo) = c("No Fam Hx", "Fam Hx") 
rownames(twobytwo) = c("No Ecig", "Ecig") 

#look again with labels
twobytwo

#odds ratio
oddsratio(twobytwo, rev="both")   ### Use the rev() argument to flip the rows and columns

#lets check it using math (A/B)/(C/D)
ORcalc <-((29/94)/(134/400))

ORcalc
#relative risk
rr<-riskratio(twobytwo, rev="both")   ### Use the rev() argument to flip the rows and columns 

rr

##########
#Do some Data Visualization using GG Pliot

## Create a data frame that is the aggregated means of e cig use by physical activity



plotdata <- as.data.frame(aggregate(Joined_data$active, list(Joined_data$ecig1), FUN=mean)) 
#rename Column names
colnames(plotdata)[colnames(plotdata) == "Group.1"] = "ecig"
colnames(plotdata)[colnames(plotdata) == "x"] = "mean"

#relevel ecig categories (not one and zero)
levels(plotdata$ecig) <- c("No E cig use", "E cig use")


p <- ggplot(plotdata, aes(y=mean, x=ecig)) + geom_bar(stat="identity") 
p

#Change the colors (using fill=)
p <- ggplot(plotdata, aes(y=mean, x=ecig, fill=ecig)) + geom_bar(stat="identity") 
p

#add some axis lables 
p <- p + labs(x = "Electronic Cigarette Use", y = "Mean Minutes of physical Activity per Day")
p

#add a title
p <- p + ggtitle("Minutes of Physical Activity per day by Electronic Cigarette Use")
p

#make background white
p <- p + theme_classic() #axis lines but no gridlines
p
