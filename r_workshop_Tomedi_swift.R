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


### Simulate some physical activity data to join with the ECIG data 


#create another dataset from ecig, using subset function to remove all variables but e cig and ID
active = subset(ecig, select = -c(survey1,first,middle,last,monthuse,tob1,gender,hisp,race,dob,famhx))

set.seed(456)
active <-active %>% 
  mutate(active=runif(825,10,50))

active <-active %>% 
  mutate(active= case_when(ecig1 == 0 ~ runif(825,5,70),
                               ecig1 == 1 ~ runif(825,5,20)))

#Join data using an inner join

Joined_data <- inner_join(ecig, active, by=c("id" = "id"))

#where to find help with packages 
help(ggplot2)
help(epitools)


###############
#### Make 2 x 2 tables with Epi tools 

###############

table(ecig$ecig1)
table(ecig$famhx)



#table in this case will make a 2X2 that goes exposure(rows) X outcome(Columns)

twobytwo<-table(ecig$famhx,ecig$ecig1)

#you need that table to 
twobytwo

#odds ratio
oddsratio(twobytwo, rev="both")   ### Use the rev() argument to flip the rows and columns
#relative risk
rr<-riskratio(twobytwo, rev="both")   ### Use the rev() argument to flip the rows and columns 



##########
#Do some Data Visualization using GG Pliot

## Create a data frame that is the aggregated means of e cig use by greenspace n family hx 



plotdata <- as.data.frame(aggregate(Joined_data$active, list(Joined_data$ecig1.x), FUN=mean)) 
#rename Column names
colnames(plotdata)[colnames(plotdata) == "Group.1"] = "ecig"
colnames(plotdata)[colnames(plotdata) == "x"] = "mean"

p <- ggplot(plotdata, aes(y=mean, x=ecig)) + geom_bar(stat="identity") 

p

#add some axis lables 
p <- p + labs(x = "Electronic Cigarette Use", y = "Mean Minutes of physical Activity per Day")
p