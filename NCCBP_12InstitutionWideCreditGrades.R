


###########################################################################################################

#Lets open the libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(utils) 
library(sqldf) #for SQL
library(lubridate) #for age and time 
library(formattable) #for fancy tables
library(readxl) #for reading Excel tables 
#install.packages("patchwork")
library(patchwork) # To display 2 charts together
#install.packages("shiny")
library(shiny) #for the multi level table 
library(reshape2) #need this too to make the multi level table 
library(eeptools) #calcuate age
###########################################################################################################

#Borrowed these argos files from a different ticket. Because Argos was not working today

df <- read_csv('H:/Tickets/W2407-0908/data/raw/all202340.csv')
summary(df)

df$firstletter <- substr(df$Grade_Code, 1, 1)

df$firstletter <- as.factor(df$firstletter)

summary(df$firstletter)



df <- read_csv('H:/Tickets/W2407-0908/data/raw/all202240.csv')
summary(df)

df$firstletter <- substr(df$Grade_Code, 1, 1)

df$firstletter <- as.factor(df$firstletter)

summary(df$firstletter)

library(readxl)
Grades_12 <- read_excel("H:/Research/NCCBP/Grades-12.xlsx")
names(Grades_12)

Grades_12$grade <- substr(Grades_12$SHRTCKG_GRDE_CODE_FINAL, 1,1)

Grades_12$grade <- as.factor(Grades_12$grade)
summary(Grades_12$grade)



