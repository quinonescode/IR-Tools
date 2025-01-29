
#Use AY 2023-2024 data.

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
all202340 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202340.csv')
all202350 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202350.csv')
all202410 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202410.csv')
all202420 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202420.csv')

df <- rbind(all202340,
            all202350,
            all202410,
            all202420)

names(df)

#Unduplicated Headcount
df1 <- df%>% 
  distinct(Student_ID, .keep_all = TRUE)

df1$Student_Type_Code <-as.factor (df1$Student_Type_Code)
summary(df1$Student_Type_Code)


