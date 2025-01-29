


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
df$SBGI_Desc <- as.factor(df$SBGI_Desc)
summary(df$SBGI_Desc)
#Home School, Attended HS did not grad has GED, Did not attend HS has GED

####Spring High School Grads
#Enter the total number of public spring 2023 high school (HS) graduates in your service area. 
#Include December 2022 graduates, but do not include home-schooled or GED students.


#Used working copy factbook 





##Total from Row 1 Who Enrolled for Next Fall Term
#Enter the total high school graduates from row 1 who enrolled 
#at your institution for the Fall 2023 term.

head(df$High_School_Graduation_Date)
df$High_School_Graduation_Date <- as.Date(df$High_School_Graduation_Date, format="%m/%d/%Y")
summary(df$High_School_Graduation_Date)



dfhs <- df %>%
  filter(High_School_Graduation_Date <= "2023-07-01" & High_School_Graduation_Date >= "2022-09-01")%>%
  filter(SBGI_Desc != 'Home School' )%>%
  filter( SBGI_Desc != 'Attended HS did not grad has GED') %>%
  filter(SBGI_Desc != 'Did not attend HS has GED')%>%
  distinct(Student_ID, .keep_all = TRUE)


dfhs$SBGI_Desc <- as.factor(dfhs$SBGI_Desc)
summary(dfhs$SBGI_Desc)
summary(dfhs$High_School_Graduation_Date)




