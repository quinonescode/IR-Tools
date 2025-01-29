

#Lets open the libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(utils) 
library(sqldf) #for SQL
library(lubridate) #for age and time 
library(formattable) #for fancy tables
library(readxl) #for reading Excel tables 


summer<-read_csv('H:/Tickets/W2407-0908/data/raw/all202020.csv')
fall <- read_csv('H:/Tickets/W2407-0908/data/raw/all202040.csv')


All<- rbind (fall, summer)

Transfer <- read_csv('H:/Research/Miliary Friendly Survey/data/raw/Clearinghouse202040.csv')

Transfer$'College Name'<- as.factor(Transfer$'College Name')
summary(Transfer$'College Name')



#Of the students in (Student have to be New between Fall 2020 - summer 2020 To count) number that transferred / 
#Student have to be New between Fall 2020 - summer 2020 To count 


#First goal: Denominator 
deno <- subset(All, Student_Type_Code == "N" &  !(is.na(Veteran)) )
deno$Veteran <- as.factor(deno$Veteran)
summary(deno$Veteran)

#The denominator is: 135
length(unique(deno$Student_ID))


names(Transfer)

#Second goal: Numerator 
num <- 
sqldf("
  SELECT 
  b.Student_ID,
  b.Veteran,
  a.'College Name' as Transfer_College,
  a.'Graduated?' as Graduated
  FROM Transfer a
  RIGHT JOIN deno b
  ON a.'Requester Return Field' = b.Student_ID
  ")

#There are a lot of dubs in this version of the num but that is fine 
#Next we remove students who stayed at LCCC
num<-subset(num, Transfer_College != "LEHIGH CARBON COMMUNITY COLLEGE")

#The numerator is: 51
length(unique(num$Student_ID))

#Transfer rate is: 0.3777778
length(unique(num$Student_ID))/length(unique(deno$Student_ID))


#####Broken down by type

num$Veteran <- as.factor(num$Veteran)
summary(num$Veteran)


#Transfer rate for Active Duty Military students
#The numerator is: 3
active <- num %>%
  filter(num$Veteran == "Active Duty Military")

length(unique(active$Student_ID))/length(unique(deno$Student_ID))
#Rate: 0.02222222



#Transfer rate for Guard Reserve Military students
#The numerator is: 3
res <- num %>%
  filter(num$Veteran == "Reservist")

length(unique(res$Student_ID))/length(unique(deno$Student_ID))
#Rate: 0.02222222

#Transfer rate for Military Spouse students
#The numerator is: 3
spouse <- num %>%
  filter(num$Veteran == "Spouse of Veteran")

length(unique(spouse$Student_ID))/length(unique(deno$Student_ID))
#Rate: 0.02222222

#Transfer rate for ALL Military Dependent
#The numerator is: 19
dependent <- num %>%
  filter(num$Veteran == "Dependant of Veteran")

length(unique(dependent$Student_ID))/length(unique(deno$Student_ID))
#Rate:  0.1407407

