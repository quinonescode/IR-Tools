


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

df1 <- df %>% 
  distinct(Student_ID , .keep_all = TRUE)

1896+4489 #Amount of full and part time from IPED Fall Enrollment, close enough

#% High School Student Concurrent Enrollment - Headcount
df1$Student_Type_Code <- as.factor(df1$Student_Type_Code)
table(df1$Student_Type_Code)
1779  / (1896+4489)

#Total Student Credit Hours
sum(df$Credit_Hours)

#Developmental Credit Hours
df2 <- df %>% 
  filter(Course_Number < 100)

(sum(df2$Credit_Hours)/(sum(df$Credit_Hours)))*100


#High School Student Concurrent Enrollment - Credit Hours
df3<-df %>% 
  filter(Student_Type_Code == "P") 

  (sum(df3$Credit_Hours)/(sum(df$Credit_Hours)))*100


#Credit Student Median Age
df$Birth_Date <- as.Date(df$Birth_Date, format = "%m/%d/%Y")
df$Term_Start_Date <- as.Date(df$Term_Start_Date, format = "%m/%d/%Y")
df$Age <- age_calc(df$Birth_Date, df$Term_Start_Date, units = "years")
median(df$Age)

#Average GPA For All Students
mean(df1$Overall_GPA)
mean(df1$Cum_GPA)


#Female Credit Students
df1$Gender <- as.factor(df$Gender)
prop.table(table(df1$Gender))


#######################################################
#First-generation Student                               What is the code for this variable?
#######################################################


#Count of IPED race (I didnt use this code, used the 2023-2024 Fall Enrollment numbers)
df1$IPEDS_Race_Desc <- as.factor(df1$IPEDS_Race_Desc)
summary(df1$IPEDS_Race_Desc)


#Tuition and Fees per Credit Hour
#ok, working backwards here, nccbp states that last year was $183
#ipeds cost 1 survey, 2022-2023 in-state, in-district tuition and fees total is 5100
5100/183 
#27.8 

#we are using fall 2023 data from ipeds cost 1 survey
#2023-2024 in-state, in-district tuition and fees total is 5215
5215/27.8 






#############I forgot to remove all high school students


clearing <- read_csv('H:/Research/NCCBP/clearinghouse.csv')
summary(df)
summary(clearing)

df.clear <- as.data.frame(sqldf("
      SELECT 
      a.Student_Type_Code,
      b.*
      FROM df1 a 
      LEFT JOIN clearing b
      ON a.Student_ID = b.'Requester Return Field'
      ")
)


df.clear.p <- df.clear %>% 
  filter(Student_Type_Code != "P")


write.csv(df.clear.p,"H:/noP.Clearinghouse1.csv")
