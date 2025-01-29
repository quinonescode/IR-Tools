


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


#Spoke with Marco, he said he would use Course_Campus_Code == DU 
df$Course_Campus_Code <- as.factor(df$Course_Campus_Code)
summary(df$Course_Campus_Code)


#Just duel enrollment students 
df1 <- df %>% 
  filter(Course_Campus_Code == "DU" ) %>%
  distinct(Student_ID, .keep_all = TRUE)

#number of credits
df2 <- df %>% 
  filter(Course_Campus_Code == "DU" ) 

sum(df2$Credit_Hours)

#% from the Row 2 Total (Concurrent Program Student Credit Hours) in Career Technical Education 




#Total ABCPDFW grades 
df3 <- df2 %>% 
  filter( Grade_Code %in% c('A', 'A-', 'A+', 'B','B+','B-','C', 'C+', 'C-', 'P', 'D', 'F', 'W')) %>% 
  count(Grade_Code)

total_count <- sum(df3$n)
total_count


#Total ABCPDFW grades 
df3 <- df2 %>% 
  filter( Grade_Code %in% c('A', 'A-', 'A+', 'B','B+','B-','C', 'C+', 'C-', 'P', 'D', 'F')) %>% 
  count(Grade_Code)

total_count <- sum(df3$n)
total_count

#Total ABCPDFW grades 
df3 <- df2 %>% 
  filter( Grade_Code %in% c('A', 'A-', 'A+', 'B','B+','B-','C', 'C+', 'C-', 'P')) %>% 
  count(Grade_Code)

total_count <- sum(df3$n)
total_count



######Current Enrollment from High School Concurrent Programs

#Borrowed these argos files from a different ticket. Because Argos was not working today
df24 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202440.csv')

df24 <- df24%>%
  distinct(Student_ID, .keep_all = TRUE)


enroll<-as.data.frame(
sqldf("
      SELECT 
      a.Student_ID,
      a.Reg_Term_Code, 
      b.Student_Type_Desc,
      b.Reg_Term_Desc 
      FROM df1 a 
      left JOIN df24 b
      ON a.Student_ID = b.Student_ID
      "))



enroll$Student_Type_Desc <- as.factor(enroll$Student_Type_Desc)
summary(enroll$Student_Type_Desc)

enroll_unique <- enroll %>%
  group_by(Student_ID)%>%
  filter(Student_Type_Desc == "New First Time")%>%
  slice(1)
#63

enroll_unique <- enroll %>%
  filter(Student_Type_Desc == "Continuing")%>%
  distinct(Student_ID, .keep_all = TRUE)

#63



####High School Concurrent Program Faculty

admin <- df2 %>%
  distinct(Primary_Instructor_Name)

##High School Concurrent Program District Partner

sd <- df1 %>%
  distinct(School_District_Desc)

#High School Concurrent Program Buildings

building<- df2%>% 
  distinct(Course_Building)