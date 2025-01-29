#date/time: 10/7/2024 at 7:56am
#Ticket  - 
# Goal 2:  12-Month Enrollment 





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
###########################################################################################################

#Borrowed these argos files from a different ticket. Because Argos was not working today
all202320 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202320.csv')
all202340 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202340.csv')
all202350 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202350.csv')
all202410 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202410.csv')
all202420 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202420.csv')

df <- rbind(all202320,
            all202340,
            all202350 ,
            all202410,
            all202420)

names(df)

#I have to limit the time frame to Summer 2 2023 to Summer 1 2024 (July1,23-June30,24)
#Redo part of term to include summer 2 and summer 1 distinction 
head(df$Reg_Term_Desc)
head(df$Course_Part_of_Term_Code)

df <- df %>%
  mutate(part_term = 
           ifelse(Reg_Term_Desc == "Fall 2023", "Fall 2023", 
                  ifelse(Reg_Term_Desc == "Winter 2023", "Winter 2023",
                         ifelse(Reg_Term_Desc == "Spring 2024", "Spring 2024",
                                ifelse( Reg_Term_Desc == "Summer 2023" & Course_Part_of_Term_Code == "2", "Summer 2 2023", 
                                        ifelse( Reg_Term_Desc == "Summer 2023" & Course_Part_of_Term_Code !=  "2",  "Summer 1 2023", 
                                                ifelse( Reg_Term_Desc == "Summer 2024" & Course_Part_of_Term_Code == "2", "Summer 2 2024",
                                                      "Summer 1 2024"  )))))))

df$part_term <- as.factor(df$part_term)
summary(df$part_term)









##I have to limit the time frame to Summer 2 2023 to Summer 1 2024 (July1,23 - June30,24)
#Exclude variables

df1 <- df %>%
  filter(!(part_term == "Summer 1 2023" | part_term == "Summer 2 2024"))



df1$Reg_Term_Desc <- as.factor(df1$Reg_Term_Desc)
df1$Course_Part_of_Term_Code <- as.factor(df1$Course_Part_of_Term_Code)
df1$part_term <- as.factor(df1$part_term)
df1$Academic_Year <- as.factor(df1$Academic_Year)

df$Reg_Term_Desc <- as.factor(df$Reg_Term_Desc)
df$Course_Part_of_Term_Code <- as.factor(df$Course_Part_of_Term_Code)
df$part_term <- as.factor(df$part_term)
df$Academic_Year <- as.factor(df$Academic_Year)

#Checking
summary(df1$part_term)
summary(df$part_term)
#Checking
summary(df1$Reg_Term_Desc)
summary(df$Reg_Term_Desc)



#N students are N in any term from 202320 through 202420, T students are T in any term,
#P or G students are ones that are P or G in any term and also not N, T, or C in any term, so
df1$Student_Type_Desc<- as.factor(df1$Student_Type_Desc)
summary(df1$Student_Type_Desc)



df1 <- df1 %>%
  group_by(Student_ID) %>%
  mutate(Student_Type_Desc1=
           ifelse(any(Student_Type_Desc== "New First Time")==TRUE, "New First Time",
                  ifelse(any(Student_Type_Desc == "Transfer")==TRUE, "Transfer",
                         ifelse(Student_Type_Desc == "Continuing"|Student_Type_Desc == "Returning", "Continuing_Returning",
                                ifelse(Student_Type_Desc == "Guest/Visiting Student" 
                                       | Student_Type_Desc == "High School/Not Graduated", "Non‑degree Seeking", "NA")))))



df1$Student_Type_Desc1 <- as.factor(df1$Student_Type_Desc1)
summary(df1$Student_Type_Desc1)


#Simplify this, just a few columns to calculate the total credits
#Add in the full time part time column 
df2 <- df1 %>%
  group_by(Student_ID, Reg_Term_Code) %>%
  mutate(total_credits = sum(Credit_Hours, na.rm = TRUE))%>%
  mutate(FPT= ifelse(total_credits >= 12, "Full-Time", "Part-Time"))



#Summarize again so that if the student was full-time at any time, then they are marked Full time 
df2 <- df2 %>%
  group_by(Student_ID) %>%
  mutate(OverallFPT=
           ifelse(any(FPT== "Full-Time")==TRUE, "Full-Time",
                  "Part-Time"))


#Put the terms in order 
#order the factors 
df2$part_term <- factor(df2$part_term, levels = c("Summer 2 2023", 
                                                      "Fall 2023", 
                                                      "Winter 2023",
                                                      "Spring 2024",
                                                      "Summer 1 2024"))

#######Add a new column "Degree/Certificate-seeking"
df2 <- df2 %>% 
  mutate(Degree_Seeking = 
           ifelse(Student_Type_Code == "P" |Student_Type_Code == "G", "Non-degree" , "Degree" ))


#Keep the first student type listed for each student by order of term 
#Bonus this also unduplicated my list for me 
names(df2)

class(df2$Term_Start_Date)
df2$Term_Start_Date <- as.Date(df2$Term_Start_Date, format= "%m/%d/%Y")

df3 <- df2 %>% 
  group_by (Student_ID) %>%
             arrange(Student_ID, Term_Start_Date) %>%
           slice(1)%>%
           ungroup ()

df3$Student_Type_Code <- as.factor(df3$Student_Type_Code)
df3$Student_Type_Desc <- as.factor(df3$Student_Type_Desc)
df3$Student_Type_Desc1 <- as.factor(df3$Student_Type_Desc1)


summary(df3$Student_Type_Code)
summary(df3$Student_Type_Desc)
summary(df3$Student_Type_Desc1)

#check for dups
n_occur <- data.frame(table(df3$Student_ID))
n_occur[n_occur$Freq > 1,]






#also check that we didn't lose anyone
check<-as.data.frame(unique(df$Student_ID))
#sadly, I am missing some people, df unduplicated = 9860 and df3 = 9140
#Oh I forgot, we dropped a lot of people who were in the summer 1 2023 and summer 2 2024
#so a better df to use would be df1
check<-as.data.frame(unique(df1$Student_ID))
#perfect


#subset the dataframe to be easier to see
df4<-df3 %>%
  select  (Student_ID, 
         Gender,
         Student_Type_Code,
         Student_Type_Desc1 ,
         OverallFPT ,
         IPEDS_Race_Desc)




##########################################################
names(df4)

#order the factors 
df4$IPEDS_Race_Desc <- factor(df4$IPEDS_Race_Desc , levels = c("U.S. Nonresident", 
                                                              "Hispanic or Latino", 
                                                              "American Indian or Alaska Native",
                                                              "Asian",
                                                              "Black or African American",
                                                              "Native Hawaiian or Other Pacific Islander",
                                                              "White",
                                                              "Two or more races",
                                                              "Race and ethnicity Unknown"))


df4$Student_Type_Desc1 <- as.factor(df4$Student_Type_Desc1 )
summary(df4$Student_Type_Desc1)

#order the factors 
df4$Student_Type_Desc1 <- factor(df4$Student_Type_Desc1, 
                                levels = c("New First Time", 
                                                  "Transfer", 
                                                  "Continuing_Returning",
                                           "Non‑degree Seeking"))

df4$Gender <- factor(df4$Gender, levels = c("M", "F", "N"))







#Answering questions 

#FIRSTLY!!!!! The question is for full time and part time students, I have to make two different df for them 
#Report 12 month unduplicated count by gender then by degree type and by sub-levels of type ("first-time" etc) then by race
#Gender, "Student_Type_Desc", 
names(df4)
df4$OverallFPT <- as.factor(df4$OverallFPT)
summary(df4$OverallFPT)

df4Full <- df4 %>% 
  filter(OverallFPT == "Full-Time")

df4Part <- df4 %>% 
  filter(OverallFPT == "Part-Time")

summary(df4Part)
summary(df4Full)


############################Answer 1 Full Time
dftableFull <- df4Full%>% 
  count(Gender, IPEDS_Race_Desc, Student_Type_Desc1) %>% 
  group_by(Gender)
############################Answer 1 Part Time
dftablePart <- df4Part%>% 
  count(Gender, IPEDS_Race_Desc, Student_Type_Desc1) %>% 
  group_by(Gender)


table(df4$Gender,df4$OverallFPT )



######################################################
#Distance Education Status 

df2$Course_Campus_Desc <- as.factor(df2$Course_Campus_Desc )
summary(df2$Course_Campus_Desc )

df2 <- df2 %>% 
  mutate(Distance = 
           ifelse(Course_Campus_Desc == "Distance Education", "Distance Education", "Onsite" ))

df2$Distance <- factor(df2$Distance, levels = c("Distance Education", "Onsite"))

dfDis <- df2 %>%
  group_by(Student_ID) %>%
  mutate(Distance_Education_Status=
           ifelse(all(Distance == "Onsite")==TRUE, "Onsite exclusively",
           ifelse(all(Distance=="Distance Education")==TRUE, "Distance exclusively",
           "Mixed ")))

#subset the dataframe to be easier to CHECK
dfDisCheck<-dfDis %>%
  select  (Student_ID, 
           Distance_Education_Status,
           Distance,
           Course_Campus_Desc )




#subset the dataframe to be easier to CHECK
dfDis<-dfDis %>%
  select  (Student_ID, 
           Gender,
           Student_Type_Code,
           Student_Type_Desc1 ,
           OverallFPT ,
           IPEDS_Race_Desc, 
           Degree_Seeking,
           Distance_Education_Status)


dfDis<- dfDis %>%
  distinct(Student_ID, .keep_all = TRUE)

dfDis <- mutate_if(dfDis, is.character, as.factor)
summary(dfDis)


######################################################
#Distance Education Answers
table(dfDis$Distance_Education_Status, dfDis$Degree_Seeking)


######################################################
#Credit hour activity

sum(df2$Credit_Hours)


######################################################
#Dual Enrollment

dualEnrollment <- df4 %>%
  filter(df4$Student_Type_Code == "P") 

table( dualEnrollment$IPEDS_Race_Desc, dualEnrollment$Gender)