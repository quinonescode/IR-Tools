


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

Grade_22 <- read_excel("H:/Research/NCCBP/Grades/Grade-2240.xlsx")
names(Grade_22)

Grade_22$grade <- substr(Grade_22$SHRTCKG_GRDE_CODE_FINAL, 1,1)

Grade_22$grade <- as.factor(Grade_22$grade)
summary(Grade_22$grade)

#Within one subsequent academic year, i.e., through Fall 2023.
Grade_2250 <- read_excel("H:/Research/NCCBP/Grades/Grade-2250.xlsx")
Grade_2310 <- read_excel("H:/Research/NCCBP/Grades/Grade-2310.xlsx")
Grade_2320 <- read_excel("H:/Research/NCCBP/Grades/Grade-2320.xlsx")
Grade_2340 <- read_excel("H:/Research/NCCBP/Grades/Grade-2340.xlsx")
  
Grade_year <- rbind(Grade_2250, Grade_2310, Grade_2320, Grade_2340 )
  
###################################### MATH #####################################################################


DevCo <- Grade_22 %>%
  filter(SHRTCKN_CRSE_NUMB <= 100 & SHRTCKN_SUBJ_CODE == "MAT" ) 


#Total A, B, C and P Grades in Fall 2022, Highest-level Developmental Courses          
Pass <- DevCo %>%
  filter( grade == "A" | grade == "B"| grade == "C" | grade == "R")

df <- Grade_year %>% 
  filter(SHRTCKN_CRSE_NUMB >= 100 & SHRTCKN_SUBJ_CODE == "MAT" )

answer <- sqldf("
      SELECT 
      a.ID,
      a.SHRTCKN_SUBJ_CODE AS fall_sub,          
      a.SHRTCKN_CRSE_NUMB AS fall_num,
      a.SHRTCKG_GRDE_CODE_FINAL AS fall_gra,
      a.grade,
      b.SHRTCKN_SUBJ_CODE AS spr_sub,
      b.SHRTCKN_CRSE_NUMB AS spr_num,
      b.SHRTCKG_GRDE_CODE_FINAL AS spr_gra
      FROM Pass a
      INNER JOIN df b
      ON a.ID = b.ID;
      ")

#Total from Row 1 Who Enrolled in Related College-level Courses
length(unique(answer$ID))

Companswer <- answer%>%
  filter(spr_gra == "A" | spr_gra == "B"| spr_gra == "C" | 
           spr_gra == "R"| spr_gra == "D" | spr_gra == "F")

#Total from Row 2 Who Completed College-level Courses with ABCPD and F Grades
length(unique(Companswer$ID))


Passanswer <- answer %>% 
  filter(spr_gra == "A" | spr_gra == "B"| spr_gra == "C" | spr_gra == "R")

#Total from Row 3 Who Completed College-level Courses with A, B,C and P Grades
length(unique(Passanswer$ID))

###################### WRITING #####################################################################################


DevCo <- Grade_22 %>%
  filter(SHRTCKN_CRSE_NUMB <= 100 & SHRTCKN_SUBJ_CODE == "ENG" ) 


#Total A, B, C and P Grades in Fall 2022, Highest-level Developmental Courses          
Pass <- DevCo %>%
  filter( grade == "A" | grade == "B"| grade == "C" | grade == "R")

df <- Grade_year %>% 
  filter(SHRTCKN_CRSE_NUMB >= 100 & SHRTCKN_SUBJ_CODE == "ENG" )

answer <- sqldf("
      SELECT 
      a.ID,
      a.SHRTCKN_SUBJ_CODE AS fall_sub,          
      a.SHRTCKN_CRSE_NUMB AS fall_num,
      a.SHRTCKG_GRDE_CODE_FINAL AS fall_gra,
      a.grade,
      b.SHRTCKN_SUBJ_CODE AS spr_sub,
      b.SHRTCKN_CRSE_NUMB AS spr_num,
      b.SHRTCKG_GRDE_CODE_FINAL AS spr_gra
      FROM Pass a
      INNER JOIN df b
      ON a.ID = b.ID;
      ")

#Total from Row 1 Who Enrolled in Related College-level Courses
length(unique(answer$ID))

Companswer <- answer%>%
  filter(spr_gra == "A" | spr_gra == "B"| spr_gra == "C" | 
           spr_gra == "R"| spr_gra == "D" | spr_gra == "F")

#Total from Row 2 Who Completed College-level Courses with ABCPD and F Grades
length(unique(Companswer$ID))


Passanswer <- answer %>% 
  filter(spr_gra == "A" | spr_gra == "B"| spr_gra == "C" | spr_gra == "R")

#Total from Row 3 Who Completed College-level Courses with A, B,C and P Grades
length(unique(Passanswer$ID))



