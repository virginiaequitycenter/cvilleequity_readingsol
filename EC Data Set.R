education_equity <- read.csv(educationgap_csv.csv)

#Set Up!
library(dplyr)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
setwd("/Users/savannahholmes/Desktop/GitHub/cvilleequity_readingsol")


#CSV
educationgap <- read.csv("educationgap.csv")
school_allrace <- read.csv("school_allrace.csv")
school_blackwhite <- read.csv("school_blackwhite.csv")
disadvantaged <- read.csv("disadvantaged.csv", header=FALSE)
foster <- read.csv("foster.csv")
homeless <- read.csv("homeless.csv")
migrant <- read.csv("migrant.csv")
disabled <- read.csv("disabled.csv")

------------------------------------------------------------------
#Pass Rates by State 

#State Data
state <- educationgap %>% 
  filter(Level == 'State') %>%
  select(-Division.Number,-Division.Name,-Test.Source)

------------------------------------------------------------------
#Pass Rates by Division 
write_csv(division,path = 'division.csv') 

  
#Albemarle
div_alb <- division %>% 
  filter(Division.Number == '2') %>% 
  spread(Race,Pass.Rate)


#Charlottesville 
div_cville <- division %>% 
  filter(Division.Number == '104') %>% 
  spread(Race,Pass.Rate) %>% 


#Fluvanna
div_fluv <- division %>% 
  filter(Division.Name == 'Fluvanna County') %>% 
  spread(Race, Pass.Rate)


#Greene
div_greene <- division %>% 
  filter(Division.Name == 'Greene County') %>% 
  spread(Race, Pass.Rate)


#Louisa
div_louisa <- division %>% 
  filter(Division.Name == 'Louisa County') %>% 
  spread(Race, Pass.Rate)


#Nelson
div_nelson <- division %>% 
  filter(Division.Name == 'Nelson County') %>% 
  spread(Race, Pass.Rate)


#Pass Rates by School
school_allrace <- school_allrace %>% 
  filter(Level == 'School') %>% 


#Albemarle
school_alb <- school_allrace %>% filter(Division.Number == '2')
school_alb_race <- school_blackwhite %>% 
  filter(Division.Number =='2') %>% 
  spread(Race,Pass.Rate)


#Charlottesville
school_cville <- school_allrace %>% filter(Division.Number == '104')
school_cville_race <- school_blackwhite %>% 
  filter(Division.Number == '104') %>% 
  spread(Race,Pass.Rate)


#Fluvanna
school_fluv <- school_allrace %>% filter(Division.Number == '32')
school_fluv_race <- school_blackwhite %>% 
  filter(Division.Number == '32') %>% 
  spread(Race,Pass.Rate)


#Greene
school_greene <- school_allrace %>% filter(Division.Number == '39')
school_greene_race <- school_blackwhite %>% 
  filter(Division.Number== '39') %>% 
  spread(Race,Pass.Rate)


#Louisa
school_louisa <- school_allrace %>% filter(Division.Number == '54')
school_louisa_race <- school_allrace %>% 
  filter(Division.Number == '54') %>% 
  spread(Race,Pass.Rate)


#Nelson
school_nelson <- school_allrace %>% filter(Division.Number == '62')
school_nelson_race <- school_blackwhite %>% 
  filter(Division.Number == '62') %>% 
  spread(Race,Pass.Rate)

------------------------------------------------------------------
#Achievement Gap by Division

#Albemarle 
division <- division %>% 
  transform(Race = as.character(Race))

div_alb_black <- division %>%
  filter(Division.Number == 2)
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_alb_white <- div_alb %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

div_alb <- div_alb %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(div_alb_black) %>%
  left_join(div_alb_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Charlottesville 
division <- division %>% 
  transform(Race = as.character(Race))
  
div_cville_black <- division %>%
  filter(Division.Number == 104) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_cville_white <- division %>%
  filter(Division.Number == 104) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

div_cville <- div_cville %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(div_cville_black) %>%
  left_join(div_cville_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Fluvanna 
division <- division %>% 
  transform(Race = as.character(Race))

div_fluv_black <- division %>%
  filter(Division.Number == 32) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_fluv_white <- division %>%
  filter(Division.Number == 32) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

div_fluv <- div_fluv %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(div_fluv_black) %>%
  left_join(div_fluv_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)


#Greene
division <- division %>% 
  transform(Race = as.character(Race))

div_greene_black <- division %>%
  filter(Division.Number == 39) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_greene_white <- division %>%
  filter(Division.Number == 39) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

div_greene <- div_greene %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(div_greene_black) %>%
  left_join(div_greene_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)


#Louisa
division <- division %>% 
  transform(Race = as.character(Race))

div_louisa_black <- division %>%
  filter(Division.Number == 54) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_louisa_white <- division %>%
  filter(Division.Number == 54) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

div_louisa <- div_louisa %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(div_louisa_black) %>%
  left_join(div_louisa_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Nelson
division <- division %>% 
  transform(Race = as.character(Race))

div_nelson_black <- division %>%
  filter(Division.Number == 62) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_nelson_white <- division %>%
  filter(Division.Number == 62) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

div_nelson <- div_nelson %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(div_nelson_black) %>%
  left_join(div_nelson_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

------------------------------------------------------------------
#Achievement Gap by School in Division 

#Albemarle 
school_blackwhite <- school_blackwhite %>% 
  transform(Race = as.character(Race))

school_alb_black <- school_blackwhite %>%
  filter(Division.Number == 2) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

school_alb_white <- school_blackwhite %>%
  filter(Division.Number == 2) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

school_alb <- school_blackwhite %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(school_alb_black) %>%
  left_join(school_alb_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)),
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Charlottesville 
school_blackwhite <- school_blackwhite %>% 
  transform(Race = as.character(Race))

school_cville_black <- school_blackwhite %>%
  filter(Division.Number == 104) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

school_cville_white <- school_blackwhite %>%
  filter(Division.Number == 104) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

school_cville <- school_blackwhite %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(school_cville_black) %>%
  left_join(school_cville_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)),
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Fluvanna 
school_blackwhite <- school_blackwhite %>% 
  transform(Race = as.character(Race))

school_fluv_black <- school_blackwhite %>%
  filter(Division.Number == 32) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

school_fluv_white <- school_blackwhite %>%
  filter(Division.Number == 32) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

school_fluv <- school_blackwhite %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(school_fluv_black) %>%
  left_join(school_fluv_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)),
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Greene 
school_blackwhite <- school_blackwhite %>% 
  transform(Race = as.character(Race))

school_greene_black <- school_blackwhite %>%
  filter(Division.Number == 39) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

school_greene_white <- school_blackwhite %>%
  filter(Division.Number == 39) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

school_greene <- school_blackwhite %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(school_greene_black) %>%
  left_join(school_greene_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)),
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Louisa
school_blackwhite <- school_blackwhite %>% 
  transform(Race = as.character(Race))

school_louisa_black <- school_blackwhite %>%
  filter(Division.Number == 54) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

school_louisa_white <- school_blackwhite %>%
  filter(Division.Number == 54) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

school_louisa <- school_blackwhite %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(school_louisa_black) %>%
  left_join(school_louisa_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)),
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Nelson 
school_blackwhite <- school_blackwhite %>% 
  transform(Race = as.character(Race))

school_nelson_black <- school_blackwhite %>%
  filter(Division.Number == 62) %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

school_nelson_white <- school_blackwhite %>%
  filter(Division.Number == 62) %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, School.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

school_nelson <- school_blackwhite %>%
  select(School.Year, Test.Level) %>%
  distinct() %>%
  left_join(school_nelson_black) %>%
  left_join(school_nelson_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)),
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)
------------------------------------------------------------------
#Cohort Analysis 



------------------------------------------------------------------
#School Comparison  






