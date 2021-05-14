# Savannah
education_equity <- read.csv(educationgap_csv.csv)

#Set Up!
library(tidyverse)
library(dplyr)

# library(sf)
setwd("/Users/savannahholmes/Desktop/GitHub/cvilleequity_readingsol")
# setwd("cvilleequity_readingsol")

educationgap <- read.csv("educationgap.csv")
school_allrace <- read.csv("school_allrace.csv")
school_blackwhite <- read.csv("school_blackwhite.csv")
division <- read.csv("division.csv")

---------------------------------
#Albemarle
div_alb <- division %>% 
  filter(Division.Number == '2') %>% 
  spread(Race,Pass.Rate)

#Charlottesville 
div_cville <- division %>% 
  filter(Division.Number == '104') %>% 
  spread(Race,Pass.Rate)

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

----------------------------
#Achievement Gap by Division

#Albemarle Grade 3 
division <- division %>% 
  transform(Race = as.character(Race))

alb_black_3 <- division %>%
  filter(Division.Number == 2, Test.Level == 'Grade 3') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

alb_white_3 <- division %>%
  filter(Division.Number == 2, Test.Level == 'Grade 3') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

alb_3 <- div_alb %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(alb_black_3) %>%
  left_join(alb_white_3) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Albemarle Grade 8
division <- division %>% 
  transform(Race = as.character(Race))

alb_black_8 <- division %>%
  filter(Division.Number == 2, Test.Level == 'Grade 8') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

alb_white_8 <- division %>%
  filter(Division.Number == 2, Test.Level == 'Grade 8') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

alb_8 <- div_alb %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(alb_black_8) %>%
  left_join(alb_white_8) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

-----------------------------
#Charlottesville 
  
#Charlottesville Grade 3 
division <- division %>% 
  transform(Race = as.character(Race))

cville_black_3 <- division %>%
  filter(Division.Number == 104, Test.Level == 'Grade 3') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

cville_white_3 <- division %>%
  filter(Division.Number == 104, Test.Level == 'Grade 3') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

cville_3 <- div_cville %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(cville_black_3) %>%
  left_join(cville_white_3) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Charlottesville Grade 8
division <- division %>% 
  transform(Race = as.character(Race))

cville_black_8 <- division %>%
  filter(Division.Number == 104, Test.Level == 'Grade 8') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

cville_white_8 <- division %>%
  filter(Division.Number == 104, Test.Level == 'Grade 8') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

cville_8 <- div_cville %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(cville_black_8) %>%
  left_join(cville_white_8) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

---------------------------
#Fluvanna 
  
#Fluvanna Grade 3 
division <- division %>% 
  transform(Race = as.character(Race))

fluv_black_3 <- division %>%
  filter(Division.Number == 32, Test.Level == 'Grade 3') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

fluv_white_3 <- division %>%
  filter(Division.Number == 32, Test.Level == 'Grade 3') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

fluv_3 <- div_fluv %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(fluv_black_3) %>%
  left_join(fluv_white_3) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Fluvanna Grade 8
division <- division %>% 
  transform(Race = as.character(Race))

fluv_black_8 <- division %>%
  filter(Division.Number == 32, Test.Level == 'Grade 8') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

fluv_white_8 <- division %>%
  filter(Division.Number == 32, Test.Level == 'Grade 8') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

fluv_8 <- div_fluv %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(fluv_black_8) %>%
  left_join(fluv_white_8) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

----------------------------
#Greene
  
#Greene Grade 3 
division <- division %>% 
  transform(Race = as.character(Race))

greene_black_3 <- division %>%
  filter(Division.Number == 39, Test.Level == 'Grade 3') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

greene_white_3 <- division %>%
  filter(Division.Number == 39, Test.Level == 'Grade 3') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

greene_3 <- div_greene %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(greene_black_3) %>%
  left_join(greene_white_3) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Greene Grade 8
division <- division %>% 
  transform(Race = as.character(Race))

greene_black_8 <- division %>%
  filter(Division.Number == 39, Test.Level == 'Grade 8') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

greene_white_8 <- division %>%
  filter(Division.Number == 39, Test.Level == 'Grade 8') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

greene_8 <- div_greene %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(greene_black_8) %>%
  left_join(greene_white_8) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

---------------------------
#Louisa
#Louisa Grade 3 
division <- division %>% 
  transform(Race = as.character(Race))

louisa_black_3 <- division %>%
  filter(Division.Number == 54, Test.Level == 'Grade 3') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

louisa_white_3 <- division %>%
  filter(Division.Number == 54, Test.Level == 'Grade 3') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

louisa_3 <- div_louisa %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(louisa_black_3) %>%
  left_join(louisa_white_3) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Louisa Grade 8
division <- division %>% 
  transform(Race = as.character(Race))

louisa_black_8 <- division %>%
  filter(Division.Number == 54, Test.Level == 'Grade 8') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

louisa_white_8 <- division %>%
  filter(Division.Number == 54, Test.Level == 'Grade 8') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

louisa_8 <- div_louisa %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(louisa_black_8) %>%
  left_join(louisa_white_8) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

--------------------------
#Nelson 
  
#Nelson Grade 3 
division <- division %>% 
  transform(Race = as.character(Race))

nelson_black_3 <- division %>%
  filter(Division.Number == 62, Test.Level == 'Grade 3') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

nelson_white_3 <- division %>%
  filter(Division.Number == 62, Test.Level == 'Grade 3') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

nelson_3 <- div_nelson %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(nelson_black_3) %>%
  left_join(nelson_white_3) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#Nelson Grade 8
division <- division %>% 
  transform(Race = as.character(Race))

nelson_black_8 <- division %>%
  filter(Division.Number == 62, Test.Level == 'Grade 8') %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

nelson_white_8 <- division %>%
  filter(Division.Number == 62, Test.Level == 'Grade 8') %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Test.Level, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

nelson_8 <- div_nelson %>%
  select(School.Year, -Test.Level) %>%
  distinct() %>%
  left_join(nelson_black_8) %>%
  left_join(nelson_white_8) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

