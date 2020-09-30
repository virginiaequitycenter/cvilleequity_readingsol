########################
# Inputting & Modelling School Level Data

library(tidyverse)
library(RColorBrewer) 


setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_readingsol")


# Read In Data ------------------------------------------------------------

division_race <-
  read_csv("data/division_data/black_students_all_divisions.csv") %>% bind_rows(read_csv("data/division_data/white_students_all_divisions.csv")) #%>%
#  filter(`Division Name` %in% c("Albemarle County", "Charlottesville City", "Fluvanna County", "Nelson County", "Orange County", "Greene County" ))


names(division_race) <- tolower(str_replace_all(names(division_race), " ", "_"))
str(division_race)


division_race <- division_race %>% 
  mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    race = recode(as.factor(race), 
                  `Black, not of Hispanic origin` = "Black",
                  `White, not of Hispanic origin` = "White")
    ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric) ) 
  

division_all <- read_csv("data/division_data/all_students_all_divisions.csv")# %>%
#  filter(`Division Name` %in% c("Albemarle County", "Charlottesville City", "Fluvanna County", "Nelson County", "Orange County", "Greene County" ))

names(division_all) <- tolower(str_replace_all(names(division_all), " ", "_"))

division_all <- division_all %>% 
   mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)) 
    ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric) ) 


names(division_all)
names(division_race)
# Calculate Race Cohorts -------------------------------------------------------
# I define cohorts as the year (Fall) that a student enrolled in 3rd grade. 
# The ayear variable is the year (Spring) that the SOL test is administered. 

division_race_cohorts <-
  division_race %>%
  select(division_name, race,  ayear, grade, total_count, pass_count, pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name,  race, ayear, grade) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) 


# Calculate Average Cohorts -----------------------------------------------

division_all_cohorts <-
  division_all %>%
  select(division_name,  ayear, grade, total_count, pass_count,  pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name, ayear, grade) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, cohort, grade, ayear) %>%
  mutate(
    race = "All")

names(division_race_cohorts)
names(division_all_cohorts)


complete_cohorts <- 
bind_rows(division_all_cohorts, division_race_cohorts) %>%
  filter(cohort > 2004, cohort < 2014) %>%
#  group_by(division_name, cohort) %>%
#  mutate(number = n()) %>%
#  filter(number > 15) %>%
#  select(-number) %>%
  arrange(cohort, division_name) %>%
  left_join(
    division_all_cohorts %>%
      select(division_name, ayear, grade, cohort, average = pass_rate)
    ) %>% 
  left_join(
    division_race_cohorts %>%
      filter(race == "Black") %>%
      select(division_name, ayear, grade, cohort, black = pass_rate) 
    )%>%
  mutate(
    black = case_when(
    is.na(black) ~ average,
    TRUE ~ black
  ) 
  )%>%
  mutate(division_use = tolower(str_replace_all(division_name, " ", "_"))) #%>%
  # filter(!is.na(pass_rate))

  
write_csv(complete_cohorts, "../vaequity-reading/assets/data/educ_equity.csv")

names(complete_cohorts)

