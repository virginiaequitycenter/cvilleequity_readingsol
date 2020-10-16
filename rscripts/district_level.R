#########################
# Inputting & Modelling School Level Data

library(tidyverse)
library(RColorBrewer)
library(grid)
library(lme4)
library(margins)

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_readingsol")


# Read In Data ------------------------------------------------------------

division_race <-
  read_csv("data/division_data/black_students_all_divisions.csv") %>% bind_rows(read_csv("data/division_data/white_students_all_divisions.csv"))

names(division_race) <- tolower(str_replace_all(names(division_race), " ", "_"))

division_race <-division_race %>%
  mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    race = recode(as.factor(race),
                  `Black, not of Hispanic origin` = "Black",
                  `White, not of Hispanic origin` = "White")
  ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric))

division_all <- read_csv("data/division_data/all_students_all_divisions.csv")
names(division_all) <- tolower(str_replace_all(names(division_all), " ", "_"))

division_all <- division_all %>%
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7))) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric))

# Calculate Race Cohorts -------------------------------------------------------
# I define cohorts as the year (Fall) that a student enrolled in 3rd grade.
# The ayear variable is the year (Spring) that the SOL test is administered.

division_race_cohorts <-
  division_race %>%
  select(division_name, race,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name,  race, ayear, grade) %>%
  mutate(min_grade = min(grade),
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, race, cohort, grade, ayear) %>%
  mutate(race_cohort = paste0(race,"_",cohort),
         race_division = paste0(race, "_", division_name),
         race_division_cohort = paste0(race, "_", division_name, "_", cohort))  # %>%
 # filter(division_name %in% c("Albemarle County", "Charlottesville City", "Fluvanna County", "Nelson County", "Orange County", "Greene County" ))

missing_counties <-
  division_race_cohorts %>%
  mutate( available = case_when(is.na(as.numeric(pass_count)) == TRUE ~ 0,
                                TRUE ~ 1)
  ) %>%
  group_by(division_name, race, cohort) %>%
  summarize(pct = round(mean(available)*100,2), num = sum(available)) %>%
  filter(is.na(pct) | pct < 100) %>%
  select(-num) %>%
  spread(cohort, pct)

unique(missing_counties$division_name)

# Make Race - Sex Cohorts if possible -------------------------------------

division_race_sex <-
  read_csv("data/division_data/black_students_by_sex_all_divisions.csv") %>% bind_rows(read_csv("data/division_data/white_students_by_sex_all_divisions.csv"))


names(division_race_sex) <- tolower(str_replace_all(names(division_race_sex), " ", "_"))

division_race_sex <- division_race_sex %>%
  mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    race = recode(as.factor(race),
                  `Black, not of Hispanic origin` = "Black",
                  `White, not of Hispanic origin` = "White")
  ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric)) %>%
  rename(sex= gender)

division_race_sex_cohorts <-
  division_race_sex %>%
  select(division_name, race, sex,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name,  race, sex, ayear, grade) %>%
  mutate(min_grade = min(grade),
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, race, sex, cohort, grade, ayear) %>%
  mutate(race_cohort = paste0(race,"_",cohort),
         race_division = paste0(race, "_", division_name),
         race_division_cohort = paste0(race, "_", division_name, "_", cohort))# %>%
 # filter(division_name %in% c("Albemarle County", "Charlottesville City", "Fluvanna County", "Nelson County", "Orange County", "Greene County" ))

division_race_sex_cohorts %>%
  mutate( available = case_when(is.na(as.numeric(pass_count)) == TRUE ~ 0,
                    TRUE ~ 1)
          ) %>%
  group_by(division_name, race, sex, cohort) %>%
  summarize(pct = round(mean(available)*100,2), num = sum(available)) %>%
  mutate(display = paste0(pct, " (", num, ")") ) %>%
  select(-pct, -num) %>%
  spread(cohort, display)  %>% View()

division_race_sex_cohorts %>%
  filter(division_name == "Fairfax County") %>% View()



missing_counties <-
division_race_sex_cohorts %>%
  mutate( available = case_when(is.na(as.numeric(pass_count)) == TRUE ~ 0,
                                TRUE ~ 1)
  ) %>%
  group_by(division_name, race, sex, cohort) %>%
  summarize(pct = round(mean(available)*100,2), num = sum(available)) %>%
  filter(is.na(pct) | pct < 100) %>%
  select(-num) %>%
  spread(cohort, pct)


unique(missing_counties$division_name)
View(missing_counties)



# Cville & Albemarle have full data. Fluvana, Greene, Nelson, & Orange have some spottier data




# Create Individual Level Data --------------------------------------------

df <-    division_race_sex_cohorts %>%
  select(division_name, race, sex, grade, total, pass_pct = pass, ayear, cohort, num_pass  = pass_count) %>%
  mutate(num_pass = as.numeric(num_pass))

df %>%
  filter(is.na(num_pass)) %>%
  nrow()

df %>%
  mutate(missing = case_when(
    is.na(num_pass) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(ayear, race, sex) %>%
  summarize(missing = mean(missing)) %>%
  spread(ayear, missing)

df %>%
  filter(is.na(num_pass)) %>%
  pull(division_name) %>%
  unique()



df[is.na(df)] <- 0
df$total[df$num_pass == 0] <- 0

sim_data <-
  df %>%
  uncount(total) %>%
  group_by(division_name, race, sex, grade, pass_pct, ayear, cohort, num_pass) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= num_pass ~ 1,
           TRUE ~ 0
         )
  )  %>%
  mutate(post_2012 = case_when(
    ayear > 2012 ~ "yes",
    TRUE ~ "no"
  ))

View(sim_data)

write_csv(sim_data, "../data/sim_data.csv")

# What am I trying to estimate? The effect of race on passing the reading test

sim_data
model1 <- glmer(pass ~ race + (1|division_name),  family = binomial("logit"), data = sim_data)
summary(model1)


# Random effect of race by division
model2 <- glmer(pass ~ race  +  (1 + race|division_name),
                family = binomial("logit"),
                data = sim_data[sample(1:nrow(sim_data), 1000000 ), ])
summary(model2)



# We want to think about division level effects as well to reduce the variance of the intercept
# Free & Reduced Lunch population
# % Black



# Look at individual Data just among race ---------------------------------
df <-    division_race_cohorts %>%
  select(division_name, race, grade, total, pass_pct = pass, ayear, cohort, num_pass  = pass_count) %>%
  mutate(num_pass = as.numeric(num_pass))

missing <-
df %>%
  filter(is.na(num_pass)) %>%
  nrow()

missing/
nrow(df)


df %>%
  mutate(missing = case_when(
    is.na(num_pass) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(ayear, race) %>%
  summarize(missing = mean(missing)) %>%
  spread(ayear, missing)

df %>%
  filter(is.na(num_pass)) %>%
  pull(division_name) %>%
  unique()
