
# 1. Load libraries and data ----
library(tidyverse)
library(lme4)
library(ggeffects)


division_hispanic_white_sex <- read_csv("../data/division_data/white_students_by_sex_all_divisions.csv") %>%
bind_rows( 
  read_csv("../data/division_data/hispanic_students_by_sex_all_divisions.csv")
)


# Clean Data --------------------------------------------------------------

names(division_hispanic_white_sex) <- tolower(str_replace_all(names(division_hispanic_white_sex), " ", "_"))

division_hispanic_white_sex <- division_hispanic_white_sex %>%
  mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    race = recode(as.factor(race),
                  `Hispanic` = "Hispanic",
                  `White, not of Hispanic origin` = "White")
  ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric)) %>%
  rename(sex= gender)

division_hispanic_white_sex_cohorts <-
  division_hispanic_white_sex %>%
  select(division_name, race, sex,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name,  race, sex, ayear, grade) %>%
  mutate(min_grade = min(grade),
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, race, sex, cohort, grade, ayear) %>%
  mutate(ses_cohort = paste0(race,"_",cohort),
         ses_division = paste0(race, "_", division_name),
         ses_division_cohort = paste0(race, "_", division_name, "_", cohort))


df <-    division_hispanic_white_sex_cohorts %>%
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

write_csv(sim_data, path = "../data/sim_hispanic_white_division.csv")



# Check -------------------------------------------------------------------
# to see if things would be different without sex
division_hispanic_white_sex_nosex <- read_csv("../data/division_data/white_students_all_divisions.csv") %>%
  bind_rows( 
    read_csv("../data/division_data/hispanic_students_all_divisions.csv")
  )

names(division_hispanic_white_sex_nosex) <- tolower(str_replace_all(names(division_hispanic_white_sex_nosex), " ", "_"))

division_hispanic_white_sex_nosex <- division_hispanic_white_sex_nosex %>%
  mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    race = recode(as.factor(race),
                  `Hispanic` = "Hispanic",
                  `White, not of Hispanic origin` = "White")
  ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric)) 

division_hispanic_white_sex_nosex_cohorts <-
  division_hispanic_white_sex_nosex %>%
  select(division_name, race,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name,  race, ayear, grade) %>%
  mutate(min_grade = min(grade),
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, race, cohort, grade, ayear) %>%
  mutate(ses_cohort = paste0(race,"_",cohort),
         ses_division = paste0(race, "_", division_name),
         ses_division_cohort = paste0(race, "_", division_name, "_", cohort))


df_ns <-    division_hispanic_white_sex_nosex_cohorts %>%
  select(division_name, race, grade, total, pass_pct = pass, ayear, cohort, num_pass  = pass_count) %>%
  mutate(num_pass = as.numeric(num_pass))

df_ns %>%
  filter(is.na(num_pass)) %>%
  nrow()

df_ns %>%
  mutate(missing = case_when(
    is.na(num_pass) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(ayear, race) %>%
  summarize(missing = mean(missing)) %>%
  spread(ayear, missing)

df %>%
  mutate(missing = case_when(
    is.na(num_pass) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(ayear, race, sex) %>%
  summarize(missing = mean(missing)) %>%
  spread(ayear, missing)
