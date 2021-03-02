## ---------------------------
## Script name: pulling_out_thoughts.R
##
## Author:Sam Powers
## Date Created: 2020-11-20
##
## ---------------------------
## Purpose of script: To start pulling together some summary thoughts on Albemarle county schools. 
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_readingsol")
## ---------------------------
## load up the packages we will need:

library(tidyverse)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:
advantage_diffs <- read_csv("data/disadvantaged_effect.csv") %>%
  rename(advantaged  = N, disadvantaged = Y, diff = Diff) %>%
  mutate(division_name = str_sub(group, 1, -8)) %>%
  select(-group) %>%
  gather(ses, pass_pct, -year, -diff, -region, -division_name)

adv_pops <- read_csv("data/division_data/cleaned_ses_sex.csv") %>%
  group_by(division_name, disadvantaged, ayear, grade) %>%
  summarize(total = sum(total)) %>%
  spread(disadvantaged, total) %>%
  rename(advantaged  = N, disadvantaged = Y, year = ayear) %>%
  gather(ses, total, -division_name, -year, -grade )
  

race_diffs <- read_csv("data/race_effect.csv") %>%
  rename(diff = Diff) %>%
  mutate(division_name = str_sub(group, 1, -8)) %>%
  select(-group) %>%
  gather(race, pass_pct, -year, -diff, -region, -division_name)

race_pops <- read_csv("data/division_data/cleaned_race_sex.csv") %>%
  group_by(division_name, race, ayear, grade) %>%
  summarize(total = sum(total)) %>%
  spread(race, total) %>%
  rename( year = ayear) %>%
  gather(race, total, -division_name, -year, -grade )

# data joins --------------------------------------------------------------


advantage_diffs %>%
  left_join(adv_pops) %>%
  mutate(num_pass = total*pass_pct) %>%
  arrange(division_name, year, grade, ses) %>%
  group_by(division_name, year, grade) %>%
  filter(ses == "disadvantaged") %>%
  mutate(to_parity = round(diff * total), 
         to_100 = round((1 - pass_pct)* total)) %>%
  filter(division_name == "Albemarle County", year == 2019)


race_diffs %>%
  left_join(race_pops) %>%
  mutate(num_pass = total*pass_pct) %>%
  arrange(division_name, year, grade, race) %>%
  group_by(division_name, year, grade) %>%
  filter(race == "Black") %>%
  mutate(to_parity = round(diff * total), 
         to_100 = round((1 - pass_pct)* total)) %>%
  filter(division_name == "Albemarle County", year == 2019)

