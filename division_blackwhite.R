# SOL reading pass rates: division metrics and cohort metrics
#   division level (school level in next phase)
#   Black-White students


# set up
library(tidyverse)
library(RColorBrewer)

setwd("cvilleequity_readingsol")

# read in data
division <- read_csv("division_blackwhite.csv")
names(division) <- tolower(str_replace(names(division), " ", "_"))

division <- division %>% 
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7)),
         pass_rate = as.numeric(pass_rate),
         total_count = as.numeric(total_count),
         race = recode(race, "Black, not of Hispanic origin" = "Black",
                       "White, not of Hispanic origin" = "White"))

# Examine by year
# division by year: Albemarle
division %>% filter(division_name == "Albemarle County") %>% 
ggplot(aes(x = ayear, y = pass_rate, color = race)) +
  geom_line() + 
  scale_x_continuous(name = "Year", breaks =seq(2006,2019,1)) +
  facet_wrap(~test_level) +
  theme(axis.text.x = element_text(angle = 90))
# 2013 appears to be year the pass threshold changes
# more variation across divisions in older grades?


# Make cohorts - wide
# make a function to do this 
cohortize <- function(df, gr, gp) {
  cols <- c("pass", "total")
  df <- df %>% 
    filter(grade == gr, race == gp) %>% 
    mutate(cohort = ayear) %>% 
    select(division_number, division_name, cohort, race, pass = pass_rate, total = total_count) %>% 
    rename_with(.fn = ~paste0(., gr), .cols = all_of(cols))
}

cohort8b <- cohortize(division, 8, "Black")
cohort7b <- cohortize(division, 7, "Black")
cohort6b <- cohortize(division, 6, "Black")
cohort5b <- cohortize(division, 5, "Black")
cohort4b <- cohortize(division, 4, "Black")
cohort3b <- cohortize(division, 3, "Black")

cohortb <- cohort3b %>% 
  full_join(cohort4b) %>% 
  full_join(cohort5b) %>% 
  full_join(cohort6b) %>% 
  full_join(cohort7b) %>% 
  full_join(cohort8b) %>% 
  arrange(division_name, cohort)

cohort8w <- cohortize(division, 8, "White")
cohort7w <- cohortize(division, 7, "White")
cohort6w <- cohortize(division, 6, "White")
cohort5w <- cohortize(division, 5, "White")
cohort4w <- cohortize(division, 4, "White")
cohort3w <- cohortize(division, 3, "White")

cohortw <- cohort3w %>% 
  full_join(cohort4w) %>% 
  full_join(cohort5w) %>% 
  full_join(cohort6w) %>% 
  full_join(cohort7w) %>% 
  full_join(cohort8w) %>% 
  arrange(division_name, cohort)

cohort <- rbind(cohortb, cohortw)


# heatmap version, better way
# make data long, and use ggplot
cohort_long <- cohort %>% 
  select(division_name, cohort, race, starts_with("pass")) %>% 
  pivot_longer(cols = starts_with("pass"), 
               names_to = "grade", names_prefix = "pass", names_transform = list(grade = as.integer),
               values_to = "passrate")

ggplot(filter(cohort_long, race == "Black"), aes(grade, cohort)) +
  geom_tile(aes(fill = passrate)) +
  geom_text(aes(label = passrate), size = 2) +
  scale_x_continuous(position = "top", breaks = seq(3,8,1)) +
  scale_y_continuous(breaks = seq(2005,2025,1)) +
  facet_wrap(~division_name) +
  labs(title = "3rd Grade Reading Pass Rates: Black Students") + 
  viridis::scale_fill_viridis() +
  theme_minimal()

ggplot(filter(cohort_long, race == "White"), aes(grade, cohort)) +
  geom_tile(aes(fill = passrate)) +
  geom_text(aes(label = passrate)) +
  scale_x_continuous(position = "top", breaks = seq(3,8,1)) +
  scale_y_continuous(breaks = seq(2005,2025,1)) +
  facet_wrap(~division_name) +
  labs(title = "3rd Grade Reading Pass Rates: White Students") + 
  viridis::scale_fill_viridis() +
  theme_minimal()

# line plot versions
ggplot(filter(cohort_long, division_name == "Albemarle County"), aes(x = grade, y = passrate, color = race)) +
  geom_line() + labs(title = "3rd Grade Pass Rates by Cohort, Albemarle County") + 
  facet_wrap(~cohort)

ggplot(filter(cohort_long, cohort == 2019), aes(x = grade, y = passrate, color = race)) +
  geom_line() + labs(title = "3rd Grade Pass Rates by Division, 2019 Cohort") +
  facet_wrap(~division_name)

# Save data for use
saveRDS(division, file = "division_blackwhite.rds")
saveRDS(cohort, file = "cohortdivision_blackwhite.rds")
