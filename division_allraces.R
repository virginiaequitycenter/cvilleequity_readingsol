# SOL reading pass rates: division metrics and cohort metrics
#   division level (school level in next phase)
#   total students (Black-white gap in next phase)


# set up
library(tidyverse)
library(RColorBrewer) 

setwd("cvilleequity_readingsol")

# read in data
division <- read_csv("dvision_allraces.csv")
names(division) <- tolower(str_replace(names(division), " ", "_"))

division <- division %>% 
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7)))

# Examine by year
# division by year
ggplot(division, aes(x = ayear, y = pass_rate, color = division_name)) +
  geom_line(aes(size = total_count), alpha = 1/3) + 
  scale_x_continuous(name = "Year", breaks =seq(2006,2019,1)) +
  facet_wrap(~test_level) +
  theme(axis.text.x = element_text(angle = 90))
# 2013 appears to be year the pass threshold changes
# more variation across divisions in older grades?

# grade by year
ggplot(division, aes(x = ayear, y = pass_rate, color = test_level)) +
  geom_line() + 
  scale_x_continuous(name = "Year", breaks =seq(2006,2019,1)) +
  facet_wrap(~division_name) +
  theme(axis.text.x = element_text(angle = 90))
# more variation across grades in some divisions than others


# Make cohorts - wide
# there is definitely a more elegant way to do this, but for expediency's sake...
cohort8 <- division %>% 
  filter(grade == 8) %>% 
  mutate(cohort = ayear) %>% 
  select(division_number, division_name, cohort, pass8 = pass_rate, total8 = total_count)

cohort7 <- division %>% 
  filter(grade == 7) %>% 
  mutate(cohort = ayear + 1) %>% 
  select(division_number, division_name, cohort, pass7 = pass_rate, total7 = total_count)

cohort6 <- division %>% 
  filter(grade == 6) %>% 
  mutate(cohort = ayear + 2) %>% 
  select(division_number, division_name, cohort, pass6 = pass_rate, total6 = total_count)

cohort5 <- division %>% 
  filter(grade == 5) %>% 
  mutate(cohort = ayear + 3) %>% 
  select(division_number, division_name, cohort, pass5 = pass_rate, total5 = total_count)

cohort4 <- division %>% 
  filter(grade == 4) %>% 
  mutate(cohort = ayear + 4) %>% 
  select(division_number, division_name, cohort, pass4 = pass_rate, total4 = total_count)

cohort3 <- division %>% 
  filter(grade == 3) %>% 
  mutate(cohort = ayear + 5) %>% 
  select(division_number, division_name, cohort, pass3 = pass_rate, total3 = total_count)

cohort <- cohort3 %>% 
  full_join(cohort4) %>% 
  full_join(cohort5) %>% 
  full_join(cohort6) %>% 
  full_join(cohort7) %>% 
  full_join(cohort8) %>% 
  arrange(division_name, cohort)

# heatmap version (using heatmap library)
# requires a matrix
cohort_matrix <- as.data.frame(cohort) %>% 
  filter(division_name == "Albemarle County") %>% 
    column_to_rownames("cohort") %>% 
  select(starts_with("pass")) %>% 
  as.matrix()

heatmap(cohort_matrix, Colv = NA, Rowv = NA, scale = "column", col = colorRampPalette(brewer.pal(5, "PuOr"))(10))

# heatmap version, better way
# make data long, and use ggplot
cohort_long <- cohort %>% 
  select(division_name, cohort, starts_with("pass")) %>% 
  pivot_longer(cols = starts_with("pass"), 
               names_to = "grade", names_prefix = "pass", names_transform = list(grade = as.integer),
               values_to = "passrate")

ggplot(cohort_long, aes(grade, cohort)) +
  geom_tile(aes(fill = passrate)) +
  geom_text(aes(label = passrate)) +
  scale_x_continuous(position = "top", breaks = seq(3,8,1)) +
  scale_y_continuous(breaks = seq(2005,2025,1)) +
  facet_wrap(~division_name) +
  viridis::scale_fill_viridis() +
  theme_minimal()

# line plot versions
ggplot(cohort_long, aes(x = grade, y = passrate, color = division_name)) +
  geom_line() + facet_wrap(~cohort)

ggplot(cohort_long, aes(x = grade, y = passrate, color = as.factor(cohort))) +
  geom_line() + facet_wrap(~division_name)
