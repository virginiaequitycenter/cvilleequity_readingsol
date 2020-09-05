#########################
# Inputting & Modelling School Level Data

library(tidyverse)
library(RColorBrewer) 
library(grid)

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_readingsol")


# Read In Data ------------------------------------------------------------

school_race <-
  read_csv("data/school_data/black_students_all_schools.csv") %>% bind_rows(read_csv("data/school_data/white_students_all_schools.csv"))

names(school_race) <- tolower(str_replace_all(names(school_race), " ", "_"))

school_race <- school_race %>% 
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7)),
         pass_rate = as.numeric(pass_rate),
         total_count = as.numeric(total_count),
         race = recode(race, "Black, not of Hispanic origin" = "Black",
                       "White, not of Hispanic origin" = "White")) %>%
  mutate(school_name_label = school_name,
         school_name = tolower(str_replace_all(school_name, " ", "")))

# View(school_race)

school_all <- read_csv("data/school_data/all_students_all_schools.csv")
names(school_all) <- tolower(str_replace_all(names(school_all), " ", "_"))

school_all <- school_all %>% 
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7)),
         pass_rate = as.numeric(pass_rate),
         total_count = as.numeric(total_count)) %>%
  mutate(school_name_label = school_name,
         school_name = tolower(str_replace_all(school_name, " ", "")))

# Calculate Race Cohorts -------------------------------------------------------
# I define cohorts as the year (Fall) that a student enrolled in 3rd grade. 
# The ayear variable is the year (Spring) that the SOL test is administered. 

school_race_cohorts <-
  school_race %>%
  select(division_name, school_name, race,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name, school_name, race, ayear, grade) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, school_name, race, cohort, grade, ayear) %>%
  mutate(race_cohort = paste0(race,"_",school_name),
         race_school = paste0(race, "_", division_name),
         race_school_cohort = paste0(race, "_", school_name, "_", cohort)) %>%
  filter(division_name %in% c("Albemarle County", "Charlottesville City", "Fluvanna County", "Nelson County", "Orange County", "Greene County" ))

table(school_race_cohorts$school_name, school_race_cohorts$grade)
 
# Calculate Average Cohorts -----------------------------------------------

school_all_cohorts <-
  school_all %>%
  select(division_name, school_name,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name, school_name, ayear, grade) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, school_name, cohort, grade, ayear) %>%
  mutate(school_cohort = paste0( school_name, "_", cohort)) %>%
  filter(division_name %in% c("Albemarle County", "Charlottesville City", "Fluvanna County", "Nelson County", "Orange County", "Greene County" ))



# Example Just Cville -----------------------------------------------------
# The issue we have with school-level analyses is that students swap schools halfway through. 

cville_schools_race_cohorts <-
  school_race_cohorts %>%
  filter(division_name == "Charlottesville City") %>%
  filter(school_name != "charlottesvillehigh")

cville_schools_all_cohorts <-
  school_all_cohorts %>%
  filter(division_name == "Charlottesville City") %>%
  filter(school_name != "charlottesvillehigh")


table(cville_schools_all_cohorts$school_name, cville_schools_all_cohorts$grade)
table(cville_schools_race_cohorts$school_name, cville_schools_race_cohorts$grade)


ggplot(cville_schools_race_cohorts, aes(x = grade, y = pass, color = race)) +
  geom_point() +
  facet_grid(school_name ~ .)

scale_factor <- .90

school_averages <-
cville_schools_all_cohorts %>% 
  select(division_name, school_name, ayear, grade, total, pass_count, pass, cohort) %>%
 group_by(division_name, cohort, grade) %>%
  mutate(n = n()) %>%
#  filter(cohort == "2010") %>%
  arrange(division_name, cohort, grade, school_name) %>%
  group_by(division_name, cohort, grade) %>%
  mutate(school_id  = floor(1:n() - n()/2)) %>%
  mutate(pass_graph = (pass/100 - .5) *scale_factor + school_id)

race_averages <-
  cville_schools_race_cohorts %>% 
  select(division_name, school_name, ayear, grade, race, total, pass_count, pass, cohort) %>%
  group_by(division_name, cohort, race, grade) %>%
  mutate(n = n()) %>%
  #  filter(cohort == "2010") %>%
  arrange(division_name, cohort, race, grade, school_name) %>%
  group_by(division_name, cohort, race, grade) %>%
  mutate(school_id  = floor(1:n() - n()/2)) %>%
  mutate(pass_graph = (pass/100 - .5) *scale_factor + school_id,
         race_school = paste0(race, "_", school_name))

add_lines <-
school_averages %>%
  group_by(division_name, school_name, cohort) %>%
  summarize(id = mean(school_id), x = min(grade), xend = max(grade)) %>%
  mutate( `25` = (25/100 - .5)*scale_factor +  id,
         `50` = (50/100 - .5)*scale_factor +  id,
         `75` = (75/100 - .5)*scale_factor +  id,
         `90` = (90/100 - .5)*scale_factor +  id
         ) %>%
  gather(rowlabel, y, -c(division_name:xend) ) %>%
  mutate(rowlabel = paste0(rowlabel, "%"))

p <-
ggplot(school_averages, aes(x = grade, y = pass_graph, group = school_name)) +
  geom_point(size = .1) +
  geom_line(size = .1) +
  geom_point(data = race_averages, aes( color = race), size = .15) +
  geom_line(data = race_averages, aes( color = race, group = race_school), size = .1) +
  geom_segment(data = add_lines, aes(x = x, xend = xend, y = y, yend = y), inherit.aes = FALSE, linetype = "dashed", size = .1) +
  geom_text(data = add_lines , aes(x = x - .1, y = y, label = rowlabel ), inherit.aes = FALSE, hjust = 1, size  = .5, vjust = .5) +
  
facet_grid(cohort ~.) +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    #  panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
    #  axis.text.x = element_text(angle = 60)
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
    
  ) 
# p

jpeg(filename = "outputs/plots_sdp/school_race_cohorts.jpg", height = 80*72/1, width = 20*72/1, units = 'px', res = 300)

## The Plot ##
p   

# Include the annotations ##
grid.text(
  "@EquityCenterUVA",
  x = 0.99,
  y = 0.99,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  ),
  just = c("right", "top")
)

dev.off()




# Just Alb ----------------------------------------------------------------

# The issue we have with school-level analyses is that students swap schools halfway through. 

alb_schools_race_cohorts <-
  school_race_cohorts %>%
  filter(division_name == "Albemarle County") %>%
  filter(!school_name %in% c( "monticellohigh", "westernalbemarlehigh", "albemarlehigh "))

alb_schools_all_cohorts <-
  school_all_cohorts %>%
  filter(division_name == "Albemarle County") %>%
  filter(!school_name %in% c( "monticellohigh", "westernalbemarlehigh" ,"albemarlehigh "))


table(alb_schools_all_cohorts$school_name, alb_schools_all_cohorts$grade)
table(alb_schools_race_cohorts$school_name, alb_schools_race_cohorts$grade)


ggplot(alb_schools_race_cohorts, aes(x = grade, y = pass, color = race)) +
  geom_point() +
  facet_grid(school_name ~ .)

scale_factor <- .90

school_averages <-
  alb_schools_all_cohorts %>% 
  select(division_name, school_name, ayear, grade, total, pass_count, pass, cohort) %>%
  group_by(division_name, cohort, grade) %>%
  mutate(n = n()) %>%
  #  filter(cohort == "2010") %>%
  arrange(division_name, cohort, grade, school_name) %>%
  group_by(division_name, cohort, grade) %>%
  mutate(school_id  = floor(1:n() - n()/2)) %>%
  mutate(pass_graph = (pass/100 - .5) *scale_factor + school_id)

race_averages <-
  alb_schools_race_cohorts %>% 
  select(division_name, school_name, ayear, grade, race, total, pass_count, pass, cohort) %>%
  group_by(division_name, cohort, race, grade) %>%
  mutate(n = n()) %>%
  #  filter(cohort == "2010") %>%
  arrange(division_name, cohort, race, grade, school_name) %>%
  group_by(division_name, cohort, race, grade) %>%
  mutate(school_id  = floor(1:n() - n()/2)) %>%
  mutate(pass_graph = (pass/100 - .5) *scale_factor + school_id,
         race_school = paste0(race, "_", school_name))

add_lines <-
  school_averages %>%
  group_by(division_name, school_name, cohort) %>%
  summarize(id = mean(school_id), x = min(grade), xend = max(grade)) %>%
  mutate( `25` = (25/100 - .5)*scale_factor +  id,
          `50` = (50/100 - .5)*scale_factor +  id,
          `75` = (75/100 - .5)*scale_factor +  id,
          `90` = (90/100 - .5)*scale_factor +  id
  ) %>%
  gather(rowlabel, y, -c(division_name:xend) ) %>%
  mutate(rowlabel = paste0(rowlabel, "%"))

p <-
  ggplot(school_averages, aes(x = grade, y = pass_graph, group = school_name)) +
  geom_point(size = .1) +
  geom_line(size = .1) +
  geom_point(data = race_averages, aes( color = race), size = .15) +
  geom_line(data = race_averages, aes( color = race, group = race_school), size = .1) +
  geom_segment(data = add_lines, aes(x = x, xend = xend, y = y, yend = y), inherit.aes = FALSE, linetype = "dashed", size = .1) +
  geom_text(data = add_lines , aes(x = x - .1, y = y, label = rowlabel ), inherit.aes = FALSE, hjust = 1, size  = .5, vjust = .5) +
  
  facet_grid(cohort ~.) +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    #  panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
    #  axis.text.x = element_text(angle = 60)
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
    
  ) 
# p

jpeg(filename = "outputs/plots_sdp/school_race_cohorts.jpg", height = 80*72/1, width = 20*72/1, units = 'px', res = 300)

## The Plot ##
p   

# Include the annotations ##
grid.text(
  "@EquityCenterUVA",
  x = 0.99,
  y = 0.99,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  ),
  just = c("right", "top")
)

dev.off()












