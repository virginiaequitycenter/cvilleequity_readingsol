library(tidyverse)
library(RColorBrewer) 

# read in data
division <- read_csv("data/division_blackwhite.csv")
names(division) <- tolower(str_replace(names(division), " ", "_"))

division <- division %>% 
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7)),
         pass_rate = as.numeric(pass_rate),
         total_count = as.numeric(total_count),
         race = recode(race, "Black, not of Hispanic origin" = "Black",
                       "White, not of Hispanic origin" = "White"))


division_allraces <- read_csv("data/dvision_allraces.csv")
names(division_allraces) <- tolower(str_replace(names(division_allraces), " ", "_"))

division_allraces <- division_allraces %>% 
  mutate(ayear = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7)))


# Calculate Race Cohorts -------------------------------------------------------
# This dataset puts the in wide format so they are easy to see. 

view_cohorts<-
division %>%
  select(division_name, race, grade, total = total_count, pass = pass_rate, ayear ) %>%
  arrange(division_name, race, grade, ayear) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start) %>%           # Their cohort year is the year they ended 3rd grade
  arrange(division_name, race, cohort, grade, ayear) %>%
  select(-ayear, -total, -years_from_start) %>%
  spread(grade, pass ) %>%
  arrange(division_name, race, cohort)

View(view_cohorts)

race_cohorts <-
division %>%
  select(division_name, race, grade, total = total_count, pass = pass_rate, ayear ) %>%
  arrange(division_name, race, grade, ayear) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start) %>%
  arrange(division_name, race, cohort, grade, ayear) %>%
  mutate(race_cohort = paste0(race,"_",cohort),
         race_division = paste0(race, "_", division_name),
         race_division_cohort = paste0(race, "_", division_name, "_", cohort))


ggplot(race_cohorts, aes(x = grade, y = pass, color = race, group = race_cohort)) +
  geom_line() +
  facet_wrap(~division_name) +
  ggtitle("Cohort Progression By Division")


ggplot(race_cohorts, aes(x = grade, y = pass, color = race, group = race_division)) +
  geom_line() +
  facet_wrap(~cohort) +
  ggtitle("Division Cohort Progression by Year Took 3rd Grade Test")



# Calculate Average Cohorts -----------------------------------------------

division_allraces %>%
  select(division_name,  grade, total = total_count, pass = pass_rate, ayear ) %>%
  arrange(division_name, grade, ayear) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start) %>%           # Their cohort year is the year they ended 3rd grade
  arrange(division_name, cohort, grade, ayear) %>%
  select(-ayear, -total, -years_from_start) %>%
  spread(grade, pass ) %>%
  arrange(division_name,  cohort)

View(view_cohorts)

average_cohorts <-
  division_allraces %>%
  select(division_name,  grade, overall_total = total_count, avg_pass = pass_rate, ayear ) %>%
  arrange(division_name, grade, ayear) %>%
  mutate(min_grade = min(grade), 
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start) %>%           # Their cohort year is the year they ended 3rd grade
  arrange(division_name, cohort, grade, ayear)  %>%
  mutate(division_cohort = paste0( division_name, "_", cohort))

# Maybe a mean-adjusted Gap? ---------------------------------------------------------
cohorts <- 
  race_cohorts  %>%
  left_join(average_cohorts) %>%
  mutate(diff_pass = pass - avg_pass) %>%
  mutate(
         cohort_label = paste0("3rd in ", paste0( "'", substr(cohort -1, 3,4), "-'", substr(cohort, 3,4)))
         )

# View(cohorts)

p <-
ggplot(data = cohorts, 
       aes(x = grade, y = diff_pass,  group = race_division_cohort) ) +
  geom_point(aes( color = race)) +
  geom_line(aes(color = race), size = .1) + 
  geom_line(aes(x = grade, y = 0), size = .1, color = "black", inherit.aes= FALSE) + 
  geom_area( aes(fill=race), alpha=0.2) +
 # scale_y_continuous(limits = c(2000.2, 2019.5), breaks = seq(2001, 2019, 1) ) +
  scale_x_continuous(limits = c(2.5, 8.75), breaks = seq(3, 8, 1) ) +
  facet_grid(cohort_label ~division_name) +
#  theme_void() +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
  #  panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
  #  axis.text.x = element_text(angle = 60)
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank()
  
  ) +
  geom_line(data = cohorts, aes(x = grade, y =  - 10, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  geom_line(data = cohorts, aes(x = grade, y =   10, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  
  geom_line(data = cohorts, aes(x = grade, y =   -20, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = -10, label = "-10%") , 
            size = 2, 
            hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = 10, label = "+10%") , size = 2, hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = -20, label = "-20%") , size = 2, hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .25, y = 0, label = "0") , size = 2, hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% filter(race == "White") %>% mutate(label = paste0( "'",substr(ayear, 3,4))), aes(x = grade, y = -.15, label = label) , size = 1.5, hjust = .5, vjust = 1,
            inherit.aes= FALSE
  ) +
  
  geom_text(data = cohorts %>% filter(race == "White") %>% mutate(label = paste0( grade, "th")), aes(x = grade, y = -31, label = label) , size = 1.5, hjust = .5, vjust = 1,
            inherit.aes= FALSE
  ) +
  
  geom_segment(data = cohorts %>% filter(race == "White"), aes(x = grade, xend = grade, y = -30, yend = -28), size = .2,
            inherit.aes = FALSE
  ) + coord_cartesian(clip = "off") +
  ggtitle("Differences in SOL Reading Passage Rates Among Black & White Youth")

jpeg(filename = "outputs/plots_sdp/area_plot.jpg", height = 82*72/1, width = 40*72/1, units = 'px', res = 300)

## The Plot ##
p   

## Include the annotations ##
# grid.text(
#   "Abbreviations: PA- Prior Authorization; ETE- Ending the HIV Epidemic; TDF/FTC- tenofovir disoproxil fumarate/emtricitabine; TAF/FTC- tenofovir alafenamide/emtricitabine",
#   x = 0.99,
#   y = 0.01,
#   gp = gpar(
#     fontsize = 10,
#     fontface = 3
#   ),
#   just = c("right", "bottom")
# )

dev.off()



# Maybe a Smoothed Gap? ---------------------------------------------------------
cohorts <- 
  race_cohorts  %>%
  left_join(average_cohorts) %>%
  mutate(diff_pass = pass - avg_pass)

View(cohorts)

p <-
  ggplot(data = filter(cohorts, 
                       cohort > 2000
                       #  cohort > 2005, 
                       #  cohort < 2015
  ), 
  aes(x = grade, y = diff_pass) ) +
  geom_point(aes( color = race)) +
  geom_line(aes(group = race_division_cohort ), size = .01)+
  stat_smooth(
    aes(fill = race),
    geom = 'area', method = 'loess', span = 1/3,
    alpha = 1/2 ) + 
  facet_wrap(~division_name)  +
  theme(
    legend.position = "none",
    panel.background = element_blank()
  )

jpeg(filename = "outputs/plots_sdp/snoothed_area_plot.jpg", height = 40*72/1, width = 40*72/1, units = 'px', res = 300)

## The Plot ##
p   

## Include the annotations ##
# grid.text(
#   "Abbreviations: PA- Prior Authorization; ETE- Ending the HIV Epidemic; TDF/FTC- tenofovir disoproxil fumarate/emtricitabine; TAF/FTC- tenofovir alafenamide/emtricitabine",
#   x = 0.99,
#   y = 0.01,
#   gp = gpar(
#     fontsize = 10,
#     fontface = 3
#   ),
#   just = c("right", "bottom")
# )

dev.off()



# What if the gap were not based on 0 -------------------------------------

cohorts <- 
  race_cohorts  %>%
  left_join(average_cohorts) %>%
  mutate(diff_pass = pass - avg_pass,
         pass_2 = case_when(
           is.na(pass) ~ avg_pass,
        #   pass == 0 ~ avg_pass,
           TRUE ~ pass
         ),
         grade_2 = case_when(
           is.na(pass) ~ grade,
           TRUE ~ grade
         )
  )

# View(cohorts)

p <-
  ggplot(data = filter(cohorts, 
                       cohort > 2000
                       #  cohort > 2005, 
                       #  cohort < 2015
  ), 
  aes(x = grade) ) +
  geom_point(aes(color = race, y = pass)) +
  geom_line(aes(color = race, y = pass,  group = race_division_cohort), size = .1) + 
  geom_line(aes(y = avg_pass,  group = race_division_cohort), size = .1, color = "black") + 
  #geom_segment( aes(color=race, xend = grade, y = avg_pass, yend = pass), alpha=0.4) +
  geom_polygon( data = data.frame(x = 
                              c(cohorts$grade_2, rev(cohorts$grade_2 )), 
                            y = c(cohorts$avg_pass, rev(cohorts$pass_2)), 
                            division_name = c(cohorts$division_name, rev(cohorts$division_name)),
                            race = c(cohorts$race, rev(cohorts$race)), 
                            cohort = c(cohorts$cohort, rev(cohorts$cohort)),
                            group = c(cohorts$race_division_cohort, rev(cohorts$race_division_cohort))
  ),
  aes(x = x, y = y,  group = group, fill = race), inherit.aes = FALSE,
  alpha = .1
  ) +
  # coord_polar() +
  # xlim(1, 8) +
  #ylim(-50, 50) +
  facet_grid(cohort ~ division_name) +
  # theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_blank()
  )

jpeg(filename = "outputs/plots_sdp/area_plot_not_normed.jpg", height = 80*72/1, width = 40*72/1, units = 'px', res = 300)

## The Plot ##
p   

## Include the annotations ##
# grid.text(
#   "Abbreviations: PA- Prior Authorization; ETE- Ending the HIV Epidemic; TDF/FTC- tenofovir disoproxil fumarate/emtricitabine; TAF/FTC- tenofovir alafenamide/emtricitabine",
#   x = 0.99,
#   y = 0.01,
#   gp = gpar(
#     fontsize = 10,
#     fontface = 3
#   ),
#   just = c("right", "bottom")
# )

dev.off()



# Just C City -------------------------------------------------------------
scalefactor = 1.5

cohorts_ccity <- 
  race_cohorts  %>%
  left_join(average_cohorts) %>%
  filter(division_name == "Charlottesville City") %>%
  mutate(diff_pass = pass - avg_pass,
         diff_pass_2 = case_when(
           is.na(pass) ~ 0,
           #   pass == 0 ~ avg_pass,
           TRUE ~ diff_pass
         )
  ) %>%
  mutate(x = ayear,
         y = cohort + (diff_pass_2/100 * scalefactor),
         ymean = cohort)

# View(cohorts)
ccity_polygons <-
data.frame(x = 
             c(cohorts_ccity$x, rev(cohorts_ccity$x )), 
           y = c(cohorts_ccity$ymean, rev(cohorts_ccity$y)), 
           division_name = c(cohorts_ccity$division_name, rev(cohorts_ccity$division_name)),
           race = c(cohorts_ccity$race, rev(cohorts_ccity$race)), 
           cohort = c(cohorts_ccity$cohort, rev(cohorts_ccity$cohort)),
           group = c(cohorts_ccity$race_division_cohort, rev(cohorts_ccity$race_division_cohort))
)

p <-
  ggplot(data = filter(cohorts_ccity, 
                       #  cohort > 2005, 
                       #  cohort < 2015
  ), 
  aes(x = ayear) ) +
  geom_point(aes( y = y, color = race)) +
  geom_line(aes(y = y, group = race_division_cohort, color = race), size = .1) + 
  geom_line(aes(y = ymean, group = race_division_cohort), size = .1, color = "black") + 
  geom_polygon( data = ccity_polygons,
  aes(x = x, y = y,  group = group, fill = race), inherit.aes = FALSE,
  alpha = .1
  )  +
  
  # theme_void() +
 # labs(y = "Started 3rd Grade In", x = "Test Year") +
  annotate("rect", xmin = 2011.75, xmax = 2013.25, ymin = 2007.5, ymax = 2012.5,
           alpha = .2) +
 annotate("text", x = 2012.5, y = 2012.6, label = "Test Changes in 2013", size = 3, hjust = .5) +
 geom_line(data = cohorts_ccity, aes(x = ayear, y = ymean - .1 *scalefactor, group = race_division_cohort ),
           size = .1, 
           color = "black",
           linetype = "dotted"
           ) +
  geom_line(data = cohorts_ccity, aes(x = ayear, y = ymean + .1 *scalefactor, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted"
  ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .1, y = cohort - .1*scalefactor, label = "-10%") , size = 2, hjust = 0
            ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .1, y = cohort + .1*scalefactor, label = "+10%") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .1, y = cohort, label = "0") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_ccity %>% filter(race == "Black"), aes(x = ayear, y = cohort - .01*scalefactor, label = grade) , size = 2, hjust = .5, vjust = 1
  )  +
  geom_text(data = cohorts_ccity %>% arrange(cohort, grade) %>% 
              group_by(cohort) %>% 
              slice(1) %>% mutate(label = paste0("3rd Grade\n ", cohort-1,"-",cohort)), aes(x = ayear - .25, y = cohort, label = label) , size =2, hjust = 1,
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(linetype = "dashed", color = "lightgrey"),
    axis.text.x = element_text(angle = 60),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )  +
  scale_y_continuous(limits = c(2000.3, 2019.5), breaks = seq(2001, 2019, 1) ) +
  scale_x_continuous(limits = c(2005.5, 2019.5), breaks = seq(2006, 2019, 1), position = "top" )  +
  ggtitle("SOL Passage Rates in Charlottesville City,\nComparisons to Class Mean")



jpeg(filename = "outputs/plots_sdp/ccity_timeline_area_plot.jpg", height = 60*72/1, width = 20*72/1, units = 'px', res = 300)

## The Plot ##
p   

## Include the annotations ##
# grid.text(
#   "Abbreviations: PA- Prior Authorization; ETE- Ending the HIV Epidemic; TDF/FTC- tenofovir disoproxil fumarate/emtricitabine; TAF/FTC- tenofovir alafenamide/emtricitabine",
#   x = 0.99,
#   y = 0.01,
#   gp = gpar(
#     fontsize = 10,
#     fontface = 3
#   ),
#   just = c("right", "bottom")
# )

dev.off()


# Just Albemarle -------------------------------------------------------------
scalefactor = 1.5

cohorts_alb <- 
  race_cohorts  %>%
  left_join(average_cohorts) %>%
  filter(division_name == "Albemarle County") %>%
  mutate(diff_pass = pass - avg_pass,
         diff_pass_2 = case_when(
           is.na(pass) ~ 0,
           #   pass == 0 ~ avg_pass,
           TRUE ~ diff_pass
         )
  ) %>%
  mutate(x = ayear,
         y = cohort + (diff_pass_2/100 * scalefactor),
         ymean = cohort)

# View(cohorts)
alb_polygons <-
  data.frame(x = 
               c(cohorts_alb$x, rev(cohorts_alb$x )), 
             y = c(cohorts_alb$ymean, rev(cohorts_alb$y)), 
             division_name = c(cohorts_alb$division_name, rev(cohorts_alb$division_name)),
             race = c(cohorts_alb$race, rev(cohorts_alb$race)), 
             cohort = c(cohorts_alb$cohort, rev(cohorts_alb$cohort)),
             group = c(cohorts_alb$race_division_cohort, rev(cohorts_alb$race_division_cohort))
  )

p <-
  ggplot(data = filter(cohorts_alb, 
                       #  cohort > 2005, 
                       #  cohort < 2015
  ), 
  aes(x = ayear) ) +
  geom_point(aes( y = y, color = race)) +
  geom_line(aes(y = y, group = race_division_cohort, color = race), size = .1) + 
  geom_line(aes(y = ymean, group = race_division_cohort), size = .1, color = "black") + 
  geom_polygon( data = alb_polygons,
                aes(x = x, y = y,  group = group, fill = race), inherit.aes = FALSE,
                alpha = .1
  )  +
  scale_y_continuous(limits = c(2000.2, 2019.5), breaks = seq(2001, 2019, 1) ) +
  scale_x_continuous(limits = c(2005.5, 2019.5), breaks = seq(2006, 2019, 1) ) +
  
  # theme_void() +
  labs(y = "Cohort", x = "Test Year") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
    axis.text.x = element_text(angle = 60)
    
  ) +
  annotate("rect", xmin = 2011.75, xmax = 2013.25, ymin = 2007.5, ymax = 2012.5,
           alpha = .2) +
  annotate("text", x = 2012.5, y = 2012.6, label = "Test Changes in 2013", size = 3, hjust = .5) +
  geom_line(data = cohorts_alb, aes(x = ayear, y = ymean - .1 *scalefactor, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted"
  ) +
  geom_line(data = cohorts_alb, aes(x = ayear, y = ymean + .1 *scalefactor, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted"
  ) +
  geom_text(data = cohorts_alb %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .1, y = cohort - .1*scalefactor, label = "-10%") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_alb %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .1, y = cohort + .1*scalefactor, label = "+10%") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_alb %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .1, y = cohort, label = "0") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_alb %>% filter(race == "Black"), aes(x = ayear, y = cohort - .3, label = grade) , size = 2, hjust = .5, vjust = 1
  )



jpeg(filename = "outputs/plots_sdp/alb_timeline_area_plot.jpg", height = 60*72/1, width = 20*72/1, units = 'px', res = 300)

## The Plot ##
p   

## Include the annotations ##
# grid.text(
#   "Abbreviations: PA- Prior Authorization; ETE- Ending the HIV Epidemic; TDF/FTC- tenofovir disoproxil fumarate/emtricitabine; TAF/FTC- tenofovir alafenamide/emtricitabine",
#   x = 0.99,
#   y = 0.01,
#   gp = gpar(
#     fontsize = 10,
#     fontface = 3
#   ),
#   just = c("right", "bottom")
# )

dev.off()


# Potential Waffle Charts -------------------------------------------------

df <-    race_cohorts %>% 
              select(division_name, race, grade, total, pass, ayear, cohort) %>%
              mutate(num_pass = round(pass*total/100))

df[is.na(df)] <- 0

sim_data <-
df %>%
  uncount(total) %>%
  group_by(division_name, race, grade, pass, ayear, cohort, num_pass) %>%
  mutate(id = 1:n(), 
         status = case_when(
           id <= num_pass ~ "pass",
           TRUE ~ "fail"
         )
      ) 
























