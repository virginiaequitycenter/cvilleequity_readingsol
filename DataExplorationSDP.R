library(tidyverse)
library(RColorBrewer) 
library(grid)

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
  ) %>%
  mutate(
    cohort_label = paste0("3rd in ", paste0( "'", substr(cohort -1, 3,4), "-'", substr(cohort, 3,4)))
  )

# View(cohorts)
non_zero_polygons <- data.frame(x = 
                               c(cohorts$grade_2, rev(cohorts$grade_2 )), 
                             y = c(cohorts$avg_pass, rev(cohorts$pass_2)), 
                             division_name = c(cohorts$division_name, rev(cohorts$division_name)),
                             race = c(cohorts$race, rev(cohorts$race)), 
                             cohort_label = c(cohorts$cohort_label, rev(cohorts$cohort_label)),
                             group = c(cohorts$race_division_cohort, rev(cohorts$race_division_cohort))
)

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
  geom_polygon( data = non_zero_polygons,
  aes(x = x, y = y,  group = group, fill = race), inherit.aes = FALSE,
  alpha = .1
  ) +
  geom_line(data = cohorts, aes(x = grade, y =  90, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  geom_line(data = cohorts, aes(x = grade, y =  80, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  geom_line(data = cohorts, aes(x = grade, y =   70, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  
  geom_line(data = cohorts, aes(x = grade, y =   60, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  
  geom_line(data = cohorts, aes(x = grade, y =   50, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted",
            inherit.aes= FALSE
  ) +
  
  
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = 90, label = "90%") , 
            size = 2, 
            hjust = 0,
            inherit.aes= FALSE
  ) +
  
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = 80, label = "80%") , 
            size = 2, 
            hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = 70, label = "70%") , size = 2, hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = 60, label = "60%") , size = 2, hjust = 0,
            inherit.aes= FALSE
  ) +
  
  geom_text(data = cohorts %>% arrange(division_name, cohort, desc(grade)) %>% 
              group_by(division_name, cohort) %>% 
              slice(1), aes(x = grade + .15, y = 50, label = "50%") , size = 2, hjust = 0,
            inherit.aes= FALSE
  ) +
  geom_text(data = cohorts %>% filter(race == "White") %>% mutate(label = paste0( grade, "th")), aes(x = grade, y = 40, label = label) , size = 1.5, hjust = .5, vjust = 1,
            inherit.aes= FALSE
  ) +
  
  geom_segment(data = cohorts %>% filter(race == "White"), aes(x = grade, xend = grade, y = 41, yend = 43), size = .2,
               inherit.aes = FALSE
  ) + coord_cartesian(clip = "off") +
  facet_grid(cohort_label ~ division_name) +
  scale_x_continuous(limits = c(2.5, 8.75), breaks = seq(3, 8, 1) ) +
  
  geom_text( data = data.frame( x = 3, y = 75, 
                                cohort_label = "3rd in '00-'01", 
                                division_name = "Albemarle County", 
                                label = "= Overall Passage Rate"),
             aes(x=x, y=y, label = label), inherit.aes = FALSE, size = 2, hjust = 0 ) +
  geom_segment( data = data.frame( x = 3, y = 80, xend = 4.5, yend = 80,
                                cohort_label = "3rd in '00-'01", 
                                division_name = "Albemarle County", 
                                label = "= Overall Passage Rate"),
             aes(x=x, y=y, xend = xend, yend = yend), inherit.aes = FALSE, size = .1 ) +
  
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
  ggtitle("SOL Reading Passage Rate of Black and White Students 3rd-8th Grade")

jpeg(filename = "outputs/plots_sdp/area_plot_not_normed.jpg", height = 80*72/1, width = 40*72/1, units = 'px', res = 300)

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
  geom_line(data = cohorts_ccity, aes(x = ayear, y = ymean -.2 *scalefactor, group = race_division_cohort ),
            size = .1, 
            color = "black",
            linetype = "dotted"
  ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .2, y = cohort - .1*scalefactor, label = "-10%") , size = 2, hjust = 0
            ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>%
              slice(1), aes(x = ayear + .2, y = cohort + .1*scalefactor, label = "+10%") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .2, y = cohort - .2*scalefactor, label = "-20%") , size = 2, hjust = 0
  ) +
  geom_text(data = cohorts_ccity %>% arrange(cohort, desc(grade)) %>% 
              group_by(cohort) %>% 
              slice(1), aes(x = ayear + .2, y = cohort, label = "0") , size = 2, hjust = 0
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
  scale_x_continuous(limits = c(2005.5, 2019.8), breaks = seq(2006, 2019, 1), position = "top" )  +
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



# Some Modelling ----------------------------------------------------------
# Problems with this analysis. We do not know the general balance between the individuals and this weights individuals the same. 
# We need simulated individual-level data. Which I can get. 
# The Level of Analysis here is at the Division-Year-Grade Level
race_cohorts

mod1 <- lm(pass ~ race, data = race_cohorts )
summary(mod1)

# Does Grade adjust any of the base starting points
mod2 <- lm(pass ~ race + grade , data = race_cohorts )
summary(mod2)
# maybe, alittle. It looks like that, on the whole, 

# Is there a Muplicative affect of grade?
mod3 <- lm(pass ~ race*grade , data = race_cohorts )
summary(mod3)
# No, there does not seem to be. 

# Is there an affect by Division?
mod4 <- lm(pass ~ race +division_name + grade, data = race_cohorts )
summary(mod4)
# Yes, we could say that Green County generally starts with lower scores

# But is this affect multiplicative with Race?
mod5 <- lm(pass ~ race*division_name + grade, data = race_cohorts )
summary(mod5)
# Yes, I think so. Although this is a potentially dangerous control given that the racial distributions vary so significantly between counties. 


mod6 <- lm(pass ~ race + as.factor(ayear), data = race_cohorts )
summary(mod6)
# When we add year as a factor var in this analysis, we can clearly see the effect of the 2012 - 2013 shift. 

# Lets try to just dichotimize that so this doesn't become too too unwieldy
race_models <-
race_cohorts %>%
  mutate(post_2012 = case_when(
    ayear > 2012 ~ "yes",
    TRUE ~ "no"
  ))

mod7 <- lm(pass ~ race + post_2012, data = race_models )
summary(mod7)

# Okay, so far, we know a few things. (1) Race is difitively a determining factor in the likelihood that one will pass the reading sol. 
# 2, I need to figure out how to weight race bc this is treating all of the data points equally. 


# Some Potentially Better Modelling ---------------------------------------
library(margins)

df <-    race_cohorts %>%  
  select(division_name, race, grade, total, pass_pct = pass, ayear, cohort) %>%
  mutate(num_pass = round(pass_pct*total/100))

df[is.na(df)] <- 0
df$total[df$num_pass == 0] <- 0

sim_data <-
  df %>%
  uncount(total) %>%
  group_by(division_name, race, grade, pass_pct, ayear, cohort, num_pass) %>%
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

# Okay, this seems like a reasonable binomial model. We can use marginal effects to determine percentage point increases/decreases. 
glm1 <- glm(pass ~ race, sim_data, family = "binomial")

summary(glm1)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(Level = rowname) %>%
  filter(!Level == "(Intercept)") %>%
  transmute(  
    level = Level,
    estimate = round(exp(Estimate),2),
    low = round(exp(Estimate  - 1.96*`Std. Error`),2),
    high = round(exp(Estimate  + 1.96*`Std. Error`),2),
    pval = round(`Pr(>|z|)`, 3)
  ) 

marg1 <- summary(margins(glm1))
marg1
# Yes, there is an AME of .229 in the area

# Should definitely control for division 
glm2 <- glm(pass ~ race + division_name, 
            sim_data, 
            family = "binomial")

summary(glm2)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(Level = rowname) %>%
  filter(!Level == "(Intercept)") %>%
  transmute(  
    level = Level,
    estimate = round(exp(Estimate),2),
    low = round(exp(Estimate  - 1.96*`Std. Error`),2),
    high = round(exp(Estimate  + 1.96*`Std. Error`),2),
    pval = round(`Pr(>|z|)`, 3)
  ) 

marg2 <- summary(margins(glm2))
marg2
# .231 when we control for the fact that there are multiple school systems

# But what about a multiplicative affect of division
glm3 <- glm(pass ~ race*division_name, 
            sim_data, 
            family = "binomial")

summary(glm3)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(Level = rowname) %>%
  filter(!Level == "(Intercept)") %>%
  transmute(  
    level = Level,
    estimate = round(exp(Estimate),2),
    low = round(exp(Estimate  - 1.96*`Std. Error`),2),
    high = round(exp(Estimate  + 1.96*`Std. Error`),2),
    pval = round(`Pr(>|z|)`, 3)
  ) 

marg3 <- summary(margins(glm3))
marg3

# So from this model, we can see that it looks like being in Cville accentuates the disparities present. 
# If I want AMEs for Cville, I may have to stratify to get each independently. 
glm_division <- list()
marg_division <- list()
for ( div in unique(sim_data$division_name) ) {
  
glm4 <- glm(pass ~ race, 
            sim_data %>%
              filter(division_name == div), 
            family = "binomial")

glm_division[[div]] <-
summary(glm4)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(Level = rowname) %>%
  filter(!Level == "(Intercept)") %>%
  transmute(  
    level = Level,
    estimate = round(exp(Estimate),2),
    low = round(exp(Estimate  - 1.96*`Std. Error`),2),
    high = round(exp(Estimate  + 1.96*`Std. Error`),2),
    pval = round(`Pr(>|z|)`, 3)
  ) %>%
  mutate(n = nobs(glm4))

marg_division[[div]] <- summary(margins(glm4)) %>% as.data.frame()
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)

# The effects are strong everywhere, but they are much Cville (.3287) and in Albemarle (.2584)
# and as low as .114 in Nelson and .131 in Green. 

## Lets add in the potential effect of grade in here
glm_division <- list()
marg_division <- list()
for ( div in unique(sim_data$division_name) ) {
  
  glm4 <- glm(pass ~ race + grade, 
              sim_data %>%
                filter(division_name == div), 
              family = "binomial")
  
  glm_division[[div]] <-
    summary(glm4)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Level = rowname) %>%
    filter(!Level == "(Intercept)") %>%
    transmute(  
      level = Level,
      estimate = round(exp(Estimate),2),
      low = round(exp(Estimate  - 1.96*`Std. Error`),2),
      high = round(exp(Estimate  + 1.96*`Std. Error`),2),
      pval = round(`Pr(>|z|)`, 3)
    ) %>%
    mutate(n = nobs(glm4))
  
  marg_division[[div]] <- summary(margins(glm4)) %>% as.data.frame()
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)

# Grade does make a subtle difference in the starting point, it appears. Does it modify the effect of race, though, is the biggest question. 

## Lets test it. 
glm_division <- list()
marg_division <- list()
for ( div in unique(sim_data$division_name) ) {
  
  glm4 <- glm(pass ~ race*grade, 
              sim_data %>%
                filter(division_name == div), 
              family = "binomial")
  
  glm_division[[div]] <-
    summary(glm4)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Level = rowname) %>%
    filter(!Level == "(Intercept)") %>%
    transmute(  
      level = Level,
      estimate = round(exp(Estimate),2),
      low = round(exp(Estimate  - 1.96*`Std. Error`),2),
      high = round(exp(Estimate  + 1.96*`Std. Error`),2),
      pval = round(`Pr(>|z|)`, 3)
    ) %>%
    mutate(n = nobs(glm4))
  
  marg_division[[div]] <- summary(margins(glm4)) %>% as.data.frame()
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)
# Grade is not an effect modifier 

## We do know there is an effect of post 2012 on overall rates. 
glm_division <- list()
marg_division <- list()
for ( div in unique(sim_data$division_name) ) {
  
  glm4 <- glm(pass ~ race + post_2012, 
              sim_data %>%
                filter(division_name == div), 
              family = "binomial")
  
  glm_division[[div]] <-
    summary(glm4)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Level = rowname) %>%
    filter(!Level == "(Intercept)") %>%
    transmute(  
      level = Level,
      estimate = round(exp(Estimate),2),
      low = round(exp(Estimate  - 1.96*`Std. Error`),2),
      high = round(exp(Estimate  + 1.96*`Std. Error`),2),
      pval = round(`Pr(>|z|)`, 3)
    ) %>%
    mutate(n = nobs(glm4))
  
  marg_division[[div]] <- summary(margins(glm4)) %>% as.data.frame()
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)
# That effect definitely does come through in the model. Is there an effect modification?

glm_division <- list()
marg_division <- list()
for ( div in unique(sim_data$division_name) ) {
  
  glm4 <- glm(pass ~ race*post_2012  + grade, 
              sim_data %>%
                filter(division_name == div), 
              family = "binomial")
  
  glm_division[[div]] <-
    summary(glm4)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Level = rowname) %>%
    filter(!Level == "(Intercept)") %>%
    transmute(  
      level = Level,
      estimate = round(exp(Estimate),2),
      low = round(exp(Estimate  - 1.96*`Std. Error`),2),
      high = round(exp(Estimate  + 1.96*`Std. Error`),2),
      pval = round(`Pr(>|z|)`, 3)
    ) %>%
    mutate(n = nobs(glm4))
  
  marg_division[[div]] <- summary(margins(glm4)) %>% as.data.frame()
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)
# Yes, there seems to be an effect modification 

## What is the effect of the interaction of post 2012 across Counties
glm_division <- list()
marg_division <- list()

for ( div in unique(sim_data$post_2012) ) {
  
  glm4 <- glm(pass ~ race + division_name + grade, 
              sim_data %>%
                filter(post_2012 == div), 
              family = "binomial")
  
  glm_division[[div]] <-
    summary(glm4)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Level = rowname) %>%
    filter(!Level == "(Intercept)") %>%
    transmute(  
      level = Level,
      estimate = round(exp(Estimate),2),
      low = round(exp(Estimate  - 1.96*`Std. Error`),2),
      high = round(exp(Estimate  + 1.96*`Std. Error`),2),
      pval = round(`Pr(>|z|)`, 3)
    ) %>%
    mutate(n = nobs(glm4))
  
  marg_division[[div]] <- summary(margins(glm4)) %>% as.data.frame()
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)

# So the effect pre - 2012 is .1820 while after it jumps to 0.2942. That is an additional difference of 0.1122 attributable 
# to the 2012 - 2013 change in structuring. 



## What about within counties
glm_division <- list()
marg_division <- list()

for ( year in unique(sim_data$post_2012) ) {
  for ( div in unique(sim_data$division_name) ) {
    
  glm4 <- glm(pass ~ race  + grade, 
              sim_data %>%
                filter(post_2012 == year, division_name == div ), 
              family = "binomial")
  
  glm_division[[div]][[year]] <-
    summary(glm4)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(Level = rowname) %>%
    filter(!Level == "(Intercept)") %>%
    transmute(  
      level = Level,
      estimate = round(exp(Estimate),2),
      low = round(exp(Estimate  - 1.96*`Std. Error`),2),
      high = round(exp(Estimate  + 1.96*`Std. Error`),2),
      pval = round(`Pr(>|z|)`, 3)
    ) %>%
    mutate(n = nobs(glm4))
  
  marg_division[[div]][[year]] <- summary(margins(glm4)) %>% as.data.frame()

  }
}

glm_division_final <- do.call(rbind, glm_division)
marg_division_final <- do.call(rbind, marg_division)

# Green County has a reversed trend of .157 to .127
# Cville City jumps .264 to .397
# Albemarle jumps .185 to .341



# Time Wise Trend Modelling -----------------------------------------------
# So above we looked at the Marginal Effects, but we don't know if schools exacerbate or close the gaps overtime.

# We could do a make-shift difference in difference here by cohorts. 
# Technically with DD we really need the intervention to happen after the start date. But ~maybe~ the intervention here is "being educated in 
# grades 4-8 while white". If we can wrap our heads aroudn it like that, then just maybe we can use it. 

# Data set up

dd_data <-
sim_data %>%
  filter(grade %in% c(3, 8)) %>%
  mutate(has8 = ifelse(grade == 8, 1, 0),
         has3 = ifelse(grade == 3, 1, 0)) %>%
  group_by(division_name, race, cohort) %>%
  mutate(has8 = sum(has8),
         has3 = sum(has3)
         ) %>%
  filter(has8 > 0, has3> 0)  %>% # We need to make sure that any cohorts used have both year 3 and year 8 in them 
  mutate(time_effect = case_when(
    grade == 8 ~ 1,
    grade == 3 ~ 0
  ),
  treated = case_when(
    race == "White" ~ 1,
    race == "Black" ~ 0
  )) %>%
  mutate(did = treated*time_effect)

# Okay, im going to fudge alittle and be an economist and just use a straight linear model. 
lineardd <- lm(pass ~ treated + time_effect +  did + division_name, data = dd_data )
summary(lineardd)

# There may be a significant timewise effect of grades in here. 

# Okay, being educated in 4-8 while white may raise your effect. 
lineardd <- glm(pass ~ treated + time_effect +  did + division_name, data = dd_data , family = "binomial")
summary(lineardd)
margdd <-summary(margins(lineardd))


# Also, are there autocorrellations that I am not handling well here?














