## ---------------------------
## Script name: takeaways_for_Ben_3_2_2021.R
##
## Author:Sam Powers
## Date Created: 2021-03-02
##
## ---------------------------
## Purpose of script: To get Ben some visualizations that he can use with community activis
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_readingsol/rscripts")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(grid)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:

# load("../data/models_byyear.Rdata")
# 
# race_plot_data <-
#   race_effect %>%
#   mutate(division = gsub("__.+$", "",group)) %>%
#   select(year, Black, White, Diff, region, division) %>%
#   arrange(year, -Diff) %>%
#   group_by(year) %>%
#   mutate(rank = 1:n()) %>%
#   mutate(label = case_when(region == 1 ~ division, TRUE ~ ""))
# 
# write_csv(race_plot_data, path = "../data/race_plot_data.csv")

race_plot_data <- read_csv("../data/race_plot_data.csv")


# Redo the hurricane but only label cville & Alb --------------------------

race_plot_data_use <-
race_plot_data %>%
  mutate(
    label = case_when(
      division %in% c("Charlottesville City", "Albemarle County") ~ paste0(division, " (+", round(Diff*100),"%)")  ,
      TRUE ~ ""
    ),
    
    highlight = case_when(
      division %in% c("Charlottesville City", "Albemarle County") ~ 1,
      TRUE ~ 0
    ),
  )


my_theme <- theme_classic(
  # base_family = "Times"
) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro")) +
  # theme(panel.background = element_rect(fill = "#F5F5F5", color = "black"),
  #       legend.position = "none")
  theme(legend.position = "none") +
  theme(plot.title = element_text(vjust = .5))

nlines = 6
low <- .5
high <- 1

annotation_lines <- data.frame(
  y = rep(5, nlines),
  yend = rep(138, nlines),
  x = seq(low, high, length.out =nlines)
) %>%
  mutate(label = 
           case_when(x > 0 ~ paste0(x*100, "%"),
                     TRUE ~ paste0(x*100, "%") )
  )

hurrican_graph <-
  ggplot(race_plot_data_use, aes(group = division)) +
  geom_segment(aes(y= rank, yend=rank, x=Black, xend=White, color = as.character(highlight))) +
  geom_point(aes(y= rank,  x=Black), color = 'Black', size = .001) +
  geom_point(aes(y= rank,  x=White), color = 'Black', size = .001) +
  
  
  facet_wrap(~ year) +
  scale_colour_manual(values = c("0" = "grey", "1" = "red")) +
  scale_x_continuous(  
    limits = c(.45, 1.2),  
    breaks = c(0, .25, .5, .75), labels = function(x){paste0(round(x*100), "%")}) +
  geom_text(col = "gray13",
            hjust = "left",
            aes(label = label, y = rank),
            x = 1,  size = 2) +
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = '') +  
  labs(y = "") +  
  
  geom_segment(data = annotation_lines, aes(x= x, xend = x, y = yend, yend = y),
               linetype = "dashed", alpha = .2, inherit.aes =  FALSE) +
  geom_text(data = annotation_lines, aes(x = x, y = -3.5, label = label), 
            hjust = .5, size = 2, alpha = .5, vjust = 0,  inherit.aes =  FALSE) +
  geom_text(data = annotation_lines, aes(x = x, y = -3.5, label = label), 
            hjust = .5, size = 2, alpha = .5, vjust = 0,  inherit.aes =  FALSE) +
  annotate("text", x = .6, y = 136, label = "Black Student Literacy Rate",  size = 2, fontface = "bold") +
  annotate("text", x = 1, y = 136, label = "White Student Literacy Rate",  size = 2, fontface = "bold")  +
  annotate("segment", x = .50, xend = .50, y = 110, yend = 10, colour = "black", size=.5, alpha=1, 
           arrow=arrow(length=unit(0.1,"cm"), type = "closed")) +
  annotate(geom = "text", x = .48, y = 100, label = "Higher Disparity Ranking", color = "black",
           angle = 90, size = 2, vjust = 0, hjust = 0) +
  my_theme +
  labs(title = "Who Does Virginia Teach to Read?",
       subtitle = "White and Black Literacy Rates in Virginina in 3rd-8th Grade as Measured by SOL Reading Passage") 





jpeg(filename = "../outputs/plots_sdp/albemarle_cville_disparites_in_context.jpg", height = 80*72/1, width = 80*72/1, units = 'px', res = 300)

## The Plot ##
hurrican_graph

# Include the annotations ##
grid.text(
  "@EquityCenterUVA",
  x = 0.99,
  y = .001,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  ),
  just = c("right", "bottom")
)

dev.off()



# Gaps by year per place --------------------------------------------------


gaps_by_year_dat <- 
race_plot_data_use %>%
filter(highlight == 1)

nlines = 6
low <- .5
high <- 1

annotation_lines <- data.frame(
  y = rep(2006, nlines),
  yend = rep(2019, nlines),
  x = seq(low, high, length.out =nlines)
) %>%
  mutate(label = 
           case_when(x > 0 ~ paste0(x*100, "%"),
                     TRUE ~ paste0(x*100, "%") )
  )

gaps_by_year <-
ggplot(gaps_by_year_dat, aes(group = division)) +
  geom_segment(aes(y= year, yend=year, x=Black, xend=White), color = "grey", size = 1) +
  geom_point(aes(y= year,  x=Black, color = 'red'), size = 1) +
  geom_point(aes(y= year,  x=White, color = 'blue'), size = 1) +
  scale_color_identity(name = "",
                       breaks = c("blue", "red"),
                       labels = c("White Literacy Rate", "Black Literacy Rate"),
                       guide = "legend") +
  
  geom_text(aes(y= year,  x= White + .02, label = paste0(round(White*100), "%") ), vjust = 0, size = 3 ) +
  geom_text(aes(y= year,  x= Black - .02, label = paste0(round(Black*100), "%") ), vjust = 1, size = 3 ) +
  

  scale_x_continuous(  
    limits = c(.45, 1),  
    breaks = c(0, .25, .5, .75), labels = function(x){paste0(round(x*100), "%")}) +
  scale_y_continuous(  
    limits = c(2005.25, 2019),  
    breaks = seq(2006,  2019, 1)) +
  geom_segment(data = annotation_lines, aes(x= x, xend = x, y = yend, yend = y),
               linetype = "dashed", alpha = .2, inherit.aes =  FALSE) +
  geom_text(data = annotation_lines, aes(x = x, y = 2005.5, label = label), 
            hjust = .5, size = 2, alpha = .5, vjust = .5,  inherit.aes =  FALSE) +
  geom_text(data = annotation_lines, aes(x = x, y = 2005.5, label = label), 
            hjust = .5, size = 2, alpha = .5, vjust = .5,  inherit.aes =  FALSE) +
#  annotate("text", x = .6, y = 2006, label = "Black Student Literacy Rate",  hjust = 0, size = 2, fontface = "bold") +
#  annotate("text", x = 1, y = 2006, label = "White Student Literacy Rate",  hjust = 0, size = 2, fontface = "bold") +
  my_theme +
  #theme(axis.ticks.y = element_line()) +
  theme(axis.text.x = element_text()) +
  theme(legend.position = "top") +
#  theme( panel.border = element_rect(color = "white", fill= NA)) +
  
  labs(title = "Who Does Virginia Teach to Read?",
       subtitle = "White and Black Literacy Rates in Albemarle & Charlottesville in 3rd-8th Grade as Measured by SOL Reading Passage",
       y = "Year", x = "") +
  facet_grid(division~., switch = "y") +
  coord_flip()


jpeg(filename = "../outputs/plots_sdp/albemarle_cville_disparites_over_time.jpg", height = 25*72/1, width = 40*72/1, units = 'px', res = 300)

## The Plot ##
gaps_by_year

# Include the annotations ##
grid.text(
  "@EquityCenterUVA",
  x = 0.99,
  y = .01,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  ),
  just = c("right", "bottom")
)


dev.off()

?grid.abline()


# Cohort Numbers ----------------------------------------------------------

cohort_stats <-
read_csv( "../../vaequity-reading/assets/data/educ_equity.csv") %>%
  filter(division_name %in% c("Charlottesville City")) %>%
  select(division_name, ayear, total_count, pass_count, pass_rate, race, cohort, grade) %>%
  filter(race %in% c("Black", "White")) %>%
  mutate(
    across(c(total_count, pass_count),
           ~case_when(
             race == "White" ~ .x*-1, 
                      TRUE ~ .x
             )
           ),
    cohort_label = paste0("Finished 8th Grade in ", cohort)
  )

cohort_numbers <-
ggplot(cohort_stats) +
  geom_col(
    aes(y = total_count, x = grade, fill = race, alpha = .5), 
  #  color = "black", 
  ) +
  geom_col(
    aes(y = pass_count, x = grade, fill = race, alpha = .7)
  ) +
  
  scale_alpha_identity(name = "Legend",
                       breaks = c(.7, .5),
                       labels = c("Passed Test", "Took Test"),
                       guide = "legend") +
  
  geom_text(
    aes(y = total_count, x = grade, label = paste0(abs(pass_count), "/", abs(total_count)), 
        hjust = (total_count/abs(total_count)*-1)/1.5 + .5),
    size = 3
  ) +
  
 geom_hline(yintercept = 0) +
  
  scale_fill_manual(values = c("#9400D3", "#228B22")) +
  scale_y_continuous(labels = function(x) abs(x), limits = c(-175, 150), breaks = seq(-150, 150, 50)) +
  scale_x_continuous(labels = function(x) paste0("Grade ", x),  breaks = seq(3, 8, 1)) +
  
  coord_flip(clip= "off") +
  facet_wrap(~cohort_label, scales = "free") +
  
  annotate("text", x = 8.7, y = 90, label = "Black", fontface = "bold" ) +
  annotate("text", x = 8.7, y = -90, label = "White", fontface = "bold" ) +
  annotate("rect", xmin = 8.6, xmax = 8.9, ymin = 135, ymax = 150, alpha = .5, fill = "#9400D3") +
  annotate("text", x = 8.75, y = 150, label = " = Took SOL", size = 3, hjust = 0)+
  annotate("rect", xmin = 8.3, xmax = 8.6, ymin = 135, ymax = 150, alpha = 1, fill = "#9400D3") +
  annotate("text", x = 8.45, y = 150, label = " = Passed SOL", size = 3, hjust = 0) +
  
  theme_classic(
    # base_family = "Times"
  ) +
  theme(axis.text.y = element_text(face = "bold")) + 
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(plot.title = element_text(face = "bold", size = 26, vjust = 3, hjust = 0)) +
  theme(plot.subtitle = element_text(vjust = 3, hjust = 0))  +
#  theme(axis.text.x = element_blank()) +
#  theme(axis.ticks.x = element_blank()) +
#  theme(axis.line.x = element_blank()) +
#  theme(panel.grid.major.y = element_line(linetype = "dashed", size = .1, color = "black")) +
  #theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro", margin(2, 2, 2, 2, "cm"))) +
  theme(panel.background = element_rect(fill = "gainsboro")) +
  theme(panel.spacing = unit(4, "lines")) +
  # theme(panel.background = element_rect(fill = "#F5F5F5", color = "black"),
  #       legend.position = "none")
  theme(legend.position = "none") +
  
theme(plot.margin = margin(2, 2, 2, 2, "cm")) +
 
labs(x = "", y = "", title = "Who Do We Teach to Read?", subtitle = "Counts of reading SOL test takers and test passers by grade for each student cohort in Charlottesville City", fill = "")


jpeg(filename = "../outputs/plots_sdp/cville_cohorts.jpg", height = 60*72/1, width = 60*72/1, units = 'px', res = 300)

## The Plot ##
cohort_numbers

# Include the annotations ##
grid.text(
  "@EquityCenterUVA",
  x = 0.99,
  y = .01,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  ),
  just = c("right", "bottom")
)


dev.off()


# Counts in Albemarle -----------------------------------------------------

cohort_stats <-
  read_csv( "../../vaequity-reading/assets/data/educ_equity.csv") %>%
  filter(division_name %in% c("Albemarle County")) %>%
  select(division_name, ayear, total_count, pass_count, pass_rate, race, cohort, grade) %>%
  filter(race %in% c("Black", "White")) %>%
  mutate(
    across(c(total_count, pass_count),
           ~case_when(
             race == "White" ~ .x*-1, 
             TRUE ~ .x
           )
    ),
    cohort_label = paste0("Finished 8th Grade in ", cohort)
  )

cohort_numbers <-
  ggplot(cohort_stats) +
  geom_col(
    aes(y = total_count, x = grade, fill = race, alpha = .5), 
    #  color = "black", 
  ) +
  geom_col(
    aes(y = pass_count, x = grade, fill = race, alpha = .7)
  ) +
  
  scale_alpha_identity(name = "Legend",
                       breaks = c(.7, .5),
                       labels = c("Passed Test", "Took Test"),
                       guide = "legend") +
  
  geom_text(
    aes(y = total_count, x = grade, label = paste0(abs(pass_count), "/", abs(total_count)), 
        hjust = (total_count/abs(total_count)*-1)/1.5 + .5),
    size = 3
  ) +
  
  geom_hline(yintercept = 0) +
  
  scale_fill_manual(values = c("#9400D3", "#228B22")) +
  scale_y_continuous(labels = function(x) abs(x), limits = c(-900, 400), breaks = seq(-700, 300, 100)) +
  scale_x_continuous(labels = function(x) paste0("Grade ", x),  breaks = seq(3, 8, 1)) +
  
  coord_flip(clip= "off") +
  facet_wrap(~cohort_label, scales = "free") +
  
  annotate("text", x = 8.7, y = 175, label = "Black", fontface = "bold" ) +
  annotate("text", x = 8.7, y = -400, label = "White", fontface = "bold" ) +
  annotate("rect", xmin = 8.6, xmax = 8.9, ymin = 350, ymax = 400, alpha = .5, fill = "#9400D3") +
  annotate("text", x = 8.75, y = 400, label = " = Took SOL", size = 3, hjust = 0)+
  annotate("rect", xmin = 8.3, xmax = 8.6, ymin = 350, ymax = 400, alpha = 1, fill = "#9400D3") +
  annotate("text", x = 8.45, y = 400, label = " = Passed SOL", size = 3, hjust = 0) +
  
  theme_classic(
    # base_family = "Times"
  ) +
  theme(axis.text.y = element_text(face = "bold")) + 
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(plot.title = element_text(face = "bold", size = 26, vjust = 3, hjust = 0)) +
  theme(plot.subtitle = element_text(vjust = 3, hjust = 0))  +
  #  theme(axis.text.x = element_blank()) +
  #  theme(axis.ticks.x = element_blank()) +
  #  theme(axis.line.x = element_blank()) +
  #  theme(panel.grid.major.y = element_line(linetype = "dashed", size = .1, color = "black")) +
  #theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro", margin(2, 2, 2, 2, "cm"))) +
  theme(panel.background = element_rect(fill = "gainsboro")) +
  theme(panel.spacing = unit(4, "lines")) +
  # theme(panel.background = element_rect(fill = "#F5F5F5", color = "black"),
  #       legend.position = "none")
  theme(legend.position = "none") +
  
  theme(plot.margin = margin(2, 2, 2, 2, "cm")) +
  
  labs(x = "", y = "", title = "Who Do We Teach to Read?", subtitle = "Counts of reading SOL test takers and test passers by grade for each student cohort in Albemarle County", fill = "")


jpeg(filename = "../outputs/plots_sdp/albemarle_cohorts.jpg", height = 60*72/1, width = 60*72/1, units = 'px', res = 300)

## The Plot ##
cohort_numbers

# Include the annotations ##
grid.text(
  "@EquityCenterUVA",
  x = 0.99,
  y = .01,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  ),
  just = c("right", "bottom")
)


dev.off()












