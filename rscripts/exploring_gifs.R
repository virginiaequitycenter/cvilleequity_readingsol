library(tidyverse)
library(gganimate)
library(grid)


race_plot_data <- read_csv("../data/race_plot_data.csv")

race_plot_data <-
race_effect %>%
  mutate(division = gsub("__.+$", "",group)) %>%
  select(year, Black, White, Diff, region, division) %>%
  arrange(year, -Diff) %>%
  group_by(year) %>%
  mutate(rank = 1:n()) %>%
  mutate(label = case_when(region == 1 ~ division, TRUE ~ ""))


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
  ggplot(race_plot_data, aes(group = division)) +
  geom_segment(aes(y= rank, yend=rank, x=Black, xend=White, color = region)) +
  geom_point(aes(y= rank,  x=Black), color = 'Black', size = .01) +
  geom_point(aes(y= rank,  x=White), color = 'Black', size = .01) +
  
  
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
  

jpeg(filename = "../outputs/plots_sdp/hurricane_plot.jpg", height = 80*72/1, width = 80*72/1, units = 'px', res = 300)

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


hurrican_graph +
  facet_null() +
  geom_text(x = 1.15 , y = -136,  
          #  family = "Times",  
            aes(label = as.character(year)),  
            size = 10, col = "grey18", hjust = "center") +
  gganimate::transition_time(year)

anim_save("../outputs/plots_sdp/literacy_rates.gif")





# Just the Pass Rate Differences ------------------------------------------


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
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5, size = 5))


nlines = 6
low <- 0
high <- .5

annotation_lines <- data.frame(
  y = rep(5, nlines),
  yend = rep(138, nlines),
  x = seq(low, high, length.out =nlines)
) %>%
  mutate(label = 
           case_when(x > 0 ~ paste0("+",x*100, "%"),
                     TRUE ~ paste0(x*100, "%") )
         )



ggplot(race_plot_data, aes(x = diff, group = division)) +
  
  geom_point(aes(y= rank, x= Diff, color = region), size = .5) +
  facet_grid(year ~.) +
  scale_colour_manual(values = c("0" = "grey", "1" = "red")) +
  scale_x_continuous(  
    limits = c(-.005, .5),  
    breaks = c(0, .25, .5, .75), labels = function(x){paste0(round(x*100), "%")}) +
  geom_text(col = "gray13",
            hjust = "left",
            aes(label = label, y = rank, x = Diff +.01),
             size = 1) +
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = "") +  
  labs(y = "") +  
  labs(title = "White Student Advantages in Literacy",
       subtitle = "White - Black Differences in 3rd-8th Grade SOL Reading Passage Rates in Virginia School Divisions") +
  
  my_theme +
  
  geom_segment(data = annotation_lines, aes(x= x, xend = x, y = yend, yend = y),
               inherit.aes =  FALSE, linetype = "dashed", alpha = .2) +
  geom_text(data = annotation_lines, aes(x = x, y = -3.5, label = label), 
            hjust = .5, size = 2, alpha = .5, vjust = 0,  inherit.aes =  FALSE) +
  
  annotate("segment", x = .15, xend = .40, y = 137, yend = 137, colour = "black", size=.5, alpha=1, 
           arrow=arrow(length=unit(0.1,"cm"), type = "closed")) +
  annotate("text", x = .17, y = 132, colour = "black", size=2, alpha=0.8, label = "Higher White Student Advantage", hjust = 0, vjust = 0) +
  
  annotate("segment", x = .06, xend = .06, y = 110, yend = 10, colour = "black", size=.5, alpha=1, 
           arrow=arrow(length=unit(0.1,"cm"), type = "closed")) +
  annotate(geom = "text", x = .05, y = 100, label = "Higher Disparity Ranking", color = "black",
           angle = 90, size = 2, vjust = 0, hjust = 0)-> facet_graph



jpeg(filename = "../outputs/plots_sdp/facet_diff.jpg", height = 120*72/1, width = 20*72/1, units = 'px', res = 300)

## The Plot ##
facet_graph

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


# set up the .gif with annotations ----------------------------------------

  facet_graph +
  facet_null() +
  geom_text(x = .5 , y = -125,  
            family = "Times",  
            aes(label = as.character(year)),  
            size = 10, col = "grey18", hjust = "right") +
  gganimate::transition_time(year)


anim_save("../outputs/plots_sdp/literacy-differences.gif")



