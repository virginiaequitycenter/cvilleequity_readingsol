library(tidyverse)
library(gganimate)

load("../data/models_byyear.Rdata")

race_plot_data <-
race_effect %>%
  mutate(division = gsub("__.+$", "",group)) %>%
  select(year, Black, White, Diff, region, division) %>%
  arrange(year, -Diff) %>%
  group_by(year) %>%
  mutate(rank = 1:n()) %>%
  mutate(label = case_when(region == 1 ~ division, TRUE ~ ""))




my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"),
        legend.position = "none")


base_graph <-
  ggplot(race_plot_data) +
  geom_segment(aes(y= rank, yend=rank, x=Black, xend=White, color = region)) +
  facet_wrap(~ year) +
  scale_colour_manual(values = c("0" = "grey", "1" = "red")) +
  scale_x_continuous(  
    limits = c(.30, 1.2),  
    breaks = c(0, .25, .5, .75), labels = function(x){paste0(round(x*100), "%")}) +
  geom_text(col = "gray13",
            hjust = "left",
            aes(label = label, y = rank),
            x = 1,  size = 2) +
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Literacy Rate') +  
  labs(y = "") +  
  my_theme 

base_graph +
  facet_null() +
  geom_text(x = .5 , y = -125,  
            family = "Times",  
            aes(label = as.character(year)),  
            size = 10, col = "grey18", hjust = "right") +
  aes(group = division) +  
  gganimate::transition_time(year)

anim_save("../outputs/plots_sdp/literacy_rates.gif")





