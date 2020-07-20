#set up 
library(tidyverse)
library(ggplot2)
library(ggalt)
library(dplyr)
library(ggcharts)

#data cleaning - division w/ no level 

division_no_level <- division_no_level %>% 
  transform(Race = as.character(Race))

div_black <- division_no_level %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- division_no_level %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

division_no_level <- division_no_level %>%
  select(School.Year, Division.Name) %>%
  distinct() %>%
  left_join(div_black) %>%
  left_join(div_white) %>%
  as.tibble() %>%
  transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
            Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
  mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)

#achievement gap black and white by test level per division per year
---------------
#Albemarle
  
division_no_level <- division_no_level %>%
    filter(Division.Name == "Albemarle County") %>%
    arrange(School.Year)
  
ggplot(division_no_level) +
    geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
    geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#BFD3C1", size=3 ) +
    geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#EAC5D8", size=3 ) +
    coord_flip() +
    theme(
      legend.position = "none",
    ) +
    xlab("") +
    ylab("Pass Rate")
    
    labs(title="Achievement Gap",
         subtitle = "Albemarle County, 2005-2019")
    
          
              
         
