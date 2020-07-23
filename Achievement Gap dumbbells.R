#DATA VIZ

#set up 
library(tidyverse)
library(ggplot2)
library(ggalt)
library(dplyr)
library(ggcharts)

setwd("/Users/savannahholmes/Desktop/GitHub/cvilleequity_readingsol"))

#data cleaning - division w/ no level 

#albemarle

gap_alb <- gap_alb %>% 
  transform(Race = as.character(Race))

div_black <- gap_alb %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- gap_alb %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

gap_alb <- gap_alb %>%
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
  
gap_alb <- gap_alb %>%
  arrange(School.Year)

ggplot(gap_alb) +
  geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
  geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
  geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
  coord_flip() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Pass Rate")

#Labels, Difference Column, Title, Subtitle and Caption 

+ labs(x=NULL, y=NULL, title="Achievement Gap between Black and White Pupils",
       subtitle="Albemarle County, 2005-2019",
       caption="Source: The Equity Center at the University of Virginia")

geom_text(data=filter(Black=="2018-2019"),
          aes(x=dem, y=concerned, label="Democrats"),
          color=blue, size=3, vjust=-1.5, fontface="bold", family="Lato") +
geom_text(data=filter(infected, concerned=="2018-2019"),
            aes(x=rep, y=concerned, label="Republicans"),
            color=red, size=3, vjust=-1.5, fontface="bold", family="Lato")

geom_rect(data=infected, aes(xmin=.5, xmax=.6, ymin=-Inf, ymax=Inf), fill="light grey") +
  geom_text(data=infected, aes(label=paste0(diff*100, "%"), y=concerned, x=.55), fontface="bold", size=3, family="Lato") +
  geom_text(data=filter(infected, concerned=="Very concerned"), 
            aes(x=.55, y=concerned, label="Difference"),
            color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +
  scale_x_continuous(expand=c(0,0), limits=c(0, .625)) +
  scale_y_discrete(expand=c(0.2,0))


-----------------------------
  
#charlottesville
gap_cville <- gap_cville %>% 
  transform(Race = as.character(Race))

div_black <- gap_cville %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- gap_cville %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

gap_cville <- gap_cville %>%
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
#charlottesville
  
gap_cville <- gap_cville %>%
  arrange(School.Year)

ggplot(gap_cville) +
  geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
  geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
  geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
  coord_flip() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Pass Rate")

labs(title="Achievement Gap",
     subtitle = "Charlottesville City, 2005-2019")

-----------------------------
  
#fluvanna
gap_fluv <- gap_fluv %>% 
  transform(Race = as.character(Race))

div_black <- gap_fluv %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- gap_fluv %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

gap_fluv <- gap_fluv %>%
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
#fluvanna
  
gap_fluv <- gap_fluv %>%
  arrange(School.Year)

ggplot(gap_fluv) +
  geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
  geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
  geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
  coord_flip() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Pass Rate")

labs(title="Achievement Gap",
     subtitle = "Fluvanna County, 2005-2019")


-----------------------------
  
#greene
  gap_grn <- gap_grn %>% 
  transform(Race = as.character(Race))

div_black <- gap_grn %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- gap_grn %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

gap_grn <- gap_grn %>%
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
#greene
  
gap_grn <- gap_grn %>%
  arrange(School.Year)

ggplot(gap_grn) +
  geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
  geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
  geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
  coord_flip() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Pass Rate")

labs(title="Achievement Gap",
     subtitle = "Greene County, 2005-2019")

-----------------------------
#louisa
gap_lsa <- gap_lsa %>% 
  transform(Race = as.character(Race))

div_black <- gap_lsa %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- gap_lsa %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

gap_lsa <- gap_lsa %>%
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
#louisa
  
gap_lsa <- gap_lsa %>%
  arrange(School.Year)

ggplot(gap_lsa) +
  geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
  geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
  geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
  coord_flip() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Pass Rate")

labs(title="Achievement Gap",
     subtitle = "Louisa County, 2005-2019")

-----------------------------
  
#nelson
gap_nel <- gap_nel %>% 
  transform(Race = as.character(Race))

div_black <- gap_nel %>%
  filter(Race == "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

div_white <- gap_nel %>%
  filter(Race != "Black, not of Hispanic origin") %>%
  select(School.Year, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

gap_nel <- gap_nel %>%
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
#nelson
  
  gap_nel <- gap_nel %>%
  arrange(School.Year)

ggplot(gap_nel) +
  geom_segment( aes(x=School.Year, xend=School.Year, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
  geom_point( aes(x=School.Year, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
  geom_point( aes(x=School.Year, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
  coord_flip() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Pass Rate")

labs(title="Achievement Gap",
     subtitle = "Nelson County, 2005-2019")








    
          
              
         
