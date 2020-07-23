#data cleaning
test_level_alb <- test_level %>% 
   transform(Race = as.character(Race))

tl_black_a <- test_level_alb %>%
   filter(Race == "Black, not of Hispanic origin") %>%
   filter(Division.Name == 'Albemarle County') %>%
   filter(School.Year == '2018-2019') %>%
   select(School.Year, Test.Level, Division.Name, Black_Pass_Count = Pass.Count, Black_Pass_Total = Total.Count, Black_Pass_Rate = Pass.Rate)

tl_white_a <- test_level_alb %>%
   filter(Race != "Black, not of Hispanic origin") %>%
   filter(Division.Name == 'Albemarle County') %>%
   filter(School.Year == '2018-2019') %>%
   select(School.Year, Test.Level, Division.Name, White_Pass_Count = Pass.Count, White_Pass_Total = Total.Count, White_Pass_Rate = Pass.Rate)

test_level_alb <- test_level_alb %>%
    select(School.Year, Division.Name, Test.Level) %>%
    distinct() %>%
    left_join(tl_black_a) %>%
    left_join(tl_white_a) %>%
    as.tibble() %>%
    transform(White_Pass_Rate = as.numeric(as.character(White_Pass_Rate)), 
              Black_Pass_Rate = as.numeric(as.character(Black_Pass_Rate))) %>%
         mutate(Achievement_Gap = White_Pass_Rate - Black_Pass_Rate)
    

#dumbbell
test_level_alb <- test_level_alb %>%
   arrange(Test.Level)

ggplot(test_level_alb) +
   geom_segment( aes(x=Test.Level, xend=Test.Level, y=Black_Pass_Rate, yend=White_Pass_Rate), color="grey") +
   geom_point( aes(x=Test.Level, y=Black_Pass_Rate), color="#79ADDC", size=3 ) +
   geom_point( aes(x=Test.Level, y=White_Pass_Rate), color="#FFC09F", size=3 ) +
   coord_flip() +
   theme(
    legend.position = "none",) +
        xlab("") +
        ylab("Pass Rate")