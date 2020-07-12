education_equity <- read.csv(educationgap_csv.csv)


#Set Up!
library(dplyr)
library(sf)
library(dplyr)
library(readr)
library(stringr)


#CSV
educationgap <- read.csv("~/Desktop/EC Educational Equity Data/educationgap.csv")
school_allrace <- read.csv("~/Desktop/EC Educational Equity Data/school_allrace.csv")
school_blackwhite <- read.csv("~/Desktop/EC Educational Equity Data/school_blackwhite.csv")
disadvantaged <- read.csv("~/Desktop/EC Educational Equity Data/disadvantaged.csv", header=FALSE)
foster <- read.csv("~/Desktop/EC Educational Equity Data/foster.csv")
homeless <- read.csv("~/Desktop/EC Educational Equity Data/homeless.csv")
migrant <- read.csv("~/Desktop/EC Educational Equity Data/migrant.csv")
disabled <- read.csv("~/Desktop/EC Educational Equity Data/disabled.csv")


------------------------------------------------------------------
#Pass Rates by State 
  
#State Data
state <- educationgap %>% select('Division.Number')
state <- educationgap %>% filter(Level == 'State')
state <- state %>% select(-Division.Number,-Division.Name)
state <- state %>% select(-Grade,-Test.Source)
state <- state %>% spread(Race,Pass.Rate)
state <- state %>% spread(Race, Pass.Rate)

------------------------------------------------------------------
#Pass Rates by Division 
  
#Albemarle
div_alb <- division %>% filter(Division.Number == '2')
div_alb <- div_alb %>% spread(Race,Pass.Rate)


#Charlottesville 
div_cville <- division %>% filter(Division.Number == '104')
div_cville <- division %>% spread(Race,Pass.Rate)
div_cville <- division %>% filter(Division.Number =='104')


#Fluvanna
div_fluv <- division %>% filter(Division.Name == 'Fluvanna County')
div_fluv <- div_fluv %>% spread(Race, Pass.Rate)


#Greene
div_greene <- division
div_greene <- division %>% filter(Division.Name == 'Greene County')
div_greene <- div_greene %>% spread(Race, Pass.Rate)


#Louisa
div_louisa <- division %>% filter(Division.Name == 'Louisa County')
div_louisa <- div_louisa %>% spread(Race, Pass.Rate)


#Nelson
div_nelson <- division %>% filter(Division.Name == 'Nelson County')
div_nelson <- div_nelson %>% spread(Race, Pass.Rate)


#Pass Rates by School
school_allrace <- school_allrace %>% filter(Level == 'School')
school
school_blackwhite <- school_blackwhite %>% filter(Level == 'School')


#Albemarle
school_alb <- school_allrace %>% filter(Division.Number == '2')
school_alb_race <- school_blackwhite %>% filter(Division.Number =='2')
school_alb_race <- school_alb_race %>% spread(Race,Pass.Rate)


#Charlottesville
school_cville <- school_allrace %>% filter(Division.Number == '104')
school_cville_race <- school_blackwhite %>% filter(Division.Number == '104')
school_cville_race <- school_cville_race %>% spread(Race,Pass.Rate)



#Fluvanna
school_fluv <- school_allrace %>% filter(Division.Number == '32')
school_fluv_race <- school_blackwhite %>% filter(Division.Number == '32')
school_fluv_race <- school_fluv_race %>% spread(Race,Pass.Rate)


#Greene
school_greene <- school
school_greene <- school_allrace %>% filter(Division.Number == '39')
school_greene_race <- school_blackwhite %>% filter(Division.Number== '39')
school_greene_race <- school_greene_race %>% spread(Race,Pass.Rate)


#Louisa
school_louisa <- school_allrace %>% filter(Division.Number == '54')
school_louisa_race <- school_allrace %>% filter(Division.Number == '54')
school_louisa_race <- school_louisa_race %>% spread(Race,Pass.Rate)


#Nelson
school_nelson <- school_allrace %>% filter(Division.Number == '62')
school_nelson_race <- school_blackwhite %>% filter(Division.Number == '62')
school_nelson_race <- school_nelson_race %>% spread(Race,Pass.Rate)

------------------------------------------------------------------
#Achievement Gap



------------------------------------------------------------------
#Cohort Analysis 



------------------------------------------------------------------
#School Comparison 






