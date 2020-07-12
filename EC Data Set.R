read.csv(educationgap csv)
education_equity <- read.csv(educationgap_csv.csv)
education_equity <- read.csv(educationgap csv.csv)

#Set Up!
library(dplyr)
library(sf)
library(dplyr)
library(readr)
library(stringr)

educationgap_csv <- read_csv("educationgap csv.csv")
View(educationgap_csv)
no_state <- educationgap_csv %>% filter('Level'='Division')
no_state <- educationgap_csv %>% filter('Level'=='Division')
View(no_state)
no_state <- educationgap_csv %>% filter(level == "Division")
View(educationgap_csv)

# No Division

# Playground
no_division_number <- educationgap_csv %>% select(-'Division Number')
filter('Division Name'== 'Charlottesville City')
cville_pass <- educationgap_csv %>% filter('Division Name'=='Charlottesville City')
View(cville_pass)
relevant_files <- educationgap_csv %>% select('School Year','Level','Division Name','Subject','Race','Test Level','Pass Count','Total Count','Pass Rate')
View(relevant_files)
cville_city <- relevant_files %>% filter('Division Name' == 'Charlottesville City')
View(cville_city)
educationgap <- educationgap_csv %>%
average_pr <- educationgap_csv %>% average('Pass Rate')
pr_black <- educationgap_csv %>% mutate(filter(`Race`='Black, not of Hispanic origin'))
county <- educationgap_csv %>% rename('County'=='Division Name')
county <- educationgap_csv %>% rename('County' ='Division Name')
View(county)
educationgap_csv <- select(School Year, Level)
educationgap_csv <- select(School_Year, Level, Division_Number, County, Race, Test_Level, Pass_Count, Total_Count, Pass_Rate)
educationgap_csv <- select('School Year', 'Level', 'Division Number', 'County', 'Race', 'Test Level', 'Pass Count', 'Total Count', 'Pass Rate')
rm(county)
rm(cville_city,cville_pass,educationgap_csv,no_division_number,no_state,relevant_files)
educationgap <- read.csv("~/Desktop/EC Educational Equity Data/educationgap.csv")
View(educationgap)
school_allrace <- read.csv("~/Desktop/EC Educational Equity Data/school_allrace.csv")
View(school_allrace)
school_blackwhite <- read.csv("~/Desktop/EC Educational Equity Data/school_blackwhite.csv")
View(school_blackwhite)
disadvantaged <- read.csv("~/Desktop/EC Educational Equity Data/disadvantaged.csv", header=FALSE)
View(disadvantaged)
foster <- read.csv("~/Desktop/EC Educational Equity Data/foster.csv")
View(foster)
homeless <- read.csv("~/Desktop/EC Educational Equity Data/homeless.csv")
View(homeless)
migrant <- read.csv("~/Desktop/EC Educational Equity Data/migrant.csv")
View(migrant)
disabled <- read.csv("~/Desktop/EC Educational Equity Data/disabled.csv")
View(disabled)


#State Data
state <- educationgap %>% select('Division.Number')



#State Data
state <- educationgap %>% select('Division.Number')
state <- educationgap %>% filter(Level == 'State')
View(state)
state <- state %>% select(-Division.Number,-Division.Name)
state <- state %>% select(-Grade,-Test.Source)
View(state)
state <- state %>% spread(Race,Pass.Rate)
library(dplyr)
state <- state %>% spread(Race, Pass.Rate)


#Division
View(school_allrace)
View(school_blackwhite)
division <- educationgap %>% filter(Level == 'Division')
View(division)
div_alb <- division %>% filter(Division.Number == '2')
View(div_alb)
div_alb <- div_alb %>% spread(Race,Pass.Rate)


#Charlottesville Division
div_cville <- division %>% filter(Division.Number == '104')
div_cville <- division %>% spread(Race,Pass.Rate)
View(div_cville)
View(div_alb)
div_cville <- division %>% filter(Division.Number =='104')
rm(div_cville)

#Let's Try This Again
div_cville <- division %>% filter(Division.Name == 'Charlottesville City')
View(div_cville)
div_cville <- div_cville %>% spread(Race,Pass.Rate)
div_cville <- div_cville %>% select(-Level,-Grade)
div_alb <- div_alb %>% select(-Level,-Grade)
division <- division %>% select(-Level,-Grade)
View(division)


#Fluvanna
div_fluv <- division %>% filter(Division.Name == 'Fluvanna County')
View(div_fluv)
div_fluv <- div_fluv %>% spread(Race, Pass.Rate)


#Greene
div_greene <- division
div_greene <- division %>% filter(Division.Name == 'Greene County')
div_greene <- div_greene %>% spread(Race, Pass.Rate)
View(div_greene)


#Louisa
div_louisa <- division %>% filter(Division.Name == 'Louisa County')
div_louisa <- div_louisa %>% spread(Race, Pass.Rate)
View(div_louisa)


#Nelson
div_nelson <- division %>% filter(Division.Name == 'Nelson County')
div_nelson <- div_nelson %>% spread(Race, Pass.Rate)


#School
school_allrace <- school_allrace %>% filter(Level == 'School')
school
school_blackwhite <- school_blackwhite %>% filter(Level == 'School')
View(school_blackwhite)


#Albemarle Schools
#Select(-) Schools
school_allrace <- school_allrace %>% select(-Grade)
school_blackwhite <- school_blackwhite %>% select(-Grade)
#school_alb <- school_allrace %>% filter(Division.Number == '2')
school_alb <- school_allrace %>% filter(Division.Number == '2')
View(school_alb)
school_alb <- school_blackwhite %>% filter(Division.Number == '2')
rm(school_alb)
rm(school_alb)


#Albemarle
school_alb <- school_allrace %>% filter(Division.Number == '2')
View(school_alb)
school_alb_race <- school_blackwhite %>% filter(Division.Number =='2')
View(school_alb_race)
school_alb_race <- school_alb_race %>% spread(Race,Pass.Rate)


#Charlottesville
school_cville <- school_allrace %>% filter(Division.Number == '104')
> school_cville_race <- school_blackwhite %>% filter(Division.Number =='104')
> school_cville_race <- school_cville_race %>% spread(Race,Pass.Rate)
View(school_cville)
school_cville_race <- school_blackwhite %>% filter(Division.Number == '104')
View(school_cville_race)
school_cville_race <- school_cville_race %>% spread(Race,Pass.Rate)
> View(school_alb_race)


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
View(school_louisa)
View(school_louisa_race)
school_louisa_race <- school_louisa_race %>% spread(Race,Pass.Rate)
school_louisa_race <- school_blackwhite %>% filter(Division.Number == '54')
school_louisa_race <- school_louisa_race %>% spread(Race,Pass.Rate)


#Nelson
school_nelson <- school_allrace %>% filter(Division.Number == '62')
school_nelson_race <- school_blackwhite %>% filter(Division.Number == '62')
school_nelson_race <- school_nelson_race %>% spread(Race,Pass.Rate)
View(school_nelson_race)
View(div_greene)
View(div_alb)
library(dplyr)
