
# 1. Load libraries and data ----
library(tidyverse)
library(lme4)
library(ggeffects)

sim_data <- read_csv("../data/sim_data.csv")

# lme4/bayes wants factors (and sex read in as FALSE for F)
sim_data <- sim_data %>% 
  mutate(sex = if_else(sex == FALSE, "F", "M", missing = "M"),
         across(where(is.character), as.factor))


# .....................................
# 2. Estimate yearly models ----
## 2019
system.time(rand_coef_2019 <- glmer(pass ~ race  + sex + 
                                      (1 + race|division_name),
                                    family = binomial("logit"), 
                                    data = sim_data[sim_data$ayear == 2019,]))
# ~ 2 minutes


## Everything
# create nested data frame
by_year <- sim_data %>% 
  group_by(ayear) %>% 
  nest()

# define model
yearly_model <- function(df) {
  glmer(pass ~ race  + sex + 
    (1 + race|division_name),
  family = binomial("logit"), 
  data = df)
}

# estimate models
by_year <- by_year %>% 
  mutate(model = map(data, yearly_model))

summary(filter(by_year, ayear == 2019)$model[[1]])


# .....................................
# 3. Generate effects ----

# to highlight our region in figure
regionlist <- c("Albemarle County", "Charlottesville City", 
                "Fluvanna County", "Greene County", 
                "Louisa County", "Nelson County")

regioncol <- c("1" = "red", "0" = "black")

## 2019
# predicted pass rates by race
race_effect2019 <- ggpredict(rand_coef_2019, terms = c("race", "division_name"), type = "random")

# make the predicted probabilities a difference
race_effect2019 <- race_effect2019 %>% 
  pivot_wider(names_from = x, 
              values_from = predicted) %>% 
  mutate(Diff = White - Black,
         region = ifelse(group %in% regionlist, "1", "0"))


## Everything
# predicted pass rates by race
by_year <- by_year %>% 
  mutate(pred = map(model, ~ggpredict(.x, terms = c("race", "division_name"), type = "random")))
  
race_effect <- bind_rows(by_year$pred, .id = "year") %>% 
  mutate(year = as.numeric(year) + 2005) %>%    # little hack to get "2006", "2007" instead of "1" and "2"
  pivot_wider(names_from = x,
              values_from = predicted) %>% 
  mutate(Diff = White - Black,
         region = ifelse(group %in% regionlist, "1", "0"),
         group = tidytext::reorder_within(group, Diff, year))




write_csv(race_effect, path = "../data/race_effect.csv")



# .....................................
# 4. Visualize: differences - points ----
## 2019
ggplot(race_effect2019, aes(x = fct_reorder(group, Diff), y = Diff, color = region)) +
  geom_point(aes(size = region)) + 
  scale_color_manual(values = regioncol, guide = FALSE) +
  scale_size_manual(values=c("1" = 2, "0" = 1), guide = FALSE) +
  geom_hline(yintercept = 0) +
  coord_flip() + ylim(-0.01, 0.4) + 
  labs(title="White - Black Pass Rates: 2019", y="Difference", x="") +
  theme(axis.text = element_text(size = 5))

## Everything (but only two at first; 14 is overwhelming)
race_effect %>% filter(year %in% c(2009, 2019)) %>% 
ggplot(aes(x = group, y = Diff, color = region)) +
  geom_point(aes(size = region)) + 
  scale_color_manual(values = regioncol, guide = FALSE) +
  scale_size_manual(values=c("1" = 2, "0" = 1), guide = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  geom_hline(yintercept = 0) +  
  ylim(-0.01, 0.4) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  labs(title="White - Black Pass Rates", y="Difference", x="") +
  theme(axis.text = element_text(size = 5))


# .....................................
# 5. Visualize: differences - dumbbells ----
## 2019
ggplot(race_effect2019) +
  geom_segment(aes(x=fct_reorder(group, Diff), xend=group, y=Black, yend=White), color="grey") +
  geom_point(aes(x=group, y=Black), color="#FFC09F", size=3 ) +
  geom_point(aes(x=group, y=White), color="#79ADDC", size=3 ) +
  coord_flip() +
  labs(x = "", y = "Black and White Pass Rate: 2019") +
  theme(legend.position = "top",
        axis.text = element_text(size = 5)) 

## Everything
race_effect %>% filter(year %in% c(2009, 2019)) %>% 
  ggplot() +
  geom_segment(aes(x=fct_reorder(group, Diff), xend=group, y=Black, yend=White), color="grey") +
  geom_point(aes(x=group, y=Black), color="#FFC09F", size=2 ) +
  geom_point(aes(x=group, y=White), color="#79ADDC", size=2 ) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Black and White Pass Rates") +
  theme(legend.position = "top",
        axis.text = element_text(size = 5)) 

# harder to see...



########### Swap to Just Race ##############
# run model with just race ------------------------------------------------

sim_data_race <- read_csv("../data/sim_data_race.csv")


# .....................................
# 2. Estimate yearly models ----

# create nested data frame
by_year_race <- sim_data_race %>% 
  group_by(ayear) %>% 
  nest()

# define model
yearly_model_race <- function(df) {
  glmer(pass ~ race  + 
          (1 + race|division_name),
        family = binomial("logit"), 
        data = df)
}

# estimate models
by_year_race <- by_year_race %>% 
  mutate(model = map(data, yearly_model_race))

summary(filter(by_year_race, ayear == 2019)$model[[1]])


# .....................................
# 3. Generate effects ----

# to highlight our region in figure
regionlist <- c("Albemarle County", "Charlottesville City", 
                "Fluvanna County", "Greene County", 
                "Louisa County", "Nelson County")

regioncol <- c("1" = "red", "0" = "black")


## Everything
# predicted pass rates by race
by_year_race <- by_year_race %>% 
  mutate(pred = map(model, ~ggpredict(.x, terms = c("race", "division_name"), type = "random")))

race_effect_no_sex <- bind_rows(by_year_race$pred, .id = "year") %>% 
  mutate(year = as.numeric(year) + 2005) %>%    # little hack to get "2006", "2007" instead of "1" and "2"
  pivot_wider(names_from = x,
              values_from = predicted) %>% 
  mutate(Diff = White - Black,
         region = ifelse(group %in% regionlist, "1", "0"),
         group = tidytext::reorder_within(group, Diff, year))


# .....................................
# 6. Save/load ----
save.image("../data/models_byyear.Rdata")
# load("../data/models_byyear.Rdata")



# Compare Predictions -----------------------------------------------------
compare_effects <- 
race_effect %>% 
  rename_with(.cols = (Black:Diff), .fn = ~paste0(.x, "_sex")) %>%
  full_join(
race_effect_no_sex %>%
  rename_with(.cols = (Black:Diff), .fn = ~paste0(.x, "_nosex"))
) %>%
  mutate(division = gsub("__.+$", "",group)) %>%
  gather( stat, number, -year, -group, -division, -region) %>%
  separate(stat, c("stat", "standardized")) %>%
  spread(standardized, number) %>%
  mutate(bias = sex - nosex)
  
ggplot(compare_effects, aes(x = bias*100)) +
  geom_histogram() +
  facet_wrap(~stat)

compare_effects %>%
  group_by(stat) %>%
  summarize(mean = mean(bias, na.rm = TRUE)* 100)

compare_effects %>%
  filter(is.na(sex))

 
