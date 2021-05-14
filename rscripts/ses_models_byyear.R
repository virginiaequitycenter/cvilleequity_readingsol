
# 1. Load libraries and data ----
library(tidyverse)
library(lme4)
library(ggeffects)


division_ses_sex <- read_csv("data/division_data/all_students_by_sex_by_advantage.csv")


# Clean Data --------------------------------------------------------------

names(division_ses_sex) <- tolower(str_replace_all(names(division_ses_sex), " ", "_"))

division_ses_sex <- division_ses_sex %>%
  mutate(
    ayear = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
  ) %>%
  mutate(across(contains("count"), ~as.numeric(str_replace_all(.x, ",", "") )   ) ) %>%
  mutate(across(contains("rate"), as.numeric)) %>%
  rename(sex= gender)
 
division_ses_sex_cohorts <-
  division_ses_sex %>%
  select(division_name, disadvantaged, sex,  ayear, grade, total = total_count, pass_count, pass = pass_rate, pass_proficient_count, pass_proficient_rate, pass_advanced_count, pass_advanced_rate ) %>%
  arrange(division_name,  disadvantaged, sex, ayear, grade) %>%
  mutate(min_grade = min(grade),
         years_from_start = grade - min_grade,
         cohort = ayear - years_from_start - 1) %>%
  arrange(division_name, disadvantaged, sex, cohort, grade, ayear) %>%
  mutate(ses_cohort = paste0(disadvantaged,"_",cohort),
         ses_division = paste0(disadvantaged, "_", division_name),
         ses_division_cohort = paste0(disadvantaged, "_", division_name, "_", cohort))



df <-    division_ses_sex_cohorts %>%
  select(division_name, disadvantaged, sex, grade, total, pass_pct = pass, ayear, cohort, num_pass  = pass_count) %>%
  mutate(num_pass = as.numeric(num_pass))

df %>%
  filter(is.na(num_pass)) %>%
  nrow()

df %>%
  mutate(missing = case_when(
    is.na(num_pass) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(ayear, disadvantaged, sex) %>%
  summarize(missing = mean(missing)) %>%
  spread(ayear, missing)

df %>%
  filter(is.na(num_pass)) %>%
  pull(division_name) %>%
  unique()

df[is.na(df)] <- 0
df$total[df$num_pass == 0] <- 0

sim_data <-
  df %>%
  uncount(total) %>%
  group_by(division_name, disadvantaged, sex, grade, pass_pct, ayear, cohort, num_pass) %>%
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

write_csv(sim_data, path = "../data/sim_ses_sex_division.csv")




# modelling ---------------------------------------------------------------

sim_data <- read_csv("../data/sim_ses_sex_division.csv")

# lme4/bayes wants factors (and sex read in as FALSE for F)
sim_data <- sim_data %>% 
  mutate(sex = if_else(sex == FALSE, "F", "M", missing = "M"),
         across(where(is.character), as.factor))


# .....................................
# 2. Estimate yearly models ----
## 2019
system.time(rand_coef_2019 <- glmer(pass ~ disadvantaged  + sex + 
                                      (1 + disadvantaged|division_name),
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
  glmer(pass ~ disadvantaged  + sex + 
          (1 + disadvantaged|division_name),
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
# predicted pass rates by disadvantaged
disadvantaged_effect2019 <- ggpredict(rand_coef_2019, terms = c("disadvantaged", "division_name"), type = "random")

# make the predicted probabilities a difference
disadvantaged_effect2019 <- disadvantaged_effect2019 %>% 
  pivot_wider(names_from = x, 
              values_from = predicted) %>% 
  mutate(Diff = N - Y,
         region = ifelse(group %in% regionlist, "1", "0"))


## Everything
# predicted pass rates by disadvantaged
by_year <- by_year %>% 
  mutate(pred = map(model, ~ggpredict(.x, terms = c("disadvantaged", "division_name"), type = "random")))

disadvantaged_effect <- bind_rows(by_year$pred, .id = "year") %>% 
  mutate(year = as.numeric(year) + 2005) %>%    # little hack to get "2006", "2007" instead of "1" and "2"
  pivot_wider(names_from = x,
              values_from = predicted) %>% 
  mutate(Diff = N - Y,
         region = ifelse(group %in% regionlist, "1", "0"),
         group = tidytext::reorder_within(group, Diff, year))



write_csv(disadvantaged_effect, path = "../data/disadvantaged_effect.csv")






# .....................................
# 4. Visualize: differences - points ----
## 2019
ggplot(disadvantaged_effect2019, aes(x = fct_reorder(group, Diff), y = Diff, color = region)) +
  geom_point(aes(size = region)) + 
  scale_color_manual(values = regioncol, guide = FALSE) +
  scale_size_manual(values=c("1" = 2, "0" = 1), guide = FALSE) +
  geom_hline(yintercept = 0) +
  coord_flip() + ylim(-0.01, 0.4) + 
  labs(title="Advantaged - Disadvantaged Pass Rates: 2019", y="Difference", x="") +
  theme(axis.text = element_text(size = 5))

## Everything (but only two at first; 14 is overwhelming)
disadvantaged_effect %>% filter(year %in% c(2009, 2019)) %>% 
  ggplot(aes(x = group, y = Diff, color = region)) +
  geom_point(aes(size = region)) + 
  scale_color_manual(values = regioncol, guide = FALSE) +
  scale_size_manual(values=c("1" = 2, "0" = 1), guide = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  geom_hline(yintercept = 0) +  
  ylim(-0.01, 0.4) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  labs(title="Advantaged - Disadvantaged Pass Rates", y="Difference", x="") +
  theme(axis.text = element_text(size = 5))

# .....................................
# 5. Visualize: differences - dumbbells ----
## 2019
ggplot(disadvantaged_effect2019) +
  geom_segment(aes(x=fct_reorder(group, Diff), xend=group, y=Black, yend=White), color="grey") +
  geom_point(aes(x=group, y=Y), color="#FFC09F", size=3 ) +
  geom_point(aes(x=group, y=N), color="#79ADDC", size=3 ) +
  coord_flip() +
  labs(x = "", y = "Advantaged and Disadvantaged Pass Rate: 2019") +
  theme(legend.position = "top",
        axis.text = element_text(size = 5)) 

## Everything
disadvantaged_effect %>% filter(year %in% c(2009, 2019)) %>% 
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


# .....................................
# 6. Save/load ----
save.image("../data/ses_models_byyear.Rdata")

load("../data/ses_models_byyear.Rdata")


