# Models on simulated data

library(tidyverse)
library(lme4)
library(sjPlot)
library(ggeffects)

sim_data <- read_csv("data/sim_data.csv")

# lme4/bayes wants factors (and sex read in as FALSE for F)
sim_data <- sim_data %>% 
  mutate(sex = if_else(sex == FALSE, "F", "M", missing = "M"),
         across(where(is.character), as.factor))


# exploratory ----
# # playing with sim_data; thinking about comparing change over time...
sim_data %>% group_by(race, grade) %>% summarize(mean_pass = mean(pass)) %>%
  ggplot(aes(x = grade, y = mean_pass, color = race)) + geom_line()

sim_data %>% group_by(race, division_name, grade) %>%
  summarize(mean_pass = mean(pass)) %>%
  ggplot(aes(x = jitter(grade), y = mean_pass, color = race)) +
  geom_point(alpha = 1/5) +
  geom_smooth()


# Initial models ----
system.time(rand_int <- glmer(pass ~ race + (1|division_name),
                              family = binomial("logit"),
                              data = sim_data))
# ~ 8 minutes
summary(rand_int)


# Building on models ----
# Random effect of race by division (sex, test change): sampled data
system.time(rand_coef <- glmer(pass ~ race  + sex + post_2012 +
                              (1 + race|division_name),
                            family = binomial("logit"),
                            data = sim_data[sample(1:nrow(sim_data), 1000000 ),]))
# ~ 30+ seconds for 100K; ~ for 1M; estimate 20 minutes for full set
re <- ranef(rand_coef)
rc <- coef(rand_coef)
summary(rc$division_name$raceWhite)
summary(rc$division_name$`(Intercept)`)

# save.image("rscripts/rc_model.Rdata")
# load("rscripts/rc_model.Rdata")

# with sjPlot
plot_model(rand_coef, type = "pred", term = "race") # marginal effect of race
plot_model(rand_coef, type = "re") # random coef by division
plot_model(rand_coef, type = "re", transform = "plogis") # probabilities by division
plot_model(rand_coef, type = "re", sort.est = "sort.all", grid = FALSE) # random coef by division

# with ggpredict
col_vec <- c("grey", "red", rep("gray", 130))
ggpredict(rand_coef, terms = c("race", "division_name"), type = "random") %>% plot(colors = col_vec, show.legend = FALSE, connect.lines = TRUE)

ggpredict(rand_coef, terms = c("race", "division_name [Charlottesville City,Albemarle County,Greene County,Fluvanna County,Louisa County,Nelson County]"), type = "random") %>%
  plot(connect.lines = TRUE)

# use ggplot
me <- ggpredict(rand_coef, terms = c("race", "division_name"), type = "random")

library(ggbeeswarm)
ggplot(me, aes(x = x, y = predicted)) +
  geom_jitter(width = .2)

# make the predicted probabilites a difference
me_wide <- pivot_wider(me, names_from = x, values_from = predicted)
me_wide <- me_wide %>% mutate(Diff = White - Black)
ggplot(me_wide, aes(x = fct_reorder(group, Diff), y = Diff)) + geom_point() + coord_flip() + geom_line(aes(x=0)) +
  theme(axis.text = element_text(size = 5))
