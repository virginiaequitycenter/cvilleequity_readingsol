
library(tidyverse)
library(rstanarm)
library(ggeffects)

sim_data <- read_csv("data/sim_data.csv")

# lme4/bayes wants factors (and sex read in as FALSE for F)
sim_data <- sim_data %>% 
  mutate(sex = if_else(sex == FALSE, "F", "M", missing = "M"),
         across(where(is.character), as.factor))

# switch to bayesian ----
# fixed effects only first, to troubleshoot
# https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html#markov-chains-did-not-converge
glm_bayes <- stan_glm(formula = pass ~ race + sex + post_2012 + division_name,
                      data = sim_data[sample(1:nrow(sim_data), 1000000 ),],
                      chains = 4, iter = 2000,
                      seed = 1017)
# that works (though suggests more iterations or chains); 
# full data? nope...
glm_bayes <- stan_glm(formula = pass ~ race + sex + post_2012 + division_name,
                      data = sim_data,
                      chains = 4, iter = 2000,
                      seed = 1017)


# rcm version, data sampled
options(mc.cores = parallel::detectCores())
chains <- 4
iters <- 2000
warmup <- 1000
bseed <- 101706

rcm_bayes <- stan_glmer(formula = pass ~ race + sex + post_2012 + 
                          (1 + race|division_name),
                        prior = normal(location = 0, 
                                       scale = 100,
                                       autoscale = FALSE),
                        prior_intercept = normal(location = 0, 
                                                 scale = 100, 
                                                 autoscale = FALSE),
                        data = sim_data[sample(1:nrow(sim_data), 1000000 ),],
                        seed = bseed, chains = chains, 
                        iter = iters, warmup = warmup,
                        save_warmup = FALSE)
