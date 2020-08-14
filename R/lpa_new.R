# This script is used to implement Latent Profile Analysis on a covid-19 dataset
# Created by Matthew Blanchard

# setup -----------------------------------------------------------
# load packages
library(tidyverse)
library(tidyLPA) # implement LPA
# library(GGally) # plot variable distributions
# library(naniar) # visualise missing data

# load data
# read_csv() uses the first 1000 columns to determine the column type (e.g., numeric, character)
# if the first 1000 rows contain NA then it will guess the column is a logical vector.
# As some variables have NA in the first 1000 rows this creates incorrect column types and 
# alters the data for these columns. Add guess_max = 1361 to resolve this
d <- read_csv("data/200805_covid_full_imputed_std_data.csv", guess_max = 1575)

# select variables for LPA
x <- d %>%  
  select(Age, Education, Physicalhealth, HealthConditions, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,CovidWorry)

# examine variable distributions
# ggpairs(x)


# # missing value analysis --------------------------------------------------
# # % missing values for each variable
# gg_miss_var(x, show_pct = TRUE)
# 
# # % missing values for each participant
# gg_miss_case(x, show_pct = TRUE)


# LPA ---------------------------------------------------------------------
# identify the number of latent profiles
# run all possible models - each model treats the variance and covariance differently
# model 1: variances = "equal",    covarainces = "zero
# model 2: variances = "varying",  covarainces = "zero"
# model 3: variances = "equal",    covarainces = "equal"
# model 6: variances = "varying",  covarainces = "varying"

# two additional models can be run if MPlus is installed on your computer
# model 4: variances = "varying",  covarainces = "equal"
# model 5: variances = "equal",  covarainces = "varying"
fit <- x %>%
  scale() %>% 
  estimate_profiles(2:6, 
                    variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying")) %>%
  compare_solutions(statistics = c("LogLik", "AIC", "BIC", "Entropy"))


fit$fits %>%
  select(Model:AIC, BIC, Entropy, BLRT_val, BLRT_p) %>%
  mutate(across(where(is.numeric), round, 3))

# implement LPA for 2-4 profile using models 1 and 3 (appear to be best fit for data), separately
# this will run 6 different models
# first, set seed value so results are reproducible
seed <- 23

models <- map(c("zero", "equal"), function(cv) { # set covariance (cv)
  models <-   map(2:4, function(i) { # set number of profiles to extract (i)
    # set seed for reproducible results
    set.seed(seed) 
    
    # fit model
    fit <- x %>% 
      scale() %>% 
      estimate_profiles(i, 
                        variances = "equal",
                        covariances = "zero")
    
    # plot profiles
    fit %>% 
      plot_profiles(ci = 0.95,
                    sd = FALSE,
                    add_line = TRUE,
                    rawdata = FALSE,
                    bw = FALSE,
                    alpha_range = c(0, 0.1)) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    # save plot
    # if (cv == "zero") {
      ggsave(paste0("output/model_1_profile_", i, "_seed_", seed, ".png"), width = 13.5, height = 6.18)
    # } else if (cv == "equal") {
    #   ggsave(paste0("output/model_3_profile_", i, "_seed_", seed, ".png"), width = 13.5, height = 6.18)
    # }
    
    # goodness of fit indices
    get_fit(fit)

    # save profile scores
    get_data(fit)
    
  })
})


# collapse list structure into df and select only model details and profile scores
# Class == the profile each participant was allocated
models_bind <- bind_rows(models) %>% 
  select(model_number, classes_number, CPROB1, CPROB2, CPROB3, CPROB4, Class)

# print the number of participants in each profile for each model
map(c(1,3), function(m) {
  map(c(2:6), function(p) {
    
    x <- bind_cols(d, models_bind %>% filter(model_number == m & classes_number == p))
    
    # model details
    print(paste0("Model = ", m, ", Profiles = ", p))
    # number of participants in each profile
    print(x %>% select(Class) %>% table)
  })
})

# print the proportion of participants in each profile from each country for each model
map(c(1,3), function(m) {
  map(c(2:6), function(p) {
    
    x <- bind_cols(d, models_bind %>% filter(model_number == m & classes_number == p))
    
    # model details
    print(paste0("Model = ", m, ", Profiles = ", p))
    # proportion of participants from each country in each profile
    x <- x %>% select(CountryLive, Class) %>% table
    print(prop.table(x, 2) %>% round(2))
    
  })
})

