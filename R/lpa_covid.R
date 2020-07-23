# This script is used to implement Latent Profile Analysis on a covid-19 dataset
# Created by Matthew Blanchard

# setup -----------------------------------------------------------
# load packages
library(tidyverse)
library(GGally) # plot variable distributions
library(naniar) # visualise missing data
library(tidyLPA) # implement LPA

# load data
d <- read_csv("data/200721_covid_lpa_data.csv")

# select variables for LPA
x <- d %>%  
  select(resilience_adapt, adaptivecoping, maladaptivecoping_reactance,
         concervatism_trust, extrv_agrebl, positive_Acceptance, g_md, Age,
         Gender, Education, PoliticalOrient, Physicalhealth, HealthConditions,
         AnnualIncome_usd, FinancialSit, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,
         CovidWorry, CheckNews, official_sources, casual_sources, SourceCheck,
         NewsShare)

# examine distributions
ggpairs(x)


# missing value analysis --------------------------------------------------
# % missing values for each variable
gg_miss_var(x, show_pct = TRUE)

# remove g_md (intelligence factor): 64% participants missing data
x <- x %>% select(-g_md)

# % missing values for each participant
gg_miss_case(x, show_pct = TRUE)


# LPA ---------------------------------------------------------------------
# identify the number of latent profiles
# run all possible models extracting 1 profile through to 6 profiles
# model 1: variances = "equal",    covarainces = "zero
# model 2: variances = "varying",  covarainces = "zero"
# model 3: variances = "equal",    covarainces = "equal"
# model 6: variances = "varying",  covarainces = "varying"

# two additional models can be run if MPlus is installed on your computer
# model 4: variances = "varying",  covarainces = "equal"
# model 5: variances = "equal",  covarainces = "varying"
x %>%
  scale() %>% 
  estimate_profiles(1:6, 
                    variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))


# implement LPA for 3 and 4 profiles using models 1 and 3 (appear to be best fit for data), separately
# this will run 4 different models
# first, set seed value so results are reproducible
seed <- 23

models <- map(c("zero", "equal"), function(cv) { # set covariance (cv)
  map(3:4, function(i) { # set number of profiles to extract (i)
    # set seed for reproducible results
    set.seed(seed) 
    
    # fit model
    fit <- x %>% 
      scale() %>% 
      estimate_profiles(i, 
                        variances = "equal",
                        covariances = cv)
    
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
    if (cv == "zero") {
      ggsave(paste0("output/test/model_1_profile_", i, "_all_seed", seed, ".png"), width = 13.5, height = 6.18)
    } else if (cv == "equal") {
      ggsave(paste0("output/test/model_3_profile_", i, "_all_seed", seed, ".png"), width = 13.5, height = 6.18)
    }
    
    # goodness of fit indices
    # get_fit(fit)
    
    # save profile scores
    get_data(fit)
    
  })
})

# collapse list structure into df and select only model details and profile scores
# Class == the profile each participant was allocated
models <- bind_rows(models) %>% 
  select(model_number, classes_number, CPROB1, CPROB2, CPROB3, CPROB4, Class)

# print the number of participants in each profile for each model
map(c(1,3), function(m) {
  map(c(3,4), function(p) {
    
    x <- bind_cols(d, models %>% filter(model_number == m & classes_number == p))
    
    # model details
    print(paste0("Model = ", m, ", Profiles = ", p))
    # number of participants in each profile
    print(x %>% select(Class) %>% table)
  })
})

# print the proportion of participants in each profile from each country for each model
map(c(1,3), function(m) {
  map(c(3,4), function(p) {
    
    x <- bind_cols(d, models %>% filter(model_number == m & classes_number == p))
    
    # model details
    print(paste0("Model = ", m, ", Profiles = ", p))
    # proportion of participants from each country in each profile
    x <- x %>% select(CountryLive, Class) %>% table
    print(prop.table(x, 2) %>% round(2))
    
  })
})

