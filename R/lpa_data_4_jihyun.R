# save LPA dataset for Jihyun

# load data
source("R/data_prep.R")

# Conduct PCA on news source variables ------------------------------------
source("R/pca_news_sources.R")

# select LPA variables to impute and standardise
x <- d %>%  
  select(resilience_adapt, adaptivecoping, maladaptivecoping_reactance,
         concervatism_trust, extrv_agrebl, positive_Acceptance, Age,
         Gender, Education, PoliticalOrient, Physicalhealth, HealthConditions,
         AnnualIncome_usd, FinancialSit, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,
         CovidWorry, CheckNews, official_sources, casual_sources, SourceCheck,
         NewsShare)

# save data for Jihyun
c <- x %>%
  mclust::imputeData(seed = 1) %>% 
  as_tibble()

name <- names(c)

c <- bind_cols(d %>% select(-name), c)

c %>% write_csv("data/200721_covid_lpa_data.csv")
