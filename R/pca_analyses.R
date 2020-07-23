# this script replicated Sabina's EFAs conducted on the reduced covid-19 dataset (N=1391)

# load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA", "here")
purrr::map(packages, library, character.only = TRUE)

# load data
source("R/data_prep.R")

# Source Functions
source(here("R/corstarsfunction.R")) # APA style correlations
source(here("R/CalcFunctions_ML.R")) # Self-created analyses functions
source(here("R/pca_functions.R"))


# remove original PCA variables -------------------------------------------
d %>% select(-gov_trust, -resilience_adapt, -adaptivecoping, -maladaptivecoping_reactance,
             -concervatism_trust, -extrv_agrebl, -positive_Acceptance, -g_md, 
             -social_distance_isolation, -sickness_actions, -Hygiene, -complaince_selfreport,
             -official_sources, -casual_sources, -scrict_isolation, -social_distancing,
             -herd_immunity_economy, -antisocial_behaviours, -prosocial_behaviours,
             -CovidWorry)



d %>% select(contains("coping_"))

# PCAs --------------------------------------------------------------------
source("R/pca_govt.R")
source("R/pca/pca_cope_adapt_reslience.R")
source("R/pca_intelligence.R")
source("R/pca_comply.R")
source("R/pca_follow.R")
source("R/pca_news_sources.R")
source("R/pca_opinion.R")
source("R/pca_socialbehave.R")
source("R/pca_worry.R")




source("R/pca_reasonleave.R")
source("R/pca_biz_statements.R")
source("R/pca_dass.R")
source("R/pca_org_statements.R")
source("R/pca_coping.R")







