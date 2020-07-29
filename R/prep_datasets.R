# this script replicates Sabina's EFAs conducted on the reduced covid-19 dataset (N=1391)

# load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA")
purrr::map(packages, library, character.only = TRUE)

# load data
# full or reduced data set?
data <- "full"

if (data == "reduced") {
# for reduced data (N=1361)
source("R/reduced_data_prep.R")
  
} else if (data == "full") {
# # for full data (N~1700)
d <- read_csv("data/COVID19_all_samples_after_cleaning.csv", guess_max = 1700) %>%
# make CountryLive and sample factors with labels
  filter(CountryLive %in% c(9, 31, 185, 187)) %>%
  mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
         sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public")))
}

# Source Functions
# source("R/corstarsfunction.R") # APA style correlations
# source("R/CalcFunctions_ML.R") # Self-created analyses functions
source("R/pca_functions.R")

# remove original PCA variables -------------------------------------------
d <- d %>% select(-gov_trust, -resilience_adapt, -adaptivecoping, -maladaptivecoping_reactance,
                  -concervatism_trust, -extrv_agrebl, -positive_Acceptance, -g_md, 
                  -social_distance_isolation, -sickness_actions, -Hygiene, -complaince_selfreport,
                  -scrict_isolation, -social_distancing, -herd_immunity_economy, 
                  -antisocial_behaviours, -prosocial_behaviours, -CovidWorry)

# PCAs --------------------------------------------------------------------
source("R/pca/pca_govt.R")
source("R/pca/pca_cope_adapt_reslience.R")
source("R/pca/pca_intelligence.R")
source("R/pca/pca_comply.R")
source("R/pca/pca_follow.R")
source("R/pca/pca_news_sources.R")
source("R/pca/pca_opinion.R")
source("R/pca/pca_socialbehave.R")
source("R/pca/pca_worry.R")


# impute missing values for LPA -------------------------------------------
# select LPA variables to impute NA
x <- d %>%  
  select(Age, Gender, Education, Physicalhealth, HealthConditions, AnnualIncome_usd, 
         FinancialSit, resilience_adapt, adaptivecoping, maladaptivecoping_reactance,
         concervatism_trust, extrv_agrebl, positive_Acceptance, scrict_isolation, 
         social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,
         CovidWorry, CheckNews, official_sources, casual_sources, 
         SourceCheck, NewsShare,
         -Gender, -Education, -AnnualIncome_usd, -FinancialSit, -SourceCheck, 
         -NewsShare, -CheckNews)

# impute NA
x <- x %>%
  mclust::imputeData(seed = 1) %>% 
  scale() %>% 
  as_tibble()

# join imputed variables to dataset
name <- names(x)
impute_d <- bind_cols(d %>% select(-all_of(name)), x)

# save dataset
if (data == "reduced") {
  impute_d %>% write_csv("data/200727_covid_reduced_imputed_std_data.csv")

} else if (data == "full") {
  impute_d %>% write_csv("data/200728_covid_full_imputed_std_data.csv")
  }

# clean environment
# rm(list = setdiff(ls(), c("d")))

# PCAs not conducted
# source("R/pca_reasonleave.R")
# source("R/pca_biz_statements.R")
# source("R/pca_dass.R")
# source("R/pca_org_statements.R")







