# this script replicates Sabina's EFAs conducted on the covid-19 dataset

# load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA")
purrr::map(packages, library, character.only = TRUE)

# load data
# full, reduced,or marvin dataset?
data <- "marvin"

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
} else if (data == "marvin") {
  d <- read_csv("data/COVID-19_FourCountries_BC_G_Removed_Imputed-Merged_V3.csv") %>% 
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
source("R/pca/pca_trust.R")


# additional variables ----------------------------------------------------
# wherework: turn into single variable
work <- d %>% 
  select(id, Wherework_1:Wherework_3) %>% 
  pivot_longer(names_to = "var", values_to = "val", cols = -id) %>% 
  group_by(id) %>% 
  mutate(n = sum(val, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(val = ifelse(n == 2 & var == "Wherework_2", NA, val),
         Wherework = ifelse(n == 2, "organisation+self_employ",
                            ifelse(var == "Wherework_1" & val == 1, "organisation",
                                   ifelse(var == "Wherework_2" & val == 1, "self_employ",
                                          ifelse(var == "Wherework_3" & val == 1, "none", NA))))) %>% 
  filter(!is.na(val)) %>% select(-var, -val, -n)


# LossofJob turn into single variable
loss <- d %>%
  select(id, LossofJob_1:LossofJob_3) %>% 
  pivot_longer(names_to = "var", values_to = "val", cols = -id) %>% 
  group_by(id) %>% 
  mutate(n = sum(val, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(val = ifelse(n == 2 & var == "LossofJob_2", NA, val),
         LossofJob = ifelse(n == 2, "laid_off+biz_closed",
                            ifelse(var == "LossofJob_1" & val == 1, "laid_off",
                                   ifelse(var == "LossofJob_2" & val == 1, "biz_closed",
                                          ifelse(var == "LossofJob_3" & val == 1, "none", NA))))) %>% 
  filter(!is.na(val)) %>% select(-var, -val, -n)

# SocialMedia turn into single variable
remove <- c("buzzfeed", "google", "abc", "tot", "news", "spotify", "bbc", "line")
double <- c("tumblr, tiktok", "4chan.org/pol", "linkedin, telegram")

social <- d %>%
  select(id, contains("SocialMedia")) %>%
  mutate(SocialMedia_8_TEXT = tolower(SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(SocialMedia_8_TEXT == "snapchat news stories", "snapchat", SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(str_detect(SocialMedia_8_TEXT, paste(remove, collapse = "|")), NA, SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(str_detect(SocialMedia_8_TEXT, paste(double, collapse = "|")), 2, SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(SocialMedia_8_TEXT == "2" | is.na(SocialMedia_8_TEXT), SocialMedia_8_TEXT, 1),
         SocialMedia_8_TEXT = as.numeric(SocialMedia_8_TEXT)) %>%
  gather(var, val, -id) %>%
  group_by(id) %>%
  summarise(social_media_n = sum(val, na.rm=T))

# fines
# exchange rates:
# 1 GBP == 1.29 USD
# 1 AUS == .71 USD
# 1 CAN == .74
# fines_usd <- d %>% 
#   select(id, CountryLive, Fines_gath_1_1, Fines_sympt_1_1) %>% 
#   pivot_longer(names_to = "var", values_to = "val", cols = c(Fines_gath_1_1, Fines_sympt_1_1)) %>% 
#   mutate(val = as.numeric(val),
#          val = ifelse(CountryLive == "Australia", val*.71,
#                       ifelse(CountryLive == "Canada", val*.74,
#                              ifelse(CountryLive == "UK", val*1.29, val)))) %>% 
#   pivot_wider(names_from = var, values_from = val) %>% 
#   select(-CountryLive) %>% 
#   rename(Fines_gath_usd = Fines_gath_1_1, Fines_sympt_usd = Fines_sympt_1_1)

# combine with main dataset
# datalist <- list(scores, work, loss, social, pca_scores)
datalist <- list(d, work, loss, social)

d <- datalist %>% reduce(left_join, by = "id")

# impute missing values for LPA -------------------------------------------
# select LPA variables to impute NA
x <- d %>%  
  select(Age, Education, Physicalhealth, HealthConditions, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,CovidWorry)

# missing value analysis
gg_miss_var(x, show_pct = TRUE)

# impute NA and standardise vars
if (any(data == c("reduced", "full"))) {
  x <- x %>%
    mclust::imputeData(seed = 1) %>% 
    scale() %>% 
    as_tibble()
  
  # join imputed variables to dataset
  name <- names(x)
  impute_d <- bind_cols(d %>% select(-all_of(name)), x)
  
} else if (data == "marvin") {
  #scale vars
  x <- x %>% 
    mutate_if(is.numeric, scale)
  
  # join standardised variables to dataset
  name <- names(x)
  scale_d <- bind_cols(d %>% select(-all_of(name)), x)
}

# save dataset
if (data == "reduced") {
  impute_d %>% write_csv("data/200727_covid_reduced_imputed_std_data.csv")

} else if (data == "full") {
  impute_d %>% write_csv("data/200728_covid_full_imputed_std_data.csv")
  
} else if (data == "marvin") {
  scale_d %>% write_csv("data/200805_covid_full_imputed_std_data.csv")
  }

# clean environment
# rm(list = setdiff(ls(), c("d")))

# PCAs not conducted
# source("R/pca_reasonleave.R")
# source("R/pca_biz_statements.R")
# source("R/pca_dass.R")
# source("R/pca_org_statements.R")







