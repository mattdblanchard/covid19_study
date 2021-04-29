library(tidyverse)
library(psych)
library(kableExtra)

source("R/pca_functions.R")

# load data
# Raw data for demogrphics and descriptives
d <- read_csv("data/COVID19_all_samples_after_cleaning.csv") %>% 
  mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
         sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public"))) %>% 
         # Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female"))
  rename(CovidWorry_Fac = CovidWorry, BC_AB_Fac = social_distance_isolation, 
         BC_MIB_Fac = sickness_actions, BC_PB_Fac = Hygiene, 
         REFB_Fac = social_distancing, PBEB_Fac = scrict_isolation,
         PBAB_Fac = herd_immunity_economy)

# PCA ---------------------------------------------------------------------
# resilience adaptability
# select variables
x <- d %>% select(resilience, adaptability_crisis, adaptability_uncertainty)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1265)))

# scree plot
scree(x)

# 6-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(ResAdapt_Fac = PC1)

# add component scores to d
d <- d %>% bind_cols(pca_scores)


# political vars
# select variables
x <- d %>% select(conservatism, RWA, PoliticalOrient)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1265)))

# scree plot
scree(x)

# 6-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(Conservatism_Fac = PC1)

# add component scores to d
d <- d %>% bind_cols(pca_scores)



x <- d %>% 
  filter(CountryLive == "Australia") %>% 
  select(Age, Gender, FinancialSit, extraversion:intellect, reactance,
         BC_AB_Fac, BC_MIB_Fac, BC_PB_Fac, CovidWorry_Fac,
         REFB_Fac, PBEB_Fac, PBAB_Fac, 
         ResAdapt_Fac, Conservatism_Fac)

corr <- x %>% 
  corrr::correlate(use = "pairwise.complete.obs", diagonal = 1) %>% 
  corrr::shave() %>% 
  mutate(across(-term, round, 3)) 

name <- names(corr)

n <- x %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  group_by(var) %>% 
  summarise(n = sum(!is.na(val)),
            term = "N") %>% 
  pivot_wider(names_from = var, values_from = n) %>% 
  select(name)
  
  
desc <- x %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  group_by(var) %>% 
  summarise(mean = round(mean(val, na.rm = T),3),
            stddev = round(sd(val, na.rm = T),3)) %>% 
  pivot_longer(-var, names_to = "term", values_to = "val") %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  select(name)
  

bind_rows(n, corr, desc) %>% 
  write_csv("output/corrs_4_marvin.csv")
  



