# # SETUP -------------------------------------------------------------------
# # load packages
# packages <- c("tidyverse", "psych", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA")
# lapply(packages, library, character.only = TRUE)
# 
# # Source Functions
# source("R/pca_functions.R")
# 
# # load data
# # full, reduced or marvin dataset?
# data <- "full"
# 
# if (data == "reduced") {
#   d <- read_csv("data/200727_covid_reduced_imputed_std_data.csv", guess_max = 1361)
# 
# } else if (data == "full") {
#   d <- read_csv("data/200728_covid_full_imputed_std_data.csv", guess_max = 1608)
# 
# } else if (data == "marvin") {
# d <- read_csv("data/200728_covid_full_imputed_std_data.csv", guess_max = 1575)
# }


# CONDUCT PCA -------------------------------------------------------------
# select variables
x <- d %>% select(cope_distraction, cope_active, cope_denial, cope_substance, 
                  cope_emotsupp, cope_instrsupp, cope_disengage, cope_venting, 
                  cope_reframing, cope_planning, cope_humor, cope_acceptance, 
                  cope_religion, cope_selfblame, 
                  extraversion, agreeableness, conscientious, neuroticism, intellect, 
                  resilience, adaptability_crisis, adaptability_uncertainty,
                  conservatism, reactance, cultural_tightloose, gov_trust,
                  RWA, PoliticalOrient)
  

# print correlations
corstarsl(x)

# reliability
# psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1265)))

# scree plot
scree(x)

# 6-component PCA
n_comp <- 6
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
if (any(data == c("reduced", "marvin"))) {
  pca_scores <- data.frame(fit$scores) %>%
    rename(resilience_adapt = RC1, adaptivecoping = RC2, maladaptivecoping_reactance = RC4,
           concervatism_trust = RC3, extrv_agrebl = RC5, positive_Acceptance = RC6)

} else if (data == "full") {
  pca_scores <- data.frame(fit$scores) %>%
    rename(resilience_adapt = RC1, maladaptivecoping_reactance = RC4, adaptivecoping = RC2,
           concervatism_trust = RC3, extrv_agrebl = RC5, positive_Acceptance = RC6)
}

# Component correlations matrix
corstarsl(pca_scores)

# add component scores to d
d <- d %>% bind_cols(pca_scores)
