# # SETUP -------------------------------------------------------------------
# # load packages
# packages <- c("tidyverse", "psych", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA")
# purrr::map(packages, library, character.only = TRUE)
# 
# # Source Functions
# source("R/pca_functions.R")
# 
# # load data
# # full or reduced data set?
# data <- "full"
# 
# if (data == "reduced") {
#   d <- read_csv(here("data/200727_covid_reduced_imputed_std_data.csv"), guess_max = 1361)
#   
# } else if (data == "full") {
#   d <- read_csv(here("data/200728_covid_full_imputed_std_data.csv"), guess_max = 1608)
#   
# }


# CONDUCT PCA -------------------------------------------------------------
# select variables
x <- d %>% select(BehaviourComply_1_1, BehaviourComply_1_2, BehaviourComply_1_3, 
                  BehaviourComply_1_4, BehaviourComply_1_5, BehaviourComply_1_6,
                  BehaviourComply_2_1, BehaviourComply_2_2, BehaviourComply_2_3, 
                  BehaviourComply_2_4, BehaviourComply_2_5, BehaviourComply_2_6)

# print correlations
corstarsl(x)

# reliability
psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1344)))

# scree plot
scree(x)

# 3-component PCA
n_comp <- 3
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
  rename(social_distance_isolation = RC1, sickness_actions = RC2, Hygiene = RC3)

# Component correlations matrix
corstarsl(pca_scores)

# add component scores to d
d <- d %>% bind_cols(pca_scores)
