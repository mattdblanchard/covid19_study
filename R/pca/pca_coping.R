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
x <- d %>% select(contains("cope_"))

# print correlations
corstarsl(x)

# reliability
psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1295)))

# Scree plot
scree(x)

# 2-component PCA
n_comp <- 2
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
  rename(adaptivecoping = RC1, maladaptivecoping_reactance = RC2)

# Component correlations matrix
corstarsl(pca_scores)

# 
# # add component scores to d
# d <- d %>% bind_cols(pca_scores)
