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
# select news sources variables
x <- d %>% select(NewsSources_1:NewsSources_7) %>% 
        rename(NS_FriendsFam = NewsSources_1, NS_OffGovt = NewsSources_2,
               NS_OffHealth = NewsSources_3, NS_Science = NewsSources_4,
               NS_WordMouth = NewsSources_5, NS_News = NewsSources_6,
               NS_SocialMedia = NewsSources_7)

# print correlations
corstarsl(x)

# reliability for news source variables
psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1361)))

# scree plot
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
if (data == "reduced") {
pca_scores <- data.frame(fit$scores) %>%
  rename(official_sources = RC1, casual_sources = RC2)

} else if (data == "full") {
  pca_scores <- data.frame(fit$scores) %>%
    rename(casual_sources = RC1, official_sources = RC2)
}

# Component correlations matrix
corstarsl(pca_scores)

# add component scores to d
d <- d %>% bind_cols(pca_scores)
