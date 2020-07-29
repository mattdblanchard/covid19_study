# SETUP -------------------------------------------------------------------
# load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA")
purrr::map(packages, library, character.only = TRUE)

# Source Functions
source("R/pca_functions.R")

# load data
# full or reduced data set?
data <- "full"

if (data == "reduced") {
  d <- read_csv(here("data/200727_covid_reduced_imputed_std_data.csv"), guess_max = 1361)
  
} else if (data == "full") {
  d <- read_csv(here("data/200728_covid_full_imputed_std_data.csv"), guess_max = 1608)
  
}


# select variables
x <- d %>% select(WFH_Biz_Statements_1, WFH_Biz_Statements_2,  WFH_Biz_Statements_3, 
                  WFH_Biz_Statements_4)

# create the correlation matrix for PCA so we know how it was done (e.g., how missing values were treated)
cor_pca <- star_matrix(x)

# print correlations
cor_pca

# Visualise correlations to see if variables appear to cluster
# corrplot(cor(x, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

# reliability
psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 84)))

# scree plot
scree(x)

# 1-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE, n.obs = 84)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

d %>% select(contains("biz"))

# save component scores as dataframe
# pca_scores <- data.frame(fit$scores) %>%
#   rename( = RC1)
# 
# # add component scores to d
# d <- d %>% bind_cols(pca_scores)
# 
# # clean environment
# rm(list = setdiff(ls(), c("d")))