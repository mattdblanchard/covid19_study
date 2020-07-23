# This script conducts PCA to reduce the number of news source variables for LPA

# select news sources variables
x <- d %>% select(NewsSources_1:NewsSources_7) %>% 
        rename(NS_FriendsFam = NewsSources_1, NS_OffGovt = NewsSources_2,
               NS_OffHealth = NewsSources_3, NS_Science = NewsSources_4,
               NS_WordMouth = NewsSources_5, NS_x = NewsSources_6,
               NS_SocialMedia = NewsSources_7)

# create the correlation matrix for PCA so we know how it was done (e.g., how missing values were treated)
cor_pca <- star_matrix(x)

# print correlations
kable(cor_pca)

# Visualise correlations to see if variables appear to cluster
# corrplot(cor(x, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

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
                 method = score_method, scores = TRUE, n.obs = 1361)

# variance explained
var_table()

# pattern matrix
pattern_matrix()


# Component correlations matrix
rownames(fit$r.scores) <- c("official_sources", "casual_sources")
colnames(fit$r.scores) <- c("official_sources", "casual_sources")

round(fit$r.scores,2)

# # save component scores as dataframe
# pca_scores <- data.frame(fit$scores) %>%
#   rename(official_sources = RC1, casual_sources = RC2)
# 
# # add component scores to d
# d <- d %>% bind_cols(pca_scores)
# 
# # clean environment
# rm(list = setdiff(ls(), c("d")))
