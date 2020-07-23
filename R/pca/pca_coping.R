# select variables
x <- d %>% select(cope_distraction, cope_active, cope_denial, cope_substance, cope_emotsupp, 
                  cope_instrsupp, cope_disengage, cope_venting, cope_reframing, cope_planning, 
                  cope_humor, cope_acceptance, cope_religion, cope_selfblame)

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
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1295)))

# Scree plot
scree(x)

# 4-component PCA
n_comp <- 4

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE, n.obs = 1295)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# Component correlations matrix
# rownames(fit$r.scores) <- c("official_sources", "casual_sources")
# colnames(fit$r.scores) <- c("official_sources", "casual_sources")

round(fit$r.scores,2)

# save component scores as dataframe
# pca_scores <- data.frame(fit$scores) %>%
#   rename( = RC1,  = RC2)
# 
# # add component scores to d
# d <- d %>% bind_cols(pca_scores)
# 
# # clean environment
# rm(list = setdiff(ls(), c("d")))