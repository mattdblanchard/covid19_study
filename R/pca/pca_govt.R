# select variables
x <- d %>% select(Govt_Satisfaction, Govt_Extreme, Govt_Truth)

# print correlations
star_matrix(x)

# Visualise correlations to see if variables appear to cluster
# corrplot(cor(x, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

# reliability
psych::alpha(d %>% select(Govt_Satisfaction, Govt_Extreme_r, Govt_Truth))$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1322)))

# scree plot
scree(x)

# 1-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE, n.obs = 1322)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(gov_trust = PC1)

# add component scores to d
d <- d %>% bind_cols(pca_scores)

# clean environment
rm(list = setdiff(ls(), c("d")))