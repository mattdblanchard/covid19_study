# select variables
x <- d %>% select(ReasonsLeaveHome_1, ReasonsLeaveHome_2, ReasonsLeaveHome_3, ReasonsLeaveHome_4,
                  ReasonsLeaveHome_5, ReasonsLeaveHome_6, ReasonsLeaveHome_7, ReasonsLeaveHome_8, 
                  ReasonsLeaveHome_9, ReasonsLeaveHome_10, ReasonsLeaveHome_11)

# print correlations
star_matrix(x)

# Visualise correlations to see if variables appear to cluster
# corrplot(cor(x, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

# reliability
psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 805)))

# scree plot
scree(x)

# 3-component PCA
n_comp <- 3
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE, n.obs = 805)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# Component correlations matrix
rownames(fit$r.scores) <- c("official_sources", "casual_sources")
colnames(fit$r.scores) <- c("official_sources", "casual_sources")

round(fit$r.scores,2)

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename( = RC1,  = RC2)

# add component scores to d
d <- d %>% bind_cols(pca_scores)

# clean environment
rm(list = setdiff(ls(), c("d")))