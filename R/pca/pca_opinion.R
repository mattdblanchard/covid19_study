# select variables
x <- d %>% select(opinion_1, opinion_2, opinion_3, opinion_4, opinion_5, opinion_6, 
                  opinion_7, opinion_8, opinion_9, opinion_10)

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
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1302)))

# scree plot
scree(x)

# 3-component PCA
n_comp <- 3
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE, n.obs = 1295)

# variance explained
var_table()

# pattern matrix
pattern_matrix()


# Component correlations matrix
rownames(fit$r.scores) <- c("scrict_isolation", "social_distancing", "herd_immunity_economy")
colnames(fit$r.scores) <- c("scrict_isolation", "social_distancing", "herd_immunity_economy")

round(fit$r.scores,2)

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(scrict_isolation = RC1, social_distancing = RC2, herd_immunity_economy = RC3)

# add component scores to d
d <- d %>% bind_cols(pca_scores)

# clean environment
rm(list = setdiff(ls(), c("d")))