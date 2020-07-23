source("R/corr_matrix_sig.R")

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
star_matrix(x)

# Visualise correlations to see if variables appear to cluster
# corrplot(cor(x, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

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
                 method = score_method, scores = TRUE, n.obs = 1265)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# Component correlations matrix
print(round(fit$r.scores,2))


# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(resilience_adapt = RC1, adaptivecoping = RC2, maladaptivecoping_reactance = RC3,
         concervatism_trust = RC4, extrv_agrebl = RC5, positive_Acceptance = RC6)

# add component scores to d
d <- d %>% bind_cols(pca_scores)

# clean environment
rm(list = setdiff(ls(), c("d")))