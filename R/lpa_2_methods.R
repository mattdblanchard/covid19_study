# used this as a guide: https://mclust-org.github.io/mclust/articles/mclust.html

# load data
source("R/reduced_data_prep.R")

# Conduct PCA on news source variables ------------------------------------
source("R/pca_news_sources.R")

# explore outliers for income
library(GGally)

d %>% 
  select(Age, Gender, AnnualIncome_usd) %>% 
  ggplot(aes(x = AnnualIncome_usd, y = Age)) +
  geom_point() +
  facet_wrap(~ Gender)

# looks like most income outliers occur for young males (esp. extreme outliers)
# examine outliers (>3SD) more closely
max <- mean(d$AnnualIncome_usd, na.rm = T) + (4*sd(d$AnnualIncome_usd, na.rm = T))

d %>% 
  filter(AnnualIncome_usd > max) %>% 
  select(AnnualIncome_usd, Age, Gender, Education, PoliticalOrient, 
         Physicalhealth, HealthConditions, FinancialSit) %>% 
  arrange(desc(AnnualIncome_usd))


# LPA using factors -------------------------------------------------------
# method 1 ----------------------------------------------------------------
# vignette: https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html
library(naniar)

# how much data is missing for each variable in the model?
gg_miss_var(d %>% select(resilience_adapt:g_md, Age, Gender, Education, PoliticalOrient, 
                         Physicalhealth, HealthConditions, AnnualIncome_usd, FinancialSit,
                         scrict_isolation, social_distancing, herd_immunity_economy,
                         complaince_selfreport, social_distance_isolation, sickness_actions, Hygiene, 
                         antisocial_behaviours, prosocial_behaviours, CovidWorry, 
                         CheckNews, official_sources, casual_sources, SourceCheck, NewsShare,
                         EAT_acc,
                         -g_md, -adaptivecoping, -concervatism_trust, -extrv_agrebl, 
                         -antisocial_behaviours, -prosocial_behaviours), show_pct = TRUE)

# 64% of g_md missing too much to impute so exclude
# select variables
x <- d %>%  
  select(resilience_adapt:g_md, Age, Gender, Education, PoliticalOrient, 
         Physicalhealth, HealthConditions, AnnualIncome_usd, FinancialSit,
         scrict_isolation, social_distancing, herd_immunity_economy,
         complaince_selfreport, social_distance_isolation, sickness_actions, Hygiene, 
         antisocial_behaviours, prosocial_behaviours, CovidWorry, 
         CheckNews, official_sources, casual_sources, SourceCheck, NewsShare,
         -g_md
         # , -adaptivecoping, -concervatism_trust, -extrv_agrebl, 
         # -antisocial_behaviours, -prosocial_behaviours
         )

# remove income outliers
# x <- x %>% filter(AnnualIncome_usd <= max | is.na(AnnualIncome_usd))

# identify the number of latent profiles
library(tidyLPA)

x %>%
  mclust::imputeData(seed = 23) %>% 
  # scale() %>%
  mutate(across(everything(), scale)) %>% 
  estimate_profiles(3:4, 
                    variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))

# run LPA model 1 and model 3 with 3 and 4 profiles
# model 1 "equal", "zero
# model 2 "varying", "zero"
# model 3 "equal", "equal"
# model 6 "varying", "varying"

# best models are:
# model == 1 ("equal", "zero) & 3 ("equal", "equal")
# profiles == 2 - 5
# seed value
seed <- 23

# 7 == 77
# 6 == 99
# 5 == 7
# 4 == 50
# 3 == 23

models <- map(c("zero", "equal"), function(cv) { # set covariance
  map(3:4, function(i) { # set number of profiles to extract
    # set seed for reproducible results
    set.seed(seed) 
    
    # fit model
    fit <- x %>% 
      single_imputation() %>%
      # scale() %>% 
      mutate(across(everything(), scale)) %>% 
      estimate_profiles(i, 
                        variances = "equal",
                        covariances = cv)
    
    # plot profiles
    fit %>% 
      plot_profiles(ci = 0.95,
                    sd = FALSE,
                    add_line = TRUE,
                    rawdata = FALSE,
                    bw = FALSE,
                    alpha_range = c(0, 0.1)) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    # save plot
    if (cv == "zero") {
      ggsave(paste0("output/test/model_1_profile_", i, "_all_seed", seed, ".png"), width = 13.5, height = 6.18)
    } else if (cv == "equal") {
      ggsave(paste0("output/test/model_3_profile_", i, "_all_seed", seed, ".png"), width = 13.5, height = 6.18)
    }
    
    # goodness of fit indices
    # get_fit(fit)
    
    # save profile scores
    get_data(fit)
    
  })
})

# collapse list structure into df
models_all3 <- bind_rows(models_all3)



map(c(3,6,7), function(i) {
profile_scores <- get(paste0("models_all", i)) %>% 
  filter(model_number == 3 & classes_number == 4) %>% 
  select(model_number, classes_number, Class, CPROB1,CPROB2,CPROB3,CPROB4)

tmp <- bind_cols(d, profile_scores)

tmp %>% select(Class) %>% table
})


profile_scores <- models_all3 %>% 
  filter(model_number == 3 & classes_number == 4) %>% 
  select(Class)

bind_cols(d, profile_scores) %>% 
  select(CountryLive, Class) %>% table



# model 1 "equal", "zero
# model 3 "equal", "equal"


set.seed(23) 

# fit model
fit <-  c %>%  
  # single_imputation() %>%
  # mutate(across(everything(), scale)) %>% 
  estimate_profiles(4, 
                    variances = "equal",
                    covariances = "zero")

map(c(77, 99), function(i) {
  set.seed(77) 
  
  # fit model
  fit <- x %>% 
    single_imputation() %>%
    scale() %>% 
    estimate_profiles(4, 
                      variances = "equal",
                      covariances = "equal")
  
  fit$model_3_class_4$fit[c(4,6)]
  
})


# plot profiles
fit %>% 
  plot_profiles(ci = 0.95,
                sd = FALSE,
                add_line = TRUE,
                rawdata = FALSE,
                bw = FALSE,
                alpha_range = c(0, 0.1)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("output/best/model_3_profile_4_seed_77.png", width = 13.5, height = 6.18)

# goodness of fit indices
get_fit(fit)

# save profile scores
pro <- get_data(fit) %>% 
  select(Class)

pro %>% select(Class) %>% table

# round(100*tmp/sum(tmp),0)

tmp <- bind_cols(d, pro) %>% 
  select(CountryLive, Class) %>% table

round(100*tmp[,1]/colSums(tmp)[1],0)
round(100*tmp[,2]/colSums(tmp)[2],0)
round(100*tmp[,3]/colSums(tmp)[3],0)
round(100*tmp[,4]/colSums(tmp)[4],0)

bind_cols(d, pro) %>% 
  filter(AnnualIncome_usd > max) %>% 
  select(Class) %>% table

# fit statistics:
# LogLik: Log-likelihood of the data, given the model.
# AIC: Aikake information criterion; based on -2 log-likelihood, and penalized by number of parameters.
# AWE: Approximate weight of evidence; combines information on model fit and on classification errors (Celeux et al., 1997).
# BIC: Bayesian information criterion; based on -2 log-likelihood, and penalized by number of parameters adjusted by sample size.
# CAIC: Consistent Aikake information criterion; based on -2 log-likelihood, and penalized by number of parameters adjusted by sample size.
# CLC: Classification Likelihood Criterion; based on -2 log-likelihood, and penalized by the entropy (Biernacki, 1997).
# KIC: Kullback information criterion; based on -2 log-likelihood, and penalized by 3 times the number of parameters -1 (Cavanaugh, 1999).
# SABIC: Sample size-adjusted Bayesian information criterion (Sclove, 1987).
# ICL: Integrated completed likelihood (Biernacki, Celeux, & Govaert, 2000).
# Entropy: A measure of classification uncertainty, reverse-coded so that 1 reflects complete certainty of classification, and 0 complete uncertainty (see Celeux & Soromenho, 1996).
# Prob. Min.: Minimum of the diagonal of the average latent class probabilities for most likely class membership, by assigned class. The minimum should be as high as possible, reflecting greater classification certainty (cases are assigned to classes they have a high probability of belonging to; see Jung & Wickrama, 2008).
# Prob. Max.: Maximum of the diagonal of the average latent class probabilities for most likely class membership, by assigned class. The maximum should also be as high as possible, reflecting greater classification certainty (cases are assigned to classes they have a high probability of belonging to).
# N Min.: Proportion of the sample assigned to the smallest class (based on most likely class membership).
# N Max.: Proportion of the sample assigned to the largest class (based on most likely class membership).
# BLRT: bootstrapped likelihood test.
# BLRT p-value: p-value for the bootstrapped likelihood ratio test.
# get_fit(mod)

# get overall mean and se profile scores for each variable
# get_estimates(mod)

# get profile scores for each participant
# get_data(mod)


# method 2 ----------------------------------------------------------------

library(careless)
library(psych)
library(mclust)
library(factoextra) # pretty plots of mclust models
library(naniar)

# remove NAs and std vars
x <- d %>%
  select(resilience_adapt:g_md, Age, Gender, Education, PoliticalOrient, 
         Physicalhealth, HealthConditions, AnnualIncome_usd, FinancialSit,
         scrict_isolation, social_distancing, herd_immunity_economy,
         complaince_selfreport, social_distance_isolation, sickness_actions, Hygiene, 
         antisocial_behaviours, prosocial_behaviours, CovidWorry, 
         CheckNews, official_sources, casual_sources, SourceCheck, NewsShare,
         -g_md, -adaptivecoping, -concervatism_trust, -extrv_agrebl, 
         -antisocial_behaviours, -prosocial_behaviours) %>% 
  na.omit() %>% 
  mutate(across(everything(), scale))

# how much data is missing for each variable in the model?
gg_miss_var(x, show_pct = TRUE)

# How many latent profiles are present?
# prior = priorControl()
mc <- Mclust(x)

# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")

# Looks like 4 latent profiles. Let's take a deeper look
# compute BIC for all model types for 1:9 clusters
# with or without prior = priorControl()?
BIC <- mclustBIC(x)

# plot Bayesian Information Criteria for all the models with profiles ranging from 1 to 9
plot(BIC)

# show the top three models based on BIC
# VVE, 4 appears to fit quite well. 
summary(BIC)

# Let's look at the performance of all models
# create vector of model names
model <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV")

# looks like 2 or 3 components is best. Select comp == 2 or 3 for each model
tmp <- purrr::map(1:14, function(i) {
  tmp <- tibble(model = model[i],
                comp = names(BIC[,i]),
                bic = BIC[,i]) 
  tmp %>%
    filter(comp == 2 | comp == 3 | comp == 4 | comp == 5 | comp == 6)
})


tmp <- bind_rows(tmp) %>% 
  mutate(best = max(bic, na.rm = T)) %>% 
  group_by(model) %>% 
  mutate(diff = bic - best) %>% 
  select(-best)

head(tmp %>% arrange(desc(diff)), 10)

# store the top models as an objects to examine
# VVE == ellipsoidal, equal orientation
# VVV == ellipsoidal, varying volume, shape, and orientation
# VEV == ellipsoidal, equal shape

mod1 <- Mclust(x, modelNames = "VVE", G = 4, x = BIC)
mod2 <- Mclust(x, modelNames = "VVE", G = 3, x = BIC)
mod3 <- Mclust(x, modelNames = "VVV", G = 3, x = BIC)
mod4 <- Mclust(x, modelNames = "VVV", G = 4, x = BIC)
mod5 <- Mclust(x, modelNames = "VVV", G = 5, x = BIC)
mod6 <- Mclust(x, modelNames = "VEV", G = 3, x = BIC)
mod7 <- Mclust(x, modelNames = "VEV", G = 5, x = BIC)
mod8 <- Mclust(x, modelNames = "VEV", G = 6, x = BIC)

summary(mod1, parameters = TRUE)

# plot variable distributions colour coded by the cluster
plot(mod1, what = "classification")

# plot variable distributions with uncertainty colour coded
plot(mod1, what = "uncertainty")

# BIC is one of the best fit indices, but it’s always recommended to look for more evidence that the 
# solution we’ve chosen is the correct one. We can also compare values of the Integrated Completed 
# Likelikood (ICL) criterion. ICL isn’t much different from BIC, except that it adds a penalty on 
# solutions with greater entropy or classification uncertainty.
# with or without prior = priorControl()?
ICL <- mclustICL(x)

# We see the same top 3 results. ICL suggests that model VVE, 4 fits quite well. 
summary(ICL)
plot(ICL)

# Finally, we’ll perform the Bootstrap Likelihood Ratio Test (BLRT) which compares model fit between k-1 
# and k cluster models. In other words, it looks to see if an increase in profiles increases fit. Based on 
# simulations by Nylund, Asparouhov, and Muthén (2007) BIC and BLRT are the best indicators for how many 
# profiles there are. This line of code will take a long time to run.
# with or without prior = priorControl()?
LRT <- mclustBootstrapLRT(x, modelName = "VVE")
LRT
# BLRT also suggests that a 4-profile solution is ideal.

# try another approach to estimate best model
# EM algorithm is used by mclust for maximum likelihood estimation. Initialisation of EM is performed using 
# the partitions obtained from agglomerative hierarchical clustering. For details see help(mclustBIC) or 
# help(Mclust), and help(hc).
# mod1
(hc1 <- hc(x, modelName = "VVV", use = "SVD"))
BIC1 <- mclustBIC(x, initialization = list(hcPairs = hc1)) # default 
summary(BIC1)

# mod2
(hc2 <- hc(x, modelName = "VVV", use = "VARS"))
BIC2 <- mclustBIC(x, initialization = list(hcPairs = hc2))
summary(BIC2)

# mod3
(hc3 <- hc(x, modelName = "EEE", use = "SVD"))
BIC3 <- mclustBIC(x, initialization = list(hcPairs = hc3))
summary(BIC3)

# # mod4
# (hc4 <- hc(x, modelName = "EEE", use = "VARS"))
# BIC4 <- mclustBIC(x, initialization = list(hcPairs = hc4))
# summary(BIC4)

BIC <- mclustBICupdate(BIC1, BIC2, BIC3)
summary(BIC)

plot(BIC)


library(reshape2)
str(mod4$parameters)
mod4$parameters$mean
mod4[[13]][[2]]

purrr::map(1:8, function(i) {
means <- data.frame(get(paste0("mod", i))[[13]][[2]], stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Factor = rowname) %>%
  melt(id.vars = "Factor", variable.name = "Profile", value.name = "Mean")

means %>%
  ggplot(aes(Factor, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "Mean score") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

ggsave(paste0("output/mod", i, ".png"))

})












# LPA using vars ----------------------------------------------------------
x <- d %>% 
  select(cope_distraction, cope_active, cope_denial, cope_substance, cope_emotsupp, 
         cope_instrsupp, cope_disengage, cope_venting, cope_reframing, cope_planning, 
         cope_humor, cope_acceptance, cope_religion, cope_selfblame, CovidWorry,
         extraversion, agreeableness, conscientious, neuroticism, intellect,
         resilience, adaptability_crisis, adaptability_uncertainty,
         gov_trust, conservatism, reactance, cultural_tightloose, RWA, PoliticalOrient,
         # CRT_conf, belief_conf, EAT_conf, CRT_acc, belief_acc, EAT_acc, 
         BehaviourAntisocial_1, BehaviourAntisocial_2, BehaviourAntisocial_3, 
         BehaviourAntisocial_4, BehaviourProsocial_1, BehaviourProsocial_2, 
         BehaviourProsocial_3, BehaviourProsocial_4, Age, 
         scrict_isolation, social_distancing, herd_immunity_economy,
         complaince_selfreport, social_distance_isolation, sickness_actions, Hygiene, 
         antisocial_behaviours, prosocial_behaviours, CovidWorry, 
         -g_md) %>% 
  na.omit() %>% 
  scale()

gg_miss_var(x, show_pct = TRUE)

BIC <- mclustBIC(x)

# plot Bayesian Information Criteria for all the models with profiles ranging from 1 to 9
plot(BIC)

# show the top three models based on BIC
# VVE, 4 appears to fit quite well. 
summary(BIC)






x %>%
  single_imputation() %>%
  scale() %>% 
  estimate_profiles(1:9, 
                    variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))



# Try it on the sub samples to see if the same structure appears