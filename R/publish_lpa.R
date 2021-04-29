# This script will implement all analyses reported in the manuscript titled:
#
# To Comply or Not Comply? A Latent Profile Analysis of Behaviours and Attitudes During the COVID-19 Pandemic
#
# This study was conducted by:
# Sabina Kleitman (1), Dayna J. Fullerton (1), Lisa M. Zhang (1), Matthew D. Blanchard (1), Jihyun Lee (2), Lazar Stankov (2), Valerie Thompson (3)
#
# 1) School of Psychology, University of Sydney
# 2) School of Education, University of New South Wales
# 3) Department of Psychology, University of Saskatchewan
#
# This script was written by and all analyses were conducted by Matthew D. Blanchard


# Setup -------------------------------------------------------------------
# create list of packages needed to run this script
packages <- c("tidyverse", "tidyLPA", "naniar", "psych")

# install any necessary packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!(packages %in% installed.packages()[,"Package"])])
}

# load packages
lapply(packages, library, character.only = TRUE)

# load data
# standardised and imputed data for LPA
d_std <- read_csv("data/201027_covid_full_imputed_std_data.csv", guess_max = 1575) %>% 
  mutate(CountryLive = factor(CountryLive),
         sample = factor(sample),
         Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female")))

# Raw data for demogrphics and descriptives
d_raw <- read_csv("data/COVID-19_FourCountries_BC_G_Removed_Imputed-Merged_V3.csv") %>% 
  mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
         sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public")),
         Govt_Extreme = round(Govt_Extreme, 0),
         Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female")))

# Latent Profile Analysis (LPA) -------------------------------------------------
# select variables for LPA
lpa <- d_std %>%  
  select(Age, Education, Physicalhealth, HealthConditions, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours, CovidWorry)

# Fit LPA models with 2-6 profiles to identify optimal number of classes
fit <- lpa %>%
  estimate_profiles(2:6,
                    variances = c("equal"),
                    covariances = c("zero")) %>%
  compare_solutions(statistics = c("LogLik", "AIC", "BIC", "Entropy", "BLRT_val", "BLRT_p"))

# Compare goodness of fit indices for all models
gfi <- fit$fits %>%
  select(Model, Classes, AIC, SABIC, BIC, Entropy, LogLik) %>%
  mutate(across(where(is.numeric), round, 3),
         LogLik_diff = LogLik - lag(LogLik),
         df = 1)

print(gfi)

# goodness of model fit
map(2:5, function(i) {
tmp <- chisq.test(gfi[i, 8:9], simulate.p.value = TRUE, B = 5000)

paste0("Model ", gfi$Classes[i] - 1, " vs Model ", gfi$Classes[i], " : Chi = ", round(tmp$statistic, 2), " , p.value = ", tmp$p.value)
})

# Fit a 2-class solution
fit2 <- lpa %>% 
  estimate_profiles(2, 
                    variances = "equal",
                    covariances = "zero")

# Examine class counts and proportions for the latent classes
class_count <- function(lpa_data) {
  get_data(lpa_data) %>% 
    count(Class) %>% 
    mutate(prop = round(n/sum(n),2))
}

class_count(fit2)

# Examine the average latent class probabilities for most likely latent class membership by latent class 
avg_prob <- function(lpa_data) {
  get_data(lpa_data) %>% 
    select(Class, CPROB1:paste0("CPROB", max(get_data(lpa_data)$Class))) %>% 
    group_by(Class) %>% 
    summarise(across(everything(), mean))  
}

avg_prob(fit2)

# Fit a 4-class solution
fit4 <- lpa %>% 
  estimate_profiles(4, 
                    variances = "equal",
                    covariances = "zero")

# Examine class counts and proportions for the latent classes
class_count(fit4)

# Examine the average latent class probabilities for most likely latent class membership by latent class 
avg_prob(fit4)

# Plot both models
# Prepare data
plot <- bind_rows(get_estimates(fit2),
          get_estimates(fit4)) %>% 
  mutate(model = factor(paste0(Classes, "-Class Solution")), 
         plot = factor(ifelse(Classes == 2, "A", "B"))) %>% 
  filter(Category == "Means") %>%
  select(plot, model, Parameter, Estimate, se, Class) %>% 
  mutate(Class = factor(Class),
         Parameter = factor(Parameter, 
                            levels = c("Age", "Education", "Physicalhealth", "HealthConditions", "CovidWorry", "scrict_isolation", "social_distancing", "herd_immunity_economy", "complaince_selfreport", "social_distance_isolation", "sickness_actions", "Hygiene", "antisocial_behaviours", "prosocial_behaviours"),
                            labels = c("Age", "Education", "Physical Health", "Health Conditions", "Covid Worry", "Perceived Benefits Beliefs", "Response Efficacy Beliefs", "Perceive%>% Barriers Beliefs", "Complaince Self-report", "Avoidant Behaviours", "Management of Illness", "Preventative Behaviours", "Antisocial Behaviours", "Prosocial Behaviours")))

# Generate plots
plot %>%   
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
  geom_line(aes(linetype = Class, colour = Class)) +
  geom_errorbar(aes(ymin = Estimate - se, ymax = Estimate + se, colour = Class),
                width = .1) +
  facet_wrap(plot ~ model, ncol = 1) +
  labs(x = "Variable",
       y = "Mean") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.x = element_text(hjust=0, size = 11, face = "bold"),
        strip.background = element_blank()) + 
  scale_colour_manual(values = c("#c03018", "#6090d8", "#f0a800", "#609048", "#484878"))


# Characteristics and membership for each class ------------------------------------------
# function to compute means and sds on demographics
# Demographics
demo <- function(lpa_data) {
  bind_cols(d_raw, get_data(lpa_data) %>% select(Class)) %>% 
    select(Class, Age, Education, Physicalhealth, HealthConditions) %>% 
    group_by(Class) %>% 
    summarise(across(where(is.numeric), list(mean = mean, sd = sd)))
}

# 2-class solution
demo(fit2)

# 4-class solution
demo(fit4)

# Country membership
member_country <- function(lpa_data) {
  bind_cols(d_raw, get_data(lpa_data) %>% select(Class)) %>% 
    group_by(CountryLive, Class) %>% 
    summarise(n = n()) %>% 
    group_by(CountryLive) %>%
    mutate(total = sum(n)) %>%
    spread(Class, n)
}

# 2-class solution
member_country(fit2)

# 4-class solution
member_country(fit4)

# Gender
member_gender <- function(lpa_data) {
  bind_cols(d_raw, get_data(lpa_data) %>% select(Class)) %>% 
    count(Gender, Class) %>% 
    group_by(Gender) %>% 
    mutate(total = sum(n)) %>%
    spread(Class, n)
}

# 2-class solution
member_gender(fit2)

# 4-class solution
member_gender(fit4)


# Differences bw non-compliant and compliant ------------------------------
# save profile scores for 2-class solution
scores <- bind_cols(d_std, get_data(fit2) %>% select(model_number, classes_number, contains("CPROB"), Class)) %>% 
  mutate(Class = factor(Class)) %>% 
  group_by(id) %>% 
  mutate(adapt = sum(adaptability_uncertainty, adaptability_crisis)/2) %>% 
  ungroup()

# function to calculate cohen's D
cohen_d <- function(data, grouping, var) {
  lx <- sum(data[[grouping]]==1) - 1
  ly <- sum(data[[grouping]]==2) - 1
  md  <- abs(mean(data[[var]][data[[grouping]]==1],na.rm=T)
             - mean(data[[var]][data[[grouping]]==2],na.rm=T)) ## mean difference (numerator)
  csd <- lx * sd(data[[var]][data[[grouping]]==1],na.rm=T)^2 + ly * sd(data[[var]][data[[grouping]]==2],na.rm=T)^2
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)  ## common sd computation
  
  cd  <- md/csd   ## cohen's d
  cd
}

# # function to conduct t-test on difference between profiles for each variable
test_vars <- function() {
  nested <- scores %>% 
    select(id, Class, all_of(name)) %>% 
    pivot_longer(all_of(name), names_to = "var", values_to = "val") %>% 
    pivot_wider(names_from = Class, values_from = val) %>% 
    mutate(`1` = as.numeric(`1`),
           `2` = as.numeric(`2`)) %>% 
    nest(data = -var)
  
  test <- nested %>% 
    mutate(fit = map(data, ~ t.test(.x$`1`, 
                                    .x$`2`, paired = F, var.equal = T)),
           tidy_fit = map(fit, broom::tidy)) %>% 
    unnest(tidy_fit) %>% 
    select(-data, -fit, -method, -alternative, -estimate1, -estimate2) %>% 
    rename(t = statistic, df = parameter) %>% 
    mutate(p.value = round(p.value, 4),
           p.value = ifelse(p.value < .001, "<.001", p.value))
  
  # calculate cohen's D
  cohen <- map(name, function(i) {
    data.frame(var = i,
               cohenD = cohen_d(scores %>% select(Class, name), "Class", i))
  })
  
  cohen <- bind_rows(cohen)
  
  # calculate means
  # overall
  mean_overall <- scores %>%
    select(all_of(name)) %>% 
    pivot_longer(all_of(name), names_to = "var", values_to = "val") %>% 
    group_by(var) %>% 
    summarise(mean_overall = round(mean(val, na.rm = T),2),
              sd_overall = round(sd(val, na.rm = T),2)) %>% 
    mutate(mean_overall = paste0(mean_overall, " (", sd_overall, ")")) %>% 
    select(-sd_overall)
  
  # classes
  mean_class <- scores %>%
    select(id, Class, all_of(name)) %>% 
    pivot_longer(all_of(name), names_to = "var", values_to = "val") %>% 
    group_by(var, Class) %>% 
    summarise(mean = round(mean(val, na.rm = T), 2),
              sd = round(sd(val, na.rm = T), 2)) %>% 
    pivot_longer(mean:sd, names_to = "par", values_to = "val") %>% 
    unite(par, par, Class) %>% 
    pivot_wider(names_from = par, values_from = val) %>% 
    mutate(mean_1 = paste0(mean_1, " (", sd_1, ")"),
           mean_2 = paste0(mean_2, " (", sd_2, ")")) %>% 
    select(-sd_1, -sd_2)
  
  datalist <- list(test, cohen, mean_overall, mean_class)
  
  reduce(datalist, left_join, by = "var") %>% 
    select(var, mean_overall, mean_1, mean_2, df, t, p.value, cohenD, conf.low, conf.high)
  
}

# Psychological variables
# select variables to test for profile differences
# not included (PHQ,DASS_stress,DASS_anxiety,DASS_depression)
name <- scores %>%
  select(agreeableness,conscientious,extraversion,intellect,neuroticism,
         CRT_acc,belief_acc,EAT_acc,trait_acc,
         CRT_conf,belief_conf,EAT_conf,trait_conf,
         resilience,adapt,cope_distraction,cope_active,cope_denial,cope_substance,cope_emotsupp,
         cope_instrsupp,cope_disengage,cope_venting,cope_reframing,cope_planning) %>%
  names()

# Conduct t-tests on profiles for each variable
test_vars() %>% 
  write_csv("output/2_profile_t-test_results_psych.csv")


# Information Consumption
# select variables to test for profile differences
name <- scores %>% 
  select(CheckNews,official_sources,casual_sources,SourceCheck,official_trust,casual_trust) %>% 
  names()

# Conduct t-tests on profiles for each variable
test_vars() %>% 
  write_csv("output/2_profile_t-test_results_info.csv")

# Political and Cultural Variables
# select variables to test for profile differences
name <- scores %>% 
  select(Govt_Truth,Govt_Satisfaction,conservatism,reactance,cultural_tightloose,RWA,morality) %>% 
  names()

# Conduct t-tests on profiles for each variable
test_vars() %>% 
  write_csv("output/2_profile_t-test_results_politics.csv")

# Future Behaviours 
# select variables to test for profile differences
name <- scores %>% 
  select(leave_going_to_work:leave_exercise_freedom) %>% 
  names()

# Conduct t-tests on profiles for each variable
test_vars() %>% 
  write_csv("output/2_profile_t-test_results_future_behave.csv")

