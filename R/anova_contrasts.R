# load packages
packages <- c("tidyverse", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA", "psych", "cowplot")
purrr::map(packages, library, character.only = TRUE)

# load data
# d <- read_csv(here("data/COVID-19_FourCountries_BC_G_Removed_Imputed-Merged_V3.csv"))
  d <- read_csv("data/201027_covid_full_imputed_std_data.csv", guess_max = 1575) %>% 
    mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
           sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public")))

# select variables for LPA
x <- d %>%  
  select(Age, Education, Physicalhealth, HealthConditions, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,CovidWorry)




fit_lpa <- function(profiles) { # set number of profiles to extract (i)
  # set seed for reproducible results
  # set.seed(23) 
  
  # fit model
  x %>% 
    # scale() %>%
    estimate_profiles(profiles, 
                      variances = "equal",
                      covariances = "zero")
}

# fit 2-5 profile models
fit2 <- fit_lpa(2)
fit3 <- fit_lpa(3)
fit4 <- fit_lpa(4)
fit5 <- fit_lpa(5)


# plot profiles
plot_lpa <- function(data1, data2) {
  bind_rows(get_estimates(data1) %>% mutate(model = paste0(max(unique(get_estimates(data1)$Class)), "-Class"), plot = "A"), 
            get_estimates(data2) %>% mutate(model = paste0(max(unique(get_estimates(data2)$Class)), "-Class"), plot = "B")) %>% 
      mutate(model = factor(model, levels = c(paste0(max(unique(get_estimates(data1)$Class)), "-Class"), 
                                              paste0(max(unique(get_estimates(data2)$Class)), "-Class")), 
                            labels = c(paste0(max(unique(get_estimates(data1)$Class)), "-Class Solution"), paste0(max(unique(get_estimates(data2)$Class)), "-Class Solution"))),
             plot = factor(plot)) %>% 
      filter(Category == "Means") %>%
      select(plot, model, Parameter, Estimate, se, Class) %>% 
      mutate(Class = factor(Class),
             Parameter = factor(Parameter, 
                                levels = c("Age", "Education", "Physicalhealth", "HealthConditions", "CovidWorry", "scrict_isolation", "social_distancing", "herd_immunity_economy", "complaince_selfreport", "social_distance_isolation", "sickness_actions", "Hygiene", "antisocial_behaviours", "prosocial_behaviours"),
                                labels = c("Age", "Education", "Physical Health", "Health Conditions", "Covid Worry", "Perceived Benefits Beliefs", "Response Efficacy Beliefs", "Perceived Barriers Beliefs", "Complaince Self-report", "Avoidant Behaviours", "Management of Illness", "Preventative Behaviours", "Antisocial Behaviours", "Prosocial Behaviours"))) %>% 
      ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
      geom_line(aes(linetype = Class, colour = Class)) +
      geom_errorbar(aes(ymin = Estimate - se, ymax = Estimate + se, colour = Class),
                    width = .1) +
      facet_wrap(plot ~ model, ncol = 1) +
      labs(x = "Variable",
           y = "Mean") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            # strip.text = element_text(hjust=0, face="bold"),
            strip.text.x = element_text(hjust=0, size = 11, face = "bold"),
            strip.background = element_blank()) + 
      scale_colour_manual(values = c("#c03018", "#6090d8", "#f0a800", "#609048", "#484878"))
}

plot_lpa(fit2, fit4)
ggsave("output/figure_1.png", height = 8, width = 10)

plot_lpa(fit3, fit5)
ggsave("output/appendix_figure.png", height = 8, width = 10)


# plot_lpa <- function(data, plot_title, x_title = "", y_title = "") {
#   get_estimates(data) %>% 
#     filter(Category == "Means") %>%
#     select(Parameter, Estimate, se, Class) %>% 
#     mutate(Class = factor(Class),
#            Parameter = factor(Parameter, 
#                               levels = c("Age", "Education", "Physicalhealth", "HealthConditions", "CovidWorry", "scrict_isolation", "social_distancing", "herd_immunity_economy", "complaince_selfreport", "social_distance_isolation", "sickness_actions", "Hygiene", "antisocial_behaviours", "prosocial_behaviours"),
#                               labels = c("Age", "Education", "Physical Health", "Health Conditions", "Covid Worry", "Perceived Benefits Beliefs", "Response Efficacy Beliefs", "Perceived Barriers Beliefs", "Complaince Selfreport", "Avoidant Behaviours", "Management of Illness", "Preventative Behaviours", "Antisocial Behaviours", "Prosocial Behaviours"))) %>% 
#     ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
#     geom_line(aes(linetype = Class, colour = Class)) +
#     geom_errorbar(aes(ymin = Estimate - se, ymax = Estimate + se, colour = Class),
#                   width = .1) +
#     labs(title = plot_title,
#          x = x_title,
#          y = y_title) +          
#     theme_classic() +
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#     scale_colour_manual(values = c("#c03018", "#6090d8", "#f0a800", "#609048", "#484878"))
# }
# 
# # 2-class plot
# p2 <- plot_lpa(fit2, plot_title = "2-Class Solution") +
#   scale_y_continuous(breaks = -3:0, limits = c(-3,.5)) +
#   theme(axis.text.x = element_blank())
# 
# # 4-class plot
# p4 <- plot_lpa(fit4, plot_title = "4-Class Solution")
# 
# # 3-class plot
# p3 <- plot_lpa(fit3, plot_title = "3-Class Solution") +
#   theme(axis.text.x = element_blank())
# 
# # 5-class plot
# p5 <- plot_lpa(fit5, plot_title = "5-Class Solution")
# 
# # cowplot::plot_grid(p2, p4, ncol = 1, rel_heights = c(.62,1))
# # cowplot::plot_grid(p2, p3, p4, ncol = 1, rel_heights = c(.65,.65,1), labels="AUTO")
# plot_grid(p2, p4, ncol = 1, rel_heights = c(.72,1), labels="AUTO")
# cowplot::plot_grid(p3, p5, ncol = 1, rel_heights = c(.72,1), labels="AUTO")



# calculate means using non-standardised data
tmp <- read_csv("data/COVID-19_FourCountries_BC_G_Removed_Imputed-Merged_V3.csv") %>% 
  mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
         sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public")),
         Govt_Extreme = round(Govt_Extreme, 0))

tmp <- bind_cols(tmp, get_data(fit4) %>% select(Class)) %>% 
  mutate(classes = ifelse(Class == 1, 1, 2),
         classes = factor(classes))

# # for fit4


tmp %>% select(Class, matches("ReasonsLeaveHome")) %>% 
  rename(leave_going_to_work = ReasonsLeaveHome_1, leave_walking_pet = ReasonsLeaveHome_2,
         leave_physical_activity = ReasonsLeaveHome_3, leave_get_food = ReasonsLeaveHome_4, 
         leave_pharmacy = ReasonsLeaveHome_5, leave_medical_treatment = ReasonsLeaveHome_6,
         leave_care_dependents = ReasonsLeaveHome_7, leave_meet_friends_family = ReasonsLeaveHome_8, 
         leave_religion = ReasonsLeaveHome_9, leave_bored = ReasonsLeaveHome_10, 
         leave_exercise_freedom = ReasonsLeaveHome_11) %>% 
  group_by(Class) %>% 
  summarise(across(everything(), list(mean = mean, sd = sd), na.rm = T)) %>% 
  pivot_longer(-Class, names_to = "var", values_to = "val") %>% 
  mutate(metric = ifelse(str_detect(var, "mean"), "mean", "sd"),
         var = str_remove(var, "_mean|_sd"),
         val = round(val, 2)) %>% 
  pivot_wider(names_from = metric, values_from = val) %>% 
  mutate(mean = paste0(mean, " (", sd, ")")) %>% 
  select(-sd) %>% 
  write_csv("output/class_reasons.csv")
  
  



# non parametric test for govt_extreme
wilcox.test(Govt_Extreme ~ classes, data = tmp)
c <- tmp %>% select(Govt_Extreme, classes) %>% mutate(classes = as.numeric(classes)) %>% table()

data.frame(resp = c("Much too extreme", "Somewhat too extreme", "Appropriate", "Somewhat insufficient", "Not at all sufficient"),
           c1 = round(c[,1]/length(tmp$classes[tmp$classes==1]),2),
           c2 = round(c[,2]/length(tmp$classes[tmp$classes==2]),2)) %>% 
  pivot_longer(c1:c2, names_to = "var", values_to = "val") %>% 
  mutate(resp = factor(resp, levels = c("Much too extreme", "Somewhat too extreme", "Appropriate", "Somewhat insufficient", "Not at all sufficient")),
         var = factor(var, levels = c("c1", "c2"), labels = c("Profile 1", "Profile 2"))) %>% 
  ggplot(aes(x = resp, y = val)) +
  geom_col() +
  facet_wrap(~var, ncol = 1) +
  labs(y = "Proportion of participants", x = "Response") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_blank())

ggsave("output/govt_extreme_plot.png")



tmp %>% 
  select(classes, Age, Education, Physicalhealth, HealthConditions) %>% 
  group_by(classes) %>% 
  summarise(across(where(is.numeric), list(mean, sd)))



tmp %>% 
  select(Class, Age, Education, Physicalhealth, HealthConditions) %>% 
  group_by(Class) %>% 
  summarise(across(where(is.numeric), sd))

# plot: compare gov_trust for country residence and each class
# scores %>% 
#   group_by(Class, CountryLive) %>% 
#   summarise(mean = mean(gov_trust)) %>% 
#   filter(Class == 1) %>%
#   ggplot(aes(x = as.factor(Class), y = mean, colour = as.factor(CountryLive))) +
#   geom_point(aes(group = 1)) +
#   # scale_y_continuous(limits = c(-1.3, 0)) +
#   labs(y = "Gov Trust") +
#   theme_classic()
# 
# ggsave("output/country_class_plot.png")

scores <- bind_cols(get_data(fit2) %>% select(Class), d)

# proportion of gender in each class
tmp <- scores %>% select(Gender, Class) %>% table()

tmp[1,]/sum(tmp[1,])
tmp[2,]/sum(tmp[2,])

# proportion of country in each class
tmp <- scores %>% select(CountryLive, Class) %>% table()

tmp[1,]/sum(tmp[1,])
tmp[2,]/sum(tmp[2,])
tmp[3,]/sum(tmp[3,])
tmp[4,]/sum(tmp[4,])


chisq.test(scores$CountryLive, scores$Class)
chisq.test(scores$Gender, scores$Class)
chisq.test(scores$Gender, scores$Class)
 
# conduct test
# which variables to test?
name <- d %>% 
  select(CRT_acc,belief_acc,EAT_acc,CRT_conf,belief_conf,EAT_conf,trait_acc,trait_conf,agreeableness,conscientious,extraversion,intellect,neuroticism,resilience_adapt,cope_distraction,cope_active,cope_denial,cope_substance,cope_emotsupp,cope_instrsupp,cope_disengage,cope_venting,cope_reframing,cope_planning,PHQ,DASS_stress,DASS_anxiety,DASS_depression,social_media_n,SourceCheck,CheckNews,official_sources,casual_sources,official_trust,casual_trust,Govt_Truth,Govt_Satisfaction,conservatism,reactance,cultural_tightloose,RWA,morality,leave_going_to_work:leave_exercise_freedom,FinancialSit,NumberDeaths,PublicReact,CovidLength, HouseholdNumber) %>% names()

# save dataset with Class
scores <- bind_cols(d, get_data(fit4) %>% select(Class)) %>% 
  mutate(classes = ifelse(Class == 1, 1, 2),
         classes = factor(classes))

# frequency tables for reason to leave home variables
scores %>% 
  select(Class, matches("leave"), -LeaveHome) %>% 
  pivot_longer(matches("leave"), names_to = "var", values_to = "val") %>% 
  drop_na() %>% 
  group_by(var, Class, val) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = val, values_from = n) %>% 
  group_by(var, Class) %>% 
  mutate(total = sum(`1`,`2`,`3`,`4`,`5`, na.rm = T)) %>% 
  write_csv("output/leave_home_frequencies.csv")

# median
tmp <- scores %>% 
  select(Class, matches("leave"), -LeaveHome) %>% 
  pivot_longer(matches("leave"), names_to = "var", values_to = "val") %>% 
  drop_na() %>% 
  group_by(var, Class) %>%
  summarise(med = median(val))
  
  
  
# 1 df test: non-compliant vs compliant
# t-test
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
  rename(t = statistic, df = parameter)

sig <- test %>% 
  filter(p.value < .05)

# calculate effect size (cohen's D)
x <- scores %>% select(Class, all_of(name)) %>% 
  # mutate(classes = as.numeric(classes)) %>% 
  select(Class, sig$var)

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


tmp <- map(sig$var, function(i) {
  # paste0(i, " = ", cohen_d(x, "classes", i))
  
  data.frame(var = i,
             cohenD = cohen_d(x, "Class", i))
})

cohen <- bind_rows(tmp)

# # anova
# nested <- scores %>%
#   select(id, classes, all_of(name)) %>%
#   pivot_longer(all_of(name), names_to = "var", values_to = "val") %>%
#   nest(data = -var)
# 
# test <- nested %>%
#   mutate(fit = map(data, ~ aov(.x$val ~ .x$classes)),
#          tidy_fit = map(fit, broom::tidy)) %>%
#   unnest(tidy_fit) %>%
#   select(-data, -fit) %>%
#   rename(`F` = statistic)

# calc mean for classes
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


# scores %>% 
#   select(agreeableness,conscientious,extraversion,intellect,neuroticism,resilience_adapt) %>% 
#   pivot_longer(agreeableness:resilience_adapt, names_to = "var", values_to = "val") %>% 
#   group_by(var) %>% 
#   summarise(mean = round(mean(val, na.rm = T),2), 
#             sd = round(sd(val, na.rm = T),2),
#             mean = paste0(mean, " (", sd, ")"))

# calc eta squared
# eta <- function(fit) {
#   ss.tot <- sum((fit$model[, 1] - mean(fit$model[, 1]))^2)
#   ss.res <- sum((fit$residuals)^2)
#   ss.eff <- ss.tot - ss.res
# 
#   ss.eff / ss.tot
# }
# 
# eta <- map(n$var, function(i) {
#   fit <- aov(as.formula(paste0(i, " ~ Class")), scores)
# 
#   data.frame(var = i,
#              eta = eta(fit))
# 
# })
# 
# eta <- bind_rows(eta)
# 
# test <- test %>%
#   group_by(var) %>%
#   mutate(dfB = df[term==".x$classes"],
#          dfW = df[term=="Residuals"],
#          SSB = sumsq[term==".x$classes"],
#          SSW = sumsq[term == "Residuals"],
#          MSB = meansq[term == ".x$classes"],
#          MSW = meansq[term == "Residuals"]) %>%
#   filter(term == ".x$classes")
  
datalist <- list(test, cohen, mean_class)

reduce(datalist, left_join, by = "var") %>% 
  select(var, mean_1, mean_2, df, t, cohenD, conf.low, conf.high, p.value) %>%  # for t-test
  # select(var, mean_1, mean_2, dfB, dfW, `F`, eta, p.value, SSB, SSW, MSB, MSW) %>% # for anova
  write_csv("output/2_profile_t-test_results.csv")

psych <- c("CRT_acc", "belief_acc", "EAT_acc", "CRT_conf", "belief_conf", "EAT_conf", "trait_acc", 
           "trait_conf", "agreeableness", "conscientious", "extraversion", "intellect", "neuroticism", 
           "resilience_adapt")

coping <- c("cope_distraction", "cope_active", "cope_denial", "cope_substance", 
            "cope_emotsupp", "cope_instrsupp", "cope_disengage", "cope_venting", "cope_reframing", "cope_planning")

info <- c("SourceCheck", "CheckNews", "official_sources", "casual_sources", "official_trust", "casual_trust")

pol <- c("gov_trust", "conservatism", "reactance", "cultural_tightloose", "RWA", "morality")  

other <- c("FinancialSit", "NumberDeaths", "PublicReact", "CovidLength", "HouseholdNumber")

leave <- c("leave_going_to_work", "leave_walking_pet", "leave_physical_activity", "leave_get_food", 
           "leave_pharmacy", "leave_medical_treatment", "leave_care_dependents", "leave_meet_friends_family", 
           "leave_religion", "leave_bored", "leave_exercise_freedom")

# create plots of variables that are sig. 
vars <- test %>% filter(p.value < .05)

test_plots <- function(data) {
  data %>% 
    ggplot(aes(x = factor(classes), y = Mean, colour = var)) +
    geom_line(aes(group = 1)) + 
    geom_errorbar(aes(ymin = Mean - ci, ymax = Mean + ci),
                  width = .1) +
    facet_wrap(~var, scales = "free_y", ncol = 2) +
    labs(x = "Profile") +
    theme_classic() +
    theme(legend.position="none",
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
}


map(c("psych", "coping", "info", "pol", "other", "leave"), function(i) {
  p <- scores %>% 
    mutate(classes = factor(classes, levels = 1:2, labels = c("Non-compliant", "Compliant"))) %>% 
    select(id, classes, all_of(get("leave"))) %>% 
    select(id, classes, matches(vars$var)) %>% 
    pivot_longer(matches(vars$var), names_to = "var", values_to = "val") %>% 
    mutate(var = factor(var, levels = unique(var))) %>% 
    group_by(classes, var) %>% 
    summarise(Mean = mean(val, na.rm=T),
              ci = 1.96 * sd(val, na.rm = T) / sqrt(n()))
  
  if (i == "psych") {
  p <- p %>% 
    mutate(var = factor(var, levels = c("EAT_acc", "agreeableness", "extraversion", "resilience_adapt"), 
                        labels = c("EAT accuracy", "Agreeableness", "Extraversion", "Resilience & Adaptability")))
  } else if (i == "coping") {
    p <- p %>% 
      mutate(var = factor(var, levels = c("cope_active", "cope_planning", "cope_distraction", "cope_denial", "cope_substance", "cope_disengage"), 
                          labels = c("Active", "Planning", "Distraction", "Denial", "Substance use", "Disengage")))
  } else if (i == "info") {
    p <- p %>% 
      mutate(var = factor(var, levels = c("CheckNews", "official_sources", "official_trust", "casual_trust"), 
                          labels = c("Check news", "Sources (official)", "Trust (official)", "Trust (casual)")))
  } else if (i == "pol") {
    p <- p %>% 
      mutate(var = factor(var, levels = c("gov_trust", "reactance", "cultural_tightloose"), 
                          labels = c("Trust in government", "Reactance", "Cultural tightness-looseness")))
  } else if (i == "other") {
    p <- p %>% 
      mutate(var = factor(var, levels = c("PublicReact"), 
                          labels = c("Public Reaction")))          
  } else if (i == "leave"){
   p <- p %>% 
      mutate(var = factor(var, levels = c("leave_going_to_work", "leave_physical_activity", "leave_get_food", "leave_care_dependents", 
                                          "leave_meet_friends_family", "leave_religion", "leave_bored", "leave_exercise_freedom"), 
                          labels = c("Going to work", "Physical activity", "Get food", "Care for dependents", "Meet friends or family", "Religion", "Bored", "Exercise personal freedom")))
  }
                  
  
  # plots
  test_plots(p)
  
  ggsave(paste0("output/", "leave", "_plots.png"), height = 7, width = 12)
  
})
  
tmp <- cat %>% 
  gather(var, val, -Class) %>% 
  nest(data = -var)

cat <- select_cat()

test <- test_cat("chi")

scores <- bind_cols(d, get_data(fit2) %>% select(Class)) %>% 
  mutate(classes = ifelse(Class == 1, 1, 2),
         classes = factor(classes))

country <- scores %>% 
  # filter(Class == 1) %>% 
  select(CountryLive, Class)


chisq.test(scores$Gender, scores$Class)
  
nested %>% 
  mutate(data = map(data, ~ filter(., !is.na(val))),
         fit = map(data, ~chisq.test(.x$CountryLive, .x$count)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -method) %>% 
  rename(chi_sq = statistic, df = parameter)

scores %>% count(CountryLive)

n = length(CountryLive)
length(scores$CountryLive[scores$Class==1])

# which variables?
name <- d %>% 
  select(CRT_acc,belief_acc,EAT_acc,CRT_conf,belief_conf,EAT_conf,trait_acc,trait_conf,agreeableness,conscientious,extraversion,intellect,neuroticism,resilience_adapt,cope_distraction,cope_active,cope_denial,cope_substance,cope_emotsupp,cope_instrsupp,cope_disengage,cope_venting,cope_reframing,cope_planning,PHQ,DASS_stress,DASS_anxiety,DASS_depression,social_media_n,SourceCheck,CheckNews,official_sources,casual_sources,official_trust,casual_trust,gov_trust,conservatism,reactance,cultural_tightloose,RWA,morality,leave_going_to_work:leave_exercise_freedom,FinancialSit,worksupportedworkinghome,businessokayworkinghome,NumberDeaths,PublicReact,CovidLength,HouseholdNumber) %>% names()

scores <- bind_cols(d, get_data(fit4) %>% select(Class)) %>% 
  mutate(Class = as.factor(Class))

nested <- scores %>% 
  select(id, classes, all_of(name)) %>% 
  pivot_longer(all_of(name), names_to = "var", values_to = "val") %>% 
  nest(data = -var)

test <- nested %>% 
  mutate(fit = map(data, ~ aov(.x$val ~ .x$classes)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit) %>% 
  rename(`F` = statistic)

n <- test %>% 
  filter(p.value < .05)



library(car)

aov(EAT_conf ~ Class, data = scores)

options('contrasts')
# options(contrasts=c('contr.sum','contr.poly'))

# setup contrasts
c1 <- c(1, -1/3, -1/3, -1/3) # non-compliant vs compliant
c2 <- c(1, -1, 0, 0) # non-compliant vs young and compliant
c3 <- c(1, 0, -1, 0) # non-compliant vs educated and compliant
c4 <- c(1, 0, 0, -1) # non-compliant vs vulnerable and compliant
mat <- cbind(c1, c2, c3, c4)

contrasts(scores$Class) <- mat

models <- map(n$var[n$var != "EAT_conf"], function(i) {
  # print(i)
  # create formula
  f <- paste0(i, " ~ Class")
  
  # fit anova
  fit <- aov(as.formula(f), data = scores)
  
  # print results
  summary.aov(fit, split = list(Class = list("non-compliant vs compliant" = 1,
                                             "non-compliant vs young and compliant" = 2,
                                             "non-compliant vs educated and compliant" = 3,
                                             "non-compliant vs vulnerable and compliant" = 4
                                             ))) 
  
  # x <- summary.aov(fit, split = list(Class = list("non-compliant vs young and compliant" = 1, 
  #                                                 "non-compliant vs educated and compliant" = 2,
  #                                                 "non-compliant vs vulnerable and compliant" = 3)))[[1]][["Pr(>F)"]]
  # p.adjust(x[-c(1,5)], method = "bonferroni")
})



scores %>% 
  filter(Class == 1) %>% 
  summarise(m = mean(casual_sources, na.rm = T),
            sd = sd(casual_sources, na.rm = T))

n$var[-1:-11]
n$var[12:16]
p.adjust(0.00064, method = "bonferroni")
1.16e-05
7.38e-05


schef

models

i <- 21
{
print(n$var[i])
# models[[i]]

f <- paste0(n$var[i]," ~ Class")
fit <- aov(as.formula(f), data = scores)

summary.aov(fit, split = list(Class = list("non-compliant vs compliant" = 1, 
                                           "non-compliant vs young and compliant" = 2, 
                                           "non-compliant vs educated and compliant" = 3,
                                           "non-compliant vs vulnerable and compliant" = 4)))
}




scores <- scores %>% 
  mutate(classes = ifelse(Class == 1, 1, 2),
         classes = factor(classes))



fit <- aov(cope_disengage ~ classes, data = scores)

ss.tot <- sum((fit$model[, 1] - mean(fit$model[, 1]))^2)
ss.res <- sum((fit$residuals)^2)
ss.eff <- ss.tot - ss.res

ss.eff / ss.tot




scores %>% 
  select(Class, all_of(n$var)) %>% 
  group_by(Class) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T)) %>% 
  select(matches("leave"))


scores %>% 
  select(Class,gov_trust,Govt_Truth, Govt_Satisfaction, Govt_Extreme) %>% 
  filter(Class == 1) %>% 
  summarise(m = across(where(is.numeric), mean, na.rm = T))


scores %>% filter(Class != 1) %>% summarise(r = cor(gov_trust, Govt_Satisfaction))


tmp <- scores %>% 
  select(classes, all_of(name)) %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = T)) %>% 
  select(matches("sources"))

name

# descriptives for paper
x <- bind_cols(read_csv("data/COVID-19_FourCountries_BC_G_Removed_Imputed-Merged_V3.csv"), get_data(fit4) %>% select(Class)) %>% 
  mutate(classes = ifelse(Class == 1, 1, 2),
         classes = factor(classes)) %>% 
  rename(leave_going_to_work = ReasonsLeaveHome_1, leave_walking_pet = ReasonsLeaveHome_2,
         leave_physical_activity = ReasonsLeaveHome_3, leave_get_food = ReasonsLeaveHome_4, 
         leave_pharmacy = ReasonsLeaveHome_5, leave_medical_treatment = ReasonsLeaveHome_6,
         leave_care_dependents = ReasonsLeaveHome_7, leave_meet_friends_family = ReasonsLeaveHome_8, 
         leave_religion = ReasonsLeaveHome_9, leave_bored = ReasonsLeaveHome_10, 
         leave_exercise_freedom = ReasonsLeaveHome_11) %>% 
  select(classes, all_of(name[!name %in% c("trait_acc", "trait_conf", "social_media_n", "official_sources", "casual_sources", "official_trust", "casual_trust")])) %>%
  pivot_longer(all_of(name[!name %in% c("trait_acc", "trait_conf", "social_media_n", "official_sources", "casual_sources", "official_trust", "casual_trust")]), names_to = "var", values_to = "val") %>% 
  group_by(var) %>% 
  summarise(mean= mean(val, na.rm = T),
            sd = sd(val, na.rm = T))
  
x <- x[match(name, x$var),]



x %>% write_csv("output/anova_descriptives.csv")
  

tmp <- d %>% 
  select(id, resilience, matches("adaptability"), -adaptability_crisis, -adaptability_uncertainty, -adaptability_2, -adaptability_4, -adaptability_8) %>% 
  pivot_longer(adaptability_1:adaptability_8R, names_to = "var", values_to = "val") %>% 
  group_by(id) %>% 
  mutate(adapt = sum(val, na.rm = T)) %>% 
  ungroup() %>% 
  select(-id, -var, -val) %>% 
  summarise(across(everything(), list(mean, sd), na.rm = T))

d %>% select(id, resilience_1:resilience_10) %>% 
  gather(var, val, -id) %>% 
  group_by(id) %>% 
  summarise(m = sum(val, na.rm = T))


d %>% select(SourceCheck, CheckNews, official_sources, casual_sources) %>% 
  summarise(across(everything(), list(mean, sd), na.rm = T))

sd(d$gov_trust)
