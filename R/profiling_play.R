# 
# x <- scores %>% 
#   mutate(Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female")),
#          Class = factor(Class, levels = 1:3, labels = c("1", "2", "3")))

packages <- c("tidyverse", "knitr", "kableExtra", "GGally", "naniar", "tidyLPA", "here",
              "psych", "broom")
purrr::map(packages, library, character.only = TRUE)

# load data
d <- read_csv(here("data/200805_covid_full_imputed_std_data.csv"), guess_max = 1575)

x <- d %>%  
  select(Age, Education, Physicalhealth, HealthConditions, scrict_isolation, social_distancing,
         herd_immunity_economy, complaince_selfreport, social_distance_isolation,
         sickness_actions, Hygiene, antisocial_behaviours, prosocial_behaviours,CovidWorry)

# functions to fit and plot LPA models
fit_lpa <- function(profiles) { # set number of profiles to extract (i)
  # set seed for reproducible results
  set.seed(23) 
  
  # fit model
  x %>% 
    # scale() %>%
    estimate_profiles(profiles, 
                      variances = "equal",
                      covariances = "zero")
}


# fit lpa model
fit <- fit_lpa(2)

# # plot profiles
get_estimates(fit) %>% 
  filter(Category == "Means") %>%
  select(Parameter, Estimate, se, Class) %>% 
  mutate(Class = factor(Class),
         Parameter = factor(Parameter, 
                            levels = c("Age", "Education", "Physicalhealth", "HealthConditions", "CovidWorry", "scrict_isolation", "social_distancing", "herd_immunity_economy", "complaince_selfreport", "social_distance_isolation", "sickness_actions", "Hygiene", "antisocial_behaviours", "prosocial_behaviours"))) %>% 
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
  geom_line(aes(linetype = Class, colour = Class)) +
  geom_errorbar(aes(ymin = Estimate - se, ymax = Estimate + se, colour = Class),
                width = .1) +
  labs(x = "Variable",
       y = "Mean") +          
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_colour_manual(values=c("red", "blue", "purple"))

# save profile scores
scores <- bind_cols(d, get_data(fit) %>% select(model_number, classes_number, contains("CPROB"), Class)) %>% 
  mutate(Class = factor(Class))







rename(leave_going_to_work = ReasonsLeaveHome_1, leave_walking_pet = ReasonsLeaveHome_2,
       leave_physical_activity = ReasonsLeaveHome_3, leave_get_food = ReasonsLeaveHome_4, 
       leave_pharmacy = ReasonsLeaveHome_5, leave_medical_treatment = ReasonsLeaveHome_6,
       leave_care_dependents = ReasonsLeaveHome_7, leave_meet_friends_family = ReasonsLeaveHome_8, 
       leave_religion = ReasonsLeaveHome_9, leave_bored = ReasonsLeaveHome_10, 
       leave_exercise_freedom = ReasonsLeaveHome_11)

# cat


# clean HouseholdNumber
# 6 said 0
# 1 said 45
# 1 said 200



# Profile each class ------------------------------------------------------
cont <- d %>% select(CRT_acc,belief_acc,EAT_acc,agreeableness,conscientious,extraversion,intellect,
             neuroticism,resilience_adapt,cope_distraction,cope_active,cope_denial,cope_substance,
             cope_emotsupp,cope_instrsupp,cope_disengage,cope_venting,cope_reframing,cope_planning,
             gov_trust,conservatism,reactance,cultural_tightloose,RWA,morality,FinancialSit,
             worksupportedworkinghome,businessokayworkinghome,PHQ,DASS_stress,DASS_anxiety,
             DASS_depression,official_sources,casual_sources,SourceCheck,CheckNews,NumberInfected,
             NumberDeaths,NumberInfectmonth,PublicReact,CovidLength,LeaveHome,
             ReasonsLeaveHome_1:ReasonsLeaveHome_11,OtherReason,HouseholdNumber,social_media_n,
             official_trust,casual_trust)

gg_miss_var(cont, show_pct = TRUE)
 
# # remove if more than 90% of data missing
miss <- miss_var_summary(cont) %>% filter(pct_miss < 90)
# cont <- cont %>% select(miss$variable)




print_test <- function() {
  if (length(unique(scores$Class))==2) {
    test %>% 
      mutate(p.value = round(p.value, 3),
             p.value = ifelse(p.value < .001, "<.001", p.value)) %>% 
      kable(booktabs = T, caption = "Differences between latent profiles on variables outside model") %>%
      kable_styling(font_size = 6, latex_options = "HOLD_position") %>% 
      row_spec(which(test$p.value < .05), bold = TRUE, color = "red")
    
  } else if (length(unique(scores$Class))>2) {
    test %>% 
      mutate(p.value = round(p.value, 3),
             p.value = ifelse(p.value < .001, "<.001", p.value),
             `F` = round(`F`, 2),
             `F` = ifelse(is.na(`F`), "", `F`),
             p.value = ifelse(is.na(p.value), "", p.value)) %>% 
      kable(booktabs = T, caption = "Differences between latent profiles on variables outside model") %>%
      kable_styling(font_size = 6, latex_options = "HOLD_position") %>% 
      row_spec(which(test$p.value < .05), bold = TRUE, color = "red")
  }
}





test_cont()

print_test()

plot_cont()








# categorical -------------------------------------------------------------
test_cat <- function(type) {
    # use Fisher's exact test if any cells contain less than 5 participants
    # identify which variables to use Fisher's exact test
    fisher <- map(names(x), function(i) {
      x <- table(x %>% select(Class, i))
      
      if (any(x < 5)) {
        tibble(var = i)
      }
    })
    
    fisher <- bind_rows(fisher)
    
    # prepare data
    nested <- x %>% 
      gather(var, val, -Class) %>% 
      nest(data = -var)
    
  if (type == "chi") {
    # compute chi square test
    test <- nested %>% 
      filter(!var %in% fisher$var) %>% 
      mutate(data = map(data, ~ filter(., !is.na(val))),
             fit = map(data, ~chisq.test(.x$Class, .x$val)),
             tidy_fit = map(fit, broom::tidy)) %>% 
      unnest(tidy_fit) %>% 
      select(-data, -fit, -method) %>% 
      rename(chi_sq = statistic, df = parameter)
    
  } else if (type == "fisher") {
    # compute fisher's exact test
    # SLOWER: to get exact p-values use this code
    # nested %>% 
    #   filter(var %in% tmp$var) %>% 
    #   mutate(data = map(data, ~ filter(., !is.na(val))),
    #          fit = map(data, ~fisher.test(.x$Class, .x$val, workspace = 2e+09)),
    #          tidy_fit = map(fit, broom::tidy)) %>% 
    #   unnest(tidy_fit) %>% 
    #   mutate_if(is.numeric, round, 4) %>%
    #   select(-data, -fit, -method) %>% 
    #   rename(chi_sq = statistic, p = p.value, df = parameter)
    
    # FASTER: to simulate p-values using monte carlo use this code
    test <- nested %>% 
      filter(var %in% fisher$var) %>% 
      mutate(data = map(data, ~ filter(., !is.na(val))),
             fit = map(data, ~fisher.test(.x$Class, .x$val, simulate.p.value = TRUE)),
             tidy_fit = map(fit, broom::tidy)) %>% 
      unnest(tidy_fit) %>% 
      select(-data, -fit, -method, -alternative)
  }
    
}

# select vars
x <- scores %>% select(Class,Gender,Workessential,LossofJob,Wherework,Layoff,MaritalStatus,Children,
                       Elderly,ElderlyLive,Job,OtherReason,
                       -OtherReason) %>% 
  mutate(Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female")),
         Workessential = factor(Workessential, levels = 1:3, labels = c("Essential", "Non-essential", "Unsure")),
         Wherework = factor(Wherework, levels = c("none", "organisation", "organisation+self_employ", "self_employ"), 
                            labels = c("none", "organisation", "self_employ", "organisation+self_employ")))

test <- test_cat(type = "fisher")

print_test()




plot_chi <- function() {
  # Plot sig. vars
  vars <- test %>% filter(p.value < .05)
  i <- "Wherework"
  p <- map(vars$var, function(i) {
    p <- x %>% 
      select(Class, i) %>%
      rename(var = i) %>% 
      drop_na() %>% 
      group_by(Class, var) %>% 
      summarise(v = i,
                n = n()) %>% 
      group_by(var) %>% 
      mutate(n_tot = sum(n)) %>% 
      group_by(Class) %>% 
      mutate(percent = round(100*n/n_tot,0),
             percent = paste0(percent, "%"))
    
    if (length(levels(x[[match(i, names(x))]]))==2) {
      p <- p %>% 
        group_by(Class) %>% 
        mutate(lab_ypos = ifelse(var==levels(var)[1], .5*n + n[var==levels(var)[2]], .5*n))
      
    } else if (length(levels(x[[match(i, names(x))]]))==3) {
      p <- p %>% 
        group_by(Class) %>% 
        mutate(lab_ypos = ifelse(var==levels(var)[1], .5*n + n[var==levels(var)[2]], 
                                 ifelse(var==levels(var)[2], .5*n + n[var==levels(var)[3]], .5*n)))
      
    } else if (length(levels(x[[match(i, names(x))]]))==4) {
      p <- p %>% 
        group_by(Class) %>% 
        mutate(lab_ypos = ifelse(var==levels(var)[1], .5*n + n[var==levels(var)[2]], na.rm = T,
                                 ifelse(var==levels(var)[2], .5*n + n[var==levels(var)[3]], na.rm = T, 
                                        ifelse(var==levels(var)[3], .5*n + n[var==levels(var)[4]], na.rm = T, .5*n))))
    }
    
    gg_list <- map(p, function(i) {
      i %>%
        ggplot(aes(x = factor(Class), y = n)) +
        geom_col(aes(fill = var), width = 0.7) +
        # facet_wrap(~v, scales = "free_y") +
        geom_text(aes(y = lab_ypos, label = percent, group = var), color = "white") +
        labs(x = "Class", y = "Frequency", fill = i$v[1], title = paste0(i$v[1], ": % in each Profile")) +
        theme_classic() +
        theme(strip.background = element_blank(),
              plot.title = element_text(hjust = 0.5))
      
    })
    
    cowplot::plot_grid(plotlist = gg_list, ncol = 1,
                       align = 'v')
    
  })
}


plot_fisher <- function() {
  p <- p %>% 
    mutate(lab_ypos = ifelse(Class==1 & var==levels(var)[1], .5*n + n[var==levels(var)[2]],
                             ifelse(Class==1 & var==levels(var)[2], .5*n + n[var==levels(var)[4]],
                                    ifelse(Class==1 & var==levels(var)[4], .5*n,
                                           ifelse(Class==2 & var==levels(var)[1], .5*n + n[var==levels(var)[2]],
                                                  ifelse(Class==2 & var==levels(var)[2], .5*n + n[var==levels(var)[3]],
                                                         ifelse(Class==2 & var==levels(var)[3], .5*n + n[var==levels(var)[4]],
                                                                ifelse(Class==2 & var==levels(var)[4], .5*n, NA))))))))
  
  p %>%
    ggplot(aes(x = factor(Class), y = n)) +
    geom_col(aes(fill = var), width = 0.7) +
    # facet_wrap(~v, scales = "free_y") +
    geom_text(aes(y = lab_ypos, label = percent, group = var), color = "white") +
    labs(x = "Class", y = "Frequency", fill = p$v[1], title = paste0(p$v[1], ": % in each Profile")) +
    theme_classic() +
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
}


plot_chi()
plot_fisher()





# categorical variables ---------------------------------------------------
# Gender # 1 = Male 2 = Female
# Diagnosis # 1 = yes 2 = think so unconfirmed 3 = no (categorical)
# Symptoms # 1 = none 2 = mild to moderate 3 = critical or serious
# Diagnosis_other # 1 = yes 2 = think so unconfirmed 3 = no (categorical)
# Symptoms_other # 1 = none 2 = mild to moderate 3 = critical or serious 4 = death
# SocialMedia_1:SocialMedia_8_TEXT # (dont bother)
# Workessential # 1 = yes 2 = no 3 = unsure
# Wherework # 1 = organisation 2 = self employed 3 = none
# # if selected none to wherework:
# LossofJob  # 1 = laid off due to covid 2 = closed business due to covid 3 = none of above
# LossofJobOrg # if laid off from org 1 = temp 2 = permanent 3 = unsure
# LossofJobBiz # if laid off from self employed 1 = temp 2 = permanent 3 = unsure
# Layoff # at risk of being laid off? 1 = yes 2 = no
# MaritalStatus # 1 = married 2 = de facto 3 = separated 4 = single
# Children # 1 = yes 2 = no
# Homeschool # 1 = yes 2 = no
# Elderly # +65 relatives? 1 = yes 2 = no
# ElderlyLive # live with elderly? 1 = yes 2 = no
# Job # 1 = yes 2 = no
# select variables
x <- scores %>% select(Class,Gender,Workessential,LossofJob,Wherework,Layoff,MaritalStatus,Children,
                  Elderly,ElderlyLive,Job,OtherReason,
                  -OtherReason) %>% 
  mutate(Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female")),
         Workessential = factor(Workessential, levels = 1:3, labels = c("Essential", "Non-essential", "Unsure")),
         Wherework = factor(Wherework, levels = c("none", "organisation", "organisation+self_employ", "self_employ"), 
                            labels = c("none", "organisation", "self_employ", "organisation+self_employ")))

# # missing value analysis
# gg_miss_var(x, show_pct = TRUE)
# 
# # remove if more than 75% of data missing
# miss <- miss_var_summary(x) %>% filter(pct_miss < 75)
# x <- x %>% select(Class, miss$variable)

# use Fisher's exact test if any cells contain less than 5 participants
# identify which variables to use Fisher's exact test
fisher <- map(names(x), function(i) {
  x <- table(x %>% select(Class, i))
  
  if (any(x < 5)) {
    tibble(var = i)
  }
})

fisher <- bind_rows(fisher)


# chi square test on categorical variables
nested <- x %>% 
  gather(var, val, -Class) %>% 
  nest(data = -var)

# compute chi square test
test <- nested %>% 
  filter(!var %in% fisher$var) %>% 
  mutate(data = map(data, ~ filter(., !is.na(val))),
         fit = map(data, ~chisq.test(.x$Class, .x$val)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  mutate(p.value = round(p.value, 3),
         p.value = ifelse(p.value < .001, "<.001", p.value)) %>% 
  select(-data, -fit, -method) %>% 
  rename(chi_sq = statistic, df = parameter)


test %>% 
  kable(booktabs = T, caption = "Goodness of fit indices for 2-6 profile models") %>%
  kable_styling(font_size = 6, latex_options = "HOLD_position") %>% 
  row_spec(which(test$p < .05), bold = TRUE, color = "red")

# Plot sig. vars
vars <- test %>% filter(p.value < .05)

map(vars$var, function(i) {
p <- x %>% 
  select(Class, i) %>%
    rename(var = i) %>% 
  drop_na() %>% 
  group_by(Class, var) %>% 
  summarise(n = n()) %>% 
  group_by(var) %>% 
  mutate(n_tot = sum(n)) %>% 
  group_by(Class) %>% 
  mutate(percent = round(100*n/n_tot,0),
         percent = paste0(percent, "%"))




if (length(levels(x[[match(i, names(x))]]))==2) {
  p <- p %>% 
    group_by(Class) %>% 
    mutate(lab_ypos = ifelse(var==levels(var)[1], .5*n + n[var==levels(var)[2]], .5*n))
  
  } else if (length(levels(x[[match(i, names(x))]]))==3) {
  p <- p %>% 
    group_by(Class) %>% 
    mutate(lab_ypos = ifelse(var==levels(var)[1], .5*n + n[var==levels(var)[2]], 
                      ifelse(var==levels(var)[2], .5*n + n[var==levels(var)[3]], .5*n)))

  } else if (length(levels(x[[match(i, names(x))]]))==4) {
    p <- p %>% 
      group_by(Class) %>% 
      mutate(lab_ypos = ifelse(var==levels(var)[1], .5*n + n[var==levels(var)[2]], na.rm = T,
                        ifelse(var==levels(var)[2], .5*n + n[var==levels(var)[3]], na.rm = T, 
                        ifelse(var==levels(var)[3], .5*n + n[var==levels(var)[4]], na.rm = T, .5*n))))
  }

  p %>% 
    ggplot(aes(x = Class, y = n)) +
    geom_col(aes(fill = var), width = 0.7) +
    geom_text(aes(y = lab_ypos, label = percent, group = var), color = "white") +
    labs(x = "Class", y = "Frequency", fill = names(x)[match(i, names(x))], title = paste0("% ", names(x)[match(i, names(x))], " in each Profile")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
})


# compute fisher's exact test
# SLOWER: to get exact p-values use this code
# nested %>% 
#   filter(var %in% tmp$var) %>% 
#   mutate(data = map(data, ~ filter(., !is.na(val))),
#          fit = map(data, ~fisher.test(.x$Class, .x$val, workspace = 2e+09)),
#          tidy_fit = map(fit, broom::tidy)) %>% 
#   unnest(tidy_fit) %>% 
#   mutate_if(is.numeric, round, 4) %>%
#   select(-data, -fit, -method) %>% 
#   rename(chi_sq = statistic, p = p.value, df = parameter)

# FASTER: to simulate p-values using monte carlo use this code
test <- nested %>% 
  filter(var %in% fisher$var) %>% 
  mutate(data = map(data, ~ filter(., !is.na(val))),
         fit = map(data, ~fisher.test(.x$Class, .x$val, simulate.p.value = TRUE)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -method, -alternative)

test %>% 
  mutate(p.value = round(p.value, 3),
         p.value = ifelse(p.value < .001, "<.001", p.value)) %>% 
  kable(booktabs = T, caption = "Goodness of fit indices for 2-6 profile models") %>%
  kable_styling(font_size = 6, latex_options = "HOLD_position") %>% 
  row_spec(which(test$p.value < .05), bold = TRUE, color = "red")

# Plot sig. vars
vars <- test %>% filter(p < .05)


  p <- x %>% 
    select(Class, Wherework) %>%
    rename(var = Wherework) %>% 
    drop_na() %>% 
    group_by(Class, var) %>% 
    summarise(n = n()) %>% 
    group_by(var) %>% 
    mutate(n_tot = sum(n)) %>% 
    group_by(Class) %>% 
    mutate(percent = round(100*n/n_tot,0),
           percent = paste0(percent, "%"),
           lab_ypos = ifelse(Class==1 & var==levels(var)[1], .5*n + n[var==levels(var)[2]],
                             ifelse(Class==1 & var==levels(var)[2], .5*n + n[var==levels(var)[4]],
                                    ifelse(Class==1 & var==levels(var)[4], .5*n,
                                           ifelse(Class==2 & var==levels(var)[1], .5*n + n[var==levels(var)[2]],
                                                  ifelse(Class==2 & var==levels(var)[2], .5*n + n[var==levels(var)[3]],
                                                         ifelse(Class==2 & var==levels(var)[3], .5*n + n[var==levels(var)[4]], 
                                                                ifelse(Class==2 & var==levels(var)[4], .5*n, NA))))))))
  
  p %>% 
    ggplot(aes(x = Class, y = n)) +
    geom_col(aes(fill = var), width = 0.7) +
    geom_text(aes(y = lab_ypos, label = percent, group = var), color = "white") +
    labs(x = "Class", y = "Frequency", fill = names(x)[match(i, names(x))], title = paste0("% ", names(x)[match(i, names(x))], " in each Profile")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  




#Gender
# 1 == Male
# 2 == Female
# 3 == Other

table(scores %>% select(Gender))

p <- x %>% 
  select(Class, Gender) %>% 
  filter(!is.na(Gender)) %>% 
  mutate(Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female"))) %>% 
  group_by(Class, Gender) %>% 
  summarise(n = n()) %>% 
  group_by(Gender) %>% 
  mutate(n_g = sum(n)) %>% 
  group_by(Class) %>% 
  mutate(percent = round(100*n/n_g,0),
         percent = paste0(percent, "%"),
         lab_ypos = ifelse(Gender == "Male", .5*n + n[Gender=="Female"], .5*n))

p %>% 
  ggplot(aes(x = Class, y = n)) +
  geom_col(aes(fill = Gender), width = 0.7) +
  geom_text(aes(y = lab_ypos, label = percent, group = Gender), color = "white") +
  labs(x = "Class", y = "Frequency", fill = "Gender", title = "% Gender in each Profile") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# ordinal variables -------------------------------------------------------
# FinancialSit # 1 = worse off 2 = same 3 = better off (ordinal)
# CheckNews, SourceCheck, NewsShare # 1-5 Never-Multiple times a day (ordinal)
# PublicReact # 1 = much too extreme 3 = appropriate 5 = not sufficient (ordinal?)
# CovidLength # 1 = less than 1 month 5 = 10-12 months 8 more than 5 years (ordinal but not equally spaced)
# Trust_1:Trust_4 # 1 = strong distrust 5 = strong trust
# morality_1:morality_6 # 1 = fully disagree 5 = fully agree
x <- scores %>% 
  select(Class,id,Age,FinancialSit,CheckNews,SourceCheck,NewsShare,PublicReact,CovidLength,
         casual_trust,official_trust,morality)

x <- scores %>% 
  select(CRT_acc,belief_acc,EAT_acc,agreeableness,conscientious,extraversion,intellect,
         neuroticism,resilience_adapt,cope_distraction,cope_active,cope_denial,cope_substance,
         cope_emotsupp,cope_instrsupp,cope_disengage,cope_venting,cope_reframing,cope_planning,
         gov_trust,conservatism,reactance,cultural_tightloose,RWA,morality,worksupportedworkinghome,
         businessokayworkinghome,PHQ,DASS_stress,DASS_anxiety,DASS_depression,official_sources,
         casual_sources,CheckNews,SourceCheck,NumberInfected,NumberDeaths,NumberInfectmonth,
         PublicReact,CovidLength,LeaveHome,ReasonsLeaveHome_1:ReasonsLeaveHome_11,HouseholdNumber,
         social_media_n,official_trust,casual_trust,Diagnosis,Symptoms,Diagnosis_Other,Symptoms_Other)

# missing value analysis
gg_miss_var(x, show_pct = TRUE)

# remove if more than 75% of data missing
# miss <- miss_var_summary(x) %>% filter(pct_miss < 75)
# x <- x %>% select(Class, miss$variable)


# conduct tests on difference between profile groups on demographic variables
test_demo <- function() {
  if (length(unique(scores$Class))==2) {
    nested <- x %>% 
      # select(id, Class, Education, AnnualIncome_usd) %>% 
      gather(var, val, -Class,-id) %>% 
      spread(Class, val) %>% 
      nest(data = -var)
    
    test <- nested %>% 
      mutate(fit = map(data, ~ t.test(.x$`1`, 
                                      .x$`2`, paired = F, var.equal = T)),
             tidy_fit = map(fit, broom::tidy)) %>% 
      unnest(tidy_fit) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      select(-data, -fit, -method, -alternative) %>% 
      rename(mean_1 = estimate1, mean_2 = estimate2, t = statistic, df = parameter)
    
  } else if (length(unique(scores$Class))>2) {
    
    nested <- x %>% 
      # select(id, Class, Education, AnnualIncome_usd) %>% 
      mutate(Class = factor(Class)) %>% 
      gather(var, val, -Class) %>% 
      nest(data = -var)
    
    test <- nested %>% 
      mutate(fit = map(data, ~ aov(.x$val ~ .x$Class)),
             tidy_fit = map(fit, broom::tidy)) %>% 
      unnest(tidy_fit) %>% 
      mutate_if(is.numeric, round, 3) %>%
      select(-data, -fit) %>% 
      rename(`F` = statistic, p = p.value) %>% 
      mutate(`F` = ifelse(is.na(`F`), "", `F`),
             p = ifelse(is.na(p), "", p))
    
  }
}
  

test_demo() %>% print













chisq.test(scores$Class, scores$Diagnosis_Other)


fisher.test(scores$Class, scores$Diagnosis)
fisher.test(scores$Class, scores$Wherework, workspace = 2000000)

Xsq <- chisq.test(scores$Class, scores$Diagnosis, simulate.p.value = F) 
chisq.test(scores$Class, scores$Wherework, simulate.p.value = T)

Chi-squared approximation may be incorrect


Xsq$observed   # observed counts (same as M)
round(Xsq$expected,0)   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals


scores$
  
  a <- table(scores$Class, scores$Wherework)
















table(scores$NewsShare)
chisq.test(scores$Class, scores$CountryBorn)


# continuous
g_md
NumberInfected
NumberDeaths
NumberInfectmonth
Infect_Pers:Infect_Avg
Fines_gath_1_1:Fines_sympt_1_2
NumberInfected, NumberDeaths, NumberInfectmonth
SocialCircle_1:SocialCircle_3
HouseholdNumber # how many live in your household?


# ordinal



# categorical









# OrgSize
# OrgSizePrev
# ElderKidsPrev
# ElderKidsPrevFreq          
# ElderKidsNow
# ElderKidsNowFreq





scores %>% select(SocialMedia_1:SocialMedia_8_TEXT)





tmp <- d %>% 
  # filter(!is.na(SocialMedia_8_TEXT)) %>% 
  # select(SocialMedia_8_TEXT) %>%   
  
  mutate()


# social <- d %>% 
#   select(id, contains("SocialMedia")) %>%
#   mutate(SocialMedia_8_TEXT = tolower(SocialMedia_8_TEXT),
#          SocialMedia_8_TEXT = ifelse(SocialMedia_8_TEXT == "snapchat news stories", "snapchat", SocialMedia_8_TEXT),
#          SocialMedia_8_TEXT = ifelse(str_detect(SocialMedia_8_TEXT, paste(remove, collapse = "|")), NA, SocialMedia_8_TEXT),
#          SocialMedia_8_TEXT = ifelse(str_detect(SocialMedia_8_TEXT, paste(double, collapse = "|")), 2, SocialMedia_8_TEXT),
#          SocialMedia_8_TEXT = ifelse(SocialMedia_8_TEXT == "2" | is.na(SocialMedia_8_TEXT), SocialMedia_8_TEXT, 1),
#          SocialMedia_8_TEXT = as.numeric(SocialMedia_8_TEXT)) %>% 
#   gather(var, val, -id) %>%
#   group_by(id) %>% 
#   summarise(social = sum(val, na.rm=T))
# 
# 
# scores <- scores %>% left_join(social, by = "id")







# machine learning: classification ----------------------------------------
# missing values analysis -------------------------------------------------
x <- scores %>% 
  select(g_md,NumberInfected,NumberDeaths,NumberInfectmonth,Infect_Pers:Infect_Avg,
         Fines_gath_usd,Fines_sympt_usd,NumberInfected, NumberDeaths, NumberInfectmonth,
         SocialCircle_1:SocialCircle_3,HouseholdNumber,
         FinancialSit,CheckNews, SourceCheck, NewsShare,PublicReact,
         CovidLength,official_trust,casual_trust,morality,
         Gender,Diagnosis,Symptoms,Diagnosis_Other,Symptoms_Other,social_media_n,
         Workessential,Wherework,LossofJob,LossofJobOrg,LossofJobBiz,
         Layoff,MaritalStatus,Children,Homeschool,Elderly,ElderlyLive,Job)

gg_miss_var(x, show_pct = TRUE)

# only use variables with less than 25% of data missing
miss <- miss_var_summary(x) %>% filter(pct_miss < 25)


tmp <- scores %>% 
  select(miss$variable) %>% 
  select_if(is.numeric) %>% 
  mclust::imputeData(seed = 1) %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, scale) %>% 
  bind_cols(scores %>% select(id, Class, Wherework))

# impute missing values

# use vars in tmp to predict Class

