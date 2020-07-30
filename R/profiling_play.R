# 
# x <- scores %>% 
#   mutate(Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female")),
#          Class = factor(Class, levels = 1:3, labels = c("1", "2", "3")))

library(naniar)
library(GGally)
library(psych)

# first prepare data
# trust
source("R/pca/pca_trust.R")

# wherework turn into single variable
work <- d %>% 
  select(id, Wherework_1:Wherework_3) %>% 
  pivot_longer(names_to = "var", values_to = "val", cols = -id) %>% 
  mutate(Wherework = ifelse(var == "Wherework_1" & val == 1, "org",
                            ifelse(var == "Wherework_2" & val == 1, "self-employ",
                                   ifelse(var == "Wherework_3" & val == 1, "none", NA)))) %>% 
  filter(!is.na(Wherework)) %>% select(-var, -val)

# LossofJob turn into single variable
loss <- scores %>% 
  select(id, LossofJob_1:LossofJob_3) %>% 
  pivot_longer(names_to = "var", values_to = "val", cols = -id) %>% 
  mutate(LossofJob = ifelse(var == "LossofJob_1" & val == 1, "laid_off",
                            ifelse(var == "LossofJob_2" & val == 1, "closed_biz",
                                   ifelse(var == "LossofJob_3" & val == 1, "none", NA)))) %>% 
  filter(!is.na(LossofJob)) %>% select(-var, -val)

# SocialMedia turn into single variable
remove <- c("buzzfeed", "google", "abc", "tot", "news", "spotify", "bbc", "line")
double <- c("tumblr, tiktok", "4chan.org/pol", "linkedin, telegram")

social <- scores %>%
  select(id, contains("SocialMedia")) %>%
  mutate(SocialMedia_8_TEXT = tolower(SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(SocialMedia_8_TEXT == "snapchat news stories", "snapchat", SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(str_detect(SocialMedia_8_TEXT, paste(remove, collapse = "|")), NA, SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(str_detect(SocialMedia_8_TEXT, paste(double, collapse = "|")), 2, SocialMedia_8_TEXT),
         SocialMedia_8_TEXT = ifelse(SocialMedia_8_TEXT == "2" | is.na(SocialMedia_8_TEXT), SocialMedia_8_TEXT, 1),
         SocialMedia_8_TEXT = as.numeric(SocialMedia_8_TEXT)) %>%
  gather(var, val, -id) %>%
  group_by(id) %>%
  summarise(social_n = sum(val, na.rm=T))

# fines
# exchange rates:
# 1 GBP == 1.29 USD
# 1 AUS == .71 USD
# 1 CAN == .74
# fines_usd <- d %>% 
#   select(id, CountryLive, Fines_gath_1_1, Fines_sympt_1_1) %>% 
#   pivot_longer(names_to = "var", values_to = "val", cols = c(Fines_gath_1_1, Fines_sympt_1_1)) %>% 
#   mutate(val = as.numeric(val),
#          val = ifelse(CountryLive == "Australia", val*.71,
#                       ifelse(CountryLive == "Canada", val*.74,
#                              ifelse(CountryLive == "UK", val*1.29, val)))) %>% 
#   pivot_wider(names_from = var, values_from = val) %>% 
#   select(-CountryLive) %>% 
#   rename(Fines_gath_usd = Fines_gath_1_1, Fines_sympt_usd = Fines_sympt_1_1)

# combine with main dataset
datalist <- list(scores, work, loss, social, pca_scores)


tmp <- datalist %>% reduce(left_join, by = "id")


# for some reason these ids have duplicate entries need to remove dup
c <- tmp %>% filter(id %in% c(89,152,271,382,480,489,623,785,1036,1046,1063,1069,1101,1346)) %>% 
  select(id, social_n,official_trust,casual_trust,Fines_gath_usd,Fines_sympt_usd)

x <- scores %>% 
  select(g_md,NumberInfected,NumberDeaths,NumberInfectmonth,Infect_Pers:Infect_Avg,
         Fines_gath_usd,Fines_sympt_usd,NumberInfected, NumberDeaths, NumberInfectmonth,
         SocialCircle_1:SocialCircle_3,HouseholdNumber,
         FinancialSit,CheckNews, SourceCheck, NewsShare,PublicReact,
         CovidLength,official_trust,casual_trust,morality,
         Gender,Diagnosis,Symptoms,Diagnosis_Other,Symptoms_Other,social_n,
         Workessential,Wherework,LossofJob,LossofJobOrg,LossofJobBiz,
         Layoff,MaritalStatus,Children,Homeschool,Elderly,ElderlyLive,Job)



gg_miss_var(x, show_pct = TRUE)

miss <- miss_var_summary(x) %>% filter(pct_miss < 25)

x <- scores %>% 
  select(id, Class, miss$variable) %>% 
  drop_na()




# chi square test on categorical variables




nested <- scores %>% 
  select(Class, Gender,Diagnosis,Symptoms,Diagnosis_Other,Symptoms_Other,SocialMedia_1:SocialMedia_8_TEXT,
         Workessential,LossofJob,LossofJobOrg,LossofJobBiz, Wherework
         Layoff,MaritalStatus,Children,Homeschool,Elderly,ElderlyLive,Job) %>% 
  gather(var, val, -Class) %>% 
  nest(data = -var)

table(scores$Wherework_1)

nested %>% 
  mutate(data = map(data, ~ filter(., !is.na(val))),
         fit = map(data, ~chisq.test(.x$Class, .x$val)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  mutate_if(is.numeric, round, 4) %>%
  select(-data, -fit, -method) %>% 
  rename(chi_sq = statistic, p = p.value, df = parameter)

















#Gender
# 1 == Male
# 2 == Female
# 3 == Other

table(scores %>% select(Gender))

p <- x %>% 
  select(Class, Gender) %>% 
  filter(!is.na(Gender)) %>% 
  # mutate(Gender = factor(Gender, levels = 1:2, labels = c("Male", "Female"))) %>% 
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
  
chisq.test(scores$Class, scores$Gender)
chisq.test(x$Class, x$Gender)


  
  



Xsq$observed   # observed counts (same as M)
round(Xsq$expected,0)   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals





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
FinancialSit # 1 = worse off 2 = same 3 = better off (ordinal)
CheckNews, SourceCheck, NewsShare # 1-5 Never-Multiple times a day (ordinal)
PublicReact # 1 = much too extreme 3 = appropriate 5 = not sufficient (ordinal?)
CovidLength # 1 = less than 1 month 5 = 10-12 months 8 more than 5 years (ordinal but not equally spaced)
Trust_1:Trust_4 # 1 = strong distrust 5 = strong trust
morality_1:morality_6 # 1 = fully disagree 5 = fully agree


# categorical
Gender # 
# CountryBorn (categorical)
Diagnosis # 1 = yes 2 = think so unconfirmed 3 = no (categorical)
Symptoms # 1 = none 2 = mild to moderate 3 = critical or serious
Diagnosis_other # 1 = yes 2 = think so unconfirmed 3 = no (categorical)
Symptoms_other # 1 = none 2 = mild to moderate 3 = critical or serious 4 = death
SocialMedia_1:SocialMedia_8_TEXT # (dont bother)
Workessential # 1 = yes 2 = no 3 = unsure
Wherework # 1 = organisation 2 = self employed 3 = none
# if selected none to wherework:
LossofJob  # 1 = laid off due to covid 2 = closed business due to covid 3 = none of above
LossofJobOrg # if laid off from org 1 = temp 2 = permanent 3 = unsure
LossofJobBiz # if laid off from self employed 1 = temp 2 = permanent 3 = unsure
Layoff # at risk of being laid off? 1 = yes 2 = no
MaritalStatus # 1 = married 2 = de facto 3 = separated 4 = single
Children # 1 = yes 2 = no
Homeschool # 1 = yes 2 = no
Elderly # +65 relatives? 1 = yes 2 = no
ElderlyLive # live with elderly? 1 = yes 2 = no
Job # 1 = yes 2 = no








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





