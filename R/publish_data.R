# calculate dataset to publish with manuscript

# load packages
library(tidyverse)

# load data
d <- read_csv("data/200805_covid_full_imputed_std_data.csv", guess_max = 1575) %>% 
  mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
         sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public")))

# rename and reverse code ReasonsLeaveHome variables (higher score = higher frequency of leaving home)
d <- d %>%
  pivot_longer(ReasonsLeaveHome_1:ReasonsLeaveHome_11, names_to = "var", values_to = "val") %>% 
  mutate(val = 6 - val) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  rename(leave_going_to_work = ReasonsLeaveHome_1, leave_walking_pet = ReasonsLeaveHome_2,
         leave_physical_activity = ReasonsLeaveHome_3, leave_get_food = ReasonsLeaveHome_4, 
         leave_pharmacy = ReasonsLeaveHome_5, leave_medical_treatment = ReasonsLeaveHome_6,
         leave_care_dependents = ReasonsLeaveHome_7, leave_meet_friends_family = ReasonsLeaveHome_8, 
         leave_religion = ReasonsLeaveHome_9, leave_bored = ReasonsLeaveHome_10, 
         leave_exercise_freedom = ReasonsLeaveHome_11)

# clean 2 unrealistic values for HouseholdNumber (>10)
# reverse code vars so higher scores = higher quantity
d <- d %>% 
  mutate(HouseholdNumber = ifelse(HouseholdNumber > 10, NA, HouseholdNumber),
         LeaveHome = 2 - LeaveHome, # 0 = will not leave home, 1 = will leave home
         PublicReact = 6 - PublicReact)  # 1 = reaction insufficient, 5 = reaction too extreme

# calculate trait-accuracy and trait-confidence
trait <- d %>% 
  select(id, EAT_acc, CRT_acc, belief_acc, EAT_conf, CRT_conf, belief_conf) %>% 
  pivot_longer(names_to = "var", values_to = "val", -id) %>%
  group_by(id) %>% 
  mutate(na = sum(is.na(val))) %>% 
  filter(!na %in% 2:6) %>% 
  mutate(v = paste0("trait_", str_extract(var, "acc|conf"))) %>% 
  group_by(id, v) %>% 
  summarise(mean = mean(val, na.rm=T)) %>% 
  pivot_wider(names_from = v, values_from = mean)

d <- d %>% left_join(trait, by = "id")

d %>% write_csv("data/201027_covid_full_imputed_std_data.csv")
