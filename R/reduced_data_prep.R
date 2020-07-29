# This script prepares the data for LPA using the reduced dataset

library(tidyverse)

# load data set
# missing values were assigned -99 in SPSS
d <- read_csv("data/COVID19_all_samples_after_cleaning.csv", guess_max = 1700)

# create dataset of only Aus, Can, US, and UK participants
# country var values
# 1 == Aus
# 2 == Can
# 3 == UK
# 4 == US
# make CountryLive and sample factors with labels
d <- d %>% 
  filter(CountryLive %in% c(9, 31, 185, 187)) %>% 
  mutate(CountryLive = factor(CountryLive, levels = c(9, 31, 185, 187), labels = c("Australia", "Canada", "UK", "US")),
         sample = factor(sample, levels = c(1,2,3), labels = c("prolific", "sona", "public")),
         Gender = factor(Gender, levels = 1:3, labels = c("male", "female", "other")),
         Education = factor(Education, levels = 1:8, labels = c("none", "primary_school", "high_school",
                                                                "vocational_certificate", "associate_degree",
                                                                "bachelor_degree", "masters_degree", "doctorate_degree")))

# make N equivalent across country groups (Aus will total 370)
# select random sample of 168 Aus SONA participants
set.seed(23)
x <- d %>% filter(CountryLive == "Australia" & sample == "sona")
x <- d %>% filter(id %in% sample(x$id, 168))

# remove aus SONA participants from dataset
remove <- d %>% filter(CountryLive == "Australia", sample == "sona")
d <- d %>% filter(!id %in% remove$id)

# append randomly selected Aus SONA participants
d <- bind_rows(d, x)

# clean environment
# rm(list = setdiff(ls(), c("d")))

