
# Example 1 ---------------------------------------------------------------
# Brief examples:
# https://www.jmichaelrosenberg.com/blog/introducing-tidylpa-an-r-package-for-carrying-out-latent-profile-analysis/

# More detailed example (better):
# https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html

library(tidyverse)
library(tidyLPA)

# install the development version
# devtools::install_github("data-edu/tidyLPA")

# Here is a brief example using the built-in pisaUSA15 data set and variables for broad interest, enjoyment, and self-efficacy. 
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(3)

# We can plot the profiles by piping the output to plot_profiles().
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(3) %>% 
  plot_profiles()


# The model can be specified in terms of whether and how the variable variances and covariances are estimated.
# The models are specified by passing arguments to the variance and covariance arguments. 
# The possible values for these arguments are:
# variances: “equal” and “zero”
# covariances: “varying”, “equal”, and “zero”
# If no values are specified for these, then the variances are constrained to be equal across classes, 
# and covariances are fixed to 0 (conditional independence of the indicators).
# These arguments allow for four models to be specified:
#     Equal variances and covariances fixed to 0 (Model 1)
#     Varying variances and covariances fixed to 0 (Model 2)
#     Equal variances and equal covariances (Model 3)
#     Varying variances and varying covariances (Model 6)

# Two additional models (Models 4 and 5) can be fit using MPlus. More information on the models can be found in the vignette: 
# https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html

# Here is an example of specifying a model with varying variances and covariances (Model 6; not run here):
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(3, 
                    variances = "varying",
                    covariances = "varying")

# The function compare_solutions() compares the fit of several estimated models, with varying numbers of profiles 
# and model specifications:
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(1:3, 
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))




# Example 2 ---------------------------------------------------------------
# https://www.r-bloggers.com/quick-example-of-latent-profile-analysis-in-r/

library(tidyverse)
library(careless)
library(psych)
library(mclust)

survey <- read_csv("https://raw.githubusercontent.com/whipson/tidytuesday/master/young_people.csv") %>%
  select(History:Pets)

# calculate the longest string of identical responses for each participant
# and outliers using Mahalanobis Distance (md) info: https://willhipson.netlify.app/post/outliers/outliers/
interests <- survey %>%
  mutate(string = longstring(.)) %>%
  mutate(md = outlier(., plot = FALSE))

# remove all P with a string > 10 or md > .001
cutoff <- (qchisq(p = 1 - .001, df = ncol(interests)))

interests_clean <- interests %>%
  filter(string <= 10,
         md < cutoff) %>%
  select(-string, -md)

# remove NAs and std vars
interests_clustering <- interests_clean %>%
  na.omit() %>%
  mutate_all(list(scale))

BIC <- mclustBIC(interests_clustering)

# plot Bayesian Information Criteria for all the models with profiles ranging from 1 to 9
plot(BIC)

# show the top three models based on BIC
summary(BIC)

# The highest BIC comes from VVE, 3. This says there are 3 clusters with variable volume, 
# variable shape, equal orientation, and ellipsodial distribution. 
# However, VEE, 3 is not far behind and actually may be a more theoretically useful model 
# since it constrains the shape of the distribution to be equal. For this reason, we’ll go 
# with VEE, 3.

# If we want to look at this model more closely, we save it as an object and inspect it with summary().
mod1 <- Mclust(interests_clustering, modelNames = "VEE", G = 3, x = BIC)
# mod2 <- Mclust(interests_clustering, modelNames = "VVE", G = 3, x = BIC)

# The summary output describes the geometric characteristics of the profiles and the number of cases 
# classified into each of the three clusters.
summary(mod1)
# summary(mod2)


# BIC is one of the best fit indices, but it’s always recommended to look for more evidence that the 
# solution we’ve chosen is the correct one. We can also compare values of the Integrated Completed 
# Likelikood (ICL) criterion. ICL isn’t much different from BIC, except that it adds a penalty on 
# solutions with greater entropy or classification uncertainty.
ICL <- mclustICL(interests_clustering)

plot(ICL)

# We see similar results. ICL suggests that model VEE, 3 fits quite well. 
summary(ICL)

# Finally, we’ll perform the Bootstrap Likelihood Ratio Test (BLRT) which compares model fit between k-1 
# and k cluster models. In other words, it looks to see if an increase in profiles increases fit. Based on 
# simulations by Nylund, Asparouhov, and Muthén (2007) BIC and BLRT are the best indicators for how many 
# profiles there are. This line of code will take a long time to run, so if you’re just following along I 
# suggest skipping it unless you want to step out for a coffee break.

mclustBootstrapLRT(interests_clustering, modelName = "VEE")

# BLRT also suggests that a 3-profile solution is ideal.

# Now that we’re confident in our choice of a 3-profile solution, let’s plot the results. Specifically, 
# we want to see how the profiles differ on the indicators, that is, the items that made up the profiles. 
# If the solution is theoretically meaningful, we should see differences that make sense.

library(reshape2)

means <- data.frame(mod1$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Interest = rowname) %>%
  melt(id.vars = "Interest", variable.name = "Profile", value.name = "Mean") %>%
  mutate(Mean = round(Mean, 2),
         Mean = ifelse(Mean > 1, 1, Mean))


p <- means %>%
  mutate(Profile = recode(Profile, 
                          X1 = "Science: 16%",
                          X2 = "Disinterest: 60%",
                          X3 = "Arts & Humanities: 24%")) %>%
  ggplot(aes(Interest, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = c("Active sport", "Adrenaline sports", "Passive sport",
                              "Countryside, outdoors", "Gardening", "Cars",
                              "Art exhibitions", "Dancing", "Musical instruments", "Theatre", "Writing", "Reading",
                              "Geography", "History", "Law", "Politics", "Psychology", "Religion", "Foreign languages",
                              "Biology", "Chemistry", "Mathematics", "Medicine", "Physics", "Science and technology",
                              "Internet", "PC",
                              "Celebrities", "Economy Management", "Fun with friends", "Shopping", "Pets")) +
  labs(x = NULL, y = "Standardized mean interest") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

library(plotly)

ggplotly(p, tooltip = c("Interest", "Mean")) %>%
  layout(legend = list(orientation = "h", y = 1.2))
