# this script replicated Sabina's EFAs conducted on the reduced covid-19 dataset (N=1391)

# load packages
library(tidyverse)
library(psych)
library(knitr)
library(GGally) # plot distributions
library(naniar) # visualise missing data
library(tidyLPA) # implement LPA

# load data
source("R/data_prep.R")

# load correlations with sig. stars function
source("R/corr_matrix_sig.R")
