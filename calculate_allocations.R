library(dplyr)
library(tidyr)

source("allocation_functions.R")


# load data ---------------------------------------------------------------

survey_points_raw <- read.csv("classification_data/dharawal_pointsTM_covars.csv",
                          header = T, stringsAsFactors = F)



# data prep ---------------------------------------------------------------
survey_points <- survey_points_raw %>%
  filter(veg_cl_tm %in% c("bt", "ew", "ttt", "wh")) %>%
  select(studyarea_, veg_cl_tm, blue_mean:nir_sd) %>%
  mutate(veg_cl_tm = as.factor(veg_cl_tm),
         id = 1:nrow(.)) %>%
  na.omit()



# run models and allocate samples -----------------------------------------


# run overall sampling as a for loop - allows cluster implementation more easily

orig_sample_iter <- 2 # number of times to sample the original data
orig_sample_frac <- 0.1
nboot <- 10 # number of bootstrap samples
rrcv_times <- 10 # number of times to do random repeat CV
kfold_times <- 1 # number of times to repeat the k-fold CV
kfold_k <- 5 # k for k-fold


for (n_iter in 1:orig_sample_iter) {
  
  sample_data <- ecologist_sample(survey_points, orig_sample_frac)
  
  # bootstrap allocations
  boot_train <- replicate(n = nboot, expr = {sample(sample_data$id)})
  boot_test <- replicate(n = nboot, expr = {sample(sample_data$id)})
  
  # rrcv sample
  
  
}







