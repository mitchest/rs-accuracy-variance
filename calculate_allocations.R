library(MASS)
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



# parameterise analysis ---------------------------------------------------

orig_sample_iter <- 300 # number of times to sample the original data
orig_sample_frac <- 0.1
nboot <- 800 # number of bootstrap samples
rrcv_times <- 800 # number of times to do random repeat CV
kfold_times <- 100 # number of times to repeat the k-fold CV
#kfold_k <- 5 # k for k-fold

rrcv_params <- data.frame(frac = rep(c(0.67,0.8), each = 3),
                          type = rep(1:3, 2),
                          name = c(paste0("train67type", 1:3),paste0("train80type", 1:3)))

kfold_params <- data.frame(kfold_k = rep(5, 3),
                          type = 1:3,
                          name = paste0("k", rep(5,3), "type", c(1:3)))

# run models and allocate samples -----------------------------------------


# run overall sampling as a for loop - allows cluster implementation more easily

big_list <- list()

for (n_iter in 1:orig_sample_iter) {
  
  # progress
  print(paste0("Iteration ", n_iter, " out of ", orig_sample_iter))
  print(Sys.time())
  
  # sample data
  sample_data <- ecologist_sample(survey_points, orig_sample_frac)
  
  # bootstrapping
  boot_list <- list()
  boot_list[["train"]] <- replicate(n = nboot, expr = {sample(sample_data$id)}, simplify = F)
  boot_list[["test"]] <- replicate(n = nboot, expr = {sample(sample_data$id)}, simplify = F)
  boot_lda <- lapply(X = 1:nboot, FUN = get_lda_allocation,
                        sample_data, boot_list[["train"]], boot_list[["test"]])
  boot_list[["train_lda"]] <- lapply(boot_lda, `[[`, 1)
  boot_list[["test_lda"]] <- lapply(boot_lda, `[[`, 2)
  ####->>>> add more models
  
  # get rrcv allocations for each parameterisation
  rrcv_list <- lapply(X = 1:nrow(rrcv_params), FUN = rrcv_allocations,
                      rrcv_params, rrcv_times, sample_data)
  names(rrcv_list) <- rrcv_params$name
  attr(rrcv_list, which = "rrcv_params") <- rrcv_params
  
  # get kfold allocations for each parameterisation
  kfold_list <- lapply(X = 1:nrow(kfold_params), FUN = kfold_allocations,
                       kfold_params, kfold_times, sample_data)
  names(kfold_list) <- kfold_params$name
  attr(kfold_list, which = "kfold_params") <- kfold_params
  
  big_list[[n_iter]] <- list(boot = boot_list,
                             rrcv = rrcv_list,
                             kfold = kfold_list)
}

save(big_list, file = "big_list.RData")


# unfinished nonsense -----------------------------------------------------












