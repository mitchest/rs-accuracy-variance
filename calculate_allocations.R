library(MASS)
library(class)
library(ranger)
library(dplyr)
library(tidyr)

source("allocation_functions.R")


# load data ---------------------------------------------------------------

survey_points_raw <- read.csv("classification_data/dharawal_points_ads40.csv",
                          header = T, stringsAsFactors = F)



# data prep ---------------------------------------------------------------
survey_points <- survey_points_raw %>%
  filter(veg_cl_tm %in% c("bt", "ew", "ttt", "wh")) %>%
  select(studyarea_, veg_cl_tm, blue_mean:nir_mean) %>%
  mutate(veg_cl_tm = as.factor(veg_cl_tm),
         id = 1:nrow(.)) %>%
  na.omit()

rm(survey_points_raw)



# parameterise analysis ---------------------------------------------------

orig_sample_iter <- 50 # number of times to sample the original data
orig_sample_frac <- 0.1
nboot <- 800 # number of bootstrap samples
rrcv_times <- 800 # number of times to do random repeat CV
kfold_times <- 160 # number of times to repeat the k-fold CV
#kfold_k <- 5 # k for k-fold
bands <- c("blue_mean", "green_mean", "red_mean", "nir_mean")

rrcv_params <- data.frame(frac = rep(c(0.67,0.8), each = 4),
                          type = rep(1:4, 2),
                          name = c(paste0("train67type", 1:4),paste0("train80type", 1:4)))

kfold_params <- data.frame(kfold_k = rep(5, 4),
                          type = 1:4,
                          name = paste0("k", rep(5,4), "type", c(1:4)))

# run models and allocate samples -----------------------------------------


# run overall sampling as a for loop - allows cluster implementation more easily

big_list <- list()

for (n_iter in 1:orig_sample_iter) {
  # progress
  print(paste0("Iteration ", n_iter, " out of ", orig_sample_iter))
  print(Sys.time())
  if (n_iter == 1) {start_time <- Sys.time()}
  if (n_iter > 1) {print(paste0("Finish ~ ", start_time + (((Sys.time() - start_time) / (n_iter-1)) * orig_sample_iter)))}
  
  # sample data
  sample_data <- ecologist_sample(survey_points, orig_sample_frac)
  
  # bootstrapping
  boot_list <- list()
  boot_list[["train"]] <- replicate(n = nboot, expr = {sample(sample_data$id, replace = T)}, simplify = F)
  boot_list[["test"]] <- replicate(n = nboot, expr = {sample(sample_data$id, replace = T)}, simplify = F)
  # boot mle classifications
  boot_lda <- lapply(X = 1:nboot, FUN = get_lda_allocation,
                        sample_data, boot_list[["train"]], boot_list[["test"]], survey_points)
  boot_list[["train_lda"]] <- lapply(boot_lda, `[[`, 1)
  boot_list[["test_lda"]] <- lapply(boot_lda, `[[`, 2)
  boot_list[["true_lda"]] <- lapply(boot_lda, `[[`, 3)
  # boot knn classificaitons
  boot_knn <- lapply(X = 1:nboot, FUN = get_knn_allocation,
                                    sample_data, boot_list[["train"]], boot_list[["test"]], bands, survey_points)
  boot_list[["test_knn"]] <- lapply(boot_knn, `[[`, 1)
  boot_list[["true_knn"]] <- lapply(boot_knn, `[[`, 2)
  # boot rf classifications
  boot_rf <- lapply(X = 1:nboot, FUN = get_rf_allocation,
                     sample_data, boot_list[["train"]], boot_list[["test"]], survey_points)
  boot_list[["train_rf"]] <- lapply(boot_rf, `[[`, 1)
  boot_list[["test_rf"]] <- lapply(boot_rf, `[[`, 2)
  boot_list[["true_rf"]] <- lapply(boot_rf, `[[`, 3)
  
  # get rrcv allocations for each parameterisation
  rrcv_list <- lapply(X = 1:nrow(rrcv_params), FUN = rrcv_allocations,
                      rrcv_params, rrcv_times, sample_data, bands, survey_points)
  names(rrcv_list) <- rrcv_params$name
  attr(rrcv_list, which = "rrcv_params") <- rrcv_params
  
  # get kfold allocations for each parameterisation
  kfold_list <- lapply(X = 1:nrow(kfold_params), FUN = kfold_allocations,
                       kfold_params, kfold_times, sample_data, bands, survey_points)
  names(kfold_list) <- kfold_params$name
  attr(kfold_list, which = "kfold_params") <- kfold_params
  
  # get allocations from trinaing on all data
  use_all_list <- list()
  fm <- lda(veg_cl_tm ~ blue_mean + green_mean + red_mean + nir_mean, data = sample_data)
  use_all_list[["all_lda"]] <- predict(fm, newdata = survey_points)$class
  use_all_list[["all_knn"]] <- knn1(train = sample_data[,bands], test = survey_points[,bands], 
                                    cl = sample_data$veg_cl_tm)
  fm <- ranger(veg_cl_tm ~ blue_mean + green_mean + red_mean + nir_mean,
               data = sample_data, num.trees = 250, mtry = 4)
  use_all_list[["all_rf"]] <- predict(fm, data = survey_points)$predictions
  
  # put into one iterations slot
  big_list[[n_iter]] <- list(boot = boot_list,
                             rrcv = rrcv_list,
                             kfold = kfold_list,
                             alldat = use_all_list)
}

save(big_list, file = "A:/1_UNSW/0_data/Dharawal_project/big_list.RData")


# unfinished nonsense -----------------------------------------------------












