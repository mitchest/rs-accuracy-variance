library(parallel)
library(MASS)
library(class)
library(ranger)
library(dplyr)
library(tidyr)

source("allocation_functions.R")


# load data ---------------------------------------------------------------

survey_points_raw <- read.csv("classification_data/observer_class_ads40.csv",
                          header = T, stringsAsFactors = F)



# data prep ---------------------------------------------------------------
survey_points <- survey_points_raw %>%
  filter(veg_class %in% c("bt", "ew", "ttt", "wh")) %>%
  select(study_area, veg_class, blue_mean:nir_mean) %>%
  mutate(veg_class = as.factor(veg_class),
         id = 1:nrow(.)) %>%
  na.omit()

rm(survey_points_raw)



# parameterise analysis ---------------------------------------------------

# sort out whether we're doing full image predictions or main analysis
array_job <- ifelse("image" %in% commandArgs(), FALSE, TRUE)

if (array_job) {
  # get job number from array job env. var.
  job <- as.numeric(Sys.getenv("PBS_ARRAYID"))
}

# parameterise resampling analysis
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


# run overall sampling - in this form it allows cluster implementation more easily (if required),
# but it could be run as a for loop, e.g.
# for (n_iter in 1:50) {
  # print(paste0("Iteration ", n_iter, " out of ", 50))
  # print(Sys.time())
  # if (n_iter == 2) {start_time <- Sys.time()}
  # if (n_iter > 2) {print(paste0("Finish ~ ", start_time + (((Sys.time() - start_time) / (n_iter-2)) * (orig_sample_iter-1))))}

#################################################
## if you're not on a cluster, just set n_iter ##
## n_iter = 1, will do image proportion preds  ##
## n_iter > 1 will do the standard analysis    ##
#################################################

# sort out paralellisation style
if (array_job) {
  n_iter <- job + 1
  image_data <- NULL
} else {
  n_iter <- 1
  image_data <- readRDS("classification_data/image_data.rds") # pre-made df with image data (quickes/easiest option here) - it is BIG, literally a data frame with squillions of rows (i.e. pixels)
}

# sample data
sample_data <- ecologist_sample(survey_points, orig_sample_frac)

# get bootstrap allocations for each parameterisation
print(paste0("Bootstrap (",memory.size(),"Mb)"))
boot_list <- boot_allocations(nboot, sample_data, bands, survey_points, image_data, n_iter)

# NOTE ######################################################################################
# This code block samples BOTH training AND test subsets using bootstrapping with replacement
#############################################################################################
# boot_list <- list()
# boot_list[["train"]] <- replicate(n = nboot, expr = {sample(sample_data$id, replace = T)}, simplify = F)
# boot_list[["test"]] <- replicate(n = nboot, expr = {sample(sample_data$id, replace = T)}, simplify = F)
# # boot mle classifications
# boot_lda <- lapply(X = 1:nboot, FUN = get_lda_allocation,
#                       sample_data, boot_list[["train"]], boot_list[["test"]], survey_points)
# boot_list[["train_lda"]] <- lapply(boot_lda, `[[`, 1)
# boot_list[["test_lda"]] <- lapply(boot_lda, `[[`, 2)
# boot_list[["true_lda"]] <- lapply(boot_lda, `[[`, 3)
# # boot knn classificaitons
# boot_knn <- lapply(X = 1:nboot, FUN = get_knn_allocation,
#                                   sample_data, boot_list[["train"]], boot_list[["test"]], bands, survey_points)
# boot_list[["test_knn"]] <- lapply(boot_knn, `[[`, 1)
# boot_list[["true_knn"]] <- lapply(boot_knn, `[[`, 2)
# # boot rf classifications
# boot_rf <- lapply(X = 1:nboot, FUN = get_rf_allocation,
#                    sample_data, boot_list[["train"]], boot_list[["test"]], survey_points)
# boot_list[["train_rf"]] <- lapply(boot_rf, `[[`, 1)
# boot_list[["test_rf"]] <- lapply(boot_rf, `[[`, 2)
# boot_list[["true_rf"]] <- lapply(boot_rf, `[[`, 3)
#############################################################################################

# get rrcv allocations for each parameterisation
print(paste0("RRCV (",memory.size(),"Mb)"))
rrcv_list <- lapply(X = 1:nrow(rrcv_params), FUN = rrcv_allocations,
                    rrcv_params, rrcv_times, sample_data, bands, survey_points, image_data, n_iter)
names(rrcv_list) <- rrcv_params$name
attr(rrcv_list, which = "rrcv_params") <- rrcv_params

# get kfold allocations for each parameterisation
print(paste0("k-fold (",memory.size(),"Mb)"))
kfold_list <- lapply(X = 1:nrow(kfold_params), FUN = kfold_allocations,
                     kfold_params, kfold_times, sample_data, bands, survey_points, image_data, n_iter)
names(kfold_list) <- kfold_params$name
attr(kfold_list, which = "kfold_params") <- kfold_params

# get allocations from training on all data
# use_all_list <- list()
# fm <- lda(veg_class ~ blue_mean + green_mean + red_mean + nir_mean, data = sample_data)
# use_all_list[["all_lda"]] <- predict(fm, newdata = survey_points)$class
# use_all_list[["all_knn"]] <- knn1(train = sample_data[,bands], test = survey_points[,bands],
#                                   cl = sample_data$veg_class)
# fm <- ranger(veg_class ~ blue_mean + green_mean + red_mean + nir_mean,
#              data = sample_data, num.trees = 250, mtry = 4)
# use_all_list[["all_rf"]] <- predict(fm, data = survey_points)$predictions

# put into one iterations slot
out_list <- list(boot = boot_list,
                           rrcv = rrcv_list,
                           kfold = kfold_list)

saveRDS(out_list, file = paste0("n_iter_",n_iter,".rds"))



# additional runs for 'all data' ------------------------------------------

# NOTE ########################################################################################
# This code calculates accuracy for models fit to full sub-sample - not used in the publication
# Also note the corresponding commented code in caclulate_accuracy.R
###############################################################################################

# rm(big_list)
# 
# orig_sample_iter <- 800 # number of times to sample the original data
# orig_sample_frac <- 0.1
# 
# bands <- c("blue_mean", "green_mean", "red_mean", "nir_mean")
# 
# big_list_alldat <- list()
# 
# for (n_iter in 1:orig_sample_iter) {
#   # progress
#   print(paste0("Iteration ", n_iter, " out of ", orig_sample_iter))
#   print(Sys.time())
#   if (n_iter == 1) {start_time <- Sys.time()}
#   if (n_iter > 1) {print(paste0("Finish ~ ", start_time + (((Sys.time() - start_time) / (n_iter-1)) * orig_sample_iter)))}
#   
#   # sample data
#   sample_data <- ecologist_sample(survey_points, orig_sample_frac)
#   oob_ids <- survey_points$id[-sample_data$id]
#   
#   # get allocations from training on all data
#   use_all_list <- list()
#   fm <- lda(veg_class ~ blue_mean + green_mean + red_mean + nir_mean, data = sample_data)
#   use_all_list[["all_lda"]] <- predict(fm, newdata = survey_points[oob_ids,])$class
#   use_all_list[["all_knn"]] <- knn1(train = sample_data[,bands], test = survey_points[oob_ids,bands], 
#                                     cl = sample_data$veg_class)
#   fm <- ranger(veg_class ~ blue_mean + green_mean + red_mean + nir_mean,
#                data = sample_data, num.trees = 250, mtry = 4)
#   use_all_list[["all_rf"]] <- predict(fm, data = survey_points[oob_ids,])$predictions
#   
#   # put into one iterations slot
#   big_list_alldat[[n_iter]] <- list(oob_ids = oob_ids,
#                                     alldat = use_all_list)
# }
# 
# saveRDS(big_list_alldat, file = "A:/1_UNSW/0_data/Dharawal_project/big_list_alldat.rds")

# unfinished nonsense -----------------------------------------------------












