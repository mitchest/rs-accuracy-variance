# get samples -------------------------------------------------------------

ecologist_sample <- function(data, samp_frac) {
  sample_out <- data %>%
    group_by(veg_cl_tm, studyarea_) %>%
    sample_frac(samp_frac)
  as.data.frame(sample_out)
}

# type: 1 = random, 2 = random strat by veg, 3 = random strat by veg & space, 4 = spatial block hold-out
rrcv_get_train <- function(data, train_frac, type) {
  if (type == 1) {
    train_ids <- data %>%
      select(id) %>%
      sample_frac(train_frac)
  } else if (type == 2) {
    train_ids <- data %>%
      select(id, veg_cl_tm) %>%
      group_by(veg_cl_tm) %>%
      sample_frac(train_frac)
  } else if (type == 3) {
    train_ids <- data %>%
      select(id, veg_cl_tm, studyarea_) %>%
      group_by(veg_cl_tm, studyarea_) %>%
      sample_frac(train_frac)
  } else if (type == 4) {
    blocks <- sample(unique(data$studyarea_), length(unique(data$studyarea_)) * train_frac)
    train_ids <- data %>%
      select(id, studyarea_) %>%
      filter(studyarea_ %in% blocks)
  }
  as.integer(train_ids$id)
}

# type: 1 = random, 2 = random strat by veg, 3 = random strat by veg & space, 4 = spatial block hold-out
kfold_get_train <- function(data, kfold_k, type) {
  if (type == 1) {
    train_ids <- data %>%
      select(id) %>%
      mutate(fold = sample(rep(sample(kfold_k), length.out = n())))
  } else if (type == 2) {
    train_ids <- data %>%
      select(id, veg_cl_tm) %>%
      group_by(veg_cl_tm) %>%
      mutate(fold = sample(rep(sample(kfold_k), length.out = n())))
  } else if (type == 3) {
    train_ids <- data %>%
      select(id, veg_cl_tm, studyarea_) %>%
      group_by(veg_cl_tm, studyarea_) %>%
      mutate(fold = sample(rep(1:kfold_k, length.out = n()))) #odd.....
  } else if (type == 4) {
    train_ids <- data %>%
      select(id, studyarea_)
    blocks <- unique(data$studyarea_)
    names(blocks) <- sample(rep(sample(kfold_k), length.out = length(blocks)))
    train_ids$fold <- names(blocks)[data$studyarea_]
  }
  get_trains_from_kfolds(train_ids)
}

get_trains_from_kfolds <- function(data_with_folds) {
  folds <- unique(data_with_folds$fold)
  lapply(X = 1:max(folds), FUN = function(x) {data_with_folds$id[data_with_folds$fold != x]})
}

get_test_from_trains <- function(train_ids, full_ids) {
  full_ids[!full_ids %in% train_ids]
}

get_test_from_trains_list <- function(train_ids_list, full_ids) {
  lapply(X = train_ids_list, FUN = function(x){full_ids[!full_ids %in% x]})
}



# allocations (model fitting) ---------------------------------------------

get_lda_allocation <- function(x, data, train_list, test_list, all_data) {
  train <- train_list[[x]]
  test <- test_list[[x]]
  fm <- lda(veg_cl_tm ~ blue_mean + green_mean + red_mean + nir_mean, # move to character argvar input
            data = inner_join(data, data.frame(id=train), by="id"),
            prior = rep(1/length(unique(data$veg_cl_tm)), length(unique(data$veg_cl_tm))))
  train_preds <- predict(fm)$class
  test_preds <- predict(fm, newdata = inner_join(data, data.frame(id=test), by="id"))$class
  true_preds <- predict(fm, newdata = all_data)$class
  list(train_preds, test_preds, true_preds)
}

get_knn_allocation <- function(x, data, train_list, test_list, bands, all_data) {
  train <- train_list[[x]]
  test <- test_list[[x]]
  train_dat <- inner_join(data, data.frame(id=train), by="id")
  test_dat <- inner_join(data, data.frame(id=test), by="id")
  test_preds <- knn1(train = train_dat[,bands], test = test_dat[,bands], cl = train_dat$veg_cl_tm)
  true_preds <- knn1(train = train_dat[,bands], test = all_data[,bands], cl = train_dat$veg_cl_tm)
  list(test_preds, true_preds)
}

get_rf_allocation <- function(x, data, train_list, test_list, all_data) {
  train <- train_list[[x]]
  test <- test_list[[x]]
  fm <- ranger(veg_cl_tm ~ blue_mean + green_mean + red_mean + nir_mean, # move to character argvar input
            data = inner_join(data, data.frame(id=train), by="id"), num.trees = 250, mtry = 2)
  train_preds <- fm$predictions
  test_preds <- predict(fm, data = inner_join(data, data.frame(id=test), by="id"))$predictions
  true_preds <- predict(fm, data = all_data)$predictions
  list(train_preds, test_preds, true_preds)
}

boot_allocations <- function(nboot, data, bands, all_data) {
  boot_method <- list()
  boot_method[["train"]] <- replicate(n = nboot, expr = {sample(data$id, replace = T)}, simplify = F)
  boot_method[["test"]] <- lapply(X = boot_method[["train"]], FUN = get_test_from_trains, data$id)
  # mle classifications
  boot_lda <- lapply(X = 1:nboot, FUN = get_lda_allocation,
                     data, boot_method[["train"]], boot_method[["test"]], all_data)
  boot_method[["train_lda"]] <- lapply(boot_lda, `[[`, 1)
  boot_method[["test_lda"]] <- lapply(boot_lda, `[[`, 2)
  boot_method[["true_lda"]] <- lapply(boot_lda, `[[`, 3)
  # knn classifications
  boot_knn <- lapply(X = 1:nboot, FUN = get_knn_allocation,
                     data, boot_method[["train"]], boot_method[["test"]], bands, all_data)
  boot_method[["test_knn"]] <- lapply(boot_knn, `[[`, 1)
  boot_method[["true_knn"]] <- lapply(boot_knn, `[[`, 2)
  # rf classifications
  boot_rf <- lapply(X = 1:nboot, FUN = get_rf_allocation,
                    data, boot_method[["train"]], boot_method[["test"]], all_data)
  boot_method[["train_rf"]] <- lapply(boot_rf, `[[`, 1)
  boot_method[["test_rf"]] <- lapply(boot_rf, `[[`, 2)
  boot_method[["true_rf"]] <- lapply(boot_rf, `[[`, 3)
  
  boot_method
}

rrcv_allocations <- function(x, rrcv_params, rrcv_times, data, bands, all_data) {
  train_frac <- rrcv_params$frac[x]
  type <- rrcv_params$type[x]
  
  rrcv_method <- list()
  rrcv_method[["train"]] <- replicate(n = rrcv_times, expr = {rrcv_get_train(data, train_frac, type)}, simplify = F)
  rrcv_method[["test"]] <- lapply(X = rrcv_method[["train"]], FUN = get_test_from_trains, data$id)
  # mle classifications
  rrcv_lda <- lapply(X = 1:rrcv_times, FUN = get_lda_allocation,
                     data, rrcv_method[["train"]], rrcv_method[["test"]], all_data)
  rrcv_method[["train_lda"]] <- lapply(rrcv_lda, `[[`, 1)
  rrcv_method[["test_lda"]] <- lapply(rrcv_lda, `[[`, 2)
  rrcv_method[["true_lda"]] <- lapply(rrcv_lda, `[[`, 3)
  # knn classifications
  rrcv_knn <- lapply(X = 1:rrcv_times, FUN = get_knn_allocation,
                                      data, rrcv_method[["train"]], rrcv_method[["test"]], bands, all_data)
  rrcv_method[["test_knn"]] <- lapply(rrcv_knn, `[[`, 1)
  rrcv_method[["true_knn"]] <- lapply(rrcv_knn, `[[`, 2)
  # rf classifications
  rrcv_rf <- lapply(X = 1:rrcv_times, FUN = get_rf_allocation,
                     data, rrcv_method[["train"]], rrcv_method[["test"]], all_data)
  rrcv_method[["train_rf"]] <- lapply(rrcv_rf, `[[`, 1)
  rrcv_method[["test_rf"]] <- lapply(rrcv_rf, `[[`, 2)
  rrcv_method[["true_rf"]] <- lapply(rrcv_rf, `[[`, 3)
  
  rrcv_method
}

kfold_allocations <- function(x, kfold_params, kfold_times, data, bands, all_data) {
  kfold_k <- kfold_params$kfold_k[x]
  type <- kfold_params$type[x]
  
  kfold_method <- list()
  kfold_method[["train"]] <- replicate(n = kfold_times, expr = {kfold_get_train(data, kfold_k, type)}, simplify = T)
  kfold_method[["test"]] <- lapply(kfold_method[["train"]], FUN = get_test_from_trains, data$id)
  # mle classifications
  kfold_lda <- lapply(X = 1:(kfold_times*kfold_k), FUN = get_lda_allocation,
                      data, kfold_method[["train"]], kfold_method[["test"]], all_data)
  kfold_method[["train_lda"]] <- lapply(kfold_lda, `[[`, 1)
  kfold_method[["test_lda"]] <- lapply(kfold_lda, `[[`, 2)
  kfold_method[["true_lda"]] <- lapply(kfold_lda, `[[`, 3)
  # knn classifications
  kfold_knn <- lapply(X = 1:(kfold_times*kfold_k), FUN = get_knn_allocation,
                                       data, kfold_method[["train"]], kfold_method[["test"]], bands, all_data)
  kfold_method[["test_knn"]] <- lapply(kfold_knn, `[[`, 1)
  kfold_method[["true_knn"]] <- lapply(kfold_knn, `[[`, 2)
  # rf classifications
  kfold_rf <- lapply(X = 1:(kfold_times*kfold_k), FUN = get_rf_allocation,
                      data, kfold_method[["train"]], kfold_method[["test"]], all_data)
  kfold_method[["train_rf"]] <- lapply(kfold_rf, `[[`, 1)
  kfold_method[["test_rf"]] <- lapply(kfold_rf, `[[`, 2)
  kfold_method[["true_rf"]] <- lapply(kfold_rf, `[[`, 3)
  
  kfold_method
}