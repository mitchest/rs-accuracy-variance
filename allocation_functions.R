# get samples -------------------------------------------------------------

ecologist_sample <- function(data, samp_frac) {
  sample_out <- data %>%
    group_by(veg_cl_tm, studyarea_) %>%
    sample_frac(samp_frac)
  as.data.frame(sample_out)
}

# type: 1 = random, 2 = random strat by veg, 3 = random strat by veg & space
rrcv_get_train <- function(data, train_frac, type) {
  if (type == 1) {
    train_ids <- sample(data$id, nrow(data) * train_frac)
  } else if (type == 2) {
    train_ids <- data %>%
      select(id, veg_cl_tm) %>%
      group_by(veg_cl_tm) %>%
      sample_frac(train_frac)
    train_ids <- as.integer(train_ids$id)
  } else if (type == 3) {
    train_ids <- data %>%
      select(id, veg_cl_tm, studyarea_) %>%
      group_by(veg_cl_tm, studyarea_) %>%
      sample_frac(train_frac)
    train_ids <- as.integer(train_ids$id)
  }
  train_ids
}

# type: 1 = random, 2 = random strat by veg, 3 = random strat by veg & space
kfold_get_train <- function(data, kfold_k, type) {
  if (type == 1) {
    train_ids <- data %>%
      select(id) %>%
      mutate(fold = rep(1:kfold_k, length.out = n()))
  } else if (type == 2) {
    train_ids <- data %>%
      select(id, veg_cl_tm) %>%
      group_by(veg_cl_tm) %>%
      mutate(fold = sample(rep(1:kfold_k, length.out = n())), replace = F)
  } else if (type == 3) {
    train_ids <- data %>%
      select(id, veg_cl_tm, studyarea_) %>%
      group_by(veg_cl_tm, studyarea_) %>%
      mutate(fold = sample(rep(1:kfold_k, length.out = n())), replace = F)
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

get_lda_allocation <- function(x, data, train_list, test_list) {
  train <- train_list[[x]]
  test <- test_list[[x]]
  fm <- lda(veg_cl_tm ~ blue_mean + green_mean + red_mean + nir_mean, data = data[data$id %in% train,])
  train_preds <- predict(fm)$class
  test_preds <- predict(fm, newdata = data[data$id %in% test,])$class
  list(train_preds, test_preds)
}

rrcv_allocations <- function(x, rrcv_params, rrcv_times, data) {
  train_frac <- rrcv_params$frac[x]
  type <- rrcv_params$type[x]
  
  rrcv_method <- list()
  rrcv_method[["train"]] <- replicate(n = rrcv_times, expr = {rrcv_get_train(data, train_frac, type)}, simplify = F)
  rrcv_method[["test"]] <- lapply(X = rrcv_method[["train"]], FUN = get_test_from_trains, data$id)
  rrcv_lda <- lapply(X = 1:rrcv_times, FUN = get_lda_allocation,
                     data, rrcv_method[["train"]], rrcv_method[["test"]])
  rrcv_method[["train_lda"]] <- lapply(rrcv_lda, `[[`, 1)
  rrcv_method[["test_lda"]] <- lapply(rrcv_lda, `[[`, 2)
  rrcv_method
}

kfold_allocations <- function(x, kfold_params, kfold_times, data) {
  kfold_k <- kfold_params$kfold_k[x]
  type <- kfold_params$type[x]
  
  kfold_method <- list()
  kfold_method[["train"]] <- replicate(n = kfold_times, expr = {kfold_get_train(data, kfold_k, type)}, simplify = T)
  kfold_method[["test"]] <- lapply(kfold_method[["train"]], FUN = get_test_from_trains, data$id)
  kfold_lda <- lapply(X = 1:(kfold_times*kfold_k), FUN = get_lda_allocation,
                      data, kfold_method[["train"]], kfold_method[["test"]])
  kfold_method[["train_lda"]] <- lapply(kfold_lda, `[[`, 1)
  kfold_method[["test_lda"]] <- lapply(kfold_lda, `[[`, 2)
  kfold_method
}