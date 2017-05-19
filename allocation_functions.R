ecologist_sample <- function(data, samp_frac) {
  sample_out <- data %>%
    group_by(veg_cl_tm, studyarea_) %>%
    sample_frac(samp_frac)
  as.data.frame(sample_out)
}

# type: 1 = random, 2 = random strat by veg, 3 = random strat by veg & space
rrcv_splitter <- function(data, train_frac, type) {
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
kfold_splitter <- function(data, kfold_k, type) {
  if (type == 1) {
    train_ids <- data %>%
      select(id) %>%
      mutate(fold = rep(1:kfold_k, length.out = n()))
    train_ids <- get_kfold_matrix(train_ids)
  } else if (type == 2) {
    train_ids <- data %>%
      select(id, veg_cl_tm) %>%
      group_by(veg_cl_tm) %>%
      mutate(fold = sample(rep(1:kfold_k, length.out = n())), replace = F)
    train_ids <- get_kfold_matrix(train_ids)
  } else if (type == 3) {
    train_ids <- data %>%
      select(id, veg_cl_tm, studyarea_) %>%
      group_by(veg_cl_tm, studyarea_) %>%
      mutate(fold = sample(rep(1:kfold_k, length.out = n())), replace = F)
    train_ids <- get_kfold_matrix(train_ids)
  }
  train_ids
}

get_kfold_matrix <- function(data_with_folds) {
  folds <- unique(data_with_folds$fold)
  fold_list <- lapply(X = 1:length(folds), FUN = function(x) {data_with_folds$id[data_with_folds$fold != x]})
  fold_matrix <- matrix(unlist(fold_list), ncol = length(folds))
}