fix_rs_dat <- function(x) {
  x[x <= 0] <- 0.001
  log(x)
}


HardClust = function(probs){
  probs = data.frame(round(probs, 5))
  probs_rowmax = apply(probs,1,max)
  hardclust = integer(nrow(probs)) # pre-allocate vector
  for (i in 1:nrow(probs)){
    hardclust[i] = which(probs[i,]==probs_rowmax[i])
  }
  return(hardclust)
}

# specific fitting function, just to make code a bit neater
# iter argument first allows easy lapply usage if desired
fit_split_brt <- function(iter, data, overall_sample_frac, train_test_frac) {
  gc()
  
  overall_sample_frac <- overall_sample_frac # can vary these two parameters later on too
  train_test_frac <- train_test_frac
  
  brt_dat_sample <- survey_points_veg %>%
    select(veg_cl_tm, studyarea_,
           eucdist_waterobs, fpc, 
           sfc_aut_g_95, 
           sfc_spr_b_50, sfc_spr_b_95, sfc_spr_d_95,
           sfc_sum_d_05, sfc_sum_b_50, sfc_sum_g_95,
           sfc_win_b_95, sfc_win_b_95, sfc_win_b_95) %>%
    mutate(veg_cl_tm = as.factor(veg_cl_tm),
           id = 1:nrow(.)) %>%
    group_by(veg_cl_tm, studyarea_) %>%
    sample_frac(overall_sample_frac) # choose some more realistic sample of data (stratified)
  
  # make a 70-30 'independent split' (stratified)
  brt_dat_train <- brt_dat_sample %>%
    group_by(veg_cl_tm) %>%
    sample_frac(train_test_frac)
  brt_dat_test <- brt_dat_sample[!brt_dat_sample$id %in% brt_dat_train$id,]
  
  # fit
  fm_brt <- gbm(formula = veg_cl_tm ~ eucdist_waterobs + fpc + 
                  sfc_aut_g_95 + 
                  sfc_spr_b_50 + sfc_spr_b_95 + sfc_spr_d_95 +
                  sfc_sum_d_05 + sfc_sum_b_50 + sfc_sum_g_95 +
                  sfc_win_b_95 + sfc_win_b_95 + sfc_win_b_95,
                data = brt_dat_train,
                distribution = "multinomial",
                n.trees = 1000,
                shrinkage = 0.05,
                interaction.depth = 1,
                bag.fraction = 0.5,
                train.fraction = 1,
                n.minobsinnode = 10,
                cv.folds = 0,
                keep.data = F, verbose = F, n.cores = 1)
  
  # diagnose
  best_brt <- suppressWarnings(
    gbm.perf(fm_brt, method = "OOB", plot.it = F)
  )
  
  # predict
  preds <- as.data.frame(predict.gbm(fm_brt, brt_dat_test, best_brt, type="response"))
  names(preds) <- tools::file_path_sans_ext(names(preds))
  # hard classify
  classified_brt <- names(preds)[HardClust(preds)]
  # test accuracy
  overall_acc <- sum(classified_brt == brt_dat_test$veg_cl_tm) / nrow(brt_dat_test)
  print(paste0("Accuracy[",iter,"]: ", round(overall_acc,2)))
  invisible(overall_acc)
  
}