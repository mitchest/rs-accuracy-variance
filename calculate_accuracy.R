library(dplyr)
library(data.table)

source("accuracy_functions.R")

# get same data as used for allocations
source_lines("calculate_allocations.R", 10:21) # careful!


# load("big_list.RData")
load("big_list_lite.RData") # temporary so development is more wieldly


iter_n <- big_list[[1]]


get_this <- data.frame(scenario = c("boot", "boot", rep("rrcv", 12), rep("kfold", 6)),
                       type = c(rep("boot",2), 
                              rep(names(big_list[[1]][["rrcv"]]), 2), 
                              rep(names(big_list[[1]][["kfold"]]), 2)),
                       method = rep(c("train_lda", "test_lda"), 10),
                       tt = rep(c("train", "test"), 10),
                       stringsAsFactors = F)


# calculate stats ---------------------------------------------------------


percentage_agreement <- function(reps, true_id, pred_class, data) {
  sum(as.character(data[true_id[[reps]], "veg_cl_tm"]) == as.character(pred_class[[reps]])) / length(pred_class[[reps]])
}


collect_pa_results <- function(this_row, get_this, iter_n, data) {
  if (get_this$type[this_row] == "boot") {
    return(
      data.frame(
        perc_agr = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = percentage_agreement,
            iter_n[["boot"]][[get_this$tt[this_row]]],
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        type = get_this$type[this_row],
        method = get_this$method[this_row])
    )
  } else {
    return(
      data.frame(
        perc_agr = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = percentage_agreement,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]],
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        type = get_this$type[this_row],
        method = get_this$method[this_row])
    )
  }
}

test = bind_rows(lapply(X = 1:nrow(get_this),
                  FUN = collect_pa_results,
                  get_this, iter_n, survey_points))

collect_one_iteration <- function(X) {
  
}
  
  
  
  
  
  
  
  

