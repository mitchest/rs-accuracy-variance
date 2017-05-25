library(dplyr)
library(data.table)

source("accuracy_functions.R")

# get same data as used for allocations
source_lines("calculate_allocations.R", 10:21) # careful!
rm(survey_points_raw)


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

pa_results <- rbindlist(lapply(
  X = 1:length(big_list),
  FUN = collect_pa_iteration,
  get_this, big_list, survey_points
))



# plots -------------------------------------------------------------------



