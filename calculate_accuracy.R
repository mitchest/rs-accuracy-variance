library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

source("accuracy_functions.R")

# get same data as used for allocations
source_lines("calculate_allocations.R", 8:23) # careful!
#rm(survey_points_raw)


load("A:/1_UNSW/0_data/Dharawal_project/big_list.RData")
#load("big_list_lite.RData") # temporary so development is more wieldly

# decide what results to extract - be VERY careful, and examine the data frame well
get_this <- rbind(
  # for lda classifier
  data.frame(
    scenario = c("boot", "boot", rep("rrcv", 12), rep("kfold", 6)),
    type = c(rep("boot", 2),
           rep(names(big_list[[1]][["rrcv"]]), each = 2),
           rep(names(big_list[[1]][["kfold"]]), each = 2)),
    method = rep(c("train_lda", "test_lda"), 10),
    tt = rep(c("train", "test"), 10),
    stringsAsFactors = F),
  # for knn classifier
  data.frame(
    scenario = c("boot", rep("rrcv", 6), rep("kfold", 3)),
    type = c("boot",
             names(big_list[[1]][["rrcv"]]),
             names(big_list[[1]][["kfold"]])),
    method = rep("test_knn", 10),
    tt = rep("test", 10),
    stringsAsFactors = F),
  # for rf classifier
  data.frame(
    scenario = c("boot", "boot", rep("rrcv", 12), rep("kfold", 6)),
    type = c(rep("boot", 2),
             rep(names(big_list[[1]][["rrcv"]]), each = 2),
             rep(names(big_list[[1]][["kfold"]]), each = 2)),
    method = rep(c("train_rf", "test_rf"), 10),
    tt = rep(c("train", "test"), 10),
    stringsAsFactors = F)
  )



# calculate stats ---------------------------------------------------------

pa_results <- rbindlist(lapply(
  X = 1:length(big_list),
  FUN = collect_pa_iteration,
  get_this, big_list, survey_points
))



# plots -------------------------------------------------------------------

# plot all data as boxplots
pa_plot <- ggplot(data = pa_results, aes(y = perc_agr)) +
  geom_boxplot(aes(x = type, colour = scenario, fill = method), notch = T) +
  scale_fill_manual(values = c("#fcbba1", "#fb6a4a", "#d4b9da", "#99d8c9", "#238b45")) +
  scale_colour_manual(values = c("#252525", "#e31a1c", "#3f007d"))
ggsave("perc-agr_results.pdf", plot = pa_plot, device = "pdf", width = 10, height = 5)

iter_n_breaks <- list(1:30, 31:60, 61:90, 91:120, 121:150, 151:180, 181:210, 211:240, 241:270, 271:300)
pa_res_plots <- lapply(X = iter_n_breaks, FUN = plot_pa_results, pa_results)
ggsave("perc-agr_results_30iters.pdf", plot = grid.arrange(grobs = pa_res_plots, ncol=2),
       device = "pdf", width = 20, height = 10)

iter_n_breaks <- as.list(sample(1:300,10))
pa_res_plots <- lapply(X = iter_n_breaks, FUN = plot_pa_results, pa_results)
ggsave("perc-agr_results_indiv-iter.pdf", plot = grid.arrange(grobs = pa_res_plots, ncol=2),
       device = "pdf", width = 20, height = 10)

# looks at mean/CI/min-max summaries
pa_plotting <- pa_results %>%
  group_by(iter_n, type, method) %>%
  summarise(mean = mean(perc_agr),
            upper = quantile(perc_agr, 0.975),
            lower = quantile(perc_agr, 0.025),
            max = max(perc_agr),
            min = min(perc_agr))


ggplot(data = pa_plotting, aes(x = iter_n)) +
  geom_(y = mean)








