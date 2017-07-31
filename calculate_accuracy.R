library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(tidyr)

source("accuracy_functions.R")

# get same data as used for allocations
source_lines("calculate_allocations.R", 7:25) # careful!
#rm(survey_points_raw)


load("A:/1_UNSW/0_data/Dharawal_project/big_list.RData")
# big_list <- big_list[1]
# save(big_list, file = "big_list.RData")
# load("big_list.RData") # temporary so development is more wieldly

# decide what results to extract - be VERY careful, and examine the data frame well
get_this <- rbind(
  # for lda classifier
  data.frame(
    scenario = c(rep("boot", 3),
                 rep("rrcv", 24),
                 rep("kfold", 12),
                 "alldat"),
    type = c(rep("boot", 3),
             rep(names(big_list[[1]][["rrcv"]]), each = 3),
             rep(names(big_list[[1]][["kfold"]]), each = 3),
             "alldat"),
    method = c(rep(c("train_lda", "test_lda", "true_lda"), 13), "all_lda"),
    tt = c(rep(c("train", "test", "true"), 13), "alldat"),
    stringsAsFactors = F),
  # for knn classifier
  data.frame(
    scenario = c(rep("boot", 2),
                 rep("rrcv", 16),
                 rep("kfold", 8),
                 "alldat"),
    type = c(rep("boot",2),
             rep(names(big_list[[1]][["rrcv"]]), each = 2),
             rep(names(big_list[[1]][["kfold"]]), each = 2),
             "alldat"),
    method = c(rep(c("test_knn", "true_knn"), 13), "all_knn"),
    tt = c(rep(c("test", "true"), 13), "alldat"),
    stringsAsFactors = F),
  # for rf classifier
  data.frame(
    scenario = c(rep("boot", 3),
                 rep("rrcv", 24),
                 rep("kfold", 12),
                 "alldat"),
    type = c(rep("boot", 3),
             rep(names(big_list[[1]][["rrcv"]]), each = 3),
             rep(names(big_list[[1]][["kfold"]]), each = 3),
             "alldat"),
    method = c(rep(c("train_rf", "test_rf", "true_rf"), 13), "all_rf"),
    tt = c(rep(c("train", "test", "true"), 13), "alldat"),
    stringsAsFactors = F)
  )



# calculate stats ---------------------------------------------------------

metric_results <- rbindlist(lapply(
  X = 1:length(big_list),
  FUN = collect_one_iteration,
  get_this, big_list, survey_points
))
save(metric_results, file="metric_results.RData")


# main plots --------------------------------------------------------------------

load("metric_results.RData")

# make a long df to plot various method/type combos
metric_results$sample_origin <- NA
metric_results$sample_origin[grep("test", metric_results$method)] <- "test"
metric_results$sample_origin[grep("train", metric_results$method)] <- "train"
metric_results$sample_origin[grep("true", metric_results$method)] <- "true"
metric_results$sample_origin[grep("all", metric_results$method)] <- "all"
metric_results$sample_origin <- factor(metric_results$sample_origin, 
                                       levels = c("true", "train", "test", "all"))

metric_results$sample_structure <- NA
metric_results$sample_structure[grep("boot", metric_results$type)] <- "bootstrap"
metric_results$sample_structure[grep("type1", metric_results$type)] <- "random"
metric_results$sample_structure[grep("type2", metric_results$type)] <- "class"
metric_results$sample_structure[grep("type3", metric_results$type)] <- "class-space"
metric_results$sample_structure[grep("type4", metric_results$type)] <- "block"
metric_results$sample_structure[grep("alldat", metric_results$type)] <- "all-data"
metric_results$sample_structure <- factor(metric_results$sample_structure,
                                          levels = c("bootstrap", "random", "block", "class", "class-space", "all-data"))

metric_results$sample_fraction <- NA
metric_results$sample_fraction[grep("boot", metric_results$type)] <- "bootstrap"
metric_results$sample_fraction[grep("67", metric_results$type)] <- "67-33"
metric_results$sample_fraction[grep("80", metric_results$type)] <- "80-20"
metric_results$sample_fraction[grep("k5", metric_results$type)] <- "5-fold"
metric_results$sample_fraction[grep("alldat", metric_results$type)] <- "all-data"
metric_results$sample_fraction <- factor(metric_results$sample_fraction,
                                          levels = c("all-data", "bootstrap", "67-33", "80-20", "5-fold"))

metric_results$model <- NA
metric_results$model[grep("lda", metric_results$method)] <- "max-likelihood"
metric_results$model[grep("knn", metric_results$method)] <- "nearest-n"
metric_results$model[grep("rf", metric_results$method)] <- "random-forest"

metric_results_long <- metric_results %>%
  select(perc_agr:alloc_dis, model, sample_structure, sample_fraction, sample_origin, iter_n) %>%
  gather("metric", "value", perc_agr:alloc_dis) %>%
  mutate(metric = factor(metric, levels = c("perc_agr", "kappa", "entropy", "purity", "quant_dis", "alloc_dis"))) %>%
  filter(!is.na(value))


# main plots
plot_train_test(metric_results_long, "max-likelihood", origins = c("train","test"))
plot_train_test(metric_results_long, "max-likelihood")
plot_train_test(metric_results_long, "random-forest")
plot_train_test(metric_results_long, "nearest-n")

# additonal plots
plot_train_test(metric_results_long, origins = c("all", "train", "test", "true"), model_type = "max-likelihood",
                structures = c("bootstrap", "random","block", "class", "class-space", "all-data"), suffix = "_true")

mle_train_test_iters <- metric_results_long %>%
  filter(sample_origin %in% c("train", "test"),
    #sample_fraction %in% c("all-data", "67-33", "5-fold"),
    metric == "perc_agr",
    iter_n %in% c(1,4,13,19), 
    model == "max-likelihood",
    metric != "purity") %>%
  ggplot(., aes(y = value)) +
  geom_violin(aes(x = sample_structure, fill = sample_fraction), scale = "area", draw_quantiles = c(0.05,0.5,0.9), lwd=0.25) +
  scale_fill_manual("Resampling design", values = c("#969696", "#cb181d", "#fc9272", "#31a354")) +
  #scale_colour_manual("Sample type", values = c("#969696", "#fdae6b", "#d94801")) + 
  ylab("Metric value") + xlab("Stratification design") +
  theme_bw() +
  facet_grid(iter_n ~ sample_origin, scales = "free", space = "free", drop = T)
ggsave("plots/max-likelihood_iters.pdf", plot = mle_train_test_iters, device = "pdf", width = 20, height = 13)


load("metric_results_long_reps.RData")
metric_results_long_reps$reps <- factor(metric_results_long_reps$reps,
                                        levels = c("5-reps","50-reps","100-reps"))
metric_results_long_reps$sample_origin <- factor(as.character(metric_results_long_reps$sample_origin),
                                                 levels = c("true", "train", "test", "all"))
mle_percagr_reps <- metric_results_long_reps %>%
  filter(sample_origin %in% c("all", "train", "test"),
    #sample_fraction %in% c("all-data", "67-33", "5-fold"),
    model == "max-likelihood",
    metric == "perc_agr") %>%
  ggplot(., aes(y = value)) +
  geom_boxplot(aes(x = sample_structure, fill = sample_fraction), outlier.size = 0.25, lwd=0.25) +
  scale_fill_manual("Resampling design", values = c("#969696", "#969696", "#cb181d", "#fc9272", "#31a354")) +
  #scale_colour_manual("Sample type", values = c("#969696", "#fdae6b", "#d94801")) + 
  ylab("Metric value") + xlab("Stratification design") +
  theme_bw() +
  facet_grid(reps ~ sample_origin, scales = "free", space = "free", drop = T)
ggsave("plots/mle_perc-agr_efficiency.pdf", plot = mle_percagr_reps, device = "pdf", width = 20, height = 13)



# means etc. --------------------------------------------------------------


# looks at mean/CI/min-max summaries
metric_stats <- metric_results %>%
  group_by(type, method) %>%
  summarise(mean = mean(perc_agr),
            median = median(perc_agr),
            upper = quantile(perc_agr, 0.975),
            lower = quantile(perc_agr, 0.025),
            max = max(perc_agr),
            min = min(perc_agr))



# junk --------------------------------------------------------------------

# # plot everything... yuk!
# big_plt <- ggplot(data = metric_results_long, aes(y = value)) +
#   geom_boxplot(aes(x = sample_structure, fill = sample_origin, colour = sample_fraction), outlier.size = 0.25, lwd=0.25, notch = F) +
#   scale_colour_manual(values = c("#969696", "#969696", "#cb181d", "#fc9272", "#31a354")) +
#   scale_fill_manual(values = c("#969696", "#9e9ac8", "#fdae6b", "#d94801")) +
#   theme_classic() +
#   facet_wrap(~ model + metric, ncol = 5, scales = "free")
# ggsave("plots/method_metric_facet.pdf", plot = big_plt, device = "pdf", width = 20, height = 10)

# plot max-like, group by sample design
# mle_train_test1 <- metric_results_long %>%
#   filter(sample_origin %in% c("all", "train", "test"),
#          #sample_fraction %in% c("all-data", "67-33", "5-fold"),
#          model == "max-likelihood",
#          metric %in% metrics) %>%
#   ggplot(., aes(y = value)) +
#   geom_violin(aes(x = sample_structure, fill = sample_fraction), scale = "area", draw_quantiles = quants, lwd=0.25) +
#   scale_fill_manual("Resampling design", values = c("#969696", "#969696", "#cb181d", "#fc9272", "#31a354")) +
#   #scale_colour_manual("Sample type", values = c("#969696", "#fdae6b", "#d94801")) + 
#   ylab("Metric value") + xlab("Stratification design") +
#   theme_bw() +
#   facet_grid(metric ~ sample_origin, scales = "free", space = "free", drop = T)
# ggsave("plots/mle_train_test1.pdf", plot = mle_train_test1, device = "pdf", width = 20, height = 13)

# # old pa plot
# pa_plot <- ggplot(data = pa_results, aes(y = perc_agr)) +
#   geom_boxplot(aes(x = type, colour = scenario, fill = method), notch = T) +
#   scale_fill_manual(values = c("#fcbba1", "#fb6a4a", "#d4b9da", "#99d8c9", "#238b45")) +
#   scale_colour_manual(values = c("#252525", "#e31a1c", "#3f007d"))
# ggsave("perc-agr_results.pdf", plot = pa_plot, device = "pdf", width = 10, height = 5)


# # test perc agreement on smaller chunks of iters and individual iters
# iter_n_breaks <- list(1:30, 31:60, 61:90, 91:120, 121:150, 151:180, 181:210, 211:240, 241:270, 271:300)
# pa_res_plots <- lapply(X = iter_n_breaks, FUN = plot_pa_results, pa_results)
# ggsave("perc-agr_results_30iters.pdf", plot = grid.arrange(grobs = pa_res_plots, ncol=2),
#        device = "pdf", width = 20, height = 10)
# 
# iter_n_breaks <- as.list(sample(1:300,10))
# pa_res_plots <- lapply(X = iter_n_breaks, FUN = plot_pa_results, pa_results)
# ggsave("perc-agr_results_indiv-iter.pdf", plot = grid.arrange(grobs = pa_res_plots, ncol=2),
#        device = "pdf", width = 20, height = 10)




