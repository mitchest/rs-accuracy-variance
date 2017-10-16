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
#load("A:/1_UNSW/0_data/Dharawal_project/big_list_alldat.RData")

#big_list <- big_list[1]
#save(big_list, file = "big_list.RData")
#load("big_list.RData") # temporary so development is more wieldly

# decide what results to extract - be VERY careful, and examine the data frame well
get_this <- rbind(
  # for lda classifier
  data.frame(
    scenario = c(rep("boot", 3),
                 rep("rrcv", 24),
                 rep("kfold", 12)),
    type = c(rep("boot", 3),
             rep(names(big_list[[1]][["rrcv"]]), each = 3),
             rep(names(big_list[[1]][["kfold"]]), each = 3)),
    method = c(rep(c("train_lda", "test_lda", "true_lda"), 13)),
    tt = c(rep(c("train", "test", "true"), 13)),
    stringsAsFactors = F),
  # for knn classifier
  data.frame(
    scenario = c(rep("boot", 2),
                 rep("rrcv", 16),
                 rep("kfold", 8)),
    type = c(rep("boot",2),
             rep(names(big_list[[1]][["rrcv"]]), each = 2),
             rep(names(big_list[[1]][["kfold"]]), each = 2)),
    method = c(rep(c("test_knn", "true_knn"), 13)),
    tt = c(rep(c("test", "true"), 13)),
    stringsAsFactors = F),
  # for rf classifier
  data.frame(
    scenario = c(rep("boot", 3),
                 rep("rrcv", 24),
                 rep("kfold", 12)),
    type = c(rep("boot", 3),
             rep(names(big_list[[1]][["rrcv"]]), each = 3),
             rep(names(big_list[[1]][["kfold"]]), each = 3)),
    method = c(rep(c("train_rf", "test_rf", "true_rf"), 13)),
    tt = c(rep(c("train", "test", "true"), 13)),
    stringsAsFactors = F)
  )

get_this_all <- data.frame(
  scenario = rep("alldat", 3),
  type = rep("alldat", 3),
  method = c("all_lda", "all_knn", "all_rf"),
  tt = rep("alldat", 3),
  stringsAsFactors = F)



# calculate stats ---------------------------------------------------------

metric_results <- rbindlist(lapply(
  X = 1:length(big_list),
  FUN = collect_one_iteration,
  get_this, big_list, survey_points
))
save(metric_results, file="metric_results_full.RData")

# metric_results_alldat <- rbindlist(lapply(
#   X = 1:length(big_list_alldat),
#   FUN = collect_one_iteration,
#   get_this_all, big_list_alldat, survey_points
# ))
# metric_results_alldat <- filter(metric_results_alldat, scenario == "alldat")
# save(metric_results_alldat, file="metric_results_alldat.RData")

load("metric_results_full.RData")
#load("metric_results_alldat.RData")
#metric_results <- rbind(metric_results, metric_results_alldat)
save(metric_results, file="metric_results.RData")



# plots -------------------------------------------------------------------------

load("metric_results.RData")

# make a long df to plot various method/type combos
metric_results$sample_origin <- NA
metric_results$sample_origin[grep("test", metric_results$method)] <- "test"
metric_results$sample_origin[grep("train", metric_results$method)] <- "train"
#metric_results$sample_origin[grep("true", metric_results$method)] <- "true"
metric_results$sample_origin[grep("all", metric_results$method)] <- "all"
# metric_results$sample_origin <- factor(metric_results$sample_origin, 
#                                        levels = c("true", "train", "test", "all"))
metric_results$sample_origin <- factor(metric_results$sample_origin, 
                                       levels = c("true", "train", "test"))

metric_results$sample_structure <- NA
metric_results$sample_structure[grep("boot", metric_results$type)] <- "bootstrap"
metric_results$sample_structure[grep("type1", metric_results$type)] <- "random"
metric_results$sample_structure[grep("type2", metric_results$type)] <- "class"
metric_results$sample_structure[grep("type3", metric_results$type)] <- "class-space"
metric_results$sample_structure[grep("type4", metric_results$type)] <- "block"
#metric_results$sample_structure[grep("alldat", metric_results$type)] <- "all-data"
# metric_results$sample_structure <- factor(metric_results$sample_structure,
#                                           levels = c("bootstrap", "random", "block", "class", "class-space", "all-data"))
metric_results$sample_structure <- factor(metric_results$sample_structure,
                                          levels = c("bootstrap", "random", "block", "class", "class-space"))

metric_results$sample_fraction <- NA
metric_results$sample_fraction[grep("boot", metric_results$type)] <- "bootstrap"
metric_results$sample_fraction[grep("67", metric_results$type)] <- "67-33"
metric_results$sample_fraction[grep("80", metric_results$type)] <- "80-20"
metric_results$sample_fraction[grep("k5", metric_results$type)] <- "5-fold"
#metric_results$sample_fraction[grep("alldat", metric_results$type)] <- "all-data"
# metric_results$sample_fraction <- factor(metric_results$sample_fraction,
#                                           levels = c("all-data", "bootstrap", "67-33", "80-20", "5-fold"))
metric_results$sample_fraction <- factor(metric_results$sample_fraction,
                                         levels = c("bootstrap", "67-33", "80-20", "5-fold"))

metric_results$model <- NA
metric_results$model[grep("lda", metric_results$method)] <- "max-likelihood"
metric_results$model[grep("knn", metric_results$method)] <- "nearest-n"
metric_results$model[grep("rf", metric_results$method)] <- "random-forest"

metric_results_long <- metric_results %>%
  select(perc_agr:wh_prod, model, sample_structure, sample_fraction, sample_origin, iter_n) %>%
  gather("metric", "value", perc_agr:wh_prod) %>%
  mutate(metric = factor(metric, levels = c("perc_agr", "kappa", "entropy", "purity", "quant_dis", "alloc_dis",
                                            "bt_user", "ew_user", "ttt_user", "wh_user",
                                            "bt_prod", "ew_prod", "ttt_prod", "wh_prod"))) %>%
  filter(!is.na(value))

metric_results_long$class <- NA
metric_results_long$class[grep("bt", metric_results_long$metric)] <- "Banksia"
metric_results_long$class[grep("ew", metric_results_long$metric)] <- "Eucalypt"
metric_results_long$class[grep("ttt", metric_results_long$metric)] <- "Tea-tree"
metric_results_long$class[grep("wh", metric_results_long$metric)] <- "Wet-heath"
metric_results_long$class <- factor(metric_results_long$class,
                                    levels = c("Banksia","Eucalypt","Tea-tree","Wet-heath"))

metric_results_long$user_prod <- NA
metric_results_long$user_prod[grep("user", metric_results_long$metric)] <- "user"
metric_results_long$user_prod[grep("prod", metric_results_long$metric)] <- "producer"
metric_results_long$user_prod <- factor(metric_results_long$user_prod,
                                    levels = c("user","producer"))

# main plots
plot_by_structure(metric_results_long, "max-likelihood", origins = c("test"), suffix = "-test")

plot_by_structure(metric_results_long, "max-likelihood", origins = c("train", "test", "true"))
plot_by_structure(metric_results_long, "random-forest", origins = c("train", "test", "true"))
plot_by_structure(metric_results_long, "nearest-n", origins = c("train", "test", "true"))

plot_by_model(metric_results_long, c("max-likelihood","random-forest", "nearest-n"), origins = c("test"), suffix = "ml-nn-rf-test")

plot_user_prod(metric_results_long, c("max-likelihood"), suffix = "ml-user-prod-test")
plot_user_prod(metric_results_long, c("max-likelihood"), origins = c("train"), suffix = "ml-user-prod-train")
# plot_user_prod(metric_results_long, c("max-likelihood"), origins = c("true"), suffix = "ml-true-user-prod")

plot_by_structure(metric_results_long, "max-likelihood", origins = c("true", "train","test"),
                  metrics = c("bt_user", "ew_user", "ttt_user", "wh_user"),
                  suffix = "-user", scales = "free_x")
plot_by_structure(metric_results_long, "max-likelihood", origins = c("true", "train","test"),
                  metrics = c("bt_prod", "ew_prod", "ttt_prod", "wh_prod"),
                  suffix = "-producer", scales = "free_x")

plot_by_structure(metric_results_long, "max-likelihood", origins = c("train","test","true"))

# plot_by_structure(metric_results_long, "random-forest", origins = c("all", "true","test"),
#                 metrics = c("perc_agr", "bt_user", "ew_user", "ttt_user", "wh_user"),
#                 suffix = "-user", scales = "free_x")
# plot_by_structure(metric_results_long, "random-forest", origins = c("all", "true","test"),
#                 metrics = c("perc_agr", "bt_prod", "ew_prod", "ttt_prod", "wh_prod"),
#                 suffix = "-producer", scales = "free_x")


mle_train_test_iters <- metric_results_long %>%
  filter(sample_origin %in% c("true", "test"),
    #sample_fraction %in% c("all-data", "67-33", "5-fold"),
    metric == "perc_agr",
    iter_n %in% c(1,4,13,19), 
    model == "max-likelihood",
    metric != "purity") %>%
  ggplot(., aes(y = value)) +
  geom_violin(aes(x = sample_structure, fill = sample_fraction), scale = "area", draw_quantiles = c(0.05,0.5,0.9), lwd=0.25) +
  scale_fill_manual("Resampling design", values = c("#969696", "#d55e00", "#f0e442", "#56b4e9")) +
  #scale_colour_manual("Sample type", values = c("#969696", "#fdae6b", "#d94801")) + 
  ylab("Metric value") + xlab("Stratification design") +
  theme_bw() +
  facet_grid(iter_n ~ sample_origin, scales = "free_x", space = "free", drop = T)
ggsave("plots/max-likelihood_iters.png", plot = mle_train_test_iters, device = "png", width = 20, height = 13)

mle_iter_cummean <- metric_results_long %>%
  filter(iter_n == 1, model == "max-likelihood", sample_structure != "all-data", metric == "perc_agr", sample_origin == "test") %>%
  select(sample_structure, sample_fraction, value) %>%
  group_by(sample_structure, sample_fraction) %>%
  mutate(iterations = 1:n(), cum_med = cummean(value), scenario = paste0(sample_structure,"_",sample_fraction)) %>%
  ggplot(., aes(y = cum_med, x = iterations)) +
  geom_line(aes(colour = scenario)) +
  ylab("Percentage agreement") + xlab("Number of iterations") + theme_bw()
ggsave("plots/mle_perc-agr_efficiency.png", plot = mle_iter_cummean, device = "png", width = 12, height = 8)

# load("metric_results_long_reps.RData")
# metric_results_long_reps$reps <- factor(metric_results_long_reps$reps,
#                                         levels = c("5-reps","50-reps","100-reps"))
# metric_results_long_reps$sample_origin <- factor(as.character(metric_results_long_reps$sample_origin),
#                                                  levels = c("true", "train", "test", "all"))
# mle_percagr_reps <- metric_results_long_reps %>%
#   filter(sample_origin %in% c("all", "train", "test"),
#     #sample_fraction %in% c("all-data", "67-33", "5-fold"),
#     model == "max-likelihood",
#     metric == "perc_agr") %>%
#   ggplot(., aes(y = value)) +
#   geom_boxplot(aes(x = sample_structure, fill = sample_fraction), outlier.size = 0.25, lwd=0.25) +
#   scale_fill_manual("Resampling design", values = c("#969696", "#969696", "#cb181d", "#fc9272", "#31a354")) +
#   #scale_colour_manual("Sample type", values = c("#969696", "#fdae6b", "#d94801")) + 
#   ylab("Metric value") + xlab("Stratification design") +
#   theme_bw() +
#   facet_grid(reps ~ sample_origin, scales = "free", space = "free", drop = T)
# ggsave("plots/mle_perc-agr_efficiency.pdf", plot = mle_percagr_reps, device = "pdf", width = 20, height = 13)



# main paper figures ------------------------------------------------------

library(grid)

# fig. 2
fig2 <- metric_results_long %>%
  filter(sample_origin %in% c("test"),
         sample_structure %in% c("bootstrap", "random","block", "class", "class-space"),
         model == "max-likelihood",
         metric %in% c("perc_agr", "kappa", "entropy", "purity", "quant_dis", "alloc_dis")) %>%
  mutate(metric = recode(metric, "perc_agr" = "percentage agreement", "quant_dis" = "quant. dis.", "alloc_dis" = "alloc. dis."),
         sample_structure = recode(sample_structure, "class-space" = "class & space")) %>%
  ggplot(., aes(y = value)) +
  geom_violin(aes(x = sample_origin, fill = sample_fraction), scale = "area", draw_quantiles = c(0.05,0.5,0.9), lwd=0.25) +
  scale_fill_manual("Resampling design", values = c("#969696", "#d55e00", "#f0e442", "#56b4e9")) +
  ylab("Accuracy metric value") + xlab("Sampling stratification design") + 
  theme_bw() + theme(axis.text.x = element_blank(), strip.text = element_text(size = 12),
                     axis.ticks.x = element_blank(), axis.title = element_text(size=14)) +
  facet_grid(metric ~ sample_structure, scales = "free", space = "free", drop = T)

# manual mucking with displays
fig2G <- ggplotGrob(fig2)

fig2G$widths[c(6,8,10,12)] <- unit(3,"null")

fig2G$heights[c(7)] <- unit(1,"null")
fig2G$heights[c(9,11,13,15,17)] <- unit(0.4,"null")

# grid.newpage()
# grid.draw(fig2G)

ggsave(plot = fig2G, filename = paste0("plots/figure2.pdf"), device = "pdf", width = 18, height = 12.5)



# fig. 3

#stats first 
# looks at mean/CI/min-max summaries
metric_stats <- metric_results_long %>%
  group_by(model, sample_structure, sample_fraction, sample_origin, metric) %>%
  summarise(mean = mean(value),
            median = median(value),
            upper = quantile(value, 0.95),
            lower = quantile(value, 0.05),
            max = max(value),
            min = min(value)) %>%
  mutate_at(vars(mean:min), funs(round(.,3))) %>%
  ungroup() %>%
  mutate(sample_structure = recode(sample_structure, "bootstrap" = "BS", "class-space" = "class & space"))

# User/prod table
user_prod_names <- c("bt_prod", "ew_prod", "ttt_prod", "wh_prod", "bt_user", "ew_user", "ttt_user", "wh_user")
user_prod_table <- metric_stats %>%
  filter(model == "max-likelihood", sample_origin == "test",
         metric %in% user_prod_names)
user_prod_table$class <- NA
user_prod_table$class[grep("bt", user_prod_table$metric)] <- "Banksia"
user_prod_table$class[grep("ew", user_prod_table$metric)] <- "Eucalypt"
user_prod_table$class[grep("ttt", user_prod_table$metric)] <- "Tea tree"
user_prod_table$class[grep("wh", user_prod_table$metric)] <- "Wet heath"
user_prod_table$user_prod <- NA
user_prod_table$user_prod[grep("user", user_prod_table$metric)] <- "user"
user_prod_table$user_prod[grep("prod", user_prod_table$metric)] <- "producer"
#write.csv(user_prod_table, file = "plots/user_prod_table.csv")

# figure
fig3 <- user_prod_table %>%
  filter(sample_origin %in% c("test"),
         sample_structure %in% c("BS", "random","block", "class", "class & space"),
         model == "max-likelihood",
         metric %in% c("bt_user", "ew_user", "ttt_user", "wh_user","perc_agr", "bt_prod", "ew_prod", "ttt_prod", "wh_prod")) %>%
  ggplot(., aes(y = median)) +
  geom_errorbar(aes(x = sample_fraction, ymin = min, ymax = max), width = 0) +
  geom_point(aes(x = sample_fraction)) +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  #scale_fill_manual("Resampling design", values = c("#969696", "#d55e00", "#f0e442", "#56b4e9")) +
  #scale_colour_manual("Resampling design", values = c("#969696", "#d55e00", "#f0e442", "#56b4e9")) +
  ylab("Accuracy value") + xlab("Sampling stratification design") + 
  theme_bw() + theme(strip.text = element_text(size = 12), axis.title = element_text(size=14)) +
  facet_grid(class + user_prod ~ sample_structure, scales = "free_x", space = "free", drop = T)

ggsave(plot = fig3, filename = paste0("plots/figure3.pdf"), device = "pdf", width = 11, height = 8)

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




