library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)
# library(gbm3) # eeek no multinomial in gbm3!
library(gbm)

source("dharawal_classification_funs.R")

# load data ---------------------------------------------------------------

survey_points <- read.csv("classification_data/dharawal_pointsTM_covars.csv",
                          header = T, stringsAsFactors = F)



# clean data --------------------------------------------------------------
survey_points_veg <- survey_points %>%
  filter(veg_cl_tm %in% c("bt", "ew", "ttt", "wh")) %>%
  na.omit()



# explore veg classes vs. predictors --------------------------------------

# par(mfrow=c(3,3))
# 
# for (covar in names(survey_points_veg)[13:45]) {
#   ydat <- fix_rs_dat(survey_points_veg[,covar])
#   boxplot(ydat ~ survey_points_veg$veg_cl_tm,
#           main = covar,
#           xlab = round(summary(lm(ydat ~ survey_points_veg$veg_cl_tm))$r.sq, 4))
# }
# 
# par(mfrow=c(1,1))




# test a BRT --------------------------------------------------------------

# # not sure on the environment overheads for gbm, so just choose the data needed
# brt_dat <- survey_points_veg %>%
#   select(veg_cl_tm, 
#          eucdist_waterobs, fpc, 
#          sfc_aut_g_95, 
#          sfc_spr_b_50, sfc_spr_b_95, sfc_spr_d_95,
#          sfc_sum_d_05, sfc_sum_b_50, sfc_sum_g_95,
#          sfc_win_b_95, sfc_win_b_95, sfc_win_b_95) %>%
#   mutate(veg_cl_tm = as.factor(veg_cl_tm))
# 
# # fit
# fm_brt_all <- gbm(formula = veg_cl_tm ~ eucdist_waterobs + fpc + 
#                 sfc_aut_g_95 + 
#                 sfc_spr_b_50 + sfc_spr_b_95 + sfc_spr_d_95 +
#                 sfc_sum_d_05 + sfc_sum_b_50 + sfc_sum_g_95 +
#                 sfc_win_b_95 + sfc_win_b_95 + sfc_win_b_95,
#               data = brt_dat,
#               distribution = "multinomial",
#               n.trees = 1000,
#               shrinkage = 0.05,
#               interaction.depth = 1,
#               bag.fraction = 0.5,
#               train.fraction = 0.5,
#               n.minobsinnode = 10,
#               cv.folds = 5,
#               keep.data = T, verbose = T, n.cores = 1)
# 
# # diagnose
# best_brt_all <- gbm.perf(fm_brt_all, method = "cv")
# 
# summary(fm_brt_all, n.trees = best_brt_all)
# print(pretty.gbm.tree(fm_brt_all,fm_brt_all$n.trees))
# 
# # predict
# preds <- as.data.frame(predict.gbm(fm_brt_all, brt_dat, best_brt_all, type="response"))
# names(preds) <- tools::file_path_sans_ext(names(preds))
# 
# # hard classify
# classified_brt_all <- names(preds)[HardClust(preds)]
# 
# # test accuracy
# sum(classified_brt_all == brt_dat$veg_cl_tm) / nrow(brt_dat)
# 
# # OK, seems to work as expected



# e.g. of a CV framework --------------------------------------------------

# note that we're not using BRTs internal CV, since that might bias the process of assessing accuracy via CV
# BRT is just used here as an example classifier, this exercise is to test the "independent" sample idea 
  # comparing doing the split -> classifiy -> test workflow once or multiple times

#==> still need to add in error handling, in case a fit fails
#==> and a better stratergy to handle the memory leak in gbm!

nboot <- 200

sample10_split20 <- numeric(nboot)
sample10_split30 <- numeric(nboot)
sample10_split40 <- numeric(nboot)
sample10_split50 <- numeric(nboot)
sample10_split60 <- numeric(nboot)
sample10_split70 <- numeric(nboot)
sample10_split80 <- numeric(nboot)
sample10_split90 <- numeric(nboot)

for (i in 1:nboot) {
  sample10_split20[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.2)
  sample10_split30[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.3)
  sample10_split40[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.4)
  sample10_split50[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.5)
  sample10_split60[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.6)
  sample10_split70[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.7)
  sample10_split80[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.8)
  sample10_split90[i] <- fit_split_brt(i, survey_points_veg, 0.1, 0.9)
}

plot_dat <- gather(
  data.frame(split20 = sample10_split20,
             split30 = sample10_split30,
             split40 = sample10_split40,
             split50 = sample10_split50,
             split60 = sample10_split60,
             split70 = sample10_split70,
             split80 = sample10_split80,
             split90 = sample10_split90))

save(plot_dat, file="plot_dat_200boots.RData")

ggplot(data = plot_dat, aes(y=value)) + 
  geom_boxplot(aes(x = key)) +
  xlab("Percentage of data used for training") + ylab("Test data accuracy (% agreement)") +
  theme_bw()














