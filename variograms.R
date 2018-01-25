library(dplyr)
#library(data.table)
library(gstat)
library(sp)
library(nabor)



# functions to do the repeated variograms ---------------------------------

ecologist_sample <- function(data, samp_frac, stratified = F) {
  if (stratified) {
    sample_out <- data %>%
      group_by(veg_class, study_area) %>%
      sample_frac(samp_frac)
    as.data.frame(sample_out)
  } else {
    sample_out <- data %>%
      group_by(study_area) %>%
      sample_frac(samp_frac)
    as.data.frame(sample_out)
  }
}

variogram_subsamples <- function(veg.type, data) {
  sub_sample <- ecologist_sample(data = data, samp_frac = 0.1)
  coordinates(sub_sample) = ~ east + north
  if (veg.type == "ew") {
    vario <- variogram(ew ~ 1, data = sub_sample, width = 5, cutoff = 300, cloud = F)
  }
  if (veg.type == "ttt") {
    vario <- variogram(ttt ~ 1, data = sub_sample, width = 5, cutoff = 300, cloud = F)
  }
  if (veg.type == "wh") {
    vario <- variogram(wh ~ 1, data = sub_sample, width = 5, cutoff = 300, cloud = F)
  }
  if (veg.type == "bt") {
    vario <- variogram(bt ~ 1, data = sub_sample, width = 5, cutoff = 300, cloud = F)
  }
  variofit <- fit.variogram(vario, vgm("Sph", "Exp", "Gau"), fit.ranges = F)
  list(dist = vario$dist, gamma = vario$gamma, psill = variofit$psill[2])
}

plot_range <- function(gamma, dist, psill) {
  min(dist[gamma > psill*0.95])
}

point_distances <- function(vegclass, data) {
  dists <- list()
  for (i in 1:100) {
    sub_sample <- ecologist_sample(data = data, samp_frac = 0.1) %>%
      filter(veg_class == vegclass)
    coordinates(sub_sample) = ~ east + north
    dists[[i]] <- knn(coordinates(sub_sample), coordinates(sub_sample), k=2)$nn.dists[,2]
  }
  dists <- unlist(dists)
  hist(dists, xlim = c(0,500), breaks = seq(0,999999,25), xaxt = 'n', xlab = "Distance between points (m)", main = "")
  axis(side = 1, at = seq(0,500,50), labels = seq(0,500,50))
  print(mean)
}



# variograms for subsamples -----------------------------------------------

point_data <- read.csv("classification_data/observer_class_ads40.csv",
                       header = T, stringsAsFactors = F)
# variograms
classes_binary <- point_data %>%
  filter(veg_class %in% c("bt", "ew", "ttt", "wh")) %>%
  select(east, north, veg_class, study_area) %>%
  mutate(ew = ifelse(veg_class == "ew", 1, 0),
         ttt = ifelse(veg_class == "ttt", 1, 0),
         wh = ifelse(veg_class == "wh", 1, 0),
         bt = ifelse(veg_class == "bt", 1, 0))

ew_varios <- replicate(100, variogram_subsamples("ew", classes_binary), simplify = F)
ew_dist <- apply(as.data.frame(lapply(ew_varios, `[[`, 1)), 1, mean)
ew_gamma <- apply(as.data.frame(lapply(ew_varios, `[[`, 2)), 1, mean)
ew_psill <- mean(unlist(lapply(ew_varios, `[[`, 3)))
plot(ew_gamma ~ ew_dist, pch = 19, col = "red", type = 'l', ylim = c(0, 0.3),
     main = "Eucalypt Woodland", ylab = "semivariance", xlab = "distance (m)")
abline(v = plot_range(ew_gamma, ew_dist, ew_psill))

wh_varios <- replicate(100, variogram_subsamples("wh", classes_binary), simplify = F)
wh_dist <- apply(as.data.frame(lapply(wh_varios, `[[`, 1)), 1, mean)
wh_gamma <- apply(as.data.frame(lapply(wh_varios, `[[`, 2)), 1, mean)
wh_psill <- mean(unlist(lapply(wh_varios, `[[`, 3)))

ttt_varios <- replicate(100, variogram_subsamples("ttt", classes_binary), simplify = F)
ttt_dist <- apply(as.data.frame(lapply(ttt_varios, `[[`, 1)), 1, mean)
ttt_gamma <- apply(as.data.frame(lapply(ttt_varios, `[[`, 2)), 1, mean)
ttt_psill <- mean(unlist(lapply(ttt_varios, `[[`, 3)))

bt_varios <- replicate(100, variogram_subsamples("bt", classes_binary), simplify = F)
bt_dist <- apply(as.data.frame(lapply(bt_varios, `[[`, 1)), 1, mean)
bt_gamma <- apply(as.data.frame(lapply(bt_varios, `[[`, 2)), 1, mean)
bt_psill <- mean(unlist(lapply(bt_varios, `[[`, 3)))

# plot
par(mfcol=c(4,2))

plot(ew_gamma ~ ew_dist, pch = 19, col = "red", type = 'l', ylim = c(0, 0.3),
     main = "Eucalypt Woodland", ylab = "semivariance", xlab = "distance (m)")
abline(v = plot_range(ew_gamma, ew_dist, ew_psill))
plot(wh_gamma ~ wh_dist, pch = 19, col = "red", type = 'l', ylim = c(0, 0.3),
     main = "Wet Heath", ylab = "semivariance", xlab = "distance (m)")
abline(v = plot_range(wh_gamma, wh_dist, wh_psill))
plot(ttt_gamma ~ ttt_dist, pch = 19, col = "red", type = 'l', ylim = c(0, 0.3),
     main = "Tea Tree Thicket", ylab = "semivariance", xlab = "distance (m)")
abline(v = plot_range(ttt_gamma, ttt_dist, ttt_psill))
plot(bt_gamma ~ bt_dist, pch = 19, col = "red", type = 'l', ylim = c(0, 0.3),
     main = "Banksia Thicket", ylab = "semivariance", xlab = "distance (m)")
abline(v = plot_range(bt_gamma, bt_dist, bt_psill))

# add distances between points
invisible(lapply(c("ew", "wh", "ttt", "bt"), point_distances, point_data))



