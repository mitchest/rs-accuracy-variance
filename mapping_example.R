
# Example scenario of a resampling fraemwork for making a map -------------

library(raster)
library(MASS)
library(data.table)
library(dplyr)

# note the function masking... 
# use dplyr::select or raster::select or MASS:select is there are issues (i've loaded dplyr last)



# load data ---------------------------------------------------------------

# load survey data
survey_points_raw <- read.csv("classification_data/observer_class_ads40.csv",
                              header = T, stringsAsFactors = F)

survey_points <- survey_points_raw %>%
  filter(veg_class %in% c("bt", "ew", "ttt", "wh"),
         study_area %in% c(12,10)) %>%
  select(study_area, veg_class, blue_mean:nir_mean) %>%
  mutate(veg_class = as.factor(veg_class),
         id = 1:nrow(.)) %>%
  na.omit()

rm(survey_points_raw)

# load image data
blue_band <- raster("classification_data/blue_mapeg.tif") # so we have dimentions/CRS to use
image_data <- data.frame(as.data.frame(raster("classification_data/blue_mapeg.tif")),
                         as.data.frame(raster("classification_data/green_mapeg.tif")),
                         as.data.frame(raster("classification_data/red_mapeg.tif")),
                         as.data.frame(raster("classification_data/nir_mapeg.tif")))
names(image_data) <- c("blue_mean","green_mean","red_mean","nir_mean") # so they match the modelling data names

# fill the few NA's (deal with them better for serious use)
backfill <- function(x) {
  x[which(is.na(x))] <- x[which(is.na(x))+1]
  x
}
forwardfill <- function(x) {
  x[which(is.na(x))] <- x[which(is.na(x))-1]
  x
}
image_data <- as.data.frame(lapply(image_data, backfill))
image_data <- as.data.frame(lapply(image_data, forwardfill))


# resampling model and map ------------------------------------------------

# this is a simplified function to demonstrate a resampling framework
# change the paramers directly in this function for different resampling designs
# for more serious use, substitue in functions from "allocation_functions.R" and "accuracy_functions.R"
# for even more serious use, investigate existing packages, such as sperrorest (https://cran.r-project.org/web/packages/sperrorest/index.html)
fit_and_map <- function(niter, data, image_data) {
  print(paste0("Fitting iteration ",niter))
  # get training sample (simple random sample, Monte Carlo cross-validation with 67:33 split ratio)
  train <- (data %>% select(id) %>% sample_frac(0.67))$id
  # get test from left overs
  test <- data$id[!data$id %in% train]
  # fit ML model to training data
  fm <- lda(veg_class ~ blue_mean + green_mean + red_mean + nir_mean,
            data = inner_join(data, data.frame(id=train), by="id"),
            prior = rep(1/length(unique(data$veg_class)), length(unique(data$veg_class))))
  # predict classes for test data
  test_preds <- predict(fm, newdata = inner_join(data, data.frame(id=test), by="id"))$class
  # get percentage agreement accuracy
  perc_agr <- sum(as.character(test_preds) == as.character(data$veg_class[test])) / length(test_preds)
  # predict classes for image data
  # this is possibly a very slow way to do raster predictions - investigate other options for serious use
  image_preds <- predict(fm, newdata = image_data)$class
  # # calculate conf matrix
  # conf_mat <- table(as.character(test_preds),
  #                   as.character(data$veg_class[test]))
  # match-up for conf_mat and single-run bootstrap
  test_true <- data.frame(test = as.character(test_preds),
                          true = as.character(data$veg_class[test]),
                          stringsAsFactors = F)
  # return a list of the accuracy and the predicitons and the confusion matrix counts
  list(perc_agr, image_preds, test_true)
}

# fit n iterations of the resmapling routine
mapping_runs <- lapply(1:10, fit_and_map, survey_points, image_data)
saveRDS(mapping_runs, file = "A:/1_UNSW/0_data/Dharawal_project/mapping_runs.rds")

mapping_runs <- readRDS("A:/1_UNSW/0_data/Dharawal_project/mapping_runs.rds")

# calculate resampling stats ----------------------------------------------

med_ci <- function(x) {
  med <- median(x)
  names(med) <- "median"
  ci <- quantile(x, c(0.025,0.975))
  round(c(med,ci)*100, 0)
}

# get vector of accuracy stats and get mean/intervals
accuracy_distribution <- unlist(lapply(mapping_runs, `[[`, 1))
med_ci(accuracy_distribution)

# get class areas and get mean/intervals
class_areas <- lapply(mapping_runs, `[[`, 2)
class_areas <- lapply(class_areas, table)

class_proportions <- lapply(class_areas, function(x){(x/sum(x))})
class_proportions <- list(
  Banksia = unlist(lapply(class_proportions, `[[`, 1)),
  Eucalypt = unlist(lapply(class_proportions, `[[`, 2)),
  Teatree = unlist(lapply(class_proportions, `[[`, 3)),
  Wetheath = unlist(lapply(class_proportions, `[[`, 4)))

lapply(class_proportions, med_ci)



# calculate simultaneous intervals ----------------------------------------

# As per Olofsson et al. 2014
# Olofsson, P., Foody, G. M., Herold, M., Stehman, S. V., Woodcock, C. E., & Wulder, M. A. (2014). Good practices for estimating area and assessing accuracy of land change. Remote Sensing of Environment, 148, 42-57.

### choose an iteration (close to median value?) to do simultaneous intervals on
eg_run <- 5 # run chosen for the simultaneous example
test_true <- mapping_runs[[eg_run]][[3]]
conf_mat <- table(test_true$test, test_true$true)
mapped_areas <- table(mapping_runs[[eg_run]][[2]])
mapped_areas_p <- as.numeric(mapped_areas / sum(mapped_areas))

# p_ij matrix
conf_mat_nij <- prop.table(conf_mat, 1)
conf_mat_pij <- matrix(mapped_areas_p, nrow = 4, ncol = 4) * conf_mat_nij

# overall accuracy
oa <- sum(diag(conf_mat_pij))
# ovarall accuracy variance
users <- diag(conf_mat_pij) / rowSums(conf_mat_pij)
oa_v <- sum(((mapped_areas_p^2 * users) * (1 - users)) / (rowSums(conf_mat) - 1))
# overall accuracy 95% CI
oa_95 <- 1.96 * sqrt(oa_v * oa)

# area estimates
areas_pk <- colSums(conf_mat_pij)
areas_est <- areas_pk * sum(mapped_areas)
# area estimate standard error
areas_std_err <- sqrt(
  colSums(mapped_areas_p^2 * 
            ((conf_mat_nij * (1 - conf_mat_nij)) / 
               (rowSums(conf_mat) - 1)))
  )
# areas 95% CI
areas_95 <- 1.96 * (areas_std_err * sum(mapped_areas))
areas_95_p <- areas_95 / sum(mapped_areas) # duh



# calculate single-run bootstrap intervals --------------------------------

# sensu Hess & Bay 1997
# Hess, G. R., & Bay, J. M. (1997). Generating confidence intervals for composition-based landscape indexes. Landscape Ecology, 12(5), 309-320.

sampled_accuracies <- function(test_true) {
  data <- test_true %>%
    sample_frac(1, replace = T) # this is the bootstrap
  conf_mat <- table(data$test, data$true)
  list(perc_agr = sum(diag(conf_mat)) / sum(conf_mat),
             user = diag(conf_mat) / rowSums(conf_mat))
}

singlerun_resample <- replicate(n = 1000,
                                expr = {sampled_accuracies(test_true)}, 
                                simplify = F)

singlerun_oa <- unlist(lapply(singlerun_resample, `[[`, 1))

#### not sure about this - needs more thought re theory, but should ~ do 
#########################################################################
singlerun_users_list <- lapply(singlerun_resample, `[[`, 2)
singlerun_users_df <- data.frame(
    Banksia = unlist(lapply(singlerun_users_list, `[[`, 1)),
    Eucalypt = unlist(lapply(singlerun_users_list, `[[`, 2)),
    Teatree = unlist(lapply(singlerun_users_list, `[[`, 3)),
    Wetheath = unlist(lapply(singlerun_users_list, `[[`, 4)))

singlerun_area_95_sim <- sqrt(
  colSums(mapped_areas_p^2 * 
            cov(singlerun_users_df))) * 1.96

singlerun_error_df <- 1 - singlerun_users_df
singlerun_area_95 <- (unlist(lapply(singlerun_error_df, quantile, 0.975)) - 
  unlist(lapply(singlerun_error_df, quantile, 0.025))) * mapped_areas / sum(mapped_areas) / 2

########################################################################


# compare estimates -------------------------------------------------------

## accuracy
# full resample
med_ci(accuracy_distribution)
# bootstrap single run
med_ci(singlerun_oa)
# simultaneous single run
round(c(estimate=oa, `2.5%`=oa-oa_95, `97.5%`=oa+oa_95)*100)
round(c(oa, oa_95)*100)
# count error matrix accuracy
round(sum(diag(conf_mat)) / sum(conf_mat) * 100)

## areas
# full resample
lapply(class_proportions, med_ci)
# bootstrap single run CIs
round(singlerun_area_95*100)
# simultaneous single run
data.frame(areas_est = areas_pk, `CI_5` = areas_pk-areas_95_p, `CI_95` = areas_pk+areas_95_p)
round(areas_pk*100)
round(areas_95_p*100)
# mapped pixel count areas
round(mapped_areas_p*100)


# make maps ---------------------------------------------------------------

# get data frame of map predicitons
map_distribution <- as.data.frame(lapply(mapping_runs, `[[`, 2))
# check levels so we know what the final raster values equate to
levels(map_distribution[,1])
# convert to integer
map_distribution <- as.data.frame(lapply(map_distribution, as.integer))
names(map_distribution) <- NULL

# calculate the mode/count for the output map
# make a function for finding the mode
Mode <- function(x) {
  ux <- unique(x)
  uxt <- tabulate(match(x, ux))
  return(list(mode = ux[which.max(uxt)],
              n = max(uxt)))
}
map_mode <- apply(map_distribution, 1, Mode)

# prepare the mode matrix and make into a raster
mode_matrix <- matrix(unlist(lapply(map_mode, `[[`, 1)), nrow = nrow(blue_band), ncol = ncol(blue_band), byrow = T)
final_mode_map <- raster(mode_matrix)
extent(final_mode_map) = extent(blue_band)
crs(final_mode_map) = crs(blue_band)
writeRaster(final_mode_map, filename = "plots/final_mode_map.tif", overwrite = T)

# prepare the confidence matrix and make into a raster
mode_count_matrix <- matrix(unlist(lapply(map_mode, `[[`, 2)), nrow = nrow(blue_band), ncol = ncol(blue_band), byrow = T)
mode_count_matrix <- mode_count_matrix / length(mapping_runs)
final_confidence_map <- raster(mode_count_matrix)
extent(final_confidence_map) = extent(blue_band)
crs(final_confidence_map) = crs(blue_band)
writeRaster(final_confidence_map, filename = "plots/final_confidence_map.tif", overwrite = T)

# prepare a single run matrix and make into a raster
singlerun_matrix <- matrix(map_distribution[[1]], nrow = nrow(blue_band), ncol = ncol(blue_band), byrow = T)
singlerun_map <- raster(singlerun_matrix)
extent(singlerun_map) = extent(blue_band)
crs(singlerun_map) = crs(blue_band)
writeRaster(singlerun_map, filename = "plots/singlerun_map.tif", overwrite = T)












