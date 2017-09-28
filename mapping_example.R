
# Example scenario of a resampling fraemwork for making a map -------------

library(raster)
library(MASS)
library(dplyr)

# note the function masking... 
# use dplyr::select or raster::select or MASS:select is there are issues (i've loaded dplyr last)



# load data ---------------------------------------------------------------

# load survey data
survey_points_raw <- read.csv("classification_data/dharawal_points_ads40.csv",
                              header = T, stringsAsFactors = F)

survey_points <- survey_points_raw %>%
  filter(veg_cl_tm %in% c("bt", "ew", "ttt", "wh"),
         studyarea_ %in% c(12,10)) %>%
  select(studyarea_, veg_cl_tm, blue_mean:nir_mean) %>%
  mutate(veg_cl_tm = as.factor(veg_cl_tm),
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
# for more serious use, substitue in functions from "allocation_functions.R" and accuracy_functions.R"
# for even more serious use, investigate existing packages, such as sperrorest (https://cran.r-project.org/web/packages/sperrorest/index.html)
fit_and_map <- function(niter, data, image_data) {
  print(paste0("Fitting iteration",niter))
  # get training sample (mccv 67:33)
  train <- (data %>% select(id) %>% sample_frac(0.67))$id
  # get test from left overs
  test <- data$id[!data$id %in% train]
  # fit ML model to training data
  fm <- lda(veg_cl_tm ~ blue_mean + green_mean + red_mean + nir_mean,
            data = inner_join(data, data.frame(id=train), by="id"),
            prior = rep(1/length(unique(data$veg_cl_tm)), length(unique(data$veg_cl_tm))))
  # predict classes for test data
  test_preds <- predict(fm, newdata = inner_join(data, data.frame(id=test), by="id"))$class
  # get percentage agreement accuracy
  perc_agr <- sum(as.character(test_preds) == as.character(data$veg_cl_tm[test])) / length(test_preds)
  # predict classes for image data
  # this is possibly a very slow way to do raster predictions - investigate other options for serious use
  image_preds <- predict(fm, newdata = image_data)$class
  # return a list of the accuracy and the predicitons
  return(list(perc_agr, image_preds))
}

# fit n iterations of the resmapling routine
mapping_runs <- lapply(1:800, fit_and_map, survey_points, image_data)



# extract stats -----------------------------------------------------------

# get vector of accuracy stats and get mean/intervals
accuracy_distribution <- unlist(lapply(mapping_runs, `[[`, 1))
# calculate stats
median(accuracy_distribution); quantile(accuracy_distribution, c(0.05,0.95))

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












