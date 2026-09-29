#Code to conduct INGENIOUS prediction
#using a compilation of evidence layers
#Written by Anna Ortega, employee of SMK GeoSciences
#Last revised on September 28, 2026

#mike comment
#Load required packages
library(corrplot)
library(mapview)
library(sf)
library(terra)
library(tidyverse)
library(xgboost)

#Clean working environment
rm(list = ls())

#Create color ramp for plotting
cols <- colorRampPalette(c(
  "#000080",
  "#0000FF",
  "#00BFFF",
  "#00FFFF",
  "#00FF80",
  "#00FF00",
  "#FFFF00",
  "#FFA500",
  "#FF0000",
  "#800000"
))(100)
## Mike Comment
#Set projection
proj <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs"

#### IMPORT ALL SHAPEFILES AND EVIDENCE LAYERS ####

#Study area:
setwd("F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV")
study_area <- st_read("GB_study_area.shp")
mapview(study_area, map.types = "OpenStreetMap")

#State boundaries:
setwd("F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/State_Boundaries")
state <- st_read("tl_2025_us_state.shp")

#Conductive heat flow (DeAngelo et al. 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/DeAngelo_HeatFlow_Data/outputs"
)
hf <- rast("USGS_gbHeatFlowMap_qcWts_UTM.tif")

#Surface conductance (Peacock and Bedrosian 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Peacock_and_Bedrosian_Electrical_Conductance/gb_conductance_surface_tp"
)
srfc_crust <- rast("gb_conductance_surface_tp_UTM.tif") #Surface conductance

#Lower-crust conductance (Peacock and Bedrosian 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Peacock_and_Bedrosian_Electrical_Conductance/gb_conductance_lower_crust_tp"
)
lwr_crust <- rast("gb_conductance_lower_crust_tp_UTM.tif")

#Mantle conductance (Peacock and Bedrosian 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Peacock_and_Bedrosian_Electrical_Conductance/gb_conductance_mantle_tp"
)
mantle <- rast("gb_conductance_mantle_tp_UTM.tif")

#Middle-crust conductance (Peacock and Bedrosian 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Peacock_and_Bedrosian_Electrical_Conductance/gb_conductance_middle_crust_tp"
)
mid_crust <- rast("gb_conductance_middle_crust_tp_UTM.tif")

#Upper-mantle conductance (Peacock and Bedrosian 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Peacock_and_Bedrosian_Electrical_Conductance/gb_conductance_upper_mantle_tp"
)
upr_mantel <- rast("gb_conductance_upper_mantle_tp_UTM.tif")

#Magnetic field data (Glen et al. 2022):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Glen_Magnetic_Field_Data"
)
mag <- rast("GB_mag_anom_UTM.tif")

#Isostatic gravity (Glen et al. 2022):
iso <- rast("GB_iso_grav_anom_UTM.tif")

#Depth to basement (Glen et al. 2022):
depth_base <- rast("GB_depth_to_basement_surface_UTM.tif")

#Independent seismic density (Kreemer and Young 2023):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Kreemer_and_Young_Seismic_Density/independent_eq/geotiffs"
)
ind_seis <- rast("ieq_n200a05_UTM.tif") #n = 200, alpha = 0.05

#Dependent seismic density (Kreemer and Young 2023):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Kreemer_and_Young_Seismic_Density/dependent_eq/geotiffs"
)
dep_seis <- rast("deq_n200a05_UTM.tif") #n = 200, alpha = 0.05

#Shear strain rate (Kreemer and Young 2023):
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Kreemer_and_Young_Strain_Rates"
)
str_rate <- rast("geod_shearrate_UTM.tif")

#Dilation strain rate (Kreemer and Young 2023):
dil_str_rate <- rast("geod_dilatrate_UTM.tif")

#Second invariant of strain (Kreemer and Young 2023):
sec_inv_str <- rast("geod_2ndinv_UTM.tif")

# #Quaternary faults (Ayling et al.):
# setwd("F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Ayling_et_al_Quarternary_Faults")
# faults<-st_read("qfaults_ingenious_nad83conus117_2023-06-27.shp")
# mapview(faults,map.types="OpenStreetMap")
# faults<-st_transform(faults,st_crs(study_area))

#Import distance to nearest Quaternary fault:
#Note, I created distance rasters in QGIS
#with the surface conductance layer as the extent layer
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/Ayling_et_al_Quarternary_Faults"
)
dist_to_quart <- rast("dist_to_quart_faults_m.tif")

#Import distance to nearest vent:
dist_to_vent <- rast("dist_to_gb_vents_m.tif")

#### PREPARE EVIDENCE LAYERS FOR ANALYSES ####

#Make sure all evidence layers are in the same CRS
st_crs(hf) == st_crs(study_area)
st_crs(srfc_crust) == st_crs(study_area)
st_crs(lwr_crust) == st_crs(study_area)
st_crs(mantle) == st_crs(study_area)
st_crs(mid_crust) == st_crs(study_area)
st_crs(upr_mantel) == st_crs(study_area)
st_crs(mag) == st_crs(study_area)
st_crs(iso) == st_crs(study_area)
st_crs(depth_base) == st_crs(study_area)
st_crs(ind_seis) == st_crs(study_area)
st_crs(dep_seis) == st_crs(study_area)
st_crs(str_rate) == st_crs(study_area)
st_crs(dil_str_rate) == st_crs(study_area)
st_crs(sec_inv_str) == st_crs(study_area)
st_crs(dist_to_quart) == st_crs(study_area)
st_crs(dist_to_vent) == st_crs(study_area)

#Clip all evidence layers to study area
hf <- mask(crop(hf, study_area), study_area)
srfc_crust <- mask(crop(srfc_crust, study_area), study_area)
lwr_crust <- mask(crop(lwr_crust, study_area), study_area)
mantle <- mask(crop(mantle, study_area), study_area)
mid_crust <- mask(crop(mid_crust, study_area), study_area)
upr_mantel <- mask(crop(upr_mantel, study_area), study_area)
mag <- mask(crop(mag, study_area), study_area)
iso <- mask(crop(iso, study_area), study_area)
depth_base <- mask(crop(depth_base, study_area), study_area)
ind_seis <- mask(crop(ind_seis, study_area), study_area)
dep_seis <- mask(crop(dep_seis, study_area), study_area)
str_rate <- mask(crop(str_rate, study_area), study_area)
dil_str_rate <- mask(crop(dil_str_rate, study_area), study_area)
sec_inv_str <- mask(crop(sec_inv_str, study_area), study_area)
dist_to_quart <- mask(crop(dist_to_quart, study_area), study_area)
dist_to_vent <- mask(crop(dist_to_vent, study_area), study_area)

#What resolutions are the data?
res(hf) #250m2
res(srfc_crust) #1km2
res(lwr_crust) #1km2
res(mantle) #1km2
res(mid_crust) #1km2
res(upr_mantel) #1km2
res(mag) #1km2
res(iso) #1km2
res(depth_base) #1km2
res(ind_seis) #500m2
res(dep_seis) #500m2
res(str_rate) #500m2
res(dil_str_rate) #500m2
res(sec_inv_str) #500m2
res(dist_to_quart) #250m2
res(dist_to_vent) #250m2

#Resample all rasters to have 250m2 resolution
#Create template raster with 250m2 resolution
r <- rast(ext = ext(study_area), resolution = 250, crs = crs(study_area))

hf <- resample(hf, r, method = "bilinear")
srfc_crust <- resample(srfc_crust, r, method = "bilinear")
lwr_crust <- resample(lwr_crust, r, method = "bilinear")
mantle <- resample(mantle, r, method = "bilinear")
mid_crust <- resample(mid_crust, r, method = "bilinear")
upr_mantel <- resample(upr_mantel, r, method = "bilinear")
mag <- resample(mag, r, method = "bilinear")
iso <- resample(iso, r, method = "bilinear")
depth_base <- resample(depth_base, r, method = "bilinear")
ind_seis <- resample(ind_seis, r, method = "bilinear")
dep_seis <- resample(dep_seis, r, method = "bilinear")
str_rate <- resample(str_rate, r, method = "bilinear")
dil_str_rate <- resample(dil_str_rate, r, method = "bilinear")
sec_inv_str <- resample(sec_inv_str, r, method = "bilinear")
dist_to_quart <- resample(dist_to_quart, r, method = "bilinear")
dist_to_vent <- resample(dist_to_vent, r, method = "bilinear")

#Double check that the resolutions are 250m2
res(hf)
res(srfc_crust)
res(lwr_crust)
res(mantle)
res(mid_crust)
res(upr_mantel)
res(mag)
res(iso)
res(depth_base)
res(ind_seis)
res(dep_seis)
res(str_rate)
res(dil_str_rate)
res(sec_inv_str)
res(dist_to_quart)
res(dist_to_vent)

#Plot rasters to verify data looks correct
plot(hf, col = cols, main = "Conductive Heat Flow")
plot(srfc_crust, col = cols, main = "Surface Conductance")
plot(lwr_crust, col = cols, main = "Lower-Crust Conductance")
plot(mantle, col = cols, main = "Mantle Conductance")
plot(mid_crust, col = cols, main = "Middle-Crust Conductance")
plot(upr_mantel, col = cols, main = "Upper-Mantle Conductance")
plot(mag, col = cols, main = "Magnetic Field")
plot(iso, col = cols, main = "Isostatic Gravity")
plot(depth_base, col = cols, main = "Depth to Basement")
plot(ind_seis, col = cols, main = "Independent Seismic Density")
plot(dep_seis, col = cols, main = "Dependent Seismic Density")
plot(str_rate, col = cols, main = "Shear Strain Rate")
plot(dil_str_rate, col = cols, main = "Dilation Strain Rate")
plot(sec_inv_str, col = cols, main = "Second Invariant of Strain")
plot(dist_to_quart, col = cols, main = "Distance to Fault")
plot(dist_to_vent, col = cols, main = "Distance to Magma")

#Combine all evidence layers into a single raster stack
compareGeom(
  hf,
  srfc_crust,
  lwr_crust,
  mantle,
  mid_crust,
  upr_mantel,
  mag,
  iso,
  depth_base,
  ind_seis,
  dep_seis,
  str_rate,
  dil_str_rate,
  sec_inv_str,
  dist_to_quart,
  dist_to_vent
)

data <- c(
  hf,
  srfc_crust,
  lwr_crust,
  mantle,
  mid_crust,
  upr_mantel,
  mag,
  iso,
  depth_base,
  ind_seis,
  dep_seis,
  str_rate,
  dil_str_rate,
  sec_inv_str,
  dist_to_quart,
  dist_to_vent
)

#Standardize each layer in the raster stack
data_scaled <- scale(data, center = TRUE, scale = TRUE)

#Verify standardization looks correct (mean ~ 0 and SD = 1)
global(data_scaled, c("mean", "sd"), na.rm = TRUE)

names(data_scaled) <- c(
  "Conductive Heat Flow",
  "Surface Cond.",
  "Lower-Crust Cond.",
  "Mantle Cond.",
  "Middle-Crust Cond.",
  "Upper-Mantle Cond.",
  "Magnetic Field",
  "Isostatic Gravity",
  "Depth to Basement",
  "Ind. Seismic Density",
  "Dep. Seismic Density",
  "Shear Strain Rate",
  "Dilation Strain Rate",
  "Second Invariant",
  "Distance to Fault",
  "Distance to Magma"
)

order <- c(
  "Second Invariant",
  "Shear Strain Rate",
  "Dilation Strain Rate",
  "Ind. Seismic Density",
  "Conductive Heat Flow",
  "Isostatic Gravity",
  "Depth to Basement",
  "Upper-Mantle Cond.",
  "Mantle Cond.",
  "Lower-Crust Cond.",
  "Middle-Crust Cond.",
  "Surface Cond.",
  "Distance to Fault",
  "Magnetic Field",
  "Distance to Magma",
  "Dep. Seismic Density"
)

#### IMPORT WELL DATA AND PREPARE FOR ANALYSES ####

#Well data with heat flow estimates:
setwd(
  "F:/SMK_Geosciences/Data/INGENIOUS_GreatBasin_NV/DeAngelo_HeatFlow_Data/outputs"
)
wells <- st_read("USGS_gbHeatFlowWells_wEstimates.shp")
wells <- st_transform(wells, st_crs(hf))
st_crs(wells) == st_crs(hf)
mapview(study_area, map.types = "OpenStreetMap") +
  mapview(wells, map.types = "OpenStreetMap")

#Clip well locations to extent of an evidence layer (i.e., study area)
wells <- st_intersection(wells, study_area)
range(wells$hfqc_resid, na.rm = TRUE) #Should be -91 - 11105 mW/m2

#Create ordinal bins and weights based on Mordensky et al. 2025 (Table 2)
wells$reference <- NA
wells$reference <- ifelse(
  wells$hfqc_resid >= -25 & wells$hfqc_resid <= 25,
  "Low",
  wells$reference
)
wells$reference <- ifelse(
  wells$hfqc_resid > 25 & wells$hfqc_resid <= 50,
  "Intermediate",
  wells$reference
)
wells$reference <- ifelse(
  wells$hfqc_resid > 50 & wells$hfqc_resid <= 325,
  "High",
  wells$reference
)
wells$reference <- ifelse(wells$hfqc_resid > 325, "Very High", wells$reference)

wells$ordinal_label <- NA
wells$ordinal_label <- ifelse(wells$reference == "Low", 0, wells$ordinal_label)
wells$ordinal_label <- ifelse(
  wells$reference == "Intermediate",
  NA,
  wells$ordinal_label
)
wells$ordinal_label <- ifelse(wells$reference == "High", 1, wells$ordinal_label)
wells$ordinal_label <- ifelse(
  wells$reference == "Very High",
  2,
  wells$ordinal_label
)

wells$weight <- NA
wells$weight <- ifelse(wells$reference == "Low", 1, wells$weight)
wells$weight <- ifelse(wells$reference == "Intermediate", NA, wells$weight)
wells$weight <- ifelse(wells$reference == "High", 2, wells$weight)
wells$weight <- ifelse(wells$reference == "Very High", 3, wells$weight)

#Do numbers match sample size in Table 2?
table(wells$ordinal_bin)
table(is.na(wells$ordinal_bin))

#Identify which cells have a high convective signal
high <- subset(wells, wells$hfqc_resid > 50)

#Calculate distance from every well to the nearest well with a high convective signal
nearest_high <- st_nearest_feature(wells, high)

wells$dist_high_m <- as.numeric(st_distance(
  wells,
  high[nearest_high, ],
  by_element = TRUE
))
head(wells)

#Remove low convective signals within 4 km of high convective signals
low <- subset(wells, wells$ordinal_bin == "Low")
to_remove <- subset(low, low$dist_high_m < 4000)

wells_final <- wells[!(wells$unique_id %in% to_remove$unique_id), ]
range(wells_final$hfqc_resid)

#### EVALUATE CORRELATIONS AMONG EVIDENCE LAYERS ####

#Calculate Pearson's correlation coefficients among all evidence layers
d <- as.data.frame(data_scaled, na.rm = FALSE)
cor_matrix <- cor(d, use = "pairwise.complete.obs", method = "pearson")
round(cor_matrix, 2)

#Convert data from wide to long format
cor_df <- as.data.frame(cor_matrix) %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r")

#Re-order evidence layers to match Mordensky et al.
cor_df <- cor_df %>%
  mutate(
    var1 = factor(var1, levels = order),
    var2 = factor(var2, levels = rev(order))
  )

#Make correlation plot

scaleFUN <- function(x) sprintf("%.2f", x)

a1 <- ggplot(cor_df, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scaleFUN(r)), size = 3.5) +
  scale_fill_gradient2(
    low = "#0000CC",
    mid = "white",
    high = "#990000",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Color by Correlation Value"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "right",
      title.theme = element_text(color = "black", angle = 270, hjust = 0.5),
      barheight = unit(14, "cm"),
      barwidth = unit(0.5, "cm")
    )
  ) +
  coord_equal() +
  ggtitle(
    "Pearson Correlation Coefficient of Data in Labels and Evidence Layers"
  )

b1 <- a1 +
  theme_bw() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

c1 <- b1 +
  theme(
    axis.text.x = element_text(
      size = 10,
      color = "black",
      angle = 90,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 12, color = "black", hjust = 0.5)
  )

d1 <- c1 +
  theme(
    plot.background = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 10, colour = "black"),
    legend.text = element_text(size = 10, colour = "black")
  )
d1

setwd("F:/SMK_Geosciences/Figures/INGENIOUS")
jpeg(
  "PearsonsCorrelations_EvidenceLayers.jpeg",
  width = 8,
  height = 8,
  units = 'in',
  res = 350
)

d1

dev.off()

#Clean working environment before running next iteration of loop
rm(list = ls()[!ls() %in% c("data_scaled", "wells")])

#### RUN PRELIMINARY XGBOOST MODEL ####

#Extract evidence layers to wells
evidence_vals <- terra::extract(data_scaled, vect(wells))

#Add evidence layer values back to well sf object
d <- bind_cols(wells, evidence_vals[, -1])
head(d)

#Create list of predictor variables
covar <- c(
  "Conductive Heat Flow",
  "Surface Cond.",
  "Lower-Crust Cond.",
  "Mantle Cond.",
  "Middle-Crust Cond.",
  "Upper-Mantle Cond.",
  "Magnetic Field",
  "Isostatic Gravity",
  "Depth to Basement",
  "Ind. Seismic Density",
  "Dep. Seismic Density",
  "Shear Strain Rate",
  "Dilation Strain Rate",
  "Second Invariant",
  "Distance to Fault",
  "Distance to Magma"
)

#Create back up of original dataframe
data_orig <- d

#Create new dataframe that drops geometry
df_all <- st_drop_geometry(data_orig)

#Make sure all extracted evidence layers are numeric
sapply(df_all[, covar], class)

#Remove intermediate convective signal from training dataset
df_model <- df_all[is.na(df_all$ordinal_label) == FALSE, ]
table(is.na(df_model$ordinal_label))

#Create matrix of predictor variables
x <- as.matrix(df_model[, covar])
class(x) #must be matrix/array
typeof(x) #must be double
storage.mode(x) #must be double

#Make sure ordinal response variable is numeric
y <- as.numeric(df_model$ordinal_label)
table(is.na(y))
nrow(x) == length(y) #Must be TRUE!

#Set up XGBoost parameters
params <- list(
  objective = "reg:squarederror", #Treats bins as an ordered numerical response
  eval_metric = "mae", #Average absolute error
  eta = 0.05, #Learning rate (smaller = more robust)
  max_depth = 5, #Depth of each decision tree (higher = more complicated)
  subsample = 0.8, #Percent of training observations (i.e., 80%)
  colsample_bytree = 0.8
) #Percent of each predictor variables

n_iterations <- 2

#Create dataframe to store metrics of model performance
results <- data.frame(
  iteration = 1:n_iterations,
  MAE = NA,
  RMSE = NA,
  best_iteration = NA
)

#Create matrix to store predictions for wells in testing dataset
test_predictions <- matrix(NA, nrow = nrow(df_model), ncol = n_iterations)

#Create empty list to store model outputs
models <- vector("list", n_iterations)

#Run Monte Carlo 80:20 train:test loop
set.seed(123)

for (i in seq_len(n_iterations)) {
  #Create training set with randomly drawn observations (80% of dataset)
  train <- as.numeric(sample(
    seq_len(nrow(x)),
    size = floor(0.80 * nrow(x)),
    replace = FALSE
  ))

  #Create testing set with the remaining 20% of observations
  test <- as.numeric(setdiff(seq_len(nrow(x)), train))

  #Create DMatrix of training dataset for XGBoost
  d_train <- xgb.DMatrix(
    data = x[train, , drop = FALSE],
    label = y[train],
    missing = NA
  )

  #Create DMatrix of testing dataset for XGBoost
  d_test <- xgb.DMatrix(
    data = x[test, , drop = FALSE],
    label = y[test],
    missing = NA
  )

  #Fit XGBoost model
  mod <- xgb.train(
    params = params,
    data = d_train,
    nrounds = 2000,
    watchlist = list(train = d_train, test = d_test),
    early_stopping_rounds = 50,
    verbose = 0
  )

  #Predict test observations using model output from XGBoost
  pred_test <- predict(mod, d_test)

  #Store predictions in their original rows
  test_predictions[test, i] <- pred_test

  #Store model outputs
  models[[i]] <- mod

  #Evaluate model performance
  results$MAE[i] <- mean(abs(y[test] - pred_test))
  results$RMSE[i] <- sqrt(mean((y[test] - pred_test)^2))
  results$best_iteration[i] <- mod$best_iteration

  #Print number of iterations completed
  if (i %% 100 == 0) {
    message("Completed ", i, " of ", n_iterations, "iterations")
  }
}

length(models)

sum(sapply(models, is.null))

#Evaluate model performance
mean(results$MAE)
sd(results$MAE)

mean(results$RMSE)
sd(results$RMSE)

median(results$best_iteration)

#Plot performance relative to realizations
ggplot(results, aes(x = MAE)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  labs(
    x = "Test MAE",
    y = "Number of realizations",
    title = "XGBoost Validation Performance"
  )

#Look at predictions for wells
#mean number of predictions should equal number of iterations * 0.20
df_model$pred_mean <- rowMeans(test_predictions, na.rm = TRUE)
df_model$pred_sd <- apply(test_predictions, 1, sd, na.rm = TRUE)
df_model$n_predictions <- rowSums(!is.na(test_predictions))
summary(df_model$n_predictions)
head(df_model)

#Create matrix for predicting all wells
x_all <- data.matrix(df_all[, covar, drop = FALSE])
storage.mode(x_all) <- "double"
x_all[!is.finite(x_all)] <- NA
identical(colnames(x_all), colnames(x))
d_all <- xgb.DMatrix(data = x_all, missing = NA)
well_predictions <- matrix(
  NA,
  nrow = nrow(df_all),
  ncol = n_iterations
)

for (i in seq_len(n_iterations)) {
  well_predictions[, i] <- predict(
    models[[i]],
    d_all
  )
}

data_orig$favorability <- rowMeans(
  well_predictions,
  na.rm = TRUE
)

data_orig$prediction_sd <- apply(
  well_predictions,
  1,
  sd,
  na.rm = TRUE
)

head(data_orig)

ggplot(data_orig) +
  geom_sf(aes(color = favorability), size = 1.5) +
  scale_color_gradientn(
    colours = c("darkgreen", "green", "yellow", "orange", "red", "darkred"),
    name = "Favorability"
  ) +
  theme_void() +
  ggtitle("Predicted Geothermal Favorability")


identical(names(data_scaled), covar)

xgb_fun <- function(model, data) {
  data <- as.matrix(data)

  storage.mode(data) <- "double"

  predict(
    model,
    data
  )
}

dir.create(
  "xgb_predictions",
  showWarnings = FALSE
)


for (i in seq_len(n_iterations)) {
  r <- terra::predict(
    data_scaled,
    models[[i]],
    fun = xgb_fun,
    na.rm = TRUE
  )

  writeRaster(
    r,
    filename = paste0(
      "F:/SMK_Geosciences/Outputs/PredictedFavorability/",
      sprintf("%04d", i),
      ".tif"
    ),
    overwrite = TRUE
  )

  if (i %% 100 == 0) {
    message(
      "Mapped ",
      i,
      " / ",
      n_iterations
    )
  }
}


pred_files <- list.files(
  "F:/SMK_Geosciences/Outputs/PredictedFavorability/",
  pattern = "\\.tif$",
  full.names = TRUE
)

pred_stack <- rast(pred_files)

favorability <- app(
  pred_stack,
  mean,
  na.rm = TRUE
)

names(favorability) <- "favorability"
