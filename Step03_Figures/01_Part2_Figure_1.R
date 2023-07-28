library(raster)
library(rasterVis)
library(PNWColors)
library(ggplot2)
library(basemaps)
library(sf)
library(ggmap)
library(vegan)
library(readxl)
library(rje)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)

# TO DO
# fix loop for output raster calc


########################################
###### Collection Data #################
########################################

master_df<-read.csv(paste0(my_working_dir, '/Data/MASTER_PREDICTOR_TABLE.csv'))
master_df$month_pred<- master_df$Month

# One row per site
filtered_df <- master_df[!duplicated(master_df$Site), ]

# De-scaling (get descaling coefficients)
sd.elevation <- sd(master_df$elevation)
mean.elevation <- mean(master_df$elevation)

sd.temp <- sd(master_df$Te_mean)
mean.temp <- mean(master_df$Te_mean)

sd.temp.diurnal <- sd(master_df$Mean_Diurnal_Range_C)
mean.temp.diurnal <- mean(master_df$Mean_Diurnal_Range_C)

sd.ocean <- sd(log10(master_df$ocean_effect+1))
mean.ocean <- mean(log10(master_df$ocean_effect+1))

sd.value.025km <- sd(log10(master_df$LV_025km+1), na.rm = TRUE)
mean.value.025km <- mean(log10(master_df$LV_025km+1), na.rm = TRUE)
sd.value.05km <- sd(log10(master_df$LV_05km+1), na.rm = TRUE)
mean.value.05km <- mean(log10(master_df$LV_05km+1), na.rm = TRUE)
sd.value.1km <- sd(log10(master_df$LV_1km+1), na.rm = TRUE)
mean.value.1km <- mean(log10(master_df$LV_1km+1), na.rm = TRUE)

sd.imperv.025km <- sd(master_df$imperv_025km)
mean.imperv.025km <- mean(master_df$imperv_025km)
sd.imperv.05km <- sd(master_df$imperv_05km)
mean.imperv.05km <- mean(master_df$imperv_05km)
sd.imperv.1km <- sd(master_df$imperv_1km)
mean.imperv.1km <- mean(master_df$imperv_1km)

sd.natural <- sd(log10(master_df$distance_natural+1), na.rm = TRUE)
mean.natural <- mean(log10(master_df$distance_natural+1), na.rm = TRUE)



########################################
###### Raw Rasters #####################
########################################

raster_files <- c("Data/spatial_data/landvalue.tif",
                  "Data/spatial_data/impervious.tif",
                  "Data/spatial_data/ocean_effects.tif",
                  "Data/spatial_data/bio01.tif", 
                  "Data/spatial_data/bio02.tif", 
                  "Data/spatial_data/elevation.tif",
                  "Data/spatial_data/distancenatural.tif"
)


# Read the first raster to serve as the reference
ref_raster <- raster(raster_files[1])

# Resample the remaining rasters to match the resolution and extent of the reference raster
raster_stack <- stack()
for (i in 1:length(raster_files)) {
  raster <- raster(raster_files[i])
  raster <- resample(raster, ref_raster, method = "bilinear")
  raster_stack <- addLayer(raster_stack, raster)
}

# Filter unreasonable values // NAN values // scale accordingly
raster_stack[[2]] <- reclassify(raster_stack[[2]], cbind(lower = 100, upper = 200, newval = NA))
raster_stack[[4]] <- reclassify(raster_stack[[4]], cbind(lower = -1e40, upper = 0, newval = NA))
raster_stack[[4]] <- (raster_stack[[4]]/10)
raster_stack[[5]] <- reclassify(raster_stack[[5]], cbind(lower = -1e40, upper = 0, newval = NA))
raster_stack[[5]] <- (raster_stack[[5]]/10)

raster_stack[[3]] <- reclassify(raster_stack[[3]], cbind(lower = 1, upper = 200, newval = NA)) # filter ocean effects -> if too far, remove


# If any raster is nan, make all nan at that cell
raster_stack <-
  raster::calc(
    raster_stack,
    fun = function(x)
      if (sum(is.na(x)) > 0)
        x * NA
    else
      x
  )


# plot raw values
plot(raster_stack)


########################################
###### Scale Rasters ###################
########################################
scaled_raster_stack <- stack()

# Get site coords
sites <- read_excel("Data_Upon_Request/bioscan.xlsx", sheet= "Annual Combined All")
site.points <- SpatialPoints(sites[, c("Longitude", "Latitude")])
site.points.df <- sites[, c("Longitude", "Latitude", "Phase")] %>%
  dplyr::mutate(Phase = as.character(Phase))

# Land value
extracted_values <- extract(raster_stack[[1]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$LV_025km, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$LV_025km, na.rm=TRUE)

scaled_raster_stack <- addLayer(scaled_raster_stack, ((log10(raster_stack[[1]]+1) - mean.value.025km)/sd.value.025km))

hist(((log10((raster_stack[[1]]/6)+1) - mean.value.025km)/sd.value.025km))
hist(scale(log10(filtered_df$LV_025km+1)), breaks=20)

extracted_values <- extract(raster_stack[[1]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$LV_05km, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$LV_05km, na.rm=TRUE)

scaled_raster_stack <- addLayer(scaled_raster_stack, ((log10(raster_stack[[1]]+1) - mean.value.05km)/sd.value.05km))

extracted_values <- extract(raster_stack[[1]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$LV_1km, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$LV_1km, na.rm=TRUE)

scaled_raster_stack <- addLayer(scaled_raster_stack, ((log10(raster_stack[[1]]+1) - mean.value.1km)/sd.value.1km))

# Impervious cover
scaled_raster_stack <- addLayer(scaled_raster_stack, ((raster_stack[[2]] - mean.imperv.025km)/sd.imperv.025km))
scaled_raster_stack <- addLayer(scaled_raster_stack, ((raster_stack[[2]] - mean.imperv.05km)/sd.imperv.05km))
scaled_raster_stack <- addLayer(scaled_raster_stack, ((raster_stack[[2]] - mean.imperv.1km)/sd.imperv.1km))

# ocean effects
scaled_raster_stack <- addLayer(scaled_raster_stack, ((log10(raster_stack[[3]]+1) - mean.ocean)/sd.ocean))

# mean annual temp
extracted_values <- extract(raster_stack[[4]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$Te_mean, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$Te_mean, na.rm=TRUE)

scaled_raster_stack <- addLayer(scaled_raster_stack, ((raster_stack[[4]] - mean.temp + (measured_values_mean - extracted_values_mean))/(sd.temp/(measured_values_sd/extracted_values_sd))))
# scaled_raster_stack <- addLayer(scaled_raster_stack, (scale(raster_stack[[4]])))

# diurnal temp
extracted_values <- extract(raster_stack[[5]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$Mean_Diurnal_Range_C, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$Mean_Diurnal_Range_C, na.rm=TRUE)

scaled_raster_stack <- addLayer(scaled_raster_stack, ((raster_stack[[5]] - mean.temp.diurnal + (measured_values_mean - extracted_values_mean))/(sd.temp.diurnal/(measured_values_sd/extracted_values_sd))))
# scaled_raster_stack <- addLayer(scaled_raster_stack, (scale(raster_stack[[5]])))

# elevation
scaled_raster_stack <- addLayer(scaled_raster_stack, ((raster_stack[[6]] - mean.elevation)/sd.elevation))

# distance_natural
scaled_raster_stack <- addLayer(scaled_raster_stack, ((log10(raster_stack[[7]]+1) - mean.natural)/sd.natural))



# Define the function to filter values
# filter_negative <- function(x) {
#   x[x < -3] <- NaN
#   return(x)
# }
# 
# # Define the function to filter values
# filter_positive <- function(x) {
#   x[x > 3] <- NaN
#   return(x)
# }

# Apply the function to each layer of the raster stack
scaled_raster_stack[[1]] <- reclassify(scaled_raster_stack[[1]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[2]] <- reclassify(scaled_raster_stack[[2]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[3]] <- reclassify(scaled_raster_stack[[3]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[4]] <- reclassify(scaled_raster_stack[[4]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[5]] <- reclassify(scaled_raster_stack[[5]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[6]] <- reclassify(scaled_raster_stack[[6]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[7]] <- reclassify(scaled_raster_stack[[7]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[8]] <- reclassify(scaled_raster_stack[[8]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[9]] <- reclassify(scaled_raster_stack[[9]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[10]] <- reclassify(scaled_raster_stack[[10]], cbind(lower = -200, upper = -3, newval = NA))
scaled_raster_stack[[11]] <- reclassify(scaled_raster_stack[[11]], cbind(lower = -200, upper = -3, newval = NA))

scaled_raster_stack[[1]] <- reclassify(scaled_raster_stack[[1]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[2]] <- reclassify(scaled_raster_stack[[2]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[3]] <- reclassify(scaled_raster_stack[[3]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[4]] <- reclassify(scaled_raster_stack[[4]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[5]] <- reclassify(scaled_raster_stack[[5]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[6]] <- reclassify(scaled_raster_stack[[6]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[7]] <- reclassify(scaled_raster_stack[[7]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[8]] <- reclassify(scaled_raster_stack[[8]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[9]] <- reclassify(scaled_raster_stack[[9]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[10]] <- reclassify(scaled_raster_stack[[10]], cbind(lower = 3, upper = 200, newval = NA))
scaled_raster_stack[[11]] <- reclassify(scaled_raster_stack[[11]], cbind(lower = 3, upper = 200, newval = NA))


# If any raster is nan, make all nan at that cell
scaled_raster_stack <-
  raster::calc(
    scaled_raster_stack,
    fun = function(x)
      if (sum(is.na(x)) > 0)
        x * NA
    else
      x
  )



# plot scaled values
plot(scaled_raster_stack)


########################################
###### Output Values and Aggregation ###
########################################

# year range constant
year_range=c(14,18)

## check main trends function
get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

groups_and_models <- list(
  # Lepidoptera = c('full_model_ind_spatial_all'),
  Phoridae = c('full_model_ind_spatial_all'),
  Syrphidae = c('full_model_ind_spatial_all'),
  Tipulomorpha = c('full_model_ind_spatial_all'),
  Drosophilidae = c('full_model_ind_spatial_all'),
  Mycetophilidae = c('full_model_ind_spatial_all'),
  Araneae = c('full_model_ind_spatial_all')
)

# raster group for storage
# sp.p.stack <- stack()
groups.025 <- list()
groups.50 <- list()
groups.975 <- list()

# get all combos of models and group to be run for
combinations <- data.frame()
for (group in names(groups_and_models)) {
  model <- groups_and_models[[group]]
  
  # Add group to lists
  groups.025[[group]] <- stack()
  groups.50[[group]] <- stack()
  groups.975[[group]] <- stack()
  
  ##Import your model outputs
  res <- readRDS(paste0("Outputs/res_", paste0(group, "_",model, ".rds")))
  res.summary <- readRDS(paste0("Outputs/res.summary_", paste0(group, "_",model, ".rds")))
  my.data <- readRDS(paste0("Data/clean_data/data_prepared/my_data_",  paste0(year_range, collapse = "_"), "_", group, "_spatial", ".rds"))
  
  mcmc_matrix <- as.matrix(res$mcmc[1][[1]])
  
  # vars <- rownames(res.summary$psrf$psrf)
  # summ <- as.data.frame(get.summ(vars))
  
  num_species <- sum(grepl("psi.sp", colnames(mcmc_matrix)))
  
  
  for (i in 1:num_species){
    # Empty raster stack to store all iterations for this species
    sp.p.stack <- stack()
    
    for (iter in 1:nrow(mcmc_matrix)) {
    
      iteration_values <- mcmc_matrix[iter,]
      group.intercept <- iteration_values['mu.psi.0']
    
      sp.intercept <- iteration_values[paste0('psi.sp[', i,']')]
      sp.temp <- iteration_values[paste0('psi.temp[', i,']')]
      sp.temp.diurnal <- iteration_values[paste0('psi.temp.diurnal[', i,']')] 
      sp.value <- iteration_values[paste0('psi.value[', i,']')] 
      sp.imperv <- iteration_values[paste0('psi.imperv[', i,']')]
      sp.ocean <- iteration_values[paste0('psi.ocean.effect[', i,']')]
      sp.elevation <- iteration_values[paste0('psi.elevation[', i,']')]
      sp.natural <- iteration_values[paste0('psi.distance.natural[', i,']')]
      
      # print(paste(sp.intercept, sp.temp, sp.temp.diurnal, sp.value, sp.imperv, sp.ocean, sp.elevation), sep=" ")
      
      
      # Define the linear model coefficients, scale dependent
      if (group %in% c("Phoridae", "Araneae")) {
        coef <- c(sp.value, 0, 0, sp.imperv, 0, 0, sp.ocean, sp.temp, sp.temp.diurnal, sp.elevation, sp.natural)
      } else if (group %in% c("Syrphidae", "Lepidoptera", "Tipulomorpha", "Drosophilidae", "Mycetophilidae")) {
        coef <- c(0, sp.value, 0, 0, sp.imperv, 0, sp.ocean, sp.temp, sp.temp.diurnal, sp.elevation, sp.natural)
      }
      
      # Create a function that calculates the new raster using the linear model
      linear_model <- function(x) {
        result <- sum(x * coef)
        return(result)
      }
      
      # Apply the linear model function to the raster stack
      sp.p <- (calc(scaled_raster_stack, linear_model) + sp.intercept + group.intercept)
      
      # Bind to 0-1 scale
      sp.p <- rje::expit(sp.p)
      
      # add raster to a raster stack for storage
      sp.p.stack <- addLayer(sp.p.stack, sp.p)
    }
    
    # get quantiles and store in raster stacks
    groups.025[[group]] <- addLayer(groups.025[[group]], calc(sp.p.stack, fun = function(x) quantile(x, probs = 0.025, na.rm=TRUE)))
    groups.50[[group]] <- addLayer(groups.50[[group]], calc(sp.p.stack, fun = function(x) quantile(x, probs = 0.5, na.rm=TRUE)))
    groups.975[[group]] <- addLayer(groups.975[[group]], calc(sp.p.stack, fun = function(x) quantile(x, probs = 0.975, na.rm=TRUE)))
  }
}


# Save outputs
saveRDS(groups.025, file = "Step03_Figures/figure_1_temp/groups_025.rds")
saveRDS(groups.50, file = "Step03_Figures/figure_1_temp/groups_50.rds")
saveRDS(groups.975, file = "Step03_Figures/figure_1_temp/groups_975.rds")



groups.025.sum <- stack()
groups.50.sum <- stack()
groups.975.sum <- stack()

for (group in names(groups.50)) {
  # GROUP LEVEL PLOT
  # plot species richness
  group.sp.richness <- sum(groups.50[[group]])
  
  # prep for total summary plot
  groups.025.sum <- addLayer(groups.025.sum, sum(groups.025[[group]]))
  groups.50.sum <- addLayer(groups.50.sum, sum(groups.50[[group]]))
  groups.975.sum <- addLayer(groups.975.sum, sum(groups.975[[group]]))
  
  # Turn into df for plotting
  richness_spdf <- as(group.sp.richness, "SpatialPixelsDataFrame")
  richness_df <- as.data.frame(richness_spdf)
  colnames(richness_df) <- c("Richness", "Longitude", "Latitude")

  # Define color palette
  pal <- pnw_palette("Bay", 35)

  # Which sites sampled for this group?
  if (group == "Lepidoptera") {
    group.sites <- site.points.df[site.points.df$Phase %in% c("1"), ]
  } else if (group == "Araneae") {
    group.sites <- site.points.df[site.points.df$Phase %in% c("2", "3"), ]
  } else if (group == "Mycetophilidae") {
    group.sites <- site.points.df[site.points.df$Phase %in% c("2", "3"), ]
  } else {
    group.sites <- site.points.df
  }

  # Define basemap
  bbox <- extent(group.sp.richness)
  sq_map1 <- get_stamenmap(bbox = c(left = bbox@xmin, bottom = bbox@ymin, right = bbox@xmax - 0.3, top = bbox@ymax), maptype = "toner-lite")

  ggmap(sq_map1) +
    geom_tile(data = richness_df, aes(x = Longitude, y = Latitude, fill = floor(Richness)), alpha = 0.8) +
    scale_fill_gradientn(colours = pal) +
    theme(panel.background = element_blank()) +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    labs(fill = paste0(group, " ", "Richness")) +
    xlab("Longitude") +
    ylab("Latitude")  +
    geom_point(data = group.sites, aes(x = Longitude, y = Latitude, shape = Phase), color = "black", size = 2.5) +
    scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 15))

  # Display the plot
  plot(last_plot())
}


########################################
###### Richness Map ####################
########################################

# plot species richness
sp.richness <- sum(groups.50.sum)

plot(sp.richness)


# Turn into df for plotting
richness_spdf <- as(sp.richness, "SpatialPixelsDataFrame")
richness_df <- as.data.frame(richness_spdf)
colnames(richness_df) <- c("Richness", "Longitude", "Latitude")

# Define color palette
pal <- pnw_palette("Bay", 35)



# Define basemap
bbox <- extent(sp.richness)
sq_map1 <- get_stamenmap(bbox = c(left = bbox@xmin, bottom = bbox@ymin, right = bbox@xmax - 0.3, top = bbox@ymax), maptype = "toner-lite")

ggmap(sq_map1) +
  geom_tile(data = richness_df, aes(x = Longitude, y = Latitude, fill = floor(Richness)), alpha = 0.8) +
  scale_fill_gradientn(colours = pal) + 
  theme(panel.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(fill = "Richness") + 
  xlab("Longitude") + 
  ylab("Latitude") +
  geom_point(data = site.points.df, aes(x = Longitude, y = Latitude, shape = Phase), color = "black", size = 2.5) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 15))

# Plot CI
lower <- sum(groups.025.sum)
upper <- sum(groups.975.sum)

CI.richness <- upper-lower

# Turn into df for plotting
richness_spdf <- as(CI.richness, "SpatialPixelsDataFrame")
richness_df <- as.data.frame(richness_spdf)
colnames(richness_df) <- c("Richness", "Longitude", "Latitude")

# Define color palette
pal <- pnw_palette("Bay", 35)

# Define basemap
bbox <- extent(sp.richness)
sq_map1 <- get_stamenmap(bbox = c(left = bbox@xmin, bottom = bbox@ymin, right = bbox@xmax - 0.3, top = bbox@ymax), maptype = "toner-lite")

ggmap(sq_map1) +
  geom_tile(data = richness_df, aes(x = Longitude, y = Latitude, fill = floor(Richness)), alpha = 0.8) +
  scale_fill_gradientn(colours = pal) + 
  theme(panel.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(fill = "Width of CI Richness") + 
  xlab("Longitude") + 
  ylab("Latitude") +
  geom_point(data = site.points.df, aes(x = Longitude, y = Latitude, shape = Phase), color = "black", size = 2.5) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 15))


########################################
###### Plot each variable ##############
########################################

rescaled_raster_stack <- stack()

# Get site coords
sites <- read_excel("Data_Upon_Request/bioscan.xlsx", sheet= "Annual Combined All")
site.points <- SpatialPoints(sites[, c("Longitude", "Latitude")])
site.points.df <- sites[, c("Longitude", "Latitude", "Phase")] %>%
  dplyr::mutate(Phase = as.character(Phase))

# Land value
extracted_values <- extract(raster_stack[[1]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$LV_025km, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$LV_025km, na.rm=TRUE)

rescaled_raster_stack <- addLayer(rescaled_raster_stack, 10**((((scaled_raster_stack[[1]])-1) * sd.value.025km) + mean.value.025km))

hist(((log10((raster_stack[[1]]/6)+1) - mean.value.025km)/sd.value.025km))
hist(scale(log10(filtered_df$LV_025km+1)), breaks=20)

extracted_values <- extract(raster_stack[[1]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$LV_05km, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$LV_05km, na.rm=TRUE)

rescaled_raster_stack <- addLayer(rescaled_raster_stack, 10**((((scaled_raster_stack[[2]])-1) * sd.value.05km) + mean.value.05km))

extracted_values <- extract(raster_stack[[1]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$LV_1km, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$LV_1km, na.rm=TRUE)

rescaled_raster_stack <- addLayer(rescaled_raster_stack, 10**((((scaled_raster_stack[[3]])-1) * sd.value.1km) + mean.value.1km))

# Impervious cover
rescaled_raster_stack <- addLayer(rescaled_raster_stack, ((scaled_raster_stack[[4]]*sd.imperv.025km + mean.imperv.025km)))
rescaled_raster_stack <- addLayer(rescaled_raster_stack, ((scaled_raster_stack[[5]]*sd.imperv.05km + mean.imperv.05km)))
rescaled_raster_stack <- addLayer(rescaled_raster_stack, ((scaled_raster_stack[[6]]*sd.imperv.1km + mean.imperv.1km)))

# ocean effects
rescaled_raster_stack <- addLayer(rescaled_raster_stack, 10**((((scaled_raster_stack[[7]])-1)*sd.ocean + mean.ocean)))

# mean annual temp
extracted_values <- extract(raster_stack[[4]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$Te_mean, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$Te_mean, na.rm=TRUE)

rescaled_raster_stack <- addLayer(rescaled_raster_stack, ((scaled_raster_stack[[8]]*(sd.temp/(measured_values_sd/extracted_values_sd)) + mean.temp - (measured_values_mean - extracted_values_mean))))
# scaled_raster_stack <- addLayer(scaled_raster_stack, (scale(raster_stack[[4]])))

# diurnal temp
extracted_values <- extract(raster_stack[[5]], site.points)
extracted_values_mean <- mean(extracted_values, na.rm=TRUE)
extracted_values_sd <- sd(extracted_values, na.rm=TRUE)
measured_values_mean <- mean(filtered_df$Mean_Diurnal_Range_C, na.rm=TRUE)
measured_values_sd <- sd(filtered_df$Mean_Diurnal_Range_C, na.rm=TRUE)

rescaled_raster_stack <- addLayer(rescaled_raster_stack, ((scaled_raster_stack[[9]]*(sd.temp.diurnal/(measured_values_sd/extracted_values_sd)) + mean.temp.diurnal - (measured_values_mean - extracted_values_mean))))
# scaled_raster_stack <- addLayer(scaled_raster_stack, (scale(raster_stack[[5]])))

# elevation
rescaled_raster_stack <- addLayer(rescaled_raster_stack, ((scaled_raster_stack[[10]]*sd.elevation + mean.elevation)))

# distance_natural
rescaled_raster_stack <- addLayer(rescaled_raster_stack, 10**((((scaled_raster_stack[[11]])-1)*sd.natural + mean.natural)))

# Get site values
site_df <- read.csv("Data/MASTER_PREDICTOR_TABLE.csv")
site_df <- site_df[!duplicated(site_df$Site), ]

sites <- read_excel("Data_Upon_Request/bioscan.xlsx", sheet= "Annual Combined All")
sites$Site <- as.integer(gsub("\\D", "", sites$Site))
site.points <- SpatialPoints(sites[, c("Longitude", "Latitude")])
site.points.df <- sites[, c("Longitude", "Latitude", "Phase", "Site")] %>%
  dplyr::mutate(Phase = as.character(Phase))

merged_sites <- merge(site.points.df, site_df, by = "Site", all = TRUE)

# Define color palette
pal <- pnw_palette("Sailboat", 35)


names <- c("Land Value 0.25 km ($/ha)", "Land Value 0.5 km ($/ha)", "Land Value 1 km ($/ha)", "Impervious Cover 0.25 km (%)", "Impervious Cover 0.5 km (%)", "Impervious Cover 1 km (%)", "Cost Distance to Ocean", "Mean Annual Temperature (C)", "Mean Diurnal Range (C)", "Elevation (m)", "Distance to Natural Area (m)")
for (layer in 1:nlayers(rescaled_raster_stack)) {
  layer <- 8
  # Turn into df for plotting
  layer_spdf <- as(rescaled_raster_stack[[layer]], "SpatialPixelsDataFrame")
  layer_df <- as.data.frame(layer_spdf)
  colnames(layer_df) <- c("data", "Longitude", "Latitude")
  
  # Define basemap
  bbox <- extent(rescaled_raster_stack)
  sq_map1 <- get_stamenmap(bbox = c(left = bbox@xmin, bottom = bbox@ymin, right = bbox@xmax - 0.3, top = bbox@ymax), maptype = "toner-lite")
  
  if (layer == 8) {
    ggmap(sq_map1) +
      geom_tile(data = layer_df, aes(x = Longitude, y = Latitude, fill = data), alpha = 0.8) +
      scale_fill_gradientn(colours = pal) + 
      theme(panel.background = element_blank()) +
      theme(legend.position="bottom") +
      theme(legend.key.width=unit(2, "cm")) +
      labs(fill = names[layer]) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      # Add the geom_point() layer for the points with black outline
      geom_point(data = merged_sites, aes(x = Longitude, y = Latitude, fill = Te_mean), size = 3, shape=21, color = "black")
  }
  
  else if (layer == 9) {
    ggmap(sq_map1) +
      geom_tile(data = layer_df, aes(x = Longitude, y = Latitude, fill = data), alpha = 0.8) +
      scale_fill_gradientn(colours = pal) + 
      theme(panel.background = element_blank()) +
      theme(legend.position="bottom") +
      theme(legend.key.width=unit(2, "cm")) +
      labs(fill = names[layer]) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      # Add the geom_point() layer for the points with black outline
      geom_point(data = merged_sites, aes(x = Longitude, y = Latitude, fill = Mean_Diurnal_Range_C), size = 3, shape=21, color = "black")
    
  }
  
  else {
    ggmap(sq_map1) +
      geom_tile(data = layer_df, aes(x = Longitude, y = Latitude, fill = data), alpha = 0.8) +
      scale_fill_gradientn(colours = pal) + 
      theme(panel.background = element_blank()) +
      theme(legend.position="bottom") +
      theme(legend.key.width=unit(2, "cm")) +
      labs(fill = names[layer]) + 
      xlab("Longitude") + 
      ylab("Latitude") + 
      geom_point(data = merged_sites, aes(x = Longitude, y = Latitude), size = 2, color = "black")
    
    
  }
  
  # Display the plot
  plot(last_plot())
}



