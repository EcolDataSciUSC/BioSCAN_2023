library(raster)
library(rasterVis)
library(PNWColors)
library(ggplot2)
library(basemaps)
library(sf)
library(ggmap)
library(vegan)
library(readxl)
library(units)
library(gdistance)


getwd()

# Set working directory to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)




########################################
###### Collection Data #################
########################################

master_df<-read.csv(paste0(my_working_dir, '/Data/MASTER_PREDICTOR_TABLE.csv'))
master_df$month_pred<- master_df$Month


########################################
###### Distance Natural Raster #########
########################################

# CPAD data should be downloaded from https://www.calands.org and added to the data folder
protected_areas <- st_read("Data/cpad/CPAD_2023a_Units.shp")  # Replace with the path to your polygon layer file

# Calculate the areas of the polygons
areas <- st_area(protected_areas)

threshold <- set_units(100000, m^2)

# Filter out polygons with an area less than the 10 hecrates (==100,000 m^2)
protected_areas <- protected_areas[areas >= threshold, ]


# Read in raster and fill with distance values
ref_raster_wgs <- raster("Data/spatial_data/elevation.tif")
ref_raster <- projectRaster(ref_raster_wgs, crs = st_crs(protected_areas)$wkt)

# Convert raster to points
points <- rasterToPoints(ref_raster)

# Create an sf object with points
points_sf <- st_as_sf(as.data.frame(points), coords = c("x", "y"))

# Set the CRS of the points_sf object
points_sf <- st_set_crs(points_sf, st_crs(protected_areas))

# Create a SpatialPointsDataFrame
points_sp <- as(points_sf, "Spatial")

distances <- st_distance(points_sf, protected_areas)
min_distances <- apply(distances, 1, min)

points_sf$distancenatural <- min_distances

# Rasterize the points by taking the mean value within each cell
distance_natural <- rasterize(points_sf, ref_raster, field="distancenatural")

distance_natural <- projectRaster(distance_natural, crs = st_crs(ref_raster_wgs)$wkt)

# plot
plot(distance_natural)


# Save raster
writeRaster(distance_natural, filename = "Data/spatial_data/distancenatural.tif", format = "GTiff", overwrite=TRUE)



########################################
###### Distance Natural Sites ##########
########################################

sites <- read_excel("bioscan.xlsx", sheet= "Annual Combined All")
site.points <- SpatialPoints(sites[, c("Longitude", "Latitude")])
site.points.df <- sites[, c("Longitude", "Latitude", "Site")]

site_distances <- extract(distance_natural, site.points)

site.points.df$distance_natural <- site_distances

# Save as csv
write.csv(site.points.df, file = "Data/distance_natural.csv", row.names = FALSE)

