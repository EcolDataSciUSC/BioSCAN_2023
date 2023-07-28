library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(sf)
library(raster)


# LOS ANGELES COUNTY
LA_parcels <- st_read(dsn = "/Users/teaganbaiotto/Downloads/LA_County_Parcels", layer = "LA_County_Parcels")
LA_is_valid <- st_is_valid(LA_parcels)

# Keep parcels with valid geometries
LA_parcels <- LA_parcels[LA_is_valid, ]

# Calculate centroids of all parcels
centroids <- st_centroid(LA_parcels, byid = TRUE)

# Access the coordinates
point_coords <- st_coordinates(centroids)

# Access the attributes
point_data <- as.data.frame(LA_parcels) %>%
  select(Roll_LandV, Roll_LandB, ShapeSTAre, UseType)

# Combine coordinates and attributes into a data frame
LA_df <- data.frame(coordinates = point_coords, point_data)


# MERGE DATASETS, FILTER
LA_df <- LA_df %>%
  rename(LandValue = Roll_LandV, BaseYear = Roll_LandB, Area = ShapeSTAre)

scaling <- read.csv("/Users/teaganbaiotto/Mirror/Research/bioscan/land_value_data/CPI_scaling.csv", skip=1) %>%
  rename(BaseYear = X)

LA_df_merged <- merge(LA_df, scaling, by = "BaseYear", all.x = TRUE)

LA_df_merged <- LA_df_merged %>% 
  filter(UseType %in% c("SINGLE FAMILY RESIDENTIAL", "Residential", "MULTIPLE FAMILY RESIDENTIAL")) %>% 
  mutate(BaseYear = as.integer(BaseYear)) %>%
  filter(BaseYear > 1949) %>% 
  filter(LandValue > 0)


LA_df_merged$AW.Value <- (LA_df_merged$LandValue*LA_df_merged$Mult_factor)/(LA_df_merged$Area/10000)

LA_df_merged <- subset(LA_df_merged, is.finite(AW.Value))

# SB COUNTY
SB_parcels <- st_read(dsn = "/Users/teaganbaiotto/Downloads/SBCo_Parcel_Polygons", layer = "SBCo_Parcel_Polygons")
SB_is_valid <- st_is_valid(SB_parcels)

# Keep parcels with valid geometries
SB_parcels <- SB_parcels[SB_is_valid, ]

# Calculate centroids of all parcels
centroids <- st_centroid(SB_parcels, byid = TRUE)

# Access the coordinates
point_coords <- st_coordinates(centroids)

# Access the attributes
point_data <- as.data.frame(SB_parcels) %>%
  select(LandValue, BaseYear, Shape__Are, AssessClas)

# Combine coordinates and attributes into a data frame
SB_df <- data.frame(coordinates = point_coords, point_data)

SB_df <- SB_df %>%
  rename(Area = Shape__Are) %>%
  rename(UseType = AssessClas) %>%
  mutate(LandValue = gsub(",", "", LandValue)) %>%
  mutate(LandValue = as.integer(LandValue))

SB_df <- SB_df %>% 
  filter(UseType %in% c("SINGLE FAMILY RESIDENTIAL", "Residential", "MULTIPLE FAMILY RESIDENTIAL")) %>% 
  filter(BaseYear > 1949) %>% 
  filter(LandValue > 0)

SB_df$AW.Value <- SB_df$LandValue/(SB_df$Area/10000)


LA_df_merged_select <- LA_df_merged %>%
  select(coordinates.X, coordinates.Y, AW.Value)

SB_df_select <- SB_df %>%
  select(coordinates.X, coordinates.Y, AW.Value)

parcels <- rbind(LA_df_merged_select, SB_df_select) %>%
  na.omit()

# Read the raster data that will define the extent and cell size
reference_raster <- raster("/Users/teaganbaiotto/Mirror/Research/bioscan/Figure_2/spatial_data/impervious.tif")

# Create a raster layer with the desired extent and cell size
output_raster <- raster(ext=extent(reference_raster), res=res(reference_raster))


# Convert the dataframe to a spatial points dataframe
coordinates(parcels) <- c("coordinates.X", "coordinates.Y")

# Rasterize the points by taking the mean value within each cell
rasterized <- rasterize(parcels, output_raster, field="AW.Value", fun=mean)

# Save raster
writeRaster(rasterized, filename = "/Users/teaganbaiotto/Mirror/Research/bioscan/Figure_2/spatial_data/landvalue.tif", format = "GTiff", overwrite=TRUE)

