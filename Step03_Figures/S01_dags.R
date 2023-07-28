library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)

# Create function for filling nan values with column means -> to be used in DAG-data consistency tests
# But it does not like missing data (which is present in the land_value column here. so we will fill it with the mean)
fill_nan_with_mean <- function(data) {
  means <- colMeans(data, na.rm = TRUE)  # Calculate column means
  is_nan <- is.na(data)  # Identify NaN values
  
  # Replace NaN values with column means
  data[is_nan] <- means[col(data)][is_nan]
  
  return(data)
}


master_df<-read.csv(paste0(my_working_dir, '/Data/MASTER_PREDICTOR_TABLE.csv')) %>%
  mutate(Year = substring(Year, 3, 4)) %>%
  mutate(Site = sprintf("%02d", Site)) %>%
  mutate(Month = sprintf("%02d", Month))

# MUST ADD SOLAR RADATION   - AND MAYBE EXPAND ROWS AND INCLUDE OCCURANCE?
master_df_sel <- master_df %>%
  dplyr::select(Site, Year, Month, prcp_sum, Te_mean, Mean_Diurnal_Range_C, RH_mean, imperv_025km, LV_025km, ocean_effect, So_mean, elevation, distance_natural) %>%
  dplyr::rename("precipitation" = "prcp_sum",
                "temperature_d" = "Te_mean",
                "diurnal_range" = "Mean_Diurnal_Range_C",
                "relative_humidity" = "RH_mean",
                "impervious_surface" = "imperv_025km",
                "land_value" = "LV_025km",
                "distance_ocean" = "ocean_effect",
                "solar_radiation" = "So_mean"
                )

# Get one value per site
master_df_sel <- master_df_sel[!duplicated(master_df_sel$Site), ]

# make dummy occupancy variable
master_df_sel$occupancy <- runif(nrow(master_df_sel), min = 0, max = 10)

# filter out outlier in land_value 
master_df_sel <- master_df_sel[master_df_sel$land_value < 1e7, ]

group_data_merged <- master_df_sel %>%
  select(-c(Site, Year, Month)) %>%
  mutate(occupancy = as.numeric(occupancy))

# Fill NaN values with column means
group_data_merged <- fill_nan_with_mean(group_data_merged)


##### Tipulids need hydrological reserviors ######
  
tipulid_dag <- dagify(occupancy ~ physiology + vegetation_cover + distance_natural,
                    physiology ~ temperature_d + diurnal_range + hydrologic_resevoirs,
                    vegetation_cover ~ impervious_surface + land_management + elevation,
                    land_management ~ land_value,
                    diurnal_range ~ distance_ocean,
                    land_value ~ living_desireability,
                    impervious_surface ~ living_desireability,
                    elevation ~ distance_ocean,
                    precipitation ~ distance_ocean,
                    hydrologic_resevoirs ~ land_management + precipitation,
                    living_desireability ~ distance_ocean + elevation + distance_natural,
                    distance_natural ~ elevation,
                    latent = c("physiology", "vegetation_cover", "land_management", 
                               "living_desireability", "hydrologic_resevoirs"),
                    # exposure = "diurnal_range",
                    outcome = "occupancy",
                    labels = c("occupancy" = "Occupancy", 
                               "physiology" = "Physiology",
                               "vegetation_cover" = "Vegeation Cover",
                               "land_management" = "Land\n Management",
                               "diurnal_range" = "Diurnal Range",
                               "land_value" = "Land Value",
                               "impervious_surface" = "Impervious\n Surface",
                               "elevation" = "Elevation",
                               "precipitation" = "Precipitation",
                               "hydrologic_resevoirs" = "Hydrologic\n Resevoirs",
                               "living_desireability" = "Living\n Desireability",
                               "distance_natural" = "Distance to\n Natural Areas",
                               "distance_ocean" = "Distance to Ocean",
                               "temperature_d" = "Mean Annual\n Temperature"))




# Fancier graph

tipulid_dag_tidy <- tipulid_dag %>% 
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

ggplot(tipulid_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "#80a283") +
  scale_fill_manual(values = status_colors, na.value = "#80a283") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()

ggdag::ggdag_adjustment_set(tipulid_dag_tidy)

# 
# ##### Leptipdoptera --  moisture for host plant ####
# lep_dag <- dagify(occupancy ~ physiology +  host_plant_presence + vegetation_cover + distance_natural,
#                       physiology ~ temperature_d + diurnal_range ,
#                       vegetation_cover ~ impervious_surface + land_management + elevation,
#                       land_management ~ land_value,
#                       diurnal_range ~ distance_ocean,
#                       host_plant_presence ~ land_management,
#                       land_value ~ living_desireability,
#                       impervious_surface ~ living_desireability,
#                       elevation ~ distance_ocean,
#                       living_desireability ~ distance_ocean + elevation + distance_natural,
#                       distance_natural ~ elevation,
#                       latent = c("physiology", "vegetation_cover", "land_management", 
#                                  "host_plant_presence", "living_desireability"),
#                       exposure = "diurnal_range",
#                       outcome = "occupancy")
# 
# 
# # Fancier graph
# lep_dag_tidy <- lep_dag %>% 
#   tidy_dagitty() %>%
#   node_status()   # Add column for exposure/outcome/latent
# 
# status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")
# 
# ggplot(lep_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_dag_edges() +
#   geom_dag_point(aes(color = status)) +
#   geom_dag_label_repel(aes(label = name, fill = status), seed = 1234,
#                        color = "white", fontface = "bold") +
#   scale_color_manual(values = status_colors, na.value = "#80a283") +
#   scale_fill_manual(values = status_colors, na.value = "#80a283") +
#   guides(color = FALSE, fill = FALSE) + 
#   theme_dag()
# 
# ggdag::ggdag_adjustment_set(lep_dag)
# 

##### phorids, mycetophilids, drosophilids, syrphids #####
phorid_dag <- dagify(occupancy ~ physiology + vegetation_cover + distance_natural,
                  physiology ~ temperature_d + diurnal_range + relative_humidity,
                  vegetation_cover ~ impervious_surface + land_management + elevation,
                  land_management ~ land_value,
                  diurnal_range ~ distance_ocean,
                  land_value ~ living_desireability,
                  impervious_surface ~ living_desireability,
                  elevation ~ distance_ocean,
                  precipitation ~ elevation,
                  relative_humidity ~ distance_ocean + temperature_d + elevation + impervious_surface + precipitation,
                  living_desireability ~ distance_ocean + elevation + distance_natural,
                  distance_natural ~ elevation,
                  latent = c("physiology", "vegetation_cover", "land_management", 
                            "living_desireability"),
                  # exposure = "relative_humidity",
                  outcome = "occupancy",
                  labels = c("occupancy" = "Occupancy", 
                             "physiology" = "Physiology",
                             "vegetation_cover" = "Vegeation Cover",
                             "land_management" = "Land\n Management",
                             "diurnal_range" = "Diurnal Range",
                             "land_value" = "Land Value",
                             "impervious_surface" = "Impervious\n Surface",
                             "elevation" = "Elevation",
                             "precipitation" = "Precipitation",
                             "hydrologic_resevoirs" = "Hydrologic\n Resevoirs",
                             "living_desireability" = "Living\n Desireability",
                             "distance_natural" = "Distance to\n Natural Areas",
                             "distance_ocean" = "Distance to Ocean",
                             "temperature_d" = "Mean Annual\n Temperature",
                             "relative_humidity" = "Relative\n Humidity"))



phorid_dag_tidy <- phorid_dag %>% 
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

ggplot(phorid_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "#80a283") +
  scale_fill_manual(values = status_colors, na.value = "#80a283") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()

ggdag::ggdag_adjustment_set(phorid_dag_tidy)



##### Araneae #####
araneae_dag <- dagify(occupancy ~ physiology + vegetation_cover + distance_natural,
                     physiology ~ temperature_d + diurnal_range ,
                     vegetation_cover ~ impervious_surface + land_management + elevation,
                     land_management ~ land_value,
                     diurnal_range ~ distance_ocean,
                     land_value ~ living_desireability,
                     impervious_surface ~ living_desireability,
                     elevation ~ distance_ocean,
                     living_desireability ~ distance_ocean + elevation + distance_natural,
                     distance_natural ~ elevation,
                     latent = c("physiology", "vegetation_cover", "land_management", 
                                "living_desireability"),
                     # exposure = "diurnal_range",
                     outcome = "occupancy",
                     labels = c("occupancy" = "Occupancy", 
                                "physiology" = "Physiology",
                                "vegetation_cover" = "Vegeation Cover",
                                "land_management" = "Land\n Management",
                                "diurnal_range" = "Diurnal Range",
                                "land_value" = "Land Value",
                                "impervious_surface" = "Impervious\n Surface",
                                "elevation" = "Elevation",
                                "precipitation" = "Precipitation",
                                "hydrologic_resevoirs" = "Hydrologic\n Resevoirs",
                                "living_desireability" = "Living\n Desireability",
                                "distance_natural" = "Distance to\n Natural Areas",
                                "distance_ocean" = "Distance to Ocean",
                                "temperature_d" = "Mean Annual\n Temperature",
                                "relative_humidity" = "Relative\n Humidity"))


araneae_dag_tidy <- araneae_dag %>% 
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

ggplot(araneae_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "#80a283") +
  scale_fill_manual(values = status_colors, na.value = "#80a283") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()

ggdag::ggdag_adjustment_set(araneae_dag_tidy)



##### mycetophilids, drosophilids, syrphids #####
others_dag <- dagify(occupancy ~ physiology + vegetation_cover + distance_natural,
                     physiology ~ temperature_d + diurnal_range ,
                     vegetation_cover ~ impervious_surface + land_management + elevation,
                     land_management ~ land_value,
                     diurnal_range ~ distance_ocean,
                     land_value ~ living_desireability,
                     impervious_surface ~ living_desireability,
                     elevation ~ distance_ocean,
                     living_desireability ~ distance_ocean + elevation + distance_natural,
                     distance_natural ~ elevation,
                     latent = c("physiology", "vegetation_cover", "land_management", 
                                "living_desireability"),
                     # exposure = "diurnal_range",
                     outcome = "occupancy",
                     labels = c("occupancy" = "Occupancy", 
                                "physiology" = "Physiology",
                                "vegetation_cover" = "Vegeation Cover",
                                "land_management" = "Land\n Management",
                                "diurnal_range" = "Diurnal Range",
                                "land_value" = "Land Value",
                                "impervious_surface" = "Impervious\n Surface",
                                "elevation" = "Elevation",
                                "precipitation" = "Precipitation",
                                "hydrologic_resevoirs" = "Hydrologic\n Resevoirs",
                                "living_desireability" = "Living\n Desireability",
                                "distance_natural" = "Distance to\n Natural Areas",
                                "distance_ocean" = "Distance to Ocean",
                                "temperature_d" = "Mean Annual\n Temperature",
                                "relative_humidity" = "Relative\n Humidity"))


others_dag_tidy <- others_dag %>% 
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

ggplot(others_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "#80a283") +
  scale_fill_manual(values = status_colors, na.value = "#80a283") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()

ggdag::ggdag_adjustment_set(others_dag_tidy)


# Test dag
# IGNORE ANY TEST WITH OCCUPANCY IN IT -> DUMMY VARIABLE
which_dag <- phorid_dag

test <- localTests(which_dag, group_data_merged)
test$p.value <- p.adjust(test$p.value)
plotLocalTestResults(test)


