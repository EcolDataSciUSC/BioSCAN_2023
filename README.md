# BioSCAN 2023
Code and data used to produce the results and figures found in Lewthwaite et. al. 2023.

This code is organized into a series of discrete steps with attainable outputs from each script. All model outputs and data are included except the raw collection data from the Natural History Museum of Los Angeles County's BioSCAN collection (please contact the corresponding author to receive these data to get the most out of the contents of this repository).

## Step 01
Step 01 is itself subdivided into four steps, each with a specific data-cleaning aim. Step 01 is performed in both R and Python.

*Script Env_01* exists to access nearby meteorological stations and interpolate daily meteorological variable values at the BioSCAN sites. While these data were not ultimately used in our analyses, they can be compared with in-situ measurements as a quality check.

*Script Env_02* involves reading in the in-situ HOBO environmental data that was collected, temporal aggregation to match the resolution of our study, removing erroneous data, and gap-filling using inverse-distance weighted interpolation of other proximal HOBO stations.

*Script Env_03* uses the California Protected Area Database to identify large (>100,000 m<sup>2</sup>) parks and natural areas within the extent of our study and create a raster representing euclidean distance to the nearest park/natural area.

*Script Env_04* aggregates all of our predictors into a site x time dataframe, pertinent for running our models.

## Step 02
Our multi-species occupancy models are run in step 02 using R and JAGS. 

*Script Occ_01* primarily takes our site x time dataframe from *Script Env_03* and creates multiple 3-dimensional (site x year x month) predictor arrays in R to be input into the occupancy models.

*Script Occ_02* prepares our raw arthropod collections data (geo-referenced occupancy of each species/taxa) for use in occupancy models by extracting our taxonomic groups of interest and updating presences in occupancy arrays.

*Script Occ_03* takes our predictor arrays (*Occ_01*) and occupancy arrays (*Occ_02*) and runs our JAGS occupancy script for each model configuration (found in *BioSCAN_2023/Step02_Occupancy_Models/models*)

## Step 03
In step 03, all figures found in the manuscript are reproduced using model results.

*Script 01* reproduces the spatial richness interpolation (Figure 1)

*Script 02* reproduces the violin plot (Figure 2)

*Script 03* reproduces the spaghetti plot (Figure 3)
