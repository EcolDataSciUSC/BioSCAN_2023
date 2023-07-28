library(data.table)
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(stringr)
library(tidyselect)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)


prepare_occurrence <- function(year_range, oc_interval, taxonomic_group){
  
  species_data_raw<-read.csv("Data_Upon_Request/bioscan_survey_data_column_clean.csv")
  
  #drop sample id column
  species_data<- dplyr::select(species_data_raw, -c(original_id))
  
  #split id into separate year, month, site columns
  species_data<-separate(data = species_data, col = id, into = c("year", "month", "phase","site"), sep = "\\_")
  species_data$year = as.integer(species_data$year)
  species_data$month = as.integer(str_remove(species_data$month, "^0+"))
  species_data<- dplyr::select(species_data, -c(phase)) 
  
  
  
  #Import taxonomic data to filter species data for specific families, groups,etc
  taxonomic_data <- read.csv("Data/master_species_list.csv")
  
  taxonomic_data$Species <- sub(" ", "_", taxonomic_data$Species) 
  
  
  
  if(taxonomic_group =="Phoridae"){
    
    #fix some of the errors in the naming/duplicates
    
    #there are 2 entries in the species data for the sanme species; this compares the two columns for each species and takes the max value of each as the value (so if it is 0 in a site for 1 column, and 1 in the same site for another column of the same species, it takes the 1)
    species_data <-species_data %>%
      mutate(species_data, Beckerina_sp1 = pmax(Beckerina_sp_1, Beckerina_yellow_sp_1)) %>%
      mutate(species_data, Beckerina_sp2 = pmax(Beckerina_sp_2, Beckerina_brown_sp_2)) %>%
      mutate(species_data, Spiniphora_bergenstammi_combine = pmax(Spiniphora_bergenstammi, Spiniphora_sp))
    
    species_data <- dplyr::select(species_data, -c(Beckerina_sp_1, Beckerina_yellow_sp_1,Beckerina_sp_2, Beckerina_brown_sp_2, Spiniphora_bergenstammi, Spiniphora_sp)) 
    
    species_data<-species_data[,order(colnames(species_data[4:451,]))]
    
    
    
    species_data <-species_data %>%
      rename("Megaselia_sp1" = "Megaselia_BB_unknown_1_sp",
             "Megaselia_sp5" = "Megaselia_BB_unknown_5_sp",
             "Megaselia_sp7" = "Megaselia_BB_unknown_7_sp",
             "Megaselia_sp8" = "Megaselia_BB_unknown_8_sp",
             "Megaselia_sp11" = "Megaselia_BB_unknown_11_sp",
             "Megaselia_spHL" = "Megaselia_Hairy_Lobe_sp",
             "Phora_sp1" = "Phora_sp_1",
             "Puliciphora_sp2" = "Puliciphora_sp_2",
             "Spiniphora_bergenstammii" = "Spiniphora_bergenstammi_combine")
    
    Phorids<- taxonomic_data %>%
      filter(Family == "Phoridae")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species" | Level_of_identification == "Morphospecies") %>%
      distinct(Species) %>%
      dplyr::select(Species)
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(Phorids$Species, ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything())
    
    
  }
  
  else if(taxonomic_group =="Lepidoptera"){
    
    #fix some of the errors in the naming/duplicates
    
    species_data<-species_data[,order(colnames(species_data[4:451,]))]
    
    Lepidoptera<- taxonomic_data %>%
      filter(Order == "Lepidoptera")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species" | Level_of_identification == "Morphospecies") %>%
      distinct(Species) %>%
      dplyr::select(Species)
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(Lepidoptera$Species, ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything())
    
  }
  
  
  else if(taxonomic_group =="Syrphidae"){
    
    #fix some of the errors in the naming/duplicates
    
    #there are 2 entries in the species data for the same species; this compares the two columns for each species and takes the max value of each as the value (so if it is 0 in a site for 1 column, and 1 in the same site for another column of the same species, it takes the 1)
    # because we only require presence / absence data , it is not necessary to alter the resulting max value.
    species_data <- species_data %>%
      mutate(species_data, Pseudodoros_clavatus = pmax(Dioprosopa_clavata, Pseudodoros_clavatus)) 
    
    ## Run 37, 39 together. Then, 45-51 together.
    
    ## Note: each mutation goes to the VERY END of the table. Except (I guess because Pseudodoros_clavatus already exists) the above mutation remains at its appropriate place.
    
    species_data <- species_data %>%
      
      ## Sphaerophoria_contigua and Sphaerophoria_sulphuripes are both misspelled to Spaerophoria_contigua and Spaerophoria_sulphuripes
      ## each also has a second duplicate.
      
      mutate(species_data, Sphaerophoria_contigua = pmax(species_data[, 379], species_data[, 381])) %>% 
      mutate(species_data, Sphaerophoria_sulphuripes = pmax(species_data[, 380], species_data[, 382]))
    
    
    species_data <- dplyr::select(species_data, -c(Dioprosopa_clavata, Spaerophoria_contigua, Spaerophoria_sulphuripes)) %>% 
      select(-c(378,379))
    # remove synonym: Dioprosopa_clavata.
    
    ##HERE WE HAVE: just Pseudodoros_clavatus with the greatest occurence value of the two synonyms; just Sphaerophoria_contigua and _sulphuripes with greatest occurence value of duplicates.
    
    species_data<-species_data[, order(colnames(species_data[4:451,]))]
    
    ## reorders species columns alphabetically.
    
    
    Syrphids<- taxonomic_data %>%
      filter(Family == "Syrphidae")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species") %>%
      filter(!Species == "Dioprosopa_clavata") %>% 
      distinct(Species) %>% ## remove duplicate species
      dplyr::select(Species)
    
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(Syrphids$Species, ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything()) ## this should order everything correctly, as written.
    
    ##Okay, so the order should match up (alphabetically)
    
    ## MAKE SURE: month year site are at the beginning.
  }
  
  else if(taxonomic_group =="Tipulomorpha"){
    species_data<-species_data[, order(colnames(species_data[4:451,]))]
    
    Tipulomorpha<- taxonomic_data %>%
      filter(Family == "Tipulidae" | Family == "Limoniidae")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species" | Level_of_identification == "Morphospecies") %>%
      distinct(Species) %>%
      dplyr::select(Species)
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(Tipulomorpha$Species, ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything())
    
  }
  
  else if(taxonomic_group =="Drosophilidae"){
    
    #fix some of the errors in the naming/duplicates
    
    species_data<-species_data[,order(colnames(species_data[4:451,]))]
    
    Drosophilidae<- taxonomic_data %>%
      filter(Family == "Drosophilidae")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species" | Level_of_identification == "Morphospecies") %>%
      distinct(Species) %>%
      dplyr::select(Species)
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(Drosophilidae$Species, ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything())
    
  }
  
  else if(taxonomic_group =="Mycetophilidae"){
    
    #fix some of the errors in the naming/duplicates
    
    species_data<-species_data[,order(colnames(species_data[4:451,]))]
    
    Mycetophilidae<- taxonomic_data %>%
      filter(Family == "Mycetophilidae")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species" | Level_of_identification == "Morphospecies") %>%
      distinct(Species) %>%
      dplyr::select(Species)
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(c(Mycetophilidae$Species, 
                               paste(sapply(Mycetophilidae$Species, function(x) paste0(strsplit(x, "_")[[1]], collapse = "_BioSCAN_"))),
                               sapply(paste(sapply(Mycetophilidae$Species, function(x) paste0(strsplit(x, "_")[[1]], collapse = "_BioSCAN_"))), function(x) {
                                 substr(x, 1, nchar(x)-1) %>%
                                   paste0("_", substr(x, nchar(x), nchar(x)))
                               })), ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything())
    
  }
  
  else if(taxonomic_group =="Araneae"){
    
    #fix some of the errors in the naming/duplicates
    
    species_data<-species_data[,order(colnames(species_data[4:451,]))]
    
    Araneae<- taxonomic_data %>%
      filter(Order == "Araneae")  %>%
      filter(!is.na(Species)) %>%
      filter(Level_of_identification == "Species" | Level_of_identification == "Morphospecies") %>%
      distinct(Species) %>%
      dplyr::select(Species)
    
    #Be careful; some species are lost at this filtering step (ex. anything with spelling differences between datasets, and anything that ends in Genus_sp1 vs. Genus_sp_1); make sure you double check this
    keep <- species_data %>%
      dplyr::select(contains(Araneae$Species, ignore.case=TRUE)) %>%
      cbind(year = species_data$year,month = species_data$month, site =species_data$site) %>%
      dplyr::select(year, month, site, everything())
    
  }
  
  
  
  
  else if(taxonomic_group == 'all'){
    
    keep<-species_data
    
  }
  
  
  #Need a matrix with site, year, month, and species, but only for the site-month-year-species combos that have a species present (get rid of absences)
  generate_species_matrix <- function(i) {
    keep %>%
      dplyr::select(site, year, month, tidyselect::all_of(i)) %>% # select only necessary columns and species i
      gather(key = "species", value = "presence", c(-site, -year, -month)) %>%
      filter(presence == 1)
  }
  
  species_matrix <- generate_species_matrix(4:ncol(keep))
  species_matrix<-as.data.table(species_matrix)

  year_month_visit_df<- keep %>%
    select(site, year, month) %>% 
    unique() %>%
    as.data.table()
  
  #setkey sorts a data.table and marks it as sorted with an attribute sorted. The sorted columns are the key. The key can be any number of columns. The columns are always sorted in ascending order.
  
  #add occ_int and visit to the observations data
  
  ## get the sites and years present in the species matrix
  site_year_in_species_matrix <- species_matrix %>%
    select(site, year) %>% unique()  
  
  ## create a visit data frame with the year and sites in the species matrix but making sure we keep months that had no detections
  species_matrix_visit_all <- year_month_visit_df %>% 
    filter(site %in% site_year_in_species_matrix$site & year %in% site_year_in_species_matrix$year) %>% 
    group_by(site) %>% 
    mutate(visit = paste0('v', 1:n()))  
  
  species_matrix_visit <- species_matrix_visit_all %>% 
    full_join(species_matrix) %>% 
    filter(!is.na(species))
  
  #create a vector of all applicable species
  species_presence <- sort(unique(species_matrix$species))
  
  site_ID <- sort(unique(species_matrix_visit_all$site)) 
  visit_ID <- sort(unique(species_matrix_visit_all$visit)) 
  nsite  <- length(unique(species_matrix_visit_all$site))
  nsp <- length(unique(species_matrix$species))
  nvisit <- length(unique(species_matrix_visit_all$visit))
  
  ## create occupancy array ##
  #unique entries for each species/site/visit combination
  #a 3-D matrix per species
  #below: creates an occupancy array with the dimensions we want, fills it in with 0s for now
  occ.arr <- array(0, dim = c(nsite, nvisit, nsp), 
                   dimnames = list(site=site_ID,
                                   visit=visit_ID,
                                   sp=species_presence))
  
  #create a visit array with the same dimensions by extracting the array from species #1
  vis.arr<-occ.arr[,,1] 
  
  #In your occupancy array, replace 0s with 1s for all species that were observed in each site/visit combination
  occ.arr[cbind(match(species_matrix_visit$site, site_ID), match(species_matrix_visit$visit, visit_ID), match(species_matrix_visit$species, species_presence))] <- 1 
  
  #print the first row of all of the matrices (i.e. site #1)
  print(occ.arr[1,,])
  
  #Print the 1st Matrix (i.e. species #1)
  print(occ.arr[,,1])
  
  #print the first row of the 1st matrix (site #1, species #1)
  print(occ.arr[1,,1])
  
  #create
  #site_year_month_visit_df<- year_month_visit_df %>%
  #  left_join(unique(select(species_matrix_visit,year,month,site))) 
  
  #replace the 0s in your visit array with 1s for all site-visit combos
  vis.arr[cbind(match(species_matrix_visit_all$site, site_ID),  match(species_matrix_visit_all$visit, visit_ID))] <- 1 
  
  names(dim(occ.arr)) <- c('nsite', 'nvisit', 'nsp')
  
  get.indices <- function(sp) {
    tmp <- which(vis.arr==1, arr.ind=TRUE)
    cbind(sp=rep(sp,nrow(tmp)),tmp)
  }
  
  master.index <-
    do.call(rbind, lapply(1:dim(occ.arr)['nsp'], get.indices))
  
  nrow(master.index)
  
  master.index <- master.index[,c(2,3,1)]
  
  X <- occ.arr[master.index]
  
  ## getting month
  site_month <- species_matrix_visit_all %>% 
    mutate(visit = as.numeric(str_remove(visit, "v"))) %>% 
    rename(site_id = site) %>% 
    left_join(data.frame(site_id = site_ID, site = 1:length(site_ID)))
  
  master.index_month <- master.index %>% 
    as.data.frame() %>% 
    left_join(site_month) 
  
  
  
  #import all predictors
  collection.arr <- readRDS(paste0("Data/clean_data/data_prepared/collection_data_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  land_value_1km.arr <- readRDS(paste0("Data/clean_data/data_prepared/land_value_data_1km_",paste0(year_range, collapse = "_"),'_spatial',".rds"))
  land_value_05km.arr <- readRDS(paste0("Data/clean_data/data_prepared/land_value_data_05km_",paste0(year_range, collapse = "_"),'_spatial',".rds"))
  land_value_025km.arr <- readRDS(paste0("Data/clean_data/data_prepared/land_value_data_025km_",paste0(year_range, collapse = "_"),'_spatial',".rds"))
  imperv_1km.arr <- readRDS(paste0("Data/clean_data/data_prepared/imperv_data_1km_",paste0(year_range, collapse = "_"),'_spatial',".rds"))
  imperv_05km.arr <- readRDS(paste0("Data/clean_data/data_prepared/imperv_data_05km_",paste0(year_range, collapse = "_"),'_spatial',".rds"))
  imperv_025km.arr <- readRDS(paste0("Data/clean_data/data_prepared/imperv_data_025km_",paste0(year_range, collapse = "_"),'_spatial',".rds"))
  RH_mean.arr <- readRDS(paste0("Data/clean_data/data_prepared/RH_mean_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  RH_std.arr <- readRDS(paste0("Data/clean_data/data_prepared/RH_std_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  prcp.arr <- readRDS(paste0("Data/clean_data/data_prepared/prcp_sum_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  prcp.std <- readRDS(paste0("Data/clean_data/data_prepared/prcp_std_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  wind.arr <- readRDS(paste0("Data/clean_data/data_prepared/wind_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  T_mean.arr <- readRDS(paste0("Data/clean_data/data_prepared/T_mean_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  T_diurnal_range.arr <- readRDS(paste0("Data/clean_data/data_prepared/T_diurnal_range_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  T_isothermality.arr <- readRDS(paste0("Data/clean_data/data_prepared/T_isothermality_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  T_std.arr <- readRDS(paste0("Data/clean_data/data_prepared/T_std_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  T_annual_range.arr <- readRDS(paste0("Data/clean_data/data_prepared/T_annual_range_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  ocean_effect.arr <- readRDS(paste0("Data/clean_data/data_prepared/ocean_effect_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  distance_natural.arr <- readRDS(paste0("Data/clean_data/data_prepared/distance_natural_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  elevation.arr <- readRDS(paste0("Data/clean_data/data_prepared/elevation_", paste0(year_range, collapse = "_"),'_spatial',".rds"))
  month.arr <- readRDS(paste0("Data/clean_data/data_prepared/month_", paste0(year_range, collapse = "_"),'_spatial',".rds"))
  
  
  
  # Convert arrays with dimensions [site, year, month] to [site, visit] corresponding to the group we are modelling since each group has different time domains
  conversion_df <- site_month
  conversion_df$visit <- paste0("v", conversion_df$visit)
  conversion_df$year <- conversion_df$year + 2000
  
  
  # Collection Array
  collection.arr.visit <- array(0, dim = c(nsite, nvisit), 
                   dimnames = list(site=site_ID,
                                   visit=visit_ID))
  
  for (i in 1:nrow(conversion_df)) { # here, we loop over every row and essentially collapse the year and month dimensions in the original array into a "visit" dimension
    row <- conversion_df[i, ]
    site_index <- which(site_ID == row$site_id)
    visit_index <- which(visit_ID == row$visit)
    collection.arr.visit[site_index, visit_index] <- collection.arr[site_index, as.character(row$year), str_trim(as.character(row$month))]
  }
  
  # Wind Array
  wind.arr.visit <- array(0, dim = c(nsite, nvisit), 
                                dimnames = list(site=site_ID,
                                                visit=visit_ID))
  
  for (i in 1:nrow(conversion_df)) { # here, we loop over every row and essentially collapse the year and month dimensions in the original array into a "visit" dimension
    row <- conversion_df[i, ]
    site_index <- which(site_ID == row$site_id)
    visit_index <- which(visit_ID == row$visit)
    wind.arr.visit[site_index, visit_index] <- wind.arr[site_index, as.character(row$year), str_trim(as.character(row$month))]
  }
  
  # Month
  month.arr.visit <- array(0, dim = c(nsite, nvisit), 
                          dimnames = list(site=site_ID,
                                          visit=visit_ID))
  
  for (i in 1:nrow(conversion_df)) { # here, we loop over every row and essentially collapse the year and month dimensions in the original array into a "visit" dimension
    row <- conversion_df[i, ]
    site_index <- which(site_ID == row$site_id)
    visit_index <- which(visit_ID == row$visit)
    month.arr.visit[site_index, visit_index] <- month.arr[site_index, as.character(row$year), str_trim(as.character(row$month))]
  }
  
  
  

  
  # Set up final dataset for input into occupancy model based on group
  
  if(taxonomic_group =="Phoridae"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_025km.arr[site_ID],
                        imperv=imperv_025km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
  }
  
  else if(taxonomic_group =="Syrphidae"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_05km.arr[site_ID],
                        imperv=imperv_05km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
    
  }
  
  else if(taxonomic_group =="Lepidoptera"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_05km.arr[site_ID],
                        imperv=imperv_05km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
    
  }
  
  else if(taxonomic_group =="Tipulomorpha"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_05km.arr[site_ID],
                        imperv=imperv_05km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
    
  }
  
  else if(taxonomic_group =="Drosophilidae"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_05km.arr[site_ID],
                        imperv=imperv_05km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
    
  }
  
  else if(taxonomic_group =="Mycetophilidae"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_05km.arr[site_ID],
                        imperv=imperv_05km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
    
  }
  
  else if(taxonomic_group =="Araneae"){
    my.data.env <- list(X=X,
                        site=master.index[,'site'],
                        visit=master.index[,'visit'],
                        month = master.index_month[,'month'],
                        year = master.index_month[,'year'],
                        sp=master.index[,'sp'],
                        nsp=dim(occ.arr)['nsp'],
                        nsite=dim(occ.arr)['nsite'],
                        nvisit=dim(occ.arr)['nvisit'],
                        nind=nrow(master.index),
                        colldays=collection.arr.visit[site_ID, visit_ID], #make sure it's the same order as occ array
                        landvalue=land_value_025km.arr[site_ID],
                        imperv=imperv_025km.arr[site_ID],
                        Temp=T_mean.arr[site_ID],
                        Temp_diurnal=T_diurnal_range.arr[site_ID],
                        Temp_isothermality=T_isothermality.arr[site_ID],
                        RH=RH_mean.arr[site_ID],
                        RH_std=RH_std.arr[site_ID],
                        wind=wind.arr.visit[site_ID,visit_ID],
                        ocean_effect=ocean_effect.arr[site_ID],
                        distance_natural=distance_natural.arr[site_ID],
                        elevation=elevation.arr[site_ID],
                        month_det=month.arr.visit[site_ID,visit_ID]) 
    
  }
  
  
  all_data <- list(my.data.env,
                   site=site_ID,
                   visit=visit_ID,
                   sp=species_presence)
  
  
  if(taxonomic_group !="all"){
    
    saveRDS(all_data, paste0("Data/clean_data/data_prepared/my_data_", paste0(year_range, collapse = "_"), "_", taxonomic_group,  "_", "spatial", ".rds"))
    
  }else if(taxonomic_group == 'all'){
    saveRDS(all_data, paste0("Data/clean_data/data_prepared/my_data_all_species", paste0(year_range, collapse = "_"),  "_", "spatial", ".rds" ))
  }
}





# MODEL OPTIONS
# _____________________________________________________________________________
#run for Phoridae
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Phoridae"

prepare_occurrence(year_range, oc_interval, taxonomic_group)


#run for Lepidoptera
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Lepidoptera"

prepare_occurrence(year_range, oc_interval, taxonomic_group)


#run for Syrphidae
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Syrphidae"

prepare_occurrence(year_range, oc_interval, taxonomic_group)


#run for Tipulomorpha
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Tipulomorpha"

prepare_occurrence(year_range, oc_interval, taxonomic_group)


#run for Drosophilidae
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Drosophilidae"

prepare_occurrence(year_range, oc_interval, taxonomic_group)


#run for Mycetophilidae
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Mycetophilidae"

prepare_occurrence(year_range, oc_interval, taxonomic_group)

#run for Araneae
year_range=c(14,18)
oc_interval<-1/4 #estimating occupancy 4x per year
taxonomic_group<-"Araneae"

prepare_occurrence(year_range, oc_interval, taxonomic_group)

