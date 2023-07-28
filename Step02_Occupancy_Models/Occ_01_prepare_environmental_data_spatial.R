library(dplyr)
library(stringr)
library(tidyr)
library(data.table)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)


prepare_predictors <- function(year_range){
  
  ########################################
  ###### Collection info #################
  ########################################
  
  master_df<-read.csv(paste0(my_working_dir, '/Data/MASTER_PREDICTOR_TABLE.csv'))
  master_df$month_pred<- master_df$Month
  
  collection_data<- select(master_df, c(Phase, Site, Year, Month, Collection.days)) 
  
  #reformatting collection dataframe, etc.
  
  #split id into separate year, month, site columns; no longer needed so this is commented out
  #collection_data<-separate(data = collection_data, col = id, into = c("year", "month", "phase","site"), sep = "\\_")
  
  collection_data<-rename_with(collection_data, tolower) # all columns to lowercase
  collection_data$site <- sprintf("%02d",collection_data$site)
  collection_data$year = as.integer(collection_data$year)
  collection_data$month = as.integer(str_remove(collection_data$month, "^0+"))
  collection_data<- select(collection_data, -c(phase)) 
  collection_data$collection.days<-scale(collection_data$collection.days)
  collection_data<-as.data.table(collection_data)
  
  #extract vectors of all unique sites, visits and the length of each vector
  site_ID <- sort(unique(collection_data$site)) 
  year_ID <- sort(unique(collection_data$year)) 
  month_ID <- sort(unique(collection_data$month)) 
  nsite  <- length(unique(collection_data$site))
  nyear <- length(unique(collection_data$year))
  nmonth  <- length(unique(collection_data$month))
  
  #create an empty array with the dimensions we want, fills it in with 0s for now
  collection.arr <- array(0, dim = c(nsite, nyear, nmonth), 
                          dimnames = list(site=site_ID, 
                                          year=year_ID,
                                          month=month_ID))
  
  #replace the 0s with the actual # of collection days
  collection.arr[cbind(match(collection_data$site, site_ID), match(collection_data$year, year_ID), match(collection_data$month, month_ID))] <- collection_data$collection.days
  
  
  #you will need to create this folder ("clean_data/data_prepared/"), or save it elsewhere if you prefer
  saveRDS(collection.arr, file = paste0("Data/clean_data/data_prepared/collection_data_",paste0(year_range, collapse = "_"),'_spatial', ".rds"))
  
  
  #############################################################
  ############ Site and Environment Predictors ################
  #############################################################
  
  # Define function for creating array
  create_env_var_array<-function(save_name, column_name, scale_type, indexing_type){
    
    
    # Get site-level LV
    env_data<- unique(select(master_df, c(Site, column_name, Year, Month))) %>%
      rename(year=Year, month=Month)
    env_data$Site<-sort(env_data$Site)
    env_data$Site <- sprintf("%02d", env_data$Site)
    
    #scaling and log-transforming as the data is skewed
    if(scale_type == 'log10'){
      env_data[,column_name]<-scale(log10(env_data[,column_name]+1))
    }
    else if(scale_type == 'linear' ){
      env_data[,column_name]<-scale(env_data[,column_name])
    }
    
    
    if(indexing_type == 'site'){
      #create empty array; 
      env_data.arr <- array(0, dim = c(nsite), 
                            dimnames = list(site=site_ID))
      
      #fill array with matching land value data
      env_data.arr[cbind(match(env_data$Site, site_ID))] <- env_data[,column_name]
      print(env_data.arr[3])
    }
    else if(indexing_type == 'visit'){
      
      # env_data<-left_join(env_data, collection_data)
      
      #create empty array
      env_data.arr<- array(NA, dim = c(nsite, nyear, nmonth), 
                      dimnames = list(site=site_ID,
                                      year=year_ID,
                                      month=month_ID))
      
      #fill array with values for each environmental predictor
      env_data.arr[cbind(match(env_data$Site, site_ID), match(env_data$year, year_ID), match(env_data$month, month_ID))] <- env_data[,column_name]
    }
    
    
    #JAGS complains about NAs in predictors; so change to 0s because not using these values anyways
    env_data.arr<-replace(env_data.arr, is.na(env_data.arr), 0)
    
    saveRDS(env_data.arr, file = paste0("Data/clean_data/data_prepared/",save_name, '_',paste0(year_range, collapse = "_"),'_spatial',".rds"))
  }
  
  
  # LAND VALUE
  create_env_var_array('land_value_data_1km', "LV_1km", 'log10', 'site')
  create_env_var_array('land_value_data_05km', "LV_05km", 'log10', 'site')
  create_env_var_array('land_value_data_025km', "LV_025km", 'log10', 'site')
  
  # IMPERVIOUS
  create_env_var_array('imperv_data_1km', 'imperv_1km', 'linear', 'site')
  create_env_var_array('imperv_data_05km', 'imperv_05km', 'linear', 'site')
  create_env_var_array('imperv_data_025km', 'imperv_025km', 'linear', 'site')
  
  # RH
  create_env_var_array('RH_mean', 'RH_mean', 'linear', 'site')
  create_env_var_array('RH_std', 'RH_std', 'linear', 'site')
  
  # Temp
  create_env_var_array('T_mean', 'Te_mean', 'linear', 'site')
  create_env_var_array('T_diurnal_range', 'Mean_Diurnal_Range_C', 'linear', 'site')
  create_env_var_array('T_isothermality', 'Isothermality', 'linear', 'site')
  create_env_var_array('T_std', 'Te_std', 'linear', 'site')
  create_env_var_array('T_annual_range', 'Annual_T_Range', 'linear', 'site')
  
  # PRCP
  create_env_var_array('prcp_sum', 'prcp_sum', 'log10', 'site')
  create_env_var_array('prcp_std', 'prcp_std', 'log10', 'site')
  
  # Wind
  create_env_var_array('wind', 'AWND_Avg_During_Collection', 'linear', 'visit')
  
  # cost distance to ocean
  create_env_var_array('ocean_effect', 'ocean_effect', 'log10', 'site')
  
  # Distance to natural area
  create_env_var_array('distance_natural', 'distance_natural', 'log10', 'site')
  
  # Elevation
  create_env_var_array('elevation', 'elevation', 'linear', 'site')
  
  # Month
  create_env_var_array('month', 'month_pred', 'linear', 'visit')

}


#specify the year range over which you'd like to extract variables below; for 2014-2018, the year range would be 14-18
prepare_predictors(c(14,18))
