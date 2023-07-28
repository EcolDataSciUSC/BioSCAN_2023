library(stringr)
library(rjags)
library(parallel)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)

#make sure you download this file and store it in this folder
#you may need to install some of the packages in this file if you haven't already
source("Step02_Occupancy_Models/src/initialize.R")

#you will need to modify the below as needed; rjags has a bug on M1 Macs, so downloading the package is not straightforward
# Mac: clang -v
# System76: gcc -v
system("gcc -v")
runjags.options(jagspath="/usr/local/bin/jags")


run_model <- function(year_range, taxonomic_group, model){
  
  ## load data
  all_data <- readRDS(paste0("Data/clean_data/data_prepared/my_data_", paste0(year_range, collapse = "_"), "_", taxonomic_group,'_spatial', ".rds" ))
  
  
  ## assign data to main vars
  
  my.data <- all_data[[1]]
  nsite <- length(all_data$site)
  nsp <- length(all_data$sp)
  nvisit<-length(all_data$visit)
  #note that the line below will have to be modified if your model is not looping over the indexed values
  nind<-length(all_data[[1]]$X)
  
  ## Initial values 
  Zst <- array(1,dim=c(nind))
  make.inits <- function() {
    RNG <- parallel.seeds("base::BaseRNG", 1)
    c(list(Z=Zst), RNG[[1]])
  }
  inits1 <- make.inits()
  inits2 <- make.inits()
  inits3 <- make.inits()
  
  
  
  ## MCMC settings 
  n.burnin <- 1e3
  n.adapt  <- 1e3
  n.iter   <- 1e5
  n.thin   <- 1e2
  
  ## source JAGS model
  source(sprintf('Step02_Occupancy_Models/models/%s.R', model))
  model.txt <- sprintf('Step02_Occupancy_Models/models/%s.txt', model)
  write.model(model.jags, con=model.txt)
  
  res <- run.jags(model=model.txt,
                  monitor=get.params(),
                  data=my.data,
                  inits=list(inits1,inits2,inits3),
                  n.chains=3,
                  burnin=n.burnin,
                  sample=floor(n.iter/n.thin),
                  thin=n.thin,
                  adapt=n.adapt,
                  method='rjags')
  
  #make sure you create these subfolders, or else save your ouptput somewhere else
  saveRDS(res, paste0("Outputs/res_",taxonomic_group, "_",model, ".rds"),compress=TRUE)
  
  
  res.summary <-  add.summary(res)
  
  
  saveRDS(res.summary, compress = TRUE, ascii = FALSE, paste0("Outputs/res.summary_", taxonomic_group, "_",model, ".rds"))
  
}


# run_model(year_range=c(14,18), taxonomic_group='Lepidoptera', model='full_model_ind_spatial_all')


# Parallel execution of different models
# ------------------------------------------------------------------------------------------
# Define the inputs for the function
groups_and_models <- list(
  Phoridae = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural', 'full_model_ind_spatial_RH_Phorids'),
  Lepidoptera = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural'),
  Syrphidae = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural'),
  Tipulomorpha = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural'),
  Drosophilidae = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural'),
  Mycetophilidae = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural'),
  Araneae = c('full_model_ind_spatial_all', 'full_model_ind_spatial_LV_impervious', 'full_model_ind_spatial_oceaneffect', 'full_model_ind_spatial_meanTemp', 'full_model_ind_spatial_diurnalRange', 'full_model_ind_spatial_distancenatural')
)

# get all combos of models and group to be run for
combinations <- data.frame()
for (group in names(groups_and_models)) {
  models <- groups_and_models[[group]]
  combinations <- rbind(combinations, expand.grid(model = models, group = group))
}

# Define the number of cores to use for parallel processing
num_cores <- detectCores()

# Parallelize the function using mclapply
results <- mclapply(1:nrow(combinations), function(i) {
  params <- combinations[i, ]
  run_model(year_range=c(14,18), taxonomic_group=params$group, model=params$model)
}, mc.cores = num_cores)

# Print the results
print(results)
# ------------------------------------------------------------------------------------------

