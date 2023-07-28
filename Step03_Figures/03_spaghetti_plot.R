library(dplyr);library(stringr);library(ggplot2);library(cowplot);library(tidyr);library(RColorBrewer);library(data.table);library(RColorBrewer);library(gplots);library(abind);library(gridExtra);library(grid);library(ggpubr);library(purrr);library(PNWColors)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)

source("Step03_Figures/src/initialize.R")

# May need to change
system("clang++ -v")
runjags.options(jagspath="/opt/homebrew/Cellar/jags/4.3.1/bin/jags")
library(rjags)

####Import your model outputs####
##res
res_list_fun<-function(taxonomic_group){
  res_fun <- function(taxonomic_group,model){readRDS(paste0("Outputs/res_", paste0(taxonomic_group, "_",model, ".rds")))}
  
  imperv_res<-res_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  LV_res<-res_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  elevation_res<-res_fun(taxonomic_group,"full_model_ind_spatial_elevation")
  diurnalRange_res<-res_fun(taxonomic_group,"full_model_ind_spatial_diurnalRange")
  ocean_effect_res<-res_fun(taxonomic_group,"full_model_ind_spatial_oceaneffect")
  temp_res<-res_fun(taxonomic_group,"full_model_ind_spatial_meanTemp")
  #RH_res<-res_fun(taxonomic_group,"full_model_ind_spatial_RH_Phorids")
  summary<-list(imperv_res,LV_res,elevation_res,diurnalRange_res,ocean_effect_res,temp_res)
  #summary<-list(imperv_res,LV_res,elevation_res,diurnalRange_res,ocean_effect_res,temp_res,RH_res)
  
  return(summary)
}
#For Phroid only
res_list_fun_phorid_only<-function(taxonomic_group){
  res_fun <- function(taxonomic_group,model){readRDS(paste0("Ouptuts/res_", paste0(taxonomic_group, "_",model, ".rds")))}
  
  imperv_res<-res_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  LV_res<-res_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  elevation_res<-res_fun(taxonomic_group,"full_model_ind_spatial_elevation")
  diurnalRange_res<-res_fun(taxonomic_group,"full_model_ind_spatial_diurnalRange")
  ocean_effect_res<-res_fun(taxonomic_group,"full_model_ind_spatial_oceaneffect")
  temp_res<-res_fun(taxonomic_group,"full_model_ind_spatial_meanTemp")
  RH_res<-res_fun(taxonomic_group,"full_model_ind_spatial_RH_Phorids")
  summary<-list(imperv_res,LV_res,elevation_res,diurnalRange_res,ocean_effect_res,temp_res,RH_res)
  
  return(summary)
}

ara_res_list<-res_list_fun("Araneae")
dro_res_list<-res_list_fun("Drosophilidae")
lep_res_list<-res_list_fun("Lepidoptera")
myc_res_list<-res_list_fun("Mycetophilidae")
syr_res_list<-res_list_fun("Syrphidae")
tip_res_list<-res_list_fun("Tipulomorpha")
#Phorid only, need to run the function from line 29
pho_res_list<-res_list_fun_phorid_only("Phoridae")

##my.data
my.data_fun <- function(taxonomic_group){readRDS(paste0("Data/clean_data/data_prepared/my_data_",  paste0(year_range, collapse = "_"), "_", taxonomic_group, "_spatial.rds"))}
ara_my.data<-my.data_fun("Araneae")
dro_my.data<-my.data_fun("Drosophilidae")
lep_my.data<-my.data_fun("Lepidoptera")
myc_my.data<-my.data_fun("Mycetophilidae")
syr_my.data<-my.data_fun("Syrphidae")
tip_my.data<-my.data_fun("Tipulomorpha")
pho_my.data<-my.data_fun("Phoridae")


##res.summary
get.summ <- function(res.summary,pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}
res.summary_list_fun<-function(taxonomic_group){
  res.summary_fun <- function(taxonomic_group,model){readRDS(paste0("Outputs/res.summary_", paste0(taxonomic_group, "_",model, ".rds")))}
  
  imperv_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  LV_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  elevation_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_elevation")
  diurnalRange_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_diurnalRange")
  ocean_effect_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_oceaneffect")
  temp_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_meanTemp")
  #RH_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_RH_Phorids")
  summary<-list(imperv_res.summary,LV_res.summary,elevation_res.summary,diurnalRange_res.summary,ocean_effect_res.summary,temp_res.summary)
  #summary<-list(imperv_res.summary,LV_res.summary,elevation_res.summary,diurnalRange_res.summary,ocean_effect_res.summary,temp_res.summary,RH_res.summary)
  
  
  #extract reauired row names from res.summary
  imperv_vars <- rownames(summary[[1]]$psrf$psrf)
  LV_vars <- rownames(summary[[2]]$psrf$psrf)
  elevation_vars <- rownames(summary[[3]]$psrf$psrf)
  diurnalRange_vars <- rownames(summary[[4]]$psrf$psrf)
  ocean_effect_vars <- rownames(summary[[5]]$psrf$psrf)
  temp_vars <- rownames(summary[[6]]$psrf$psrf)
  #RH_vars <- rownames(summary[[7]]$psrf$psrf)
  var_list<-list(imperv_vars,LV_vars,elevation_vars,diurnalRange_vars,ocean_effect_vars,temp_vars)
  #var_list<-list(imperv_vars,LV_vars,elevation_vars,diurnalRange_vars,ocean_effect_vars,temp_vars,RH_vars)
  
  imperv_summ <- get.summ(summary[[1]],var_list[[1]])
  LV_summ <- get.summ(summary[[2]],var_list[[2]])
  elevation_summ <- get.summ(summary[[3]],var_list[[3]])
  diurnalRange_summ <- get.summ(summary[[4]],var_list[[4]])
  ocean_effect_summ <- get.summ(summary[[5]],var_list[[5]])
  temp_summ <- get.summ(summary[[6]],var_list[[6]])
  #RH_summ<- get.summ(summary[[7]],var_list[[7]])
  summ_list<-list(imperv_summ,LV_summ,elevation_summ,diurnalRange_summ,ocean_effect_summ,temp_summ)
  #summ_list<-list(imperv_summ,LV_summ,elevation_summ,diurnalRange_summ,ocean_effect_summ,temp_summ, RH_summ)
  
  return(summ_list)
  
}
res.summary_list_fun_phorid_only<-function(taxonomic_group){
  res.summary_fun <- function(taxonomic_group,model){readRDS(paste0("Outputs/res.summary_", paste0(taxonomic_group, "_",model, ".rds")))}
  
  imperv_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  LV_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  elevation_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_elevation")
  diurnalRange_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_diurnalRange")
  ocean_effect_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_oceaneffect")
  temp_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_meanTemp")
  RH_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_RH_Phorids")
  summary<-list(imperv_res.summary,LV_res.summary,elevation_res.summary,diurnalRange_res.summary,ocean_effect_res.summary,temp_res.summary,RH_res.summary)
  
  
  #extract reauired row names from res.summary
  imperv_vars <- rownames(summary[[1]]$psrf$psrf)
  LV_vars <- rownames(summary[[2]]$psrf$psrf)
  elevation_vars <- rownames(summary[[3]]$psrf$psrf)
  diurnalRange_vars <- rownames(summary[[4]]$psrf$psrf)
  ocean_effect_vars <- rownames(summary[[5]]$psrf$psrf)
  temp_vars <- rownames(summary[[6]]$psrf$psrf)
  RH_vars <- rownames(summary[[7]]$psrf$psrf)
  
  var_list<-list(imperv_vars,LV_vars,elevation_vars,diurnalRange_vars,ocean_effect_vars,temp_vars,RH_vars)
  
  imperv_summ <- get.summ(summary[[1]],var_list[[1]])
  LV_summ <- get.summ(summary[[2]],var_list[[2]])
  elevation_summ <- get.summ(summary[[3]],var_list[[3]])
  diurnalRange_summ <- get.summ(summary[[4]],var_list[[4]])
  ocean_effect_summ <- get.summ(summary[[5]],var_list[[5]])
  temp_summ <- get.summ(summary[[6]],var_list[[6]])
  RH_summ<- get.summ(summary[[7]],var_list[[7]])
  
  summ_list<-list(imperv_summ,LV_summ,elevation_summ,diurnalRange_summ,ocean_effect_summ,temp_summ, RH_summ)
  
  return(summ_list)
  
}

ara_res.summary_list<-res.summary_list_fun("Araneae")
dro_res.summary_list<-res.summary_list_fun("Drosophilidae")
lep_res.summary_list<-res.summary_list_fun("Lepidoptera")
myc_res.summary_list<-res.summary_list_fun("Mycetophilidae")
syr_res.summary_list<-res.summary_list_fun("Syrphidae")
tip_res.summary_list<-res.summary_list_fun("Tipulomorpha")
#Phorid only, need to run the function from line 101
pho_res.summary_list<-res.summary_list_fun_phorid_only("Phoridae")

#create a list of psi.env for each family group (6 parameters, 7 for Phorids)
psi_list_fun<-function(summ){
  psi_summary_fun<-function(summ,mu_psi_env,psi_env){
    species_trends<-summ %>%
      as.data.frame() %>%
      filter(!str_detect(rownames(summ), mu_psi_env)) #remove the row with mu_psi_env
    species_trends<-species_trends%>%
      as.matrix()
    species_trends <- cbind(data.frame(species_trends[str_detect(rownames(species_trends), psi_env),]))
    return(species_trends)
  }
  imperv_psi<-psi_summary_fun(summ[[1]],"mu.psi.imperv","psi.imperv")
  LV_psi<-psi_summary_fun(summ[[2]],"mu.psi.value","psi.value")
  elevation_psi<-psi_summary_fun(summ[[3]],"mu.psi.elevation","psi.elevation")
  diurnalRange_psi<-psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal","psi.temp.diurnal")
  ocean_effect_psi<-psi_summary_fun(summ[[5]],"mu.psi.ocean.effect","psi.ocean.effect")
  temp_psi<-psi_summary_fun(summ[[6]],"mu.psi.temp","psi.temp")
  
  psi_list<-list(imperv_psi,LV_psi,elevation_psi,diurnalRange_psi,ocean_effect_psi,temp_psi)
  
  return(psi_list)
}
psi_list_fun_phorid_only<-function(summ){
  psi_summary_fun<-function(summ,mu_psi_env,psi_env){
    species_trends<-summ %>%
      as.data.frame() %>%
      filter(!str_detect(rownames(summ), mu_psi_env)) #remove the row with mu_psi_env
    species_trends<-species_trends%>%
      as.matrix()
    species_trends <- cbind(data.frame(species_trends[str_detect(rownames(species_trends), psi_env),]))
    return(species_trends)
  }
  imperv_psi<-psi_summary_fun(summ[[1]],"mu.psi.imperv","psi.imperv")
  LV_psi<-psi_summary_fun(summ[[2]],"mu.psi.value","psi.value")
  elevation_psi<-psi_summary_fun(summ[[3]],"mu.psi.elevation","psi.elevation")
  diurnalRange_psi<-psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal","psi.temp.diurnal")
  ocean_effect_psi<-psi_summary_fun(summ[[5]],"mu.psi.ocean.effect","psi.ocean.effect")
  temp_psi<-psi_summary_fun(summ[[6]],"mu.psi.temp","psi.temp")
  RH_psi<-psi_summary_fun(summ[[7]],"mu.psi.RH","psi.RH")
  psi_list<-list(imperv_psi,LV_psi,elevation_psi,diurnalRange_psi,ocean_effect_psi,temp_psi,RH_psi)
  
  return(psi_list)
}
#create a list of mu.psi.env for each family group (6 parameters, 7 for Phorids))
mu_psi_list_fun<-function(summ){
  mu_psi_summary_fun<-function(summ,mu_psi_env){
    species_trends<-summ %>%
      as.data.frame() %>%
      filter(str_detect(rownames(summ), mu_psi_env))
    species_trends<-species_trends%>%
      as.matrix()
    return(species_trends)
  }
  imperv_mu_psi<-mu_psi_summary_fun(summ[[1]],"mu.psi.imperv")
  LV_mu_psi<-mu_psi_summary_fun(summ[[2]],"mu.psi.value")
  elevation_mu_psi<-mu_psi_summary_fun(summ[[3]],"mu.psi.elevation")
  diurnalRange_mu_psi<-mu_psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal")
  ocean_effect_mu_psi<-mu_psi_summary_fun(summ[[5]],"mu.psi.ocean.effect")
  temp_mu_psi<-mu_psi_summary_fun(summ[[6]],"mu.psi.temp")
  mu_psi_list<-list(imperv_mu_psi,LV_mu_psi,elevation_mu_psi,diurnalRange_mu_psi,ocean_effect_mu_psi,temp_mu_psi)
  
  return(mu_psi_list)
}
mu_psi_list_fun_phorid_only<-function(summ){
  mu_psi_summary_fun<-function(summ,mu_psi_env){
    species_trends<-summ %>%
      as.data.frame() %>%
      filter(str_detect(rownames(summ), mu_psi_env))
    species_trends<-species_trends%>%
      as.matrix()
    return(species_trends)
  }
  imperv_mu_psi<-mu_psi_summary_fun(summ[[1]],"mu.psi.imperv")
  LV_mu_psi<-mu_psi_summary_fun(summ[[2]],"mu.psi.value")
  elevation_mu_psi<-mu_psi_summary_fun(summ[[3]],"mu.psi.elevation")
  diurnalRange_mu_psi<-mu_psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal")
  ocean_effect_mu_psi<-mu_psi_summary_fun(summ[[5]],"mu.psi.ocean.effect")
  temp_mu_psi<-mu_psi_summary_fun(summ[[6]],"mu.psi.temp")
  RH_mu_psi<-mu_psi_summary_fun(summ[[7]],"mu.psi.RH")
  
  mu_psi_list<-list(imperv_mu_psi,LV_mu_psi,elevation_mu_psi,diurnalRange_mu_psi,ocean_effect_mu_psi,temp_mu_psi,RH_mu_psi)
  
  return(mu_psi_list)
}
#Araneae
ara_psi_list<-psi_list_fun(ara_res.summary_list)
ara_mu_psi_list<-mu_psi_list_fun(ara_res.summary_list)
#Drosophilidae
dro_psi_list<-psi_list_fun(dro_res.summary_list)
dro_mu_psi_list<-mu_psi_list_fun(dro_res.summary_list)
#Lepidoptera
lep_psi_list<-psi_list_fun(lep_res.summary_list)
lep_mu_psi_list<-mu_psi_list_fun(lep_res.summary_list)
#Mycetophilidae
myc_psi_list<-psi_list_fun(myc_res.summary_list)
myc_mu_psi_list<-mu_psi_list_fun(myc_res.summary_list)
#Syrphidae
syr_psi_list<-psi_list_fun(syr_res.summary_list)
syr_mu_psi_list<-mu_psi_list_fun(syr_res.summary_list)
#Tipulomorpha
tip_psi_list<-psi_list_fun(tip_res.summary_list)
tip_mu_psi_list<-mu_psi_list_fun(tip_res.summary_list)
#Phoridae
pho_psi_list<-psi_list_fun_phorid_only(pho_res.summary_list)
pho_mu_psi_list<-mu_psi_list_fun_phorid_only(pho_res.summary_list)

#combine the list to become a dataframe and and an parameter column
psi_matrix_fun<-function(psi_list,group){
  psi_matrix<-psi_list%>%setattr(., 'names', c("Impervious Surface","Land Value","Elevation","Diurnal Range","Ocean Effect","Temperature"))%>%
    rbindlist(.,idcol = "parameter")%>%
    cbind(., group=group)
  return(psi_matrix)}
mu_psi_matrix_fun<-function(mu_psi_list,group){
  mu_psi_matrix<-as.data.frame(do.call(rbind,mu_psi_list))%>%
    mutate(parameter=c("Impervious Surface","Land Value","Elevation","Diurnal Range","Ocean Effect","Temperature"))%>%
    mutate(group=group)
  return(mu_psi_matrix)}
#Araneae
ara_psi<-psi_matrix_fun(ara_psi_list,"Araneae")
ara_mu_psi<-mu_psi_matrix_fun(ara_mu_psi_list,"Araneae")
#Drosophilidae
dro_psi<-psi_matrix_fun(dro_psi_list,"Drosophilidae")
dro_mu_psi<-mu_psi_matrix_fun(dro_mu_psi_list,"Drosophilidae")
#Lepidoptera
lep_psi<-psi_matrix_fun(lep_psi_list,"Lepidoptera")
lep_mu_psi<-mu_psi_matrix_fun(lep_mu_psi_list,"Lepidoptera")
#Mycetophilidae
myc_psi<-psi_matrix_fun(myc_psi_list,"Mycetophilidae")
myc_mu_psi<-mu_psi_matrix_fun(myc_mu_psi_list,"Mycetophilidae")
#Syrphidae
syr_psi<-psi_matrix_fun(syr_psi_list,"Syrphidae")
syr_mu_psi<-mu_psi_matrix_fun(syr_mu_psi_list,"Syrphidae")
#Tipulomorpha
tip_psi<-psi_matrix_fun(tip_psi_list,"Tipulomorpha")
tip_mu_psi<-mu_psi_matrix_fun(tip_mu_psi_list,"Tipulomorpha")
#Phoridae
#Without relative humidity
pho_psi<-pho_psi_list[-7]%>%setattr(., 'names', c("Impervious Surface","Land Value","Elevation","Diurnal Range","Ocean Effect","Temperature"))%>%
  rbindlist(.,idcol = "parameter")%>%
  cbind(., group="Phoridae")
pho_mu_psi<-as.data.frame(do.call(rbind, pho_mu_psi_list[-7]))%>%
  mutate(parameter=c("Impervious Surface","Land Value","Elevation","Diurnal Range","Ocean Effect","Temperature"))%>%
  mutate(group="Phoridae")

##psi value summary
psi_summary_all<-rbind(ara_psi,dro_psi,lep_psi,myc_psi,syr_psi,tip_psi,pho_psi)
##mu psi value summary
mu_psi_summary_all<-rbind(ara_mu_psi,dro_mu_psi,lep_mu_psi,myc_mu_psi,syr_mu_psi,tip_mu_psi,pho_mu_psi)



##### Spaghetti plots #########

### assign correct chains ## ????
chains_fun<-function(my.data){
  nsite <- as.integer(my.data[[1]]$nsite)
  nsp <- as.integer(my.data[[1]]$nsp)
  nseason <- as.integer(my.data[[1]]$nseason)
  nvisit <- as.integer(my.data[[1]]$nvisit)}
#chains_fun(lep_my.data)

#Look at each species individually
#sim.mat
sims.mat_fun<- function(res,x){do.call(rbind, res[[x]]$mcmc)}
sims.mat_list_fun<-function(res){
  sims.mat_list<-list()
  for (i in 1:length(res)){
    sims.mat_list[[i]]<-sims.mat_fun(res,i)
  }
  return(sims.mat_list)
}
ara_sims.mat<-sims.mat_list_fun(ara_res_list)
dro_sims.mat<-sims.mat_list_fun(dro_res_list)
lep_sims.mat<-sims.mat_list_fun(lep_res_list)
myc_sims.mat<-sims.mat_list_fun(myc_res_list)
syr_sims.mat<-sims.mat_list_fun(syr_res_list)
tip_sims.mat<-sims.mat_list_fun(tip_res_list)
pho_sims.mat<-sims.mat_list_fun(pho_res_list)

### species specific function 
get.y.val <- function(sims.mat, index, ss, imp, lv, elev, dr, oe, tmp) {
  
  if(index == 1){
  
  chains <- expit(sims.mat[[index]][,'mu.psi.0']+
                  sims.mat[[index]][,sprintf('psi.sp[%d]', ss)]+
                  sims.mat[[index]][,sprintf('psi.imperv[%s]', ss)]*imp+
                  sims.mat[[index]][,sprintf('psi.value[%s]', ss)]*lv+
                  sims.mat[[index]][,sprintf('psi.elevation[%s]', ss)]*elev+
                  sims.mat[[index]][,sprintf('psi.ocean.effect[%s]', ss)]*oe)

  data.table(data.frame(ss, imp, lv, elev, oe,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
  
  }else if(index == 2){

    chains <- expit(sims.mat[[index]][,'mu.psi.0']+
                    sims.mat[[index]][,sprintf('psi.sp[%d]', ss)]+
                    sims.mat[[index]][,sprintf('psi.imperv[%s]', ss)]*imp+
                    sims.mat[[index]][,sprintf('psi.value[%s]', ss)]*lv+
                    sims.mat[[index]][,sprintf('psi.elevation[%s]', ss)]*elev+
                    sims.mat[[index]][,sprintf('psi.ocean.effect[%s]', ss)]*oe)
    
    data.table(data.frame(ss, imp, lv, elev, oe,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
    
  }else if(index == 3){
    
    chains <- expit(sims.mat[[index]][,'mu.psi.0']+
                      sims.mat[[index]][,sprintf('psi.sp[%d]', ss)]+
                      sims.mat[[index]][,sprintf('psi.elevation[%s]', ss)]*elev+
                      sims.mat[[index]][,sprintf('psi.ocean.effect[%s]', ss)]*oe)
                    
    data.table(data.frame(ss, elev, oe,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
    
  }else if(index == 4){
    
    chains <- expit(sims.mat[[index]][,'mu.psi.0']+
                    sims.mat[[index]][,sprintf('psi.sp[%d]', ss)]+
                    sims.mat[[index]][,sprintf('psi.temp.diurnal[%s]', ss)]*dr+
                    sims.mat[[index]][,sprintf('psi.ocean.effect[%s]', ss)]*oe)
    
    data.table(data.frame(ss, dr, oe,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
    
  }else if(index == 5){
    
    chains <- expit(sims.mat[[index]][,'mu.psi.0']+
                    sims.mat[[index]][,sprintf('psi.sp[%d]', ss)]+
                    sims.mat[[index]][,sprintf('psi.ocean.effect[%s]', ss)]*oe)
    
    data.table(data.frame(ss, oe,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
    
    
  }else if(index == 6){ 
    
    chains <- expit(sims.mat[[index]][,'mu.psi.0']+
                    sims.mat[[index]][,sprintf('psi.sp[%d]', ss)]+
                      sims.mat[[index]][,sprintf('psi.temp[%s]', ss)]*tmp)
    data.table(data.frame(ss, tmp,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
  }
}
    
get.y.val.all <- function(sims.mat, ev, ss, my.data){
  
  if(ev == "Impervious Surface"){
    
    i <- seq(from=min(my.data[[1]]$imperv),  
             to=  max(my.data[[1]]$imperv),
             length.out=1000)
    l <- mean(my.data[[1]]$landvalue)
    e <- mean(my.data[[1]]$elevation)
    d <- mean(my.data[[1]]$Temp_diurnal)
    o <- mean(my.data[[1]]$ocean_effect)
    t <-mean(my.data[[1]]$Temp)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(i), function(x) get.y.val(sims.mat, index=1, ss = ss, imp = i[x], lv = l, elev = e, dr=d, oe=o, tmp=t)))
    
    
  }else if(ev == 'Land Value'){
    
    i <- mean(my.data[[1]]$imperv)
    l <- seq(from=min(my.data[[1]]$landvalue),  
             to=  max(my.data[[1]]$landvalue),
             length.out=1000)
    e <- mean(my.data[[1]]$elevation)
    d <- mean(my.data[[1]]$Temp_diurnal)
    o <- mean(my.data[[1]]$ocean_effect)
    t <-mean(my.data[[1]]$Temp)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(l), function(x) get.y.val(sims.mat, index=2, ss = ss, imp = i, lv = l[x], elev = e, dr=d, oe=o, tmp=t)))
    
  }else if(ev == 'Elevation'){
    
    i <- mean(my.data[[1]]$imperv)
    l <- mean(my.data[[1]]$landvalue)
    e <- seq(from=min(my.data[[1]]$elevation),  
             to=  max(my.data[[1]]$elevation),
             length.out=1000)
    d <- mean(my.data[[1]]$Temp_diurnal)
    o <- mean(my.data[[1]]$ocean_effect)
    t <-mean(my.data[[1]]$Temp)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(e), function(x) get.y.val(sims.mat, index=3, ss = ss, imp = i, lv = l, elev = e[x], dr=d, oe=o, tmp=t)))
    
    
  }else if(ev == 'Diurnal Range'){
    i <- mean(my.data[[1]]$imperv)
    l <- mean(my.data[[1]]$landvalue)
    e <- mean(my.data[[1]]$elevation)
    d <- seq(from=min(my.data[[1]]$Temp_diurnal),  
             to=  max(my.data[[1]]$Temp_diurnal),
             length.out=1000)
    o <- mean(my.data[[1]]$ocean_effect)
    t <-mean(my.data[[1]]$Temp)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(d), function(x) get.y.val(sims.mat, index=4, ss = ss, imp = i, lv = l, elev = e, dr=d[x], oe=o, tmp=t)))  
    
  }else if(ev == 'Ocean Effect'){
    i <- mean(my.data[[1]]$imperv)
    l <- mean(my.data[[1]]$landvalue)
    e <- mean(my.data[[1]]$elevation)
    d <- mean(my.data[[1]]$Temp_diurnal)
    o <- seq(from=min(my.data[[1]]$ocean_effect),  
             to=  max(my.data[[1]]$ocean_effect),
             length.out=1000)
    t <-mean(my.data[[1]]$Temp)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(o), function(x) get.y.val(sims.mat, index=5, ss = ss, imp = i, lv = l, elev = e, dr=d, oe=o[x], tmp=t)))  
    
    
  }else if(ev == 'Temperature'){
    i <- mean(my.data[[1]]$imperv)
    l <- mean(my.data[[1]]$landvalue)
    e <- mean(my.data[[1]]$elevation)
    d <- mean(my.data[[1]]$Temp_diurnal)
    o <- mean(my.data[[1]]$ocean_effect)
    t <-seq(from=min(my.data[[1]]$Temp),  
            to=  max(my.data[[1]]$Temp),
            length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t), function(x) get.y.val(sims.mat, index=6, ss = ss, imp = i, lv = l, elev = e, dr=d, oe=o, tmp=t[x]))) }
  
  return(one_sp_ev_occ)
}


sp_occupancy_env_summary_fun<-function(sims.mat,my.data){
  sp_occupancy_fun <- function(env){lapply(1:as.integer(my.data[[1]]$nsp), FUN = function(x) get.y.val.all(sims.mat = sims.mat, ss = x, ev=env, my.data = my.data))}
  sp_imperv<-sp_occupancy_fun('Impervious Surface')
  sp_lv<-sp_occupancy_fun('Land Value')
  sp_elev<-sp_occupancy_fun('Elevation')
  sp_dr<-sp_occupancy_fun('Diurnal Range')
  sp_oe<-sp_occupancy_fun('Ocean Effect')
  sp_tmp<-sp_occupancy_fun('Temperature')
  sp_summary<-list(sp_imperv,sp_lv,sp_elev,sp_dr,sp_oe,sp_tmp) 
  return(sp_summary)
}

ara_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(ara_sims.mat,ara_my.data)
dro_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(dro_sims.mat,dro_my.data)
lep_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(lep_sims.mat,lep_my.data)
myc_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(myc_sims.mat,myc_my.data)
syr_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(syr_sims.mat,syr_my.data)
tip_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(tip_sims.mat,tip_my.data)
pho_sp_occupancy_env_summary<-sp_occupancy_env_summary_fun(pho_sims.mat,pho_my.data)


species_trends_summary_fun<-function(psi_list, my.data){
  species_trends_sig<- list()
  for (i in 1:length(psi_list)){
    species_trends_sig[[i]] <-psi_list[[i]]%>%
      mutate(
        sig = case_when(
          `X2.5.` < 0 & `X97.5.`< 0 ~ "negative",
          `X2.5.` > 0 & `X97.5.`> 0 ~ "positive",
          `X2.5.` < 0 & `X97.5.`> 0 ~ "not significant"))%>%
      cbind(., species = my.data$sp)

    
    }
  return(species_trends_sig)
  }

ara_species_trends_summary<-species_trends_summary_fun(ara_psi_list,ara_my.data)
dro_species_trends_summary<-species_trends_summary_fun(dro_psi_list,dro_my.data)
lep_species_trends_summary<-species_trends_summary_fun(lep_psi_list)
myc_species_trends_summary<-species_trends_summary_fun(myc_psi_list)
syr_species_trends_summary<-species_trends_summary_fun(syr_psi_list)
tip_species_trends_summary<-species_trends_summary_fun(tip_psi_list)
pho_species_trends_summary<-species_trends_summary_fun(pho_psi_list,pho_my.data)

sp_occupancy_merged_list_fun<-function(sp_occupancy_env_summary,species_trends_summary){
  
  joined_sp_list<- list()
  species_env_trends<-list()
  species_trends<-list()
  
  for(i in 1:length(sp_occupancy_env_summary)){
    
   species_trends[[i]] <- sp_occupancy_env_summary[[i]] %>% 
      rbindlist()
    
    species_env_trends[[i]] <- species_trends_summary[[i]] %>% 
      select(species, sig) %>% 
      tibble::rownames_to_column("ss") %>% 
      mutate(ss = str_extract(ss, "\\[\\d+")) %>% 
      mutate(ss = str_remove(ss, "\\[")) %>% 
      mutate(ss = as.numeric(ss))
    
    joined_sp_list[[i]] <- species_env_trends[[i]] %>% 
      left_join(species_trends[[i]])
    
    
  }
  return(joined_sp_list)
}

pho_sp_occupancy_merged_list<-sp_occupancy_merged_list_fun(pho_sp_occupancy_env_summary,pho_species_trends_summary)

#rmb to convert back to real value for rach parameter. step 2 , figure 2
#the plots (need data to change the x axis back to real value! Multiply the scaled data by the standard deviation of the original data and add the mean of the original data.) Change the axis value position to central.
sp_plot_fun <- function(data, parameter, x_title, y_title){
    ggplot() +
      geom_line(data = data, aes_string(x = parameter, y = "mean", group= "ss", color="sig")) +
      #geom_line(data = main_trend_value$data, aes_string(x = "tt", y = "mean"), colour = 'black') +
      scale_color_manual(values = c("positive" = "blue",
                                    "negative"="red",
                                    "not significant"="grey"),name="Trend")+
      theme_cowplot()+
      theme(legend.position="",
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            plot.background=element_rect(fill="white"),
            #strip.background =element_blank(), 
            axis.text = element_text(angle = 90)) +
      ylab(y_title) +
      xlab(x_title)   }
sp_plot_fun_without_y_axis_legend <- function(data, parameter, x_title, y_title){ggplot() +
      geom_line(data = data, aes_string(x = parameter, y = "mean", group= "ss", color="sig")) +
      #geom_line(data = main_trend_value$data, aes_string(x = "tt", y = "mean"), colour = 'black') +
      scale_color_manual(values = c("positive" = "blue",
                                    "negative"="red",
                                    "not significant"="grey"),name="Trend")+
      theme_cowplot()+
      theme(legend.position="",
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            plot.background=element_rect(fill="white"),
            #strip.background =element_blank(), 
            axis.text = element_text(angle = 90)) +
      ylab(y_title) +
      xlab(x_title)   
}

#For phorid, 0.25km is used in both impervious surface and land value
pho_sp_occupancy_plot_imperv<-ggplot() +geom_line(data = pho_sp_occupancy_merged_list[[1]], 
                              aes_string(x = pho_sp_occupancy_merged_list[[1]]$imp*15.07341+55.64427, y =     "mean", group= pho_sp_occupancy_merged_list[[1]]$ss, color="sig")) +
                              scale_color_manual(values = c("positive" = "blue","negative"="red", 
                              "not significant"="grey"),name="Trend")+
                              theme_cowplot()+
                              theme(legend.position="",
                              plot.background=element_rect(fill="white"),
                              axis.text = element_text(angle = 90),
                              axis.text.x = element_text(angle = 0)) +
                              ylab("Occupancy of Phoridae") +
                              xlab("Percentage of Impervious Surface")+
  scale_y_continuous(limit=c(0,1),labels=function(x) sprintf("%.1f", x))
  

pho_sp_occupancy_plot_lv<-ggplot() +geom_line(data = pho_sp_occupancy_merged_list[[2]],aes_string(x =10^(pho_sp_occupancy_merged_list[[2]]$lv*0.3323909+5.705213)-1, y ="mean", group= "ss", color="sig")) +
  scale_color_manual(values = c("positive" = "blue","negative"="red", 
                                "not significant"="grey"),name="Trend")+
  theme_cowplot()+
  theme(legend.position="",
        plot.background=element_rect(fill="white"),
        axis.text = element_text(angle = 90),
        axis.text.x = element_text(angle = 0)) +
  ylab("") +
  scale_x_continuous(breaks=c(1e3,1e7,2e7))+
  scale_y_continuous(limit=c(0,1),labels=function(x) sprintf("%.1f", x))+ 
  xlab("Land Value (U.S. Dollar / ha)")

pho_sp_occupancy_plot_elev<-ggplot() +
  geom_line(data = pho_sp_occupancy_merged_list[[3]], 
            aes_string(x =pho_sp_occupancy_merged_list[[3]]$elev*101.927+151.7101, y ="mean", group= "ss", color="sig")) +
  scale_color_manual(values = c("positive" = "blue","negative"="red", 
                                "not significant"="grey"),name="Trend")+
  theme_cowplot()+
  theme(legend.position="",
        plot.background=element_rect(fill="white"),
        axis.text = element_text(angle = 90),
        axis.text.x = element_text( angle = 0)) +
  ylab("") +
  xlab("Elevation (meter)")+
  scale_y_continuous(limit=c(0,1),labels=function(x) sprintf("%.1f", x))

pho_sp_occupancy_plot_dr<-ggplot() +
  geom_line(data = pho_sp_occupancy_merged_list[[4]], 
            aes_string(x =pho_sp_occupancy_merged_list[[4]]$dr*5.01597+23.19125, y ="mean", group= "ss", color="sig")) +
  scale_color_manual(values = c("positive" = "blue","negative"="red", 
                                "not significant"="grey"),name="Trend")+
  theme_cowplot()+
  theme(legend.position="",
        plot.background=element_rect(fill="white"),
        axis.text = element_text(angle = 90),
        axis.text.x = element_text( angle = 0)) +
  ylab("Occupancy of Phoridae") +
  xlab("Diurnal Range (°C)")+
  scale_y_continuous(limit=c(0,1),labels=function(x) sprintf("%.1f", x))

pho_sp_occupancy_plot_oe<-ggplot() +
  geom_line(data = pho_sp_occupancy_merged_list[[5]], 
            aes_string(x =10^(pho_sp_occupancy_merged_list[[5]]$oe*0.06151302+0.120149)-1, y ="mean", group= "ss", color="sig")) +
  scale_color_manual(values = c("positive" = "blue","negative"="red", 
                                "not significant"="grey"),name="Trend")+
  theme_cowplot()+
  theme(legend.position="",
        plot.background=element_rect(fill="white"),
        axis.text = element_text(angle = 90),
        axis.text.x = element_text( angle = 0)) +
  ylab("") +
  xlab("Ocean Effect")+
  scale_x_continuous(labels=function(x) sprintf("%.1f", x))+
  scale_y_continuous(limit=c(0,1),labels=function(x) sprintf("%.1f", x))

pho_sp_occupancy_plot_temp<-ggplot() +
  geom_line(data = pho_sp_occupancy_merged_list[[6]], 
            aes_string(x =pho_sp_occupancy_merged_list[[6]]$tmp*0.8750243+19.02678, y ="mean", group= "ss", color="sig")) +
  scale_color_manual(values = c("positive" = "blue","negative"="red", 
                                "not significant"="grey"),name="Trend")+
  theme_cowplot()+
  theme(legend.position="",
        plot.background=element_rect(fill="white"),
        axis.text = element_text(angle = 90),
        axis.text.x = element_text( angle = 0)) +
  ylab("") +
  xlab("Temperature (°C)")+
  scale_y_continuous(limit=c(0,1),labels=function(x) sprintf("%.1f", x))
#a function to extract a legend from a ggplot
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#extract the common legend
spat_legend_plot<-ggplot() +
  geom_line(data = pho_sp_occupancy_merged_list[[6]], 
            aes_string(x =pho_sp_occupancy_merged_list[[6]]$tmp*0.8750243+19.02678, y ="mean", group= "ss", color="sig")) +
  scale_color_manual(values = c("positive" = "blue","negative"="red", 
                                "not significant"="grey"),name="Trend", labels=c("Negative","Not Significant","Positive"))+
  theme_cowplot()+
  theme(legend.direction="horizontal",
        plot.background=element_rect(fill="white"),
        axis.text = element_text(angle = 90),
        legend.text = element_text(size = 15),
        legend.title = element_text(size =15))
#the common legend
spat_legend<-get_legend(spat_legend_plot)


pho_spaghetti_plot<-ggarrange(pho_sp_occupancy_plot_imperv,pho_sp_occupancy_plot_lv,pho_sp_occupancy_plot_elev,pho_sp_occupancy_plot_dr,pho_sp_occupancy_plot_oe,pho_sp_occupancy_plot_temp,spat_legend,nrow=3,ncol = 3,widths = c(8,8,8), heights = c(8,8,5),labels=c("a","b","c","d","e","f",""))

ggsave2("Step03_Figures/plots/pho_spaghetti_plot.png",plot=pho_spaghetti_plot, dpi=400, height=10, width=15, bg="white")

