library(dplyr);library(stringr);library(ggplot2);library(cowplot);library(tidyr);library(RColorBrewer);library(data.table);library(RColorBrewer);library(gplots);library(abind);library(gridExtra);library(grid);library(ggpubr);library(purrr);library(PNWColors)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- "/Users/jennycheung/Documents/phd_study/BioSCAN_2023"

setwd(my_working_dir)

source("Step03_Figures/src/initialize.R")

# May need to change
system("clang++ -v")
runjags.options(jagspath="/opt/homebrew/Cellar/jags/4.3.1/bin/jags")
library(rjags)
####Import your model outputs (res.summary) and create a summary list for each family group####
#function to extract useful columns from res.summary
get.summ <- function(res.summary,pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}
#read in the outputs by family name
res.summary_list_fun<-function(taxonomic_group){
  res.summary_fun <- function(taxonomic_group,model){readRDS(paste0("model_outputs/res.summary_", paste0(taxonomic_group, "_",model, ".rds")))}
  
  imperv_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  LV_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  dtn_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_distancenatural")
  diurnalRange_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_diurnalRange")
  ocean_effect_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_oceaneffect")
  temp_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_meanTemp")
  summary<-list(imperv_res.summary,LV_res.summary,dtn_res.summary,diurnalRange_res.summary,ocean_effect_res.summary,temp_res.summary)
  
  
  #extract reauired row names from res.summary
  imperv_vars <- rownames(summary[[1]]$psrf$psrf)
  LV_vars <- rownames(summary[[2]]$psrf$psrf)
  dtn_vars <- rownames(summary[[3]]$psrf$psrf)
  diurnalRange_vars <- rownames(summary[[4]]$psrf$psrf)
  ocean_effect_vars <- rownames(summary[[5]]$psrf$psrf)
  temp_vars <- rownames(summary[[6]]$psrf$psrf)
  var_list<-list(imperv_vars,LV_vars,dtn_vars,diurnalRange_vars,ocean_effect_vars,temp_vars)
  
  imperv_summ <- get.summ(summary[[1]],var_list[[1]])
  LV_summ <- get.summ(summary[[2]],var_list[[2]])
  dtn_summ <- get.summ(summary[[3]],var_list[[3]])
  diurnalRange_summ <- get.summ(summary[[4]],var_list[[4]])
  ocean_effect_summ <- get.summ(summary[[5]],var_list[[5]])
  temp_summ <- get.summ(summary[[6]],var_list[[6]])
  summ_list<-list(imperv_summ,LV_summ,dtn_summ,diurnalRange_summ,ocean_effect_summ,temp_summ)
  
  return(summ_list)
  
}
res.summary_list_fun_phorid_only<-function(taxonomic_group){
  res.summary_fun <- function(taxonomic_group,model){readRDS(paste0("model_outputs/res.summary_", paste0(taxonomic_group, "_",model, ".rds")))}
  
  imperv_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  LV_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_LV_impervious")
  dtn_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_distancenatural")
  diurnalRange_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_diurnalRange")
  ocean_effect_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_oceaneffect")
  temp_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_meanTemp")
  RH_res.summary<-res.summary_fun(taxonomic_group,"full_model_ind_spatial_RH_Phorids")
  summary<-list(imperv_res.summary,LV_res.summary,dtn_res.summary,diurnalRange_res.summary,ocean_effect_res.summary,temp_res.summary,RH_res.summary)
  
  
  #extract reauired row names from res.summary
  imperv_vars <- rownames(summary[[1]]$psrf$psrf)
  LV_vars <- rownames(summary[[2]]$psrf$psrf)
  dtn_vars <- rownames(summary[[3]]$psrf$psrf)
  diurnalRange_vars <- rownames(summary[[4]]$psrf$psrf)
  ocean_effect_vars <- rownames(summary[[5]]$psrf$psrf)
  temp_vars <- rownames(summary[[6]]$psrf$psrf)
  RH_vars <- rownames(summary[[7]]$psrf$psrf)
  
  var_list<-list(imperv_vars,LV_vars,dtn_vars,diurnalRange_vars,ocean_effect_vars,temp_vars,RH_vars)
  
  imperv_summ <- get.summ(summary[[1]],var_list[[1]])
  LV_summ <- get.summ(summary[[2]],var_list[[2]])
  dtn_summ <- get.summ(summary[[3]],var_list[[3]])
  diurnalRange_summ <- get.summ(summary[[4]],var_list[[4]])
  ocean_effect_summ <- get.summ(summary[[5]],var_list[[5]])
  temp_summ <- get.summ(summary[[6]],var_list[[6]])
  RH_summ<- get.summ(summary[[7]],var_list[[7]])
  
  summ_list<-list(imperv_summ,LV_summ,dtn_summ,diurnalRange_summ,ocean_effect_summ,temp_summ, RH_summ)
  
  return(summ_list)
  
}



ara_res.summary_list<-res.summary_list_fun("Araneae")
dro_res.summary_list<-res.summary_list_fun("Drosophilidae")
myc_res.summary_list<-res.summary_list_fun("Mycetophilidae")
syr_res.summary_list<-res.summary_list_fun("Syrphidae")
tip_res.summary_list<-res.summary_list_fun("Tipulomorpha")
#Phorid only, need to run the function from line 61
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
  #elevation_psi<-psi_summary_fun(summ[[3]],"mu.psi.elevation","psi.elevation")
  dtn_psi<-psi_summary_fun(summ[[3]],"mu.psi.distance.natural","psi.distance.natural")
  diurnalRange_psi<-psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal","psi.temp.diurnal")
  ocean_effect_psi<-psi_summary_fun(summ[[5]],"mu.psi.ocean.effect","psi.ocean.effect")
  temp_psi<-psi_summary_fun(summ[[6]],"mu.psi.temp","psi.temp")
  
  psi_list<-list(imperv_psi,LV_psi,dtn_psi,diurnalRange_psi,ocean_effect_psi,temp_psi)
  
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
  #elevation_psi<-psi_summary_fun(summ[[3]],"mu.psi.elevation","psi.elevation")
  dtn_psi<-psi_summary_fun(summ[[3]],"mu.psi.distance.natural","psi.distance.natural")
  diurnalRange_psi<-psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal","psi.temp.diurnal")
  ocean_effect_psi<-psi_summary_fun(summ[[5]],"mu.psi.ocean.effect","psi.ocean.effect")
  temp_psi<-psi_summary_fun(summ[[6]],"mu.psi.temp","psi.temp")
  RH_psi<-psi_summary_fun(summ[[7]],"mu.psi.RH","psi.RH")
  psi_list<-list(imperv_psi,LV_psi,dtn_psi,diurnalRange_psi,ocean_effect_psi,temp_psi,RH_psi)
  
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
  #elevation_mu_psi<-mu_psi_summary_fun(summ[[3]],"mu.psi.elevation")
  dtn_mu_psi<-mu_psi_summary_fun(summ[[3]],"mu.psi.distance.natural")
  diurnalRange_mu_psi<-mu_psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal")
  ocean_effect_mu_psi<-mu_psi_summary_fun(summ[[5]],"mu.psi.ocean.effect")
  temp_mu_psi<-mu_psi_summary_fun(summ[[6]],"mu.psi.temp")
  mu_psi_list<-list(imperv_mu_psi,LV_mu_psi,dtn_mu_psi,diurnalRange_mu_psi,ocean_effect_mu_psi,temp_mu_psi)
  
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
  #elevation_mu_psi<-mu_psi_summary_fun(summ[[3]],"mu.psi.elevation")
  dtn_mu_psi<-mu_psi_summary_fun(summ[[3]],"mu.psi.distance.natural")
  diurnalRange_mu_psi<-mu_psi_summary_fun(summ[[4]],"mu.psi.temp.diurnal")
  ocean_effect_mu_psi<-mu_psi_summary_fun(summ[[5]],"mu.psi.ocean.effect")
  temp_mu_psi<-mu_psi_summary_fun(summ[[6]],"mu.psi.temp")
  RH_mu_psi<-mu_psi_summary_fun(summ[[7]],"mu.psi.RH")
  
  mu_psi_list<-list(imperv_mu_psi,LV_mu_psi,dtn_mu_psi,diurnalRange_mu_psi,ocean_effect_mu_psi,temp_mu_psi,RH_mu_psi)
  
  return(mu_psi_list)
}
#Araneae
ara_psi_list<-psi_list_fun(ara_res.summary_list)
ara_mu_psi_list<-mu_psi_list_fun(ara_res.summary_list)
#Drosophilidae
dro_psi_list<-psi_list_fun(dro_res.summary_list)
dro_mu_psi_list<-mu_psi_list_fun(dro_res.summary_list)
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
  psi_matrix<-psi_list%>%setattr(., 'names', c("Impervious Surface","Land Value","Distance to Natural Area","Diurnal Range","Ocean Effect","Temperature"))%>%
    rbindlist(.,idcol = "parameter")%>%
    cbind(., group=group)
  return(psi_matrix)}
mu_psi_matrix_fun<-function(mu_psi_list,group){
  mu_psi_matrix<-as.data.frame(do.call(rbind,mu_psi_list))%>%
    mutate(parameter=c("Impervious Surface","Land Value","Distance to Natural Area","Diurnal Range","Ocean Effect","Temperature"))%>%
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
pho_psi<-pho_psi_list[-7]%>%setattr(., 'names', c("Impervious Surface","Land Value","Distance to Natural Area","Diurnal Range","Ocean Effect","Temperature"))%>%
  rbindlist(.,idcol = "parameter")%>%
  cbind(., group="Phoridae")
pho_mu_psi<-as.data.frame(do.call(rbind, pho_mu_psi_list[-7]))%>%
  mutate(parameter=c("Impervious Surface","Land Value","Distance to Natural Area","Diurnal Range","Ocean Effect","Temperature"))%>%
  mutate(group="Phoridae")

##psi value summary
psi_summary_all<-rbind(ara_psi,dro_psi,myc_psi,syr_psi,tip_psi,pho_psi)%>%
  mutate(group=str_replace(group,"Tipulomorpha","Tipuloidea"))

##mu psi value summary
mu_psi_summary_all<-rbind(ara_mu_psi,dro_mu_psi,myc_mu_psi,syr_mu_psi,tip_mu_psi,pho_mu_psi)%>%
  mutate(group=str_replace(group,"Tipulomorpha","Tipuloidea"))

####violin plot####
##setting up the color for the seven insect groups
pal <- pnw_palette("Bay",7)
# a function to extract the legend of a ggplot
get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

####prepare data for plots####
#without Araneae and Mycetophilidae
psi_summary_minus_2<-psi_summary_all%>%
  filter(group!=c("Araneae"))%>%
  filter(group!=c("Mycetophilidae"))

mu_psi_summary_minus_2<-mu_psi_summary_all%>%
  filter(group!=c("Araneae"))%>%
  filter(group!=c("Mycetophilidae"))%>%
  as.data.frame()

#only Araneae and Mycetophilidae
psi_summary_ara_myc<-psi_summary_all%>%
  filter(group=="Araneae"|group=="Mycetophilidae")

mu_psi_summary_ara_myc<-mu_psi_summary_all%>%
  as.data.frame()%>%
  filter(group=="Araneae"|group=="Mycetophilidae")
##Create a list to add the significance label to the plot
signif_label_1 <- data.frame(group = c("Drosophilidae", "Phoridae","Phoridae","Syrphidae"),
                       parameter=c("Distance to Natural Area","Land Value","Diurnal Range","Temperature"),
                       value = c(0.5, 0.5, 0.5,0.5))

signif_label_2 <- data.frame(group = c("Mycetophilidae"),
                             parameter=c("Temperature"),
                             value = c(7.2))

####Plot functions####
#First row, without x-axis labels
violin_plot_fun1<-function(psi.data,mu.psi.data,variable, y_title){
  ggplot(psi.data[which(psi.data$parameter==variable), ], mapping=aes(x=group, y = mean, group=group,color = group))+ 
    geom_violin()+ 
    geom_jitter(width= 0.1,size=0.5)+
    scale_color_manual(name="Group",values=c("#59A082","#EDA417","#E7720B","#DD4124"))+
    geom_point(mu.psi.data[which(mu.psi.data$parameter==variable), ],mapping=aes(x=group, y=mean),colour = "black",size=0.8)+
    geom_linerange(mu.psi.data[which(mu.psi.data$parameter==variable),], mapping=aes(ymin=mu.psi.data[which(mu.psi.data$parameter==variable),]$`2.5%`, ymax=mu.psi.data[which(mu.psi.data$parameter==variable), ]$`97.5`),colour="black")+
    geom_hline(yintercept=0,linetype=2,colour="grey")+
    theme_cowplot()+
    theme(legend.position="",
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          plot.background=element_rect(fill="white"),
          strip.background =element_rect(fill="white"),
          axis.ticks.x=element_blank(),
    ) +
    ylab(y_title) +
    xlab("")+
    ggtitle("")+
    ylim(-0.85,0.85)+
    geom_text(signif_label_1[which(signif_label_1$parameter==variable),],mapping=aes(x=group, y=value),label = "*",colour="black",size=8)
}
violin_plot_fun3<-function(psi.data,mu.psi.data,variable){
  ggplot(psi.data[which(psi.data$parameter==variable), ], mapping=aes(x=group, y = mean, group=group,color = group))+ 
    geom_violin()+ 
    geom_jitter(width= 0.1,size=0.5)+
    scale_color_manual(name="Group",values=c("#0A718F","#EDD746"))+
    geom_point(mu.psi.data[which(mu.psi.data$parameter==variable), ],mapping=aes(x=group, y=mean),colour = "black",size=0.8)+
    geom_linerange(mu.psi.data[which(mu.psi.data$parameter==variable),], mapping=aes(ymin=mu.psi.data[which(mu.psi.data$parameter==variable),]$`2.5%`, ymax=mu.psi.data[which(mu.psi.data$parameter==variable), ]$`97.5`),colour="black")+
    geom_hline(yintercept=0,linetype=2,colour="grey")+
    theme_cowplot()+
    theme(legend.position="",
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          plot.background=element_rect(fill="white"),
          strip.background =element_rect(fill="white"),
          axis.ticks.x=element_blank(),
    ) +
    ylab("") +
    xlab("")+
    ggtitle("")+
    geom_text(signif_label_2[which(signif_label_2$parameter==variable),],mapping=aes(x=group, y=value),label = "*",colour="black",size=8)
  
}

#Second row, with x-axis labels
violin_plot_fun2<-function(psi.data,mu.psi.data,variable, y_title){
  ggplot(data=psi.data[which(psi.data$parameter==variable), ], mapping=aes(x=group, y = mean, group=group,color = group))+ 
    geom_violin()+ 
    geom_jitter(width= 0.1,size=0.5)+
    scale_color_manual(name="Group",values=c("#59A082","#EDA417","#E7720B","#DD4124"))+
    geom_point(mu.psi.data[which(mu.psi.data$parameter==variable), ],mapping=aes(x=group, y=mean),colour = "black",size=0.8)+
    geom_linerange(mu.psi.data[which(mu.psi.data$parameter==variable),], mapping=aes(ymin=mu.psi.data[which(mu.psi.data$parameter==variable),]$`2.5%`, ymax=mu.psi.data[which(mu.psi.data$parameter==variable), ]$`97.5`),colour="black")+
    geom_hline(yintercept=0,linetype=2,colour="grey")+
    theme_cowplot()+
    theme(legend.position="",
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          #axis.text.x = element_blank(),
          axis.text.x = element_text(hjust = 1, angle = 45),
          plot.background=element_rect(fill="white"),
          strip.background =element_rect(fill="white"),
          axis.ticks.x=element_blank(),
    ) +
    ylab(y_title) +
    xlab("")+
    ggtitle("")+
    ylim(-0.85,0.85)+
    geom_text(signif_label_1[which(signif_label_1$parameter==variable),],mapping=aes(x=group, y=value),label = "*",colour="black",size=8)
}
violin_plot_fun4<-function(psi.data,mu.psi.data,variable){
  ggplot(data=psi.data[which(psi.data$parameter==variable), ], mapping=aes(x=group, y = mean, group=group,color = group))+ 
    geom_violin()+ 
    geom_jitter(width= 0.1,size=0.5)+
    scale_color_manual(name="Group",values=c("#0A718F","#EDD746"))+
    geom_point(mu.psi.data[which(mu.psi.data$parameter==variable), ],mapping=aes(x=group, y=mean),colour = "black",size=0.8)+
    geom_linerange(mu.psi.data[which(mu.psi.data$parameter==variable),], mapping=aes(ymin=mu.psi.data[which(mu.psi.data$parameter==variable),]$`2.5%`, ymax=mu.psi.data[which(mu.psi.data$parameter==variable), ]$`97.5`),colour="black")+
    geom_hline(yintercept=0,linetype=2,colour="grey")+
    theme_cowplot()+
    theme(legend.position="",
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          #axis.text.x = element_blank(),
          axis.text.x = element_text(hjust = 1, angle = 45),
          plot.background=element_rect(fill="white"),
          strip.background =element_rect(fill="white"),
          axis.ticks.x=element_blank(),
    ) +
    ylab("") +
    xlab("")+
    ggtitle("")+
 geom_text(signif_label_2[which(signif_label_2$parameter==variable),],mapping=aes(x=group, y=value),label = "*",colour="black",size=8)
}

####Plots####
#Impervious surface
imperv_plot_1<-violin_plot_fun2(psi_summary_minus_2,mu_psi_summary_minus_2, "Impervious Surface","Mean Trend of Impervious Surface")
imperv_plot_2<-violin_plot_fun4(psi_summary_ara_myc,mu_psi_summary_ara_myc, "Impervious Surface")+ylim(-12.3,12.3)
#Land Value
lv_plot_1<-violin_plot_fun2(psi_summary_minus_2,mu_psi_summary_minus_2,"Land Value","Mean Trend of Land Value")
lv_plot_2<-violin_plot_fun4(psi_summary_ara_myc,mu_psi_summary_ara_myc,"Land Value")+ylim(-12.3,12.3)
#Distance to Natural Area
dtn_plot_1<-violin_plot_fun1(psi_summary_minus_2,mu_psi_summary_minus_2,"Distance to Natural Area","Mean Trend of Distance to Natural Area")
dtn_plot_2<-violin_plot_fun3(psi_summary_ara_myc,mu_psi_summary_ara_myc,"Distance to Natural Area")+
  ylim(-12.3,12.3)
#Diurnal Range
dr_plot_1<-violin_plot_fun1(psi_summary_minus_2,mu_psi_summary_minus_2,"Diurnal Range","Mean Trend of Diurnal Range")
dr_plot_2<-violin_plot_fun3(psi_summary_ara_myc,mu_psi_summary_ara_myc,"Diurnal Range")+ylim(-0.85,0.85)
#Ocean Effect
oe_plot_1<-violin_plot_fun1(psi_summary_minus_2,mu_psi_summary_minus_2,"Ocean Effect","Mean Trend of Ocean Effect")
oe_plot_2<-violin_plot_fun3(psi_summary_ara_myc,mu_psi_summary_ara_myc,"Ocean Effect")+ylim(-0.85,0.85)
#Temperature
temp_plot_1<-violin_plot_fun1(psi_summary_minus_2,mu_psi_summary_minus_2,"Temperature","Mean Trend of Temperature")
temp_plot_2<-violin_plot_fun3(psi_summary_ara_myc,mu_psi_summary_ara_myc,"Temperature")+ylim(-12.3,12.3)
#create a common legend
legend_plot_1<-ggplot(data=psi_summary_all[which(psi_summary_all$parameter=="Land Value"), ], mapping=aes(x=group, y = mean, group=group,color = group))+ 
  geom_jitter(width= 0.1,size=5, shape=15)+
  scale_color_manual(name="Group",values=c("#0A718F","#59A082","#EDD746","#EDA417","#E7720B","#DD4124"))+
  theme_cowplot()+
  theme(legend.direction="horizontal")+
  guides(color = guide_legend(nrow = 1),
         plot.background=element_rect(fill="white"),
         strip.background =element_rect(fill="white"))

violin_legend_1<-get_legend(legend_plot_1)

final_plot<-ggarrange(dtn_plot_1,dtn_plot_2, oe_plot_1,oe_plot_2, temp_plot_1, temp_plot_2, dr_plot_1, dr_plot_2, imperv_plot_1,imperv_plot_2, lv_plot_1,lv_plot_2,  violin_legend_1, ncol = 4,nrow=4, widths = c(18,7,18,7), heights = c(22,22,26.5,5), labels=c("a","","b","","c","","d","","e","","f","",""))


ggsave2("Step03_Figures/plots/figure2_updated.png",plot=final_plot, dpi=400, height=14, width=15, bg="white")

















