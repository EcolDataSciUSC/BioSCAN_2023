library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(RColorBrewer)
library(data.table)
library(RColorBrewer)
library(gplots)

getwd()

# Set to main folder (Lewthwaite_et_al_2023)
my_working_dir <- ""

setwd(my_working_dir)



source("Step02_Occupancy_Models/src/initialize.R")

# For a group
# Remove some things
# Maybe get rid of RH and GDD? Encapsulated by season a bit already
# Species specific detection intercept - may not be useful/enough info

# Are scales of each predictor roughly equivalent? How skewed are they...need to make sure ~normally distributed


year_range=c(14,18)
taxonomic_group<-"Phoridae"
model<- "full_model_ind_spatial_distancenatural"

##Import your model outputs
res <- readRDS(paste0("Outputs/res_", paste0(taxonomic_group, "_",model, ".rds")))
res.summary <- readRDS(paste0("Outputs/res.summary_", paste0(taxonomic_group, "_",model, ".rds")))
my.data <- readRDS(paste0("Data/clean_data/data_prepared/my_data_",  paste0(year_range, collapse = "_"), "_", taxonomic_group, "_spatial", ".rds"))


## check main trends
get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}


vars <- rownames(res.summary$psrf$psrf)
summ <- get.summ(vars)


summ.occupancy <- summ[str_detect(rownames(summ), 'mu.psi'),]
summ.occupancy

#edit the below line, depending on your predictors of detection
# summ.detection <- summ[c('p.colldays', 'p.prcp', 'p.wind'),]
summ.detection <- summ[c('p.colldays', 'p.wind'),]
summ.detection








##Examine which species significantly increase/decrease with predictors

#Replace "mu.psi.value" with whatever other predictor you want to examine; ex. with "mu.psi.RH", etc
summ<-summ %>%
  as.data.frame() %>%
  filter(!str_detect(rownames(summ), 'mu.psi.value')) %>%
  as.matrix()


species_trends <- cbind(data.frame(summ[str_detect(rownames(summ), 'psi.value'),]), species = my.data$sp)

species_increasing <- species_trends %>% 
  mutate(sig = ifelse((`X2.5.` < 0 & `X97.5.`< 0) | (`X2.5.` > 0 & `X97.5.`> 0), TRUE, FALSE)) %>% 
  filter(sig == TRUE & mean >0) %>% 
  arrange(desc(mean)) 

species_increasing %>% View()

write.csv(species_increasing, "Outputs/species_increasing.csv")

species_decreasing <- species_trends %>% 
  mutate(sig = ifelse((`X2.5.` < 0 & `X97.5.`< 0) | (`X2.5.` > 0 & `X97.5.`> 0), TRUE, FALSE)) %>% 
  filter(sig == TRUE & mean <0) %>% 
  arrange(mean)

species_decreasing %>% View()

write.csv(species_decreasing, "Outputs/species_decreasing.csv")

species_not_significantly <- species_trends %>% 
  mutate(sig = ifelse((`X2.5.` < 0 & `X97.5.`< 0) | (`X2.5.` > 0 & `X97.5.`> 0), TRUE, FALSE)) %>% 
  filter(sig == FALSE) %>% 
  arrange(desc(mean)) 

species_not_significantly %>% View()

write.csv(species_not_significantly, "Outputs/species_not_significantly.csv")


species_intercepts <- cbind(data.frame(summ[str_detect(rownames(summ), 'psi.sp'),]), species = my.data$sp)






##### Spaghetti plots #########

#note that you will have to edit the following source script depending on your predictors/what you want to plot; right now, it plots occupancy trends across GDD3, RH, % Impervious and Land Value 
source("Step02_Occupancy_Models/src/plot_env_functions.R")


### assign correct chains ##

nsite <- as.integer(my.data[[1]]$nsite)
nsp <- as.integer(my.data[[1]]$nsp)
nvisit <- as.integer(my.data[[1]]$nvisit)


species_directory <- data.frame(sp_n = my.data$sp) %>% 
  mutate(ss = 1:n())


sims.mat <- do.call(rbind, res$mcmc)
sims.arr <-
  aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))


# Change for what you want to plot
env_list <- 'Temp'

# ALSO must change ii (imperv), tt (temp) below
# Stored in plot_env_functions.R line 14


for(env in env_list){
  print(env)
  
  # main trend occupancy
  
  main_occupancy <- get.y.val.main.all(sims.mat = sims.mat, env, my.data = my.data)
  
  #make sure you create this subfolder or change the directory to which you save this
   # saveRDS(main_occupancy, paste0('plots/env/main_', env,'_occupancy.rds'))
  
}

# plot main trend occupancy (so the MEAN ACROSS ALL SPECIES)
main_trend_value <- main_occupancy %>%
  ggplot() +
  geom_line(aes(x = main_occupancy$tt, y = main_occupancy$mean)) +
  geom_ribbon(aes(x = main_occupancy$tt, ymin = main_occupancy$X2.5., ymax = main_occupancy$X97.5.), alpha = 0.3) +
  theme_cowplot() +
  theme(strip.background =element_blank(), 
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 20), 
        axis.title = element_text(size = 20)) +
  ylab("Occupancy") +
  xlab("Tavg")

main_trend_value



#Now, look at each species individually

sp_occupancy <- lapply(1:nsp, FUN = function(x) get.y.val.all(sims.mat = sims.mat, ss = x, env, my.data = my.data))


sp_occupancy <- sp_occupancy %>% 
  rbindlist() %>% 
  left_join(species_directory)

sp_plot <- ggplot() +
  geom_line(data = sp_occupancy[sp_occupancy$sp_n=='Hyles_lineata'], aes_string(x = "tt", y = "mean", group= "ss"), colour = 'grey') +
  #geom_line(data = main_trend_value$data, aes_string(x = "tt", y = "mean"), colour = 'black') +
  theme(strip.background =element_blank(), 
        axis.text = element_text(angle = 90)) +
  ylab("Occupancy") +
  xlab("Tavg")   +
  ylim(0,1)

sp_plot
#each grey line= 1 species, black line = average across all species







##### Heat plots #########
vars <- rownames(res.summary$psrf$psrf)
summ <- get.summ(vars)

heat_map_df <- t(data.frame(Tavg=summ[grepl("^psi.GDD" , rownames(summ), perl=TRUE), ][,1],
                          RH=summ[grepl("^psi.RH" , rownames(summ), perl=TRUE), ][,1],
                          #prcp=summ[grepl("^psi.prcp" , rownames(summ), perl=TRUE), ][,1],
                          LV=summ[grepl("^psi.value" , rownames(summ), perl=TRUE), ][,1],
                          Imperv=summ[grepl("^psi.imperv" , rownames(summ), perl=TRUE), ][,1]))

lower <- t(as.matrix(data.frame(Tavg=summ[grepl("^psi.GDD" , rownames(summ), perl=TRUE), ][,2],
                          RH=summ[grepl("^psi.RH" , rownames(summ), perl=TRUE), ][,2],
                          #prcp=summ[grepl("^psi.prcp" , rownames(summ), perl=TRUE), ][,2],
                          LV=summ[grepl("^psi.value" , rownames(summ), perl=TRUE), ][,2],
                          Imperv=summ[grepl("^psi.imperv" , rownames(summ), perl=TRUE), ][,2])))

upper <- t(as.matrix(data.frame(Tavg=summ[grepl("^psi.GDD" , rownames(summ), perl=TRUE), ][,3],
                          RH=summ[grepl("^psi.RH" , rownames(summ), perl=TRUE), ][,3],
                          #prcp=summ[grepl("^psi.prcp" , rownames(summ), perl=TRUE), ][,3],
                          LV=summ[grepl("^psi.value" , rownames(summ), perl=TRUE), ][,3],
                          Imperv=summ[grepl("^psi.imperv" , rownames(summ), perl=TRUE), ][,3])))

bin_mat <- (sign(lower) * sign(upper)) > 0

# Set column names for heatmap plot
colnames(heat_map_df) <- my.data$sp

col_order <- order(heat_map_df[1, ])

# Heatmap
dev.off()

heatmap.2(as.matrix(heat_map_df), trace='none', density.info='none', dendrogram='none', Rowv=FALSE, Colv=col_order,
          cellnote=matrix(ifelse(bin_mat, "*", ""), nrow = nrow(bin_mat), ncol = ncol(bin_mat)), notecol='black', notecex=1.5,
          col= brewer.pal(9, "RdYlGn"), margins=c(20,8), cexRow=2, cexCol=1, , lwid=c(0.1,4), lhei=c(0.1,4))

# Phoridae: col= brewer.pal(9, "RdYlGn"),  margins=c(20,8), cexRow=2, cexCol=1, lwid=c(0.1,4), lhei=c(0.1,4))







##### Get 90% BCI #########
mcmc <- rbind(as.data.frame(res$mcmc[[1]]), as.data.frame(res$mcmc[[2]]), as.data.frame(res$mcmc[[3]]))
ci <- t(apply(mcmc, 2, function(x) quantile(x, c(0.5, 0.05, 0.95))))
ci[c("mu.psi.0", "mu.psi.GDD1", 'mu.psi.RH', 'mu.psi.value', 'mu.psi.imperv'), ]

##### Get % of species that are positively associated with variable #########
# Define a custom function to calculate the percentage of positive values in a vector
perc_pos <- function(x) {
  sum(x > 0) / length(x) * 100
}

# Apply the function to each row of the matrix and assign the results to a vector
perc_pos_row <- apply(heat_map_df, 1, perc_pos)

# Print the result
print(perc_pos_row)






##### Heat plot % Species Inc/Dec #########
perc_spec_df <- t(data.frame(Tavg=c(0.1667,	0.5,	0.147,	0.111,	0.111,0),
                            RH=c(0.3148,	0.416,	0.91,	0.5,	0.777,	0.435),
                            LV=c(0.71,	0.875,	0.382,	0.555,	0.963,	0),
                            Imperv=c(0.037,	0.1667,	0.88,	0.5,	1,	0.087)))

sig_df <- t(data.frame(Tavg=c(2,	0,	0,	0,	0, 0),
                             RH=c(0,	0,	0,	0,	0, 0),
                             LV=c(1,	0,	0,	0,	0, 0),
                             Imperv=c(2,	0,	0,	0,	0, 0)))

colnames(perc_spec_df) <- c('Phoridae',	'Lepidoptera',	'Syrphidae',	'Tipuloidea',
                            'Drosophilidae',	'Mycetophilidae')

# Heatmap
heatmap.2(as.matrix(perc_spec_df), trace='none', density.info='none', dendrogram='none', Rowv=FALSE,
          cellnote=matrix(ifelse(sig_df==2, "**", ifelse(sig_df==1, '*', '')), nrow = nrow(bin_mat), ncol = ncol(bin_mat)), notecol='black', notecex=1.5,
          col= brewer.pal(5, "RdYlGn"), margins=c(16,12), cexRow=2.5, cexCol=2.5,  key.xlab = '\n\nSig of group trend: \n *: a=0.1 \n **: a=0.05', key.title='', key.xtickfun = '') #key.title='% Species with\n Mean Positive Association',
dev.off()
