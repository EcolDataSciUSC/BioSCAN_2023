library(dplyr);
#library(tidyverse)
####import the data
inflation_rate <- read.csv("CPI_scaling.csv")%>%
                  mutate_at("Year", as.numeric)%>%
                  filter(!is.na(Year))%>%
                  rename("Multiply_factor"="X")%>%
                  rbind(c(2023,294.4,"8.60%",1))

# %>%#using the same values as the year 2022 for year 2023
#                   mutate(Annual.Percent.Change = str_remove_all(Annual.Percent.Change, '\\%'))%>%
#                   mutate_if(is.character, as.numeric)
####1km buffer####
land_v_res <- read.csv("parcels.csv") %>%
  
land_v_res <- land_v_res %>%
  filter(Year!=0)

merged_df <- merge(land_v_res, inflation_rate, by = "Year")

              filter(LA_Year!=0)%>%
              rename("Year"="LA_Year")%>%
              left_join(inflation_rate, by = "Year")%>%
              mutate(land_value_adjusted=LandV_Num...*Multiply_factor)%>%
              group_by(Site)%>%
              mutate(site_total_area=sum(Area.ha.))%>%
              ungroup()%>%
              mutate(land_area_weight=Area.ha./site_total_area)%>%
              mutate(relative_land_value=land_area_weight*land_value_adjusted)%>%
              group_by(Site)%>%
              summarise(weighted_avarage_land_V=sum(relative_land_value))
write.csv(land_v_res, "onlyresidential_weight_average_landV.csv", row.names=FALSE)

####0.5km buffer####
land_v_res_05km <- read.csv("bioscan_onlyresidential_landval_yrfixed_05km.csv")%>%
  filter(LA_Year!=0)%>%
  rename("Year"="LA_Year")%>%
  left_join(inflation_rate, by = "Year")%>%
  mutate(land_value_adjusted=LandV_Num*Multiply_factor)%>%
  group_by(Site)%>%
  mutate(site_total_area=sum(Area))%>%
  ungroup()%>%
  mutate(land_area_weight=Area/site_total_area)%>%
  mutate(relative_land_value=land_area_weight*land_value_adjusted)%>%
  group_by(Site)%>%
  summarise(weighted_avarage_land_V=sum(relative_land_value))
write.csv(land_v_res, "onlyresidential_weight_average_landV_05km.csv", row.names=FALSE)



####0.25km buffer####
land_v_res_025km <- read.csv("bioscan_onlyresidential_landval_yrfixed_025km.csv")%>%
  filter(LA_Year!=0)%>%
  rename("Year"="LA_Year")%>%
  left_join(inflation_rate, by = "Year")%>%
  mutate(land_value_adjusted=LandV_Num...*Multiply_factor)%>%
  group_by(Site)%>%
  mutate(site_total_area=sum(Area.ha.))%>%
  ungroup()%>%
  mutate(land_area_weight=Area.ha./site_total_area)%>%
  mutate(relative_land_value=land_area_weight*land_value_adjusted)%>%
  group_by(Site)%>%
  summarise(weighted_avarage_land_V=sum(relative_land_value))
write.csv(land_v_res, "onlyresidential_weight_average_landV_025km.csv", row.names=FALSE)



####testing to see if the above steps are correct####
land_v_res_test <-read.csv("bioscan_onlyresidential_landval_v2.csv")%>%
                  filter(Site=="Site01")%>%
                  rename("Year"="LA_Year")%>%
                  left_join(inflation_rate, by = "Year")%>%
                  mutate(land_value_adjusted=LandV_Num...*Multiply_factor)%>%
                  group_by(Site)%>%
                  mutate(site_total_area=sum(Area.ha.))%>%
                  ungroup()%>%
                  mutate(land_area_weight=Area.ha./site_total_area)%>%
                  mutate(relative_land_value=land_area_weight*land_value_adjusted)%>%
                  group_by(Site)%>%
                  summarise(weighted_avarage_land_V=sum(relative_land_value))
