#sampling data frame
require(readxl)
require(dbplyr)
require(tidyr)
require(tidyverse)
### load bioscan data (phase 1,2 and 3 only)
monthly_1 <- read_excel("bioscan.xlsx", sheet = "Monthly BioSCAN Phase I") 
monthly_2 <- read_excel("bioscan.xlsx", sheet = "Monthly BioSCAN Phase II")
monthly_3 <- read_excel("bioscan.xlsx", sheet = "Monthly BioSCAN Phase III")

### select required columns
monthly_1 <-monthly_1 %>% select(c(1,3:6))
monthly_2 <-monthly_2 %>% select(c(1,3:6))
monthly_3 <-monthly_3 %>% select(c(1,3:6))

### combine all three phases by column names
colnames(monthly_1)[5]<-"Collection days" #this standardized the column names across all 3 phases
monthly_all<-do.call("rbind", list(monthly_1, monthly_2, monthly_3))

### change the month values to (MM)
monthly_all$Month<- substr(monthly_all$Month, 1, 2)%>%as.numeric()
monthly_all$Month<-sprintf("%02d",monthly_all$Month)

### change the site values to (NN)
monthly_all$Site<- substr(monthly_all$Site, 5, 6)

### change the phase values to (NN)
monthly_all$Phase<-sprintf("%02d",monthly_all$Phase)

### change the year value to YYYY
monthly_all$Year[monthly_all$Year == 14] <- 2014
monthly_all$Year[monthly_all$Year == 15] <- 2015
monthly_all$Year[monthly_all$Year == 16] <- 2016

### add the start/ end dates
monthly_all$start_date<-1
monthly_all$start_date<-paste(monthly_all$start_date,monthly_all$Month,monthly_all$Year,sep="/")%>%as.Date(monthly_all$start_date,format="%d/%m/%Y")
monthly_all$end_date<-monthly_all$`Collection days`
monthly_all$end_date<-paste(monthly_all$end_date,monthly_all$Month,monthly_all$Year,sep="/")%>%as.Date(monthly_all$end_date,format="%d/%m/%Y")

###Create new sampling ID (Year_Month_Phase_Site, e.g., 14_01_01_01)
monthly_all$id<-paste(substr(monthly_all$Year, 3, 4),sep="_")%>%
paste(monthly_all$Month,sep="_")%>%
paste(monthly_all$Phase,sep="_")%>%
paste(monthly_all$Site,sep="_")
###Checking if the id is unique
length(unique(monthly_all$id))
### export the csv file"
write.csv(monthly_all, "sampling_data_frame.csv", row.names=FALSE)
