#Object of this script is to tidy and join sonde data


library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(readxl)


# Import Data -------------------------------------------------------------

#work links
#Deployment 1 
EXOdata_BURN_111924_010625 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/EXOdata_BURN_111924_010625.csv",skip=8)
EXOdata_Untreated_111924_120724 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/EXOdata_Untreated_111924_120724.csv",skip=8)
EXOdata_BurnHerbicide_111924_010625 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/EXOdata_BurnHerbicide_111924_010625.csv",skip=8)
EXOdata_Herbicide_111924_010625 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/EXOdata_Herbicide_111924_010625.csv",skip=8)

#Deployment 2
EXOdata_Herbicide_011525_042825 <- read_csv("./Data/Sonde/EXOdata_Herbicide_011525_042825.csv",skip=8)
EXOdata_BURN_011525_042825 <- read_csv("./Data/Sonde/EXOdata_BURN_011525_042825.csv", skip = 8)
EXOdata_BurnHerbicide_011525_042825 <- read_csv("./Data/Sonde/EXOdata_BurnHerbicide_011525_042825.csv", skip = 8)
EXOdata_Untreated_011525_042825 <- read_csv("./Data/Sonde/EXOdata_Untreated_011525_042825.csv",  skip = 8)

#Deployment 3
EXOdata_BURN_051425_070725 <- read_csv("./Data/Sonde/EXOdata_BURN_051425_070725.csv", skip = 8)
EXOdata_BurnHerbicide_051425_070725 <- read_csv("./Data/Sonde/EXOdata_BurnHerbicide_051425_070725.csv",  skip = 8)
EXOdata_Herbicide_051425_070725 <- read_csv("./Data/Sonde/EXOdata_Herbicide_051425_070725.csv", skip = 8)
EXOdata_Untreated_051425_070725 <- read_csv("./Data/Sonde/EXOdata_Untreated_051425_070725.csv",  skip = 8)


# Tidy Data ---------------------------------------------------------------

#Deployment 1
EXOdata_BURN_111924_010625_Tidy <- EXOdata_BURN_111924_010625 %>% 
na.omit() %>%    # remove empty rows
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(`Time (HH:mm:ss)`),minute(`Time (HH:mm:ss)`),0)) %>%   #create date and date_time
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>% #remove redundant rows
mutate(across(2:19,as.numeric))

EXOdata_BurnHerbicide_111924_010625_Tidy <- EXOdata_BurnHerbicide_111924_010625 %>% 
filter(`Site Name`==	"BURN_HERBICIDE") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%   #create date and date_time
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>% #remove redundant rows
mutate(across(2:19,as.numeric))

EXOdata_Herbicide_111924_010625_Tidy <- EXOdata_Herbicide_111924_010625  %>% 
na.omit() %>%    # remove empty rows
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(`Time (HH:mm:ss)`),minute(`Time (HH:mm:ss)`),0)) %>%   #create date and date_time
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>% #remove redundant rows
mutate(across(2:19,as.numeric))

EXOdata_Untreated_111924_120724_Tidy <- EXOdata_Untreated_111924_120724 %>% 
filter(`Site Name`==	"UNTREATED") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%   #create date and date_time
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>% #remove redundant rows
mutate(across(2:19,as.numeric))


#Deployment 2
EXOdata_Herbicide_011525_042825_Tidy <- EXOdata_Herbicide_011525_042825 %>% 
na.omit() %>%    # remove empty rows
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(`Time (HH:mm:ss)`),minute(`Time (HH:mm:ss)`),0)) %>%   #create date and date_time
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`)   %>%#remove redundant rows
mutate(across(2:19,as.numeric))

EXOdata_BURN_011525_042825_Tidy <- EXOdata_BURN_011525_042825 %>% 
filter(`Site Name`=="BURN") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>%
mutate(across(2:19,as.numeric))  #convert columns to numeric class

EXOdata_BurnHerbicide_011525_042825_Tidy <- EXOdata_BurnHerbicide_011525_042825 %>% 
filter(`Site Name`==	"BURN_HERBICIDE") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>%
mutate(across(2:19,as.numeric))   #convert columns to numeric class

EXOdata_Untreated_011525_042825_Tidy <- EXOdata_Untreated_011525_042825 %>% 
filter(`Site Name`==	"UNTREATED") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>%
mutate(across(2:19,as.numeric))   #convert columns to numeric class

#Deployment 3
EXOdata_Untreated_051425_070725_Tidy <- EXOdata_Untreated_051425_070725  %>% 
filter(`Site Name`==	"UNTREATED") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>%
mutate(across(2:19,as.numeric))   #convert columns to numeric class

EXOdata_BurnHerbicide_051425_070725_Tidy <- EXOdata_BurnHerbicide_051425_070725 %>% 
filter(`Site Name`==	"BURN_HERBICIDE") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>%
mutate(across(2:19,as.numeric))   #convert columns to numeric class

EXOdata_Herbicide_051425_070725_Tidy <- EXOdata_Herbicide_051425_070725 %>% 
filter(!is.na(`Date (MM/DD/YYYY)`) & `Date (MM/DD/YYYY)`!="Date (MM/DD/YYYY)" & 	`Date (MM/DD/YYYY)`!="Time (HH:mm:ss)") %>%
mutate(Date=mdy(`Date (MM/DD/YYYY)`),time=strptime(`Time (HH:mm:ss)`, "%H:%M:%S")) %>%
mutate(Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(`time`),minute(`time`),0)) %>%   #create date and date_time
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`,-time)   %>%#remove redundant rows
mutate(across(2:19,as.numeric))

EXOdata_BURN_051425_070725_Tidy <- EXOdata_BURN_051425_070725 %>% 
filter(`Site Name`=="BURN") %>%  
mutate(Date=mdy(`Date (MM/DD/YYYY)`),Date_time=ISOdatetime(year(Date),month(Date),day(Date),hour(hms(`Time (HH:mm:ss)`)),minute(hms(`Time (HH:mm:ss)`)),0)) %>%
select(-`Date (MM/DD/YYYY)`,-`Time (HH:mm:ss)`,-`Time (Fract. Sec)`) %>%
mutate(across(2:19,as.numeric))  #convert columns to numeric class




# Join Data ---------------------------------------------------------------

#Join Data Deployment 1
Deployment_1 <- bind_rows(EXOdata_BURN_111924_010625_Tidy,EXOdata_BurnHerbicide_111924_010625_Tidy,EXOdata_Herbicide_111924_010625_Tidy,EXOdata_Untreated_111924_120724_Tidy) %>%
filter(Date_time<"2024-12-17 09:00:00",Date_time>"2024-11-19 13:00:00") %>% #deployment period
mutate(Deployment="Deployment 1")

#Join Data Deployment 2
Deployment_2 <- bind_rows(EXOdata_Herbicide_011525_042825_Tidy,EXOdata_BURN_011525_042825_Tidy,EXOdata_Untreated_011525_042825_Tidy,EXOdata_BurnHerbicide_011525_042825_Tidy) %>%
filter(Date_time<"2025-04-21 09:00:00",Date_time>"2025-01-15 12:30:00") %>%#deployment period
mutate(Deployment="Deployment 2")

#Join Data Deployment 3
Deployment_3 <- bind_rows(EXOdata_Herbicide_051425_070725_Tidy,EXOdata_BURN_051425_070725_Tidy,EXOdata_Untreated_051425_070725_Tidy,EXOdata_BurnHerbicide_051425_070725_Tidy) %>%
filter(Date_time<"2025-07-01 10:00:00",Date_time>"2025-05-14 11:30:00") %>%#deployment period
mutate(Deployment="Deployment 3")

Sonde_Wide_Tidy <- bind_rows(Deployment_1,Deployment_2,Deployment_3)

#Convert to long format
Sonde_Long_Tidy <- Sonde_Wide_Tidy %>%
pivot_longer(names_to="Parameter",values_to = "Value",2:19)  %>%
filter(Parameter %in% c("Battery V","Cable Pwr V","Wiper Position volt","pH mV","nLF Cond �S/cm","ODO % CB","ODO % sat","Sal psu","TSS mg/L","TDS mg/L","Cond �S/cm","TAL PC RFU")==FALSE)

#Save Data
write_csv(Sonde_Long_Tidy,"./Data/Sonde/Sonde_Long_Tidy.csv")
write_csv(Sonde_Wide_Tidy,"./Data/Sonde/Sonde_Wide_Tidy.csv")

# EDA figures -----------------------------------------------------------------

#all parameters
ggplot(Sonde_Long_Tidy ,aes(Date_time ,Value,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+facet_wrap(~Parameter,scales="free")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#pH
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,pH,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#SpCond �S/cm
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`SpCond �S/cm`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = xmax = as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#ODO mg/L
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`ODO mg/L`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax =  as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#ODO % sat
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`ODO % sat`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#Chlorophyll RFU
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`Chlorophyll RFU`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#TAL PC RFU
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`TAL PC RFU`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax =  as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#Temp �C
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`Temp �C`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()

#Turbidity FNU
ggplot(Sonde_Wide_Tidy ,aes(Date_time ,`Turbidity FNU`,color=`Site Name`,fill=`Site Name`))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax =  as_datetime("2025-04-21 10:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
geom_point(shape=21,size=2,color="black")+theme_bw()






