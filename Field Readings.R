#Object of this script is to import and tidy field readings




library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)

# Import Data -------------------------------------------------------------

Field_Readings <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Field Readings/Field Readings.xlsx")



# Tidy Data ---------------------------------------------------------------


Field_Readings_tidy <- Field_Readings %>%
mutate(time=if_else(str_length(`Collection Time`)==3,paste(str_sub(`Collection Time`,1,1),":",str_sub(`Collection Time`,2,3)),paste(str_sub(`Collection Time`,1,2),":",str_sub(`Collection Time`,2,3)))) %>%
mutate(`Date Time`=ymd_hm(paste(Date," ",time)))  %>%
pivot_longer(names_to ="TEST_NAME",values_to="VALUE",7:16) %>%
select(-time) %>%
rename(STATION="Station") %>%
mutate(Treatment=case_when(str_detect(STATION,"Untreated")~"Untreated",                       #add treatment variable
                             str_detect(STATION,"Burn_Herb")~"Burn_Herb",
                             STATION %in% c("Burn A","Burn B","Burn C")~"Burn",
                             str_detect(STATION,"Herbicide")~"Herbicide",
                             TRUE ~ NA))  %>%
mutate(Block=case_when(str_detect(STATION," A")~"A",                       #add treatment variable
                       str_detect(STATION," B")~"B",
                       str_detect(STATION,"C")~"C",
                       TRUE ~ NA)) %>%
mutate(Phase=ifelse(Date<"2025-04-10","Pre-Burn","Post-Burn")) %>%
mutate(Phase=factor(Phase,c("Pre-Burn","Post-Burn")))  



write_csv(Field_Readings_tidy ,"//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Field Readings/Field_Readings_tidy.csv")
