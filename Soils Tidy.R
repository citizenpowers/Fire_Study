#Import and tidy soil data


library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)

# Import Data -------------------------------------------------------------

#work links
LIMSP_Provisional_Data <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Water Quality Data/LIMSP Provisional Data.xlsx")





# Tidy Data ---------------------------------------------------------------

Soils_tidy <- LIMSP_Provisional_Data %>%
filter(MATRIX %in% c("SO","FLOC")) %>%
mutate(Date_time=ymd_hms(COLLECT_DATE)) %>%  
mutate(Phase=if_else(Date_time<"2025-04-10 12:00:00","Pre-burn","Post-burn")) %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))


write_csv(Soils_tidy ,"./Data/Soils/Soils_tidy.csv")
