#Object of this script is to tidy and join sonde data


library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(readxl)


# Import Data -------------------------------------------------------------

#work links
EXOdata_Herbicide_011525_042825 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/EXOdata_Herbicide_011525_042825.csv",skip =9)
