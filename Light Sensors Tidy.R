# Goal of this script is to import and tidy light sensor data

library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)

# Import ------------------------------------------------------------------

#Deployment 1
TOP_UNTREATED_022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_UNTREATED_022425.csv", skip = 1) %>% mutate(Site="Untreated",Position="Top")
MID_UNTREATED_022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_UNTREATED_022425.csv", skip = 1) %>% mutate(Site="Untreated",Position="Mid")
BOTTOM_UNTREATED022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_UNTREATED022425.csv",  skip = 1) %>% mutate(Site="Untreated",Position="Bottom")

TOP_HERB_022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_HERB_022425.csv",  skip = 1) %>% mutate(Site="Herbicide",Position="Top")
MID_HERB_022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_HERB_022425.csv",  skip = 1) %>% mutate(Site="Herbicide",Position="Mid")
BOTTOM_HERB022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_HERB022425.csv", skip = 1) %>% mutate(Site="Herbicide",Position="Bottom")

TOP_BURN_HERB022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_BURN_HERB022425.csv", skip = 1) %>% mutate(Site="Burn_Herb",Position="Top")
MID_BURN_HERB022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_BURN_HERB022425.csv", skip = 1) %>% mutate(Site="Burn_Herb",Position="Mid")
BOTTOM_BURN_HERB022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_BURN_HERB022425.csv", skip = 1) %>% mutate(Site="Burn_Herb",Position="Bottom")

TOP_BURN022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_BURN022425.csv", skip = 1) %>% mutate(Site="Burn",Position="Top")
MID_BURN022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_BURN022425.csv",  skip = 1) %>% mutate(Site="Burn",Position="Mid")
BOTTOM_BURN022425 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_BURN022425.csv",  skip = 1) %>% mutate(Site="Burn",Position="Bottom")

#Deployemnt 2
TOP_UNTREATED_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_UNTREATED_072325.csv", skip = 1) %>% mutate(Site="Untreated",Position="Top")
MID_UNTREATED_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_UNTREATED_072325.csv", skip = 1) %>% mutate(Site="Untreated",Position="Mid")
BOTTOM_UNTREATED_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_UNTREATED_072325.csv",  skip = 1) %>% mutate(Site="Untreated",Position="Bottom")

TOP_HERB_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_HERB_072325.csv",  skip = 1) %>% mutate(Site="Herbicide",Position="Top")
MID_HERB_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_HERB_072325.csv",  skip = 1) %>% mutate(Site="Herbicide",Position="Mid")
BOTTOM_HERB_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_HERB_072325.csv", skip = 1) %>% mutate(Site="Herbicide",Position="Bottom")

TOP_BURN_HERB_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_BURN_HERB_072325.csv", skip = 1) %>% mutate(Site="Burn_Herb",Position="Top")
MID_BURN_HERB_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_BURN_HERB_072325.csv", skip = 1) %>% mutate(Site="Burn_Herb",Position="Mid")
BOTTOM_BURN_HERB_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_BURN_HERB_072325.csv", skip = 1) %>% mutate(Site="Burn_Herb",Position="Bottom")

TOP_BURN_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/TOP_BURN_072325.csv", skip = 1) %>% mutate(Site="Burn",Position="Top")
MID_BURN_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/MID_BURN_072325.csv",  skip = 1) %>% mutate(Site="Burn",Position="Mid")
BOTTOM_BURN_072325 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/HOBO/BOTTOM_BURN_072325.csv",  skip = 1) %>% mutate(Site="Burn",Position="Bottom")



# Tidy and join Data  --------------------------------------------------------------

#Function to clean-up column names
tidy_column_names<-function(df){
df %>%
rename("Date Time"=2,"Temp"=3,"Intensity (Lux)"=4) %>%
mutate(`Date Time`= mdy_hms(`Date Time`)) %>%  
mutate(`Label Time`=ISOdate(2000, 1, 1, hour = hour(`Date Time`), min = minute(`Date Time`), sec = second(`Date Time`), tz = "GMT")) %>%
select("Date Time",`Label Time`,Site,Position,"Temp","Intensity (Lux)")  
}

Deployment_1_Tidy <- bind_rows(tidy_column_names(TOP_UNTREATED_022425),tidy_column_names(MID_UNTREATED_022425),tidy_column_names(BOTTOM_UNTREATED022425)) %>%
bind_rows(tidy_column_names(TOP_HERB_022425),tidy_column_names(MID_HERB_022425),tidy_column_names(BOTTOM_HERB022425))  %>%
bind_rows(tidy_column_names(TOP_BURN_HERB022425),tidy_column_names(MID_BURN_HERB022425),tidy_column_names(BOTTOM_BURN_HERB022425))   %>%
bind_rows(tidy_column_names(TOP_BURN022425),tidy_column_names(MID_BURN022425),tidy_column_names(BOTTOM_BURN022425)) %>%
filter(`Date Time`<"2025-02-10 23:30:00",`Date Time`>"2024-11-12 00:00:00")     


Deployment_2_Tidy <- bind_rows(tidy_column_names(TOP_UNTREATED_072325),tidy_column_names(MID_UNTREATED_072325),tidy_column_names(BOTTOM_UNTREATED_072325)) %>%
bind_rows(tidy_column_names(TOP_HERB_072325),tidy_column_names(MID_HERB_072325),tidy_column_names(BOTTOM_HERB_072325))  %>%
bind_rows(tidy_column_names(TOP_BURN_HERB_072325),tidy_column_names(MID_BURN_HERB_072325),tidy_column_names(BOTTOM_BURN_HERB_072325))   %>%
bind_rows(tidy_column_names(TOP_BURN_072325),tidy_column_names(MID_BURN_072325),tidy_column_names(BOTTOM_BURN_072325)) %>%
filter(`Date Time`<"2025-05-07 23:30:00",`Date Time`>"2025-02-26 00:00:00")    


All_Light_Data_Tidy <- bind_rows(Deployment_1_Tidy,Deployment_2_Tidy) %>%
mutate(Phase=ifelse(`Date Time`>"2025-04-10 12:00:00","Post Burn","Pre Burn"))


write_csv(All_Light_Data_Tidy ,"./Data/HOBO/All_Light_Data_Tidy.csv")

#check to see if data exist for each site and position in deployment 1
ggplot(Deployment_1_Tidy,aes(`Date Time`,`Intensity (Lux)`,color=Site))+geom_point()+
scale_x_datetime(date_breaks = "2 weeks")+facet_grid(Site~Position)+theme_bw()

#check to see if data exist for each site and position in deployment 2
ggplot(Deployment_2_Tidy,aes(`Date Time`,`Intensity (Lux)`,color=Site))+geom_point()+
scale_x_datetime(date_breaks = "2 weeks")+facet_grid(Site~Position)+theme_bw()

#check to see if data exist for each site and position in all data
ggplot(All_Light_Data_Tidy,aes(`Date Time`,`Intensity (Lux)`,color=Site))+geom_point()+
scale_x_datetime(date_breaks = "2 weeks")+facet_grid(Site~Position)+theme_bw()

ggplot(filter(All_Light_Data_Tidy,`Intensity (Lux)`>0 ),aes(`Label Time`,`Intensity (Lux)`,color=Site))+geom_point(alpha=.3)+
scale_x_datetime(date_breaks = "4 hours",date_labels = "%H")+geom_smooth()+coord_cartesian(expand = 0)+
facet_grid(Site~Position)+theme_bw()
  
#closeup of pre and post burn
ggplot(All_Light_Data_Tidy,aes(`Date Time`,`Intensity (Lux)`,color=Site))+geom_point()+
scale_x_datetime(date_breaks = "2 day",date_labels = "%b %d")+facet_grid(Site~Position)+coord_cartesian(xlim = as.POSIXct(c("2025-04-03 00:00:00", "2025-04-21 00:00:00")))+theme_bw()




