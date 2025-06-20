#Object of this script is to import and tidy NuLab SRP data




library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)
library(Rccp)
library(scales)



# Import Data -------------------------------------------------------------

NuLab_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/NuLab Data.csv")
NuLab_Data2 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/NuLab Data 2.csv")
All_Phosphate_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/All Phosphate Data.csv", 
                               skip = 3)

LIMSP_Provisional_Data_Tidy <- read_csv("./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy.csv")

# Tidy Data ---------------------------------------------------------------



NuLab_Data_Tidy <- NuLab_Data  %>%
mutate(Voltage=as.numeric(Voltage),`Data Source`="Nulab 1") %>%
bind_rows(mutate(NuLab_Data2,`Data Source`="Nulab 2"))  %>%
bind_rows(mutate(All_Phosphate_Data,`Data Source`="All_Phosphate_Data",Light=as.numeric(Light),
Ch1_LED=as.numeric(Ch1_LED),Ch2_LED=as.numeric(Ch2_LED)))  %>%
mutate(Date_Time=as.POSIXct(mdy_hms(`#MM/DD/YY HH:mm:SS`)))   %>%
filter(!is.na(`Date_Time`))  %>%
#add_row(Date_Time=as.POSIXct("2025-04-18 00:30:00"),Flag=112)  %>%  #add blank row of data to break up data into two parts 
arrange(Date_Time)


SRP_WQ_data <-LIMSP_Provisional_Data_Tidy %>%
filter(TEST_NAME=="SRP",MATRIX=="SW",COLLECT_METHOD=="GP",STATION=="Burn_Herb_A",COLLECT_DATE>"2025-04-10 00:30:00")  %>%
select(COLLECT_DATE,VALUE) %>%
rename(Date_Time="COLLECT_DATE") %>%
mutate(Day=day(Date_Time),Hour=hour(Date_Time))  




# join WQ and Nulab data --------------------------------------------------
NuLab_vs_WQ <- NuLab_Data_Tidy %>%
filter(Flag==112) %>%
select(Date_Time,Smp_conc)  %>%
mutate(Day=day(Date_Time),Hour=hour(Date_Time)) %>%
left_join(SRP_WQ_data,by=c("Day","Hour"))  


# Figures -----------------------------------------------------------------



#Grab samples OPO4 only Points
ggplot(filter(NuLab_Data_Tidy,Flag==112 ),aes(Date_Time,Smp_conc))+
geom_point(data=SRP_WQ_data,aes(Date_Time,VALUE),shape=21,size=3,color="black",fill="#33a02c",inherit.aes = F)+  
geom_smooth(data=SRP_WQ_data,aes(Date_Time,VALUE),size=.5,color="#33a02c",inherit.aes = F)+    
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 day")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(OPO4~(mg/L)))+labs(title="OPO4 over time")+
geom_point(shape=21,size=2.5,color="black",fill="#ff7f00")+geom_line(span=.5)+
theme_bw()

ggsave(plot = last_plot(),filename="./Figures/NuLab and WQ post-burn.jpeg",width =16, height =9, units = "in")


#Filter to just time that the NULab has been working. 
ggplot(filter(NuLab_Data_Tidy,Date_Time>"2025-06-01 10:10:00"),aes(Date_Time,Smp_conc))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 day")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
geom_point(data=filter(SRP_WQ_data,Date_Time>"2025-06-01 10:10:00"),aes(Date_Time,VALUE),shape=21,size=3,color="black",fill="#33a02c",inherit.aes = F)+  
ylab(expression(OPO4~(mg/L)))+labs(title="OPO4 over time")+
geom_point(shape=21,size=2.5,color="black",fill="#ff7f00")+geom_line(span=.5)+
theme_bw()
 




