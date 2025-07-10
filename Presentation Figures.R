#Goal of this script is to create figures for presentations


library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
#library(plyr)
library(ggsci)
library(cowplot)
library(readxl)

# Import Data -------------------------------------------------------------

LIMSP_Provisional_Data <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Water Quality Data/LIMSP Provisional Data.xlsx")

LIMSP_Provisional_Data_Tidy_letters <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Water Quality Data/LIMSP_Provisional_Data_Tidy_letters.csv")

STA34_flow_DA_tidy <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Flow/STA34_flow_DA_tidy.csv")

STA34_compliance_provisional <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Water Quality Data/STA34_compliance_provisional.xlsx")

LIMSP_Provisional_Data_Tidy <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Water Quality Data/LIMSP_Provisional_Data_Tidy.csv")

Field_Readings_tidy <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Field Readings/Field_Readings_tidy.csv")

Sonde_Data_Qualified <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/Sonde Data Qualified.csv")

Soils_tidy <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Soils/Soils_tidy.csv")

# Custom Theme ------------------------------------------------------------

fire_theme <-list(scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c","#e5d8bd","#fed9a6")),scale_color_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c","#e5d8bd","#fed9a6")),theme_bw(base_size = 20),theme(legend.position="right",axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1)),xlab(""))

# WQ_Summary ---Mean, SD, SE for each day and parameter---------------------------------------------------

WQ_Summary    <- LIMSP_Provisional_Data_Tidy %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP")  %>%
mutate(Date=as.Date(COLLECT_DATE)) %>%  
group_by(Date,`Burn Day`,Treatment,TEST_NAME)  %>%
summarise(n=n(),Mean=mean(VALUE),SD=sd(VALUE),SE=SD/sqrt(n),Units=max(UNITS))  


Post_burn_data <- LIMSP_Provisional_Data_Tidy %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP",COLLECT_DATE <"2025-05-01 00:00:00",COLLECT_DATE>"2025-04-09 00:00:00") %>%
add_row(`Burn Day`= 2, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA) %>%
add_row(`Burn Day`= 3, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA) %>%
add_row(`Burn Day`= 5, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA) %>%
add_row(`Burn Day`= 7, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA) %>%
add_row(`Burn Day`= 8, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA) %>%
add_row(`Burn Day`= 9, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA) %>%
add_row(`Burn Day`= 10, Treatment="Burn",TEST_NAME="TPO4",VALUE =NA)


Post_burn_data_summary <- Post_burn_data %>%
mutate(Date=as.Date(COLLECT_DATE)) %>%  
group_by(Date,`Burn Day`,Treatment,TEST_NAME)  %>%
summarise(n=n(),Mean=mean(VALUE),SD=sd(VALUE),SE=SD/sqrt(n),Units=max(UNITS))  

#summary of field readings by treatment
Field_Readings_tidy_summary_Treatment <- Field_Readings_tidy %>%
group_by(Date,Treatment,TEST_NAME)  %>%
summarise(n=n(),Mean=mean(VALUE),SD=sd(VALUE),SE=SD/sqrt(n))  

#summary of field readings by Day
Field_Readings_tidy_summary_day <- Field_Readings_tidy %>%
group_by(Date,TEST_NAME)  %>%
summarise(n=n(),Mean=mean(VALUE),SD=sd(VALUE),SE=SD/sqrt(n))  


#Field readings- difference from daily mean
Field_Readings_tidy_diff <- Field_Readings_tidy %>%
left_join(Field_Readings_tidy_summary_day,by=c("Date","TEST_NAME")) %>%
mutate(`Differnce from daily mean`=VALUE-Mean) 



# Water quality figures ---------------------------------------------------------------

#TP Overview
ggplot(filter(WQ_Summary,TEST_NAME=="TPO4",Date <"2026-05-01"),aes(Date ,Mean*1000,ymin = (Mean - SE)*1000, ymax = (Mean + SE)*1000,color=Treatment,fill=Treatment))+
geom_rect(aes(xmin =as.Date("2025-04-09") ,xmax=as.Date("2025-04-25"),ymin=-Inf,ymax=Inf),fill="#fb9a99",alpha=.5,color="grey20")+  
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+
scale_x_date(date_labels = "%b %d",date_breaks = "2 weeks")+coord_cartesian(xlim = as.Date(c("2024-11-14", "2025-04-22")),ylim=c(0,550),expand = FALSE)+
ylab(expression(TPO4~(ug/L)))+labs(title="")+fire_theme+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/TP Overtime.jpeg",width =16, height =9, units = "in")

#TP close_Up
ggplot(filter(Post_burn_data_summary ,TEST_NAME=="TPO4",Date >"2025-04-09"),aes(Date ,Mean*1000,ymin = (Mean - SE)*1000, ymax = (Mean + SE)*1000,color=Treatment,fill=Treatment))+
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+geom_line()+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+coord_cartesian(xlim = as.Date(c("2025-04-10", "2025-04-21")))+
ylab(expression(TPO4~(ug/L)))+labs(title="")+fire_theme

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/TP closeup.jpeg",width =16, height =9, units = "in")

#P fractions close_Up
ggplot(filter(Post_burn_data_summary ,TEST_NAME %in% c("PP","SRP","DOP"),Date >"2025-04-09"),aes(Date ,Mean*1000,ymin = (Mean - SE)*1000, ymax = (Mean + SE)*1000,color=Treatment,fill=Treatment))+
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+geom_line()+facet_wrap(~TEST_NAME,nrow = 3,scales="free_y",strip.position="left")+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+coord_cartesian(xlim = as.Date(c("2025-04-10", "2025-04-21")))+
ylab(expression(P~Forms~(ug/L)))+fire_theme

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/P fractions closeup.jpeg",width =16, height =9, units = "in")

#stacked bar plot of P fractions  
ggplot(filter(WQ_Summary ,TEST_NAME %in% c("PP","SRP","DOP"),`Date` >"2025-01-01",`Date` <"2025-05-01"),aes(as.factor(`Burn Day`) ,Mean*1000,fill=TEST_NAME))+
geom_col(color="grey20")+facet_wrap(~Treatment,nrow = 1,strip.position="top")+
ylab(expression(ug/L))+fire_theme+theme(legend.position="bottom")+xlab("Days from burn")+labs(fill="P Form") 

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/P fractions stacked columns closeup.jpeg",width =16, height =9, units = "in")

#Nitrogen forms close_Up
ggplot(filter(Post_burn_data_summary ,TEST_NAME %in% c("TN","NOX","NH4"),Date >"2025-04-09"),aes(Date ,Mean,ymin = (Mean - SE), ymax = (Mean + SE),color=Treatment,fill=Treatment))+
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+geom_line()+facet_wrap(~TEST_NAME,nrow = 3,scales="free_y",strip.position="left")+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+coord_cartesian(xlim = as.Date(c("2025-04-10", "2025-04-21")))+
ylab(expression(Nitrogen~Forms~(mg/L)))+fire_theme

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/N fractions closeup.jpeg",width =16, height =9, units = "in")

#photosynthetic pigments  forms close_Up
ggplot(filter(Post_burn_data_summary ,TEST_NAME %in% c("Chlorophyll A","Chlorophyll B","Pheophytin A"),Date >"2025-04-09"),aes(Date ,Mean,ymin = (Mean - SE), ymax = (Mean + SE),color=Treatment,fill=Treatment))+
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+geom_line()+facet_wrap(~TEST_NAME,nrow = 3,scales="free_y",strip.position="left")+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+coord_cartesian(xlim = as.Date(c("2025-04-10", "2025-04-21")))+
ylab(expression(Photosynthetic~Pigments~(mg/L)))+fire_theme

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/photosynthetic pigments closeup.jpeg",width =16, height =9, units = "in")


#DOC  close_Up
ggplot(filter(Post_burn_data_summary ,TEST_NAME %in% c("DOC"),Date >"2025-04-09"),aes(Date ,Mean,ymin = (Mean - SE), ymax = (Mean + SE),color=Treatment,fill=Treatment))+
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+geom_line()+facet_wrap(~TEST_NAME,nrow = 3,scales="free_y",strip.position="left")+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+coord_cartesian(xlim = as.Date(c("2025-04-10", "2025-04-21")))+
ylab(expression(DOC~(mg/L)))+fire_theme

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/DOC closeup.jpeg",width =16, height =9, units = "in")

#DOC Overview
ggplot(filter(WQ_Summary,TEST_NAME=="DOC",Date <"2025-05-01"),aes(Date ,Mean,ymin = (Mean - SE), ymax = (Mean + SE),color=Treatment,fill=Treatment))+
geom_rect(aes(xmin =as.Date("2025-04-09") ,xmax=as.Date("2025-04-25"),ymin=-Inf,ymax=Inf),fill="#fb9a99",alpha=.5,color="grey20")+  
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+
scale_x_date(date_labels = "%b %d",date_breaks = "2 weeks")+coord_cartesian(xlim = as.Date(c("2024-11-14", "2025-04-22")),ylim=c(15,35),expand = FALSE)+
ylab(expression(DOC~(mg/L)))+labs(title="")+fire_theme+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/DOC Overtime.jpeg",width =16, height =9, units = "in")



# Field Readings ----------------------------------------------------------

# Field reading by date 
ggplot(filter(Field_Readings_tidy_summary_Treatment, TEST_NAME %in% c("Temp","DO","SpCond","pH"),Date <"2025-05-01",Date >"2025-04-09"),aes(Date ,Mean,ymin = (Mean - SE), ymax = (Mean + SE),color=Treatment,fill=Treatment))+
geom_errorbar(size=1)+  geom_point(shape=21,size=4,color="black")+guides(colour = "none")+facet_wrap(~TEST_NAME,nrow = 4,scales="free_y",strip.position="left")+
scale_x_datetime(date_labels = "%b %d",date_breaks = "2 days")+coord_cartesian(xlim = as.POSIXct(c("2025-04-10", "2025-04-22")))+
ylab(expression(Temp~(mg/L)))+labs(title="")+fire_theme+theme(legend.position="bottom")

# Field reading by difference from daily mean over time
ggplot(filter(Field_Readings_tidy_diff, TEST_NAME %in% c("Temp","DO","SpCond","pH")),aes(Date ,`Differnce from daily mean`,color=Treatment,fill=Treatment))+
geom_point(shape=21,size=4,color="black")+guides(colour = "none")+facet_wrap(~TEST_NAME,nrow = 4,scales="free_y",strip.position="left")+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 week")+coord_cartesian(xlim = as.POSIXct(c("2024-11-10", "2025-07-22")))+
ylab(expression(Temp~(mg/L)))+labs(title="")+fire_theme+theme(legend.position="bottom")

# Field reading by difference from daily- mean boxplot pre vs post burn
ggplot(filter(Field_Readings_tidy_diff, TEST_NAME %in% c("Temp","DO","SpCond","pH")),aes(Phase,`Differnce from daily mean`,fill=Treatment))+
geom_boxplot()+guides(colour = "none")+facet_wrap(~TEST_NAME,nrow = 1,scales="free_y",strip.position="top")+
ylab("")+labs(title="")+fire_theme+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/Field Readings Difference from daily mean.jpeg",width =16, height =9, units = "in")



# Flow Figures ------------------------------------------------------------
STA34_flow_DA_tidy 

# Flow over time 
ggplot(STA34_flow_DA_tidy ,aes(Date ,Flow,color=`Structure Location`,color=`Structure Location`))+
geom_rect(aes(xmin =as.Date("2025-04-10") ,xmax=as.Date("2025-04-21"),ymin=-Inf,ymax=Inf),fill="#fb9a99",alpha=.5,color="grey20")+  
geom_line(size=1.5)+
scale_x_date(date_labels = "%b %d",date_breaks = "1 week")+coord_cartesian(xlim = as.Date(c("2025-01-01", "2025-06-29")),expand=F)+
ylab(expression(Flow~(cfs)))+labs(title="")+fire_theme+scale_color_manual(values = c("#cab2d6","#33a02c"))+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/Flow over time.jpeg",width =16, height =9, units = "in")



# Depth Figures -----------------------------------------------------------

#Water Depth from field readings
ggplot(filter(Field_Readings_tidy, str_detect(TEST_NAME,"Water Depth")),aes(`Date Time`,VALUE,color=Treatment,fill=Treatment))+
geom_point()+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+coord_cartesian(xlim = as.POSIXct(c("2025-01-01", "2025-06-29")))+
ylab(expression(Water~Depth~(cm)))+labs(title="")+fire_theme+theme(legend.position="bottom")

#Depth to consolidated substrate (DCS) from field readings
ggplot(filter(Field_Readings_tidy, str_detect(TEST_NAME,"DCS")),aes(`Date Time`,VALUE,color=Treatment,fill=Treatment))+
geom_point()+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+coord_cartesian(xlim = as.POSIXct(c("2025-01-01", "2025-06-29")))+
ylab(expression(Water~Depth~(cm)))+labs(title="")+fire_theme+theme(legend.position="bottom")

#Depth calculated from stage gauge
ggplot(STA34_depth_tidy,aes(`Date Time`,`Depth to Consolidated Substrate (calculated)`,color=`Gauge`))+
geom_line(size=1.5)+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 week")+coord_cartesian(xlim = as.POSIXct(c("2025-01-01", "2025-06-29")))+
ylab("Water Depth Calculated (cm)")+labs(title="")+fire_theme+scale_color_manual(values = c("#cab2d6","#33a02c"))+theme(legend.position="bottom")

#Depth calculated from stage gauge and field readings
ggplot(STA34_depth_tidy,aes(`Date Time`,`Depth to Consolidated Substrate (calculated)`,color=`Gauge`))+
geom_rect(aes(xmin =as.POSIXct("2025-04-10") ,xmax=as.POSIXct("2025-04-21"),ymin=-Inf,ymax=Inf),fill="#fb9a99",alpha=.5,color="grey20")+  
geom_line(size=1.5)+  
geom_point(data=filter(Field_Readings_tidy, str_detect(TEST_NAME,"DCS")),aes(`Date Time`,VALUE,fill=Treatment),color="grey30",shape=21)+  
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 week")+coord_cartesian(xlim = as.POSIXct(c("2025-01-01", "2025-06-29")),expand=F)+
ylab("Depth to Consolidated Substrate Calculated (cm)")+labs(title="")+fire_theme+
scale_color_manual(values = c("#e5d8bd","#fed9a6"))+scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))+
theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Presentation figs/Depth over time.jpeg",width =16, height =9, units = "in")



# Sonde Figures -----------------------------------------------------------


Pre_post_fire_sonde <- Sonde_Data_Qualified %>%
mutate(Phase=if_else(Date_time<"2025-04-10 12:00:00","Pre-burn","Post-burn")) %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))


#Diel Trends for all parameters
ggplot(filter(Pre_post_fire_sonde,Parameter %in% c("Temp °C","DO (mg/L)","SpCond (µS/cm)","pH","Chl (RFU)"),is.na(`Remark Code`)),aes(hour(Date_time) ,Value,color=`Site`,fill=`Site`))+
#geom_point(shape=21,size=2,color="black",alpha=.9)+
geom_smooth()+
scale_x_continuous(limits = c(0,24),breaks=seq(0,24,4))+  
facet_grid(Parameter~Phase,scales = "free_y")+fire_theme

#time series for all parameters
ggplot(filter(Pre_post_fire_sonde,Parameter %in% c("Temp °C","DO (mg/L)","SpCond (µS/cm)","pH","Chl (RFU)"),is.na(`Remark Code`)),aes(Date_time ,Value,color=`Site`,fill=`Site`))+
geom_point(shape=21,size=2,color="black",alpha=.9)+scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
facet_wrap(~Parameter,scales = "free_y",nrow=5)+fire_theme

#time series for all parameters close-up of burn period
ggplot(filter(Pre_post_fire_sonde,Parameter %in% c("Temp °C","DO (mg/L)","SpCond (µS/cm)","pH","Chl (RFU)"),is.na(`Remark Code`)),aes(Date_time ,Value,color=`Site`,fill=`Site`))+
geom_point(shape=21,size=2,color="black",alpha=.5)+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
coord_cartesian(xlim = as.POSIXct(c("2025-03-15", "2025-05-25")))+  
facet_wrap(~Parameter,scales = "free_y",nrow=5)+fire_theme

#time series for all parameters close-up of burn period for temp DO and pH
ggplot(filter(Pre_post_fire_sonde,Parameter %in% c("Temp °C","DO (mg/L)","pH"),Date_time>as.POSIXct("2025-04-01"),Date_time<as.POSIXct("2025-05-01")),aes(Date_time ,Value,color=`Site`,fill=`Site`))+
geom_rect(aes(xmin =as.POSIXct("2025-04-10") ,xmax=as.POSIXct("2025-04-21"),ymin=-Inf,ymax=Inf),fill="#fb9a99",alpha=.5,color="grey20")+  
geom_point(shape=21,size=2,color="black",alpha=.5)+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
coord_cartesian(xlim = as.POSIXct(c("2025-04-01", "2025-04-25")))+  
facet_wrap(~Parameter,scales = "free_y",nrow=5)+fire_theme

#time series for preburn period with all 4 sondes working close-up of burn period for temp DO and pH
ggplot(filter(Pre_post_fire_sonde,Parameter %in% c("Temp °C","DO (mg/L)","pH"),Date_time>as.POSIXct("2025-03-01"),Date_time<as.POSIXct("2025-03-20")),aes(Date_time ,Value,color=`Site`,fill=`Site`))+
geom_point(shape=21,size=2,alpha=.5)+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 month")+
coord_cartesian(xlim = as.POSIXct(c("2025-03-07", "2025-03-14")))+  
facet_wrap(~Parameter,scales = "free_y",nrow=5)+fire_theme

#time series for postburn period with all 4 sondes working close-up of burn period for temp DO and pH
ggplot(filter(Pre_post_fire_sonde,Parameter %in% c("Temp °C","DO (mg/L)","pH"),Date_time>as.POSIXct("2025-05-01"),Date_time<as.POSIXct("2026-06-20"),if_else(Site=="Herbicide"& Parameter=="pH",F,T)),aes(Date_time ,Value,color=`Site`,fill=`Site`))+
geom_point(shape=21,size=2,alpha=.5)+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 week")+
coord_cartesian(xlim = as.POSIXct(c("2025-05-16", "2025-05-30")))+  
facet_wrap(~Parameter,scales = "free_y",nrow=5)+fire_theme




# Soils Figures --------------------------------------------------------------
#time series for postburn period with all 4 sondes working close-up of burn period for temp DO and pH
ggplot(filter(Soils_tidy,TEST_NAME %in% c("TCA-SOL","TP-SOL","TN-SOL","TC-SOL")),aes(MATRIX ,VALUE,fill=`Phase`))+
geom_boxplot(color="grey20")+#geom_point(shape=21,size=2,alpha=.5,color="grey20")+
facet_grid(TEST_NAME~STATION,scales="free_y")+fire_theme



