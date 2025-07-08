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


# Test figs ---------------------------------------------------------------



LIMSP_Provisional_WQ <-LIMSP_Provisional_Data_Tidy %>%
mutate(Date=as.Date(COLLECT_DATE),`Source`="Fire Study") %>%
filter(TEST_NAME=="TPO4",Date >"2025-04-09",MATRIX=="SW",COLLECT_METHOD=="GP")  %>%
select(Date,`STATION`,VALUE) %>%
  pivot_wider(names_from="STATION",values_from = VALUE) 


STA34_Compliance_tidy <- STA34_compliance_provisional %>%
mutate(Date=as.Date(ymd_hms(COLLECT_DATE))) %>% 
select(Date,`STATION`,`COLLECT_METHOD`,VALUE) %>%
pivot_wider(names_from=c("COLLECT_METHOD","STATION"),values_from = VALUE) 

All_data_tidy <- STA34_flow_DA_tidy %>%
select(Date) %>%  
distinct() %>%  
left_join(STA34_Compliance_tidy,by="Date") %>%
left_join(LIMSP_Provisional_WQ,by="Date")  %>%
pivot_longer(names_to = "Station",values_to="Value",2:17)  %>%
mutate(`Source`=if_else(str_detect(Station,"G38"),"Compliance","Fire Study"))  


#flow and WQ post-burn
ggplot(filter(STA34_flow_DA_tidy,Date >"2025-04-09"),aes(Date,`Flow`,color=`Structure Location`))+  
geom_rect(aes(xmin =as.Date("2025-04-09") ,xmax=as.Date("2025-04-11"),ymin=-Inf,ymax=Inf),fill="#fb9a99",alpha=.5,color="grey80")+   
geom_line(size=2)+
geom_point(data=filter(All_data_tidy,Date >"2025-04-09"),aes(Date,Value*1000,fill=Source),inherit.aes = F,color="black",shape=21,size=2.5,alpha=.8)+
scale_x_date(date_labels = "%b %d",date_breaks = "2 weeks")+
scale_y_continuous(expression(TPO4~(ug/L)), sec.axis = sec_axis(~ . * 1, name = "Flow (cfs)")) + 
scale_fill_manual(values = c("#e31a1c","#33a02c"))+scale_color_manual(values = c("#cab2d6","#91bfdb"))+
theme_bw(base_size = 20)+theme(legend.position="bottom",axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1)) + 
labs(title="")

ggsave(plot = last_plot(),filename="./Figures/Compliance and Fires Study data.jpeg",width =16, height =9, units = "in")


