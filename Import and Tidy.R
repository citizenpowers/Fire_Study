#Import and tidy fire data




library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)
library(Rccp)

# Import Data -------------------------------------------------------------

#work links
LIMSP_Provisional_Data <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Water Quality Data/LIMSP Provisional Data.xlsx")



# Tidy Data ---------------------------------------------------------------

LIMSP_Provisional_Data_Tidy <- LIMSP_Provisional_Data %>%
mutate(Treatment=case_when(str_detect(STATION,"Untreated")~"Untreated",
                           str_detect(STATION,"Burn_Herb")~"Burn_Herb",
                           STATION %in% c("Burn_A","Burn_B","Burn_C")~"Burn",
                           str_detect(STATION,"Herbicide")~"Herbicide",
                           TRUE ~ NA)) %>%
mutate(Block=case_when(STATION %in% c("Untreated_A","Burn_A","Herbicide_A","Burn_Herb_A")~"A",
                       STATION %in% c("Untreated_B","Burn_B","Herbicide_B","Burn_Herb_B")~"B",
                       STATION %in% c("Untreated_C","Burn_C","Herbicide_C","Burn_Herb_C")~"C",
                       TRUE ~ NA))  %>%
mutate(FIRST_TRIGGER_DATE=mdy_hms(FIRST_TRIGGER_DATE))
 test<- format("03/22/2025 03:00:00 AM")



# OPO4 Figures -----------------------------------------------------------------

#Grab samples OPO4 only Points

ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="OPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes(as_datetime(COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))+
geom_smooth( span = 1,se=T,alpha=.1)+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = as_datetime("2025-04-10 14:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
xlab("Month")+
ylab(expression(OPO4~(mg/L)))+labs(title="OPO4 over time")+
geom_point(shape=21,size=2,color="black")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal Flow.jpeg",width =16, height =9, units = "in")

#Grab samples OPO4 only column
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="OPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes(as.Date(COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment))+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
#geom_rect(aes(xmin = as.Date("2025-04-10 10:00:00"), xmax = as.Date("2025-04-10 14:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
xlab("Month")+
ylab(expression(OPO4~(mg/L)))+labs(title="OPO4 over time")+
geom_col(position="dodge",color="black",alpha=.5)+theme_bw()

#Grab samples OPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="OPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=F,aes(xmin = "2025-04-10",xmax="2025-04-21",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(OPO4~(mg/L)))+labs(title="OPO4 over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()


# TP figures --------------------------------------------------------------

ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes(as_datetime(COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))+
geom_smooth( span = 5,se=T,alpha=.1)+
geom_rect(aes(xmin = as_datetime("2025-04-10 10:00:00"), xmax = as_datetime("2025-04-10 14:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
xlab("Month")+
ylab(expression(TPO4~(mg/L)))+labs(title="TPO4 over time")+
geom_point(shape=21,size=2,color="black")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal Flow.jpeg",width =16, height =9, units = "in")

#Grab samples TPO4 only column
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes(as.Date(COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment))+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
#geom_rect(aes(xmin = as.Date("2025-04-10 10:00:00"), xmax = as.Date("2025-04-10 14:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
xlab("Month")+
ylab(expression(TPO4~(mg/L)))+labs(title="TPO4 over time")+
geom_col(position="dodge",color="black",alpha=.5)+theme_bw()

#Grab samples TPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
#geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-21",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+scale_y_continuous(breaks = seq(0,750,50))+#scale_y_log10()+
ylab(expression(TPO4~(ug/L)))+labs(title="TPO4 over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/TP over time- Provisional.jpeg",width =16, height =9, units = "in")


1# TDPO4 -------------------------------------------------------------------

#Grab samples TPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TDPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(TDPO4~(mg/L)))+labs(title="TDPO4 over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()


# TN figures  -------------------------------------------------------------

#Grab samples TN only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TN",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(TN~(mg/L)))+labs(title="TN over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()

# DOC Figures -------------------------------------------------------------

#Grab samples DOC only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="DOC",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(DOC~(mg/L)))+labs(title="DOC over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()

# NH4 Figures -------------------------------------------------------------

#Grab samples NH4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="NH4",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(NH4~(mg/L)))+labs(title="NH4 over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()


# NOx Figures -------------------------------------------------------------

#Grab samples NOX only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="NOX",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(NOX~(mg/L)))+labs(title="NOX over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()


# Chlorophyll A Figures ---------------------------------------------------

#Grab samples Chlorophyll A only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="Chlorophyll A",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(Chlorophyll~A~(mg/L)))+labs(title="Chlorophyll A over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()

# Chlorophyll B Figures ---------------------------------------------------

#Grab samples Chlorophyll B only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="Chlorophyll B",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(Chlorophyll~B~(mg/L)))+labs(title="Chlorophyll B over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()

# Pheophytin A Figures ---------------------------------------------------

#Grab samples Pheophytin A only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="Pheophytin A",COLLECT_METHOD=="GP",COLLECT_DATE>"2024-03-09 00:00:00",MATRIX=="SW"),aes(as.factor(as.Date(COLLECT_DATE)) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(aes(xmin = "2025-04-10",xmax="2025-04-16",ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(Pheophytin~A~(mg/L)))+labs(title="Pheophytin A over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()


# Autosampler figures -----------------------------------------------------

#Grab samples Pheophytin A only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="ADT",MATRIX=="SW",FST>"2024-01-22"),aes((as.Date(FST)),fill=Treatment,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+
xlab("Date")+theme_bw()+
ylab(expression(TPO4~(mg/L)))+labs(title="TPO4 over time")+theme(axis.text.x=element_text(angle=90, hjust=1))+
geom_point(shape=21,size=2)
