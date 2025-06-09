#Import and tidy fire water quality data




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
LIMSP_Provisional_Data_letters <- read_csv("./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy_letters.csv")


# Tidy Data ---------------------------------------------------------------

LIMSP_Provisional_Data_Tidy1 <- LIMSP_Provisional_Data %>%
select(SAMPLE_ID,STATION,MATRIX,COLLECT_METHOD,SAMPLE_TYPE,COLLECT_DATE,DEPTH,FST,FIRST_TRIGGER_DATE,TEST_NAME,VALUE,UNITS, SAMP_COMMENT_NR ) %>%  
mutate(TEST_NAME=if_else(TEST_NAME=="OPO4","SRP",TEST_NAME)) %>%                              #rename OPO4 to SRP
mutate(Treatment=case_when(str_detect(STATION,"Untreated")~"Untreated",                       #add treatment variable
                           str_detect(STATION,"Burn_Herb")~"Burn_Herb",
                           STATION %in% c("Burn_A","Burn_B","Burn_C")~"Burn",
                           str_detect(STATION,"Herbicide")~"Herbicide",
                           TRUE ~ NA)) %>%
mutate(Block=case_when(STATION %in% c("Untreated_A","Burn_A","Herbicide_A","Burn_Herb_A")~"A",          #add block variable
                       STATION %in% c("Untreated_B","Burn_B","Herbicide_B","Burn_Herb_B")~"B",
                       STATION %in% c("Untreated_C","Burn_C","Herbicide_C","Burn_Herb_C")~"C",
                       TRUE ~ NA))  %>%
mutate(FIRST_TRIGGER_DATE=mdy_hms(FIRST_TRIGGER_DATE)) %>%
mutate(`Burn Day`= as.numeric(difftime(date(COLLECT_DATE),date("2025-04-10 14:00:00 UTC"),units="day"))) %>% #calculate days to burn
mutate(`Date_Treatment`=paste(`Burn Day`," ",Treatment)) 

#Calculate DOP 
DOP <- LIMSP_Provisional_Data_Tidy1 %>%
filter(TEST_NAME %in% c("TDPO4","SRP","TPO4"),COLLECT_METHOD=="GP")  %>%
pivot_wider(names_from = "TEST_NAME",values_from = "VALUE")  %>%
filter(!is.na(TDPO4)) %>%  
mutate(DOP=TDPO4-SRP,PP=TPO4-TDPO4) %>%
pivot_longer(names_to = "TEST_NAME",values_to = "VALUE",16:20) %>%
filter(TEST_NAME %in% c("PP","DOP"))  

#Add DOP to dataset 
LIMSP_Provisional_Data_Tidy <- bind_rows(LIMSP_Provisional_Data_Tidy1,DOP) 

write_csv(LIMSP_Provisional_Data_Tidy ,"./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy.csv")




# All parameters (histograms) ---------------------------------------------

ggplot(filter(LIMSP_Provisional_Data_Tidy_letters_log ,COLLECT_METHOD=="GP",COLLECT_DATE>"2025-02-01 00:00:00",COLLECT_DATE<"2025-05-01 00:00:00",MATRIX=="SW"),aes(VALUE))+
geom_histogram()+  facet_wrap(~TEST_NAME,scales="free")+
theme_bw()+theme(legend.position="bottom")




# OPO4 Figures -----------------------------------------------------------------

#Grab samples OPO4 only Points

ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="OPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes((COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))+
geom_smooth( span = 1,se=T,alpha=.1)+
geom_rect(inherit.aes=F,aes(xmin =stage("2025-04-10", after_scale = xmin-0.5) ,xmax=stage("2025-04-21", after_scale = xmax+0.5),,ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
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
geom_rect(inherit.aes=F,aes(xmin =stage("2025-04-10", after_scale = xmin-0.5) ,xmax=stage("2025-04-21", after_scale = xmax+0.5),,ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Date")+
ylab(expression(OPO4~(mg/L)))+labs(title="OPO4 over time")+
geom_boxplot(aes(fill=Treatment))+theme_bw()

#Grab samples OPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="OPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 3, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 3, label = "Post-burn") +
ylab(expression(OPO4~(mu~g/L)))+scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()


# TP figures --------------------------------------------------------------

ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes(as_datetime(COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment))+
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))+
geom_smooth( span = 5,se=T,alpha=.1)+
#geom_rect(inherit.aes=F,aes(xmin =stage("2025-04-10", after_scale = xmin-0.5) ,xmax=stage("2025-04-21", after_scale = xmax+0.5),,ymin=-Inf,ymax=Inf),fill="#fb9a99")+  
xlab("Month")+
ylab(expression(TPO4~(mg/L)))+labs(title="TPO4 over time")+
geom_point(shape=21,size=2,color="black")+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Seasonal Flow.jpeg",width =16, height =9, units = "in")

#Grab samples TPO4 only column
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-04-09 00:00:00",MATRIX=="SW"),aes(as.Date(COLLECT_DATE) ,VALUE,color=Treatment,fill=Treatment,))+
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
#geom_rect(aes(xmin = as.Date("2025-04-10 10:00:00"), xmax = as.Date("2025-04-10 14:00:00"),ymin = -Inf, ymax = Inf),fill="#fcbba1",color="#fcbba1")+
xlab("Month")+
ylab(expression(TPO4~(mg/L)))+labs(title="TPO4 over time")+
geom_col(position="dodge",color="black",alpha=.5)+theme_bw()

#Grab samples TPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 10, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 10, label = "Post-burn") +
ylab(expression(TPO4~(mu~g/L)))+scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/TP over time- Provisional.jpeg",width =16, height =9, units = "in")

# DOP -------------------------------------------------------------------

#Grab samples TPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="DOP",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 3, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 3, label = "Post-burn") +
ylab(expression(DOP~(mu~g/L)))+scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()


# TN figures  -------------------------------------------------------------

#Grab samples TN only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,TEST_NAME=="TN",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 900, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 900, label = "Post-burn") +
ylab(expression(TN~(mu~g/L)))+scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()

# DOC Figures -------------------------------------------------------------

#Grab samples DOC only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,TEST_NAME=="DOC",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 15000, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 15000, label = "Post-burn") +
ylab(expression(DOC~(mu~g/L)))+#scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()

# NH4 Figures -------------------------------------------------------------

#Grab samples NH4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,TEST_NAME=="NH4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 10, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 10, label = "Post-burn") +
ylab(expression(NH4~(mu~g/L)))+#scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()


# NOx Figures -------------------------------------------------------------

#Grab samples NOX only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="NH4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=0,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=0,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 10, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 10, label = "Post-burn") +
ylab(expression(NOx~(mu~g/L)))+#scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()


# Chlorophyll A Figures ---------------------------------------------------

#Grab samples Chlorophyll A only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,TEST_NAME=="Chlorophyll A",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 10, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 10, label = "Post-burn") +
ylab(expression(Chlorophyll~A~(mu~g/L)))+#scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()

# Chlorophyll B Figures ---------------------------------------------------

#Grab samples Chlorophyll B only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,TEST_NAME=="Chlorophyll B",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 10, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 10, label = "Post-burn") +
ylab(expression(Chlorophyll~B~(mu~g/L)))+#scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()

# Pheophytin A Figures ---------------------------------------------------

#Grab samples Pheophytin A only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,TEST_NAME=="Pheophytin A",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=T,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=T,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 10, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 10, label = "Post-burn") +
ylab(expression(Pheophytin~A~(mu~g/L)))+#scale_y_log10()+
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .8))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()


# Autosampler figures -----------------------------------------------------

#Grab samples Pheophytin A only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="TPO4",COLLECT_METHOD=="ADT",MATRIX=="SW",FST>"2024-01-22"),aes((as.Date(FST)),fill=Treatment,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
scale_x_date(date_labels = "%b %d",date_breaks = "1 day")+
xlab("Date")+theme_bw()+
ylab(expression(TPO4~(mg/L)))+labs(title="TPO4 over time")+theme(axis.text.x=element_text(angle=90, hjust=1))+
geom_point(shape=21,size=2)
