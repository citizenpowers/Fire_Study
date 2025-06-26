#Object of this script is to import and tidy NuLab SRP data




library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)
#library(Rccp)
library(scales)
library(ggpmisc)



# Import Data -------------------------------------------------------------

NuLab_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/NuLab Data.csv")
NuLab_Data2 <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/NuLab Data 2.csv")
All_Phosphate_Data <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/All Phosphate Data.csv", skip = 3)
NuLAB_standards_test_02172025 <- read_excel("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/NuLab Sensor/NuLAB standards test 02172025.xlsx")


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




# Lab test 1: test of 4 lab standards std1=8, std2=50, std3=200,std4=300 in labratory conditions--------

Bench_test_with_lab_standards <- NuLAB_standards_test_02172025 %>%
mutate(`Standard Value`=case_when(`Standard ID`=="DI"~"DI",
                                  `Standard ID`=="Standard 1"~"8 ppb",
                                  `Standard ID`=="Standard 2"~"50 ppb",
                                  `Standard ID`=="Standard 3"~"200 ppb",
                                  `Standard ID`=="Standard 4"~"300 ppb")) %>%
mutate(`Standard`=case_when(`Standard ID`=="DI"~2,
                                    `Standard ID`=="Standard 1"~8,
                                    `Standard ID`=="Standard 2"~50,
                                    `Standard ID`=="Standard 3"~200,
                                    `Standard ID`=="Standard 4"~300)) %>%  
mutate(`Standard Value` = factor(`Standard Value`, levels = c("DI", "8 ppb", "50 ppb","200 ppb","300 ppb")))


#Low Concentration bench test 
ggplot(filter(Bench_test_with_lab_standards,`Standard Value` %in% c("8 ppb","50 ppb","DI") ),aes(`Standard Value`,Smp_conc*1000,fill=`Standard Value`))+
scale_y_continuous(breaks = breaks_pretty(5))+  
ylab(expression(OPO4~(ug/L)))+labs(title="NuLab Bench Test with Lab Standards")+ geom_boxplot()+
geom_jitter(shape=21,size=2.5,color="black",height=0,alpha=.5)+theme_bw(base_size = 16)

ggsave(plot = last_plot(),filename="./Figures/Low Concentration bench test.jpeg",width =8, height =9, units = "in")


#calculate r
r_squared <- cor(filter(Bench_test_with_lab_standards,`Standard Value` %in% c("8 ppb","50 ppb","DI"),!is.na(Smp_conc))$Standard,filter(Bench_test_with_lab_standards,`Standard Value` %in% c("8 ppb","50 ppb","DI"),!is.na(Smp_conc))$Smp_conc*1000)^2 %>%
format(digits=3)
#Low Concentration bench test with R squared
ggplot(filter(Bench_test_with_lab_standards,`Standard Value` %in% c("8 ppb","50 ppb","DI"),!is.na(Smp_conc) ),aes(`Standard`,Smp_conc*1000,fill=`Standard Value`))+
scale_y_continuous(breaks = breaks_pretty(5))+geom_smooth(method="lm",fill="grey",color="grey20")+
annotate(geom = "text", x =20, y = 50,label=paste("R squared ",r_squared))+  
ylab(expression(NuLab~OPO4~(ug/L)))+xlab("Lab Measurement")+labs(title="NuLab Bench Test with Lab Standards")+
geom_point(shape=21,size=2.5,color="black",alpha=.5)+theme_bw(base_size = 20)+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Low Concentration bench test with R squared.jpeg",width =16, height =9, units = "in")


#High level bench test 
ggplot(filter(Bench_test_with_lab_standards,`Standard Value` %in% c("200 ppb","300 ppb") ),aes(`Standard Value`,Smp_conc*1000,fill=`Standard Value`))+
scale_y_continuous(breaks = breaks_pretty(5))+  
ylab(expression(OPO4~(ug/L)))+labs(title="NuLab Bench Test with Lab Standards")+ geom_boxplot()+
geom_jitter(shape=21,size=2.5,color="black",height=0,alpha=.5)+theme_bw(base_size = 16)

ggsave(plot = last_plot(),filename="./Figures/High Concentration bench test.jpeg",width =8, height =9, units = "in")


# Lab test 2: Test of field samples in laboratory conditions --------------------------

Lab_test_2 <- NuLab_Data_Tidy %>% 
filter(Date_Time >"2025-02-20 00:00:00",Date_Time<"2025-02-23 10:00:00") %>%
mutate(Site=case_when(Date_Time>"2025-02-21 11:00:00"~"G310 (Lab Measured <MDL)",
                      Date_Time<"2025-02-20 09:00:20"~"DI",
                      TRUE~"G304J (Lab Measured TP=126, OPO4 not measured)"))  %>%
mutate(`Site` = factor(`Site`, levels = c("DI", "G310 (Lab Measured <MDL)", "G304J (Lab Measured TP=126, OPO4 not measured)")))  
  
#Field Samples in Lab test
ggplot(Lab_test_2,aes(Site,Smp_conc*1000,fill=Site))+
scale_y_continuous(breaks = breaks_pretty(10))+  
ylab(expression(OPO4~(ug/L)))+labs(title="NuLab Bench Test with Field Samples")+
geom_jitter(shape=21,size=2.5,color="black",height=0,alpha=.5)+theme_bw(base_size = 20)+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/Bench test with Field samples.jpeg",width =16, height =9, units = "in")



# Test 3: NuLab in Field --------------------------------------------------

#Filter to just time that the NULab has been working. 
ggplot(filter(NuLab_Data_Tidy,Date_Time>"2025-04-11 10:10:00",QC_Flag=="'00000000'"),aes(Date_Time,Smp_conc*1000))+
geom_rect(aes(xmin =as_datetime("2025-04-11 10:10:00") ,xmax=as_datetime("2025-04-28 10:10:00"),ymin=-Inf,ymax=Inf),fill="#fbb4ae")+ 
annotate(geom = "text", x =as_datetime("2025-04-19 10:10:00"), y = 50,label="Blown fuse. Transfer pump not working")+  
scale_x_datetime(date_labels = "%b %d",date_breaks = "1 week")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
geom_smooth(data=filter(SRP_WQ_data,Date_Time>"2025-04-01 10:10:00"),aes(Date_Time,VALUE*1000),size=1,color="black",inherit.aes = F,span=.9)+  
ylab(expression(OPO4~(ug/L)))+labs(title="NuLab in Field")+
geom_point(shape=21,size=2.5,color="black",fill="#ff7f00")+
geom_point(data=filter(SRP_WQ_data,Date_Time>"2025-04-01 10:10:00"),aes(Date_Time,VALUE*1000),shape=21,size=3,color="black",fill="#33a02c",inherit.aes = F)+
theme_bw(base_size = 20)

ggsave(plot = last_plot(),filename="./Figures/Field Test NuLab.jpeg",width =16, height =9, units = "in")


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
 




