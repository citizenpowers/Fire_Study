

library(stringr)
library(scales)
library(tidyverse)

# Import Data -------------------------------------------------------------

STA2_stations <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/STA Outflow/STA2_stations.csv")
STA34_stations <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/STA Outflow/STA34_stations.csv")
STA56_stations <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/STA Outflow/STA56_stations.csv")
STA1E_stations <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/STA Outflow/STA1E_stations.csv")
STA1W_stations <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/STA Outflow/STA1W_stations.csv")




# Tidy --------------------------------------------------------------------

STA2_stations_tidy <- STA2_stations %>%
mutate(Date_Time=dmy_hm(Collection_Date)) %>%
mutate(`Label Time`=ISOdate(3000,month(Date_Time),day(Date_Time),0,0),Year=as.factor(year(Date_Time)))  %>%
filter(is.na(Flag)) %>%
mutate(`Flow-way`=case_when(`Station ID`=="G330D"~paste("FW-1 ",`Station ID`),
                            `Station ID`=="G332"~paste("FW-2 ",`Station ID`),
                            `Station ID`=="G334"~paste("FW-3 ",`Station ID`),
                            `Station ID`=="G368"~paste("FW-4 ",`Station ID`),
                            `Station ID`=="G441"~paste("FW-8 ",`Station ID`))) 

STA34_stations_tidy <- STA34_stations %>%
mutate(Date_Time=dmy_hm(Collection_Date),`Station ID`=str_sub(`Station ID`,1,4)) %>%
mutate(`Label Time`=ISOdate(3000,month(Date_Time),day(Date_Time),0,0))  %>%
filter(is.na(Flag)) %>%
mutate(`Flow-way`=case_when(`Station ID`=="G381"~paste("FW-3 ",`Station ID`),
                              `Station ID`=="G379"~paste("FW-2 ",`Station ID`),
                              `Station ID`=="G376"~paste("FW-1 ",`Station ID`)))


write_csv(STA34_stations_tidy  ,"./Data/Water Quality Data/STA34_compliance_tidy.csv")

                            

STA56_stations_tidy <- STA56_stations %>%
mutate(Site=`Station ID`)%>%  
mutate(Date_Time=dmy_hm(Collection_Date),`Station ID`=str_sub(`Station ID`,1,4)) %>%
mutate(`Label Time`=ISOdate(3000,month(Date_Time),day(Date_Time),0,0))  %>%
filter(is.na(Flag)) %>%
  mutate(`Flow-way`=case_when(str_detect(`Station ID`,"G344")~paste("FW-1B-5B ",`Site`),
                              `Station ID`=="G352"~paste("FW-6-2 ",`Station ID`),
                              `Station ID`=="G354"~paste("FW-6-5 ",`Station ID`),
                              `Station ID`=="G393"~paste("FW-6-3 ",`Station ID`)))
                             


distinct(STA56_stations,`Station ID`)




STA1E_stations_tidy <- STA1E_stations %>%
  mutate(Site=`Station ID`)%>% 
mutate(Date_Time=dmy_hm(Collection_Date),`Station ID`=str_sub(`Station ID`,1,4)) %>%
mutate(`Label Time`=ISOdate(3000,month(Date_Time),day(Date_Time),0,0))  %>%
filter(is.na(Flag)) %>%
mutate(`Flow-way`=case_when(str_detect(`Station ID`,"S369")~paste("FW-4S ",`Site`),
                              `Station ID`=="S365"~paste("FW-2 ",`Station ID`),
                              `Station ID`=="S372"~paste("FW-6 ",`Station ID`)))


distinct(STA1E_stations,`Station ID`)                            

STA1W_stations_tidy <- STA1W_stations %>%
  
mutate(Date_Time=dmy_hm(Collection_Date),`Station ID`=str_sub(`Station ID`,1,4)) %>%
mutate(`Label Time`=ISOdate(3000,month(Date_Time),day(Date_Time),0,0))  %>%
filter(is.na(Flag)) %>%
  mutate(`Flow-way`=case_when(`Station ID`=="G306"~paste("FW-5B ",`Station ID`),
                              `Station ID`=="G332"~paste("FW-2 ",`Station ID`),
                              `Station ID`=="G334"~paste("FW-3 ",`Station ID`),
                              `Station ID`=="G368"~paste("FW-4 ",`Station ID`),
                              `Station ID`=="G441"~paste("FW-8 ",`Station ID`)))



# Figures -----------------------------------------------------------------

#STA2 annual variability last 10 years 
ggplot(filter(STA2_stations_tidy,Date_Time>as_datetime("2015-01-01 00:00:00")),aes(`Label Time`,Value*1000,fill=`Flow-way`,color=`Flow-way`))+
scale_x_datetime(date_labels = "%b" ,date_breaks = "1 month")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(TP~(ug/L)))+xlab("Month")+labs(title="TP over time STA-2")+geom_smooth(se=T,span=.2,method="loess")+
geom_point(shape=21,size=2.5,color="black",alpha=.1)+coord_cartesian( ylim = c(0, 100))+
theme_bw(base_size = 20)

ggsave(plot = last_plot(),filename="./Figures/STA2 outflow trends.jpeg",width =16, height =9, units = "in")


#STA34 annual variability last 10 years 
ggplot(filter(STA34_stations_tidy,Date_Time>as_datetime("2015-01-01 00:00:00")),aes(`Label Time`,Value*1000,fill=`Flow-way`,color=`Flow-way`))+
scale_x_datetime(date_labels = "%b" ,date_breaks = "1 month")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(TP~(ug/L)))+xlab("Month")+labs(title="TP over time STA-3/4")+geom_smooth(se=T,span=.2,method="loess")+
geom_point(shape=21,size=2.5,color="black",alpha=.1)+coord_cartesian( ylim = c(0, 100))+
theme_bw(base_size = 20)

ggsave(plot = last_plot(),filename="./Figures/STA34 outflow trends.jpeg",width =16, height =9, units = "in")


#STA56 annual variability last 10 years 
ggplot(filter(STA56_stations_tidy,Date_Time>as_datetime("2015-01-01 00:00:00")),aes(`Label Time`,Value*1000,fill=`Flow-way`,color=`Flow-way`,linetype = `Flow-way`))+
scale_x_datetime(date_labels = "%b" ,date_breaks = "1 month")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(TP~(ug/L)))+xlab("Month")+labs(title="TP over time STA-5/6")+
geom_point(shape=21,size=2.5,color="black",alpha=.1)+geom_smooth(se=F,span=.2,method="loess")+coord_cartesian( ylim = c(0, 200))+
theme_bw(base_size = 20)

ggsave(plot = last_plot(),filename="./Figures/STA56 outflow trends with stations.jpeg",width =16, height =9, units = "in")


#STA1E annual variability last 10 years 
ggplot(filter(STA1E_stations_tidy,Date_Time>as_datetime("2015-01-01 00:00:00")),aes(`Label Time`,Value*1000,fill=`Flow-way`,color=`Flow-way`))+
scale_x_datetime(date_labels = "%b" ,date_breaks = "1 month")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(TP~(ug/L)))+xlab("Month")+labs(title="TP over time STA-1E")+geom_smooth(se=T,span=.2,method="loess")+
geom_point(shape=21,size=2.5,color="black",alpha=.1)+coord_cartesian( ylim = c(0, 100))+
theme_bw(base_size = 20)

ggsave(plot = last_plot(),filename="./Figures/STA1E outflow trends.jpeg",width =16, height =9, units = "in")

#STA1W annual variability last 10 years 
ggplot(filter(STA1W_stations_tidy,Date_Time>as_datetime("2015-01-01 00:00:00"),`Station ID` !="G259"),aes(`Label Time`,Value*1000,fill=`Station ID`,color=`Station ID`))+
scale_x_datetime(date_labels = "%b" ,date_breaks = "1 month")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(TP~(ug/L)))+xlab("Month")+labs(title="TP over time STA-1W")+geom_smooth(se=T,span=.2,method="loess")+
geom_point(shape=21,size=2.5,color="black",alpha=.1)+coord_cartesian( ylim = c(0, 200))+
theme_bw(base_size = 20)

ggsave(plot = last_plot(),filename="./Figures/STA1W outflow trends.jpeg",width =16, height =9, units = "in")


#STA2 FW-2 variability last 10 years 
ggplot(filter(STA2_stations_tidy,Date_Time>as_datetime("2015-01-01 00:00:00"),`Flow-way`=="FW-2  G332"),aes(`Label Time`,Value*1000,fill=Year,color=Year))+
scale_x_datetime(date_labels = "%b" ,date_breaks = "1 month")+scale_y_continuous(breaks=pretty_breaks(n = 7))+
ylab(expression(TP~(ug/L)))+xlab("Month")+labs(title="TP over time STA-2")+geom_smooth(se=F,span=.6,method="loess")+
geom_point(shape=21,size=2.5,color="black",alpha=.5)+coord_cartesian( ylim = c(10, 180))+
theme_bw(base_size = 20)+guides(size = "legend", fill = "none")

ggsave(plot = last_plot(),filename="./Figures/STA2 FW2 outflow trends.jpeg",width =16, height =9, units = "in")



