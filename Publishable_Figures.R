# Object of this script is to create publishable figures



library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)
library(cowplot)
library(magick)
library(grid)
library(gridExtra)

 # Import Data -------------------------------------------------------------

LIMSP_Provisional_Data_Tidy_letters  <- read_csv("./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy_letters.csv")


# Printed Figures ---------------------------------------------------------

costom_theme <-theme(legend.position="none",,axis.title.y=element_blank())


#Grab samples OPO4
TP_boxplot <- ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",,TEST_NAME=="TPO4",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)  +
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="grey80")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="grey95")+  
geom_boxplot(aes(fill=`Treatment`))+ annotate(geom = "text", x = "-72", y = 1, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 1, label = "Post-burn") +
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .85),size=3.5)+
ggtitle("TPO4")+xlab("Days from Burn")+scale_y_continuous(breaks= seq(0, 600,100))+theme_bw()+costom_theme

#Grab samples OPO4
SRP_boxplot <-ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",,TEST_NAME=="SRP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)  +
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="grey80")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="grey95")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 1, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 1, label = "Post-burn") +
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment),, position = position_dodge(width = .85),size=3.5)+
ggtitle("SRP")+geom_boxplot(aes(fill=`Treatment`))+theme_bw()+costom_theme

#Grab samples DOP
DOP_boxplot <-ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",TEST_NAME=="DOP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)  +
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="grey80")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="grey95")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 1, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 1, label = "Post-burn") +
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .85),size=3.5)+
ggtitle("DOP")+geom_boxplot(aes(fill=`Treatment`))+theme_bw()+costom_theme

#Grab samples TN
TN_boxplot <-ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",TEST_NAME=="TN",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)  +
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="grey80")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="grey95")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 1, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 1, label = "Post-burn") +
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .85),size=3.5)+
ggtitle("TN")+geom_boxplot(aes(fill=`Treatment`))+theme_bw()+costom_theme

#Grab samples NH4
NH4_boxplot <-ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",TEST_NAME=="NH4",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)+
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="grey80")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="grey95")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 1, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 1, label = "Post-burn") +
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .85),size=3.5)+
ggtitle("NH4")+geom_boxplot(aes(fill=`Treatment`))+theme_bw()+costom_theme

#Grab samples NOX
NOX_boxplot <- ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",TEST_NAME=="NOX",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)+
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),ymin=-Inf,ymax=Inf),fill="grey80")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),ymin=-Inf,ymax=Inf),fill="grey95")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 3, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 3, label = "Post-burn") +
geom_text(aes(as.factor(`Burn Day`),max(VALUE)*1100,label=Letter,group=Treatment), position = position_dodge(width = .85),size=3.5)+
ggtitle("NOx")+geom_boxplot(aes(fill=`Treatment`))+theme_bw()+costom_theme


#create separate legend to be used in cowplot 
legend_plot <- ggplot(filter(LIMSP_Provisional_Data_Tidy_letters,COLLECT_METHOD=="GP",TEST_NAME=="NOX",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE*1000))+
scale_fill_grey(start = 0, end = .9)+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()+theme(legend.position="bottom")

ggsave(plot = last_plot(),filename="./Figures/legend_plot.jpeg",width =8, height =8, units = "in")


#combine plots into figure
#Plot elements 
Legend <- ggplot()+ theme_void()+draw_image(image_read("./Figures/legend_plot_only.png"))
y.grob <- textGrob(expression((mu~g/L)),gp=gpar(fontsize=14), rot=90)

Nutrient_plot_top <- plot_grid(TP_boxplot, TN_boxplot, SRP_boxplot,NH4_boxplot,DOP_boxplot,NOX_boxplot ,labels = "AUTO", ncol = 2,align = "hv",axis = "rlbt")
Nutrient_plot_top1 <- grid.arrange(arrangeGrob(Nutrient_plot_top, left = y.grob))


Nutrient_plot <- plot_grid(Nutrient_plot_top1 , Legend , labels = "", rel_heights = c(10, 1),rel_widths =c(1, 2),ncol=1)

ggsave(plot = Nutrient_plot ,filename="./Figures/Nutrient_plot.jpeg",width =12, height =14, units = "in")




# Online figures --------------------------------------------------------------

#Fig 2 Online Version- Grab samples OPO4 only boxplots
ggplot(filter(LIMSP_Provisional_Data_Tidy,TEST_NAME=="OPO4",COLLECT_METHOD=="GP",COLLECT_DATE>"2025-01-01 00:00:00",MATRIX=="SW"),aes(as.factor(`Burn Day`) ,VALUE))+
scale_fill_manual(values = c("#e31a1c","#ff7f00","#cab2d6","#33a02c"))  +
geom_rect(inherit.aes=F,aes(xmin =stage("0", after_scale = xmin-0.5) ,xmax=stage("11", after_scale = xmax+0.6),,ymin=-Inf,ymax=Inf),fill="#fbb4ae")+  
geom_rect(inherit.aes=F,aes(xmin =stage("-72", after_scale = xmin-0.6) ,xmax=stage("-16", after_scale = xmax+0.5),,ymin=-Inf,ymax=Inf),fill="#ccebc5")+  
xlab("Days from Burn")+annotate(geom = "text", x = "-72", y = 0.074, label = "Pre-burn") +annotate(geom = "text", x = "0", y = 0.074, label = "Post-burn") +
ylab(expression(OPO4~(mg/L)))+
geom_boxplot(aes(fill=`Treatment`))+theme_bw()
