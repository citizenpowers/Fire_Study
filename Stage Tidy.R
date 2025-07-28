#objective of this script is to tidy stage data




library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)




# import data -------------------------------------------------------------



Stage_G381_G384_instant <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Depth/Stage_G381_G384.csv",  skip = 4)


# tidy data ---------------------------------------------------------------



STA34_depth_tidy <- Stage_G381_G384_instant  %>%
rename(Station="G384C_T",Date="VV482",Stage="ft NAVD88") %>%  
mutate(`Gauge`=if_else(str_detect(Station,"G381"),"STA-3/4 Outflow","STA-3/4 Midflow")) %>%
mutate(`Date Time`=dmy_hms(Date))  %>%
mutate(Date=as.Date(`Date Time`)) %>%
select(Date,Station,`Gauge`,`Date Time`,Stage)  %>%
mutate(`Depth to Consolidated Substrate (calculated)`=(Stage-8.3)*30.48)   #Average ground elevation in STA3/4 Cell 3B is 8.3 ft NAVD88. convert to cm

write_csv(STA34_depth_tidy  ,"./Data/Depth/STA34_depth_tidy.csv")

STA34_depth_tidy_DA <- STA34_depth_tidy  %>%
group_by(Date, Gauge) %>%
summarise(`Depth to Consolidated Substrate (calculated)`=mean(`Depth to Consolidated Substrate (calculated)`,na.rm=T))

write_csv(STA34_depth_tidy_DA  ,"./Data/Depth/STA34_depth_tidy_DA.csv")

