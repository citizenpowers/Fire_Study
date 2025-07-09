#The goal of this script is to qualify deployed sonde data based on whether the sensor passed post-calibration and measurements are within sensor range.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(stringr)
library(lubridate)
library(tidyverse)



# Step 1.) Run helper function that assists in cleaning column headers -------------------------------------------

Clean_Column_Headers <-  function(df)
{
  df %>%
  rename(Site="Site Name") %>%  
  mutate(`Parameter`=case_when(Parameter=="ODO mg/L"~"DO (mg/L)",
                               Parameter=="Turbidity FNU"~"Turbidity (FNU)",
                               Parameter=="SpCond µS/cm"~"SpCond (µS/cm)",
                               Parameter=="Chlorophyll RFU"~"Chl (RFU)",
         .default= as.character(Parameter)))    %>%
    mutate(`Site`=case_when(Site=="BURN"~"Burn",
                                 Site=="HERBICIDE"~"Herbicide",
                                 Site=="UNTREATED"~"Untreated",
                                 Site=="BURN_HERBICIDE"~"Burn_Herb",
                                 .default= as.character(Site)))     
}


# Step 2.) Import Calibration limits and sensor range provided from manufacturer --------------------------------------------------
#Manufacturer Defined Parameter Ranges c(Lower,Upper)    
Temp_Range <-c(-5,50)
Cond_Range <-c(0,200000)
SpCond_Range <-c(0,200000)
ODO_mg_Range <-c(0,50) 
pH_Range <-c(0,14)
Turbidity_Range <-c(0,4000)
fDOM_RFU_Range <-c(0,100)

#Calibration ranges for bracketing
SpCond_2000 <-c(1900,2100)
SpCond_200 <-c(190,210)
pH_4 <-c(3.7,4.3)
pH_7 <-c(6.7,7.3)
pH_10 <-c(9.7,10.3)
Turbidity_124<-c(111.6,136.4)
Turbidity_0<-c(-2,2)
FDOM_RFU_0 <-c(0,2)
fDOM_RFU_100 <-c(90,110)
CHL_0 <- c(-2,2)

# Step 3.) Import Post-Cal Data -------------------------------------------


Post_Cal_Deployment_1 <- mutate(read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/Post-Cal Deployment 1.csv"),Deployment="Deployment 1")
Post_Cal_Deployment_2 <- mutate(read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/Post-Cal Deployment 2.csv"),Deployment="Deployment 2")
Post_Cal_Deployment_3 <- mutate(read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/Post_Cal_Deployement_3.csv"),Deployment="Deployment 3")


# Step 4.) Tidy Post Cal Data ------------------------------------------------------

#Checks to see if post-cal passed single calibration points
Post_Cal_Data <- bind_rows(Post_Cal_Deployment_1,Post_Cal_Deployment_2,Post_Cal_Deployment_3) %>%    
pivot_longer(names_to="Parameter",values_to="Value",cols=c("SpCond 200", "SpCond 2000","DO", "pH 7", "pH 10", "Turbidity 124","Turbidity 0","pH 4","Temp","CHL_0","CHL")) %>% 
mutate(Calibration=case_when(Parameter=="SpCond 2000" &  between(Value,SpCond_2000[1],SpCond_2000[2])   ~ "Passed SpCond 2000",
                               Parameter=="SpCond 2000" & !between(Value,SpCond_2000[1],SpCond_2000[2])  ~ "Failed SpCond 2000",
                               Parameter=="SpCond 200" & between(Value,SpCond_200[1],SpCond_200[2])~ "Passed SpCond 200",
                               Parameter=="SpCond 200" & !between(Value,SpCond_200[1],SpCond_200[2]) ~ "Failed SpCond 200",
                               Parameter=="pH 7" & between(Value,pH_7[1],pH_7[2]) ~ "Passed pH 7",
                               Parameter=="pH 7" & !between(Value,pH_7[1],pH_7[2]) ~ "Failed pH 7",
                               Parameter=="pH 10" & between(Value,pH_10[1],pH_10[2]) ~ "Passed pH 7",
                               Parameter=="pH 10" & !between(Value,pH_10[1],pH_10[2]) ~ "Failed pH 7",
                               Parameter=="pH 4" & between(Value,pH_4[1],pH_4[2]) ~ "Passed pH 4",
                               Parameter=="pH 4" & !between(Value,pH_4[1],pH_4[2]) ~ "Failed pH 4",
                               Parameter=="Turbidity 124" & between(Value,Turbidity_124[1],Turbidity_124[2]) ~ "Passed Turbidity 124",
                               Parameter=="Turbidity 124" & !between(Value,Turbidity_124[1],Turbidity_124[2]) ~ "FailedTurbidity 124",
                               Parameter=="Turbidity 0" & between(Value,Turbidity_0[1],Turbidity_0[2]) ~ "Passed Turbidity 0",
                               Parameter=="Turbidity 0" & !between(Value,Turbidity_0[1],Turbidity_0[2]) ~ "Failed Turbidity 0",
                               Parameter=="CHL_0" & between(Value,CHL_0[1],CHL_0[2]) ~ "Passed CHL 0",
                               Parameter=="CHL_0" & !between(Value,CHL_0[1],CHL_0[2]) ~ "Failed CHL 0",
                               .default = as.character("Missing Calibration Data"))) %>%
mutate(Calibration=ifelse(is.finite(Value) & is.finite(`DO T-val`) & Parameter=="DO",ifelse(abs(Value-`DO T-val`)<=0.3,"Passed DO","Failed DO") ,Calibration)) %>%       #check if DO is within 0.3 mg/l to True value based on temp
mutate(Calibration=ifelse(is.finite(Value) & is.finite(`CHL T-Val`) & Parameter=="CHL",ifelse(abs(Value-`CHL T-Val`)<=.2*`CHL T-Val`,"Passed CHL","Failed CHL") ,Calibration))        #check if CHL is within 0.3 mg/l to True value based on temp


#checks to see if post-cal was passed for the multiple calibrations points needed to bracket the data
Post_Cal_Summarized <- Post_Cal_Data %>%
  select(Deployment,Site,Sonde,Parameter,Calibration) %>%  
  pivot_wider(names_from = "Parameter", values_from = "Calibration")  %>%
  mutate(`DO (mg/L)`=case_when(str_detect(DO,"Passed")~"DO Passed Post-Calibration",                            
                               str_detect(DO,"Failed")~"DO Failed Post-Calibration",
                               str_detect(DO,"Missing")~"DO Missing Post-Calibration Data",
                               .default = as.character("DO Missing Post-Calibration Data"))) %>%
  mutate(`SpCond (µS/cm)`=case_when(str_detect(`SpCond 2000`,"Passed") &  str_detect(`SpCond 200`,"Passed") ~"SpCond Passed Post-Calibration",
                                    str_detect(`SpCond 2000`,"Failed") |  str_detect(`SpCond 200`,"Failed")~"SpCond Failed Post-Calibration",
                                    str_detect(`SpCond 2000`,"Missing") |  str_detect(`SpCond 200`,"Missing")~"SpCond Missing Post-Calibration Data",
                                    .default = as.character("SpCond Missing Post-Calibration Data"))) %>%
  mutate(`pH`=case_when(str_detect(`pH 7`,"Passed") &  str_detect(`pH 10`,"Passed") ~"pH Passed Post-Calibration",
                        str_detect(`pH 7`,"Failed") |  str_detect(`pH 10`,"Failed")~"pH Failed Post-Calibration",
                        str_detect(`pH 7`,"Missing") |  str_detect(`pH 10`,"Missing")~"pH Missing Post-Calibration Data",
                        .default = as.character("SpCond Missing Post-Calibration Data"))) %>%
  mutate(`Turbidity (FNU)`=case_when(str_detect(`Turbidity 124`,"Passed") & str_detect(`Turbidity 0`,"Passed")~"Turbidity Passed Post-Calibration",                              
                                     str_detect(`Turbidity 124`,"Failed") | str_detect(`Turbidity 0`,"Failed")~"Turbidity Failed Post-Calibration",
                                     str_detect(`Turbidity 124`,"Missing") |str_detect(`Turbidity 0`,"Missing") ~"Turbidity Missing Post-Calibration Data",
                                     .default = as.character("Turbidity Missing Post-Calibration Data"))) %>%
  mutate(`Chl (RFU)`=case_when(str_detect(`CHL`,"Passed") & str_detect(`CHL_0`,"Passed")~"Chl Passed Post-Calibration",                              
                                     str_detect(`CHL`,"Failed") | str_detect(`CHL_0`,"Failed")~"Chl Failed Post-Calibration",
                                     str_detect(`CHL`,"Missing") |str_detect(`CHL_0`,"Missing") ~"Chl Missing Post-Calibration Data",
                                     .default = as.character("Turbidity Missing Post-Calibration Data"))) %>%
pivot_longer(names_to = "Parameter",values_to ="Post-Calibration",cols = c("DO (mg/L)", "SpCond (µS/cm)","pH","Turbidity (FNU)","Chl (RFU)"))  %>%
select(Deployment,Site,Parameter,`Post-Calibration`)



# Step 5.) Import and join deployed data to post-cal data ----------------------------------------------------------------
Sonde_Long_Tidy <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Sonde/Sonde_Long_Tidy.csv")

Deployment_Data_Post_cal <- Clean_Column_Headers(Sonde_Long_Tidy)  %>%
left_join(Post_Cal_Summarized,by=c("Deployment","Site","Parameter")) %>%
mutate(`Post-Calibration`=if_else(Parameter=="Temp °C","Factory Calibrated",`Post-Calibration`))



# Step 6.) Checks to see if deployed data is within manufacturers range ------------

Deployment_Data_Post_cal_manufacurer_check <- Deployment_Data_Post_cal %>%
mutate(`Manufacturer Range`=case_when(Parameter=="Temp (C°)" & between(Value,Temp_Range[1],Temp_Range[2]) ==FALSE~ "Value outside sensor range.",
                                        Parameter=="DO (mg/L)" & between(Value,ODO_mg_Range[1],ODO_mg_Range[2]) ==FALSE~ "Value outside sensor range.",  
                                        Parameter=="SpCond (µS/cm)" & between(Value,SpCond_Range[1],SpCond_Range[2]) ==FALSE~ "Value outside sensor range.",
                                        Parameter=="pH" &  between(Value,pH_Range[1],pH_Range[2])  ==FALSE~ "Value outside sensor range.",
                                        Parameter=="Turbidity (FNU)" & between(Value,Turbidity_Range[1],Turbidity_Range[2]) ==FALSE~ "Value outside sensor range.",
                                        .default = as.character(""))) 



# Step 7.) Add qualifier code and remark notes with reason for qualification -------


Deployment_Data_Qualified  <- Deployment_Data_Post_cal_manufacurer_check %>%
mutate(`Remark Code`="",`Remark Note`="") %>%
mutate(`Remark Code`=case_when(`Manufacturer Range`=="Value outside sensor range."~"?",
                                 str_detect(`Post-Calibration`,"Failed") | str_detect(`Post-Calibration`,"Missing") ~"J",
                                 `Parameter` =="Turbidity (FNU)" ~"H",
                                 .default = `Remark Code`)) %>%

mutate(`Remark Note`=trimws(paste(`Post-Calibration`,". ",`Manufacturer Range`))) %>%
select(-`Post-Calibration`,-`Manufacturer Range`,-Date)  

write.csv(Deployment_Data_Qualified,"./Data/Sonde/Sonde Data Qualified.csv",row.names=FALSE)  #Save qualified data
