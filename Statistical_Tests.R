#The object of this script is to calculate statistical significance for Fire study data

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
#library(viridis)
#library(Hmisc)
#library(ggpmisc)
#library(ggrepel)
library(zoo)
library(multcompView)
#library(broom)





# Import Data -------------------------------------------------------------


LIMSP_Provisional_Data_Tidy <-read_csv("./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy.csv")


#Test treatments against each other for each day sampled with ANOVA and adding stat sig letters   ----------------------------------------------

#Run ANOVA and Tukey's HSD on analyte concentrations.  
LIMSP_Provisional_Data_letters <- LIMSP_Provisional_Data_Tidy %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP") %>%  
nest_by(`Burn Day`,TEST_NAME) %>% 
mutate(Model = list(lm(VALUE ~ Treatment, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=list(multcompLetters4(Model, Tukey_HSD)$Treatment$Letters)) %>% #Label ecotopes that are not sig different with same letter
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Treatment",values_to="Letter",7:10) %>%
select(`Burn Day`,TEST_NAME,Treatment,Letter)   	


#Add statistical significance to dataset 
LIMSP_Provisional_Data_Tidy_letters <- LIMSP_Provisional_Data_Tidy %>%
left_join(LIMSP_Provisional_Data_letters,by=c("Burn Day", "TEST_NAME","Treatment")) %>%
mutate(`Burn Day`=as.factor(`Burn Day`))

write_csv(LIMSP_Provisional_Data_letters ,"./Data/Water Quality Data/LIMSP_Provisional_Data_letters.csv")
write_csv(LIMSP_Provisional_Data_Tidy_letters ,"./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy_letters.csv")



# Log transformed data ----------------------------------------------------
#log Transform values before running tests

#Run ANOVA and Tukey's HSD on analyte concentrations.  
LIMSP_Provisional_Data_letters_log <- LIMSP_Provisional_Data_Tidy %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP") %>%  
mutate(VALUE=log(VALUE)) %>%  
nest_by(`Burn Day`,TEST_NAME) %>% 
mutate(Model = list(lm(VALUE ~ Treatment, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=list(multcompLetters4(Model, Tukey_HSD)$Treatment$Letters)) %>% #Label ecotopes that are not sig different with same letter
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Treatment",values_to="Letter log",7:10) %>%
mutate(`Burn Day`=as.factor(`Burn Day`)) %>% 
select(`Burn Day`,TEST_NAME,Treatment,`Letter log`)   	


#Add statistical significance to dataset 
LIMSP_Provisional_Data_Tidy_letters_log <- LIMSP_Provisional_Data_Tidy_letters %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP") %>%    
left_join(LIMSP_Provisional_Data_letters_log,by=c("Burn Day", "TEST_NAME","Treatment")) %>%
mutate(`Burn Day`=as.factor(`Burn Day`))

#join stat sig performed on log transformed data to untransformed stats

LIMSP_Provisional_Data_Tidy_letters_log <- LIMSP_Provisional_Data_Tidy_letters%>%
left_join(LIMSP_Provisional_Data_letters_log,by=c("Burn Day", "TEST_NAME","Treatment")) %>%
mutate(`Burn Day`=as.factor(`Burn Day`))

write_csv(LIMSP_Provisional_Data_Tidy_letters_log ,"./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy_letters_log.csv")



# Statistical tests on the difference from the daily average --------------

#Find the daily average by test name
Daily_average_summary <- LIMSP_Provisional_Data_Tidy %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP") %>%    
group_by(`Burn Day`,TEST_NAME) %>%
summarise(Daily_Average=as.numeric(format(mean(VALUE,na.rm = T),scientific=F,digits=4)))


#Run ANOVA and Tukey's HSD on analyte concentrations.  
LIMSP_Provisional_Data__diff_letters <- LIMSP_Provisional_Data_Tidy %>%
filter(MATRIX=="SW",COLLECT_METHOD=="GP") %>% 
left_join(Daily_average_summary,by=c("Burn Day","TEST_NAME"))  %>%
mutate(Diff=VALUE-Daily_Average) %>%                                      #calculate difference from the daily average
nest_by(`Burn Day`,TEST_NAME) %>% 
mutate(Model = list(lm(Diff ~ Treatment, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=ifelse(anyNA(Tukey_HSD$Treatment)==FALSE,list(multcompLetters4(Model, Tukey_HSD)$Treatment$Letters),list(c(Untreated = "a", Burn_Herb = "a", Burn = "a",Herbicide="a")))) %>% #Label treatments that are not sig different with same letter. NOX on day -114 had no differences and throws error. manually coded as no stat dif
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Treatment",values_to="Letter",7:10) %>%
select(`Burn Day`,TEST_NAME,Treatment,Letter)   	

#Add statistical significance to dataset 
LIMSP_Provisional_Data_Tidy_diff_letters <- LIMSP_Provisional_Data_Tidy %>%
left_join(LIMSP_Provisional_Data__diff_letters,by=c("Burn Day", "TEST_NAME","Treatment")) %>%
mutate(`Burn Day`=as.factor(`Burn Day`))

