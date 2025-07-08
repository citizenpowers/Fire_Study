#objective of this script is to tidy flow data








# import data -------------------------------------------------------------



STA34_G380_G81_DA <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Flow/STA34_G380_G81_DA.csv", 
                              skip = 14)

# tidy data ---------------------------------------------------------------



STA34_flow_DA_tidy <- STA34_G380_G81_DA  %>%
mutate(`Structure Location`=if_else(str_detect(Station,"G381"),"STA-3/4 Outflow","STA-3/4 Inflow")) %>%
mutate(Date=as.Date(dmy(`Daily Date`))) %>%
group_by(Date,`Structure Location`) %>%
summarise(n(),Flow=sum(`Data Value`,na.rm=T))  


write_csv(STA34_flow_DA_tidy ,"./Data/Flow/STA34_flow_DA_tidy.csv")
