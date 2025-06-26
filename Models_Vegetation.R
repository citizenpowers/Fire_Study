# Goal of this script is to use Before-After-Control-Impact models to determine the effect of fire on vegetation

library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
#library(predict3d)
#library(plyr)
library(car)
library(lme4)
library(sjPlot)
library(ggsci)


# Custom Theme ------------------------------------------------------------

fire_theme <-list(scale_fill_frontiers(), scale_color_frontiers(),theme_bw())


# Import Data -------------------------------------------------------------



Veg_and_biomass_tidys <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Vegetation/Veg_and_biomass_tidys.csv")



# Tidy Data for model -----------------------------------------------------

Veg_and_biomass_tidys_mod <- Veg_and_biomass_tidys %>%
filter(Species %in% c("Dead Cattail and Litter","Live Cattail")) %>%
mutate(Species=as.factor(Species),Treatment=as.factor(Treatment),Phase=as.factor(Phase),
Burned=factor(if_else(str_detect(Treatment,"Burn") & Date > "2025-02-26","Burned","Not Burned"),c("Not Burned","Burned")),
Herbicide=factor(if_else(str_detect(Treatment,"Herb"),"Herbicide","No Herbicide"),c("No Herbicide","Herbicide")))


#total biomass 
Biomass_total_tidys_mod <- Veg_and_biomass_tidys %>%
filter(Species %in% c("Dead Cattail and Litter","Live Cattail")) %>%
mutate(Species=as.factor(Species),Treatment=as.factor(Treatment),Phase=as.factor(Phase),
Burned=factor(if_else(str_detect(Treatment,"Burn") & Date > "2025-02-26","Burned","Not Burned"),c("Not Burned","Burned")),
Herbicide=factor(if_else(str_detect(Treatment,"Herb"),"Herbicide","No Herbicide"),c("No Herbicide","Herbicide"))) %>%
group_by(Phase,Rep,Site,Burned,Herbicide) %>%
summarise(n(),`Total Weight`=sum(Weight,na.rm=T))  

Biomass_total_summary <- Biomass_total_tidys_mod %>%
group_by(Burned,Herbicide,Site) %>%
summarise(n(),`Mean Total`=mean(`Total Weight`),SD=sd(`Total Weight`),SE=SD/sqrt(n()))  


# Distribution of data -------------------------------------------------------

#Distribution of cover appears bimodal
ggplot(Veg_and_biomass_tidys_mod,aes(Cover))+geom_histogram()+theme_bw()

#When faceting by veg type it there are two distinct distributions 
ggplot(Veg_and_biomass_tidys_mod,aes(Cover))+geom_histogram(binwidth=10)+theme_bw()+facet_wrap(~Species)

#Biomass also appears to have bimodal distribution
ggplot(Veg_and_biomass_tidys_mod,aes(Weight))+geom_histogram()+theme_bw()

#When faceting by veg type it there are two distinct distributions 
ggplot(Veg_and_biomass_tidys_mod,aes(Weight))+geom_histogram(binwidth=200)+theme_bw()+facet_wrap(~Species)


# Models for total biomass using Burn and Herbicide as dependent variable ********--------

#LM without interaction
total_weight_mod_lm_burn_herb <- lm(`Total Weight` ~ `Burned`+Herbicide, data = Biomass_total_tidys_mod)
summary(total_weight_mod_lm_burn_herb )
Anova(total_weight_mod_lm_burn_herb )

#LM Interaction
total_weight_mod_lm_burn_herb_int <- lm(`Total Weight` ~ `Burned`+Herbicide+Burned*Herbicide, data = Biomass_total_tidys_mod)
summary(total_weight_mod_lm_burn_herb_int)
Anova(total_weight_mod_lm_burn_herb_int)

#mixed model RI
Total_weight_mod_ri  <- lmer(`Total Weight` ~ `Burned`+Herbicide+(1|Site) , data = Biomass_total_tidys_mod)
summary(Total_weight_mod_ri)
Anova(Total_weight_mod_ri)

#mixed model RI and RS
Total_weight_mod_ri_rs  <- lmer(`Total Weight` ~ `Burned`+Herbicide+(Site|Burned) , data = Biomass_total_tidys_mod)
summary(Total_weight_mod_ri_rs)
Anova(Total_weight_mod_ri_rs)

#mixed model RI and RS GAMMMA error distribution
Total_weight_mod_GLM_ri_rs  <- glmer(`Total Weight` ~ `Burned`+Herbicide+(Site|Burned) , data = Biomass_total_tidys_mod,family = "Gamma"(link='log'))
summary(Total_weight_mod_GLM_ri_rs)
Anova(Total_weight_mod_GLM_ri_rs)

#add predictions to dataframe
Model_predictions_total_weight <- Biomass_total_tidys_mod %>%
ungroup() %>%  
mutate(Predict_total_total_weight_mod_lm_burn_herb  = predict(total_weight_mod_lm_burn_herb , newdata = Biomass_total_tidys_mod)) %>%
mutate(Predict_total_weight_mod_lm_burn_herb_int = predict(total_weight_mod_lm_burn_herb_int, newdata = Biomass_total_tidys_mod)) %>%
mutate(Predict_total_weight_mod_ri  = predict(Total_weight_mod_ri , newdata = Biomass_total_tidys_mod)) %>%
mutate(Predict_total_weight_mod_ri_rs = predict(Total_weight_mod_ri_rs, newdata = Biomass_total_tidys_mod)) %>%  
mutate(Predict_Total_weight_mod_GLM_ri_rs   = predict(Total_weight_mod_GLM_ri_rs  , newdata = Biomass_total_tidys_mod,type="response")) %>%  
  
pivot_longer(names_to="Model",values_to = "Prediction",8:12) %>%
mutate(`Residuals`=`Total Weight`-Prediction)

#Visualize model predictions
ggplot(Model_predictions_total_weight,aes(Burned,Prediction,color=Model,fill = Model))+geom_boxplot(aes(Burned,`Total Weight`),color="grey50",fill="white")+geom_jitter(size=2,height=0)+#geom_point(aes(Phase,Cover),color="grey50")+
facet_grid(~Herbicide)+fire_theme

#Visualize model predictions
ggplot(Model_predictions_total_weight,aes(Herbicide,Prediction,color=Model,fill = Model))+geom_boxplot(aes(Herbicide,`Total Weight`),color="grey50",fill="white")+geom_jitter(size=2,height=0)+#geom_point(aes(Phase,Cover),color="grey50")+
facet_grid(~Burned)+fire_theme

#Visualize model predictions by site
ggplot(Model_predictions_total_weight,aes(Burned,Prediction,color=Model,fill = Model))+geom_boxplot(aes(Burned,`Total Weight`),color="grey50",fill="white")+geom_jitter(size=2,height=0)+#geom_point(aes(Phase,Cover),color="grey50")+
facet_grid(~Site)+fire_theme

#Residuals vs predictions
ggplot(Model_predictions_total_weight,aes(Prediction,Residuals,color=Model,shape=Burned))+geom_point()+geom_smooth()+fire_theme  

#Residuals vs measurements
ggplot(Model_predictions_total_weight,aes(`Total Weight`,Residuals,color=Model,shape=Burned))+geom_point()+geom_smooth()+fire_theme  

# Calculate summary stats for the different models
Model_evaluation <- Model_predictions_total_weight %>%
group_by(Model) %>%
summarise(n(),RMSE=sqrt((sum(`Residuals`^2,na.rm=T)/n())),SD=sd(Residuals),`Mean Residual`=mean(abs(round(Residuals,3)),na.rm=T))

#HTML format of model summary
tab_model(Total_weight_mod_ri_rs)




#Models for percent cover using Burn and Herbicide as dependent variables --------


Cover_mod_lm_species_burn_herb <- lm(Cover ~ Species+`Burned`+Herbicide, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_species_burn_herb)
Anova(Cover_mod_lm_species_burn_herb)

Cover_mod_lm_species_burn_herb_int <- lm(Cover ~ Species+`Burned`+Herbicide+Burned*Herbicide, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_species_burn_herb_int)
Anova(Cover_mod_lm_species_burn_herb)

Model_predictions_cover<- Veg_and_biomass_tidys_mod %>%
mutate(Predict_Cover_mod_lm_species_burn_herb = predict(Cover_mod_lm_species_burn_herb, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Cover_mod_lm_species_burn_herb_int = predict(Cover_mod_lm_species_burn_herb_int, newdata = Veg_and_biomass_tidys_mod)) %>%
pivot_longer(names_to="Model",values_to = "Prediction",11:12) %>%
mutate(`Residuals`=Cover-Prediction)

#Visualize model predictions
ggplot(Model_predictions_cover,aes(Burned,Prediction,color=Model,fill = Model))+geom_boxplot(aes(Burned,Cover),color="grey50",fill="white")+geom_jitter(size=2,height=0)+#geom_point(aes(Phase,Cover),color="grey50")+
facet_grid(Species~Herbicide)+fire_theme

#Residuals vs predictions
ggplot(Model_predictions_cover,aes(Prediction,Residuals,color=Model,shape=Burned))+geom_point()+facet_grid(Species~Herbicide)+fire_theme  

#Residuals vs cover(%)
ggplot(Model_predictions_cover,aes(Cover,Residuals,color=Model,shape=Burned))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(Species~Herbicide)+fire_theme  


# Models for weight using Burn and Herbicide as dependent variables -------

Weight_mod_lm_species_burn_herb <- lm(Weight ~ Species+`Burned`+Herbicide, data = Veg_and_biomass_tidys_mod)
summary(Weight_mod_lm_species_burn_herb)
Anova(Weight_mod_lm_species_burn_herb)

Weight_mod_lm_species_burn_herb_int <- lm(Weight ~ Species+`Burned`+Herbicide+Burned*Herbicide, data = Veg_and_biomass_tidys_mod)
summary(Weight_mod_lm_species_burn_herb_int)
Anova(Weight_mod_lm_species_burn_herb_int)

Model_predictions_Weight<- Veg_and_biomass_tidys_mod %>%
mutate(Predict_Weight_mod_lm_species_burn_herb = predict(Weight_mod_lm_species_burn_herb, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Weight_mod_lm_species_burn_herb_int = predict(Weight_mod_lm_species_burn_herb_int, newdata = Veg_and_biomass_tidys_mod)) %>%
pivot_longer(names_to="Model",values_to = "Prediction",11:12) %>%
mutate(`Residuals`=Weight-Prediction) 

#Visualize model predictions
ggplot(Model_predictions_Weight,aes(Burned,Prediction,color=Model,fill = Model))+geom_boxplot(aes(Burned,Weight),color="grey50",fill="white")+geom_jitter(size=2,height=0)+#geom_point(aes(Phase,Cover),color="grey50")+
facet_grid(Species~Herbicide)+fire_theme

#Residuals vs predictions
ggplot(Model_predictions_Weight,aes(Prediction,Residuals,color=Model,shape=Burned))+geom_point()+facet_grid(Species~Herbicide)+fire_theme  

#Residuals vs cover(%)
ggplot(Model_predictions_Weight,aes(Weight,Residuals,color=Model,shape=Burned))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(Species~Herbicide)+fire_theme  






#  Models for percent cover using treatment as covariate-------------------------------------------------

#LM with with only phase 
Cover_mod_lm_phase <- lm(Cover ~ `Phase`, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_phase)
Anova(Cover_mod_lm_phase)

Cover_mod_lm_treatment_species <- lm(Cover ~ Treatment+Species, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_treatment_species)
Anova(Cover_mod_lm_treatment_species)

#LM with with only phase and species
Cover_mod_lm_phase_species <- lm(Cover ~ `Phase`+Species, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_phase_species)
Anova(Cover_mod_lm_phase_species)

#LM with with all variables used as predictors
Cover_mod_lm_phase_species_treatment <- lm(Cover ~ `Phase`+Species+Treatment, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_phase_species_treatment)
Anova(Cover_mod_lm_phase_species_treatment)

#LM with treatment and interaction
Cover_mod_lm_phase_species_treatment_int <- lm(Cover ~ `Phase`+Species+Treatment+Phase*Treatment, data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_lm_phase_species_treatment_int)
Anova(Cover_mod_lm_phase_species_treatment_int)

#Mixed model with random effects (slope and intercept) by site 
Cover_mod_ri_rs  <- lmer(Cover ~ `Phase`+Species+Treatment+(Phase|Site) , data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_ri_rs)
Anova(Cover_mod_ri_rs)

#Mixed model with random effects (intercept only) by site 
Cover_mod_ri  <- lmer(Cover ~ `Phase`+Species+Treatment+(1|Site) , data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_ri)
Anova(Cover_mod_ri)

#GLM with GAMMA distribution
Veg_and_biomass_tidys_mod_glm <- Veg_and_biomass_tidys_mod %>% mutate(Cover=if_else(Cover==0,1,Cover))  #gamma distribution cannot have 0s in it. 

Cover_mod_glm_gamma <- glm(Cover ~ `Phase`+Species+Treatment, data = Veg_and_biomass_tidys_mod_glm,family = "Gamma"(link='log'))
summary(Cover_mod_glm_gamma)
Anova(Cover_mod_glm_gamma)

#GLM with POISON distribution
Veg_and_biomass_tidys_mod_glm <- Veg_and_biomass_tidys_mod %>% mutate(Cover=if_else(Cover==0,1,Cover))  #gamma distribution cannot have 0s in it. 

Cover_mod_glm_gamma <- glm(Cover ~ `Phase`+Species+Treatment, data = Veg_and_biomass_tidys_mod_glm,family = "Gamma"(link='log'))
summary(Cover_mod_glm_gamma)
Anova(Cover_mod_glm_gamma)

#GLM with POISSON distribution
Cover_mod_glm_POISSON <- glm(Cover ~ `Phase`+Species+Treatment, data = Veg_and_biomass_tidys_mod,family = poisson)
summary(Cover_mod_glm_POISSON)
Anova(Cover_mod_glm_POISSON)


#make predictions- 
Model_predictions<- Veg_and_biomass_tidys_mod %>%
mutate(Predict_Cover_mod_lm_phase = predict(Cover_mod_lm_phase, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Cover_mod_lm_phase_species = predict(Cover_mod_lm_phase_species, newdata = Veg_and_biomass_tidys_mod)) %>% 
mutate(Predict_Cover_mod_lm_treatment_species = predict(Cover_mod_lm_treatment_species, newdata = Veg_and_biomass_tidys_mod)) %>% 
mutate(Predict_Cover_mod_lm_phase_species_treatment = predict(Cover_mod_lm_phase_species_treatment, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Cover_mod_lm_phase_species_treatment_int = predict(Cover_mod_lm_phase_species_treatment_int, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Cover_mod_ri_rs = predict(Cover_mod_ri_rs, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Cover_mod_ri = predict(Cover_mod_ri, newdata = Veg_and_biomass_tidys_mod)) %>%
mutate(Predict_Cover_mod_glm_gamma = predict(Cover_mod_glm_gamma, newdata = Veg_and_biomass_tidys_mod_glm,type = "response")) %>%
mutate(Predict_Cover_mod_glm_POISSON = predict(Cover_mod_glm_POISSON, newdata = Veg_and_biomass_tidys_mod,type = "response")) %>%  
pivot_longer(names_to="Model",values_to = "Prediction",9:17) %>%
mutate(`Residuals`=Cover-Prediction) %>%
mutate(Phase=factor(Phase,c("PreBurn","PostBurn")))
  

#Visualize model predictions
ggplot(Model_predictions,aes(Phase,Prediction,color=Model,fill = Model))+geom_boxplot(aes(Phase,Cover),color="grey50",fill="white")+geom_jitter(size=2,height=0)+#geom_point(aes(Phase,Cover),color="grey50")+
facet_grid(Species~Treatment)+fire_theme


#Residuals vs predictions
ggplot(Model_predictions,aes(Prediction,Residuals,color=Model))+geom_point()+facet_grid(Species~Treatment)+fire_theme  

#Residuals vs cover(%)
ggplot(Model_predictions,aes(Cover,Residuals,color=Model))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(Species~Treatment)+fire_theme  

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(Cover_mod_lm)

#visualization from https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
plot_model(Cover_mod_lm, type = "pred", terms = c("Species","Phase","Treatment"))+theme_bw()
plot_model(Cover_mod_lm_int, type = "int",terms = c("Species","Phase","Treatment"))+theme_bw()

# General linear model for percent cover ----------------------------------

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(Cover_mod_glm)

#visualization from https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
plot_model(Cover_mod_glm, type = "pred", terms = c("Species","Phase","Treatment"))+theme_bw()
plot_model(Cover_mod_glm, type = "int",terms = c("Species","Phase","Treatment"))+theme_bw()


# Mixed model with random slope and intercept -----------------------------

Cover_mod_ri_rs  <- lmer(Cover ~ `Phase`+Species+Treatment+(Phase|Site) , data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_ri_rs)
Cover_mod_ri  <- lmer(Cover ~ `Phase`+Species+Treatment+(1|Site) , data = Veg_and_biomass_tidys_mod)
summary(Cover_mod_ri)




#make predictions
Model_predictions<- Veg_and_biomass_tidys_mod %>%
mutate(Predict_Cover_mod_re = predict(Cover_mod_re , newdata = Veg_and_biomass_tidys_mod))

#Visualize model
ggplot(Model_predictions,aes(Phase,Cover))+geom_point()+geom_jitter(aes(Phase,Predict_Cover_mod_re),color="red",size=2,height=0)+
facet_grid(Species~Site)+theme_bw()

par(mfrow=c(2,2)) 
plot(Cover_mod_re )

plot_model(Cover_mod_re , type = "pred", terms = c("Species","Phase","Treatment"))+theme_bw()

# Model Comparison --------------------------------------------------------

anova(Cover_mod_lm, Cover_mod_re)



# Biomass vs Cover -----------------------------------------------------------------


ggplot(Veg_and_biomass_tidys,aes(Cover,Weight,color=Species))+geom_point()+
facet_wrap(~Treatment)+geom_smooth(method="lm")+theme_bw()
