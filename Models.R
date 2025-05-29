# Goal of this script is to use Before-After-Control-Impact models to determine duration of fire effects

.libPaths()

library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(predict3d)
library(plyr)
library(car)
library(lme4)


# Import Data -------------------------------------------------------------



LIMSP_Provisional_Data_Tidy_letters_log  <- read_csv("./Data/Water Quality Data/LIMSP_Provisional_Data_Tidy_letters_log.csv") #log tranformed before stat test




# Tidy Data for models ----------------------------------------------------

Model_data_SRP <- LIMSP_Provisional_Data_Tidy_letters_log %>%
filter(COLLECT_DATE<"2025-05-01 00:00:00") %>% 
mutate(Phase=ifelse(COLLECT_DATE<"2025-04-10 00:00:00","Pre-burn","Post-Burn")) %>%
filter( TEST_NAME=="SRP",Phase=="Post-Burn") 

# Linear Model ------------------------------------------------------------

linear_mod_SRP <- lm(VALUE ~ `Burn Day` +Treatment, data = Model_data_SRP)
summary(linear_mod_SRP)

par(mfrow=c(2,2))
plot(linear_mod_SRP)


# General linear model fixed effects----------------------------------------------------

glm_mod_SRP <- glm(VALUE ~ `Burn Day` +Treatment, data = Model_data_SRP,family = "Gamma"(link='log'))

summary(glm_mod_SRP)

plot(glm_mod_SRP)

exp(coef(glm_mod_SRP)[1])   #intercept. exponentiation to transform back from log scaled values

exp(coef(glm_mod_SRP)[2])  #slope. exponentiation to transform back from log scaled values

Model_data_SRP.predict <- cbind(Model_data_SRP, rename(data.frame(predict(glm_mod_SRP,type = "response")),"Prediction"=1))

ggplot(Model_data_SRP.predict,aes(`Burn Day`,VALUE,color=Treatment))+geom_point()+theme_bw()+
geom_line(aes(`Burn Day`,Prediction,color=Treatment),size=2)  


# General linear model fixed and random intercept----------------------------------------------------

mixed_mod_SRP <- lmer(VALUE ~ `Burn Day` +Treatment+(1|STATION) , data = Model_data_SRP)

summary(mixed_mod_SRP)

plot(mixed_mod_SRP)

#predict conditional and marginal effects
Model_data_SRP.predict <- Model_data_SRP %>%
mutate(fit.marginal = predict(mixed_mod_SRP, re.form = NA),fit.conditional = predict(mixed_mod_SRP, re.form = NULL))  %>%
mutate(resid = resid(mixed_mod_SRP))

#Conditional fits from the random effect model with random intercepts on the raw data points, facetted by Station.
ggplot(Model_data_SRP.predict,aes(`Burn Day`,VALUE,color=STATION))+geom_point(color="grey80")+theme_bw()+
geom_line(aes(y = fit.marginal), col = 1, size = 2) +facet_wrap(vars(STATION))

#Marginal fit from the random effect model with random intercepts on the conditional residuals of the experimental units, differentiated by color.
ggplot(Model_data_SRP.predict,aes(`Burn Day`,fit.marginal+resid,color=STATION))+geom_point(color="grey80")+theme_bw()+
geom_line(aes(y = fit.conditional, col = STATION), size = 1)+
geom_line(aes(y = fit.marginal), col = 1, size = 2) +facet_wrap(vars(Treatment))



# Mixed model with random slope and intercept -----------------------------

mixed_mod_RI_RS_SRP <- lmer(VALUE ~ `Burn Day` +Treatment+(Treatment|STATION) , data = Model_data_SRP)

summary(mixed_mod_RI_RS_SRP)

plot(mixed_mod_RI_RS_SRP)

#predict conditional and marginal effects
mixed_mod_RI_RS_SRP_predict <- Model_data_SRP %>%
mutate(fit.marginal = predict(mixed_mod_RI_RS_SRP, re.form = NA),fit.conditional = predict(mixed_mod_RI_RS_SRP, re.form = NULL))  %>%
mutate(resid = resid(mixed_mod_RI_RS_SRP))

#Conditional fits from the random effect model with random intercepts on the raw data points, facetted by Station.
ggplot(mixed_mod_RI_RS_SRP_predict,aes(`Burn Day`,VALUE,color=STATION))+geom_point(color="grey80")+theme_bw()+
geom_line(aes(y = fit.marginal), col = 1, size = 2) +facet_wrap(vars(STATION))

#Marginal fit from the random effect model with random intercepts on the conditional residuals of the experimental units, differentiated by color.
ggplot(mixed_mod_RI_RS_SRP_predict,aes(`Burn Day`,fit.marginal+resid,color=STATION))+geom_point(color="grey80")+theme_bw()+
geom_line(aes(y = fit.conditional, col = STATION), size = 1)+
  geom_line(aes(y = fit.marginal), col = 1, size = 2) +  
facet_wrap(vars(Treatment))
