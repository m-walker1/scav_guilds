#load packages
library(dplyr)
library(MuMIn)
library(ggplot2)
library(psych)

#import data
data <- read.csv('datafile.csv')


#calculating basic stats about average species richness with standard errors
#here, species_richness_a = # of species visiting; 
#species_richness_b = # of species consuming the carcass

std_error <- function(x) {
  sqrt(var(x)/length(x))
}

std_error(data$species_richness_a)
mean(data$species_richness_a)  

std_error(data$species_richness_b)  
mean(data$species_richness_b)    


#consuming species richness at sites without vultures
v <- c(7, 1, 3, 1, 4, 8, 1, 3)
std_error(v)
mean(v)

#consuming species richness at sites WITH vultures
w <- c(8, 8, 3, 5)
std_error(w)
mean(w)


#GLM for visiting species richness (species richness A)

#step-wise reduction attempt:
glm_visit1 <- glm(species_richness_a ~ distance.to.water + grassland + shrubland + forest + animal.died.month + type +
                    elevation + size.carcass + animal.died.season + Carcass_species + distance.to.major.road.in.meters, 
                  na.action = na.fail, data = data, family = poisson())
summary(glm_visit1)


glm_visit2 <- glm(species_richness_a ~ distance.to.water + size.carcass + animal.died.season + 
                    Carcass_species, data = data, family = poisson())
summary(glm_visit2)


glm_visit3 <- glm(species_richness_a ~ distance.to.water + 
                    animal.died.season + Carcass_species, data = data, family = poisson())
summary(glm_visit3)

glm_visit4 <- glm(species_richness_a ~ animal.died.season + Carcass_species, data = data, family = poisson())
summary(glm_visit4)

#dredge function instead to come up with best models, after accounting for correlated variables
poisson_a <- glm(species_richness_a ~ distance.to.water + grassland + type + shrubland +
                   elevation + size.carcass + animal.died.season + Carcass_species + distance.to.major.road.in.meters, 
                 na.action = na.fail, data = data, family = poisson())
summary(poisson_a)
dredge(poisson_a)



##deviance explained by visitation models: 

deviance_expl <- function(x, y) {
  ((x - y)/(x)) * 100
}


visitations_1 <- glm(species_richness_a ~ Carcass_species, data = data, family=poisson())
summary(visitations_1)
#null deviance = 14.3491, residual = 8.6647

deviance_expl(14.3491, 8.6647)
# = 39.615

#next-best visitation model:
visitations_2 <- glm(species_richness_a ~ Carcass_species + elevation, data = data, family=poisson())
summary(visitations_2)
#null deviance = 14.3491, residual = 5.3743

deviance_expl(14.3491, 5.3743)
# = 62.54608


visitations_3 <- glm(species_richness_a ~ grassland, data = data, family=poisson())
summary(visitations_3)

deviance_expl(14.3491, 8.9894)
## = 37.35217


visitations_4 <- glm(species_richness_a ~ elevation, data = data, family=poisson())
summary(visitations_4)

deviance_expl(14.349, 9.215)
## = 35.7795


visitations_5 <- glm(species_richness_a ~ shrubland, data = data, family=poisson())
summary(visitations_5)

deviance_expl(14.3491, 9.9973)
## = 30.33



#GLMs for consuming species (species richness B)

#Species richness B

glm_consume1 <- glm(species_richness_b ~ distance.to.water + grassland + shrubland + forest + type +
                      elevation + size.carcass + animal.died.season + distance.to.major.road.in.meters + Carcass_species, 
                    na.action = na.fail, data = data, family = poisson())
summary(glm_consume1)

dredge(glm_consume1)


#deviance explained by best consuming species model: 
consume_1 <- glm(species_richness_b ~ animal.died.season + Carcass_species, data = data, family=poisson())
summary(consume_1)
deviance_expl(29.8411, 2.9807)
#90.01

#next-best model:
consume_2 <- glm(species_richness_b ~ elevation + Carcass_species, data = data, family=poisson())
summary(consume_2)

deviance_expl(29.8411, 13.021)
## = 56.365


consume_3 <- glm(species_richness_b ~ grassland, data = data, family=poisson())
summary(consume_3)

deviance_expl(29.8411, 17.943)
## = 39.87

consume_4 <- glm(species_richness_b ~ Carcass_species + grassland, data = data, family=poisson())
summary(consume_4)

deviance_expl(29.8411, 14.803)
## = 50.39

consume_5 <- glm(species_richness_b ~ Carcass_species, data = data, family=poisson())
summary(consume_5)

deviance_expl(29.8411, 18.271)
## = 38.77




#GLMs for detection time

# '0' is a stand-in for N/A

data_edit <- data %>% 
  filter(detection_time_hours != 0)

#basic stats

mean(data_edit$detection_time_hours)
std_error(data_edit$detection_time_hours)

glm_detect1 <- glm(detection_time_hours ~ distance.to.water + shrubland +
                     + elevation + size.carcass + distance.to.major.road.in.meters +
                     animal.died.season + Carcass_species, 
                   data = data_edit, na.action = na.fail, family = poisson())
summary(glm_detect1)

dredge(glm_detect1)


##deviance explained by best detection time model: 
detect_time <- glm(detection_time_hours ~ animal.died.season + distance.to.water
                   + elevation + distance.to.major.road.in.meters, data = data_edit, family=poisson())
summary(detect_time)

deviance_expl(120.515, 15.317)
## = 87.29038 



#GLMs for consumption time

data_edit_2 <- data %>% 
  filter(consumption_time_days != 0)

mean(data_edit_2$consumption_time_days)
std_error(data_edit_2$consumption_time_days)

glm_contime1 <- glm(consumption_time_days ~ distance.to.water + grassland + distance.to.major.road.in.meters +
                      size.carcass + shrubland + animal.died.season + Carcass_species, data = data_edit_2,
                    na.action = na.fail, family = poisson())
summary(glm_contime1)
dredge(glm_contime1)

##deviance explained by best consumption time model: 
consum_time <- glm(consumption_time_days ~ animal.died.season + distance.to.water
                   + size.carcass, data = data_edit_2, family=poisson())
summary(consum_time)

deviance_expl(236.2323, 3.8479)
## = 98.37114
