library(dplyr)
library(MuMIn)
library(ggplot2)

#continued from glm_code.R; this code for GLM visualization

#import data
data <- read.csv('datafile.csv', stringsAsFactors=FALSE)

#reconstruct best GLM for visiting species richness
best_visitation_glm <- glm(species_richness_a ~ Carcass_species + elevation, data = data, family=poisson())
summary(best_visitation_glm)

with(best_visitation_glm, cbind(res.deviance = deviance, df = df.residual,
                                p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#construct confidence intervals for plotting

data$phat <- predict(best_visitation_glm, type="response")
data$conf <- predict(best_visitation_glm, interval = 'confidence')
data <- data[with(data, order(Carcass_species, elevation)), ]


ilink <- poisson()$linkinv
## add fit and se.fit on the link scale
data <- bind_cols(data, setNames(as_tibble(predict(best_visitation_glm, data, se.fit = TRUE)[1:2]),
                                 c('fit_link','se_link')))
## create the interval and backtransform
data <- mutate(data,
               fit_resp  = ilink(fit_link),
               right_upr = ilink(fit_link + (2 * se_link)),
               right_lwr = ilink(fit_link - (2 * se_link)))
## show
data

r <- ggplot(data, aes(x = elevation, y = phat, colour = Carcass_species)) +
  geom_point(aes(y = species_richness_a), alpha=.5, position=position_jitter(h=.2)) +
  scale_color_manual(values=c('#FA4616','#0021A5'))+
  labs(x = "Elevation (m)", y = "Number of visiting species", colour = 'Carcass species') +
  theme_minimal() +
  theme(text = element_text(size=20)) +
  geom_line(size = 1)
r
r2 <- r + geom_ribbon(data = data,
                      aes(ymin = right_lwr, ymax = right_upr,
                          fill = Carcass_species, color = NULL), show.legend = F, alpha = .08 )
r2

ggsave('results/poissonFig1.png', width = 8, height = 8)



##reconstruct best GLM for consuming species richness

data2 <- read.csv('datafile.csv', stringsAsFactors=FALSE)

best_consuming_glm <- glm(species_richness_b ~ animal.died.season + Carcass_species, data = data2, family=poisson())
summary(best_consuming_glm )

#plotting
data2$phat2 <- predict(best_consuming_glm , type="response")
data2 <- data2[with(data2, order(Carcass_species, season)), ]

ilink <- poisson()$linkinv
## add fit and se.fit on the **link** scale
data2 <- bind_cols(data2, setNames(as_tibble(predict(best_consuming_glm , data2, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
data2 <- mutate(data2,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))
## show
data2

q <- ggplot(data2, aes(x = season, y = phat2, colour = Carcass_species)) +
  geom_point(aes(y = species_richness_b), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Season animal died", y = "Number of consuming species", colour = 'Carcass species') +
  scale_color_manual(values=c('#FA4616','#0021A5'))+
  theme_minimal() +
  theme(text = element_text(size=20))
q

q2 <- q + geom_ribbon(data = data2,
                      aes(ymin = right_lwr, ymax = right_upr,
                          fill = Carcass_species, color = NULL), show.legend = F, alpha = .08 )
q2

ggsave('results/poisson_Supp_Fig1.png', width = 8, height = 8)
       