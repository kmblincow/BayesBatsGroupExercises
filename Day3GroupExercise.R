#Kayla Blincow
#Day 3 Group Exercise

#data source: https://election.lab.ufl.edu/voter-turnout/

#QUESTION: What was the voter turnout rate before/after civil war?

#ALLOW STUDENTS TO DISCUSS AMONGST THEMSELVES WHAT THEY THINK WOULD HAPPEN 
#(BEFORE LOOKING AT ANYTHING). HAVE THEM WRITE DOWN THEIR THOUGHTS (THIS WILL 
# INFORM THEIR PRIOR CHOICES LATER).


#GIVE THEM THE DATASET, ALLOW THEM TO DO THE DATA MANIPULATION, AND DO BASIC EXPLORATION.

#clear workspace
rm(list = ls())

#load packages
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(broom.mixed)

#read data
election <- read.csv("US_VEP_Turnout_Rates.csv")

#clean data
pres <- election %>% 
  drop_na(TURNOUT_RATE_PRES) %>% 
  select(-TURNOUT_RATE_MIDTERM)

#take a look
hist(pres$TURNOUT_RATE_PRES)
plot(pres$YEAR, pres$TURNOUT_RATE_PRES, type = "l")

#civil war breaking point pre-1864, post-1864
precivil <- pres %>% filter(YEAR < 1864)
postcivil <- pres %>% filter(YEAR > 1864)




#do the thing (pre-civil war)
precivilfit <- stan_glm(data = precivil, family = gaussian,
                TURNOUT_RATE_PRES ~ 1, 
                prior_intercept = normal(0.5, 0.1),
                prior_aux = cauchy(0, 2), 
                chains = 4, iter = 5000*2, seed = 84735, refresh = FALSE
)

#diagnostics
mcmc_trace(precivilfit) # show trace plots
mcmc_dens_overlay(precivilfit) # show posteriors
bayesplot::rhat(precivilfit)
neff_ratio(precivilfit)

#look at output
tidy(precivilfit,
     effects = c("fixed", "aux"),
     conf.int = TRUE,
     conf.level = 0.95)

#do the thing (post-civil war)
postcivilfit <- stan_glm(data = postcivil, family = gaussian,
                        TURNOUT_RATE_PRES ~ 1, 
                        prior_intercept = normal(0.5, 0.1),
                        prior_aux = cauchy(0, 2), 
                        chains = 4, iter = 5000*2, seed = 84735, refresh = FALSE
)

#diagnostics
mcmc_trace(postcivilfit) # show trace plots
mcmc_dens_overlay(postcivilfit) # show posteriors
bayesplot::rhat(postcivilfit)
neff_ratio(postcivilfit)

tidy(postcivilfit,
     effects = c("fixed", "aux"),
     conf.int = TRUE,
     conf.level = 0.95) # written summary of the output


#plot the posteriors and assess the results combined
#get posterior values
postsimdata <- as.data.frame(postcivilfit)
presimdata <- as.data.frame(precivilfit)

#data cleaning/formatting
postsimdata$prepost <- "POST-CIVIL WAR"
presimdata$prepost <- "PRE-CIVIL WAR"

combo <- rbind(postsimdata, presimdata)
combo$prepost <- factor(combo$prepost, levels = c("PRE-CIVIL WAR",
                                                  "POST-CIVIL WAR"))
#make the plots
pmean <- ggplot() +
  geom_density(data = combo, aes(x = `(Intercept)`, 
                                 fill = prepost, 
                                 color = prepost),
               alpha = 0.5) +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Posterior Distribution of the Mean", y = "Density",
       color = element_blank(), fill = element_blank()) +
  theme_bw() +
  theme(legend.position = "bottom")

psd <- ggplot() +
  geom_density(data = combo, aes(x = sigma, 
                                 fill = prepost, 
                                 color = prepost),
               alpha = 0.5) +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Posterior Distribution of Sigma", y = "Density",
       color = element_blank(), fill = element_blank()) +
  theme_bw() +
  theme(legend.position = "bottom")

pts <- ggplot(data = pres, aes(x = YEAR, y = TURNOUT_RATE_PRES)) +
  geom_line() + 
  labs(x = "Year", y = "Voter Turnout Rate") +
  theme_bw()

#combined plot
library(patchwork) #best package ever!
(pmean + psd) / pts +
  plot_layout(guides = "collect",
              heights = c(2,1)) & theme(legend.position = 'top')
  