# --------------
# @title Baysian Linear Models
# @date 10/30/2020 Snowday!
#---------------

#libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#libraries for bayes
library(brms)
options(mc.cores=parallel::detectCores()-1)

seals <- read.csv("lab/lab_data/17e8ShrinkingSeals Trites 1996.csv")

# 1. Fit a linear model with Bayes ####

#This will take a long time to run!
seal_lm_bayes <- brm(length.cm ~age.days,
                     family=gaussian(link="identity"),
                     data = seals, 
                     file = "lab/brms_fits/seal_ml_bayes", #save file so you dont have to run the whole thing again
                     chains = 3,
                     seed= 607)
  
# 2. Assess that our Golem (model) isnt going to burn down Prague ####
library(bayesplot) #for pretty plots! See cheat sheet for details
color_scheme_set("viridis")

# visually investigate our chains

plot(seal_lm_bayes) 

#or, if you want to look at just the intercept:
plot(seal_lm_bayes, par="b_Intercept") 

mcmc_trace(seal_lm_bayes)

#look at diagnostic of convergence

#Gelman_Rubin statistic- Rhat

rhat(seal_lm_bayes)

rhat(seal_lm_bayes) %>% mcmc_rhat()

#assess autocorrelation (shouldn't be a problem)

mcmc_acf(seal_lm_bayes) #mcmc "autocorrelation function" Should start at one, and then drop to 0 (may fluctuate around 0)!!! If it doesnt ever drup to 0, you have a problem. 
  
# check our MODEL assumptions ####
#see ?pp_check file for all the ways to check out assumptions!

#check the match between out data and our chains for dist of y
pp_check(seal_lm_bayes, "dens_overlay")

#is our error normal?
pp_check(seal_lm_bayes, "error_hist", bins=10) #we want normal dist

#or...
pp_check(seal_lm_bayes, "error_scatter") #we want linear relationship

#or...
pp_check(seal_lm_bayes, "error_scatter_avg") #takes average across all posteriors (observed vs error). Generally positive.

#to see fitted vs residuals

seal_res <- residuals(seal_lm_bayes) %>%  #first get residuals
  as_tibble #make df

seal_fit <- fitted(seal_lm_bayes) %>% #then get fitted values
  as_tibble

plot(y=seal_res$Estimate, x=seal_fit$Estimate)

# For a model of the data, the simulated vs. observed data technique is the single most informative thing
# For predictive models, look at ___ See lab video. missed this and its important.

#Inference with our models - what is here?

summary(seal_lm_bayes)

# Visualize our posteriors ####
library(tidybayes) #extractions and tidying
library(ggdist) #visualization

seal_coef_draws <- gather_draws(seal_lm_bayes,
                                b_Intercept,
                                b_age.days,
                                sigma)

#see if jarrett could fix this in the calss code- all parameters should have peaks
ggplot(seal_coef_draws,
       mapping=aes(x=.value))+
  facet_wrap(~.variable, scale="free")+
  stat_halfeye()

#what is the probability that our slope is less than 0? Or any parameter
seal_coef_draws %>% 
  group_by(.variable) %>% 
  summarize(prob_less_than_0=
              sum(.value<0)/ n())

#what is the probability that our slope is less than 0.0023? 
seal_coef_draws %>% 
  group_by(.variable) %>% 
  summarize(prob_less_than_0.0023=
              sum(.value<0.0023)/ n())

#visualize out line!

seal_plot <- ggplot(seals,
                    aes(x=age.days,
                        y=length.cm))+
  geom_point()

#add a line
seal_coefs <- brms::fixef(seal_lm_bayes)

#ADD SEAL COEFS WIDE FROM CLASS CODE- Missed

#show uncertainty in fit
seal_plot+
  geom_abline(data=seal_coefs_wide,
              aes(slope= b_age.days,
                  intercept= b_Intercept),
              color="blue",
              alpha=0.1) +
  #median fit
  seal_plot+
  geom_abline(slope=seal_coefs[2,1],
              intercept= seal_coefs[1,1],
              color="red",
              size=1)

#Show uncertainty in prediction
seal_predict <- posterior_predict(
  seal_lm_bayes,
  newdata= data.frame(age.days = c(2000,8000))) %>% 
  as.data.frame()

predict_data <- data.frame(
  age.days=c(2000,8000)) %>% 
  add_predicted_draws(model=seal_lm_bayes)

seal_plot+
  geom_line(data = predict_data,
            aes(x=age.days,
                y=.prediction,
                group=.draw),
            alpha=0.05,
            color="lightgreen") +
  #median fit
  geom_abline(slope=seal_coefs[2,1],
              intercept= seal_coefs[1,1],
              color="red",
              size=1)

#ADD NEXT PART FROM CLASS CODE-MISSED 


#Breif of LOO ####
library(loo)

seal_waic <- waic(seal_lm_bayes) #get WAIC score
seal_waic

seal_loo <- loo(seal_lm_bayes) #Leave one out inference
seal_loo


#lets say I had a fit model, seal_poly... (Jarrett commented out because this would take too long to run)
#seal_poly <- brm(length.cm ~poly(age.days, r),
#     data=seals,
#     family=gaussian(link="identity"))
#ploy_loo <- loo(seal_poly)
#
#compare(seal_loo, poly_loo)

#for kfold cross validation
#seal_k10 <- kfold(seal_lm_bayes, k=10)

#Futz with priors ####
prior_summary(seal_lm_bayes)

seal_lm_prior <- brm(length.cm ~age.days,
                     data=seals,
                     family = gaussian(link="identity"),
                     prior=c(prior(coef="age.days",
                             prior=normal(1,1))),
                     chains=3)
#note: if you have a good prior, sampling usually goes faster :)

fixef(seal_lm_prior)
fixef(seal_lm_bayes)

seal_prior_waic <- waic(seal_lm_prior)

#note: prior is part of model structure, so if you change it, all the modeling steps (assumptions, etc) need to be done again