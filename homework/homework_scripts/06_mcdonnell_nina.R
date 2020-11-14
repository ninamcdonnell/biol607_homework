library(knitr)
library(tidyr)
library(mvtnorm)
library(broom)
library(ggplot2)
library(dplyr)
library(MASS)
library(profileModel)

### 3. Grid Sampling! Based on Friday’s lab, load up the pufferfish data and 
#use grid sampling to find the MLE of the slope, intercept and residual SD 
#of this model. Feel free to eyeball results from an lm() fit to get 
#reasonable values. Try not to do this for a grid of more than ~100K points 
#(more if you want!). It’s ok to be coarse. Compare to lm.

#read in data
puffer <- read.csv("PufferfishMimicry.csv")

#what's here? 
str(puffer)

#plot resemblance ~ predators
ggplot(data= puffer, mapping=aes(x=resemblance, y=predators))+
  geom_point()+
  geom_smooth(method="lm")

#fit model - get slope and intercept
puffer_model <- lm(resemblance~predators, data= puffer)

#get sd of model residuals
puffer_residuals <- residuals(puffer_model)
residual_sd <- sd(puffer_residuals)

#draw sample 
samp <- rnorm(20, mean=15, sd=3)

#functions for likelihood and log-likelihood
norm_lik <- function(m, s){
  dnorm(samp, mean = m, sd = s, log = FALSE) %>% prod()
}

norm_loglik <- function(m, s){
  dnorm(samp, mean = m, sd = s, log = TRUE) %>% sum()
}

lik_df_mult <- tibble(mean = seq(10, 20, length.out = 100),
                      sd= seq(1, 5, length.out = 100)) %>%
  rowwise(mean) %>%
  mutate(lik = norm_lik(mean, sd),
         loglik = norm_loglik(mean, sd)) %>%
  ungroup()

#intercept (lm = 0.54) run all code together 

samp <- rnorm(20, mean=0.54, sd=3)

lik_df_intercept <- tibble(mean = seq(0, 1.5, length.out = 100),
                      sd= seq(1, 5, length.out = 100)) %>%
  rowwise(mean) %>%
  mutate(lik = norm_lik(mean, sd),
         loglik = norm_loglik(mean, sd)) %>%
  ungroup()

ggplot(lik_df_intercept,
       aes(x = mean, y = lik)) +
  geom_point()

#get maxlik
intercept_mle <- lik_df_intercept %>% 
  filter(lik==max(lik)) %>% 
  summarize(mean)

intercept_mle

#slope (lm= 0.2)

samp <- rnorm(20, mean=0.2, sd=3)

lik_df_slope <- tibble(mean = seq(0, .75, length.out = 100),
                           sd= seq(1, 5, length.out = 100)) %>%
  rowwise(mean) %>%
  mutate(lik = norm_lik(mean, sd),
         loglik = norm_loglik(mean, sd)) %>%
  ungroup()

ggplot(lik_df_slope,
       aes(x = mean, y = lik)) +
  geom_point()

slope_mle <- lik_df_slope %>% 
  filter(lik==max(lik)) %>% 
  summarize(mean)

slope_mle

#residual sd (lm= 0.77)

samp <- rnorm(20, mean=0.77, sd=3)

lik_df_residual_sd <- tibble(mean = seq(.5, 1.5, length.out = 100),
                           sd= seq(1, 5, length.out = 100)) %>%
  rowwise(mean) %>%
  mutate(lik = norm_lik(mean, sd),
         loglik = norm_loglik(mean, sd)) %>%
  ungroup()

ggplot(lik_df_residual_sd,
       aes(x = mean, y = lik)) +
  geom_point()

residual_sd_mle <- lik_df_residual_sd %>% 
  filter(lik==max(lik)) %>% 
  summarize(mean)

residual_sd_mle

# Q4 ####

#log-likelihood surface plot

?dnorm #use residual_sd for SD argument (?)

puffer_loglik_suface <- function(slope, int){
  yhat <- puffer$resemblance * slope + int
  sum(dnorm(yhat, puffer$predators, residual_sd)) 
}
coefs <- crossing(int = seq(-5,10, length.out = 200),
                  slope = seq(0,3.5, length.out = 200)) %>%
  rowwise(slope, int) %>%
  mutate(logLik = puffer_loglik_suface(slope, int)) %>%
  ungroup()
ll_plot_reg <- ggplot(data = coefs,
                      aes(x = slope, y = int, fill = logLik)) +
  geom_raster() +
  scale_fill_viridis_c(option = "A")
ll_plot_reg

#see line 129 of lab code for Q4

#Q5 ####

#fit glm
puffer_glm <- glm(resemblance~predators, data= puffer,
                  familly=gaussian(link="identity"))

#check assumptions
#relationship appears linear from scatterplot 
plot(puffer_glm, which = 1) #looks good, no relationship
plot(puffer_glm, which = 2) #looks good, about on-the-line
hist(residuals(puffer_glm)) #looks roughly normal
shapiro.test(residuals(puffer_glm)) #passes
plot(puffer_glm, which = 4) 
plot(puffer_glm, which = 5) #looks good, no high-leverage outliers

#check simulations against observed -> looks fine
model_sims <- simulate(puffer_glm, nsim = 100) %>%
  pivot_longer(
    cols = everything(),
    names_to = "sim",
    values_to = "predator"
  )
ggplot() +
  geom_density(data = model_sims,
               mapping = aes(x = predator, group = sim), 
               size = 0.2)  +
  geom_density(data = model_sims,
               mapping = aes(x = predator),
               size = 2, color = "blue")

#check profiles 
prof <- profileModel(puffer_glm,
                     objective = "ordinaryDeviance")

plot(prof, print.grid.points = TRUE) #good parabola

#tau
prof_predators <- profile(puffer_glm)
plot(prof_predators) #yes, it is a straight line

#confidence interval
confint(prof_predators)

# Q6 ####

#what's this??
?bbmle::mle2 #estimates parameters using maxlik

puffer_mle2 <- bbmle::mle2(minuslog1, start=list(slope = 2, intercept = 5, resid_sd = 2), data=puffer)



#let's make our likelihood surface!
lik_df_norm <- crossing(m = seq(10, 20, length.out = 300),
                        s = seq(1, 5, length.out = 300),
                        ) %>%
  group_by(m, s) %>%
  mutate(lik = norm_lik(m, s),
         loglik = norm_loglik(m,s),
         deviance = -2 * loglik) %>%
  ungroup
