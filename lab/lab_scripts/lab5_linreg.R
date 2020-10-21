
#--------------
#'@title Linear Regression
#'
#'@date 2020-10-09
#'-------------

#' The fundamental Steps of executing a regression:
#' 1. Load the data
#' 2. visualize the data- just to detect problems and preform cursory test of assumptions!
#' 3. Fit the model
#' 4. Use the model fit to test assumptions
#' 5. Evaluate the model 
#' 6. Visualize the data - Do this last, or you might get confirmation bias from catering to an apparently good model during the evaluation proccess!!!

#Seal model ####

library(dplyr)
library(ggplot)
library(tidyr)

#1. load the data
seals <- read.csv("lab/lab_data/17e8ShrinkingSeals Trites 1996.csv") #if this doesnt run, try replacing spaces with _

#what's here?
str(seals)
summary(seals)

#2. visualize data

#visdat
skimr::skim(seals)

#plot it!
seal_plot <- ggplot(seals,
       mapping = aes(x=age.days, y=length.cm))+
  geom_point(alpha=0.5)

#3. fit the model

#model formulas are in y~x format
seal_lm <- lm(length.cm~age.days, data=seals)

seal_lm
coef(seal_lm)

#4. use the fit model to test assumptions

#does the dist of out predictions match our data?

#simulate data from model (20 times)
seal_sims <- simulate(seal_lm, nsim= 20) %>% 
  pivot_longer(
    cols=everything(),
    names_to= "sim",
    values_to = "length.cm"
  )

#plot simulated data against real data (blue line)
ggplot()+
  geom_density(data=seal_sims,
               mapping=aes(x=length.cm, group=sim),
               size= 0.2)+
  geom_density(data= seal_sims,
               mapping=aes(x=length.cm),
               size=2, color="blue")


?plot.lm

#is there a relationship between ditted and residual values?

plot(seal_lm, which = 1) #plot will give several plot options. Which specified which plot you want. 1 is residuals vs fitted

#with ggplot
library(ggfortify)
autoplot(seal_lm, which=1, ncol=1)

#did we satisfy normality and homoskedacity using a qq plot and levene test

#levine test 
residuals(seal_lm) %>% hist() #looks great

#shapiro-wilks of residuals 
residuals(seal_lm) %>% shapiro.test() #Jarrett needs to fix this. This test is also very sensitive so other metrics can be more useful

#look for outliers with leverage - COPY JERRETTS CODE FROM ETHER PAD:
dat <- tibble(x=c(1:10, 100),
           y=c(rnorm(10, x), 50))

ggplot(data=dat[,-11],
       aes(x=x, y=y)) +
  geom_point()+
  stat_smooth(method="lm")


#look for outliers with leverage

#5. evaluate the model 

library(broom)

#f-test
#did we explain any wariation in the data other than noise?
#Null H- our model should have just as much explanatory power as the noise we observe - var(model)/var(error) = F ratio
#if we get a small probability value, we reject null

anova(seal_lm)
anova(seal_lm) %>% 
  tidy()

#t-test of parameters
# if we divide a parameter by its SE= t 
#we can use that to see if we can reject the hypothseis that our parameter=0

summary(seal_lm)

#to get just the R2 or coef
summary(seal_lm)$r.squared
summary(seal_lm)$coef

#5. visualize your model ####

seal_plot +
  stat_smooth(method = "lm") #shows error around our fit, called "fit interval"

?predict #this is what stat_smooth is actually uwing for lm
fit_seals <- predict(seal_lm, 
                     interval= "confidence") %>% 
  as_tibble() %>% 
  rename(lwr_ci=lwr,
         upr_ci= upr)

seals <- cbind(seals, fit_seals)

ggplot(seals, 
       aes(x=age.days, ymin=lwr_ci,
           ymax= upr_ci,
           y=fit)) +
  geom_ribbon()+
  geom_line(color = "blue")

#prediction interval- the band for predictions will be wider because it is taking into account the error

predict_seals <-  predict(seal_lm,
                          interval= "prediction") %>% 
  as_tibble() %>% 
  rename(lwr_pi=lwr,
         upr_pi=upr)
seals <- cbind(seals, predict_seals)

#[get ggplot code from etherpad]
