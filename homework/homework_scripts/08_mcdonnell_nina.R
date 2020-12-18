
# list assumptions for 1.2

#libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) 
library(emmeans)
library(brms)
library(bayesplot)
library(loo)
library(ggdist)
library(tidybayes)


# 1 #### 

#1.1
#read in data
pines <- read.csv(file="https://www.zoology.ubc.ca/~whitlock/ABD/teaching/datasets/15/15q22LodgepolePineCones.csv")

#what's here?
str(pines)

#plot cone mass
ggplot(pines, aes(x=habitat, y=conemass, fill=habitat))+
  geom_violin(trim=FALSE,alpha=0.8)+
  geom_point(position= position_jitterdodge())+
  geom_boxplot(fill="white", width=.15, alpha=0.8)+
  scale_fill_manual(values=c("forestgreen", "saddlebrown", "yellowgreen"))+
  labs(x="habitat", y="cone mass", title="Lodgepole pine cone mass by habitat")+
  theme_minimal()

#1.2
pines_lm <- lm(conemass~habitat, data=pines)

# check assumptions
plot(pines_lm, which = 1)
plot(pines_lm, which = 2)
shapiro.test(residuals(pines_lm))

plot(pines_lm, which = 4)
plot(pines_lm, which = 5)

#add residuals to our data frame
pines <- pines %>%
  modelr::add_residuals(pines_lm)

#plotting residuals by group with car
ggplot(pines,
       aes(x = habitat, y = resid)) +
  geom_boxplot()

#check for HOV
car::leveneTest(pines_lm)

# Let's evaluate our model! ####
anova(pines_lm)

# car::Anova
car::Anova(pines_lm, Type="II")

#1.2-2

summary(pines_lm)
#the adjusted R-squared value is 0.86, so the model explains 0.86 of the variation

#1.3 

pines_lm_no_int <- update(pines_lm, formula = . ~ . -1)

summary(pines_lm_no_int)


pines_em <- emmeans(pines_lm, ~habitat)

contrast(pines_em,
         method = "tukey", adjust="bonferroni") #island.absent - island.present and island.absent - mainland.present were significantly different

# HELP 1.3 ####

plot(contrast(pines_em,
         method = "tukey", adjust="bonferroni")) #how do i get line through this?

#tried it without intercept...
pines_em_no_int <- emmeans(pines_lm_no_int, ~habitat)

plot(contrast(pines_em_no_int,
              method = "tukey")) +
  geom_vline(xintercept = 0, color = "red", lty=2)

?contrast
# 1.4 ####

#Yes, I added a bonferroni correction to lower the chance of false discovery arising from multiple comparisons at once. 
#This may be conservative but since there are only three comparisons being made it probably makes no difference. 


# 2 #### 

#read in data
cover <- read.csv(file="homework/homework_data/fouling_transplant_data.csv") %>% 
  janitor::clean_names()
str(cover)

#2.1. Load and plot the data. We are interested in change in percent cover. Choose a plot that not only shows the raw data, 
#but also the means and SE or CI of those means. +1 EC if Michael thinks it’s fancy.

#plot
ggplot(data=cover, 
       mapping=aes(y=change_in_cover, x=caged, fill=position_on_block)) +
  geom_boxplot(width=0.9, position=position_dodge(width=1))+
  scale_fill_manual(values=c("coral1","seagreen2"))+
  labs(y="change in percent cover", x= NULL, fill="position on block")+
  theme_bw()
colors()
#2.2 Fit a model using likelihood and evaluate all relevant assumptions. Do you meet assumptions?
cover_glm <- glm(change_in_cover~caged*position_on_block, data=cover, family = gaussian(link="identity"))
summary(cover_glm)

# check assumptions
plot(cover_glm, which = 1) #bad
plot(cover_glm, which = 2) #bad
shapiro.test(residuals(cover_glm)) #bad

plot(cover_glm, which = 4)

#add residuals to our data frame
cover <- cover %>%
  modelr::add_residuals(cover_glm)

#plotting residuals by group -- unequal variance
ggplot(cover,
       aes(x = caged, y = resid)) +
  geom_boxplot()

ggplot(cover,
       aes(x = position_on_block, y = resid)) +
  geom_boxplot()

ggplot(cover,
       aes(x = caged, y = resid, fill= position_on_block)) +
  geom_boxplot()

#check for HOV
car::leveneTest(cover_glm)

# Let's evaluate our model! ####
anova(pines_lm)

# car::Anova
car::Anova(cover_glm)

#2.3 - HELP ####
#1
cover_glm_1 <- glm(initial_cover+change_in_cover~caged*position_on_block, data=cover, family = gaussian(link="identity"))
summary(cover_glm)

plot(cover_glm_1, which = 1) 
plot(cover_glm_1, which = 2) # not great, better than 2
plot(cover_glm_1, which = 3)
plot(cover_glm_1, which = 4)

#2
cover2 <- cover %>% 
  mutate(percent_change_relative=change_in_cover/initial_cover)

cover_glm_2 <- glm(percent_change_relative~caged*position_on_block, data=cover2, family = gaussian(link="identity"))
summary(cover_glm)

plot(cover_glm_2, which = 1) # variance is not equal
plot(cover_glm_2, which = 2) # diverges from normal line
plot(cover_glm_2, which = 3)
plot(cover_glm_2, which = 4)

#3
cover3 <- cover2 %>% 
  mutate(dif_logit_cover=logit(initial_cover) - logit(final_cover))

cover_glm_3 <- glm(dif_logit_cover~caged*position_on_block, data=cover3, family = gaussian(link="identity"))
summary(cover_glm)

plot(cover_glm_3, which = 1) # horrible
plot(cover_glm_3, which = 2) # horrible
plot(cover_glm_3, which = 3)
plot(cover_glm_3, which = 4) 

#2.4

summary(cover_glm_1)

# 3 ####

#3.1

#read in data
rats <- read.csv(file="https://www.zoology.ubc.ca/~whitlock/ABD/teaching/datasets/18/18e4MoleRatLayabouts.csv")

#str(rats)

#check distrubution
hist(rats$lnenergy)
shapiro.test(rats$lnenergy)

#check distrubution
hist(rats$lnmass)
shapiro.test(rats$lnmass)

#lnenergy ~ caste
ggplot(rats, aes(x=caste, y=lnenergy, color=caste))+
  geom_point(position=position_jitter(width=0.2))

#lnenergy ~ lnmass
ggplot(rats, aes(x=lnmass, y=lnenergy))+
  geom_point()

#interaction
rat_brm <- brm(lnenergy~lnmass*caste, data=rats, family=gaussian(link="identity"))

#additive 
rat_brm2 <- brm(lnenergy~lnmass+caste, data=rats, family=gaussian(link="identity"))

#interaction

seal_loo <- loo(rat_brm) #Leave one out inference
seal_loo

#additive - more predictive!

seal_loo <- loo(rat_brm2) #Leave one out inference
seal_loo

?loo

contrast(emmeans(rat_brm2, ~caste), method="tukey") %>% 
  plot()+geom_vline(xintercept=0)

rat_em <- emmeans(rat_brm2, ~caste)
contrast(rat_em,
         method = "tukey", adjust="bonferroni")






rat_brm_no_int <- brm(lnenergy~caste, data=rats, family=gaussian(link="identity"))

rat_brm_int <- update(rat_brm2, formula=.~. -mean(lnmass))
summary(rat_brm_int)
plot(rat_brm_int)


rat_coef_draws <- gather_draws(rat_brm_int,
                               b_Intercept,
                               b_casteworker,
                               sigma)
rat_coef_draws %>% 
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh( .width = c(0.8, 0.95))

modelr::data_grid(rats, .rat_brm2)
?tidybayes

ggplot(rat_coef_draws,
       mapping=aes(x=.value))+
  facet_wrap(~.variable, scale="free")+
  stat_halfeye()

contrast(emmeans(rat_brm2, ~caste), method="tukey") %>% 
  plot()+geom_vline(xintercept=0)