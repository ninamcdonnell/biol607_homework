
#libraries

library(dplyr)
library(modelr)
library(car)
library(tidyr)
library(emmeans)
library(brms)
library(tidybayes)
library(ggdist)
library(RColorBrewer)

intertidal <- read_csv("lab/lab_data/data_lab10/18e3IntertidalAlgae.csv")

#play with data visualization

ggplot(data=intertidal, 
       mapping=aes(x=herbivores, y=sqrtarea,
                   color=height)) +
  geom_point(position = position_dodge(width=1))

ggplot(data=intertidal, 
       mapping=aes(x=herbivores, y=sqrtarea,
                   fill=height)) +
  stat_halfeye(position = position_dodge(width=0.5))

ggplot(data=intertidal, 
       mapping=aes(x=herbivores, y=sqrtarea,
                   fill=height)) +
  geom_boxplot(position = position_dodge(width=0.5),
               width=0.5)

ggplot(data=intertidal, 
       mapping=aes(x=herbivores, y=sqrtarea,
                   fill=herbivores)) +
  geom_boxplot(position = position_dodge(width=0.5),
               width=0.5)

ggplot(data=intertidal, 
       mapping=aes(x=herbivores, y=sqrtarea,
                   fill=height)) +
  geom_violin(position = position_dodge(width=0.5),
               width=0.8, alpha=0.4)+
  geom_boxplot(position = position_dodge(width=0.5),
               width=0.1)+
  theme_bw()

ggplot(data=intertidal, 
       mapping=aes(x=height, y=sqrtarea,
                   color=herbivores)) +
  stat_summary()+
  stat_summary(fun=mean, geom="line",
               aes(group=herbivores))


#model out factorial design ####

intertidal_lm <- lm(sqrtarea ~ herbivores + height +
                      herbivores:height,
                    data=intertidal)

#this is a simplet way to write the above...
intertidal_lm <- lm(sqrtarea ~ herbivores * height, data=intertidal)

#for 3-way, it would be a*b*c, for 2 way interactions of a and b with c but not eachother: (a+b)*c (i think)

#assumptions
plot(intertidal_lm, which=1)
plot(intertidal_lm, which=2)
plot(intertidal_lm, which=4)

residualPlot(intertidal_lm)

#get residuals and add them to our df
intertidal <- intertidal %>% 
  modelr::add_residuals(intertidal_lm)

#evaluate our assumptions in terms of no group affect on residuals
#eah group should have similar residual SD

ggplot(intertidal,
       aes(x=height, y=resid))+
  geom_boxplot()

ggplot(intertidal,
       aes(x=herbivores, y=resid))+
  geom_boxplot()

ggplot(intertidal,
       aes(x=herbivores, y=resid, fill=height))+
  geom_boxplot()

ggplot(intertidal,
       aes(x=herbivores, y=resid, fill=height))+
  geom_point(position=position_dodge(width=0.5))

#evaluate our model###
#start with Anova or anodev - note: R Anova defaults to type II sums of squares, but some other stats tools use type III- can cause discepancies

Anova(intertidal_lm)
Anova(intertidal_lm, type="III")

#posthocs ####
#because we have interaction effect, we can NO LONGER look at individual effects alone
#if you try to use eemeans() with only one variable i.e. hervivores, it will yell at you.  Only proceed if there was NO INTERACTION effect. Otherwise it is wrong.

intertidal_em <- emmeans(intertidal_lm,
                         specs=~herbivores+height)

contrast(intertidal_em, "tukey", adjustment="none") 

contrast(intertidal_em, "tukey", adjustment="none") %>% plot()+
  geom_vline(xintercept= 0, color="red") #helpful for visualizing frequentist interests

#but, what if we just want to know, "does herbivore treatment matter at high or low heights?"

intertidal_em_2 <- emmeans(intertidal_lm, 
                           ~herbivores | height) #nest herbivore treatments in height

contrast(intertidal_em_2, "tukey", adjustment="none") 

contrast(intertidal_em_2, "tukey", adjustment="none") %>% plot()+
  geom_vline(xintercept= 0, color="red") 

#--------------#
#faded example - SOME THINGS ARE NOT RIGHT... REPLACE WITH CLASS CODE
#interested in Predator_Diversity and Trial
#Prop_change

kelp <- read.csv("lab/lab_data/data_lab10/kelp_pred_div_byrnesetal2006.csv")

#check and correct for non-factors
str(kelp) #most things are numeric
kep <- kelp %>% 
  mutate(Predator_Diversity= as.factor(Predator_Diversity),
         Trial=as.factor(Trial))

#visualize
ggplot(data=kelp, 
       mapping=aes(x=Treatment, y=Porp_Change,
                   fill=Trial)) +
  geom_boxplot(position = position_dodge(width=0.5),
               width=0.5)

ggplot(kelp, 
       aes(x=Predator_Diversity,
           y=Porp_Change,
           fill=Trial))+
  geom_boxplot(position=position_dodge())

#fit
kelp_lm <- lm(Porp_Change~Trial*Predator_Diversity, data=kelp)

#assumptions
plot(kelp_lm, which=1)
plot(kelp_lm, which=2)
plot(kelp_lm, which=4)
plot(kelp_lm, which=5)

#equal variance of residuals...
#get residuals and add them to our df
kelp <- kelp %>% 
  modelr::add_residuals(kelp_lm)

ggplot(kelp,
       aes(x=Trial, y=resid, fill=Predator_Diversity))+
  geom_point(position=position_dodge(width=0.5))

ggplot(kelp,
       aes(x=Predator_Diversity, y=resid, fill=Trial))+
  geom_point(position=position_dodge(width=0.5))

#Anova

Anova(kelp_lm)

#Tukey's HSD for simple effects
contrast(emmeans(kelp_lm, ~Predator_Diversity), method="tukey", adjustment="none") %>% plot()+
  geom_vline(xintercept= 0, color="red")

#-------#

#GLM with both categorican and continuous predictors

neand <- read.csv("lab/lab_data/data_lab11/18q09NeanderthalBrainSize.csv")

ggplot(neand,
       aes(x=species, y=lnbrain))+
  geom_boxplot() #they do not look different without the covariate included

ggplot(neand,
       aes(x=lnmass, y=lnbrain, color=species))+
  geom_point() +
  stat_smooth(method = "lm") #with covariate "lnmass" included, we can see a positive relationship that is different between speccies

ggplot(neand,
       aes(x=lnmass, y=species))+
  stat_halfeye()

#fit a model

neand_glm <- glm(lnbrain~lnmass+species,
                 data=neand,
                 family=gaussian(link="identity"))

#assumptions
plot(neand_glm, which=1)
plot(neand_glm, which=2)
plot(neand_glm, which=4)

residualPlots(neand_glm)

neand <- neand %>% 
  add_residuals(neand_glm)

qplot(species, resid, data=neand, geom="boxplot")

#try an interaction model instead of just additive
neand_int <- glm(lnbrain~lnmass*species,
                 data=neand,
                 family=gaussian(link="identity"))

Anova(neand_int)

#post_hocs
neand_em <- emmeans(neand_glm, specs= ~species) #it warns that this may be misleading! It's only okay because interaction effects wasnt significant

# if we were to examine differences inlnbrain means between species for a specific value of lnmass - particularly helpful if the two fit lines for species are not parallel i.e. the difference will change depending on lnmass value
#neand_em <- emmeans(neand_glm, specs= ~species | lnmass,
#     at=list(lnmass=4)) 

contrast(neand_em, method="tukey")

#look at slopes
emtrends(neand_glm, specs=~species, var="lnmass")

#if we had an interaction
emtrends(neand_glm, specs=~species, var="lnmass")
emmeans(neand_int, specs= ~species|lnmass, 
        at= list(lnmass=c(2,4,6))) %>% #see how means changed at these specified levels of lnmass
  contrast(method="tukey") %>% plot()

