
library(dplyr)
library(car)
library(ggplot2)
library(broom)
library(modelr)
library(visreg)

keeley <- read.csv("lab/lab_data/data_lab11/Keeley_rawdata_select4.csv")

head(keeley)

ggplot(data=keeley,
       aes(x=cover, y=rich, color=firesev))+
  geom_point()+
  scale_color_gradient(low="yellow", high="red")+
  facet_wrap(~cut_number(firesev,4))

pairs(keeley) #plots all pairwise comparisons, works with numeric

#Fit a MLR

keeley_mlr <- lm(rich~ firesev+cover, data=keeley)

plot(keeley_mlr, which=1)
plot(keeley_mlr, which=2) #ends are a little wonky but not too bad
shapiro.test(residuals(keeley_mlr)) #not normal, but s-w is very sensitive to large sample size. think about alternatives
plot(keeley_mlr, which=5)

#also consider individuals for individual predictors!

residualPlots(keeley_mlr) #indicates potential non-linearity. No super clear patterns. Lines are not great indicator- look at point pattens more thanline

#are predictors too correlated?
keeley %>% 
  select(firesev, cover) %>% 
  cor() #correlation matrix shows cor of -0.437 (something over 0.8 would be worrysome, always look at VIF too)

#variance inflation factor
vif(keeley_mlr) #all good 

#Evaluation
#f-test
Anova(keeley_mlr)

summary(keeley_mlr) #expected richness with cover=0 and firesev=0. You may want to center cover because cover=0 is not really a realistic value

#Visualization ####

library(visreg)
visreg(keeley_mlr, gg=TURE) #shows one effect at mean value of other effects!!

#our own hommade visreg:

k_cover_pred <- data.frame(firesev=median(keeley$firesev),
                                 cover=seq(0,1.6, length.out = 100))

k_cover_fit<- cbind(k_cover_pred, 
                      predict(keeley_mlr,
                              newdata = k_cover_pred,
                              interval= "confidence"))

#plot our fit(s) with the data
ggplot(keeley,
       aes(x = cover, y = rich)) +
  geom_point() +
  geom_line(data = k_cover_fit, aes(y = fit), color = "blue") +
  geom_ribbon(data = k_cover_fit,
              aes(y = fit, ymin = lwr, ymax = upr),
              alpha = 0.3, color = "lightgrey")

#visualization with multiple combinations of predictors

k_firesev_explore <- data_grid(keeley, 
                               cover=seq_range(cover, 100),
                               firesev=seq_range(firesev, 4)) %>% 
  add_predictions(model=keeley_mlr, var = "rich")

#now add line to plot from before
ggplot(data=keeley,
       aes(x=cover, y=rich, color=firesev))+
  geom_point()+
  geom_line(data=k_firesev_explore)+
  scale_color_gradient(low="yellow", high="red")+
  facet_wrap(~cut_interval(firesev,4)) #now you need to use cut_interval because k_firesev_explore uses interval

 #add interaction effect ####

keeley_mlr_int <- lm(rich~cover*firesev, data=keeley)

residualPlots(keeley_mlr_int) #better!
vif(keeley_mlr_int) #adding interaction includes something colinear, so now its higher 

#fit a centered model
keeley <- keeley %>% 
  mutate(firesev_c = firesev-mean(firesev), #centered
         cover_c=cover-mean(cover))

keeley_mlr_int_c <- lm(rich~cover_c*firesev_c, data=keeley)
vif(keeley_mlr_int_c) # much lower

#compare centered vs not
Anova(keeley_mlr_int)
Anova(keeley_mlr_int_c) #does not affect outcome

#visualize interactions ####

visreg(keeley_mlr_int, "cover", by= "firesev", gg=TRUE)

keeley_int_explore <- data_grid(keeley,
                                cover = seq_range(cover, 100),
                                firesev = seq_range(firesev, 4)) %>%
  add_predictions(model = keeley_mlr_int, var = "rich")


ggplot(data = keeley, 
       aes(x = cover, y = rich, color = firesev)) +
  geom_point() +
  geom_line(data = keeley_int_explore, aes(group = firesev)) +
  scale_color_gradient(low = "yellow", high = "red")

# surface
