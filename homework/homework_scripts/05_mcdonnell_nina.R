#'---------HEADER---------
#'@title Homework 5
#'@author Nina McDonnell
#'@Date 10/15/20

#NOTE: THIS IS NOT A FINAL HOMEWORK SUBMISSION. SEE .RMD FOR GRADING

library(dplyr)
library(ggplot)
library(tidyr)
library(ggfortify)
library(broom)

#read in the data
grey_matter <- read.csv("homework/homework_data/chap16q15LanguageGreyMatter.csv")

#what's here?
str(grey_matter)

#1a

#plot greymatter ~ proficiency 
profficiency_plot <- ggplot(grey_matter, 
                                 mapping=aes(x=proficiency, y= greymatter))+
  geom_point()

profficiency_plot

#1b

#fit a model
model <- lm(proficiency~greymatter, data=grey_matter)

#get cor coef
cor(x=grey_matter$greymatter, y=grey_matter$proficiency) # <- this one!

#1c

##use an F test to compare explained variation against null (see below, test assumptions, first)
anova(model) %>% 
  tidy() #make neat data frame


# test assumptions ####

# Does the distribution of our predictions match our data? - YES
#simulate 100 times from model
model_sims <- simulate(model, nsim = 100) %>%
  pivot_longer(
    cols = everything(),
    names_to = "sim",
    values_to = "greymatter"
  )
#plot simulations and data to compare distributions
ggplot() +
  geom_density(data = model_sims,
               mapping = aes(x = greymatter, group = sim), 
               size = 0.2)  +
  geom_density(data = model_sims,
               mapping = aes(x = greymatter),
               size = 2, color = "blue")

# Is there a relationship between fitted and residual values? - slight curve, but not bad
autoplot(model, which = 1, ncol = 1)

# Did we satisfy normality and homoskedacticity? - not perfect, but close
# QQ plot
plot(model, which = 2)

# check normality of residuals - passes shapiro-wilks and levene tests
residuals(model) %>% hist()

residuals(model) %>% 
  shapiro.test()

# Look for outliers with leverage
plot(model, which = 4) 
plot(model, which = 5) # no outliers are outside of the dotted red line for cook's distance, so I think its fine. 
#however, the spread of the residuals decreases with greater leverage, so the data may not have homoskedasticty (more 
#predictor variables may need to be considered in the model)



# Q2 ####

#read in the data
rat_tauro <- read.csv("homework/homework_data/chap16q19LiverPreparation.csv")

#what's here?
str(rat_tauro)

#2a

#fit a model
model2 <- lm(unboundFraction~concentration, data=rat_tauro)
coef(model2)

#use an F test to compare explained variation against null
anova(model2) %>% 
  tidy() #make neat data frame

#get cor coeff
cor(x=rat_tauro$concentration, y= rat_tauro$unboundFraction)

#2b plot the data
rat_plot <- ggplot(rat_tauro, 
                            mapping=aes(x=concentration, y= unboundFraction))+
  geom_point()

rat_plot

rat_plot <- ggplot(rat_tauro, 
                   mapping=aes(x=concentration, y= unboundFraction))+
  geom_point()+
  stat_smooth(method = "lm", se=FALSE, linetype= "dashed", color="gray")+
  theme_bw()


rat_plot

# Q3 ####

#3a

#set up the data frame
cats <- c(-.3, .42, .85, -.45, .22, -.12, 1.46, -.79, .4, -.07)
happiness_score <- c(-.57, -.1, -.04, -.29, .42, -.92, .99, -.62, 1.14, .33)
cat_lady <- data.frame(x=cats, y=happiness_score)

str(cat_lady)

#get cor coef for the variables
cat_cor <- cor(x=cats, y=happiness_score)
summary(cat_cor)

#to test hypothesis, first make model
model_cat_lady <- lm(happiness_score~cats, data=cat_lady)
summary(model_cat_lady)

#use an F test to compare explained variation against null
anova(model_cat_lady) %>% 
  tidy() #make neat data frame

#3b. What is the SE of the correlation based on the info from cor.test()

test <- cor.test(x=cats, y=happiness_score) %>% 
  tidy() #get SE from this by dividing 95% CI by 1.96

se2 <-  test$conf.high - test$conf.low #95% confidence interval = 1.96 SE

se1 <- se2/1.96
se1

#3c. Now, what is the SE via simulation? To do this, you’ll need to use cor() and get 
#the relevant parameter from the output (remember - you get a matrix back, so, what’s 
#the right index!), replicate(), and sample() or dplyr::sample_n() with replace=TRUE to get, 
#let’s say, 1000 correlations. How does this compare to your value above?

#replicate 1000 correlations for samples drawn from cat_lady
cor(cat_lady)
cat_sim <- replicate(1000,
                     cor(slice_sample(cat_lady, 
                                      n=nrow(cat_lady),
                                      replace=TRUE)) #gives matrix of correlations as output
                     [1,2]) %>% #index [1,2] or [2,1] gives correlation of x and y
  sd() #pipe to sd() to get SE 


#Q4 ####

#read in data
grassland <- read.csv("homework/homework_data/chap17q19GrasslandNutrientsPlantSpecies.csv")

#what's here
str(grassland)

#4a
#plot variables with explanatory (x) = nutrients and response (y) = species
grassland_plot <- ggplot(grassland, 
                   mapping=aes(x=nutrients, y= species))+
  geom_point()

grassland_plot

#4b

#fit model
grassland_model <- lm(species~nutrients, data=grassland)

# use summary to get coefficients
summary(grassland_model) #slope for species= -3.339, SE= 1.098. For every one unit increase in nutrients, there is a 3.339 decrease in number of species

#4c

#add least squares regression line to plot 
grassland_plot <- ggplot(grassland, 
                         mapping=aes(x=nutrients, y= species))+
  geom_point()+
  stat_smooth(method = "lm")

grassland_plot

#get R2 value
summary(grassland_model)$r.squared

#4d

#test against null hypothesis
anova(grassland_model) %>% 
  tidy()

#Q5 ####

#5a

#read in data
beetle <- read.csv("homework/homework_data/chap17q25BeetleWingsAndHorns.csv")

#what's here?
str(beetle)

#make the plot shown in the text
beetle_plot <- ggplot(beetle, 
                         mapping=aes(x=hornSize, y= wingMass))+
  geom_point()+
  stat_smooth(method = "lm", se=FALSE)
beetle_plot

#fit a model and calculate residuals
beetle_model <- lm(wingMass~hornSize, data=beetle)

#calculate residuals, test normality
beetle_residuals <- residuals(beetle_model)
beetle_residuals

shapiro.test(beetle_residuals) #is normal
hist(beetle_residuals) #looks about right

#5b 

#qqplot from plot() option 2
plot(beetle_model, which=2)

#5c

#' Test assumptions: 
#' 1. does dist of predictions match the data? - NOT QUITE 
#' 2. is there a relationship between fitted and residual values? - YES (it curves)
#' 3. did we satisfy normality and homoskedacticity? - YES (see 5b)
#' 4. look for outliers with leverage - the wing mass = -40 point seems to have high leverage

# 1. does dist of predictions match the data?

#simulate 100 samples based on model and compare to the real data
beetle_sims <- simulate(beetle_model, nsim = 100) %>%
  pivot_longer(
    cols = everything(),
    names_to = "sim",
    values_to = "wingMass"
  ) 

#compare simulations and data in plot
ggplot() +
  geom_density(data = beetle_sims,
               mapping = aes(x = wingMass, group = sim), 
               size = 0.2)  +
  geom_density(data = beetle,
               mapping = aes(x = wingMass),
               size = 2, color = "blue") #the peak from simulations is to the left of the peak from data... not quite right

# 2. is there a relationship between fitted and residual values?

#residuals vs. fitted values plot (plot 1 from function plot())
plot(beetle_model, which = 1) #yes, there is a relationship. There is a curved shape

# 4. look for outliers with leverage
plot(beetle_model, which = 4) 
plot(beetle_model, which = 5)

#try again with the -40 point removed (just to see how it affects things, not for drawing conclusions!)
beetle_no_outlier <- beetle %>% 
  filter(wingMass>-40) 

beetle_model_no_outlier <- lm(wingMass~hornSize, data=beetle_no_outlier)

plot(beetle_model_no_outlier, which = 5)

coef(beetle_model)
coef(beetle_model_no_outlier) #outlier point was enough to change the coef of hornSize from -132.6 to -101

# 5d 

#'This does not appear to be a linear relationship, and violates several test assumptions:
#'1. The simulations of the model have a peak to the left of the actual data peak- so something is 
#'probably going on that was not captured in the model
#'2. The relationship between residual and fitted values was curved, so there may not be homoscedacicity
#'4. The datapoint at wingmass=-40 does appear to have high leverage. I would reccomend first checking that
#'this datapoint is not an entry or measurement error, and if not, either: __ transform the data before fitting a model, or consider a non-linear model. 
#'I think it may also be worth measuring other structures, since wings might not tell the whole story here- but, the investigators probably have their reasons for choosing wings only...


#re-do plot without the outlier
beetle_plot_no_outlier <- ggplot(beetle_no_outlier, 
                      mapping=aes(x=hornSize, y= wingMass))+
  geom_point()+
  stat_smooth(method = "lm", se=FALSE)
beetle_plot_no_outlier

#re-do simulations and plotting, as above
beetle_sims <- simulate(beetle_model_no_outlier, nsim = 100) %>%
  pivot_longer(
    cols = everything(),
    names_to = "sim",
    values_to = "wingMass"
  ) 

ggplot() +
  geom_density(data = beetle_sims,
               mapping = aes(x = wingMass, group = sim), 
               size = 0.2)  +
  geom_density(data = beetle,
               mapping = aes(x = wingMass),
               size = 2, color = "blue") #this looks much better than before

# check fitted and residual again, as above
plot(beetle_model, which = 1) #still has the same curved shape with outlier removed

#check normality of residuals again, as above - dist of residuals has not changed much
shapiro.test(beetle_residuals) 
hist(beetle_residuals) 
plot(beetle_model, which=2)

#6

#read in data
teeth <- read.csv("homework/homework_data/chap17q30NuclearTeeth.csv")

#what's here?
str(teeth)

#fit model
teeth_model <- lm(dateOfBirth~deltaC14, data=teeth)

summary(teeth_model) #slope is approximately -5.326

#fit confidence interval
teeth_ci <- predict(teeth_model,
                    interval="confidence") %>% 
  as_tibble() %>% 
  rename(lwr_ci = lwr,
         upr_ci = upr)

teeth<- cbind(teeth, teeth_ci)

#fit prediction interval
teeth_pi <- predict(teeth_model,
                         interval="prediction") %>% 
  as_tibble() %>% 
  rename(lwr_pi = lwr,
         upr_pi = upr,
         fit2 = fit)

teeth <- cbind(teeth, teeth_pi) 

ggplot(data = teeth,
       mapping = aes(x = deltaC14,
                     y = dateOfBirth)) +
  #scatter plot
  geom_point()+
  #regression line
  stat_smooth(method="lm", se=FALSE)+
  #prediction interval
  geom_ribbon(mapping = aes(ymin = lwr_pi,
                            ymax = upr_pi),
              alpha = 0.25,
              fill="blue") +
  # fit interval
  geom_ribbon(mapping = aes(ymin = lwr_ci,
                            ymax = upr_ci),
              fill = "red",
              alpha = 0.25)+
  theme_classic()