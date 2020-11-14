
#general libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

#for cross validation
library(rsample)
library(boot)
library(modelr)

roaches <- read.csv("lab/lab_data/chap17f5_4CockroachNeurons.csv")

#plot ot with a fit lin reg
ggplot(data=roaches,
       aes(x=temperature, y=rate))+
  geom_point()+
  stat_smooth(method="lm", fill=NA)+ #plot lm (intercept and slope)
  stat_smooth(method="lm", fill=NA, #plot null model (y~1)
              formula= y~1, fill=NA,
              color="red", lineytpe = 2)+
  theme_bw()

#simplest case for CV is one point ####
roaches_no_15 <- roaches[-15,]
roaches_15 <- roaches[15,]

#CV in a nutshell
#1. fit model with training data

roach_lm_no_15 <- lm(rate~temperature, data=roaches_no_15)
roach_int_no_15 <- lm(rate~1, data=roaches_no_15) #null model

#2. evaluate out of sample deviance (MSE or RMSE) for test data

rmse(model= roach_lm_no_15, data= roaches_15)
rmse(model= roach_int_no_15, data= roaches_15) #The SD of the predictions is much greater in the null model

#K-fold cross validation ####

#make a folded dataset object  GET EC FOR FINDING OUT WHY ITS CALLED VFOLD
roach_five_fold <- vfold_cv(roaches, v=5) #makes five "list-columns"

#analysis(roach_five_fold$split[[1]]) 
#assessment(roach_five_fold$split[[1]])

#fit a model to each fold

roach_five_fold <- roach_five_fold %>% #start with our tibble
  mutate(mods= map(splits, #make a new column for model, using map to iterate all our splits (in map, ~ means "evalueate the part that comes next")
                   ~lm(rate~temperature, #for each split, fit a model (training)
                       data= analysis(.x)))) #.x is a standin for every element of the splits

roach_five_fold #now there is another list that includes the models

#extract the out-of-sample RMSE for each model at each fold, usinh function map2()

x <- 1:10
y <- 11:20

map2_dbl(x, y, ~.x +.y)

#start with our tibble

roach_five_fold <- roach_five_fold %>% 
  #create a new column, rmse, which we make with map2
  #iterating over all splits AND fit models
  mutate(rmse = map2_dbl(.x= splits, .y= mods, #splits will be .x and mods will be .y
                  ~rmse(model = .y,
                        data=assessment(.x))))

mean(roach_five_fold$rmse) #average out-of-sample SD!!!

#Get Jarretts code for the rmse plot

#LOOCV ####

#start bt making a LOO tibble
roach_loo <- roaches %>% 
  loo_cv() %>%  #creates 57 splits! One missing data point in each
  #fit the temp mod and intercept only mod
  #for each LOO split
  mutate(temp_mod = map(splits, #create a new column using map to interate over all splits
                        ~lm(rate~temperature, #fit that model on the training data from each split
                           data= analysis(.x))),
         int_mod = map(splits,
                       ~lm(rate~1,
                          data= analysis(.x))))
         
# Get the rmse of each model and each model TYPE for each LOO split

#start with R tibble
roach_loo <- roach_loo %>% 
  #pivot to put all models in one comumn
  pivot_longer(cols=c(temp_mod, int_mod),
               names_to= "model_name",
               values_to= "fit_model") %>% 
  #get out rmse just like before, with map2!
  mutate(rmse= map2_dbl(.x= splits, .y= fit_model, #what were using
                    ~rmse(data= assessment(.x), #our test data
                         mod= .y))) #our model to generate predictions

#the answer!
roach_loo %>% 
  group_by(model_name) %>% 
  summarize(loo_rmse = mean(rmse))

ggplot(data=roach_loo,
       mapping = aes(x=id, y=rmse, color=model_name))+
  geom_point()+
  scale_x_discrete(labels=NULL)

#using boot::cv.glm for LOO of K-fold ####

roach_glm <- glm(rate~temperature, data= roaches,
                 family=gaussian(link = "identity"))  #only worksfor glm fits, not lm

loo_roach <- cv.glm(data= roaches,
                    glmfit = roach_glm,
                    K=nrow(roaches))

#what is our LOO CV score (mse)?
loo_roach$delta[1] %>% 
  sqrt() #rmse - only valid for identity link- for others, use mse

#AIC ####

roach_lm <- lm(rate~temperature, data=roaches)
roach_int <- lm(rate~1, data=roaches)
roach_sq <- lm(rate~ poly(temperature, 2), data=roaches)
roach_cub <- lm(rate~ poly(temperature, 3), data=roaches)

AIC(roach_lm)
AIC(roach_int)

library(VGAM)
library(AICcmodavg)

mod_list <- list(roach_lm,roach_int,roach_sq, roach_cub)
name_vec <- c("linear", "int", "quad", "cube")

aictab(cand.set = mod_list, modnames = name_vec)

#what are coefs?
broom::tidy(roach_cub)

