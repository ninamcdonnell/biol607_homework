# 1. #### 
#Let’s first look at the data. Plot it, along with a polynomial fit 
#(remember, formula = y ~ poly(x,2) for a quadratic). Then, compare the r2
# value of a linear versus fith order fit. What do you see?

#libraries 
library(dplyr)
library(ggplot2)
library(tidyr)
library(rsample)
library(purrr)


#read in data

progesterone_data <- read.csv("homework/homework_data/chap17q07ProgesteroneExercise.csv")

str(progesterone_data)

#plot it -  NOT DONE

#linear fit
progesterone_linear <- lm(progesterone~ventilation, data=progesterone_data)
summary(progesterone_linear)$r.squared

#5th order fit
progesterone_5order <- lm(progesterone ~ poly(ventilation,5), data = progesterone_data)
summary(progesterone_5order)$r.squared

#2 ####

#Fit each model with 5-fold CV
#Does that result hold up, or is it due to overfitting? Let’s evaluate by comparing 
#5-fold CV scores using RMSE. Let’s do this efficiently, though!

#A. Get things ready! Make a 5-fold cross validation tibble using 
#rsample::vfold_cv() and then combine each possible fold with the 
#polynomials 1:5 using tidyr::crossing()

progesterone_5order_five_fold <- vfold_cv(progesterone_data, v=5) %>% 
  mutate(model=crossing(1,
                        order=c(1,2,3,4,5)))

#maybe try pivoting or somthing so that for each split, we can have 5 order values
sample_df <- crossing(order=c(1,2,3,4,5),
                      fold=c(1,2,3,4,5))

#fit a model to each fold

progesterone_5order_five_fold <- progesterone_5order_five_fold %>% 
  mutate(mods=map(splits,
                  ~lm(progesterone ~ poly(ventilation,5), #for each split, fit a model (training)
                      data= analysis(.x))))

roach_five_fold <- roach_five_fold %>% #start with our tibble
  mutate(mods= map(splits, #make a new column for model, using map to iterate all our splits (in map, ~ means "evalueate the part that comes next")
                   ~lm(progesterone ~ poly(ventilation,5), #for each split, fit a model (training)
                       data= analysis(.x)))) #.x is a standin for every element of the splits


# 3 ####


