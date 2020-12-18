
#Get help with 3C, 5C and D

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
library(modelr)
library(AICcmodavg)
library(palmerpenguins)
library(RColorBrewer)
library(ggdist)
library(brms)

#1 ####

#read in data
progesterone_data <- read.csv("homework/homework_data/chap17q07ProgesteroneExercise.csv")

#str(progesterone_data)

#plot it 
ggplot(progesterone_data, aes(x=ventilation, y=progesterone))+
  geom_point()+
  stat_smooth(method = "lm",se=FALSE)+
  stat_smooth(method="lm", formula=y ~ poly(x, 5), se=FALSE, color="red")

#linear fit
progesterone_linear <- lm(progesterone~ventilation, data=progesterone_data)
summary(progesterone_linear)$r.squared

#5th order fit
progesterone_5order <- lm(progesterone ~ poly(ventilation,5), data = progesterone_data)
summary(progesterone_5order)$r.squared

# 2 ####

#Fit each model with 5-fold CV
#Does that result hold up, or is it due to overfitting? Let’s evaluate by comparing 
#5-fold CV scores using RMSE. Let’s do this efficiently, though!

#A. Get things ready! Make a 5-fold cross validation tibble using 
#rsample::vfold_cv() and then combine each possible fold with the 
#polynomials 1:5 using tidyr::crossing()

progesterone_5order_five_fold <- vfold_cv(progesterone_data, v=5)

cv_df <- crossing(progesterone_5order_five_fold, poly=1:5)
str(cv_df)

# B. Now you have splits and a column of coefficients. Use purr::map2() to make a list column of fit models, where you use the splits and data and the polynomials for you poly() call in the model.

model_df <- cv_df %>% 
  mutate(mods= map2(splits, 
                   poly,
                   ~lm(progesterone ~ poly(ventilation,.y),
                          data= analysis(.x)))) 

#C. Great! Now, calculate the rmse for each fold/polynomial combination as we did in lab.

model_df <- model_df %>% 
  mutate(rmse = map2_dbl(splits, 
                         mods,
                         ~rmse(model = .y,
                               data=assessment(.x)))) #something is wrong with "poly"... may need to save it differently in dplyr, above?

# D. Implications - ok, given that the 5-fold score is the average RMSE across all folds for a given 
# polynomial, show in both a table and figure the relationship between polynomial and out-of-sample RMSE. 
# What does this tell you?

#just code this out

# 3. Compare models and see how they differ from AIC ####

poly_list <- c(1,2,3,4,5)

poly_mods <- poly_list %>% 
  map(~glm(progesterone ~ poly(ventilation,.), data=progesterone_data, family = gaussian(link="identity")))

mod_list <- poly_mods
name_vec <- c("linear", "squared", "cubed", "4-order","5-order")

aictab(cand.set = mod_list, modnames = name_vec)

# EC 4 ####
?cv.glm()

# 5. Grid sample with Bayes ####

#a. Let’s start with the Palmer Penguins data. Let’s look at just the Gentoo. Why don’t you plot 
# the distribution of the average flipper length of females. We’ll use this data for the exercise. 
# Remember to remove NAs - it will make the rest of the exercise easier. 1 EC for each thing you 
# do to snaz the plot up.

#load penguin data
data(penguins)

gentoo_f <- penguins %>% 
  filter(species == "Gentoo", sex== "female") %>% 
  filter(flipper_length_mm>0, na.rm=TRUE)

ggplot(gentoo_f, aes(x=flipper_length_mm, fill=flipper_length_mm))+
  stat_halfeye(fill="lightblue1", slab_colour="black", slab_size=.5)+
  labs(x="Flipper length (mm)", y="Frequency", title="Distribution of flipper lengths for female Gentoo penguins")+
  theme_bw()
?stat_halfeye

#b grid sample
#get mean and SD of parameter
flipper_mean <- mean(gentoo_f$flipper_length_mm)
flipper_sd <- sd(gentoo_f$flipper_length_mm)

#function for likelihood
norm_lik <- function(m, s){
  dnorm(samp, mean = m, sd = s, log = FALSE) %>% prod()
}

#function for log-likelihood
norm_loglik <- function(m, s){
  dnorm(samp, mean = m, sd = s, log = TRUE) %>% sum()
}

#sample based on mean and sd
samp <- rnorm(20, mean=flipper_mean, sd=flipper_sd)

#data frame
lik_df <- crossing(mean = seq(212, 220, length.out = 100),
                           sd= seq(1, 5, length.out = 100)) %>%
  rowwise(mean) %>%
  mutate(lik = norm_lik(mean, sd),
         loglik = norm_loglik(mean, sd)) %>%
  ungroup()

#plot surface
ggplot(lik_df,
       aes(x = mean, y = lik)) +
  geom_point()

#get mle
intercept_mle <- lik_df %>% 
  filter(lik==max(lik)) %>% 
  summarize(mean)%>% 
  round(digits=2)

intercept_mle

#c. bayes grid sample - HELP

#p(m, s|flipper length)*p(m)*p(s)


lik_df <- crossing(mean = seq(212, 220, length.out = 100),
                   sd= seq(1, 5, length.out = 100))

bayes_fn <- function(m, s){
  posterior <- sum(dnorm(gentoo_f$flipper_length_mm, m, s, log=TRUE))+
    dnorm(m,210, 50, log=TRUE)+dunif(s,1,10, log=TRUE)
}

trial <- bayes_fn(m=210, s=50)

?geom_boxplot
#### 6

#use 3-d plotly for data viz!