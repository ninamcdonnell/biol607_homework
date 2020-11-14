#'-------------------------------
#' @title Biol 607 Midterm Exam
#' @author Nina McDonnell
#' @date 11/6/2020
#' ------------------------------

#libraries
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(brms)
library(visdat)
library(car) 
library(emmeans)
library(MASS)
library(profileModel)
library(bayesplot)
library(tidybayes)
library(ggdist)
library(AICcmodavg)


#Question 2a ####

#read in data
covid_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  dplyr::select(-Combined_Key,-Long_,-Lat,-Country_Region,-Admin2, -FIPS, -code3, -iso3, -iso2, -UID) 

state_pop_data <- read_tsv("https://raw.githubusercontent.com/thedatachest/us_population/master/statepop.tsv") %>% 
  filter(Year== max(Year)) #get data from most recent year

#what's here
str(covid_data)

str(state_pop_data)

#Question 2b ####

#data function 
state_data <- function(state){
  state_pop <- state_pop_data %>% 
    filter(State== state) %>% 
    summarize(Population)
  
  covid_long <- covid_data %>% 
    filter(Province_State== state) %>% 
    pivot_longer(cols= -Province_State,
                 names_to= "date",
                 values_to = "cumulative_cases") %>% 
    mutate(date=mdy(date)) %>% 
    group_by(date) %>% 
    summarize(cumulative_cases=sum(cumulative_cases)) %>% 
    mutate(daily_cases= as.numeric(cumulative_cases - lag(cumulative_cases, 1))) %>% 
    mutate(daily_cases_per_10_mil= (daily_cases*10000000)/state_pop$Population) %>% 
    filter(daily_cases >=0) %>% #remove errors below 0
    mutate(cum_cases_per_100000= (cumulative_cases*100000)/state_pop$Population)}

Mass_data <- state_data("Massachusetts")
max(Mass_data$cumulative_cases)

#Question 2c ####

Mass_data %>% 
  ggplot()+
  geom_area(mapping=aes(x=date, y=daily_cases, fill=daily_cases), fill= "green", alpha=.5, color="black")+
  geom_area(mapping=aes(x=date, y=cum_cases_per_100000, fill=cum_cases_per_100000), show.legend=TRUE, fill= "blue", alpha=.2, color="black")+
  labs(title="Massachusetts covid cases", 
       colour="type",
       x= "Date", 
       y="Cases", 
       subtitle="Daily cases (green) and cumulative cases per 100,000 residents (blue)")+
  theme_bw()

  
#Question 2d ####

#single state plot function
single_state_plot<- function(state){
  data <- state_data(state)
  plot <- data %>% 
    ggplot()+
    geom_area(data=data, mapping=aes(x=date, y=daily_cases), show.legend=TRUE, fill= "green", alpha=.5, color="black")+
    geom_area(data=data, mapping=aes(x=date, y=cum_cases_per_100000), fill= "blue", alpha=.2, color="black")+
    labs(title=state, 
         x= "Date", 
         y="Cases", 
         subtitle="State daily cases (green) and cumulative cases per 100,000 residents (blue)")+
    theme(legend.position = "right")
  return(plot)}

#single state plot
ND_plot <- single_state_plot("North Dakota")

#try to get legend into this... ------------
single_state_plot<- function(state){
  data <- state_data(state)
  plot <- data %>% 
    ggplot()+
    geom_area(data=data, mapping=aes(x=date, y=daily_cases), show.legend=TRUE, alpha=.5)+
    geom_area(data=data, mapping=aes(x=date, y=cum_cases_per_100000), alpha=.2)+
    labs(title=state, 
         x= "Date", 
         y="Cases", 
         subtitle="State daily cases (green) and cumulative cases per 100,000 residents (blue)")+
    theme(legend.position="bottom")+
    scale_color_manual(values=c(daily_cases="green", cum_cases_per_100000="red")) +  
    scale_fill_manual(values=c(daily_cases ="green", cum_cases_per_100000="red"), guide=TRUE)   
  return(plot)}

ND_plot <- single_state_plot("North Dakota")

#---------------

#to-date cumulative cases by state - for ordering multi-state plots
covid_states <- covid_data %>% 
  pivot_longer(cols= -Province_State,
               names_to= "date",
               values_to = "cumulative_cases") %>% 
  mutate(date=mdy(date)) %>% 
  group_by(Province_State,date) %>% 
  summarize(cumulative_cases=sum(cumulative_cases)) %>% 
  group_by(Province_State) %>% 
  summarize(cumulative_cases=max(cumulative_cases)) %>% 
  arrange(desc(cumulative_cases)) %>% #sort in descending order of cumulative cases 
  #remove states from covid data that are not in pop data- otherwise this causes a problem when they go through the state_data() function within state_plot()
  filter(Province_State != "American Samoa", Province_State!= "Diamond Princess", Province_State!= "Northern Mariana Islands", Province_State!= "Grand Princess", Province_State!= "Guam", Province_State!= "Puerto Rico", Province_State!= "Virgin Islands") %>% 
  ungroup()

#new df with pop data and case data merged
state_data_for_merge <- state_pop_data %>% 
  rename(Province_State=State) %>% 
  dplyr::select(!Year)

covid_states <- covid_states %>% 
  merge(state_data_for_merge, by="Province_State", sort=FALSE) %>% 
  mutate(cum_cases_per_100000= (cumulative_cases*100000)/Population)

#list states in descending order of number of cases
covid_list <- c(covid_states$Province_State[1:51])

#make a legend
legend_df <- tibble(x=c(1,2,3,4),
                    y=c(4,5,6,7),
                    z=c(a,a,b,b))
legend_df <- legend_df %>% 
  mutate(z=as.factor(z))

ggplot(legend_df, aes(x,y, color=z))+
  geom_point()+
  scale_color_manual(values = c("green", "blue"))+
  legend(z,title= "covid cases")

?scale_color_manual

values=c("green", "blue"))


#multi-state plot function - minimizes plot elements for better presentation
multi_state_plot<- function(state){
  data <- state_data(state)
  
  title <- paste(paste(state, ",", sep=""), max(data$cumulative_cases), "cases", sep = " ")
  
  plot <- data %>% 
    ggplot()+
    geom_area(data=data, mapping=aes(x=date, y=daily_cases_per_10_mil), fill= "green", alpha=.5, color="black")+
    geom_area(data=data, mapping=aes(x=date, y=cum_cases_per_100000), fill= "blue", alpha=.2, color="black")+
    labs(title=title, x=NULL, y=NULL)+
    scale_y_continuous(limits=c(0,max(covid_states$cum_cases_per_100000)))+ #uniform y-axis for states, based on state with most cumulative cases
    theme_bw(base_size = 5)
  return(plot)}

multi_state_plot("Massachusetts") #test on one state

#run state_plot() for each covid_list state value
covid_plot_list <- lapply(covid_list, multi_state_plot)

#all plots! Now in order of most cumulative cases to fewest cumulative cases!
do.call("grid.arrange", c(covid_plot_list, 
                          ncol=5, 
                          bottom="Daily COVID-19 cases per 1 million residents (green) and cumulative cases \n per 100,000 residents (blue), by state, in order of most total cases to fewest total cases."))

#total cumulative cases over time for all states
all_state_cum <- covid_data %>% 
  pivot_longer(cols= -Province_State,
               names_to= "date",
               values_to = "cumulative_cases") %>% 
  mutate(date=mdy(date)) %>% 
  group_by(Province_State,date) %>% 
  summarize(cumulative_cases=sum(cumulative_cases))

#plot cumulative cases for all states
total_cases <- all_state_cum %>% 
  ggplot(mapping=aes(x=date, y=cumulative_cases, group=Province_State, color=Province_State))+
  geom_line() #put labels next to each line later... maybe try geom_label() again


#Question 4 ####

# If S= sun explosion and R= result...

sun_prior <- 0.0001  #P(S)  the probability that the sun explodes
false_positive <- 0.027  #P(R|1-S)  the probability of the positive result given that the sun did  not explode
true_positive <- (1 - false_positive)  #P(R|S)  the probability of the positive result given that the sun did explode

# The probability of the sun exploding, given the result, is equal to the chance of 
# a true positive (P(R|S)) times the chance of the sun exploding (from the prior = 0.0001), 
# divided by the probability of any positive result-- including a false positive. 
# P(S|R) = (P(R|S)*sun_prior) / (P(R|S)*sun_prior + P(R|1-S)*(1-sun_prior))
# or P(S|R) = (P(R|S)*sun_prior) / (P(R|S)*sun_prior + P(R|1-S)*(1-sun_prior))

explode_prob <- (sun_prior*true_positive) / ((sun_prior*true_positive) + ((1-sun_prior)*false_positive))

# Question 5a ####

#read in data
quail_data <- read_csv("Documents/GitHub/biol607_mcdonnell/homework/homework_data/midterm_quail_data/Morphology data.csv") %>% 
  janitor::clean_names()

#what's here
str(quail_data)

visdat::vis_miss(quail_data) #NAs in tarsus and culmen

#remove data points with NA for tarsus or culmen
quail_data <- quail_data %>% 
  filter(!is.na(tarsus_mm),!is.na(culmen_mm))

visdat::vis_miss(quail_data) #no more NAs in columns of interest

#plot culmen ~ tarsus
ggplot(quail_data, mapping=aes(x=culmen_mm, y=tarsus_mm))+
  geom_point()+
  stat_smooth(method="lm", se=FALSE) #just to visualize linearity

?stat_smooth
#LEAST SQUARES - fit lm
quail_lm <- lm(culmen_mm~tarsus_mm, data=quail_data)
summary(quail_lm)

#check assumptions

#Equal variance and normality of errors:
plot(quail_lm, which=1) # may be a bit heteroscedastic 
plot(quail_lm, which=2) # strays a bit below the line at the bottom and above at the top- may not be linear, data could have long tails
hist(residuals(quail_lm)) # residuals look normal
shapiro.test(residuals(quail_lm)) # residuals technically fail normality test, but since we are not predicting things, 
# I'm not too worried about this

#Minimal outlier influence:
plot(quail_lm, which=4) # no values >1

#Model captures features in the data:
# simulate data and plot against real data - looks good!
quail_sims <- simulate(quail_lm, nsim= 30) %>% 
  pivot_longer(
    cols=everything(),
    names_to= "sim",
    values_to = "length.cm"
  )
ggplot()+
  geom_density(data=quail_sims,
               mapping=aes(x=length.cm, group=sim),
               size= 0.2)+
  geom_density(data= quail_sims,
               mapping=aes(x=length.cm),
               size=2, color="blue")


# LIKELIHOOD - glm

quail_glm <- glm(culmen_mm~tarsus_mm, 
                 data=quail_data, 
                 family = gaussian(link="identity"))
summary(quail_glm)

#check assumptions

#Equal variance and normality of errors:
plot(quail_glm, which=1) # no clear relationship
plot(quail_glm, which=2) # strays a bit, but mostly on the line
hist(residuals(quail_glm)) # residuals look normal
shapiro.test(residuals(quail_glm)) # residuals technically fail normality test, but since we are not predicting things, 
# I'm not too worried about this

#Minimal outlier influence:
plot(quail_glm, which=4) # no values >1

#Model captures features in the data:
# simulate data and plot against real data - looks good!
quail_sims <- simulate(quail_glm, nsim= 30) %>% 
  pivot_longer(
    cols=everything(),
    names_to= "sim",
    values_to = "length.cm"
  )
ggplot()+
  geom_density(data=quail_sims,
               mapping=aes(x=length.cm, group=sim),
               size= 0.2)+
  geom_density(data= quail_sims,
               mapping=aes(x=length.cm),
               size=2, color="blue")

#check profiles for intercept and tarsus_mm - they are nice parabolas centered around
#the values, so this looks good. 
prof <- profileModel(quail_glm,
                     objective= "ordinaryDeviance")

plot(prof, print.grid.points = TRUE)

#you can add a CI
prof_ci <- profileModel(quail_glm,
                        objective= "ordinaryDeviance",
                        quantile= qchisq(0.95, 1))
plot(prof_ci)

confint(quail_glm)

summary(quail_glm)

#brm - bayes
quail_brm <- brm(culmen_mm~tarsus_mm, 
                 data=quail_data, 
                 family = gaussian(link="identity"),
                 chains=3,
                 seed=802)
# check assumptions
color_scheme_set("viridis")

# check that chains are all aligned with one another for each estimate
plot(quail_brm) 

mcmc_trace(quail_brm)

summary(quail_brm)

#look at diagnostic of convergence

#Gelman_Rubin statistic (Rhat) is close to 1
rhat(quail_brm)
rhat(quail_brm) %>% mcmc_rhat() #in plot, we can see that all Rhats are left of the dashed line (good)

#assess autocorrelation
mcmc_acf(quail_brm) #It starts at 1 (totally correlated), and then drops to 0-- so, no autocorrelation!

# check our MODEL assumptions ####

#check the match between out data and our chains for dist of y
pp_check(quail_brm, "dens_overlay") #does not quite fit...

#is our error normal?
pp_check(quail_brm, "error_hist", bins=10) #looks about normal 

#equal variance of errors - see fitted vs residuals:
quail_res <- residuals(quail_brm) %>%  #first get residuals
  as_tibble #make df

quail_fit <- fitted(quail_brm) %>% #then get fitted values
  as_tibble

plot(y=quail_res$Estimate, x=quail_fit$Estimate) # no clear relationship

summary(quail_brm)




quails_data_function <- function(slope, intercept, resid_sd){
  preds_fit_line <- intercept + (slope * quail_data$tarsus_mm)
  sum <- sum(dnorm(quail_data$culmen_mm, preds_fit_line, resid_sd, log = TRUE))
  return(sum)}

quails_data_function(slope=0.5, intercept=0.7, resid_sd=.7)

quails_data_grid <- crossing(s = seq(0, 0.5, 0.001), i = seq(-0.098), sd = seq(0.1, 2, 0.1))

quails_data_grid <- quails_data_grid %>% 
  rowwise() %>%
  mutate(quails_data_function (slope = s, intercept = i, resid_sd = sd)) %>%
  ungroup() 

summary(quail_lm)

summary(quail_glm)

summary(quail_brm)

#5c ####

plot(prof, print.grid.points = TRUE) #good parabola

#tau
prof_quail <- profile(quail_glm)
plot(prof_quail) #good straight line

#confidence interval - slope for tarsus does not cross 0!
confint(prof_quail)

#tarsus slope estimate= 0.37293
#tarsus slope standard error = 0.006646
quail_n <- length(quail_data)
quail_se <- 0.006646

#find tarsus slope estimate SD for making the surface:
quail_sd <- quail_n*quail_se #= 0.06646

#get random sample with mean and SD of slope estimate for lik surface
samp <- rnorm(20,mean=0.37293, sd=0.06646)

#function for likelihood
norm_lik <- function(m, s){
  dnorm(samp, mean = m, sd = s, log = FALSE) %>% prod()
}

#function for log-likelihood
norm_loglik <- function(m, s){
  dnorm(samp, mean = m, sd = s, log = TRUE) %>% sum()
}

lik_df_norm <- crossing(m=seq(0,1, length.out=300),
                        s=seq(0,.35, length.out=300)) %>% 
  group_by(m,s) %>% 
  mutate(lik=norm_lik(m,s),
         loglik= norm_loglik(m,s),
         deviance=-2*loglik) %>% 
  ungroup()

#visualize

ggplot(lik_df_norm %>% filter(loglik>max(loglik)-5),
       aes(x=m, y=s, z=loglik))+
  geom_contour_filled(bins=20)

#MLE of parameters

lik_df_norm %>% filter(deviance == min(deviance))

#get slice across values of m (profile for m)
like_prof_m <- lik_df_norm %>% 
  group_by(m) %>% 
  filter(deviance == min(deviance)) %>% 
  ungroup()

#get slice across values of s (profile for s)
like_prof_s <- lik_df_norm %>% 
  group_by(s) %>% 
  filter(deviance == min(deviance)) %>% 
  ungroup()

#plot profiles
ggplot(like_prof_m %>% filter(loglik>max(loglik)-5),
       aes(x=m, y=loglik))+
  geom_point()

ggplot(like_prof_s %>% filter(loglik>max(loglik)-5),
       aes(x=s, y=loglik))+
  geom_point()

#95% CI for mean

CI_data_frame_95 <- lik_df_norm %>% 
  filter(loglik>=max(loglik) - qchisq(0.95, df=1)/2) %>% 
  as.data.frame()

View(CI_data_frame_95) #95% CI= 0.3678930 - 0.4247492


#80% CI for mean

CI_data_frame_80 <- lik_df_norm %>% 
  filter(loglik>=max(loglik) - qchisq(0.8, df=1)/2) %>% 
  as.data.frame()

View(CI_data_frame_80) #80% CI= 0.3511706 - 0.3812709

#check using profileModel()
prof_ci <- profileModel(quail_glm,
                        objective= "ordinaryDeviance",
                        quantile= qchisq(0.95, 1))
plot(prof_ci)

#5d ####

length(quail_data$tarsus_mm) #confirm: yes, I have 766 lines of data

#quail brm
quail_brm

#show that prior with m=0.7, sd= 0.01 is overwhelmed

quail_brm_prior <- brm(culmen_mm~tarsus_mm, 
                 data=quail_data, 
                 family = gaussian(link="identity"),
                 chains=3,
                 prior=c(prior(coef="tarsus_mm",
                               prior=normal(0.7, 0.01))),
                 seed=802)

plot(quail_brm_prior)
plot(quail_brm)

fixef(quail_brm_prior)
fixef(quail_brm) #the prior is overwhelmed by the data (outside of 95% CI) such that the posterior estimate for the model with the
#prior is 0.5040553, as opposed to the estimate of 0.3730030 from the model with a flat prior

#randomly sample 10, 100, 300, and 500 points from the data set- at what point does the prior become overwhelmed (prior slope becomes highly unlikely)?
#At ~100 data points!

#make function to sample x values from quail_data and then fit a bayes model
quail_subsample_bayes <- function(number){
  quail_data_n <- slice_sample(.data=quail_data, n=number, replace=TRUE)
  quail_brm_subsample <- brm(culmen_mm~tarsus_mm, 
                         data=quail_data_n, 
                         family = gaussian(link="identity"),
                         chains=3,
                         prior=c(prior(coef="tarsus_mm",
                                       prior=normal(0.7, 0.01))),
                         seed=802)
  return(quail_brm_subsample)}

#for 10 random values...
quail_subsample_10 <- quail_subsample_bayes(10)
plot(quail_subsample_10)
fixef(quail_subsample_10) #tarsus estimate = 0.6981741, 95% CI includes the prior (not overwhelmed)

#for 100 random values...
quail_subsample_100<- quail_subsample_bayes(100)
plot(quail_subsample_100)
fixef(quail_subsample_100) #tarsus estimate = 0.6757016, prior value of 0.7 is at very edge of 95% CI (getting overwhelmed)

#for 300 random values...
quail_subsample_300<- quail_subsample_bayes(300)
plot(quail_subsample_300)
fixef(quail_subsample_300) #tarsus estimate = 0.6286125, prior value of 0.7 is outside of 95% CI (now overwhelmed)

#for 500 random values...
quail_subsample_500<- quail_subsample_bayes(500)
plot(quail_subsample_500)
fixef(quail_subsample_500) #tarsus estimate = 0.5668878, prior value of 0.7 is well outside of 95% CI (overwhelmed)

#communicate this visually, explain choices
#(use gather_draws@vid 120)

quail_subsample_10_draws <- quail_subsample_10 %>% 
  gather_draws(b_tarsus_mm)

quail_subsample_100_draws <- quail_subsample_100 %>% 
  gather_draws(b_tarsus_mm)

quail_subsample_300_draws <- quail_subsample_300 %>% 
  gather_draws(b_tarsus_mm)

quail_subsample_500_draws <- quail_subsample_500 %>% 
  gather_draws(b_tarsus_mm)

# plot posteriors and prior!

plot_10 <- ggplot(quail_subsample_10_draws, 
       aes(x=.value))+
  stat_halfeye(.width=c(0.9, 0.95), fill="sky blue", alpha=0.5)+
  labs(title="10 data points", x="tarsus_mm estimate")+
  geom_vline(xintercept=0.7, color="red", size=1, linetype= "dotted")+
  theme_bw()

plot_100 <- ggplot(quail_subsample_100_draws, 
                  aes(x=.value))+
  stat_halfeye(.width=c(0.9, 0.95), fill="sky blue", alpha=0.5)+
  labs(title="100 data points", x="tarsus_mm estimate")+
  geom_vline(xintercept=0.7, color="red", size=1, linetype= "dotted")+
  theme_bw()

plot_300 <- ggplot(quail_subsample_300_draws, 
                   aes(x=.value))+
  stat_halfeye(.width=c(0.9, 0.95), fill="sky blue", alpha=0.5)+
  labs(title="300 data points", x="tarsus_mm estimate")+
  geom_vline(xintercept=0.7, color="red", size=1, linetype= "dotted")+
  theme_bw()

plot_500 <- ggplot(quail_subsample_500_draws, 
                   aes(x=.value))+
  stat_halfeye(.width=c(0.9, 0.95), fill="sky blue", alpha=0.5)+
  labs(title="500 data points", x="tarsus_mm estimate")+
  geom_vline(xintercept=0.7, color="red", size=1, linetype= "dotted")+
  theme_bw()

grid.arrange(plot_10,plot_100, plot_300, plot_500)

# Question 6 ####

#fit linear, squared, cubic, and exponential models of culmen~tarsus
linear_quail <- glm(culmen_mm~tarsus_mm, 
                    data= quail_data, 
                    family = gaussian(link="identity"))
                    
squared_quail <- glm(culmen_mm~poly(tarsus_mm,2), 
                    data= quail_data, 
                    family = gaussian(link="identity"))

cubed_quail <- glm(culmen_mm~poly(tarsus_mm,3), 
                     data= quail_data, 
                     family = gaussian(link="identity"))

exponential_quail <- glm(culmen_mm~tarsus_mm, 
                      data= quail_data, 
                      family = Gamma(link="identity"))

#compare predictive ability of models with AIC
mod_list <- list(linear_quail,squared_quail,cubed_quail, exponential_quail)
name_vec <- c("linear", "squared", "cubed", "exponential")

aictab(cand.set = mod_list, modnames = name_vec) #clear winner is the exponential model

?str
  