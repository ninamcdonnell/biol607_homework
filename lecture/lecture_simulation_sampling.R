#'---------HEADER---------
#'@title Simularion of Sampling to answer...
#'HOW BIG SHOULD MY SAMPLE SIZE BE?!?
#'@author Nina McDonnell
#'@Date 9/22/20

#libraries for this code
library(dplyr)
library(purrr)

#Random numver generation ####
#draw from normal pop
rnorm(n=5, mean=3, sd=2)
hist(rnorm(1000, mean =3, sd=2))

#doing same/similar things with pipes...
rnorm(n=1000, mean=3, sd=2) %>% 
  hist()

rnorm(1000, mean=3, sd=2) %>% 
  density() %>% 
  plot()

#pseudo-randomness

#if using set seed to make simulation results replicable, try many different seeds to make sure they all produce the same results (or else, dont put them in the code at all). 
set.seed(607) 
rnorm(n=5, mean=5, sd=2)

#simulate flipping coin- binomial
#how many heads do we get if we flip an unbiased coin 10 times
rbinom(1, prob=0.5, size = 10) #one flip

#repeat the 10 flips 100 times (100 trials)
rbinom(100, prob=0.5, size = 10) #one flip

# equal probability of any number in a range
runif(50, min=-5, max=5)

#random whole numbers
runif(50, min=-5, max=5) %>% 
  round()

#assuming we have intuition about population, lets sample!

#'Steps:
#'Assume some pop parameters
#'Create a data frame with a variety of plausible sample sizes/properties
#'For each sample size (set of params)...
#'Replicate calculating estimated parameters from a random draw of some # of times

#our assumptions
mean_pop <- 45 #45 inches seems like about average fish size
sd_pop <- 15 #most fish you see fall between 30 and 60 inches

#Lets set up our sim
#What sample sizes do we need?

samp_sim <- data.frame(samp_size=3:50)

#NOW we need to do some simulations
#if we just wanted random draws...
samp_sim_one_replicate <- samp_sim %>% 
  rowwise(samp_size) %>% #group by row number
  summarize(samp=rnorm(samp_size, mean=mean_pop, sd=sd_pop))

head(samp_sim_one_replicate)
plot(samp~samp_size, data= samp_sim_one_replicate)

#this is great, but we want to estimate the mean at each sample size some huge number of times - 1000
samp_sim_means <- samp_sim %>% 
  rowwise(samp_size) %>% #group by row number
  summarize(samp_mean= replicate(1000,
              rnorm(samp_size, mean=mean_pop, sd=sd_pop) %>% 
              mean()))
      
head(samp_sim_means)
plot(samp_mean~samp_size, data=samp_sim_means)
 #WOW!!!


#Lets get simulated means and SD to exaimine sample size

#'Steps:
#'Assume some pop parameters
#'Create a data frame with a variety of plausible sample sizes/properties
#'For each sample size (set of params)...
#'Replicate calculating estimated parameters from a random draw of some # of times

#Step 1
mean_pop <- 45
sd_pop <- 15

#step 2
sim_results <- data.frame(samp_size=3:30) %>% 
  #for each samp size (set of params)...
  rowwise(samp_size) %>% 
  #replicate calculating estimates params from a random draw for some number of times
  summarize(samp_mean=replicate(100,
                                mean(rnorm(samp_size, mean_pop, sd_pop))),
            samp_sd= replicate (100,
                                mean(rnorm(samp_size, mean_pop, sd_pop))))
sim_results



# I replicated 100 simulations twice. Annoying!
# What if I only did it once, and made a DF for each sim
# Create a data frame with a variety of plausible sample sizes/properties
sim_results <- data.frame(samp_size = 3:30) %>%
  # For each sample size (set of params)...
  rowwise(samp_size) %>%
  
  # Replicate calculating estimated parameters 
  # from a random draw 
  # some # of times
  summarize(map_df(1:100,
                   ~data.frame(sim = .x,
                               samp_mean =  mean(rnorm(samp_size, mean_pop,sd_pop)),
                               samp_sd =  sd(rnorm(samp_size, mean_pop,sd_pop)))))

sim_results


#faded example 1. simulate

#Some preperatory material
set.seed(42)
mean_pop <- 10
sd_pop <- 3
nsim <- 100
sampSim <- data.frame(samp_size = 3:50)

#(place coursor on top line/object (ex. "sampSim") to see results)
sampSim %>% 
  rowwise(samp_size) %>% 
  summarize(samp_mean = 
              replicate(nsim,
                        rnorm(samp_size, mean_pop, sd_pop) %>% mean()))
#faded example 2. simulate median
samp_sim %>% 
  rowwise(samp_size) %>%
  summarize(samp_median = replicate(nsim, 
                                  rnorm(samp_size, mean_pop,sd_pop) %>% median()))

#faded example 3. iqr
samp_sim %>% 
  rowwise(samp_size) %>%
  summarize(samp_iqr = 
              replicate(nsim, 
                        rnorm(samp_size, mean_pop,sd_pop) %>% IQR()))
