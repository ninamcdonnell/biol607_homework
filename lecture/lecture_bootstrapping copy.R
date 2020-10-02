#'---------HEADER---------
#'@title Simularion of Sampling to answer...
#'HOW BIG SHOULD MY SAMPLE SIZE BE?!?
#'@author Nina McDonnell
#'@Date 9/24/20

#libraries for this code
library(dplyr)
library(purrr)

#'Bootstrapping is based on repeated draws from a SMAPLE
#'Not the same as simulation sampling based on POPULATION parameters

#Create the sample we are going to work with ####
set.seed(2020)
samp <- rnorm(41, mean=10, sd=3)

#one bootstrap sample ####

#a bootstrap sa,ple is a resamples set of values
#with the same n, sampled with replacement

one_boot <- sample(samp,
                   size= length(samp),
                   replace= TRUE)

#show that they are drawn from each other:
one_boot %in% samp


#boostrapped medians! ####

#1000 times, sample from samp and calculate median
boot_med <- replicate(1000,
                      sample(samp, 
                             size=length(samp),
                             replace= TRUE) %>% median)
length(boot_med) #you can see that it is  1000 long

#Let's look at it
hist(boot_med)

#the SE of the median is the SD of these bootstrapps!
sd(boot_med)

#the 2/3 condifence interval is 
mean(boot_med) + sd(boot_med)
mean(boot_med) - sd(boot_med)

median(samp)
mean(boot_med)

#SE of the mean ####
boot_mean <- replicate(1000,
                      sample(samp, 
                             size=length(samp),
                             replace= TRUE) %>% mean())

#compare SE
sd(boot_mean)
sd(samp)/sqrt(length(samp))

#the 2/3 condifence interval is 
mean(boot_mean) + sd(boot_mean)
mean(boot_mean) - sd(boot_mean)

median(samp)
mean(boot_med)

#pair programming exercise####

samp_iqr <- replicate(1000,
                      sample(samp,
                             size=length(samp),
                             replace=TRUE) %>% IQR())

mean(samp)
sd(samp)

#we will make 2 columns- one for sample size and one for IQRs
#'Here's the plan:
#'start with a data frame that has a column for sample sizes and a column for IQRs calculated from sampling our vector
#'for each sample size, replicate 1000 times
#'bootstrap draws
#'and ger an IQR
#'and now I have 1000 bootstraped IQRs and different sample sizes
  #sample mean was 10
  #sample sd was 3

boot_iqrs <- data.frame(samp_size=10:20) %>% 
  rowwise(samp_size) %>% 
  summarize(boot_iqr=replicate(1000,
                               sample(samp,
                                      size=samp_size, 
                                      replace=TRUE %>% 
                                        IQR)))

boot_se_iqrs <- boot_iqrs %>% 
  group_by(samp_size) %>% 
  summarize(se_iqr = sd(boot_iqr))

plot(se_iqr ~ samp_size, data=boot_se_iqrs,
     type= "l")
