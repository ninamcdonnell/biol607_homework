#'---------HEADER---------
#'@title Liklihood!
#'
#'@date 2020-10-16
#'------------------------
#libraries ####
library(dplyr)
library(ggplot2)

#likelihood of a single data point ####

#pop is gaussian w/ mean of 10 and sd of 3

#we have single data point:
x <- 10

#our liklihood for any given hypothesis = density of our data given hypothesis
# p(D|H)

?dnorm

#digression: to plot normal curve
norm <- data.frame (x=seq(-3,3, length.out=100)) %>% 
  mutate(p=dnorm(x, mean=0, sd=1))

qplot(x, p, data=norm, geom="line")

#something with x isnt working- replace this section with class code and try again
#if we want the likelihood of a hypothesized mean of 15 and sd of 3
dnorm(x, mean=15, sd=3)

#so to ger a max like estimate, we need to test a lot of possible means

lik_vec <- dnorm(x,
                 mean=seq(8, 12, length.out=100),
                 sd=3)

lik_df <- data.frame (mean=seq(6, 12, length.out=100)) %>% 
  mutate(lik=dnorm(x, mean=mean, sd=3))

ggplot(lik_df,
       aes(lik=mean, y=lik))+
  geom_point()


#likelihood with multiple data points ####
#our pop has mean od 15 and sd of 3

set.seed(607)
samp <- rnorm(20, mean=15, sd=3)

norm_lik <- function(m, s = 3){ #set it to 3 by default to see just one patameter
  dnorm(samp, mean=m, sd=s, log=FALSE) %>% prod() #jarrett discribed this well in lab video
}

norm_lik(4)

#or loglike (use sum)() at the end instead of prod() because its for logs)
norm_loglik <- function(m, s = 3){ #set it to 3 by default to see just one patameter
  dnorm(samp, mean=m, sd=s, log=TRUE) %>% sum() #jarrett discribed this well in lab video
}

norm_loglik(4)

#lets ger out likelihood surface
lik_df_mult <- tibble(mean=seq(10, 20, length.out = 100)) %>% 
  rowwise(mean) %>% 
  mutate(lik=norm_lik(mean),
         loglik = norm_loglik(mean)) %>% 
  ungroup()

#likelihood surface
ggplot(lik_df_mult,
       aes(x=mean, y=lik))+
  geom_point()

#loglik surface- much smoother
ggplot(lik_df_mult,
       aes(x=mean, y=loglik))+
  geom_point()

#get out max like estimate (MLE)
lik_df_mult %>% filter(loglik == max(loglik))

#95% CI - that the points are 1.92 away from the MLEs loglik
#we want quantile od chisq divided by 2 so we get both tails
qchisq(.95, df=1)/2
#or 68% CI...
qchisq(.68, df=1)/2

lik_df_mult %>% 
  filter(loglik >=max(loglik)- qchisq(.95, df=1)/2) %>% 
  as.data.frame()

#two-dimensional likelihood surfaces ####

#remember, our pop has a mean of 10 and a sd of 3
#what if we need to estimate both the mean and the SD

#crossing to make a grid
crossing(1:3, 4:6, 7:9)

lik_df_norm <- crossing(m=seq(10, 20, length.out=100),
                        s=seq(1,5, length.out = 100))
#the problem with this grid sampling ^ is that you get a LOT of rows really fast


lik_df_norm <- crossing(m=seq(10, 20, length.out=100),
                        s=seq(1,5, length.out = 100)) %>% 
  group_by(m, s) %>% 
  mutate(lik=norm_lik(m,s),
         deviance= -2*loglik) %>% 
  ungroup()

#ADD EVERYTHING I SKIPPED HERE


#Regression with likelihood ####

seals <- read.csv("data/17e8ShrinkingSeals Trites 1996.csv")

#secret, you can do *most* of this with lm
seal_lm <- lm(length.cm ~ age.days, data = seals)
logLik(seal_lm)

#but, we will do most of this with glm

seal_mle <- glm(length.cm ~ age.days, 
                data = seals,
                family= gaussian(link="identity"))
#assumptions
plot(seal_mle, which = 1)
plot(seal_mle, which = 2)
hist(residuals(seal_mle))

#the new thing- make sure our profiles are well behaved!

library(MASS)
library(profileModel)

prof <- profileModel(seal_mle,
                     objective= "ordinaryDeviance")

plot(prof) #you want nice parabolas!

#to see which points were sampled:
plot(prof, print.grid.points = TRUE)

#you can add a CI
prof_ci <- profileModel(seal_mle,
                     objective= "ordinaryDeviance",
                     quantile= qchisq(0.95, 1))
plot(prof_ci)

#----
#lets do this with mass

#tau = square root of deviance- makes straight line into parabola. easier to see deviations from a strait line
sqrt(4)
-sqrt(4)
prof_mass <- profile(seal_mle)
plot(prof_mass)

prof_mass
confint(prof_mass)