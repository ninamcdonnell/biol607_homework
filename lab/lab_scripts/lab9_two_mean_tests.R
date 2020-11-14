#'------------------------
#'T=tests as a linear model
#'@date 11-6-2020
#'------------------------

#Paired data
#data where we have paired samples
#(two plots next to each other, two measurements before and after intervention, etc.)

#libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) #companion to applied regression - by john fox
library(emmeans) #expected means

blackbird <- read_csv("lab/lab_data/data_lab9/12e2BlackbirdTestosterone.csv")
str(blackbird)
#we are interested in difference before and afrer testosterone, called "dif"

ggplot(blackbird,
       aes(x=dif))+
  geom_histogram(bins=8) #distribution does not look normal, but lets make a model and check the diagnostics

# v v v not sure what this was v v v 
ggplot(blackbird,
       aes(x=`dif in logs`))+
  geom_histogram(bins=8)

#get data viz from class code to see more claerly what the dif means

#fit a lm
blackbird_mod <- lm(dif~1, data=blackbird) #intercept-only model to see if difference intercept crosses 0 (is there a difference?)

#assumptions
plot(blackbird_mod, which=1) #slight skew, but doesnt look too bad
plot(blackbird_mod, which=2) #hmm... doesnt quite line up but not terrible
shapiro.test(residuals(blackbird_mod)) #passes

#are my means different
summary(blackbird_mod) #we fail to reject the null hypothesis

library(brms)
blackbird_brm <-  brm(dif~1,
                    data=blackbird,
                    family=gaussian(link="identity"),
                    chains=1)

fixef(blackbird_brm, probs=c(0.025, 0.1, 0.9, 0.975))
plot(blackbird_brm)

blackbird_post <- as.data.frame(blackbird_brm)

#how much is greater than 0?
sum(blackbird_post$b_Intercept>0)/nrow(blackbird_post)

#the more classic approach that doesnt show the model is...
t.test(blackbird$dif)

#Comparing two separate means ####

salmon <- read_csv("lab/lab_data/data_lab9/12e4BrookTrout.csv") %>% 
  janitor::clean_names()

str(salmon)

#we are going to look at "brook_trout" and "mean_chinook_survival"

ggplot(salmon,
       aes(x=brook_trout,
           y=mean_chinook_survival))+
  geom_boxplot()

#fit a model with dummy encoded categorical variables
#OR -   one hot encoding!
#y=b0 +b1*trout+e
#b0= mean of whatever group is 0
#b1= difference between groups

salmon_mod <- lm(mean_chinook_survival~brook_trout,
                 data=salmon)

#what's happening inside
model.matrix(mean_chinook_survival~brook_trout,
             data=salmon)
# coefficient matrix! It's turned brook_trout into 1s and 0s to model yes/no for presence

# this way we can make separate columns for the yes/no brook_trout:
model.matrix(mean_chinook_survival~brook_trout-1,
             data=salmon)

#evaluate model
summary(salmon_mod)#p-val for brook_trout+ says that there is a difference in streams with brook_trout (check interepetation with class video- i missed a minute)

#CHECK ASSUMPTIONS!

plot(salmon_mod, which=1) #oh dear... variance is quite different bewtween groups

leveneTest(salmon_mod) #confirms that there is a problematic difference in variance between groups

#non-parametric test! we analyze ranks
salmon_rank <- lm(rank(mean_chinook_survival)~brook_trout,
                       data=salmon)
summary(salmon_rank)

#----come back to this once Jarrett figures it out----

library(nlme)
salmon_gls <- gls(mean_chinook_survival~brook_trout,
                  data=salmon,
                  weights=varIdent(form=~1|brook_trout)) #it actually looks pretty similar to before

library(glmmTMB)

salmon_tmb <- glmmTMB(mean_chinook_survival~brook_trout,
                      dispformula =~brook_trout,
                      data=salmon)
#-----------------------------------------------------

#Same thing, bayes style!

salmon_brm_mod <- brmsformula(mean_chinook_survival~brook_trout,
                              sigma~brook_trout) #SD as a function of brook_trout! You lose degrees of freedom, but it allows you to compare the groups with unequal variance
salmon_brm <- brm(salmon_brm_mod, data=salmon)

fixef(salmon_brm)
plot(salmon_brm)

#Welch's t-test
t.test(mean_chinook_survival~brook_trout,
       data=salmon,
       unequal.var= TRUE)

#what are our means?
#y=b1*no trout+b2*trout+e
#b1= mean of no trout, b2=mean od trout
#with emmeans
salmon_em <- emmeans(salmon_brm ~brook_trout)
emmeans(salmon_mod, ~brook_trout)

salmon_em
plot(salmon_em)