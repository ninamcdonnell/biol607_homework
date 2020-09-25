#'---------HEADER---------
#'@title Simulation, Dplyr, and All that
#'@author Nina McDonnell
#'@Date 9/18/20
#'
#'---------PIPES---------------
#pipies

vec<-1:10

#we want log of sqrt of length.  We could do this with many cluttery objects:
len <- length(vec)
sq <- sqrt(len)
log(sq)

#Or ugly nested functions:
log(sqrt(length(vec)))

#neater, but still a hot mess:

log(
  sqrt(
    length(vec)
  )
)

#what if we could do something with plain language? PIPIES! 
#You will need to load library "magrittr" for pipies

library(magrittr)

#pipe for summation. Says "i want to pass 1:10 through function sum()
1:10 %>% sum()

#pipe shorthand is cmd+shift+m on mac
#we want vec of 1:10:
1:10 %>%
  #take its length,
  length() %>%
  #take sqrt length,
  sqrt()  %>%
  #take the log
  log()

#Let’s try a few exercises with pipes.
#1. Use pipes to sum the log of 100:200.
#2. Use pipes to take the square root of the mean of 100 random uniform numbers.
#3. Let’s dive into the guts of R. Using the mtcars data frame, get it’s summary and str that summary. What do you get back?

100:200 %>%
  log()%>%
  sum()

runif(100)%>%
  mean()%>%
  sqrt()

data(mtcars)%>%
  summary(mtcars)%>%
  str(mtcars)

#----------------BASE PLOT-----------
vals <- runif(n=1000, min=-10, max=10)

#histogram
hist(vals, xlim= c(-20, 20))

#scatter plot
my_df <- data.frame(x=1:10, y=1:10)
plot(y~x, data=my_df)

#--------DPLYR---------------
#used for data manipulation and aggregation

library(dplyr)

#mutate- we add a new column to the data!
mtcars2 <- mutate(mtcars, logmpg = log(mpg))
head(mtcars2)

#incorporate pipes!
mtcars2 <- mtcars %>%
  mutate(mtcars, logmpg = log(mpg),
         sqrt_cyl = sqrt(cyl))
head(mtcars2)

#group_by
# I want to add a column that has the average
# mpg for each # of grears...

mtcars_group <- mtcars %>%
  group_by(gear)%>%
  mutate(avg_mpg=mean(mpg))

#^ made a tibble instead of plain data frame- can include additional properties, such as groups!

#to remove groups:
#ALWAYS UNGROUP AFTER YOU MUTATE!!
mtcars_group <- mtcars %>%
  group_by(gear)%>%
  mutate(avg_mpg=mean(mpg))%>%
  ungroup()

View(mtcars_group)

#summarize
#we want to create a defived data set
#lets say we want the avg and sd of mpg by gear and ONLY THAT

mtcars_summary <- mtcars %>%
  group_by(gear)%>%
  summarize(avg_mpg=mean(mpg),
            sd_mpg= sd(mpg))
mtcars_summary
#output is 3x3 table. Summarize ungrouped for us (see console message), but to be safe you could still manually ungroup.

#filter
#remove all data where #cylinders=4
unique(mtcars$cyl) #we start with values of cyl=4

#apply filter...
mtcars_filter <- mtcars %>% 
  filter(cyl !=4) #this is a boolean, so == for true, not just =

unique(mtcars_filter$cyl)
#we can see that there are no longer values of cyl= 4

#the ! and NOT
TRUE
FALSE 
!TRUE #means not true (false)
!FALSE

#select
#only choose certain columns

mtcars %>% 
  select(mpg) %>% 
  head()
#returns ONLY the mpg column

#How about everthing but mpg? Use (- OR !) to get rid of column
mtcars %>% 
  select(!mpg) %>% 
  head()

#Just a few columns
mtcars %>% 
  select(gear, carb, disp) %>% 
  head()

#all columns with an "m" in them...
mtcars %>% 
  select(contains("m")) %>% 
  head()

#you can also reorder columns!

# DPLYR Exercises
#1. Add some columns to mtcars to plot the log of mpg by the 
#square root of hp.

#2. Get the average hp per gear and plot them against each other.

#3. Make a data fame for only 6 cylinder engines with only the 
#disp and carb columns. Create a boxplot of how carb influences disp.
# boxplot()

#1
head(mtcars)
mtcars_logmpg <- mtcars %>%
  mutate(logmpg=log(mpg))%>%
  mutate(sqrthp=sqrt(hp))%>%
  head()

plot(logmpg~sqrthp, data=mtcars_logmpg)

#2
mtcars_hpgear <- mtcars %>%
  group_by(gear)%>%
  summarize(avg_hp=mean(hp))

plot(avg_hp~gear, data=mtcars_hpgear)

#3
mtcars %>% 
  select(cyl, carb, disp) %>% 
  filter(cyl==6)%>% 
  head() %>% 
  boxplot(disp~carb, data=.)

#'---------SIMULATIONS--------
#'Now we're entering a simulated world:
#'Computers are great at doing simulations
#'Functions to do this:
#Base::replicate (the :: means that function comes from that package, packagea

replicate(n=10, sum(1:100))

#purrr:map_dbl (dbl means double)
library(purrr)

map_dbl(1:10, ~sum(1:100)) #needs tilda for complex function. Gives same as the replicate

map_dbl(1:10, ~sum(1:.x)) #1:10 is split into 10 pieces, each called ".x". ".x" is a stand in for each number in the 1:10 sequence
#^it is summing (1:1), then (1:2), then (1:3), etc...

map_df(1:10, ~data.frame(x=.x,
                         y=sum(1:.x)))  #map dataframe

#couple with random number generation to make simulations!!

#Exercises:
#Use replicate() to repeatedly average the numbers 1:10 seven times.
replicate(n=7, mean(1:10))

#Do the same thing with map_dbl() - also what happens if you use other map functions?
map_dbl(1:7, ~mean(1:10))

# Start with a vector:
my_vec <- c(11, 10, 10, 9, 10, 11, 10, 9, 10, 
            12, 9, 11, 8, 11, 11, 10, 11, 10, 
            11, 9)
#Use map_df() to make a data frame that, for the numbers 3 through 15, returns two columns. One is the the average 
#of the element of the vector 1 through the chosen number, the second is the standard deviation.
#e.g. 10.2 for a mean - but the 10 will be .x for you!
map_df(3:15, ~data.frame(m = mean(my_vec[1:.x]),
                         s = sd (my_vec[1:.x])))
