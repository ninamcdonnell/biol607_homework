#'---------HEADER---------
#'@title Homework 2
#'@author Nina McDonnell
#'@Date 9/24/20

#------Question 1 ----------
#1a Load palmerpenguins data set
library(palmerpenguins)
data(penguins)

#1b Head data set
head(penguins)

#1c What do you learn from str() and summary()?
str(penguins)   #The data is a 344 row x8 column tibble including factor, numeric, and integer vectors. 
summary(penguins)    #Gives totals of counts for each species, island, and year (factors), and describes variance of numeric and integer values- such as beak and flipper measurements and mass. 

#1d What are quanitles for bill depth accross whole dataset? What do those mean?
quantile(penguins$bill_depth_mm, na.rm=TRUE) #(see output for quanitles).  This means that 100% of penguins have bill lengths between 13.1 and 21.5 mm. (details in markdown). Had to use na.rm=true to remove NA values.

#------Question 2 ----------

#2a What is mean of the below vec? NA will cause a problem, use ?mean to solve.
my_vec <- c(1,4,7,NA,9)
mean(my_vec)
?mean
mean(my_vec, na.rm = TRUE) #mean =5.25

#2b. What is the mean, sd, and median of body mass across the data set? 
#Note, these NAs are going to cause some problems, so you might need to look at the documentation for the relevant functions.

mean(penguins$body_mass_g, na.rm=TRUE) #4201.754
median(penguins$body_mass_g, na.rm=TRUE) #4050
sd(penguins$body_mass_g, na.rm=TRUE) #801.9545

#2c. Repeat 2b, but, show us how these quantities differ by species

library(dplyr)
library(magrittr)

penguins %>% 
  group_by(species) %>% 
  summarize(avg_mass_species=mean(body_mass_g, na.rm=TRUE))

penguins %>% 
  group_by(species) %>% 
  summarize(med_mass_species=median(body_mass_g, na.rm=TRUE))

penguins %>% 
  group_by(species) %>% 
  summarize(sd_mass_species=sd(body_mass_g, na.rm=TRUE))

#2d. Repeat 2c, but just for Biscoe island. What is different in the results?

penguins %>% 
  filter(island=="Biscoe") %>% 
  group_by(species) %>% 
  summarize(avg_mass_species=mean(body_mass_g, na.rm=TRUE))

penguins %>% 
  filter(island=="Biscoe") %>% 
  group_by(species) %>% 
  summarize(med_mass_species=median(body_mass_g, na.rm=TRUE))

penguins %>% 
  filter(island=="Biscoe") %>% 
  group_by(species) %>% 
  summarize(sd_mass_species=sd(body_mass_g, na.rm=TRUE))

#2e Make a species-island column in penguins using paste(). This is an awesome function that takes multiple strings, 
#and slams them together using the argument sep = to define how the string should be combined. Try out paste("Hello", 
#"Goodbye", sep = "! ") to see how it works. Have the two be separated by _.

paste("Hello", "Goodbye", sep ="!")

penguins %>% 
  mutate(species_island=paste(species, island, sep="_"))

#------Question 3 ----------

#3a. Show the distribution of flipper_length_mm by species and island using boxplots. 

penguins %>% 
  group_by(species) %>% 
  summarise(flipper_length <- flipper_length_mm)%>% 
  boxplot()

#E.C. For one point of extra credit, redo creating the species_island column with the sep as \n instead of _. What does \n do? You will find it very handy in the future.

penguins %>% 
  mutate(species_island=paste(species, island, sep="\n"))
View(penguins)
