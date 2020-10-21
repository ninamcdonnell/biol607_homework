#'----------------
#'@title Wide and long data with tidyr
#'@date 2020-10-02
#----------------

library(tidyr)
library(dplyr)
library(ggplot2)


#Reshaping wide and long data ####

#Some data for the tidyr lecture
mammals <- data.frame(site = c(1,1,2,3,3,3), 
                      taxon = c('Suncus etruscus', 'Sorex cinereus', 
                                'Myotis nigricans', 'Notiosorex crawfordi', 
                                'Suncus etruscus', 'Myotis nigricans'),
                      density = c(6.2, 5.2, 11.0, 1.2, 9.4, 9.6))

mammals #data set is long

#what if we want wide data set of mammals? With longm I cant plot density of one species against another
m_wide <- pivot_wider(mammals,  #you dont need quotes because its a tidyverse function
                      names_from= taxon,
                      values_from = density)
m_wide #Lots of NA values because some speies werent present at sites

#or with dplyr...

m_wide <- mammals %>% 
  pivot_wider(names_from= taxon,
                      values_from = density)

m_wide

#what do we have?
m_wide
visdat::vis_dat(m_wide)

#filling in NAs with a value (0)
m_wide_0 <- mammals %>% 
  pivot_wider(names_from= taxon,
               values_from = density,
              values_fill = list(density= 0))
m_wide_0 #now the missing species are 0!

#treat new data frame with as much suspission as original one 
#what if oops! qw had already pivoted and wanted to move forward
#fill in those 0s without pivoting

m_wide_0_nopivot <- mammals %>% 
  complete(site, taxon,
           fill = list(density= 0))

m_wide_0_nopivot

#pivoting long

m_long_0 <- m_wide_0 %>% #now we need to specify columns we want to work with.  We could list all the columns we want, or all the columns we dont want (often more helpful)
  pivot_longer(cols=-site,#all cols EXCEPT site
               names_to= "taxon",
               values_to = "density")


m_long_0 <- m_wide_0 %>% #now we need to specify columns we want to work with.  We could list all the comuns we want, or all the columns we dont want (often more helpful)
  pivot_longer(cols=`Suncus etruscus`: `Notiosorex crawfordi`,
               names_to= "taxon",
               values_to = "density")


ggplot(m_wide_0,
       aes(x=`Suncus etruscus`, y=`Notiosorex crawfordi`))+
  geom_jitter()

ggplot(m_long_0,
       aes(x=taxon, y=density))+
  geom_violin()+
  stat_summary(color="red")

# Exercise
# Using palmer penguins, which is wide - make a long
# data frame so that we can see the average of all measurements
# split up by year, species, island, and sex (faceting, colors, etc.)
# get rid year if you want

library(palmerpenguins)
penguins

penguins_long <- penguins %>% 
  pivot_longer(cols=bill_length_mm:body_mass_g,
               names_to= "measurement",
               values_to="value")

ggplot(data=penguins_long,
       mapping= aes(x= species,
                    y= value,
                    fill= sex))+
  geom_boxplot()+
  facet_wrap(~measurement, scales = "free_y") #to accound for variavles with very different scales!

