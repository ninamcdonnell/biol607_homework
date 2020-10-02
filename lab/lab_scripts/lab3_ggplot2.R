#'---------HEADER---------
#'@title Lab 3: ggplot2 intro
#'@author Nina McDonnell
#'@Date 9/24/20
#'
#'@description: This is my first foray into ggplot2!
#'
-------------------

#libraries 
library(ggplot2)
library(dplyr)
library(palmerpenguins)

#---------Look at distribution of bill length----------
  
head(penguins)

#properties of a figure to map to:
#x, y, color, size, shape, alphs...

----
#density plot of bill lengths
bill_dens <-ggplot(data=penguins, 
                   mapping= aes(x= bill_length_mm))
bill_dens #there is nothing displayed in the figure except for the axis because we have not specified how we want it represented

bill_dens + 
  geom_histogram(bins=40)  #+ adds layers of information. now there are 40 bins

bill_dens +
  geom_freqpoly() #now we cen add geom density

#---------Look at distribution of bill length by species----------

bill_dens_species <-ggplot(data=penguins, 
                   mapping= aes(x= bill_length_mm,
                                group=species))
bill_dens_species +
  geom_density()

#lets add some fill colors...

bill_dens_species +
  geom_density(mapping=aes(fill=species))

#lets make all of these density plots a little transparent...
#we are going to specify something about an aesthetic, but NOT have it mapped to the data

#PROPPERTIES OF GEOM THAT ARE NOT MAPPED TO DATA SO NOT GO INTO AES!!!!!

bill_dens_species +
  geom_density(mapping=aes(fill=species), #maps to data
               alpha = 0.3) #does not map to data- its fixed (alpha channel)


bill_dens_species +
  geom_density(mapping=aes(fill=species), #maps to data
               position = "stack") #does not map to data- its fixed (alpha channel)
   #now they are overlayed on one another! Jarrett says something about this relating to microbial OTU data...?

# Exercise: Now you try geom_histogram. How’s it look? 
# Bad, right? Try different colors, alphas, and fills 
# to see if you can improve. Maybe a different position?
# What works best?

bill_dens_species +
  geom_histogram(mapping=aes(fill=species), #figure out why this isnt working...
               show.legend = TRUE,
               alpha= 0.5,
               boundary= TRUE,
               binwidth= 1,
               position = "identity") +
               xlab("Bill length (mm)")

pen_plot_base <- ggplot(data=penguins,
                        mapping = aes(y=body_mass_g,
                                      x= species,
                                      color= species))

pen_plot_base

pen_plot_base+
  geom_poitn(size=3,
             alpha=0.3)

#geom_jitter - move the points around
pen_plot_base +
  geom_jitter(size=2, 
              alpha=0.6)

pen_plot_base +
  geom_jitter(size=2, 
              alpha=0.6,
              position=position_jitter(width=0,
                                       height= 0.5))

# Exercise
# 1. Try out the following geoms - geom_boxplot(), 
#    geom_violin(), stat_summary(). Which do you prefer?
# 2. Try adding multiple geoms together. 
#     Does order matter?
# EC. If you want to get saucy, install ggridges and 
#   try out geom_density_ridges()

#1
penguins %>% 
  ggplot(aes(x=species, y=flipper_length_mm, fill=species))+
  geom_boxplot()

penguins %>% 
  ggplot(aes(x=species, y=flipper_length_mm, fill=species))+
  geom_violin(trim=FALSE)

penguins %>% 
  ggplot(aes(x=species, y=flipper_length_mm, fill=species))+
  stat_summary() #Eh

#2 combining plots...
penguins %>% 
  ggplot(aes(x=species, y=flipper_length_mm, fill=species))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")
           
#with 2 continuous variables...
penguins %>% 
  ggplot(aes(x=body_mass_g, y=flipper_length_mm, color=species))+
  geom_point()


#another way to do the same thing- less repetitive if making multiple plots...

pen_flipper_mass <- ggplot(data=penguins,
                           mapping = aes(y=body_mass_g,
                                         x= flipper_length_mm,
                                         color= species))
pen_flipper_mass+
  geom_point()

#-----multi-panel plots!----------

#use faceting
pen_flipper_mass +
  geom_point() +
  facet_wrap(~species)
  
#use + in facet_wrap to group by multiple variables
pen_flipper_mass +
  geom_point() +
  facet_wrap(~species + island)

# 1. Given that we have the same species of penguin on different 
# islands, what do you see if you use facet_grid() with both 
# island and species?

# 2. Incorporate other faceting variables - sex, year, etc. Or 
# mix up what is a facet and what is a color. What do you learn?

#1
pen_flipper_mass +
  geom_point() +
  facet_grid(~species + island)

#2
penguins2 <- penguins %>% 
  filter(!is.na(sex))

pen_flipper_mass2 <- ggplot(data=penguins2,
                           mapping = aes(y=body_mass_g,
                                         x= flipper_length_mm,
                                         color= sex)) #color code points by sex
pen_flipper_mass2 +
  geom_point() +
  facet_grid(~species)+ #group by species to see male/female points for each
  xlab("flipper length (mm)")+
  ylab("body mass (g)")


#---Make out plots sing----------

pen_flipper_scatter <- pen_flipper_mass2 +
  geom_point()

#labels
pen_flipper_scatter2 <- pen_flipper_scatter +
  labs(title="Penguin flipper length vs body mass",
       subtitle= "Data from Palmer LTER",
       x= "Body mass (g)",
       y= "Bill depth (mm)",
       color = "species of \nPenguin")

pen_flipper_scatter2 +
  theme_bw(base_size = 14,
           base_family = "Times")

#Jarretts fav package of themes

library(ggthemes)
pen_flipper_scatter +
  theme_tufte()

pen_flipper_scatter +
  theme_excel()

#if you want to set a theme
theme_set(theme_classic(base_size = 12, base_family = "Times"))

#colors!!!####

#make palette manually
pen_flipper_scatter+
  scale_color_manual(values = c("orange", "purple", "darkblue"))

#many pre-built palletes
pen_flipper_scatter+
  scale_color_brewer(palette= "Dark2")

pen_flipper_scatter+
  scale_color_viridis_d()

pen_flipper_scatter+
  scale_color_viridis_d(option = "A")

pen_flipper_scatter+
  scale_color_manual(values = rainbow(3))

#packeges with color palettes
library(wesanderson)
pen_flipper_scatter+
  scale_color_manual(values=wes_palette("BottleRocket2"))

#continuous color scales####

#make plot with 2 continuous variables
pen_mass_col <- ggplot(data=penguins,
                       mapping=aes(x=bill_depth_mm,
                                   y=bill_length_mm,
                                   color=body_mass_g))+
  geom_point()+
  facet_wrap(~species)

pen_mass_col

#VIRIDIS!!!
pen_mass_col +
  scale_color_viridis_c()

#VIRIDIS!!!
pen_mass_col +
  scale_color_viridis_c(option = "B") #option b is "magma" pallete!

#or assign it yourself
pen_mass_col +
  scale_color_gradient(low="blue", high="red") 

pen_mass_col +
  scale_color_gradientn(colors = c("blue", "green", "orange", "red"))


# Exercise: OK - let’s look at how flipper length relates to 
# bill length and depth. Feel free to choose what gets to be a 
# color and what gets to be a coordinate. Combine other aspects - 
# sex, year, etc., to find something more interesting. 
# But above all, make this plot have a great color scale with 
# matching theme to make it pop.

pen_flipper_bill <- ggplot(data=penguins,
                       mapping=aes(x=bill_depth_mm,
                                   y=bill_length_mm,
                                   color=flipper_length_mm,
                                   shape=sex))+
  geom_point()+
  facet_wrap(~species)+
  xlab("Bill depth (mm)")+
  ylab("Bill length (mm)")+
  scale_color_viridis_c()
         

pen_flipper_bill

#WOW!!!