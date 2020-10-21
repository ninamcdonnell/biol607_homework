#question 1

vole_vaso <- c(98,96,94,88,86,82,77,74,70,60,
               59,52,50,47,40,35,29,13,6,5)

#1a. actually not at all, and i am genuinly surprised because usually I dont do well with tongue-twisters!

#1b. 

library(dplyr)
library(magrittr)
library(purrr)
library(tidyverse)
library(ggplot2)

mean(vole_vaso)
median(vole_vaso)
sd(vole_vaso)
IQR(vole_vaso)

#1c

se <- function(x){
  ret_value <- (sd(x))/(sqrt(length(x)))
  return(ret_value)
}

se(vole_vaso)

#1d 

#2. We can get the upper quartile value of vole vassopressin with

quantile(vole_vaso, probs = 0.75)


#2a. Use sample() to get just one resample with a sample size of 10. What is its upper quartile?
  
vole_vaso_resamp <- sample(vole_vaso,
                        size=10,
                        replace = TRUE) %>% 
  quantile(probs=0.75) #also get mean ets?

#2b. Build an initial data frame for simulations with the sample sizes 5 through 20.

set.seed(802) #make it replicable. My old area code.
boot_vole_vaso <- data.frame(samp_size=5:20) #data frame with sample sizes 5-20

boot_vole_vaso 

#2c. Use this data frame to get simulated upper quartiles at each sample size 1,000 times (i.e., for 1,000 simulations).
boot_vole_vaso2 <- boot_vole_vaso %>% 
  rowwise(samp_size) %>% #calculate for each row:
  summarize(boot_quantile=replicate(1000, 
                                    quantile(sample(vole_vaso, #take quantile of values from vole_vaso simulation
                                           size=samp_size,
                                           replace= TRUE), probs=0.75)))

boot_vole_vaso2

#2d. With a ggplot, make a guesstimate as to the best sample size for estimating the upper quartile of the population. Use whatever geom you feel makes things most easy to see. E.C. Add a red dashed line using geom_vline() or geom_hline() to show where that should be, perhaps.

#plot simulated quartile values for each sample size
guesstimate <- boot_vole_vaso2 %>% 
  mutate(samp_size_factor=as.factor(samp_size)) %>% #mutate sample size to factor so that boxplots can be made to show variability in quantile values estimated at each sample size.
  ggplot(mapping= aes(x=samp_size_factor, y= boot_quantile))+
  geom_boxplot()+
  geom_vline(xintercept=15, color= "red")+ #E.C.: add vertical line where x=15
  labs(y="upper quartile",
       x="sample size",
       caption="The range of upper quartile values looks similar after a sample size of 14-15, so this is probably sufficient for estimating the population parameter.")

guesstimate

?caption 

#2e. Plot the SE of the estimate of the upper quantile by sample size. Again, what it the best way to see this? Does it level off? Is there a level you feel acceptable? Justify your answer. Does this match with what you put in 3d?

boot_se_quantile <-boot_vole_vaso2 %>% 
  group_by(samp_size) %>% #grouping SE of the estimate of upper quantile by sample size
  summarise(se_quantile=sd(boot_quantile)) #take standard deviation of the estimated quantile to get SE

se_quant_plot <- ggplot(data=boot_se_quantile, 
                        mapping= aes(x= samp_size, y= se_quantile))+
  geom_line()+
  geom_vline(xintercept=14, color= "red")+ #E.C.: add vertical line where y=14
  labs(x="sample size",
       y="SE of the estimate of the upper quantile",
       caption="The SE of the estimated upper quartile levels off around a sample size of 14-15. This concurrs with what we saw in 2d.")

se_quant_plot

#3a

#libraries
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)

theme_set(theme_bw(base_size=12))

ice <- read_csv("http://biol607.github.io/homework/data/NH_seaice_extent_monthly_1978_2016.csv") %>%
  mutate(Month_Name = factor(Month_Name),
         Month_Name = fct_reorder(Month_Name, Month)) #values of month_name are reordered so that the values of month are in ascending order. 

str(ice)

#3b

month_extent <- ggplot(data=ice, mapping=aes(x=Month_Name, y=Extent, fill=Month_Name))+
  geom_boxplot()+
  labs(x="Month",
       y="Sea ice extent 10^6 km^2",
       title="Monthly variability in sea ice extent")+
  scale_fill_brewer(palette="Paired")

month_extent

## 3c. Use dplyr to get the annual minimum sea ice extent. Plot minimum ice by year. What do you observe?
min_extent <- ice %>% 
  group_by(Year) %>% 
  summarize(min_extent=min(Extent))

str(min_extent)

max(ice$Year)

min_extent %>% 
  ggplot(aes(x=Year, y=min_extent, color=Year))+
  labs(x="Year",
       y="Minimum ice extent 10^6 km^2",
       title="Minimum sea ice by year")+
  geom_point()
  
#3d 

cut_interval(1:10, n = 5) #testing cut interval

ice %>% 
  ggplot(aes(x=Year, y=Extent, group= Month, color=Month))+
  labs(x="Year",
       y="Sea ice extent 10^6 km^2",
       title="Minimum sea ice by year")+
  geom_line()+
  scale_color_viridis_c(option = "D")+
  facet_wrap(~cut_interval(Month, n=4))
  
#3e 

#extra credit: Colorfindr!
library(colorfindr)

#Find top 100 colors from NatGeo ice-loss picture
last_ice <- get_colors("https://www.nationalgeographic.com/content/dam/environment/2018/12/book_glaciers/01_book_glacier_h_12_7_00010056.ngsversion.1545405649508.adapt.1900.1.jpg", 
                       top_n = 100, exclude_col ="white") #exclude white from list of colors
last_ice_pallette <- last_ice$col_hex #make an object from a vector of colors to serve as color values in plot

#plot 
ice2 <- ice %>% 
  mutate(year_factor=as.factor(Year)) #mutate year to factor so that palette can color it as discrete

#plot extent by month, with a separate line for each year (group)
ggice1 <-ggplot(data=ice2, mapping=aes(x=Month_Name, y=Extent, group=Year, color=Year))+
  labs(x="Month",
       y="Sea ice extent 10^6 km^2",
       title="Monthly minimum sea ice extent from 1978 to 2016")+
  #use geom_line to visualize trend over time
  geom_line()+
  scale_color_gradient(low="gray", high="blue")+
  theme_clean()

min(ice$Extent)#find min ice extent
View(ice)

ggice1

#plot extent by month, with a separate line for each year (group)
ggice2 <-ggplot(data=ice2, mapping=aes(x=Month_Name, y=Extent, group=Year, color=year_factor))+
  labs(x="Month",
       y="Sea ice extent 10^6 km^2",
       title="Monthly minimum sea ice extent from 1978 to 2016")+
  #use geom_line to visualize trend over time
  geom_line()+
  labs(color="Year") +
  scale_color_manual(values = last_ice_pallette[30:69])+
  theme_clean()

ggice2

#same as first graph, but animated
ggice3 <-ggplot(data=ice2, mapping=aes(x=Month_Name, y=Extent, group=Year, color=Year))+
  labs(x="Month",
       y="Sea ice extent 10^6 km^2",
       title="Monthly minimum sea ice extent from 1978 to 2016")+
  #use geom_line to visualize trend over time
  geom_line()+
  scale_color_gradient(low="gray", high="blue")+
  theme_clean()+
  transition_reveal(Year) +
  ease_aes("linear")
  
animate(ggice3)

ice_animate

?labs


?transition_states
#animate
library(ggplot2)
library(gganimate)

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

?geom_hline

