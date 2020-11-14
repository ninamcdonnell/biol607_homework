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

#Question 2a ####

#read in data
covid_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  select(-Combined_Key,-Long_,-Lat,-Country_Region,-Admin2, -FIPS, -code3, -iso3, -iso2, -UID) 

state_pop_data <- read_tsv("https://raw.githubusercontent.com/thedatachest/us_population/master/statepop.tsv") %>% 
  filter(Year== max(Year)) %>% #get data from most recent year
  select(!Year)

#merge covid data with state pop data

#rename state pop column to match covid data
state_pop_data <- state_pop_data %>% 
  rename(Province_State=State)

#combine df
covid_data <- covid_data %>% 
  pivot_longer(cols= -Province_State,
               names_to= "date",
               values_to = "cumulative_cases") %>% 
  mutate(date=mdy(date)) %>% 
  group_by(Province_State,date) %>% 
  summarize(cumulative_cases=sum(cumulative_cases)) %>% 
  ungroup() %>% 
  merge(state_data_for_merge, by="Province_State", sort=FALSE) %>% 
  mutate(cum_cases_per_100000= (cumulative_cases*100000)/Population) %>% 
  mutate(daily_cases= as.numeric(cumulative_cases - lag(cumulative_cases, 1))) %>% 
  mutate(daily_cases_per_10_mil= (daily_cases*10000000)/Population) %>% 
  filter(daily_cases >=0) #remove errors from daily cases reported below 0

#data function - make this cut-off at 0
state_data <- function(state){
  state_data <- covid_data %>% 
    filter(Province_State== "Massachusetts") %>% 
    group_by(date) %>% 
    summarize(cumulative_cases=sum(cumulative_cases), Population=Population) %>% 
    ungroup() %>% 
    mutate(daily_cases= as.numeric(cumulative_cases - lag(cumulative_cases, 1))) %>% 
    mutate(daily_cases_per_10_mil= (daily_cases*10000000)/Population) %>% 
    filter(daily_cases >=0) %>% #remove errors below 0
    mutate(cum_cases_per_100000= (cumulative_cases*100000)/Population) %>% 
    arrange(desc(date))}

Mass_data <- state_data("Massachusetts")

#Question 2c/2d ####

#single state plot function
single_state_plot<- function(state){
  data <- state_data(state)
  plot <- data %>% 
    ggplot()+
    geom_area(data=data, mapping=aes(x=date, y=daily_cases), fill= "green", alpha=.5, color="black")+
    geom_area(data=data, mapping=aes(x=date, y=cum_cases_per_100000), fill= "blue", alpha=.2, color="black")+
    labs(title=state, 
         x= "Date", 
         y="Cases", 
         subtitle="State daily cases (green) and cumulative cases per 100,000 residents (blue)")+
    theme_bw()
  return(plot)}

#single state plot
Mass_plot <- single_state_plot("Massachusetts")

#to-date cumulative cases by state - for ordering multi-state plots
covid_states <- merged_data %>% 
  pivot_longer(cols= -Province_State,
               names_to= "date",
               values_to = "cumulative_cases") %>% 
  mutate(date=mdy(date)) %>% 
  group_by(Province_State,date) %>% 
  summarize(cumulative_cases=sum(cumulative_cases)) %>% 
  mutate(daily_cases= as.numeric(cumulative_cases - lag(cumulative_cases, 1))) %>% 
  mutate(daily_cases_per_10_mil= (daily_cases*10000000)/Population) %>% 
  filter(daily_cases >=0) %>% #remove errors below 0
  mutate(cum_cases_per_100000= (cumulative_cases*100000)/Population)

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
  select(!Year)

covid_states <- covid_states %>% 
  merge(state_data_for_merge, by="Province_State", sort=FALSE) %>% 
  mutate(cum_cases_per_100000= (cumulative_cases*100000)/Population)


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

#list states in descending order of number of cases
covid_list <- c(covid_states$Province_State[1:51])

#run state_plot() for each covid_list state value
covid_plot_list <- lapply(covid_list, multi_state_plot)

#all plots! Now in order of most cumulative cases to fewest cumulative cases!
do.call("grid.arrange", c(covid_plot_list, ncol=5))

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
plot(culmen_mm~tarsus_mm, data=quail_data) #looks linear

quail_lm <- lm(culmen_mm~tarsus_mm, data=quail_data)
summary(quail_lm)

quail_glm <- glm(culmen_mm~tarsus_mm, 
                 data=quail_data, 
                 family = gaussian(link="identity"))
summary(quail_glm)

quail_brm <- brm(culmen_mm~tarsus_mm, 
                 data=quail_data, 
                 family = gaussian(link="identity"),
                 chains=3,
                 seed=802)
plot(quail_brm)
