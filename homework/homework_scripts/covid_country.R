#'-------------------------------
#' @title COVID Country
#' @author Nina McDonnell
#' @date 11/6/2020
#' ------------------------------
#' 
#' Run 'single state' before 'all states' 
#' For the big plot of all states, just run through to the end  

# Libraries
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggdist)

# Fetch most recent data
covid_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  dplyr::select(-Combined_Key,-Long_,-Lat,-Country_Region,-Admin2, -FIPS, -code3, -iso3, -iso2, -UID) 

state_pop_data <- read_tsv("https://raw.githubusercontent.com/thedatachest/us_population/master/statepop.tsv") %>% 
  filter(Year== max(Year)) #get data from most recent year

str(covid_data)

str(state_pop_data)

# SINGLE STATE ####

#---- run this section for setup -------

#Single state data set functions
state_data <- function(state){
  state_pop <- state_pop_data %>% 
    filter(State== state) %>% 
    summarize(Population)
  
  covid_long <- covid_data %>% 
    filter(Province_State== state) %>% 
    pivot_longer(cols= -Province_State,
                 names_to= "date",
                 values_to = "cumulative_cases") %>% 
    mutate(date=mdy(date)) %>% 
    group_by(date) %>% 
    summarize(cumulative_cases=sum(cumulative_cases)) %>% 
    mutate(daily_cases= as.numeric(cumulative_cases - lag(cumulative_cases, 1))) %>% 
    mutate(daily_cases_per_10_mil= (daily_cases*10000000)/state_pop$Population) %>% 
    filter(daily_cases >=0) %>% #remove errors below 0
    mutate(cum_cases_per_100000= (cumulative_cases*100000)/state_pop$Population)}

#test
state_df <- state_data("Massachusetts") # <- test: input state name to see data 

# Single state plot function
single_state_plot<- function(state){
  data <- state_data(state)
  plot <- data %>% 
    ggplot()+
    geom_area(data=data, mapping=aes(x=date, y=daily_cases), show.legend=TRUE, fill= "green", alpha=.5, color="black")+
    geom_area(data=data, mapping=aes(x=date, y=cum_cases_per_100000), fill= "blue", alpha=.2, color="black")+
    labs(title=state, 
         x= "Date", 
         y="Cases", 
         subtitle="State daily cases (green) and cumulative cases per 100,000 residents (blue)")+
    theme(legend.position = "right")
  return(plot)}


# -----Plot a state! ------

single_state_plot("Massachusetts") # <- input state name to plot



# ALL STATES ####

#---- run this section for setup -------

#to-date cumulative cases by state - for ordering multi-state plots
covid_states <- covid_data %>% 
  pivot_longer(cols= -Province_State,
               names_to= "date",
               values_to = "cumulative_cases") %>% 
  mutate(date=mdy(date)) %>% 
  group_by(Province_State,date) %>% 
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
  dplyr::select(!Year)

covid_states <- covid_states %>% 
  merge(state_data_for_merge, by="Province_State", sort=FALSE) %>% 
  mutate(cum_cases_per_100000= (cumulative_cases*100000)/Population)

#list states in descending order of number of cases
covid_list <- c(covid_states$Province_State[1:51])

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

multi_state_plot("Massachusetts") #test on one state (optional)

#run state_plot() for each state in 'covid_list' 
covid_plot_list <- lapply(covid_list, multi_state_plot)

# plot all! Now in order of most cumulative cases to fewest cumulative cases:
covid_country <- do.call("grid.arrange", c(covid_plot_list, 
                                           ncol=5, 
                                           bottom="Daily COVID-19 cases per 1 million residents (green) and cumulative cases \n per 100,000 residents (blue), by state, in order of most total cases to fewest total cases."))

#----------- Get all plots ---------------

# See all plots!

covid_country



