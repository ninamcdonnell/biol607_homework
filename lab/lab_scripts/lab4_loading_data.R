#'----------------
#'@title Loading Data
#'@date 2020-10-02
#-----------------
library(here)

#here for good practice
setwd(here::here())

#Navigating our filesystem with R! ####

#where does R think it is looking for files on the comp?
getwd() #switch working directory if its not what you want

#what files can R get rn?
list.files()

#okay, what is in data?
list.files("lab/lab_data") #note: you need to be in the project that is in the folder with the data for this to work

install.packages("here")
# https://here.r-lib.org #package for easy file referencing
library(here)
here()

#one last trick
#sometimes you will see ./ or ../
#./ means realtive to the current working directory
list.files("../")
list.files("../lab_data") #not working...


# Reading in a file####

# old school file reading

sheet1 <- read.csv("lab/lab_data/test_sheet_1.csv")

# what to do once you read in a file

sheet1 <- read.csv("lab/lab_data/test_sheet_1.csv", #path in directory
                   na.strings=c("NA", ".", "-")) #all of these notations will be input as NA values!

names(sheet1) #it will convert spaces in column names (not in my file) to "."
str(sheet1) #check that variables are correct. Sometimes NAs read in funny and change variable type
summary(sheet1)

#looking at large datasets
library(visdat) 
vis_dat(sheet1)

sheet1[2,2] <- NA #what if 2,2 were an NA value?
vis_dat(sheet1) #now you can see the NA

library(naniar) #summarizes NA data from large data sets
miss_var_summary(sheet1) #shows us that 1 value is missing from variable_1 (the value we changed to NA above), and tells us what percent of data is missing

#skimming your data
library(skimr)
skim(sheet1)

#THE POINT IS-- EXAMINE AND REEXAMINE YOUR DATA BEFORE MOVING FORWARD AND GETTING FRUSTRATED


#Loading the data the new school way! ####

library(readr)
sheet2 <- read_csv("lab/lab_data/test_sheet_1.csv",
                   col_types="ddc")
names(sheet2) #readr will preserve spaces (but there are none in my sheet)

#to clean names 
library(janitor)
clean_names(sheet2) #removes spaces

#or

sheet2 <- sheet2 %>% clean_names()

#loading data from excel or other things ####

#good if working with changing excel documents and collaborators that don't like .csv files

library(readxl) 

sheet1_xl <- read_excel("lab/lab_data/test_data.xls", sheet="test_sheet_1") %>% 
                        janitor::clean_names()

sheet1_xl

#for googlesheets use googlesheets4
#if you have HUGE data, use vroom::vroom() or data.table::fread()
