# title: "Case Study Documentatation"
# author: "IDA Group 7"
# date: "July 2019"


# Installation of dependencies
if( !require(tidyverse)){
  install.packages("tidyverse")
}

if( !require(shiny)){
  install.packages("shiny")
}

if( !require(dplyr)){
  install.packages("dplyr")
}

if( !require(data.table)){
  install.packages("data.table")
}

if( !require(lubridate)){
  install.packages("lubridate")
}


# Loading libraries

library(readr)
library(dplyr)
library(data.table)
library(lubridate) # For date functions
library(stringr) # For analyzing strings


# # CSV with pattern A (clean)
list_A <- c("T04", "T06", "T08", "T10", "T13", "T14", "T18", "T21", "T25", "T26", "T33", "T37", "T40")

# #CSV with pattern B (dirty)
list_B <- c("T15", "T23", "T32", "T38")

# #CSV with pattern C (dirty with extra cols)
list_C <- c("T12", "T17", "T30")


#### Importing the data frames

# Get a char vector with all the paths
pathVector <- list.files("Data/Einzelteil")
paste("Data/Einzelteil/", pathVector)

path <- "Data/Einzelteil/Einzelteil_T04.csv"

determineTidyFunction <- function(pathVector){
  for (i in 1:length(pathVector)) {
    
  }
  
  
  return(object)
}


tidyCSV_a <- function(path){
  # return data.frame
}


tidyCSV_b <- function(path){
  # return data.frame
}


tidyCSV_c <- function(pathVector){
  # return data.frame
}

tidyTXT_a <- function(pathVector){
  # return data.frame
}

tidyTXT_xyz <- function(pathVector){
  # return data.frame
}



#Read CSV 
df <- read_csv2(path)
df2 <- df

#Read TXT ( | | delimited)
dftxt <- read_table(path2)

#Store all counted days in vector
daycount <-df$Produktionsdatum_Origin_01011970

#Check if origin has a single unique value
if (length(unique(df$origin))==1) {
  # Reformat date from data frame to fit as.Date
  betterDates <- as.Date(daycount, origin = "1970-01-01")
} else {
  print("Aborting, multiple values found!")
}

# Add date column with correctly formatted dates
df2$date <- betterDates

### Tidy: Deleting Columns

#Check if X1 == X1_1
if (sum(!df$X1 == df$X1_1) == 0) {
  # Delete X1_1
  df2$X1_1 <- NULL
}

# Drop previously date-related columns
df2$Produktionsdatum_Origin_01011970 <- NULL
df2$origin <- NULL
