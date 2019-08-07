# title: "tidy_data_Komponente"
# author: "IDA Group 7"
# date: "August 2019"


# Installation of dependencies
# if( !require(tidyverse)){
#   install.packages("tidyverse")
# }

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


# # CSV with pattern A (clean, 5 main cols)
list_A <- c("K1BE1", "K1BE2", "K1DI1", "K1DI2")

# #CSV with pattern B (clean, but only 4 main cols)
list_B <- c("K2LE1", "K2LE2", )

# CSV with pattern B2 (clean, 4 main cols + extra cols)
list_B2 <- c("K2ST1")

# #CSV with pattern C (dirty with extra cols)
list_C <- c("T12", "T17", "T30")

# TXT with pattern D (simple formatted txt)
list_D <- c("K7")

# TXT with pattern E (sep = "\")
list_E <- c("K2LE2", "K3AG2")

# TXT with pattern F (sep = "|")
list_F <- c("K2ST1")


#### Importing the data frames

# Get a char vector with all the paths
partFileNames <- list.files("Data/Einzelteil")
pathVector <- paste("Data/Einzelteil/", partFileNames, sep="")

# #Reduction for testing purposes
# pathVector <- pathVector[1:5] 


determineTidyFunction <- function(pathVector) {
  
  # # Reduction for testing purposes
  # for (i in 1:5) {
  
  for (i in length(pathVector)) {
    if (length(which(str_detect(pathVector[i], list_A))) == 1){
      print(paste("logging:", i, pathVector[i])) # console logging
      tidyCSV_a(pathVector[i], list_A[i])
    } else if (length(which(str_detect(pathVector[i], list_B))) == 1){
      print(paste("logging:", i, pathVector[i])) # console logging
      tidyCSV_b(pathVector[i], list_B[i])
    } else if (length(which(str_detect(pathVector[i], list_C))) == 1){
      print(paste("logging:", i, pathVector[i])) # console logging
      tidyCSV_c(pathVector[i], list_C[i])
    }  else {
      print(paste("logging:", i, pathVector[i])) # console logging
      print("txt file or multiple matches found, aborting...");
    }
  }
}

df <- 1 #define data frame

tidyCSV_a <- function(path, fileName) {
  # return tidy data.frame for pattern a csv
  print(paste0("tidyCSV_a called with path: ", path))
  
  #Read CSV and store in temporary data frame (df)
  df <<- read_csv2(path)
  
  #Store all counted days in vector
  daycount <<-df$Produktionsdatum_Origin_01011970
  
  #Check if origin has a single unique value and reformat
  if (length(unique(df$origin))==1) {
    # Reformat date from data frame to fit as.Date
    betterDates <<- as.Date(daycount, origin = "1970-01-01")
  } else {
    print("Aborting, multiple values found!")
  }
  
  # Add date column with correctly formatted dates
  df$date <<- betterDates
  
  
  # Tidy: Deleting Columns
  #Check if X1 == X1_1
  if (sum(!df$X1 == df$X1_1) == 0) {1
    # Delete X1_1
    df$X1_1 <<- NULL
  }
  
  # Drop previously date-related columns
  df$Produktionsdatum_Origin_01011970 <<- NULL
  df$origin <<- NULL
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !date<"2015-01-01")
  df <<- subset(df, !date>"2016-12-31")
  
  # print(df)
  return(df)
}

tidyCSV_b <- function(path){
  # return tidy data.frame for pattern b csv
  print("---- called tidy b fn ----")
}


tidyCSV_c <- function(pathVector){
  # return tidy data.frame for pattern c csv
  print("---- called tidy c fn ----")
}

### Functions to tidy txt data

tidyTXT_a <- function(pathVector){
  # return tidy data.frame for pattern a txt
}

tidyTXT_xyz <- function(pathVector){
  # return data.frame
}


#################################

# Run the function / Script
determineTidyFunction(pathVector)

