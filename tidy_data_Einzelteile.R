# title: "Case Study Documentatation"
# author: "IDA Group 7"
# date: "August 2019"


# Installation of dependenciespathv
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

# if( !require(pryr)){
#   install.packages("pryr")
# }


# Loading libraries 

library(readr)
library(dplyr)
library(data.table)
library(lubridate) # For date functions
library(stringr) # For analyzing strings
# library(pryr) # memory management


# # CSV with pattern A (clean)
list_A <- c("T04", "T10", "T13", "T14", "T18", "T21", "T26", "T40") # semicolon
list_A2 <- c("T06", "T08", "T25", "T33", "T37") # comma

# #CSV with pattern B (extra cols)
list_B <- c("T12", "T15", "T17", "T23", "T32") # semicolon


list_B2 <- c("T30", "T38") # comma
# T38 - 16 col
# T30 - 23 col

#### Importing the data frames

# Get a char vector with all the paths
partFileNames <- list.files("Data/Einzelteil")
pathVector <- paste("Data/Einzelteil/", partFileNames, sep="")


# Function determines, based on predefined lists, which tidy function shall be applied
determineTidyFunction <- function(filePath) {
  
    if (length(which(str_detect(filePath, list_A))) == 1){
      print(paste("found match in list_A:", filePath)) # console logging
      tidyCSV_a(filePath)
    } else if (length(which(str_detect(filePath, list_A2))) == 1){
      print(paste("found match in list_A2:", filePath)) # console logging
      tidyCSV_a(filePath, ",")
    } else if (length(which(str_detect(filePath, list_B))) == 1){
      print(paste("found match in list_B:", filePath)) # console logging
      tidyCSV_b(filePath)
    } else if (length(which(str_detect(filePath, list_B2))) == 1){
      print(paste("found match in list_B2:", filePath)) # console logging
      tidyCSV_b(filePath, ",")
    }  else {
      print(paste("logging:", filePath)) # console logging
      tidyTXT_a(filePath)
    }
}

# define data frame variable, necessary for returning data frame from function
df <- 1 

# Define empty list for data frames reporting
# NA values found in imported data frames
importStats <- list()

# Function to tidy CSV with format "a"
# returns data frame, standard delimiter ";"
tidyCSV_a <- function(path, delim = ";") {
  print(paste0("tidyCSV_a called with path: ", path))
  
  #Read CSV and store in temporary data frame (df)
  if (delim == ",") {
    df <<- read_csv(path)
  } else {
    df <<- read_csv2(path)
  }

  #Store all counted days in vector
  daycount <<-df$Produktionsdatum_Origin_01011970

  #Check if origin has a single unique value and reformat
  if (length(unique(df$origin))==1) {
    # Reformat date from data frame to fit as.Date
    betterDates <<- as.Date(daycount, origin = "1970-01-01")
  } else {
    print("Aborting, multiple values found for 'origin'")
  }
  
  # Add date column with correctly formatted dates
  df$prod_date <<- betterDates
  
  
  # Tidy: Deleting Columns
  #Check if X1 == X1_1
  if (sum(!df$X1 == df$X1_1) == 0) {
    # Delete X1_1
    df$X1_1 <<- NULL
    print("Column X1_1 deleted")
  }
  
  # # Drop previously date-related columns
  # df$Produktionsdatum_Origin_01011970 <<- NULL
  # df$origin <<- NULL
  
  # Drop columns  
  df <<- df[-c(5:9)]
  
  # Renaming cols
  names(df)[1] <<- "id"
  names(df)[2] <<- "global_id"
  names(df)[3] <<- "oem"
  names(df)[4] <<- "factory"
  
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !prod_date<"2015-01-01")
  df <<- subset(df, !prod_date>"2016-12-31")
  
  # Check for NA values, should be elaborated for better detection
  if(length(which(is.na(df)))>0) {
    print("Found NA values")
    i = length(importStats)
    
    # Analyze for NAs and append to list importStats
    importStats[[i+1]] <<- sapply(df, function(x) sum(is.na(x))); 
  } else {
    print("Good, no NA values found")
  }
  
  return(df)
}

# Function to tidy CSV with format "b"
# returns data frame, standard delimiter ";"
tidyCSV_b <- function(path, delim = ";") {
  print("---- called tidyCSV_b ----")
  
  #Read CSV and store in temporary data frame (df)  
  if (delim == ",") {
    df <<- read.csv(path, stringsAsFactors = FALSE)
  } else {
    df <<- read.csv2(path, stringsAsFactors = FALSE)
  }
  
  # Combine related columns, since after some row number, values appear in different columns
    df <<- unite(df, "prod_date", contains("Produktionsdatum"), sep="_")
    df <<- unite(df, "oem",  contains("Herstellernummer"), sep="_")
    df <<- unite(df, "factory", contains("Werksnummer"), sep="_")
    df <<- unite(df, "global_id", contains("ID_T"), sep="_") 
  
  # Clean newly united col names from NA
  df$prod_date <<- gsub(pattern="_NA|NA_", replace="", x=df$prod_date)
  df$oem <<- gsub(pattern="_NA|NA_", replace="", x=df$oem)
  df$factory <<- gsub(pattern="_NA|NA_", replace="", x=df$factory)
  df$global_id <<- gsub(pattern="_NA|NA_", replace="", x=df$global_id)
  names(df)[1] <<- "id"
  
  
  # Tidy: Deleting Columns
  #Check if X1 == X1_1
  if (sum(!df$X1 == df$X1_1) == 0) {
    # Delete X1_1
    df$X1_1 <<- NULL
    print("Column X1_1 deleted")
  }
  
  # Delete unnecessary cols, reorder
  df <<- subset(df, select=c(1,3,5,6,4)) 
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !prod_date<"2015-01-01")

  df <<- subset(df, !prod_date>"2016-12-31")  
  # Check for NA values, should be elaborated for better detection
  if(length(which(is.na(df)))>0) {
    print("Found NA values")
    i = length(importStats)
    
    # Analyze for NAs and append to list importStats
    importStats[[i+1]] <<- sapply(df, function(x) sum(is.na(x))); 
  } else {
    print("Good, no NA values found")
  }
  
  return(df)
}

### Functions to tidy txt data
# returns data frame
tidyTXT_a <- function(path){
  print("---- called tidyTXT_a ----")
  
  # Create dummy data frame (because of errors)
  df <<- data.frame(ID = c(1, 2, 3, 4, 5),
                    var1 = c('a', 'b', 'c', 'd', 'e'),
                    var2 = c(1, 1, 0, 0, 1))
  return(df)
}



#################################
# Run the function / Script
#################################

# Call this function to start importing all data from ./Einzelteile/
startImport <- function() {
  print("starting importing")
  df_list <<- list()
  for (i in seq_along(pathVector)) {
    df_list[[i]] <<- determineTidyFunction(pathVector[i])
    
    # # Renaming items in data frame list, implement when txt imports are done
    # names(df_list) <- gsub("\\Einzelteil+", "", partFileNames)
    # names(df_list) <- gsub("\\.csv$", "", partFileNames)
    # names(df_list) <- gsub("\\.txt$", "", partFileNames)
  }
}


