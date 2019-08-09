# title: "Case Study Documentatation"
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
list_A <- c("T04", "T06", "T10", "T13", "T14", "T18", "T21", "T26", "T40") # semicolon

list_A2 <- c("T08", "T25", "T33", "T37") # comma

# #CSV with pattern B (extra cols)
list_B <- c("T12", "T15", "T17", "T23", "T32") # semicolon

list_B2 <- c("T30", "T38") # comma



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

# Function to tidy CSV with format "a"
# returns data frame
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
  df$date <<- betterDates
  
  
  # Tidy: Deleting Columns
  #Check if X1 == X1_1
  if (sum(!df$X1 == df$X1_1) == 0) {1
    # Delete X1_1
    df$X1_1 <<- NULL
    print("Column X1_1 deleted")
  }
  
  # Drop previously date-related columns
  df$Produktionsdatum_Origin_01011970 <<- NULL
  df$origin <<- NULL
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !date<"2015-01-01")
  df <<- subset(df, !date>"2016-12-31")
  
  return(df)
}

# Function to tidy CSV with format "b"
# returns data frame
tidyCSV_b <- function(path, delim = ";") {
  print("---- called tidyCSV_b ----")
  
  #Read CSV and store in temporary data frame (df)
  if (delim == ",") {
    df <<- read_csv(path)
  } else {
    df <<- read_csv2(path)
  }
  
  # Renaming
  colnames(df)[4] <<- "date"
  
  # Tidy: Deleting Columns
  #Check if X1 == X1_1
  if (sum(!df$X1 == df$X1_1) == 0) {1
    # Delete X1_1
    df$X1_1 <<- NULL
    print("Column X1_1 deleted")
  }
  # Delete other columns
  df <<- df[1:6]
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !date<"2015-01-01")
  df <<- subset(df, !date>"2016-12-31")
  
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

#Pattern Number 1

tx  <- readLines("Data/Einzelteil/Einzelteil_T01.txt")
tx2 <- gsub(pattern = "\\| \\|", replace = "\\|", x = tx)
tx3  <- gsub(pattern = '" "', replace = '"\n"', x = tx2)
tx4  <- gsub(pattern = '[^\\|] "', replace = '\n"', x = tx3)
writeLines(tx4, con = "backup.txt")

einzelteil_1 <- read.table("Einzelteil/backup", sep="|", header=TRUE)

#Pattern Number 2

#Pattern Number 3 
tx  <- readLines("Data/Einzelteil/Einzelteil_T03.txt")
tx2  <- gsub(pattern = '', replace = '\n', x = tx)
tx3  <- gsub(pattern = '\\|', replace = ',', x = tx2)
writeLines(tx3, con = "backup.txt")

einzelteil_3 <- read.table("backup",sep = ",", header=TRUE)

#Pattern Number 7 
tx  <- readLines("Data/Einzelteil/Einzelteil_T07.txt")
tx2  <- gsub(pattern = '""', replace = '"\n"', x = tx)
writeLines(tx2, con = "backup.txt")


einzelteil_7 <- read.table("backup.txt", header = TRUE)

#Pattern Number 9 
tx  <- readLines("Data/Einzelteil/Einzelteil_T09.txt")
tx2  <- gsub(pattern = '', replace = '\n', x = tx)
tx3  <- gsub(pattern = '\\\\', replace = ',', x = tx2)
writeLines(tx3, con = "backup.txt")

einzelteil_9 <- read.table("backup.txt",sep = ",", header=TRUE)

#Pattern Number 11 
tx  <- readLines("Data/Einzelteil/Einzelteil_T11.txt")
tx2  <- gsub(pattern = '', replace = '\n', x = tx)
writeLines(tx2, con = "backup.txt")

einzelteil_11 <- read.table("backup.txt", header=TRUE)

#Pattern Number 16

#For T20 same as Pattern Number 1
tx  <- readLines("Data/Einzelteil/Einzelteil_T20.txt")
tx2 <- gsub(pattern = "\\| \\|", replace = "\\|", x = tx)
tx3  <- gsub(pattern = '" "', replace = '"\n"', x = tx2)
tx4  <- gsub(pattern = '[^\\|] "', replace = '\n"', x = tx3)
writeLines(tx4, con = "backup.txt")

einzelteil_20 <- read.table("backup.txt", sep="|", header=TRUE)

#Pattern Number 22

#For T24 same as Pattern 11
tx  <- readLines("Data/Einzelteil/Einzelteil_T24.txt")
tx2  <- gsub(pattern = '', replace = '\n', x = tx)
writeLines(tx2, con = "backup.txt")

einzelteil_24 <- read.table("backup.txt", header=TRUE)



#Pattern Number 35
tx  <- readLines("Data/Einzelteil/Einzelteil_T35.txt")
tx2  <- gsub(pattern = '""', replace = '"\n"', x = tx)
tx3  <- gsub(pattern = 'A"', replace = 'A\n"', x = tx2)
tx4  <- gsub(pattern = '\\\\', replace = ' ', x = tx3)
writeLines(tx4, con = "backup.txt")

einzelteil_35 <- read.table("backup.txt", header = TRUE, row.names=NULL)

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


