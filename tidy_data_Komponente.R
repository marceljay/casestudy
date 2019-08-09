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

if( !require(lessR)){
  install.packages("lessR")
}


# Loading libraries

library(readr)
library(dplyr)
library(data.table)
library(lubridate) # For date functions
library(stringr) # For analyzing strings
library(lessR)

#################################################################################################################
#PATTERNS
#################################################################################################################

# Bestandteile Dateien
# # CSV with pattern A (clean, 5 main cols)
BE_list_A <- c("Bestandteile_Komponente_K1BE1", "Bestandteile_Komponente_K1BE2", "Bestandteile_Komponente_K1DI1", 
               "Bestandteile_Komponente_K1DI2", "Bestandteile_Komponente_K6")

# #CSV with pattern B (clean, but only 4 main cols)
BE_list_B <- c("Bestandteile_Komponente_K2LE1", "Bestandteile_Komponente_K2LE2", "Bestandteile_Komponente_K2ST2", 
               "Bestandteile_Komponente_K3AG1", "Bestandteile_Komponente_K3AG2", "Bestandteile_Komponente_K3SG1", 
               "Bestandteile_Komponente_K3SG2", "Bestandteile_Komponente_K4", "Bestandteile_Komponente_K5")

# CSV with pattern B2 (clean, 4 main cols + extra cols)
BE_list_B2 <- c("Bestandteile_Komponente_K2ST1")

# #CSV with pattern C (clean, 6 main cols)
BE_list_C <- c("Bestandteile_Komponente_K7")

#####################################################

# Produktionsdatum Dateien
# CSV with pattern A (clean, sep = ; 10 total cols, needs date cleaning)
Kcsv_list_A <- c("Komponente_K1BE2", "Komponente_K2ST2", "Komponente_K6")

# CSV with pattern B (fairly clean, sep = , 10 total cols, needs date cleaning)
Kcsv_list_B <- c("Komponente_K1BE1", "Komponente_K3SG2")

# CSV with pattern C1 (very dirty, sep = , 23 total cols with .x .y normal, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Motor.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Motor.x...
# from first col x1 = 477053 values are in the mid cols ID_Motor.y... 
# from first col x1 = 715579 values are in the Last cols ID_Motor...)
Kcsv_list_Cxyz1 <- c("Komponente_K1DI1")

# CSV with pattern C2 (very dirty, sep = , 23 total cols with .x .y normal, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Schaltung.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Schaltung.x...
# from first col x1 = 143116 values are in the mid cols ID_Schaltung.y... 
# from first col x1 = 381642 values are in the Last cols ID_Schaltung...)
Kcsv_list_Cxyz2 <- c("Komponente_K3AG1")

# CSV with pattern CX (very dirty, sep = , 16 total cols with .x .y, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Schaltung.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Schaltung.x...
# from first col x1 = 763284 values are in the mid cols ID_Schaltung.y...)
Kcsv_list_Cxy1 <- c("Komponente_K3SG1")

# CSV with pattern CX (very dirty, sep = , 16 total cols with .x .y, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Karosserie.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Karosserie.x...
# from first col x1 = 326477 values are in the mid cols ID_Karosserie.y...)
Kcsv_list_Cxy2 <- c("Komponente_K5")

# CSV with pattern CX (very dirty, sep = ; 16 total cols with .x .y, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Karosserie.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Karosserie.x...
# from first col x1 = 790866 values are in the mid cols ID_Karosserie.y...)
Kcsv_list_Cxy3 <- c("Komponente_K4")

######################################################

# TXT with pattern A (simple formatted txt sep = \t)
Ktxt_list_A <- c("K7.txt")

# TXT with pattern B (sep = "\")
Ktxt_list_B <- c("K2LE2.txt", "K3AG2.txt")

# TXT with pattern C (sep = "|")
Ktxt_list_C <- c("K2ST1.txt")


#################################################################################################################


#### Importing the data frames

# Get a char vector with all the paths
partFileNames <- list.files("Data/Komponente")
pathVector <- paste("Data/Komponente/", partFileNames, sep="")


# Function determines, based on predefined lists, which tidy function shall be applied
determineTidyFunction <- function(filePath) {

  if (length(which(str_detect(filePath, BE_list_A))) == 1){
    print(paste("found match in BE_list_A:", filePath)) # console logging
    tidyCSV_BE_A(filePath)
  } else if (length(which(str_detect(filePath, BE_list_B))) == 1){
    print(paste("found match in BE_list_B:", filePath)) # console logging
    tidyCSV_BE_B(filePath)
  } else if (length(which(str_detect(filePath, BE_list_B2))) == 1){
    print(paste("found match in BE_list_B2:", filePath)) # console logging
    tidyCSV_BE_B2(filePath)
  } else if (length(which(str_detect(filePath, BE_list_C))) == 1){
    print(paste("found match in BE_list_C:", filePath)) # console logging
    tidyCSV_BE_C(filePath)
    
  } else if (length(which(str_detect(filePath, Kcsv_list_A))) == 1){
    print(paste("found match in Kcsv_list_A:", filePath)) # console logging
    tidyCSV_Kcsv_A(filePath)
  } else if (length(which(str_detect(filePath, Kcsv_list_B))) == 1){
    print(paste("found match in Kcsv_list_B:", filePath)) # console logging
    tidyCSV_Kcsv_B(filePath, sep=",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxyz1))) == 1){
    print(paste("found match in Kcsv_list_Cxyz1:", filePath)) # console logging
    tidyCSV_Kcsv_Cxyz1(filePath, sep=",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxyz2))) == 1){
    print(paste("found match in Kcsv_list_Cxyz2:", filePath)) # console logging
    tidyCSV_Kcsv_Cxyz2(filePath, sep=",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxy1))) == 1){
    print(paste("found match in Kcsv_list_Cxy1:", filePath)) # console logging
    tidyCSV_Kcsv_Cxy1(filePath, sep=",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxy2))) == 1){
    print(paste("found match in Kcsv_list_Cxy2:", filePath)) # console logging
    tidyCSV_Kcsv_Cxy2(filePath, sep=",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxy3))) == 1){
    print(paste("found match in Kcsv_list_Cxy3:", filePath)) # console logging
    tidyCSV_Kcsv_Cxy3(filePath)
    
  } else if (length(which(str_detect(filePath, Ktxt_list_A))) == 1){
    print(paste("found match in Ktxt_list_A:", filePath)) # console logging
    tidyTXT_A(filePath)
  } else if (length(which(str_detect(filePath, Ktxt_list_B))) == 1){
    print(paste("found match in Ktxt_list_B:", filePath)) # console logging
    tidyTXT_B(filePath, sep="\\")
  } else if (length(which(str_detect(filePath, Ktxt_list_C))) == 1){
    print(paste("found match in Ktxt_list_C:", filePath)) # console logging
    tidyTXT_C(filePath, sep="|")
  } else {
    print(paste("logging:", filePath)) # console logging
    tidyTXT_X(filePath) # for the oneliner txts
  }
  
}

# define data frame variable, necessary for returning data frame from function
df <- 1 

# #import .txt files
# temp <- list.files(path = "Data/Komponente/", pattern="*.txt")
# batchpath <- paste("Data/Komponente/",temp, sep="")
# 
# txtfile <- read.table(batchpath[1], sep="\\")


# Function to tidy CSV with format "a"
# returns data frame
tidyCSV_BE_A <- function(path, delim = ";") {
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
  
  # print(df)
  return(df)
}

tidyCSV_b <- function(path, fileName){
  # return tidy data.frame for pattern b csv
  print("---- called tidy b fn ----")
}


tidyCSV_c <- function(path, fileName){
  # return tidy data.frame for pattern c csv
  print("---- called tidy c fn ----")
}

### Functions to tidy txt data

tidyTXT_d <- function(path, delim = "\\"){
  print(paste0("tidyTXT_d called with path: ", path))
  
  #Read TXT and store in temporary data frame (df)
  if (delim == "|") {
    df <<- read.table(path, SEP = "|")
  } else {
    df <<- read.table(path)
  }
  
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

tidyTXT_e <- function(pathVector){
  # return data.frame
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


