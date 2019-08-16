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

# CSV with pattern B (fairly clean, sep = , 10 total cols, needs date cleaning) -> can use Code for A
Kcsv_list_B <- c("Komponente_K1BE1", "Komponente_K3SG2")

# CSV with pattern Cxyz1 (very dirty, sep = , 23 total cols with .x .y normal, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Motor.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Motor.x...
# from first col x1 = 477053 values are in the mid cols ID_Motor.y... 
# from first col x1 = 715579 values are in the Last cols ID_Motor...)
Kcsv_list_Cxyz1 <- c("Komponente_K1DI1")

# CSV with pattern Cxyz2 (very dirty, sep = , 23 total cols with .x .y normal, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Schaltung.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Schaltung.x...
# from first col x1 = 143116 values are in the mid cols ID_Schaltung.y... 
# from first col x1 = 381642 values are in the Last cols ID_Schaltung...)
Kcsv_list_Cxyz2 <- c("Komponente_K3AG1")

# CSV with pattern Cxy1 (very dirty, sep = , 16 total cols with .x .y, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Schaltung.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Schaltung.x...
# from first col x1 = 763284 values are in the mid cols ID_Schaltung.y...)
Kcsv_list_Cxy1 <- c("Komponente_K3SG1")

# CSV with pattern Cxy2 (very dirty, sep = , 16 total cols with .x .y, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Karosserie.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Karosserie.x...
# from first col x1 = 326477 values are in the mid cols ID_Karosserie.y...)
Kcsv_list_Cxy2 <- c("Komponente_K5")

# CSV with pattern Cxy3 (very dirty, sep = ; 16 total cols with .x .y, ascending second col X1_1 skipping numbers,
# cols have different arrangement: x1,x1_1,ID_Karosserie.x,Proddatum.x,Herstllnr.x,Werknr.x,Fehlr.x,Fehlrdatum.x,FehlrFahr.x...
# from first col x1 = 1 values are in the first cols ID_Karosserie.x...
# from first col x1 = 790866 values are in the mid cols ID_Karosserie.y...)
Kcsv_list_Cxy3 <- c("Komponente_K4")

######################################################

# TXT with pattern A (simple formatted txt sep = \t)
Ktxt_list_A <- c("K7.txt")

# TXT with pattern B (sep = "\")
Ktxt_list_B <- c("K2LE2.txt", "K3AG2.txt")

# TXT with pattern C (sep = "|"), date already clean
Ktxt_list_C <- c("K2ST1.txt")

# TXT with pattern D (sep = "\" zeilenende \t) SEE VICTORS CODE
Ktxt_list_D <- c("K1DI2.txt")

# TXT with pattern E (sep = "II", zeilenende "") SEE VICTORS CODE
Ktxt_list_E <- c("K2LE1.txt")


#################################################################################################################


#### Importing the data frames

# Get a char vector with all the paths
partFileNames <- list.files("Data/Komponente")
fullPath <- paste("Data/Komponente/", partFileNames, sep="")
BEVector <- fullPath[1:16]
pathVector <- fullPath[17:32]


# Function determines, based on predefined lists, which tidy function shall be applied
determineTidyFunction <- function(filePath) {

  if (length(which(str_detect(filePath, BE_list_A))) == 1){
    print(paste("found match in BE_list_A:", filePath)) # console logging
    tidyCSV_BE(filePath)
  } else if (length(which(str_detect(filePath, BE_list_B))) == 1){
    print(paste("found match in BE_list_B:", filePath)) # console logging
    tidyCSV_BE(filePath)
  } else if (length(which(str_detect(filePath, BE_list_B2))) == 1){
    print(paste("found match in BE_list_B2:", filePath)) # console logging
    tidyCSV_BE(filePath)
  } else if (length(which(str_detect(filePath, BE_list_C))) == 1){
    print(paste("found match in BE_list_C:", filePath)) # console logging
    tidyCSV_BE(filePath)
    
  } else if (length(which(str_detect(filePath, Kcsv_list_A))) == 1){
    print(paste("found match in Kcsv_list_A:", filePath)) # console logging
    tidyCSV_Kcsv_AB(filePath)
  } else if (length(which(str_detect(filePath, Kcsv_list_B))) == 1){
    print(paste("found match in Kcsv_list_B:", filePath)) # console logging
    tidyCSV_Kcsv_AB(filePath,",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxyz1))) == 1){
    print(paste("found match in Kcsv_list_Cxyz1:", filePath)) # console logging
    tidyCSV_Kcsv_Cxyz(filePath,",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxyz2))) == 1){
    print(paste("found match in Kcsv_list_Cxyz2:", filePath)) # console logging
    tidyCSV_Kcsv_Cxyz(filePath,",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxy1))) == 1){
    print(paste("found match in Kcsv_list_Cxy1:", filePath)) # console logging
    tidyCSV_Kcsv_Cxy(filePath,",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxy2))) == 1){
    print(paste("found match in Kcsv_list_Cxy2:", filePath)) # console logging
    tidyCSV_Kcsv_Cxy(filePath,",")
  } else if (length(which(str_detect(filePath, Kcsv_list_Cxy3))) == 1){
    print(paste("found match in Kcsv_list_Cxy3:", filePath)) # console logging
    tidyCSV_Kcsv_Cxy(filePath)
    
  } else if (length(which(str_detect(filePath, Ktxt_list_A))) == 1){
    print(paste("found match in Ktxt_list_A:", filePath)) # console logging
    tidyTXT_A(filePath)
  } else if (length(which(str_detect(filePath, Ktxt_list_B))) == 1){
    print(paste("found match in Ktxt_list_B:", filePath)) # console logging
    tidyTXT_B(filePath)
  } else if (length(which(str_detect(filePath, Ktxt_list_C))) == 1){
    print(paste("found match in Ktxt_list_C:", filePath)) # console logging
    tidyTXT_C(filePath)
  } else if (length(which(str_detect(filePath, Ktxt_list_D))) == 1){
    print(paste("found match in Ktxt_list_D:", filePath)) # console logging
    tidyTXT_D(filePath)
  } else if (length(which(str_detect(filePath, Ktxt_list_E))) == 1){
    print(paste("found match in Ktxt_list_E:", filePath)) # console logging
    tidyTXT_E(filePath)
  } else {
    print(paste("logging:", filePath)) # console logging
    tidyTXT_X(filePath) # for the oneliner txts
  }
  
}

# define data frame variable, necessary for returning data frame from function
df <- 1 

# Function to tidy CSV with format "BE"-------------------------------------------
# returns data frame


tidyCSV_BE <- function(path) {
  print(paste0("tidyCSV_BE called with path: ", path))
  
  # Read CSV and store in temporary data frame (df)
  df <<- read.csv2(path, stringsAsFactors = FALSE)

  
  #Check if X == X1
  if (sum(!df$X == df$X1) == 0) {1
    # Delete redundant X1
    df$X1 <<- NULL
    print("Redundant Column X1 deleted")
    names(df)[1] <<- "X1"
  }
  
  # print(df)
  return(df)
}



# Function to tidy CSV with format "Kcsv"----------------------------------------
# returns data frame
tidyCSV_Kcsv_AB <- function(path, delim = ";") {
  print(paste0("tidyCSV_AB called with path: ", path))
  
  #Read CSV and store in temporary data frame (df)
  if (delim == ",") {
    df <<- read.csv(path, stringsAsFactors = FALSE)
  } else {
    df <<- read.csv2(path, stringsAsFactors = FALSE)
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
  #Check if X == X1
  if (sum(!df$X == df$X1) == 0) {1
    # Delete redundant X1
    df$X1 <<- NULL
    print("Redundant Column X1 deleted")
  }
  
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
  
  # print(df)
  return(df)
}

tidyCSV_Kcsv_Cxyz <- function(path, delim = ";") {
  print(paste0("tidyCSV_Kcsv_Cxyz called with path: ", path))
  
  #Read CSV and store in temporary data frame (df)
  if (delim == ",") {
    df <<- read.csv(path, stringsAsFactors = FALSE)
  } else {
    df <<- read.csv2(path, stringsAsFactors = FALSE)
  }
  
  # Combine .x .y .z cols into one
  df <<- unite(df, "prod_date", "Produktionsdatum.x", "Produktionsdatum.y", "Produktionsdatum", sep="_")
  df <<- unite(df, "oem", "Herstellernummer.x", "Herstellernummer.y", "Herstellernummer", sep="_")
  df <<- unite(df, "factory", "Werksnummer.x", "Werksnummer.y", "Werksnummer", sep="_")
  df <<- unite(df, "global_id", 3, 10, 14, sep="_")
  
  # Clean newly united col names from NA
  df$prod_date <<- gsub(pattern="_NA|NA_",replace="",x=df$prod_date)
  df$oem <<- gsub(pattern="_NA|NA_",replace="",x=df$oem)
  df$factory <<- gsub(pattern="_NA|NA_",replace="",x=df$factory)
  df$global_id <<- gsub(pattern="_NA|NA_",replace="",x=df$global_id)
  names(df)[1] <<- "id"
  
  # Delete unnecessary cols, reorder
  df <<- subset(df, select=c(1,3,5,6,4)) 
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !prod_date<"2015-01-01")
  df <<- subset(df, !prod_date>"2016-12-31")
  
  # print(df)
  return(df)
}

tidyCSV_Kcsv_Cxy <- function(path, delim = ";") {
  print(paste0("tidyCSV_KCSV_Cxy called with path: ", path))
  
  #Read CSV and store in temporary data frame (df)
  if (delim == ",") {
    df <<- read.csv(path, stringsAsFactors = FALSE)
  } else {
    df <<- read.csv2(path, stringsAsFactors = FALSE)
  }
  
  # combine .x .y cols into one
  df <<- unite(df, "prod_date", "Produktionsdatum.x", "Produktionsdatum.y", sep="_")
  df <<- unite(df, "oem", "Herstellernummer.x", "Herstellernummer.y", sep="_")
  df <<- unite(df, "factory", "Werksnummer.x", "Werksnummer.y", sep="_")
  df <<- unite(df, "global_id", 3, 10, sep="_")
  
  # Clean newly united col names from NA
  df$prod_date <<- gsub(pattern="_NA|NA_",replace="",x=df$prod_date)
  df$oem <<- gsub(pattern="_NA|NA_",replace="",x=df$oem)
  df$factory <<- gsub(pattern="_NA|NA_",replace="",x=df$factory)
  df$global_id <<- gsub(pattern="_NA|NA_",replace="",x=df$global_id)
  names(df)[1] <<- "id"
  
  # Delete unncessary cols, reorder
  df <<- subset(df, select=c(1,3,5,6,4))

  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !prod_date<"2015-01-01")
  df <<- subset(df, !prod_date>"2016-12-31")
  
  # print(df)
  return(df)
}

### Functions to tidy TXT------------------------------------------------------

tidyTXT_A <- function(path){
  print(paste0("tidyTXT_A called with path: ", path))
  
  #Read TXT and store in temporary data frame (df)
  df <<- read.table(path, stringsAsFactors = FALSE)
  
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
  df$prod_date <<- betterDates
  
  
  # Tidy: Deleting Columns
  #Check if X == X1
  if (sum(!df$X == df$X1) == 0) {1
    # Delete X1
    df$X1 <<- NULL
  }
  
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
  
  # print(df)
  return(df)
}

tidyTXT_B <- function(path){
  print(paste0("tidyTXT_B called with path: ", path))
  
  #Read TXT and store in temporary data frame (df)
  df <<- read.table(path, sep = "\\", stringsAsFactors = FALSE)
  
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
  df$prod_date <<- betterDates
  
  
  # Tidy: Deleting Columns
  #Check if X == X1
  if (sum(!df$X == df$X1) == 0) {1
    # Delete X1
    df$X1 <<- NULL
  }
  
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
  
  # print(df)
  return(df)
}

tidyTXT_C <- function(path){
  print(paste0("tidyTXT_C called with path: ", path))
  
  #Read TXT and store in temporary data frame (df)
  df <<- read.table(path, sep = "|", stringsAsFactors = FALSE)
  
  
  # Tidy: Deleting Columns
  #Check if X == X1
  # if (sum(!df$X == df$X1) == 0) {1
  #   # Delete X1
  #   df$X1_1 <<- NULL
  # }
  
  # Renaming cols
  names(df)[1] <<- "id"
  names(df)[2] <<- "global_id"
  names(df)[3] <<- "prod_date"
  names(df)[4] <<- "oem"
  names(df)[5] <<- "factory"
  
  # Delete unnecessary cols, reorder
  df <<- subset(df, select=c(1,2,4,5,3)) 
  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !prod_date<"2015-01-01")
  df <<- subset(df, !prod_date>"2016-12-31")
  
  # print(df)
  return(df)
}

tidyTXT_D <- function(path){
  print(paste0("tidyTXT_D called with path: ", path))
  
  
  readLines(path) %>% 
  gsub(pattern='"\t"', replace = '"\n"') %>% 
  gsub(pattern='\t', replace = "") %>% 
  paste0('""\\',.) %>% 
  read.table(text=., sep = "\\", header=TRUE, stringsAsFactors = FALSE) ->> df
  # Maybe delete the temporary backup_Komp.txt
  
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
  df$prod_date <<- betterDates
  
  
  # Tidy: Deleting Columns
  #Check if X1 == X1_1
  if (sum(!df$X == df$X1) == 0) {1
    # Delete X1_1
    df$X1 <<- NULL
    print("Redundant Column X1 deleted")
  }
  
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
  
  # print(df)
  return(df)
}

tidyTXT_E <- function(path){
  print(paste0("tidyTXT_E called with path: ", path))
  
  # Unfinished Code, requires more testing
  tx <- readLines("Data/Komponente/Komponente_K2LE1.txt")
  tx2 <- gsub(pattern='', replace = '"\n"', x = tx)
  #tx3 <- gsub(pattern = '\"', replace =" ", x = tx2)
  tx4 <- gsub(pattern = "II", replace = "\\", x = tx2)
  writeLines(tx4, con = "testing.txt")
  df <- read.table("testing.txt", sep = "\\")
  
  df <- read.table("backup_Komp.txt", sep = "\\")
  # Maybe delete the temporary backup_Komp.txt
  
  
  # combine .x .y into one acc. to swap in specific rows

  
  # Deleting rows that shall be disregarded because of date range
  df <<- subset(df, !Produktionsdatum<"2015-01-01")
  df <<- subset(df, !Produktionsdatum>"2016-12-31")
  
  # print(df)
  return(df)
}


#################################
# Run the function / Script
#################################

# Call this function to start importing all data from ./Komponenten/
startImport <- function() {
  print("starting importing Bestandteile")
  BE_list <<- list()
  for (i in seq_along(BEVector)) {
    BE_list[[i]] <<- determineTidyFunction(BEVector[i])
  }
  print("starting importing Komponenten")
  df_list <<- list()
  for (i in seq_along(pathVector)) {
    df_list[[i]] <<- determineTidyFunction(pathVector[i])
    
    # # Renaming items in data frame list, implement when txt imports are done
    # names(df_list) <- gsub("\\Einzelteil+", "", partFileNames)
    # names(df_list) <- gsub("\\.csv$", "", partFileNames)
    # names(df_list) <- gsub("\\.txt$", "", partFileNames)
  }
}


