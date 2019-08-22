# title: "Case Study Documentatation"
# author: "IDA Group 7"
# date: "August 2019"


# # Installation of dependencies (commented for faster testing)
# if (!require(tidyverse)) { install.packages("tidyverse") }
# 
# if (!require(shiny)) { install.packages("shiny") }
# 
# if (!require(dplyr)) { install.packages("dplyr") }
# 
# if (!require(data.table)) { install.packages("data.table") }
# 
# if (!require(lubridate)) { install.packages("lubridate") }
# 
# if (!require(dplyr)) { install.packages("dplyr") }


# Loading libraries 
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate) # For date functions
library(stringr) # For analyzing strings



# CSV with pattern A (clean)
list_A <- c("T04", "T10", "T13", "T14", "T18", "T21", "T26", "T40") # semicolon
list_A2 <- c("T06", "T08", "T25", "T33", "T37") # comma

# CSV with pattern B (extra cols)
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
      print(paste("found TXT file:", filePath)) # console logging
      importTXT(filePath)
    }
}


# Define empty list for data frames reporting
# NA values found in imported data frames
importStats <- list()

# Function to tidy CSV with format "a"
# returns data frame, standard delimiter ";"
tidyCSV_a <- function(path, delim = ";") {
  print(paste0("tidyCSV_a called with path: ", path))
  
  #Import CSV depending on delimiter
  if (delim == ",") {
    df <- read_csv(path)
  } else {
    df <- read_csv2(path)
  }

  # Tidy dates in short table
  df <- tidyDate(df)
  
  # # Tidy: Deleting Columns
  # #Check if X1 == X1_1
  # if (sum(!df$X1 == df$X1_1) == 0) {
  #   # Delete X1_1
  #   df$X1_1 <- NULL
  #   print("Column X1_1 deleted")
  # }
  
  # # Drop columns (prod_date was appended as 11th column)
  df <- df[c(3:5, 11)] # Keeps oem and factory columns
  df <- df[c(3, 11)]
  
  # Renaming cols
  # names(df)[1] <- "id"  # probably not needed
  names(df)[1] <- "global_id"
  names(df)[2] <- "oem"
  names(df)[3] <- "factory"

  # Check for NA values
  importAnalysis(df)
  
  return(df)
}


# Function to tidy CSV with format "b"
# returns data frame, standard delimiter ";"
tidyCSV_b <- function(path, delim = ";") {
  print("---- called tidyCSV_b ----")
  
  #Import CSV depending on delimiter
  if (delim == ",") {
    df <- read.csv(path, stringsAsFactors = FALSE)
  } else {
    df <- read.csv2(path, stringsAsFactors = FALSE)
  }
  
  # Unite, rename, filter dates
  df <- tidyLong(df)

  # Drop columns except the 4 necessary ones
  # df <- df[3:6] # Keeps oem and factory
  df <- df[3:4]
  
  # Check for NA values
  importAnalysis(df)

  return(df)
}

### GENERAL TIDY FUNCTIONS

# Unite, rename, filter dates
# Currently not uniting oem and factory columns due to dropping requirements
tidyLong <- function(df) {
  print("tidyLong called!")
  
  # Unite related columns, since after some row number, values appear in different columns
  df <- unite(df, "prod_date", contains("Produktionsdatum"), sep="_")
  # df <- unite(df, "oem",  contains("Herstellernummer"), sep="_")
  # df <- unite(df, "factory", contains("Werksnummer"), sep="_")
  df <- unite(df, "global_id", contains("ID_T"), sep="_") 
  
  # Clean newly united col names from NA
  df$prod_date <- gsub(pattern="_NA|NA_", replace="", x=df$prod_date)
  # df$oem <- gsub(pattern="_NA|NA_", replace="", x=df$oem)
  # df$factory <- gsub(pattern="_NA|NA_", replace="", x=df$factory)
  df$global_id <- gsub(pattern="_NA|NA_", replace="", x=df$global_id)
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !prod_date<"2015-01-01")
  df <- subset(df, !prod_date>"2016-12-31")
  
  return(df)
}


# Function to tidy short table pattern with origin date col
tidyDate <- function(df) {
  print("tidyDate called!")
  
  daycount <- df$Produktionsdatum_Origin_01011970
  
  if (length(unique(df$origin))==1) {
    # Reformat date from data frame to fit as.Date
    betterDates <- as.Date(daycount, origin = "1970-01-01")
  } else {
    betterDates <- as.Date(daycount, origin = "1970-01-01")
    print("WARNING! Multiple values found!")
  }
  
  # Add date column with correctly formatted dates
  df$prod_date <- betterDates
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !prod_date<"2015-01-01")
  df <- subset(df, !prod_date>"2016-12-31")

  return(df)
}

dropAndRename <- function(df) {
  
  #  Drop columns (prod_date was appended as 11th column)
  # df <- df[c(2:4, 10)] # Keeps Factory / OEM
  df <- df[c(2, 10)]
  
  # Renaming cols
  # names(df)[1] <- "id"  # probably not needed
  names(df)[1] <- "global_id"
  # names(df)[2] <- "oem"
  # names(df)[3] <- "factory"
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}

# Function to assign the correct import and tidy function to a .txt file
importTXT <- function(path) {
  print(path)
  
    if (str_detect(path, "T01.txt")) {
      print("tidyTXT_1 called")
      return(tidyTXT_1(path))
      
    } else if(str_detect(path, "T02.txt")) {
      print("tidyTXT_2 called")
      return(tidyTXT_2(path))
      
    } else if(str_detect(path, "T03.txt")) {
      print("tidyTXT_3 called")
      return(tidyTXT_3(path))
    
    } else if(str_detect(path, "T07.txt")) {
      print("tidyTXT_7 called")
      return(tidyTXT_7(path))
      
    } else if(str_detect(path, "T09.txt")) {
      print("tidyTXT_9 called")
      return(tidyTXT_9(path))
      
    } else if(str_detect(path, "T11.txt")) {
      print("tidyTXT_11 called")
      return(tidyTXT_11(path))

    } else if(str_detect(path, "T16.txt")) {
      print("tidyTXT_16 called")
      return(tidyTXT_16(path))
      
    } else if(str_detect(path, "T20.txt")) {
      print("tidyTXT_20 called")
      return(tidyTXT_20(path))
      
    } else if(str_detect(path, "T22.txt")) {
      print("tidyTXT_22 called")
      return(tidyTXT_22(path))
      
    } else if(str_detect(path, "T24.txt")) {
      print("tidyTXT_24 called")
      return(tidyTXT_24(path))
      
    } else if(str_detect(path, "T27.txt")) {
      print("tidyTXT_27 called")
      return(tidyTXT_27(path))
      
    } else if(str_detect(path, "T31.txt")) {
      print("tidyTXT_31 called")
      return(tidyTXT_31(path))
      
    } else if(str_detect(path, "T34.txt")) {
      print("tidyTXT_34 called")
      return(tidyTXT_34(path))
      
    } else if(str_detect(path, "T35.txt")) {
      print("tidyTXT_35 called")
      return(tidyTXT_35(path))
      
    } else if(str_detect(path, "T36.txt")) {
      print("tidyTXT_36 called")
      return(tidyTXT_36(path))
      
    } else if(str_detect(path, "T39.txt")) {
      print("tidyTXT_39 called")
      return(tidyTXT_39(path))
    }  
  
}
    
# WORKS ALMOST FINE (memory can be bottleneck though, WEIRD WHITESPACE)
# Structure: wide (dirty)
tidyTXT_1 <- function(path) {

  ##### NUMEROUS EXTRA WHITE SPACES GET IMPORTED ######
  x <- readLines(path) %>%
    gsub(pattern = "\\| \\|", replace = "\\|",.) %>%
    gsub(pattern = '(?<=[^\\|]) "', replace = '\n"',., perl = TRUE)

  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), sep="|", header=TRUE)
  }

  # Unite related columns, since after some row number, values appear in different columns
  df <- unite(df, "prod_date", contains("Produktionsdatum"), sep="_")
  df <- unite(df, "oem",  contains("Herstellernummer"), sep="_")
  df <- unite(df, "factory", contains("Werksnummer"), sep="_")
  df <- unite(df, "global_id", contains("ID_T"), sep="_")


  # Clean newly united col names from NA
  df$prod_date <- gsub(pattern=" _ NA|NA _ ", replace="", x=df$prod_date)
  df$oem <- gsub(pattern=" _ NA|NA _ ", replace="", x=df$oem)
  df$factory <- gsub(pattern=" _ NA |NA _ ", replace="", x=df$factory)
  df$global_id <- gsub(pattern=" _ NA|NA _ ", replace="", x=df$global_id)

  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !prod_date<"2015-01-01")
  df <- subset(df, !prod_date>"2016-12-31")

  # Drop columns except the 4 necessary ones
  df <- df[2:5]

  # Check for NA values
  importAnalysis(df)

  return(df)
}

# WORKS FINE (memory can be bottleneck though)
# Structure: wide (dirty)
tidyTXT_2 <- function(path){
  
  x <- readLines(path) %>%
    gsub(pattern = '(?<=")\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=A)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=[^-]\\d0)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=[^-][\\d|\\.][0-9])\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)

  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }

  df  <- tidyLong(df)

  # Drop columns except the 4 necessary ones
  df <- df[2:5]

  # Check for NA values
  importAnalysis(df)

  return(df)
}

# WORKS FINE
# Structure: slim
tidyTXT_3 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    gsub(pattern = '\\|', replace = ',', .)
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), sep=",", header=TRUE)
  }
  
  df <- tidyDate(df)

  # Drop columns and rename
  df <- dropAndRename(df)
  
  return(df)
}

# WORKS FINE
# Structure: slim
tidyTXT_7 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '""', replace = '"\n"', .) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
  
  df <- tidyDate(df)
  
  # Drop columns and rename 
  df <- dropAndRename(df)
  
  return(df)
}

#############################################################################
# DOES NOT WORK
# Structure: wide (dirty)
tidyTXT_9 <- function(path) {
   x <- readLines(path) %>%
        gsub(pattern = '  ', replace = '\n', .) %>%
        gsub(pattern = '\\\\', replace = ',', .) %>%
        writeLines(., con = "backup.txt")
   
   df <- read.table("backup.txt", header = TRUE)
     
  
  # for (i in 2:length(x) ) {
  #   df <- read.table(textConnection(x[i]), sep=",", header=TRUE)
  # }
    
  # df  <- tidyLong(df)
  # 
  # # Drop columns except the 4 necessary ones
  # df <- df[2:5]
  # 
  # # Check for NA values
  # importAnalysis(df)
  
  return(df)
}

# WORKS FINE (did not work initially)
# Structure: slim
tidyTXT_11 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]),                      header=TRUE)
  }
    
  df <- tidyDate(df)
  
  # Drop columns and rename 
  df <- dropAndRename(df)  
  
  return(df)
}

# WORKS FINE
# Structure: wide (dirty)
tidyTXT_16 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '(?<!\\|)\\s+"(?! \\|)', replace = '\n"', ., perl = TRUE) %>%
    gsub(pattern = 'A[^|]"', replace = 'A\n"', .) %>%
    gsub(pattern = '0[^|0-9]"', replace = '0\n"', .) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    gsub(pattern = "\\| \\|", replace = " ", .) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
  
  df  <- tidyLong(df)

  # Drop columns except the 4 necessary ones
  df <- df[2:5]

  # Check for NA values
  importAnalysis(df)

  return(df)
}

# DOES NOT WORK
# Changed
# Structure: slim (MULTIPLE VALUES FOR origin found !!!)
tidyTXT_20 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '" "', replace = '"\n"', .) %>%
    gsub(pattern = '[^\\|] "', replace = '\n"', .) %>%
    gsub(pattern = "\\| \\|", replace = "\\|", .)
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), sep = "|", header=TRUE)
  }
  
  df["origin"] <- "01-01-1970"
  
  df <- tidyDate(df)
   
  # # Drop columns and rename 
  df <- dropAndRename(df)
  
  return(df)
}

# WORKS FINE
# Structure: wide (dirty)
tidyTXT_22 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '(?<!\\s)"(?!\\s)', replace = '\n"', ., perl = TRUE) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }

  df <- tidyLong(df) 
  
  # Drop columns except the 4 necessary ones
  df <- df[2:5]
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}

# WORKS FINE
# Structure: wide (dirty)
tidyTXT_24 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '', replace = '\n', .)
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
    
  df <- tidyLong(df) 
  
  # Drop columns except the 4 necessary ones
  df <- df[2:3]
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}


# DOES NOT WORK
# Changed: Multiple values found
# Structure: slim
tidyTXT_27 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '(?<=origin)\\W+(?=[0-9])', replace = '"\n"', ., perl = TRUE) %>%
    gsub(pattern = '(?<=01-1970)\\W+(?=[0-9])', replace = '"\n"', ., perl = TRUE) %>%
    gsub(pattern = "\\| \\|", replace = "\\|", .) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), sep = "|", header=TRUE)
  }
  
  df["origin"] <- "01-01-1970" #Change all Origin Values to 01-01-1970
  
  df <- tidyDate(df)
  
  # Drop columns and rename 
  df <- dropAndRename(df)
  
  return(df)
}

# WORKS FINE (or almost, has a weird column at the end with a special character)
# Structure: slim
tidyTXT_31 <- function(path) {
  x <- readLines(path) %>%
    gsub(pattern = '(?<!\\s)"(?=[0-9])', replace = '"\n"', ., perl = TRUE) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
  
  # Remove last errorous column
  df <- df[-c(10)]
  
  df <- tidyDate(df)
  
  #  Drop columns and rename 
  df <- dropAndRename(df)
  
  return(df)
}

#WORKS FINE 
# Structure: Slim
tidyTXT_34 <- function(path) {
  x <- readLines(path) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    gsub(pattern = "\\| \\|", replace = " ",.) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
  
  df <- tidyDate(df)
  
  # Drop columns and rename 
  df <- dropAndRename(df)
  
  return(df)
}


# WORKS FINE
# Structure: wide (dirty)
tidyTXT_35 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    gsub(pattern = '(?<!\\\\)"(?!\\\\|")', replace = '\n"', ., perl = TRUE) %>%
    gsub(pattern = '\\\\', replace = ' ', .)
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
    
  df <- tidyLong(df) 
  
  # Drop columns except the 4 necessary ones
  df <- df[2:5]
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}

# WORKS FINE
# Structure: slim
tidyTXT_36 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '" "', replace = '"\n"', .) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
  
  df <- tidyDate(df)
  
  #  Drop columns and rename 
  df <- dropAndRename(df)
  
  return(df)
}


# WORKS FINE
# Structure: wide (totally messed up, SPECIAL CASE)
tidyTXT_39 <- function(path){
  x <- readLines(path)  %>%
    gsub(pattern = '""', replace = '"\n"', .)  %>%
    gsub(pattern = '(?<!\\\\)"(?!\\\\|")', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '\\\\', replace = ' ', .)
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), header=TRUE)
  }
    
  # combine .x .y cols into one 
  unite("global_id", "Produktionsdatum.x", "Produktionsdatum.y", sep="_") %>%
  unite("prod_date", "Herstellernummer.x", "Herstellernummer.y",sep="_") %>%
  unite("oem", "Werksnummer.x", "Werksnummer.y", sep="_") %>%
  unite("factory", "Fehlerhaft.x", "Fehlerhaft.y", sep="_") 

  # Clean newly united col names from NA
  df$prod_date <- gsub(pattern="_NA|NA_",replace="",x=df$prod_date)
  df$oem <- gsub(pattern="_NA|NA_",replace="",x=df$oem)
  df$factory <- gsub(pattern="_NA|NA_",replace="",x=df$factory)
  df$global_id <- gsub(pattern="_NA|NA_",replace="",x=df$global_id)

  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !prod_date<"2015-01-01")
  df <- subset(df, !prod_date>"2016-12-31")
  
  # Drop columns except the 4 necessary ones
  df <- df[3:6]
  
  return(df)
}

#################################
# Helper Functions
#################################


# Function to check for NA values
importAnalysis <- function(df) {
  
  if(length(which(is.na(df)))>0) {
    print("Found NA values")
    i = length(importStats)
    
    # Analyze for NAs and append to list importStats
    importStats[[i+1]] <<- sapply(df, function(x) sum(is.na(x))); 
  } else {
    print("Good, no NA values found")
  }
  
}


#################################
# Run the function / Script
#################################

# Call this function to start importing all data from ./Einzelteile/
df_list <<- list()

startImport <- function() {
  print("starting importing")
  for (i in seq_along(pathVector)) {
    df_list[[i]] <<- determineTidyFunction(pathVector[i])
    
    # # Renaming items in data frame list, implement when txt imports are done
    # names(df_list) <- gsub("\\Einzelteil+", "", partFileNames)
    # names(df_list) <- gsub("\\.csv$", "", partFileNames)
    # names(df_list) <- gsub("\\.txt$", "", partFileNames)
  }
}


