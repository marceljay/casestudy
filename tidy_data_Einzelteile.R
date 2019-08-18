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
# T38 - 16 col
# T30 - 23 col

# TXT  with pattern A (clean)
list_TXT_A <- c("T03", "T07", "T11" )

# TXT  with pattern A (long, dirty)
list_TXT_B <- c("T01", "T02", "T09", "T16", "T20", "T22", "T24")


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
  df <- df[c(3:5, 11)]
  
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
  df <- df[3:6]

  # Check for NA values
  importAnalysis(df)

  return(df)
}


# Unite, rename, filter dates
tidyLong <- function(df) {
  print("tidyLong called!")
  
  # Unite related columns, since after some row number, values appear in different columns
  df <- unite(df, "prod_date", contains("Produktionsdatum"), sep="_")
  df <- unite(df, "oem",  contains("Herstellernummer"), sep="_")
  df <- unite(df, "factory", contains("Werksnummer"), sep="_")
  df <- unite(df, "global_id", contains("ID_T"), sep="_") 
  
  # Clean newly united col names from NA
  df$prod_date <- gsub(pattern="_NA|NA_", replace="", x=df$prod_date)
  df$oem <- gsub(pattern="_NA|NA_", replace="", x=df$oem)
  df$factory <- gsub(pattern="_NA|NA_", replace="", x=df$factory)
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
    print("WARNING! Multiple values found!")
  }
  
  # Add date column with correctly formatted dates
  df$prod_date <- betterDates
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !prod_date<"2015-01-01")
  df <- subset(df, !prod_date>"2016-12-31")

  return(df)
}

# Function to assign the correct import and tidy function to a .txt file
importTXT <- function(path) {
  # print("---- called tidyTXT_a ----")
  
    if (str_detect(path, "T01.txt")) {
      tidyTXT_1(path)
      print("tidyTXT_1 called")
      
    } else if(str_detect(path, "T02.txt")) {
      tidyTXT_2(path)
      print("tidyTXT_2 called")
      
    } else if(str_detect(path, "T03.txt")) {
      tidyTXT_3(path)
      print("tidyTXT_3 called")
    
    } else if(str_detect(path, "T07.txt")) {
      tidyTXT_7path)
      print("tidyTXT_7 called")
      
    } else if(str_detect(path, "T09.txt")) {
      tidyTXT_9(path)
      print("tidyTXT_9 called")
      
    } else if(str_detect(path, "T11.txt")) {
      tidyTXT_11(path)
      print("tidyTXT_11 called")

    } else if(str_detect(path, "T16.txt")) {
      tidyTXT_16(path)
      print("tidyTXT_16 called")
      
    } else if(str_detect(path, "T20.txt")) {
      tidyTXT_20(path)
      print("tidyTXT_20 called")
      
    } else if(str_detect(path, "T22.txt")) {
      tidyTXT_22(path)
      print("tidyTXT_22 called")
      
    } else if(str_detect(path, "T24.txt")) {
      tidyTXT_24(path)
      print("tidyTXT_24 called")
      
    } else if(str_detect(path, "T27.txt")) {
      tidyTXT_27(path)
      print("tidyTXT_27 called")
      
    } else if(str_detect(path, "T31.txt")) {
      tidyTXT_31(path)
      print("tidyTXT_31 called")
      
    } else if(str_detect(path, "T34.txt")) {
      tidyTXT_34(path)
      print("tidyTXT_34 called")
      
    } else if(str_detect(path, "T35.txt")) {
      tidyTXT_35(path)
      print("tidyTXT_35 called")
      
    } else if(str_detect(path, "T36.txt")) {
      tidyTXT_36(path)
      print("tidyTXT_36 called")
      
    } else if(str_detect(path, "T39.txt")) {
      tidyTXT_2(path)
      print("tidyTXT_39 called")
    }  
  
}
    

# Structure: wide (dirty)
tidyTXT_1 <- function(path) {

  x <- readLines(path) %>%
      gsub(pattern = "\\| \\|", replace = "\\|",.) %>%
      gsub(pattern = '(?<=[^\\|]) "', replace = '\n"',.,perl = TRUE)
    
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), sep="|", header=TRUE)
  }
    
  df <- tidyLong(df)
  
  return(df)
}

# Structure: wide (dirty)
tidyTXT_2 <- function(path){
  
  readLines(path) %>%
    gsub(pattern = '(?<=")\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=A)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=[^-]\\d0)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=[^-][\\d|\\.][0-9])\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header=TRUE) %>%
  
  # x <-readLines(path) %>%
  #   gsub(pattern = '(?<=")\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
  #   gsub(pattern = '(?<=A)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
  #   gsub(pattern = '(?<=[^-]\\d0)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
  #   gsub(pattern = '(?<=[^-][\\d|\\.][0-9])\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)
  #   
  #   for (i in 2:length(x) ) {
  #     df <- read.table(textConnection(x[i]), sep="|", header=TRUE)
  #   }
    
  # df <- tidyLong(df)
  
  return(df)
}

tidyTXT_3 <- function(path){
  x <- readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    gsub(pattern = '\\|', replace = ',', .) 
  
  for (i in 2:length(x) ) {
    df <- read.table(textConnection(x[i]), sep="|", header=TRUE)
  }
  
  # df <- read.table("backup.txt", sep = ",", header=TRUE)
  df <- tidyDate(df)
  
  # # Drop columns (prod_date was appended as 11th column)
  df <- df[c(2:4, 10)]
  
  # Renaming cols
  # names(df)[1] <- "id"  # probably not needed
  names(df)[1] <- "global_id"
  names(df)[2] <- "oem"
  names(df)[3] <- "factory"
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}

tidyTXT_7 <- function(path){
  readLines(path) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    writeLines(., con = "backup.txt") 
  
  df <- read.table("backup.txt", header = TRUE)%>%
    tidyTXT_a()
  
  return(df)
}

# Structure: wide (dirty)
tidyTXT_9 <- function(path){
  readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    gsub(pattern = '\\\\', replace = ',', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt",sep = ",", header=TRUE)
    
  df  <- tidyLong(df)
  
  # Drop columns except the 4 necessary ones
  df <- df[2:5]
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}

tidyTXT_11 <- function(path){
  readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header=TRUE)%>%
    tidyTXT_a()
  
  return(df)
}

# Structure: wide (dirty)
tidyTXT_16 <- function(path){
  readLines(path) %>%
    gsub(pattern = '(?<!\\|)\\s+"(?! \\|)', replace = '\n"', ., perl = TRUE) %>%
    gsub(pattern = 'A[^|]"', replace = 'A\n"', .) %>%
    gsub(pattern = '0[^|0-9]"', replace = '0\n"', .) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    gsub(pattern = "\\| \\|", replace = " ", .) %>%
    writeLines(., con = "Einzelteil/teste.txt")
  
  df <- read.table("backup.txt", header=TRUE)
  
  df  <- tidyLong(df)
  
  # Drop columns except the 4 necessary ones
  df <- df[2:5]
  
  # Check for NA values
  importAnalysis(df)
  
  return(df)
}

tidyTXT_20 <- function(path){
  readLines(path) %>%
    gsub(pattern = '" "', replace = '"\n"', .) %>%
    gsub(pattern = '[^\\|] "', replace = '\n"', .) %>%
    gsub(pattern = "\\| \\|", replace = " ", .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt",  header=TRUE)%>%
    tidyTXT_a()
  
  return(df)
}

# Structure: wide (dirty)
tidyTXT_22 <- function(path){
  readLines(path) %>%
    gsub(pattern = '(?<!\\s)"(?!\\s)', replace = '\n"', ., perl = TRUE) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header = TRUE)
  
  # combine .x .y cols into one 
  unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y", "Produktionsdatum", sep="_") %>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", "Herstellernummer", sep="_") %>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", "Werksnummer", sep="_") %>%
    unite("ID", "ID_T22.x", "ID_T22.y", "ID_T22", sep="_") %>%
    unite("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y", "Fehlerhaft", sep="_") %>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", "Fehlerhaft_Datum", sep="_") %>%
    unite("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y", "Fehlerhaft_Fahrleistung.", sep="_")
  
  # Clean newly united col names from NA
  df$Produktionsdatum <- gsub(pattern="_NA|NA_",replace="",x=df$Produktionsdatum)
  df$Herstellernummer <- gsub(pattern="_NA|NA_",replace="",x=df$Herstellernummer)
  df$Werksnummer <- gsub(pattern="_NA|NA_",replace="",x=df$Werksnummer)
  df$ID <- gsub(pattern="_NA|NA_",replace="",x=df$ID)
  df$Fehlerhaft <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft)
  df$Fehlerhaft_Datum <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Datum )
  df$Fehlerhaft_Fahrleistung <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Fahrleistung)
  
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !Produktionsdatum<"2015-01-01")
  df <- subset(df, !Produktionsdatum>"2016-12-31")
  
  return(df)
}

# Structure: wide (dirty)
tidyTXT_24 <- function(path){
  readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header=TRUE) %>%
    
    # combine .x .y cols into one 
    unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y", "Produktionsdatum", sep="_") %>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", "Herstellernummer", sep="_") %>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", "Werksnummer", sep="_") %>%
    unite("ID", "ID_T24.x", "ID_T24.y", "ID_T24", sep="_") %>%
    unite("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y", "Fehlerhaft", sep="_") %>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", "Fehlerhaft_Datum", sep="_") %>%
    unite("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y", "Fehlerhaft_Fahrleistung", sep="_")
  
  # Clean newly united col names from NA
  df$Produktionsdatum <- gsub(pattern="_NA|NA_",replace="",x=df$Produktionsdatum)
  df$Herstellernummer <- gsub(pattern="_NA|NA_",replace="",x=df$Herstellernummer)
  df$Werksnummer <- gsub(pattern="_NA|NA_",replace="",x=df$Werksnummer)
  df$ID <- gsub(pattern="_NA|NA_",replace="",x=df$ID)
  df$Fehlerhaft <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft)
  df$Fehlerhaft_Datum <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Datum )
  df$Fehlerhaft_Fahrleistung <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Fahrleistung)
  
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !Produktionsdatum<"2015-01-01")
  df <- subset(df, !Produktionsdatum>"2016-12-31")
  
  return(df)
}

tidyTXT_27 <- function(path){
  readLines(path) %>%
    gsub(pattern = '(?<=origin)\\W+(?=[0-9])', replace = '"\n"', ., perl = TRUE) %>%
    gsub(pattern = '(?<=01-1970)"(?=[0-9])', replace = '\n"', ., perl = TRUE) %>%
    gsub(pattern = "\\| \\|", replace = "\\|", .) %>%
    writeLines(., con = "backup.txt")
  
  
  df <- read.table("backup.txt", header = TRUE)
  
  
  daycount <- df$Produktionsdatum_Origin_01011970
  
  betterDates <- as.Date(daycount, origin = "1970-01-01")
  
  
  # Add date column with correctly formatted dates
  df$date <- betterDates
  
  # Drop previously date-related columns
  df$Produktionsdatum_Origin_01011970 <- NULL
  df$origin <- NULL
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !date<"2015-01-01")
  df <- subset(df, !date>"2016-12-31")
  
  return(df)
}

tidy_TXT31 <- function(path){
  readLines(path) %>%
    gsub(pattern = '(?<!\\s)"(?=[0-9])', replace = '"\n"', ., perl = TRUE) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header = TRUE)
  
  df <- df[1:9]
  tidyTXT_a(df)
}

# Structure: wide (dirty)
tidyTXT_35 <- function(path){
  readLines(path) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    gsub(pattern = '(?<!\\\\)"(?!\\\\|")', replace = '\n"', ., perl = TRUE) %>%
    gsub(pattern = '\\\\', replace = ' ', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header = TRUE) %>%
    
    # combine .x .y cols into one 
    unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y",sep="_") %>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", sep="_") %>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", sep="_") %>%
    unite("ID", "ID_T35.x", "ID_T35.y", sep="_") %>%
    unite("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y", sep="_") %>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", sep="_") %>%
    unite("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y.", sep="_")
  
  # Clean newly united col names from NA
  df$Produktionsdatum <- gsub(pattern="_NA|NA_",replace="",x=df$Produktionsdatum)
  df$Herstellernummer <- gsub(pattern="_NA|NA_",replace="",x=df$Herstellernummer)
  df$Werksnummer <- gsub(pattern="_NA|NA_",replace="",x=df$Werksnummer)
  df$ID <- gsub(pattern="_NA|NA_",replace="",x=df$ID)
  df$Fehlerhaft <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft)
  df$Fehlerhaft_Datum <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Datum )
  df$Fehlerhaft_Fahrleistung <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Fahrleistung)
  
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !Produktionsdatum<"2015-01-01")
  df <- subset(df, !Produktionsdatum>"2016-12-31")
  
  return(df)
}

tidyTXT_36 <- function(path){
  readLines(path) %>%
    gsub(pattern = '" "', replace = '"\n"', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header = TRUE)%>%
    tidyTXT_a()
  
  return(df)
}


# Structure: wide (totally messed up, SPECIAL CASE)
tidy_TXT39 <- function(path){
  readLines(path)  %>%
    gsub(pattern = '""', replace = '"\n"', .)  %>%
    gsub(pattern = '(?<!\\\\)"(?!\\\\|")', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '\\\\', replace = ' ', .)  %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header = TRUE) %>%
    
    # combine .x .y cols into one 
    unite("ID", "Produktionsdatum.x", "Produktionsdatum.y", sep="_") %>%
    unite("Produktionsdatum", "Herstellernummer.x", "Herstellernummer.y",sep="_") %>%
    unite("Herstellernummer", "Werksnummer.x", "Werksnummer.y", sep="_") %>%
    unite("Werksnummer", "Fehlerhaft.x", "Fehlerhaft.y", sep="_") %>%
    unite("Fehlerhaft", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", sep="_") %>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y.", sep="_") %>%
    unite("Fehlerhaft_Fahrleistung", "ID_T39.y", "X.", sep="_")
  
  # Clean newly united col names from NA
  df$Produktionsdatum <- gsub(pattern="_NA|NA_",replace="",x=df$Produktionsdatum)
  df$Herstellernummer <- gsub(pattern="_NA|NA_",replace="",x=df$Herstellernummer)
  df$Werksnummer <- gsub(pattern="_NA|NA_",replace="",x=df$Werksnummer)
  df$ID <- gsub(pattern="_0|0_",replace="",x=df$ID)
  df$Fehlerhaft <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft)
  df$Fehlerhaft_Datum <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Datum )
  df$Fehlerhaft_Fahrleistung <- gsub(pattern="_NA|NA_",replace="",x=df$Fehlerhaft_Fahrleistung)
  
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !Produktionsdatum<"2015-01-01")
  df <- subset(df, !Produktionsdatum>"2016-12-31")
  
  df$ID_T39.x <- NULL
  
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


