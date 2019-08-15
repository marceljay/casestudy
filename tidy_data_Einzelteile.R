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
if( !require(dplyr)){
  install.packages("dplyr")
}
if( !require(tidyverse)){
  install.packages("tidyverse")
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
library(dplyr)
library(tidyverse)
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

#Function to tidy a dataframe previously imported from a TXT file.
#Return a tidy dataframe
tidyTXT_a <- function(df){
  daycount <- df$Produktionsdatum_Origin_01011970
  
  if (length(unique(df$origin))==1) {
    # Reformat date from data frame to fit as.Date
    betterDates <- as.Date(daycount, origin = "1970-01-01")
  } else {
    print("Aborting, multiple values found!")
  }
  
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

tidyTXT_1 <- function(path){
  readLines(path) %>%
    gsub(pattern = "\\| \\|", replace = "\\|",.) %>%
    gsub(pattern = '(?<=[^\\|]) "', replace = '\n"',.,perl = TRUE) %>%
    writeLines(con = "backup.txt")
  
  df <- read.table("backup.txt", sep="|", header=TRUE) %>%
    
    
    # combine .x .y cols into one 
    unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y", "Produktionsdatum", sep="_")%>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", "Herstellernummer", sep="_")%>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", "Werksnummer", sep="_")%>%
    unite("ID", "ID_T01.x", "ID_T01.y", "ID_T01", sep="_")%>%
    unite("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y", "Fehlerhaft", sep="_")%>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", "Fehlerhaft_Datum", sep="_")%>%
    unite("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y", "Fehlerhaft_Fahrleistung", sep="_")
  
  # Clean newly united col names from NA
  df$Produktionsdatum <- gsub(pattern="_ NA|NA_ ",replace="",x=df$Produktionsdatum)
  df$Herstellernummer <- gsub(pattern="_ NA|NA_ ",replace="",x=df$Herstellernummer)
  df$Werksnummer <- gsub(pattern="_ NA|NA_ ",replace="",x=df$Werksnummer)
  df$ID <- gsub(pattern="_ NA|NA _",replace="",x=df$ID)
  df$Fehlerhaft <- gsub(pattern="_ NA|NA _",replace="",x=df$Fehlerhaft)
  df$Fehlerhaft_Datum <- gsub(pattern="_ NA|NA _",replace="",x=df$Fehlerhaft_Datum )
  df$Fehlerhaft_Fahrleistung <- gsub(pattern="_ NA|NA _",replace="",x=df$Fehlerhaft_Fahrleistung)
  
  
  # Deleting rows that shall be disregarded because of date range
  df <- subset(df, !Produktionsdatum<" 2015-01-01 ")
  df <- subset(df, !Produktionsdatum>" 2016-12-31 ")
  
  return(df)
}

tidyTXT_2 <- function(path){
  readLines(path) %>%
    gsub(pattern = '(?<=")\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=A)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=[^-]\\d0)\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '(?<=[^-][\\d|\\.][0-9])\\s+"(?=[0-9][^-])', replace = '\n"', ., perl = TRUE)  %>%
    writeLines(., con = "backup.txt")
  
  einzelteil_2 <- read.table("backup.txt", header=TRUE) %>%
    
    # combine .x .y cols into one 
    unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y",sep="_") %>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", sep="_") %>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", sep="_") %>%
    unite("ID", "ID_T02.x", "ID_T02.y", sep="_") %>%
    unite("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y", sep="_") %>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", sep="_") %>%
    unite("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y", sep="_")
  
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

tidyTXT_3 <- function(path){
  readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    gsub(pattern = '\\|', replace = ',', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt",sep = ",", header=TRUE) %>%
    tidyTXT_a()
  
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

tidyTXT_9 <- function(path){
  readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    gsub(pattern = '\\\\', replace = ',', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt",sep = ",", header=TRUE)%>%
    
    
    # combine .x .y cols into one 
    unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y",sep="_") %>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", sep="_") %>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", sep="_")%>%
    unite("ID", "ID_T09.x", "ID_T09.y", sep="_")%>%
    unite("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y", sep="_")%>%
    unite("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y", sep="_")%>%
    dunite("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y", sep="_")
  
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

tidyTXT_11 <- function(path){
  readLines(path) %>%
    gsub(pattern = '', replace = '\n', .) %>%
    writeLines(., con = "backup.txt")
  
  df <- read.table("backup.txt", header=TRUE)%>%
    tidyTXT_a()
  
  return(df)
}

tidyTXT_16 <- function(path){
  readLines(path) %>%
    gsub(pattern = '(?<!\\|)\\s+"(?! \\|)', replace = '\n"', ., perl = TRUE) %>%
    gsub(pattern = 'A[^|]"', replace = 'A\n"', .) %>%
    gsub(pattern = '0[^|0-9]"', replace = '0\n"', .) %>%
    gsub(pattern = '""', replace = '"\n"', .) %>%
    gsub(pattern = "\\| \\|", replace = " ", .) %>%
    writeLines(., con = "Einzelteil/teste.txt")
  
  df <- read.table("backup.txt", header=TRUE)
  
  
  # combine .x .y cols into one 
  unite("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y", "Produktionsdatum", sep="_") %>%
    unite("Herstellernummer", "Herstellernummer.x", "Herstellernummer.y", "Herstellernummer", sep="_") %>%
    unite("Werksnummer", "Werksnummer.x", "Werksnummer.y", "Werksnummer", sep="_") %>%
    unite("ID", "ID_T16.x", "ID_T16.y", "ID_T16", sep="_") %>%
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

einzelteil_01 <- tidyTXT_01("Data/Einzelteil/Einzelteil_T01.txt")
einzelteil_02 <- tidyTXT_02("Data/Einzelteil/Einzelteil_T02.txt")
einzelteil_03 <- tidyTXT_03("Data/Einzelteil/Einzelteil_T03.txt")
einzelteil_07 <- tidyTXT_07("Data/Einzelteil/Einzelteil_T07.txt")
einzelteil_09 <- tidyTXT_09("Data/Einzelteil/Einzelteil_T09.txt")
einzelteil_11 <- tidyTXT_11("Data/Einzelteil/Einzelteil_T11.txt")
einzelteil_16 <- tidyTXT_16("Data/Einzelteil/Einzelteil_T16.txt")
einzelteil_20 <- tidyTXT_20("Data/Einzelteil/Einzelteil_T20.txt")
einzelteil_22 <- tidyTXT_22("Data/Einzelteil/Einzelteil_T22.txt")
einzelteil_24 <- tidyTXT_24("Data/Einzelteil/Einzelteil_T24.txt")
einzelteil_27 <- tidyTXT_27("Data/Einzelteil/Einzelteil_T27.txt")
einzelteil_31 <- tidyTXT_31("Data/Einzelteil/Einzelteil_T31.txt")
einzelteil_34 <- tidyTXT_34("Data/Einzelteil/Einzelteil_T34.txt")
einzelteil_35 <- tidyTXT_35("Data/Einzelteil/Einzelteil_T35.txt")
einzelteil_36 <- tidyTXT_36("Data/Einzelteil/Einzelteil_T36.txt")
einzelteil_39 <- tidyTXT_39("Data/Einzelteil/Einzelteil_T39.txt")

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


