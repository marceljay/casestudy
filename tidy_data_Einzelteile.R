# title: "Case Study Documentatation"
# author: "IDA Group 7"
# date: "August 2019"


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
library(tidyr)



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



tidyTXT_39 <- function(path){
  x <- readLines(path)  %>%
    gsub(pattern = '""', replace = '"\n"', .)  %>%
    gsub(pattern = '(?<!\\\\)"(?!\\\\|")', replace = '\n"', ., perl = TRUE)  %>%
    gsub(pattern = '\\\\', replace = ' ', .)  %>%
  df <- read.table(., header = TRUE) %>%
    
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

einzelteil_01 <- tidyTXT_1("Data/Einzelteil/Einzelteil_T01.txt")
einzelteil_02 <- tidyTXT_2("Data/Einzelteil/Einzelteil_T02.txt")
einzelteil_03 <- tidyTXT_3("Data/Einzelteil/Einzelteil_T03.txt")
einzelteil_07 <- tidyTXT_7("Data/Einzelteil/Einzelteil_T07.txt")
einzelteil_09 <- tidyTXT_9("Data/Einzelteil/Einzelteil_T09.txt")
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



