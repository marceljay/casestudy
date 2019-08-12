# title: "Case Study Documentatation"
# author: "IDA Group 7"
# date: "July 2019"


# Installation of dependencies
if( !require(readr)){
  install.packages("readr")
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
library(lubridate)


#Import Fahrzeuge_OEM1_Typ12.csv matching the required format 

df21 <- read_csv("C:/casestudy/Fahrzeuge/Daten/Fahrzeuge_OEM2_Typ21.csv")





# Delete unnecessary column "origin"
df21$origin <- NULL


# Delete unnecessary column "Fehlerhaft"
df21$Fehlerhaft <- NULL


#Check if X1 == X1_1
if (sum(!df21$X1 == df21$X1_1) == 0) {
  # Delete X1_1
  df21$X1_1 <- NULL
}


#Store all counted days in vector
daycount <- df21$Produktionsdatum_Origin_01011970



# Reformat date from data frame to fit as.Date
  betterDates <- as.Date(daycount, origin = "1970-01-01")

  
  
# Add date column with correctly formatted dates
df21$Produktionsdatum <- betterDates


# Delete Column Produktionsdatum_Origin_01011970 
df21$Produktionsdatum_Origin_01011970 <- NULL



View(df21)


