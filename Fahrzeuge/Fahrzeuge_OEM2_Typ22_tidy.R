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

df22 <-  read_delim("C:/casestudy/Fahrzeuge/Daten/Fahrzeuge_OEM2_Typ22.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)




#Check if X1 == X1_1
if (sum(!df22$X1 == df22$X1_1) == 0) {
  # Delete X1_1
  df22$X1_1 <- NULL
}



# Delete unnecessary column five "Fehlerhaft"
df22$Fehlerhaft <- NULL





#Store all counted days in vector
daycount <- df22$Produktionsdatum_Origin_01011970



# Reformat date from data frame to fit as.Date
betterDates <- as.Date(daycount, origin = "1970-01-01")



# Add date column with correctly formatted dates
df22$Produktionsdatum <- betterDates


# Delete Column Produktionsdatum_Origin_01011970 
df22$Produktionsdatum_Origin_01011970 <- NULL


# Delete unnecessary column eight "origin"
df22$origin <- NULL


View(df22)
