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


#Import Fahrzeuge_OEM1_Typ11.csv matching the required format 
Fahrzeuge_OEM1_Typ11 <- read_csv("C:/casestudy/Fahrzeuge/Daten/Fahrzeuge_OEM1_Typ11.csv")

df11 <- Fahrzeuge_OEM1_Typ11 

# Delete unnecessary column "Fehlerhaft"
df11$Fehlerhaft <- NULL


#Check if X1 == X1_1
if (sum(!df11$X1 == df11$X1_1) == 0) {
  # Delete X1_1
  df11$X1_1 <- NULL
}


View(df11)




