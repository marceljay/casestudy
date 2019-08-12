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
Bestandteile_Fahrzeuge_OEM2_Typ22 <- read_delim("C:/Users/sinan/OneDrive/Desktop/Uni/IDA/Gruppenarbeit/Fahrzeug/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv", 
                                                ";", escape_double = FALSE, trim_ws = TRUE)

df22 <- Bestandteile_Fahrzeuge_OEM2_Typ22



View(df22)




