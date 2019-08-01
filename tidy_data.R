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

#Import Data
path <- "Data/Einzelteil/Einzelteil_T04.csv"

df <- read_csv2(path)
df2 <- df

df2$Produktionsdatum_Origin_01011970 <- replace(df$Produktionsdatum_Origin_01011970, is.numeric(df$Produktionsdatum_Origin_01011970), toString(dmy("01-01-1970")+days(df$Produktionsdatum_Origin_01011970)) ) 




