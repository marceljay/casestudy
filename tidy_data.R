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

#Import Einzelteil.csv matching the required format 
path <- "Data/Einzelteil/Einzelteil_T04.csv"

df <- read_csv2(path)
df2 <- df

#Store all counted days in vector
daycount <-df$Produktionsdatum_Origin_01011970

#Check if origin has a single unique value
if (length(unique(df$origin))==1) {
  # Reformat date from data frame to fit as.Date
  betterDates <- as.Date(daycount, origin = "1970-01-01")
} else {
  print("Aborting, multiple values found!")
}

# Add date column with correctly formatted dates
df2$date <- betterDates

### Tidy: Deleting Columns

#Check if X1 == X1_1
if (sum(!df$X1 == df$X1_1) == 0) {
  # Delete X1_1
  df2$X1_1 <- NULL
}

# Drop previously date-related columns
df2$Produktionsdatum_Origin_01011970 <- NULL
df2$origin <- NULL
