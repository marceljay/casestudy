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

if ( !require(dplyr)){
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





# Importing Data

df_B11 <- read_csv2("Data/Fahrzeuge/Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
df_B12 <- read_csv2("Data/Fahrzeuge/Bestandteile_Fahrzeuge_OEM1_Typ12.csv")
df_B21 <- read_csv2("Data/Fahrzeuge/Bestandteile_Fahrzeuge_OEM2_Typ21.csv")
df_B22 <- read_csv2("Data/Fahrzeuge/Bestandteile_Fahrzeuge_OEM2_Typ21.csv")

df_F11 <- read_csv("Data/Fahrzeuge/Fahrzeuge_OEM1_Typ11.csv")
df_F12 <- read_csv2("Data/Fahrzeuge/Fahrzeuge_OEM1_Typ12.csv")
df_F21 <- read_csv("Data/Fahrzeuge/Fahrzeuge_OEM2_Typ21.csv")
df_F22 <- read_csv2("Data/Fahrzeuge/Fahrzeuge_OEM2_Typ22.csv")



# Seperation and tidying

### Semicolon seperation: 

# Bestandteile_Fahrzeuge_OEM1_Typ11: already tidy
# Bestandteile_Fahrzeuge_OEM1_Typ12: already tidy
# Bestandteile_Fahrzeuge_OEM2_Typ21: already tidy
# Bestandteile_Fahrzeuge_OEM2_Typ22: already tidy

# Fahrzeuge_OEM1_Typ12: x1 = x_1, delete Fehlerhaft
# Fahrzeuge_OEM2_Typ22: adjust Produktionsdatum, delete origin, x1 = x_1, delete Fehlerhaft



### comma seperation:

# Fahrzeuge_OEM1_Typ11: x1 = x_1, delete Fehlerhaft
# Fahrzeuge_OEM2_Typ21: adjust Produktionsdatum, delete origin, x1 = x_1, delete Fehlerhaft




# Tidy df_F11 & df_F12



#Check if X1 == X1_1
if (sum(!df_F11$X1 == df_F11$X1_1) == 0) {
  # Delete X1_1
  df_F11$X1_1 <- NULL
}

# Drop unnecessary column  "Fehlerhaft"
df_F11$Fehlerhaft <- NULL


#Check if X1 == X1_1
if (sum(!df_F12$X1 == df_F12$X1_1) == 0) {
  # Delete X1_1
  df_F12$X1_1 <- NULL
}

# Drop unnecessary column  "Fehlerhaft"
df_F12$Fehlerhaft <- NULL




# Tidy df_F21 & df_F22


#Store all counted days in vector
daycount_F21 <- df_F21$Produktionsdatum_Origin_01011970


# Reformat date from data frame to fit as.Date
betterDates <- as.Date(daycount_F21, origin = "1970-01-01")


# Add date column with correctly formatted dates
df_F21$Produktionsdatum <- betterDates


# Delete Column Produktionsdatum_Origin_01011970 
df_F21$Produktionsdatum_Origin_01011970 <- NULL


# Delete unnecessary column eight "origin"
df_F21$origin <- NULL


#Check if X1 == X1_1
if (sum(!df_F21$X1 == df_F21$X1_1) == 0) {
  # Delete X1_1
  df_F21$X1_1 <- NULL
}


# Drop unnecessary column  "Fehlerhaft"
df_F21$Fehlerhaft <- NULL


########



#Store all counted days in vector
daycount_F22 <- df_F22$Produktionsdatum_Origin_01011970


# Reformat date from data frame to fit as.Date
betterDates <- as.Date(daycount_F22, origin = "1970-01-01")


# Add date column with correctly formatted dates
df_F22$Produktionsdatum <- betterDates


# Delete Column Produktionsdatum_Origin_01011970 
df_F22$Produktionsdatum_Origin_01011970 <- NULL


# Delete unnecessary column eight "origin"
df_F22$origin <- NULL


#Check if X1 == X1_1
if (sum(!df_F22$X1 == df_F22$X1_1) == 0) {
  # Delete X1_1
  df_F22$X1_1 <- NULL
}


# Drop unnecessary column  "Fehlerhaft"
df_F22$Fehlerhaft <- NULL

# View tidy data frames

View(df_B11)
View(df_B12)
View(df_B21)
View(df_B22)
View(df_F11)
View(df_F12)
View(df_F21)
View(df_F22)






