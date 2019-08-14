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

df_B11 <- read_csv2("Case Study/Fahrzeuge/Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
df_B12 <- read_csv2("Case Study/Fahrzeuge/Bestandteile_Fahrzeuge_OEM1_Typ12.csv")
df_B21 <- read_csv2("Case Study/Fahrzeuge/Bestandteile_Fahrzeuge_OEM2_Typ21.csv")
df_B22 <- read_csv2("Case Study/Fahrzeuge/Bestandteile_Fahrzeuge_OEM2_Typ21.csv")

df_F11 <- read_csv("Case Study/Fahrzeuge/Fahrzeuge_OEM1_Typ11.csv")
df_F12 <- read_csv2("Case Study/Fahrzeuge/Fahrzeuge_OEM1_Typ12.csv")
df_F21 <- read_csv("Case Study/Fahrzeuge/Fahrzeuge_OEM2_Typ21.csv")
df_F22 <- read_csv2("Case Study/Fahrzeuge/Fahrzeuge_OEM2_Typ22.csv")





### Semicolon seperation: 

# Bestandteile_Fahrzeuge_OEM1_Typ11: already tidy
# Bestandteile_Fahrzeuge_OEM1_Typ12: already tidy
# Bestandteile_Fahrzeuge_OEM2_Typ21: already tidy
# Bestandteile_Fahrzeuge_OEM2_Typ22: already tidy

# Fahrzeuge_OEM1_Typ12: x1 = x_1, drop Fehlerhaft
# Fahrzeuge_OEM2_Typ22: Produktionsdatum, origin, x1 = x_1, drop Fehlerhaft



### comma seperation:

# Fahrzeuge_OEM1_Typ11: x1 = x_1, Fehlerhaft
# Fahrzeuge_OEM2_Typ21: Produktionsdatum, origin, x1 = x_1, Fehlerhaft


list_A <- list(df_F11, df_F12) 



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






list_B <- list(df_F22, df_F21)




# Drop unnecessary column "Fehlerhaft"


View(df_F11)


