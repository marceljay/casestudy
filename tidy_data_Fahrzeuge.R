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
library(tidyr)




# Importing Data

df_B11 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
df_B12 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv")
df_B21 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv")
df_B22 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv")

df_F11 <- read_csv("Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv")
df_F12 <- read_csv2("Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv")
df_F21 <- read_csv("Data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv")
df_F22 <- read_csv2("Data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv")



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




#Add column Produktionsstandort to Bestandteile_Fahrzeuge_OEM1_Typ11 

df_B11$Produktionsstandort <- df_B11$ID_Fahrzeug


df_B11$Produktionsstandort <- gsub(pattern = ".*-11-.*" , replacement = "Nuernberg" , df_B11$Produktionsstandort)
df_B11$Produktionsstandort <- gsub(pattern = ".*-12-.*" , replacement = "Bonn" , df_B11$Produktionsstandort)




#Add column Produktionsstandort to Bestandteile_Fahrzeuge_OEM1_Typ12 

df_B12$Produktionsstandort <- df_B12$ID_Fahrzeug


df_B12$Produktionsstandort <- gsub(pattern = ".*-12-.*" , replacement = "Bonn" , df_B12$Produktionsstandort)




#Add column Produktionsstandort to Bestandteile_Fahrzeuge_OEM1_Typ21 

df_B21$Produktionsstandort <- df_B21$ID_Fahrzeug


df_B21$Produktionsstandort <- gsub(pattern = ".*-21-.*" , replacement = "Goettingen" , df_B21$Produktionsstandort)




#Add column Produktionsstandort to Bestandteile_Fahrzeuge_OEM1_Typ22

df_B22$Produktionsstandort <- df_B22$ID_Fahrzeug


df_B22$Produktionsstandort <- gsub(pattern = ".*-21-.*" , replacement = "Goettingen" , df_B22$Produktionsstandort)




#Add column Produktionsstandort to Bestandteile_Fahrzeuge_OEM1_Typ22

df_B22$Produktionsstandort <- df_B22$ID_Fahrzeug


df_B22$Produktionsstandort <- gsub(pattern = ".*-21-.*" , replacement = "Goettingen" , df_B22$Produktionsstandort)




#Add column Produktionsstandort to Fahrzeuge_OEM1_Typ11

df_F11$Produktionsstandort <- df_F11$ID_Fahrzeug

df_F11$Produktionsstandort <- gsub(pattern = ".*-11-.*" , replacement = "Nuernberg" , df_F11$Produktionsstandort)
df_F11$Produktionsstandort <- gsub(pattern = ".*-12-.*" , replacement = "Bonn" , df_F11$Produktionsstandort)




#Add column Produktionsstandort to Fahrzeuge_OEM1_Typ12

df_F12$Produktionsstandort <- df_F12$ID_Fahrzeug

df_F12$Produktionsstandort <- gsub(pattern = ".*-12-.*" , replacement = "Bonn" , df_F12$Produktionsstandort)



#Add column Produktionsstandort to Fahrzeuge_OEM1_Typ21

df_F21$Produktionsstandort <- df_F21$ID_Fahrzeug

df_F21$Produktionsstandort <- gsub(pattern = ".*-21-.*" , replacement = "Goettingen" , df_F21$Produktionsstandort)


#Add column Produktionsstandort to Fahrzeuge_OEM1_Typ22

df_F22$Produktionsstandort <- df_F22$ID_Fahrzeug

df_F22$Produktionsstandort <- gsub(pattern = ".*-21-.*" , replacement = "Goettingen" , df_F22$Produktionsstandort)
df_F22$Produktionsstandort <- gsub(pattern = ".*-22-.*" , replacement = "Regensburg" , df_F22$Produktionsstandort)




# Merge data frames Bestandteile_Fahrzeuge_OEM_Typxx

BE_vehicle_df <- rbind(df_B11, df_B12, df_B21,df_B21)


# Delete unnecessary column Bestandteile_Fahrzeuge_OEM_Typxx
BE_vehicle_df$X1 <- NULL


# Gather Bestandteile columns
BE_vehicle_df <- gather(BE_vehicle_df, Bestandteile, ID_Bestandteile, -ID_Fahrzeug, -Produktionsstandort)


# Delete unnecessary column Bestandteile from Bestandteile_Fahrzeuge_OEM_Typxx
BE_vehicle_df$Bestandteile <- NULL


# Rename columns in Fahrzeuge_OEM2
names(BE_vehicle_df)[1] <- "vehicle_global_id"
names(BE_vehicle_df)[2] <- "vehicle_prod_factory"
names(BE_vehicle_df)[3] <- "comp_global_id"


# reorder by column name
BE_vehicle_df <- BE_vehicle_df[c("vehicle_global_id", "comp_global_id", "vehicle_prod_factory")]
# Remove redundant col
BE_vehicle_df$vehicle_prod_factory <- NULL


# Merge data frames Fahrzeuge_OEM1
vehicle_df <- rbind(df_F11, df_F12, df_F21, df_F22)




# Delete unnecessary columns Fahrzeuge_OEM2

vehicle_df$Herstellernummer <- NULL
vehicle_df$Werksnummer <- NULL
vehicle_df$Fehlerhaft_Datum <- NULL
vehicle_df$Fehlerhaft_Fahrleistung <- NULL
vehicle_df$X1 <- NULL



# Rename columns in Fahrzeuge_OEM2
names(vehicle_df)[1] <- "vehicle_global_id"
names(vehicle_df)[2] <- "vehicle_prod_date"
names(vehicle_df)[3] <- "vehicle_prod_factory"




# View tidy data frames

View(BE_vehicle_df)
View(vehicle_df)
