# Adjustment master_df for ShinyApp




# Create column for production time
master_df$prod_time <- master_df$vehicle_prod_date - master_df$part_prod_date


#Put max production time for each vehicle global id in a list
master_list_shiny <- with(master_df, tapply(prod_time, vehicle_global_id, max)) 


#Convert list to a data frame
master_df_shiny <- as.data.frame(master_list_shiny)

# Change index rownames and use vehicle_global id as normal column
master_df_shiny <- cbind(vehicle_global_id = rownames(master_df_shiny), master_df_shiny)
rownames(master_df_shiny) <- 1:nrow(master_df_shiny)

# Rename columns in master_df_shiny data
names(master_df_shiny)[names(master_df_shiny) == "master_list_shiny"] <- "prod_time"



#Add column vehicle_prod_factory to master_df_shiny
master_df_shiny$vehicle_prod_factory <- master_df_shiny$vehicle_global_id

master_df_shiny$vehicle_prod_factory <- gsub(pattern = ".*-11-.*" , replacement = "Nuernberg" , master_df_shiny$vehicle_prod_factory)
master_df_shiny$vehicle_prod_factory <- gsub(pattern = ".*-12-.*" , replacement = "Bonn" ,master_df_shiny$vehicle_prod_factory)
master_df_shiny$vehicle_prod_factory <- gsub(pattern = ".*-21-.*" , replacement = "Goettingen" , master_df_shiny$vehicle_prod_factory)
master_df_shiny$vehicle_prod_factory <- gsub(pattern = ".*-22-.*" , replacement = "Regensburg" , master_df_shiny$vehicle_prod_factory)



# Add column vehicle type to master_df_shiny

master_df_shiny$vehicle_type <- master_df_shiny$vehicle_global_id

master_df_shiny$vehicle_type <- gsub(pattern = "^11-.*" , replacement = "11" , master_df_shiny$vehicle_type)
master_df_shiny$vehicle_type <- gsub(pattern = "^12-.*" , replacement = "12" ,master_df_shiny$vehicle_type)
master_df_shiny$vehicle_type <- gsub(pattern = "^21-.*" , replacement = "21" , master_df_shiny$vehicle_type)
master_df_shiny$vehicle_type <- gsub(pattern = "^22-.*" , replacement = "22" , master_df_shiny$vehicle_type)



#### Adjustments for boxplot


# Convert column vehicle_type to factor
master_df_shiny$vehicle_type <- as.factor(master_df_shiny$vehicle_type)

ALL <- master_df_shiny$vehicle_type


# Create data frame for table
master_df_table <- master_df_shiny



#Delete column vehicle_global_id
master_df_table$vehicle_global_id <- NULL



# Rename columns of master_df_shiny for table
names(master_df_table)[1] <- "production time"
names(master_df_table)[2] <- "OEM factory"
names(master_df_table)[3] <- "vehicle type"




View(master_df_shiny)
View(master_df_table)

