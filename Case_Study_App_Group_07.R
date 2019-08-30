# Shiny App


# Dependencies


if( !require(plotly)){
  install.packages("plotly")
}

if( !require(shiny)){
  install.packages("shiny")
}

if( !require(colourpicker)){
  install.packages("colourpicker")
}




library(shiny)
library(colourpicker)
library(plotly)




# Create user interface
ui <- fluidPage(
  # Create a heading
  h1(strong("Introduction to Engineering Data Analytics with R")),
  h2("shiny app Group 7"),
  # Create tabs
  tabsetPanel(
    
    # Create tab for input
    tabPanel(
      title = "input",
      
      # Create a sidebar layout
      sidebarLayout(
        
        # Create a sidebar panel
        sidebarPanel(
          # Create a heading
          h3("basic input options"),
          
          # Create a group of check boxes to select vehicle types
          checkboxGroupInput(inputId = "vehicle_type", label = "vehicle type selection:",
                             choiceNames = c("11","12","21","22"),
                             choiceValues = levels(master_df_shiny$vehicle_type), selected = "11"),
          
          # Create a checkbox for switching between single and separate boxplot display
          checkboxInput(inputId = "separation", label = "display selected vehicle types in a single boxplots", value = FALSE),
          
          # Create radio buttons for switching between vehicle type and OEM factory boxplot display
          radioButtons(inputId = "select", label = "divide boxplot by:", choices = c("vehicle type", "OEM factory"), selected = "vehicle type")
          
          
        ),
        # Create a main panel
        mainPanel(
          # Create a heading
          h3("additional input options"),
          
          # Create a color input for the boxplot
          colourInput( inputId = "boxplot_color", label = "boxplot color:", value  = "black"),
          # Create a color input for the boxplot background
          colourInput( inputId = "background_color", label = "boxplot background color:", value  = "#e5e5e5"),
          
          # Create a slider input for the production time range
          sliderInput(inputId = "prod_time_range", label = "select production time range:",
                      min = min(master_df_shiny$prod_time), max = max(master_df_shiny$prod_time), value = c(min(master_df_shiny$prod_time), max(master_df_shiny$prod_time)))
        )
      )
    ),
    # Create tab for boxplot
    tabPanel(
      title = "boxplot",
      plotlyOutput("plot", width = "600px", height = "600px")
    ),
    
    # Create tab for reactive tabel
    tabPanel(
      title = "table",
      dataTableOutput("table")
    )
  )
)







# Define the server logic
server <- function(input, output) {
  
  
  # Create a reactive variable called filtered_data()
  filtered_data <- reactive({
    
    data <- master_df_shiny
    
    # Determine subset of data which is selected by production time range slider
    data <- subset(data, prod_time >= input$prod_time_range[1]
                   & prod_time <= input$prod_time_range[2]
    )
    
    # Determine subset of vehicle type which is selected by check boxes
    data <- subset(data, vehicle_type %in% input$vehicle_type)
    
    
    data
    
  })
  output$plot <- renderPlotly({
    
    data <- filtered_data()
    
    
    ggplotly({
      
      # Creating a boxplot for single boxplot display of vehicle types
      p <- ggplot(data, aes(vehicle_type, prod_time)) +
        geom_boxplot(color = input$boxplot_color)
      
      # Add labeling and color option to the plot
      p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time [day]") +
        theme(panel.background = element_rect(fill = input$background_color))
      
      
      
      # if command to select between seperated boxplots and single boxplot view
      if (input$separation == TRUE) {
        
        p <- ggplot(data, aes(y = prod_time)) +
          geom_boxplot(color = input$boxplot_color)
        
        # Add labeling and color option to the plot
        p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time [day]") +
          theme( axis.text.x = element_blank(), panel.background = element_rect(fill = input$background_color))
        
        
      }
      
      # if command to select between vehicle type and OEM factory view
      if (input$select == "OEM factory" ) {
        p <- ggplot(data, aes(vehicle_prod_factory, prod_time)) +
          geom_boxplot(color = input$boxplot_color) +
          theme(panel.background = element_rect(fill = input$background_color))
        
        # Add labeling and  to the plot
        p <- p + labs(title = "divided by OEM factory", x = "OEM factory", y = "production time [day]")
      }
      
      
      p
    })
  })
  output$table <- renderDataTable({
    
    data <- filtered_data()
    
    
    # Rename column names
    names(data)[1] <- "vehicle global id"
    names(data)[2] <- "production time"
    names(data)[3] <- "vehicle production factory"
    names(data)[4] <- "vehicle type"
    
    data
    
    
    
  })
  
  
  
}

# Combine ui and server into a shiny app and run it
shinyApp(ui = ui, server = server)

```