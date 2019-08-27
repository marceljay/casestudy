<<<<<<< Updated upstream
# shiny app
if( !require(shiny)){
  install.packages("shiny")
}

if( !require(plotly)){
  install.packages("plotly")
}

if( !require(colourpicker)){
  install.packages("colourpicker")
}

library(shiny)
library(plotly)
library(colourpicker)



ui <- fluidPage(
  
  h1(strong("Introduction to Engineering Data Analytics with R")),
  h2("shiny app Group 7"),
  
  # Create tabs
  tabsetPanel(
    
    # Create tab for input
    tabPanel(
      title = "input",
      
      # Create a sidebar layout
      sidebarLayout(
        
        sidebarPanel(
          h3("basic input options"),
          
          
          checkboxGroupInput(inputId = "vehicle_type", label = "vehicle type selection:",
                             choiceNames = c("11","12","21","22"),
                             choiceValues = levels(master_df_shiny$vehicle_type), selected = "11"),
          
          
          checkboxInput(inputId = "separation", label = "display selected vehicle types in a single boxplots", value = FALSE),
          
          
          radioButtons(inputId = "select", label = "divide boxplot by:", choices = c("vehicle type", "OEM factory"),
                       selected = "vehicle type")
          
          
        ),
        
        mainPanel(
          h3("additional input options"),
          colourInput( inputId = "boxplot_color", label = "boxplot color:", value  = "black"),
          
          colourInput( inputId = "background_color", label = "boxplot background color:", value  = "#e5e5e5"),
          
          
          sliderInput(inputId = "prod_time_range", label = "select production time range:", 
                      min = 0, max = max(master_df_shiny$prod_time), value = c(0, max(master_df_shiny$prod_time)))
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
  
  
  
  filtered_data <- reactive({
    
    data <- master_df_shiny
    
    
    data <- subset(data, prod_time >= input$prod_time_range[1] 
                   & prod_time <= input$prod_time_range[2]
    )
    
    
    data <- subset(data, vehicle_type == input$vehicle_type)
    
    
    data
    
  })
  
  
  
  output$plot <- renderPlotly({
    
    data <- filtered_data()
    
    
    ggplotly({
      
      
      p <- ggplot(data, aes(vehicle_type, prod_time)) +
        geom_boxplot(color = input$boxplot_color, outlier.shape  = input$shape)
      
      p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time") +
        theme(panel.background = element_rect(fill = input$background_color))
      
      
      
      
      if (input$separation == TRUE) {
        
        p <- ggplot(data, aes(y = prod_time)) +
          geom_boxplot(color = input$boxplot_color, outlier.shape  = input$shape)
        p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time") +
          theme( axis.text.x = element_blank(), panel.background = element_rect(fill = input$background_color))
        
        
      }
      
      
      if (input$select == "OEM factory" ) {
        p <- ggplot(data, aes(vehicle_prod_factory, prod_time)) +
          geom_boxplot(color = input$boxplot_color,  outlier.shape  = input$shape) +
          theme(panel.background = element_rect(fill = input$background_color))
        p <- p + labs(title = "divided by OEM factory", x = "OEM factory", y = "production time")
      }
      
      
      p 
    })
  })
  
  output$table <- renderDataTable({
    
    data <- filtered_data()
    
    
    
    names(data)[1] <- "vehicle global id"
    names(data)[2] <- "production time"
    names(data)[3] <- "vehicle production factory"
    names(data)[4] <- "vehicle type"
    
    data
    
    
    
  })
  
  
  
}

=======

# shiny app
if ( !require(shiny)){
  install.packages("shiny")
}

if ( !require(plotly)){
  install.packages("plotly")
}

if ( !require(colourpicker)){
  install.packages("colourpicker")
}





ui <- fluidPage(
  
  h1(strong("Introduction to Engineering Data Analytics with R")),
  h2("shiny app Group 7"),
  
  # Create tabs
  tabsetPanel(
    
    # Create tab for input
    tabPanel(
      title = "input",
      
      # Create a sidebar layout
      sidebarLayout(
        
        sidebarPanel(
          h3("basic input options"),
          
          
          checkboxGroupInput(inputId = "vehicle_type", label = "vehicle type selection:",
                             choiceNames = c("11","12","21","22"),
                             choiceValues = levels(master_df_shiny$vehicle_type), selected = "11"),
          
          
          checkboxInput(inputId = "separation", label = "display selected vehicle types in a single boxplots", value = FALSE),
          
          
          radioButtons(inputId = "select", label = "divide boxplot by:", choices = c("vehicle type", "OEM factory"),
                       selected = "vehicle type")
          
          
        ),
        
        mainPanel(
          h3("additional input options"),
          colourInput( inputId = "boxplot_color", label = "boxplot color:", value  = "black"),
          
          colourInput( inputId = "background_color", label = "boxplot background color:", value  = "#e5e5e5"),
          
          
          sliderInput(inputId = "prod_time_range", label = "select production time range:", 
                      min = 0, max = max(master_df_shiny$prod_time), value = c(0, max(master_df_shiny$prod_time)))
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
  
  
  
  filtered_data <- reactive({
    
    data <- master_df_shiny
    
    
    data <- subset(data, prod_time >= input$prod_time_range[1] 
                   & prod_time <= input$prod_time_range[2]
    )
    
    
    data <- subset(data, vehicle_type == input$vehicle_type)
    
    
    data
    
  })
  
  
  
  output$plot <- renderPlotly({
    
    data <- filtered_data()
    
    
    ggplotly({
      
      
      p <- ggplot(data, aes(vehicle_type, prod_time)) +
        geom_boxplot(color = input$boxplot_color, outlier.shape  = input$shape)
      
      p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time [day]") +
        theme(panel.background = element_rect(fill = input$background_color))
      
      
      
      
      if (input$separation == TRUE) {
        
        p <- ggplot(data, aes(y = prod_time)) +
          geom_boxplot(color = input$boxplot_color, outlier.shape  = input$shape)
        p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time [day]") +
          theme( axis.text.x = element_blank(), panel.background = element_rect(fill = input$background_color))
        
        
      }
      
      
      if (input$select == "OEM factory" ) {
        p <- ggplot(data, aes(vehicle_prod_factory, prod_time)) +
          geom_boxplot(color = input$boxplot_color,  outlier.shape  = input$shape) +
          theme(panel.background = element_rect(fill = input$background_color))
        p <- p + labs(title = "divided by OEM factory", x = "OEM factory", y = "production time [day]")
      }
      
      
      p 
    })
  })
  
  output$table <- renderDataTable({
    
    data <- filtered_data()
    
    
    
    names(data)[1] <- "vehicle global id"
    names(data)[2] <- "production time"
    names(data)[3] <- "vehicle production factory"
    names(data)[4] <- "vehicle type"
    
    data
    
    
    
  })
  
  
  
}


# shiny app




if ( !require(shiny)) {
  install.packages("shiny")
}

if ( !require(plotly)) {
  install.packages("plotly")
}

if ( !require(colourpicker)) {
  install.packages("colourpicker")
}




ui <- fluidPage(
  
  h1(strong("Introduction to Engineering Data Analytics with R")),
  h2("shiny app Group 7"),
  
  # Create tabs
  tabsetPanel(
    
    # Create tab for input
    tabPanel(
      title = "input",
      
      # Create a sidebar layout
      sidebarLayout(
        
        sidebarPanel(
          h3("basic input options"),
          
          
          checkboxGroupInput(inputId = "vehicle_type", label = "vehicle type selection:",
                             choiceNames = c("11","12","21","22"),
                             choiceValues = levels(master_df_shiny$vehicle_type), selected = "11"),
          
          
          checkboxInput(inputId = "separation", label = "display selected vehicle types in a single boxplots", value = FALSE),
          
          
          radioButtons(inputId = "select", label = "divide boxplots by:", choices = c("vehicle type", "OEM factory"),
                       selected = "vehicle type")
          
          
        ),
        
        mainPanel(
          h3("additional input options"),
          colourInput( inputId = "boxplot_color", label = "boxplot color:", value  = "black"),
          
          colourInput( inputId = "background_color", label = "boxplot background color:", value  = "#e5e5e5"),
          
          
          sliderInput(inputId = "prod_time_range", label = "select production time range:", 
                      min = 0, max = max(master_df_shiny$prod_time), value = c(0, max(master_df_shiny$prod_time)))
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
  
  
  
  filtered_data <- reactive({
    
    data <- master_df_shiny
    
    
    data <- subset(data, prod_time >= input$prod_time_range[1] 
                   & prod_time <= input$prod_time_range[2]
    )
    
    
    data <- subset(data, vehicle_type == input$vehicle_type)
    
    
    data
    
  })
  
  
  
  output$plot <- renderPlotly({
    
    data <- filtered_data()
    
    
    ggplotly({
      
      
      p <- ggplot(data, aes(vehicle_type, prod_time)) +
        geom_boxplot(color = input$boxplot_color, outlier.shape  = input$shape)
      
      p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time [day]") +
        theme(panel.background = element_rect(fill = input$background_color))
      
      
      
      
      if (input$separation == TRUE) {
        
        p <- ggplot(data, aes(y = prod_time)) +
          geom_boxplot(color = input$boxplot_color, outlier.shape  = input$shape)
        p <- p + labs(title = "vehicle type & production time", x = "vehicle type", y = "production time [day]") +
          theme( axis.text.x = element_blank(), panel.background = element_rect(fill = input$background_color))
        
        
      }
      
      
      if (input$select == "OEM factory" ) {
        p <- ggplot(data, aes(vehicle_prod_factory, prod_time)) +
          geom_boxplot(color = input$boxplot_color,  outlier.shape  = input$shape) +
          theme(panel.background = element_rect(fill = input$background_color))
        p <- p + labs(title = "divided by OEM factory", x = "OEM factory", y = "production time [day]")
      }
      
      
      p 
    })
  })
  
  output$table <- renderDataTable({
    
    data <- filtered_data()
    
    
    
    names(data)[1] <- "vehicle global id"
    names(data)[2] <- "production time"
    names(data)[3] <- "vehicle production factory"
    names(data)[4] <- "vehicle type"
    
    data
    
    
    
  })
  
  
  
}

>>>>>>> Stashed changes
shinyApp(ui = ui, server = server)