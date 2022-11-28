# load libraries
library(shiny)
library(readr)
library(plotly)
library(shinydashboard)
library(dplyr)


# load data
consumer_df <- read_delim("consumer_dataset.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

# clean dataset
consumer_df <- na.omit(consumer_df)
consumer_df <- consumer_df[-c(which(consumer_df$Income == max(consumer_df$Income)),which(consumer_df$Year_Birth <= 1900)), ]

# Adjust dataset
consumer_df$Age = 2022 - consumer_df$Year_Birth
consumer_df$Children = consumer_df$Kidhome + consumer_df$Teenhome
consumer_df$Marital_Status[consumer_df$Marital_Status %in% c('Absurd', 'Alone', 'YOLO')] <- 'Other'
consumer_df <- consumer_df %>%
  mutate(TotalPurchases = rowSums(across(c(NumStorePurchases,NumWebPurchases,NumCatalogPurchases))))


# Avoid plotly issues ----------------------------------------------
# pdf(NULL)



header <- dashboardHeader(
    title = 'My dashboard for HW3')
  
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem('Graphs', tabName = 'Graphs', icon = icon('tree')), 
      menuItem('Table', tabName = 'Table', icon = icon('truck')),
      sliderInput("ageSelect",
                  "Select consumer's age range:",
                  min = min(consumer_df$Age), max = max(consumer_df$Age),
                  value = c(min(consumer_df$Age), max(consumer_df$Age)),
                  step = 1)
    )
  )

body <-  dashboardBody(tabItems(
  tabItem("Graphs",
          fluidRow(
            infoBoxOutput("income_box", width = 10),
          ),
          
          fluidRow(
            valueBoxOutput("store_box", width = 3), 
            valueBoxOutput("web_box", width = 3), 
            valueBoxOutput("catalogue_box", width = 3)
          ),
          
          fluidRow(
            box(plotOutput('first_plot'), width = 10, align = 'center')
          ),
          
    ),
  
  tabItem("Table",
    # add table
  )
  )
) 
  



server <- function(input, output){
  
  # Reactivity
  
  data <- reactive({
    filtered_data <- consumer_df %>%
      
      # Slider Filter ----------------------------------------------
    filter(Age >= input$ageSelect[1] & Age <= input$ageSelect[2])
    
    # Homeworld Filter ----------------------------------------------
    # if (length(input$worldSelect) > 0 ) {
    #   starwars <- subset(starwars, homeworld %in% input$worldSelect)
    # }
    
    return(filtered_data)
  })
  
  # Income Info Box
  output$income_box <- renderInfoBox({
    mean_income <- round(mean(data()$Income), 0)
    infoBox("Average Income of customers", value = mean_income, subtitle = paste(nrow(data()), "customers"), icon = icon("sack-dollar"), color = "green")
  })
  
  # Store purchases Info Box
  output$store_box <- renderValueBox({
    mean_store <- round(mean(data()$NumStorePurchases / data()$TotalPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases made in store', value = paste0(mean_store * 100, "%"))
  })
  
  # Web purchases Info Box
  output$web_box <- renderValueBox({
    mean_web <- round(mean(data()$NumWebPurchases / data()$TotalPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases made online', value = paste0(mean_web * 100, "%"))
  })
  
  # Catalogue purchases Info Box
  output$catalogue_box <- renderValueBox({
    mean_catalogue <- round(mean(data()$NumCatalogPurchases / data()$TotalPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases made via catalogue', value = paste0(mean_catalogue * 100, "%"))
  })
  
  
  
  
  # 
  # Plot 1
  output$first_plot <- renderPlot({
    plot(consumer_df$Income, consumer_df$MntMeatProducts)
  })
}

ui <- dashboardPage(header, sidebar, body)

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
