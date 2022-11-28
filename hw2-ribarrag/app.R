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
consumer_df <- consumer_df %>%
  mutate(MntPurchases = rowSums(across(c(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds))))


# Avoid plotly issues ----------------------------------------------
# pdf(NULL)



header <- dashboardHeader(
    title = 'My dashboard for HW3')
  
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem('Products sold', tabName = 'Products', icon = icon('tree')), 
      menuItem('Sales channels', tabName = 'Channels', icon = icon('truck')),
      sliderInput("ageSelect",
                  "Select consumer's age range:",
                  min = min(consumer_df$Age), max = max(consumer_df$Age),
                  value = c(min(consumer_df$Age), max(consumer_df$Age)),
                  step = 1)
    )
  )

body <-  dashboardBody(tabItems(
  tabItem("Products",
          fluidRow(
            infoBoxOutput("income_box", width = 10),
          ),
          fluidRow(
            valueBoxOutput("meat_prds", width = 3), 
            valueBoxOutput("fish_prds", width = 3), 
            valueBoxOutput("fruit_prds", width = 3)
          ),
          fluidRow(
            valueBoxOutput("wine_prds", width = 3), 
            valueBoxOutput("sweet_prds", width = 3), 
            valueBoxOutput("gold_prds", width = 3)
          ),
    ),
  
  tabItem("Channels",
          fluidRow(
            valueBoxOutput("store_box", width = 3), 
            valueBoxOutput("web_box", width = 3), 
            valueBoxOutput("catalogue_box", width = 3)
          ),
          
          fluidRow(
            box(plotOutput('first_plot'), width = 10, align = 'center')
          ),
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
  
  # Meat purchases Info Box
  output$meat_prds <- renderValueBox({
    percent_meat <- round(mean(data()$MntMeatProducts / data()$MntPurchases, na.rm = TRUE), 3)
    percent_meat_ALL <- round(mean(consumer_df$MntMeatProducts / consumer_df$MntPurchases, na.rm = TRUE), 3)
    # if the mean purchases of meat for the subset is greater than the average meat purchases for the whole data, then green color the box
    if (percent_meat > percent_meat_ALL){
    valueBox('Percentage purchases of meat products', value = paste0(percent_meat * 100, "%"), icon = icon('drumstick-bite'), color = 'green')
    }
    # if the mean purchases of meat for the subset is lower than the average meat purchases for the whole data, then red color the box
    else if (percent_meat < percent_meat_ALL){
      valueBox('Percentage purchases of meat products', value = paste0(percent_meat * 100, "%"), icon = icon('drumstick-bite'), color = 'red')
    }
    # if they are both the same, then color ir blue
    else {
      valueBox('Percentage purchases of meat products', value = paste0(percent_meat * 100, "%"), icon = icon('drumstick-bite'), color = 'blue')
    }
  })
  
  # Fish purchases Info Box
  output$fish_prds <- renderValueBox({
    percent_fish <- round(mean(data()$MntFishProducts / data()$MntPurchases, na.rm = TRUE), 3)
    if (percent_fish > .3){
      valueBox('Percentage purchases of fish products', value = paste0(percent_fish * 100, "%"), icon = icon('fish'), color = 'red')
    }
    else if (percent_fish < .3){
      valueBox('Percentage purchases of fish products', value = paste0(percent_fish * 100, "%"), icon = icon('fish'), color = 'blue')
    }
  })
  
  # Fruits purchases Info Box
  output$fruit_prds <- renderValueBox({
    percent_fruit <- round(mean(data()$MntFruits / data()$MntPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases of fruits products', value = paste0(percent_fruit * 100, "%"), icon = icon('apple'))
  })

  # Wine purchases Info Box
  output$wine_prds <- renderValueBox({
    percent_wine <- round(mean(data()$MntWines / data()$MntPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases of wine products', value = paste0(percent_wine * 100, "%"), icon = icon('wine-glass'))
  })
  
  # Sweet purchases Info Box
  output$sweet_prds <- renderValueBox({
    percent_sweet <- round(mean(data()$MntSweetProducts / data()$MntPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases of sweet products', value = paste0(percent_sweet * 100, "%"), icon = icon('ice-cream'))
  })
  
  # Other purchases Info Box
  output$gold_prds <- renderValueBox({
    percent_other <- round(mean(data()$MntGoldProds / data()$MntPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases of other products', value = paste0(percent_other * 100, "%"), icon = icon('basket-shopping'))
  })
  

  
  # Plot 1
  output$first_plot <- renderPlot({
    plot(consumer_df$Income, consumer_df$MntMeatProducts)
  })
}

ui <- dashboardPage(header, sidebar, body)

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
