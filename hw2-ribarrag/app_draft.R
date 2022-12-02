# load libraries
library(shiny)
library(readr)
library(plotly)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(DT)
library(shinyWidgets)

# load data
consumer_df <- read_delim("consumer_dataset.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

# clean dataset
consumer_df <- na.omit(consumer_df)
consumer_df <- consumer_df[-c(which(consumer_df$Income == max(consumer_df$Income)),which(consumer_df$Year_Birth <= 1900)), ]

# Adjust dataset: create Age column, create Children as sum of kids and teens in the household, 
# group 'uncommon' marital status categories into Other, create Total_Purchases and MntPurschases as the sum of 
# items and amount bought by each customer, create the column Month
consumer_df$Age = 2022 - consumer_df$Year_Birth
consumer_df$Children = consumer_df$Kidhome + consumer_df$Teenhome
consumer_df$Marital_Status[consumer_df$Marital_Status %in% c('Absurd', 'Alone', 'YOLO')] <- 'Other'
consumer_df <- consumer_df %>%
  mutate(TotalPurchases = rowSums(across(c(NumStorePurchases,NumWebPurchases,NumCatalogPurchases))))
consumer_df <- consumer_df %>%
  mutate(MntPurchases = rowSums(across(c(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds))))
consumer_df <- consumer_df %>%
  mutate(Month = month(as.POSIXlt(consumer_df$Dt_Customer, format="%d-%m-%Y")))
consumer_df[, c('Month', "Dt_Customer")]

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


header <- dashboardHeader(
  title = 'HW2 Dashboard')

sidebar <- dashboardSidebar(
  # created three menuItems
  sidebarMenu(
    menuItem('Products sold', tabName = 'Products', icon = icon('scale-unbalanced-flip')), 
    menuItem('Sales channels', tabName = 'Channels', icon = icon('truck')),
    menuItem('Data table', tabName = 'Tables', icon = icon('table')),
    # And then a slider, a checkbox and a Picker for user interaction with the data
    sliderInput("ageSelect",
                "Select consumer's age range:",
                min = min(consumer_df$Age), max = max(consumer_df$Age),
                value = c(min(consumer_df$Age), max(consumer_df$Age)),
                step = 1),
    
    checkboxGroupInput(inputId = "select_education",
                       label = "Include only customers with the following education level",
                       choices = c('2n Cycle', 'Basic', 'Graduation', 'Master', 'PhD' ),
                       selected = c('2n Cycle', 'Basic', 'Graduation', 'Master', 'PhD' )),
    
    pickerInput(inputId = "select_children",
                label = "Only include customers that have these many children:", 
                # If in a new data set, there are customers with more children, the options will be updated
                choices = c(levels(as_factor(consumer_df$Children))), 
                options = list(`actions-box` = TRUE), 
                multiple = T, 
                selected = c(levels(as_factor(consumer_df$Children))))
  )
)

body <-  dashboardBody(tabItems(
  # first tab contains information of sales by type of product
  # one info box, six value boxes and a graph section
  tabItem("Products",
          fluidRow(
            infoBoxOutput("income_box", width = 10)
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
          fluidRow(
            tabBox(title = "Your daily insights for purchases and consumer information",
                   width = 10,
                   tabPanel("Income", plotlyOutput('hist_income'), align = 'center'), 
                   tabPanel("Purchases", plotlyOutput('stackedbar_purchases'), align = 'center'))
                   # tabPanel("Height", plotlyOutput("plot_height")))
          )
  ),
  # second tab, channels, with info about sale channels, three valueboxes and a graph
  tabItem("Channels",
          fluidRow(
            valueBoxOutput("store_box", width = 4), 
            valueBoxOutput("web_box", width = 4), 
            valueBoxOutput("catalog_box", width = 4)
          ),
          
          fluidRow(
            box(title = 'These are sales by month per sale channel', plotlyOutput('sales_channel'), width = 12)
          ),
  ),
  # third tab for the table
  tabItem("Tables", 
          fluidPage((
            box(title = 'The table', DT::dataTableOutput('table'), width = 12)
          )
          )
          )
)
) 


server <- function(input, output){
  
  # Reactivity: filtering the data according to user's selections
  data <- reactive({
    req(input$select_education, input$ageSelect[1], input$ageSelect[2], input$select_children)
    filtered_data <- consumer_df %>%
      
      # Slider Filter ----------------------------------------------
    filter(Age >= input$ageSelect[1] & Age <= input$ageSelect[2]) %>%
      
      filter(Education %in% input$select_education) %>%
    
    filter(Children %in% input$select_children)
    
    return(filtered_data)
  })
  # Creation of InfoBox (1) for the products tab --------------------------------
  # Income Info Box
  output$income_box <- renderInfoBox({
    mean_income <- round(mean(data()$Income), 2)
    infoBox("Average Income of customers", value = paste('$', formatC(mean_income, big.mark = ',', digits = 2 ,format ='f')), subtitle = paste(nrow(data()), "customers"), icon = icon("sack-dollar"), color = "green")
  })
  # Creation of Valueboxes (3) for the channel tab --------------------------------
  # Store purchases Info Box
  output$store_box <- renderValueBox({
    mean_store <- round(mean(data()$NumStorePurchases / data()$TotalPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases made in STORE', value = paste0(mean_store * 100, "%"))
  })
  
  # Web purchases Info Box
  output$web_box <- renderValueBox({
    mean_web <- round(mean(data()$NumWebPurchases / data()$TotalPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases made ONLINE', value = paste0(mean_web * 100, "%"))
  })
  
  # Catalog purchases Info Box
  output$catalog_box <- renderValueBox({
    mean_catalog <- round(mean(data()$NumCatalogPurchases / data()$TotalPurchases, na.rm = TRUE), 3)
    valueBox('Percentage purchases made via CATALOG', value = paste0(mean_catalog * 100, "%"))
  })
  
  # Creation of InfoBoxes (6) for the products tab --------------------------------
  # These change of color depending if the subselection of the user has a vlue that is lower (turns red) or higher (turns) green than the total data set values
  
  # Meat purchases Info Box
  output$meat_prds <- renderValueBox({
    # Getting rid of the missing values, otherwise, problems
    percent_meat <- round(mean(data()$MntMeatProducts / data()$MntPurchases, na.rm = TRUE), 3) %>% replace(is.na(.), 0)
    # percent_meat <- max(!is.na(percent_meat_a), percent_meat_a)
    percent_meat_ALL <- round(mean(consumer_df$MntMeatProducts / consumer_df$MntPurchases, na.rm = TRUE), 3)
    # if the mean purchases of meat for the subset is greater than the average meat purchases for the whole data, then green color the box
    if (percent_meat > percent_meat_ALL){
      valueBox('Percentage purchases of meat products', value = paste0(percent_meat * 100, "%"), icon = icon('drumstick-bite'), color = 'green')
    }
    # if the mean purchases of meat for the subset is lower than the average meat purchases for the whole data, then red color the box
    else if (percent_meat < percent_meat_ALL){
      valueBox('Percentage purchases of meat products', value = paste0(percent_meat * 100, "%"), icon = icon('drumstick-bite'), color = 'red')
    }
    # if they are both the same, then color is blue
    else {
      valueBox('Percentage purchases of meat products', value = paste0(percent_meat * 100, "%"), icon = icon('drumstick-bite'), color = 'teal')
    }
  })
  
  # Fish purchases Info Box
  output$fish_prds <- renderValueBox({
    # Getting rid of the missing values, otherwise, problems
    percent_fish <- round(mean(data()$MntFishProducts / data()$MntPurchases, na.rm = TRUE), 3) %>% replace(is.na(.), 0)
    # percent_fish <- max(!is.na(percent_fish_a), percent_fish_a)
    percent_fish_ALL <- round(mean(consumer_df$MntFishProducts / consumer_df$MntPurchases, na.rm = TRUE), 3)
    if (percent_fish > percent_fish_ALL){
      valueBox('Percentage purchases of fish products', value = paste0(percent_fish * 100, "%"), icon = icon('fish'), color = 'green')
    }
    else if (percent_fish < percent_fish_ALL){
      valueBox('Percentage purchases of fish products', value = paste0(percent_fish * 100, "%"), icon = icon('fish'), color = 'red')
    }
    else{
      valueBox('Percentage purchases of fish products', value = paste0(percent_fish * 100, "%"), icon = icon('fish'), color = 'teal')
    }
  })
  
  # Fruits purchases Info Box
  output$fruit_prds <- renderValueBox({
    # Getting rid of the missing values, otherwise, problems
    percent_fruit <- round(mean(data()$MntFruits / data()$MntPurchases, na.rm = TRUE), 3) %>% replace(is.na(.), 0)
    # percent_fruit <- max(!is.na(percent_fruit_a), percent_fruit_a)
    percent_fruit_ALL <- round(mean(consumer_df$MntFruits / consumer_df$MntPurchases, na.rm = TRUE), 3)
    if (percent_fruit > percent_fruit_ALL){
      valueBox('Percentage purchases of fruits products', value = paste0(percent_fruit * 100, "%"), icon = icon('apple'), color = 'green')
    }
    else if (percent_fruit < percent_fruit_ALL){
      valueBox('Percentage purchases of fruits products', value = paste0(percent_fruit * 100, "%"), icon = icon('apple'), color = 'red')
    }
    else {
      valueBox('Percentage purchases of fruits products', value = paste0(percent_fruit * 100, "%"), icon = icon('apple'), color = 'teal')
    }
  })
  
  # Wine purchases Info Box
  output$wine_prds <- renderValueBox({
    # Getting rid of the missing values, otherwise, problems
    percent_wine <- round(mean(data()$MntWines / data()$MntPurchases, na.rm = TRUE), 3) %>% replace(is.na(.), 0)
    # percent_wine <- max(!is.na(percent_wine_a), percent_wine_a)
    percent_wine_ALL <- round(mean(consumer_df$MntWines / consumer_df$MntPurchases, na.rm = TRUE), 3)
    if (percent_wine > percent_wine_ALL){
      valueBox('Percentage purchases of wine products', value = paste0(percent_wine * 100, "%"), icon = icon('wine-glass'), color = 'green')  
    }
    else if  (percent_wine < percent_wine_ALL){
      valueBox('Percentage purchases of wine products', value = paste0(percent_wine * 100, "%"), icon = icon('wine-glass'), color = 'red')  
    }
    else {
      valueBox('Percentage purchases of wine products', value = paste0(percent_wine * 100, "%"), icon = icon('wine-glass'), color = 'teal')  
    }
  })
  
  # Sweet purchases Info Box
  output$sweet_prds <- renderValueBox({
    # Getting rid of the missing values, otherwise, problems
    percent_sweet <- round(mean(data()$MntSweetProducts / data()$MntPurchases, na.rm = TRUE), 3) %>% replace(is.na(.), 0)
    # percent_sweet <- max(!is.na(percent_sweet_a), percent_sweet_a)
    percent_sweet_ALL <- round(mean(consumer_df$MntSweetProducts / consumer_df$MntPurchases, na.rm = TRUE), 3)
    if (percent_sweet > percent_sweet_ALL){
      valueBox('Percentage purchases of sweet products', value = paste0(percent_sweet * 100, "%"), icon = icon('ice-cream'), color = 'green')  
    }
    else if(percent_sweet < percent_sweet_ALL){
      valueBox('Percentage purchases of sweet products', value = paste0(percent_sweet * 100, "%"), icon = icon('ice-cream'), color = 'red')
    }
    else{
      valueBox('Percentage purchases of sweet products', value = paste0(percent_sweet * 100, "%"), icon = icon('ice-cream'), color = 'teal')
    }
  })
  
  # Other purchases Info Box
  output$gold_prds <- renderValueBox({
    # Getting rid of the missing values, otherwise, problems
    percent_other <- round(mean(data()$MntGoldProds / data()$MntPurchases, na.rm = TRUE), 3) %>% replace(is.na(.), 0)
    # percent_other <- max(!is.na(percent_other_a), percent_other_a)
    percent_other_ALL <- round(mean(consumer_df$MntGoldProds / consumer_df$MntPurchases, na.rm = TRUE), 3)
    if (percent_other > percent_other_ALL){
      valueBox('Percentage purchases of other products', value = paste0(percent_other * 100, "%"), icon = icon('basket-shopping'), color = 'green')
    }
    else if(percent_other < percent_other_ALL){
      valueBox('Percentage purchases of other products', value = paste0(percent_other * 100, "%"), icon = icon('basket-shopping'), color = 'red')
    }
    else{
      valueBox('Percentage purchases of other products', value = paste0(percent_other * 100, "%"), icon = icon('basket-shopping'), color = 'teal')
    }
  })
  
  # Histogram income for Products tab
  output$hist_income <- renderPlotly({
    ggplot(data(), aes(Income)) + 
    geom_histogram(bins = 50) +
      labs(y = 'Consumers') + 
      ggtitle("How many consumers there are per income bin?") +
      # I dont want the x axis to be changing when the user makes selections, so I set the limit to the max possible in the complete dataset
      coord_cartesian(xlim = c(0, max(consumer_df$Income)))
  })
  
  # Lets first melt the data for the plot
  melted_sale_channel_data <- reactive({
    data()[, c('Month', 'NumWebPurchases', 'NumStorePurchases', 'NumCatalogPurchases')] %>%
      melt(id = 'Month')
  })
  
  melted_sale_amount <- reactive({
    data()[, c('Month', "MntWines", "MntFruits", 
               "MntMeatProducts", "MntFishProducts", 
               "MntSweetProducts", "MntGoldProds")] %>%
      melt(id = 'Month')
  })
  
  
  # Stacked bar chart for Products tab
  output$stackedbar_purchases<- renderPlotly({
    req(melted_sale_amount()$variable)
    ggplot(data = melted_sale_amount(), aes(x = Month, fill = variable)) + 
      geom_bar(stat = "count") + 
      ggtitle('How much do people spend in each type of grocery, by month') +
      labs(y = 'Amount ($)', fill = 'Gorcery type ') +
      scale_x_continuous(breaks = 1:12, labels = month.name)
  })
  
  
  # Bar chart for each sales channel for Channels tab
  output$sales_channel <- renderPlotly({
    # requiring this: otherwise graph crashes when user's selection is empty set
    req(melted_sale_channel_data()$variable)

      # Needed to take the goruped sums, otherwise the tooltip from plotly gave me the value for each individual customer instead of the aggregated value
    melted_sale_channel_agg <- melted_sale_channel_data() %>%
      group_by(Month, variable) %>%
      summarise_at(vars(value), list(value = sum))

    ggplot(data = melted_sale_channel_agg, aes(x = Month, y = value, fill = variable)) + 
      labs(y = 'Number of sales') +
      geom_bar(stat = "identity", show.legend = FALSE) + facet_wrap(facets = ~ fct_reorder(variable, -value)) +
      scale_x_continuous(breaks = 1:12, labels = abbreviate(month.name, 2)) +
      ggtitle("How many sales are done in the store, online or thru the catalog, in each month and for the customer profile selected?")
  })
  
  # The table
  output$table <- DT::renderDataTable({
    subset(data(), select = c('ID', 'Income', 'Education',  'MntPurchases', 'Marital_Status', 'Age', 'Children'))
  })
}

ui <- dashboardPage(header, sidebar, body)

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
