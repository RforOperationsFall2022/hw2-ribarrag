# load libraries
library(shiny)
library(readr)
library(plotly)
library(shinydashboard)

# load data
consumer_df <- read_delim("consumer_dataset.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

# clean data
consumer_df <- na.omit(consumer_df)


# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Star Wars Dashboard",

# Drop down menus ------------------------------                          
                          
                          
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    

    
    # Birth year Selection ----------------------------------------------
    sliderInput("ageSelect",
                "Select consumer's age range:",
                min = min(consumer_df$Age, na.rm = T),
                max = max(consumer_df$Age, na.rm = T),
                value = c(min(consumer_df$Age, na.rm = T), max(consumer_df$Age, na.rm = T)),
                step = 1)

    # # Inputs: select variables to plot ----------------------------------------------
    # selectInput("worldSelect",
    #             "Homeworld:",
    #             choices = sort(unique(starwars.load$homeworld)),
    #             multiple = TRUE,
    #             selectize = TRUE,
    #             selected = c("Naboo", "Tatooine"))
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Mass", plotlyOutput("plot_mass")),
                   tabPanel("Height", plotlyOutput("plot_height")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    starwars <- starwars.load %>%
      
      # Slider Filter ----------------------------------------------
    filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    
    # Homeworld Filter ----------------------------------------------
    if (length(input$worldSelect) > 0 ) {
      starwars <- subset(starwars, homeworld %in% input$worldSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(starwars)
  })
  
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  
  # A plot showing the mass of characters -----------------------------
  output$plot_mass <- renderPlotly({
    dat <- subset(mwInput(), variable == "mass")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  
  # A plot showing the height of characters -----------------------------------
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
  })
  
  # Mass mean info box ----------------------------------------------
  output$mass <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$mass, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  
  # Height mean value box ----------------------------------------------
  output$height <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$height, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
