library(RMySQL)
library(shinydashboard)
library(DT)
library(readr)
library(shinythemes)
library(ijtiff)
library(raster)
library(imager)
library(shiny)
library(shinyWidgets)
library(DBI)
library(dplyr)           # For data manipulation
library(ggplot2)         # For drawing plots

mydat <- read.csv("datset.csv", stringsAsFactors = FALSE)  

# Create connection
makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
dbtrigger <- makereactivetrigger()
con = dbConnect(MySQL(), user = 'root', password = 'viki@123',
                dbname = 'real_estate', host = 'localhost')



ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage(
                  "REAL ESTATE MANAGEMENT SYSTEM",
                  tabsetPanel(
                    tabPanel("Home" , tabName = "Home", icon = icon("house-user"),
                             mainPanel("Home",
                                       h1("DREAM REAL ESTATE!"),
                                       h2("Fresh estate with a fresh environment,your Dream real estate! "),
                                       h3("Dream real!Dream real estate!")
                             ),
                    ),
                    
                    
                    
                    # Navbar 1, tabPanel
                    tabPanel("Value", tabName = "Value", icon = icon("list-alt"),
                             fluidPage(
                               fluidRow(
                                 sidebarPanel(h2("heaven rooms"),width = 100,
                                              textInput("Yearly_price", "Enter yearly price: "),
                                              textInput("Num_checks", "Enter no of checks: "),
                                              textInput("Num_bedrooms", "Enter no of bedrooms: "),
                                              textInput("Area_sqft", "Enter the area square feet: "),
                                              textInput("Description", "Enter the description: "),
                                 ),
                                 
                                 actionButton('writetoreview', 'Save'),
                                 tableOutput('reviewtable'),
                                 
                                 mainPanel(
                                   h1("Thank you for Dream Villa!"),
                                   
                                   h4("Please confirm your villa Values! !!"),
                                   
                                   
                                 )
                                 
                               )
                             ),),
                    tabPanel("Analysis",tabName="Analysis",icon = icon("table"),
                             sidebarLayout(
                               sidebarPanel(
                                 
                                 # Filter Input for Rental Price Range
                                 sliderInput("priceInput",                  # Name of input
                                             "Price Range",                 # Display Label
                                             min = 135000,                  # Lowest Value of Range
                                             max = 450000,                  # Highest Value of Range
                                             value = c(135000, 450000),     # Pre-selected values
                                             pre = "AED ",                  # Unit to display
                                             step = 5000),                  # Size per step change
                                 
                                 # Filter Input for Area in sqft
                                 sliderInput("areaInput",                   # Name of input
                                             "Area Range",                  # Display Label
                                             min = 2000,                    # Lowest Value of Range
                                             max = 15000,                   # Highest Value of Range
                                             value = c(2000, 15000),        # Pre-selected values
                                             step = 100),                   # Size per step change
                                 
                                 # Filter Input for Number of Bedrooms
                                 sliderInput("bedsInput",                   # Name of input
                                             "Bedroom Range",               # Display Label
                                             min = 0,                       # Lowest Value of Range
                                             max = 5,                       # Highest Value of Range
                                             value = c(0, 5)),              # Pre-selected values
                                 
                                 # Select if number of beds should be color coded
                                 checkboxInput("bedsColorInput",            # Name of input
                                               "Show Bedrooms",             # Display Label
                                               value = TRUE),               # Pre-selected value
                                 
                                 # Choose Model to fit from Dropdown Menu
                                 selectInput("model",                       # Name of input
                                             "Model Type",                  # Display Label
                                             choices = c("None" = "none",   # Available choices in the dropdown
                                                         "Linear" = "lm",
                                                         "Smooth" = "smooth"))
                               ),
                               
                               # Items to show in the Main Panel
                               mainPanel(
                                 
                                 # Show Scatterplot
                                 plotOutput("scatterPlot")
                               )
                             )
                    ),
                    tabPanel("Help & Support", tabName = "Help", icon = icon("phone"),
                             h1("For further info contact us on:"),
                             h1("phone no:7689564567 "),
                             h1("Email: realestate@gmail.com")),
                    tabPanel("About us" , tabName = "About", icon = icon("award"),
                             h1("To fastly register the heaven villa!,unveiling the box of your lucky HOME "))
                    
                    
                    
                  )
                )
)

# navbarPage



# Define server function  
server <- function(input, output) {
  
  
  output$reviewout <- renderText({
    paste0("Your yearly price: ",input$Yearly_price ,"\nYourchecks: ",input$Num_checks, "\nno of bedrooms: ",input$Num_bedrooms,"\nArea square feet: ",input$Area_sqft,"\nenter villa model:",input$Description, sep = "\n" )
  })
  
  
  reviewinshiny <- reactive({
    dbtrigger$depend()
    dbGetQuery(con, 'SELECT * from layout')
  })
  
  
  observeEvent(input$writetoreview,{
    
    sql2 = "INSERT INTO layout (Yearly_price, Num_checks,Num_bedrooms ,Area_sqft,Description) VALUES (?Yearly_price, ?Num_checks, ?Num_bedrooms, ?Area_sqft, ?Description)"
    sql4 <- sqlInterpolate(con, sql2, Yearly_price=input$Yearly_price, Num_checks=input$Num_checks, Num_bedrooms = input$Num_bedrooms,Area_sqft = input$Area_sqft,Description = input$Description)
    dbExecute(con, sql4)
    dbtrigger$trigger()
  })
  
  
  output$reviewtable <- renderTable({
    reviewinshiny()
  })
  # Define the Plot UI output
  output$scatterPlot <- renderPlot({
    
    # Define my own variables
    minPrice <- input$priceInput[1]
    maxPrice <- input$priceInput[2]
    
    # Filter data based on user input
    filtered <- mydat %>%
      filter(Yearly_price >= input$priceInput[1],
             Yearly_price <= input$priceInput[2],
             Area_sqft >= input$areaInput[1],
             Area_sqft <= input$areaInput[2],
             Num_bedrooms >= input$bedsInput[1],
             Num_bedrooms <= input$bedsInput[2]
      )
    
    # XY Scatter Plot, X = Area, Y = Price
    ## Color Code the bedroom numbers
    if (input$bedsColorInput == TRUE) {
      g <- ggplot(filtered, aes(x = Area_sqft, y = Yearly_price, color = Num_bedrooms)) +
        geom_point(size = 5, alpha = 0.5) +
        theme(legend.position="bottom")
    }
    
    ## without bedroom number color coding
    else {
      g <- ggplot(filtered, aes(x = Area_sqft, y = Yearly_price)) +
        geom_point(size = 5, alpha = 0.5)
    }
    
    # Plot design elements: title, scale labels etc.
    g <- g + labs(
      title = "Real Estate Data",
      subtitle = paste0("Prices from ", formatC(minPrice, big.mark = ","), 
                        " to ", formatC(maxPrice, big.mark = ","), " AED"),
      caption = "Source: various real estate websites"
    ) +
      xlab("Area in sqft") + ylab("Yearly Rent in AED") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma)
    
    # Display Model Fit (Line through data)
    ## Linear Model Fit
    if (input$model == "lm") {
      g <- g + geom_smooth(method = "lm") 
    }
    
    ## Smooth Model Fit
    else if (input$model == "smooth") {
      g <- g + geom_smooth(method = "loess")
    }
    
    # Display the Plot
    g
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)