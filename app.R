library(shiny)
library(dplyr)
library(faraway)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Regression Diagnostics"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Checkbox for the predictors to include in linear model.
      checkboxGroupInput("predictors",
                  "Predictors in linear model:",
                  c("pop15", "pop75", "dpi", "ddpi"),
                  selected = c("pop15", "pop75", "dpi", "ddpi")
    
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  #Update table and linear model
  rv <- reactive({
    columns <- c("sr", input$predictors)
    filtered <- savings %>% select(columns)
    if(length(columns) == 1) {
      model <- lm(data = filtered, sr ~ 1)
    }
    else {
      model <- lm(data = filtered, sr ~ .)
    }
    list(table = filtered, model = model)
  })
  
  output$summary <- renderPrint({
    summary(rv()$model)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(rv()$table)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)