library(shiny)
library(dplyr)
library(faraway)
library(car)
library(leaps)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Regression Diagnostics"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file", "Dataset (.csv)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      tags$hr(),
      uiOutput("responsechoice"),
      uiOutput("predictorschoice")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", 
                           fluidRow(
                             column(width = 12, sprintf("If no dataset is provided, the savings dataset from the faraway package is used."))
                           ),
                           fluidRow(
                             column(width = 12, verbatimTextOutput("summary"))
                           )
                           ),
                  tabPanel("Standard Diagnostics",
                           fluidRow(
                             column(width = 12, sprintf("What plot(lm...) yields."))
                           ),
                           fluidRow(
                             column(width = 12, plotOutput("standardplot"))
                           )
                           ),
                  tabPanel("Outlier Detection",
                           withMathJax(),
                           sprintf("Cook's distance measures how the fitted values change when the ith observation is deleted, in essence how much \\(\\hat{\\beta}\\) and \\(\\hat{\\beta}_{[i]}\\) differ. 
                                    If observation i has a large \\(C_i\\) it is probably an outlier."),
                           sprintf("$$C_i = \\frac{\\left(\\hat \\beta - \\hat \\beta_{[i]}\\right)^T\\left(X^TX\\right)\\left(\\hat \\beta - \\hat \\beta_{[i]}\\right)}{\\left(p+1\\right)\\hat{\\sigma}^2} = \\frac{r_i^2}{p+1} \\frac{h_i}{1−h_i}$$"),
                           fluidRow(
                             column(width = 12, plotOutput("leverage")),
                             column(width = 12, plotOutput("cooks"))
                           )),
                  tabPanel("Nonlinearity",
                           fluidRow(
                             column(width = 12, sprintf("Parital residual plots for 4 random predictors."))
                           ),
                           fluidRow(
                             column(width = 12, plotOutput("partialres"))
                           )
                           ),
                  tabPanel("Feature Selection",
                           fluidRow(
                             column(width = 12, sprintf("Backwards elimination with p-value cutoff of 0.15."))
                           ),
                           fluidRow(
                             column(width = 12, verbatimTextOutput("features"))
                           )),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  #Update table and linear model
  
  df <- reactive({
    if(is.null(input$file)) {
      savings
    } else {
      read.csv(input$file$datapath, header = input$header, sep = input$sep)
    }
  })
  
  linearmodel <- reactive({
    resp <- input$response
    pred <- input$predictors
    columns <- c(resp, pred)
    filtered <- df() %>% select(columns)
    # If no predictors are specified, regress on the intercept term alone.
    if(length(columns) == 1) {
      formula <- paste(resp, "~", "1")
      model <- lm(data = filtered, formula)
    }
    # Else regress on every predictor.
    else {
      vars_concat <- paste(pred, collapse = "+")
      formula <- paste(resp, "~", vars_concat)
      model <- lm(data = filtered, formula)
    }
    #This allows me to access the table, or the model via list indexing.
    list(table = df(), model = model)
  })
  
  output$responsechoice <- renderUI({
    columns <- colnames(df())
    selectInput("response", "Choose Response variable", columns)
  })
  
  output$predictorschoice <- renderUI({
    columns <- colnames(df())
    resp <- input$response
    allowed_columns <- columns[!(columns %in% resp)]
    checkboxGroupInput("predictors", "Choose Predictors", columns, selected = allowed_columns)
  })
  
  output$summary <- renderPrint({
    summary(linearmodel()$model)
  })
  
  output$standardplot <- renderPlot({
    par(mfrow=c(2,2))
    plot(linearmodel()$model)
  })
  
  output$leverage <- renderPlot({
    leverages <- influence(linearmodel()$model)$hat
    rowname <- rownames(linearmodel()$table)
    plot(leverages, main = 'Leverage')
    abline(h = mean(leverages))
    leverages.sorted <- sort(leverages, decreasing=T, index.return=T)
    for(i in leverages.sorted$ix[1:2]) {
      text(i, leverages[i]-0.02, rowname[i])
    }
  })
  
  output$cooks <- renderPlot({
    cooks <- cooks.distance(linearmodel()$model)
    rowname <- rownames(linearmodel()$table)
    plot(cooks, main = 'Cooks Distance')
    abline(h = mean(cooks))
    cooks.sorted <- sort(cooks, decreasing=T, index.return=T)
    for(i in cooks.sorted$ix[1:2]) {
      text(i, cooks[i]-0.02, rowname[i])
    }
  })
  
  output$partialres <- renderPlot({
    selected_predictors <- sample(input$predictors, 4)
    par(mfrow = c(2,2))
    #Select random 4 predictors to examine 
    crPlot(linearmodel()$model, variable = selected_predictors[1])
    crPlot(linearmodel()$model, variable = selected_predictors[2])
    crPlot(linearmodel()$model, variable = selected_predictors[3])
    crPlot(linearmodel()$model, variable = selected_predictors[4])
  })
  
  #Backward eliniation feature selection.
  output$features <- renderPrint({
    best_model <- paste(input$response, "~.")
    current <- lm(best_model, data = df())
    cutoff <- 0.15
    
    repeat {
      max_p <- max(summary(current)$coefficients[,4])
      if (max_p < cutoff) {
        break
      } else {
        eliminated <- names(which.max(summary(current)$coefficients[,4]))
        best_model <- paste0(best_model, "- ", eliminated)
        current <- lm(best_model, data = df())
      }
    }
    best_backward <- formula(current)
    summary(current)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(linearmodel()$table)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)