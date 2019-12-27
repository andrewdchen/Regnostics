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
                  tabPanel("Standard Diagnostics",
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
                             column(width = 12, plotOutput("partialres"))
                           )
                           ),
                  tabPanel("Feature Selection", verbatimTextOutput("features")),
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
    # If no predictors are specified, regress on the intercept term alone.
    if(length(columns) == 1) {
      model <- lm(data = filtered, sr ~ 1)
    }
    # Else regress on every predictor.
    else {
      model <- lm(data = filtered, sr ~ .)
    }
    list(table = filtered, model = model)
  })
  
  output$summary <- renderPrint({
    summary(rv()$model)
  })
  
  output$standardplot <- renderPlot({
    par(mfrow=c(2,2))
    plot(rv()$model)
  })
  
  output$leverage <- renderPlot({
    leverages <- influence(rv()$model)$hat
    rowname <- rownames(rv()$table)
    plot(leverages, main = 'Leverage')
    abline(h = mean(leverages))
    leverages.sorted <- sort(leverages, decreasing=T, index.return=T)
    for(i in leverages.sorted$ix[1:2]) {
      text(i, leverages[i]-0.02, rowname[i])
    }
  })
  
  output$cooks <- renderPlot({
    cooks <- cooks.distance(rv()$model)
    rowname <- rownames(rv()$table)
    plot(cooks, main = 'Cooks Distance')
    abline(h = mean(cooks))
    cooks.sorted <- sort(cooks, decreasing=T, index.return=T)
    for(i in cooks.sorted$ix[1:2]) {
      text(i, cooks[i]-0.02, rowname[i])
    }
  })
  
  output$partialres <- renderPlot({
    par(mfrow = c(2,2))
    crPlot(rv()$model, variable = "pop15")
    crPlot(rv()$model, variable = "pop75")
    crPlot(rv()$model, variable = "dpi")
    crPlot(rv()$model, variable = "ddpi")
  })
  
  #Feature selection only applicable when there is more than one feature.
  b <- regsubsets(sr ~ ., data = savings, nvmax = 4, nbest = 1, method = "exhaustive")
  rs <- summary(b)
  best_bic_index <- which.min(rs$bic)
  selected <- rs$which[best_bic_index,]
  
  output$features <- renderPrint({
    selected
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(rv()$table)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)