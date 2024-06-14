library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", multiple = FALSE, accept = ".csv"),
      br(),
      actionButton("analyzeBtn", "Statistical Analysis Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput('tbl')),
        tabPanel("Summary Data", verbatimTextOutput("summaryText")),
        tabPanel("Box Plot", fluidPage(
          sidebarLayout(
            sidebarPanel(
              h5("Select variables for the boxplot:"),
              uiOutput("var_box_ui")
            ),
            mainPanel(
              plotOutput("boxPlot")
            )
          )
        )),
        tabPanel("Bar Plot", fluidPage(
          sidebarLayout(
            sidebarPanel(
              h5("Select variables for the barplot:"),
              uiOutput("var_bar_ui")
            ),
            mainPanel(
              plotOutput("barPlot")
            )
          )
        )),
        tabPanel("Scatter Plot", fluidPage(
          sidebarLayout(
            sidebarPanel(
              h5("Select variables for the scatter plot:"),
              uiOutput("var_x_ui"),
              uiOutput("var_y_ui")
            ),
            mainPanel(
              plotOutput("scatterPlot"),
              textOutput("correlationText")
            )
          )
        )),
        tabPanel("Data and ANOVA Test Results", fluidPage(
          titlePanel("Analysis of Variance"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("vardipen_ui"),
              uiOutput("varindepen_ui")
            ),
            mainPanel(
              titlePanel("Data"),
              DT::dataTableOutput('tbl1'),
              titlePanel("Result"),
              verbatimTextOutput("anovaText")
            )
          )
        ))
      )
    )
  )
)

server <- function(input, output, session) {
  # Load and process data from the uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Render the data table
  output$tbl <- DT::renderDataTable({
    data()
  })
  
  # Summary data
  output$summaryText <- renderPrint({
    summary(data())
  })
  
  # Dynamically populate variable selection inputs
  observe({
    df <- data()
    updateSelectInput(session, "var_box", choices = names(df))
    updateSelectInput(session, "var_bar", choices = names(df))
    updateSelectInput(session, "vardipen", choices = names(df))
    updateSelectInput(session, "varindepen", choices = names(df))
    updateSelectInput(session, "var_x", choices = names(df))
    updateSelectInput(session, "var_y", choices = names(df))
  })
  
  # Box plot variable UI
  output$var_box_ui <- renderUI({
    selectInput("var_box", "Variable:", choices = names(data()))
  })
  
  # Bar plot variable UI
  output$var_bar_ui <- renderUI({
    selectInput("var_bar", "Variable:", choices = names(data()))
  })
  
  # Scatter plot X variable UI
  output$var_x_ui <- renderUI({
    selectInput("var_x", "X Variable:", choices = names(data()))
  })
  
  # Scatter plot Y variable UI
  output$var_y_ui <- renderUI({
    selectInput("var_y", "Y Variable:", choices = names(data()))
  })
  
  # ANOVA test dependent variable UI
  output$vardipen_ui <- renderUI({
    selectInput("vardipen", "Dependent Variable:", choices = names(data()))
  })
  
  # ANOVA test independent variable UI
  output$varindepen_ui <- renderUI({
    selectInput("varindepen", "Independent Variable:", choices = names(data()))
  })
  
  # Box plot
  output$boxPlot <- renderPlot({
    req(input$var_box)
    ggplot(data(), aes_string(x = input$var_box)) +
      geom_boxplot() +
      labs(title = paste("Box Plot of", input$var_box))
  })
  
  # Bar plot
  output$barPlot <- renderPlot({
    req(input$var_bar)
    ggplot(data(), aes_string(x = input$var_bar)) +
      geom_bar() +
      labs(title = paste("Bar Plot of", input$var_bar))
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlot({
    req(input$var_x, input$var_y)
    if (!is.numeric(data()[[input$var_x]]) || !is.numeric(data()[[input$var_y]])) {
      return(NULL)  # If either variable is not numeric, return NULL plot
    }
    if (any(is.na(data()[[input$var_x]])) || any(is.na(data()[[input$var_y]]))) {
      return(NULL)  # If either variable contains NA, return NULL plot
    }
    ggplot(data(), aes_string(x = input$var_x, y = input$var_y)) +
      geom_point() +
      labs(title = paste("Scatter Plot of", input$var_x, "vs", input$var_y))
  })
  
  # Pearson correlation
  output$correlationText <- renderText({
    req(input$var_x, input$var_y)
    if (!is.numeric(data()[[input$var_x]]) || !is.numeric(data()[[input$var_y]])) {
      return("Variables are not numeric")
    }
    if (any(is.na(data()[[input$var_x]])) || any(is.na(data()[[input$var_y]]))) {
      return("Variables contain missing values")
    }
    cor_val <- cor(data()[[input$var_x]], data()[[input$var_y]], use = "complete.obs")
    paste("Pearson Correlation:", round(cor_val, 3))
  })
  
  # ANOVA test
  output$tbl1 <- DT::renderDataTable({
    data()
  })
  
  output$anovaText <- renderPrint({
    anova_result <- aov(as.formula(paste(input$vardipen, "~", input$varindepen)), data = data())
    summary(anova_result)
  })
}

shinyApp(ui, server)
