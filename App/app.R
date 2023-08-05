library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Multiple Linear Regression App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload your CSV file:"),
      selectInput("response_var", "Select Response Variable:", ""),
      uiOutput("predictor_vars_ui")
    ),
    mainPanel(
      plotOutput("regression_plot"),
      verbatimTextOutput("regression_summary")
    )
  )
)

server <- function(input, output, session) {
  # Read the uploaded CSV file and store it in a reactive environment
  data_reactive <- reactive({
    infile <- input$data_file
    if (is.null(infile))
      return(NULL)
    read.csv(infile$datapath)
  })
  
  # Update the response variable choices based on the uploaded data
  observe({
    data <- data_reactive()
    updateSelectInput(session, "response_var", choices = names(data), selected = NULL)
  })
  
  # Create dynamic UI for predictor variable selection
  output$predictor_vars_ui <- renderUI({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    selectInput("predictor_vars", "Select Predictor Variable(s):", choices = names(data), multiple = TRUE)
  })
  
  # Create a reactive environment for linear regression and visualization
  output$regression_plot <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    # Prepare the formula for multiple linear regression
    formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = "+")))
    
    # Perform multiple linear regression using lm() function
    model <- lm(formula, data = data)
    
    # Generate the plot using ggplot2
    ggplot(data, aes_string(x = paste(predictor_vars, collapse = "+"), y = response_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Multiple Linear Regression",
           x = paste("Predictor Variable(s):", paste(predictor_vars, collapse = ", ")),
           y = paste("Response Variable:", response_var))
  })
  
  # Show the summary of the linear regression model
  output$regression_summary <- renderPrint({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    # Prepare the formula for multiple linear regression
    formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = "+")))
    
    # Perform multiple linear regression using lm() function
    model <- lm(formula, data = data)
    
    # Show the summary of the linear regression model
    summary(model)
  })
}

shinyApp(ui = ui, server = server)
