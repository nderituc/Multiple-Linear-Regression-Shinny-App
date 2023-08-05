library(shiny)
library(ggplot2)
library(dplyr)
library(parsnip)
library(skimr)
library(GGally) # Load GGally for ggpairs
library(broom)  # Load broom for tidying model output

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
      tableOutput("regression_metrics_table"),
      tableOutput("regression_coefs_table"),
      plotOutput("ggpairs_plot"),
      verbatimTextOutput("data_summary"),
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
    
    # Perform multiple linear regression using parsnip package
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    # Generate the plot using ggplot2
    ggplot(data, aes_string(x = paste(predictor_vars, collapse = "+"), y = response_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Multiple Linear Regression",
           x = paste("Predictor Variable(s):", paste(predictor_vars, collapse = ", ")),
           y = paste("Response Variable:", response_var))
  })
  
  # Create the dataset summary
  output$data_summary <- renderPrint({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    
    cat("Dataset Summary:\n")
    cat("Number of Rows:", nrow(data), "\n")
    cat("Number of Columns:", ncol(data), "\n")
    cat("Column Names:", paste(names(data), collapse = ", "), "\n")
  })
  
  # Create the ggpairs plot
  output$ggpairs_plot <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- c(input$predictor_vars, response_var) # Include the response variable
    
    if (is.null(data) || length(predictor_vars) == 0)
      return(NULL)
    
    # Filter data to include only selected variables
    data_filtered <- data %>%
      select(all_of(predictor_vars))
    
    # Create the ggpairs plot using GGally
    ggpairs(data_filtered)
  })
  
  # Create the model summary table for model metrics
  output$regression_metrics_table <- renderTable({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    # Prepare the formula for multiple linear regression
    formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = "+")))
    
    # Perform multiple linear regression using parsnip package
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    # Get the fitted model using extract_fit_engine() from parsnip
    fitted_model <- extract_fit_engine(mlr_mod)
    
    # Get the model summary using broom::glance
    model_glance <- broom::glance(fitted_model)
    
    # Create a data frame for the model metrics
    model_metrics <- data.frame(
      Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC", "F-statistic"),
      Value = c(
        model_glance$r.squared, 
        model_glance$adj.r.squared, 
        AIC(fitted_model),
        BIC(fitted_model),
        summary(fitted_model)$fstatistic[1]
      )
    )
    
    model_metrics
  })
  
  # Create the model summary table for model coefficients
  output$regression_coefs_table <- renderTable({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    # Prepare the formula for multiple linear regression
    formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = "+")))
    
    # Perform multiple linear regression using parsnip package
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    # Get the fitted model using extract_fit_engine() from parsnip
    fitted_model <- extract_fit_engine(mlr_mod)
    
    # Get the model coefficients using broom::tidy
    model_coefs <- broom::tidy(fitted_model)
    
    model_coefs
  })
}

shinyApp(ui = ui, server = server)











