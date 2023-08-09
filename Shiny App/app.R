library(shiny)
library(ggplot2)
library(dplyr)
library(parsnip)
library(broom)
library(GGally)
library(plotly)

ui <- fluidPage(
  titlePanel("Multiple Linear Regression"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload your CSV file:"),
      selectInput("response_var", "Select Response Variable:", choices = ""),
      uiOutput("predictor_vars_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Regression Plot",
                 h3("Multiple Linear Regression Plot"),
                 plotOutput("regression_plot")
        ),
        tabPanel("Dataset Summary",
                 h3("Dataset Summary"),
                 verbatimTextOutput("data_summary")
        ),
        tabPanel("Regression Model Metrics",
                 h3("Regression Model Metrics"),
                 tableOutput("regression_metrics_table")
        ),
        tabPanel("Regression Model Coefficients",
                 h3("Regression Model Coefficients"),
                 tableOutput("regression_coefs_table")
        ),
        tabPanel("Scatterplot Matrix",
                 h3("Scatterplot Matrix"),
                 plotOutput("scatterplot_matrix")
        ),
        tabPanel("Residuals vs. Fitted",
                 h3("Residuals vs. Fitted"),
                 plotOutput("residuals_vs_fitted")
        ),
        tabPanel("Normal Q-Q Plot",
                 h3("Normal Q-Q Plot"),
                 plotOutput("normal_qq_plot")
        ),
        tabPanel("Scale-Location Plot",
                 h3("Scale-Location Plot"),
                 plotOutput("scale_location_plot")
        ),
        tabPanel("Residuals vs. Leverage",
                 h3("Residuals vs. Leverage"),
                 plotlyOutput("residuals_vs_leverage")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_reactive <- reactive({
    infile <- input$data_file
    if (is.null(infile))
      return(NULL)
    data <- read.csv(infile$datapath)
    
    # Check if the data contains numeric columns only
    if (!all(sapply(data, is.numeric))) {
      showNotification("Error: The data must contain numeric columns only.", type = "error")
      return(NULL)
    }
    
    data
  })
  
  observeEvent(data_reactive(), {
    data <- data_reactive()
    if (!is.null(data)) {
      updateSelectInput(session, "response_var", choices = names(data), selected = names(data)[1])
    } else {
      updateSelectInput(session, "response_var", choices = "", selected = "")
    }
  })
  
  output$predictor_vars_ui <- renderUI({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    selectInput("predictor_vars", "Select Predictor Variable(s):", choices = names(data), multiple = TRUE)
  })
  
  output$regression_plot <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    ggplot(data, aes_string(x = paste(predictor_vars, collapse = "+"), y = response_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Multiple Linear Regression",
           x = paste("Predictor Variable(s):", paste(predictor_vars, collapse = ", ")),
           y = paste("Response Variable:", response_var))
  })
  
  output$data_summary <- renderPrint({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    
    cat("Dataset Summary:\n")
    cat("Number of Rows:", nrow(data), "\n")
    cat("Number of Columns:", ncol(data), "\n")
    cat("Column Names:", paste(names(data), collapse = ", "), "\n")
  })
  
  output$regression_metrics_table <- renderTable({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    fitted_model <- extract_fit_engine(mlr_mod)
    
    model_glance <- broom::glance(fitted_model)
    
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
  
  output$regression_coefs_table <- renderTable({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    fitted_model <- extract_fit_engine(mlr_mod)
    
    model_coefs <- broom::tidy(fitted_model)
    
    model_coefs
  })
  
  output$scatterplot_matrix <- renderPlot({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    
    ggpairs(data, title = "Scatterplot Matrix")
  })
  output$residuals_vs_fitted <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    # Remove rows with missing values
    data <- na.omit(data)
    
    # Get residuals
    residuals <- residuals(mlr_mod)
    
    # Calculate the variance of residuals at different levels of predictor variables
    variance_data <- data %>%
      group_by_at(vars(predictor_vars)) %>%
      summarise(Variance_Residuals = var(residuals))
    # Create a plot of variance of residuals against predictor variables
    p <- ggplot(variance_data, aes(x = !!sym(predictor_vars), y = Variance_Residuals)) +
      geom_point() +
      geom_line() +
      labs(title = "Variance of Residuals vs. Predictor Variable(s)",
           x = paste("Predictor Variable(s):", paste(predictor_vars, collapse = ",")),
           y = "Variance of Residuals")
    
    # Convert ggplot to plotly
    ggplotly(p)
    
  })
  
  output$normal_qq_plot <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    fitted_model <- extract_fit_engine(mlr_mod)
    
    # Extract residuals
    residuals <- residuals(fitted_model)
    
    # Create normal Q-Q plot
    ggplot(data.frame(Standardized_Residuals = scale(residuals)), aes(sample = Standardized_Residuals)) +
      geom_qq() +
      geom_qq_line() +
      labs(title = "Normal Q-Q Plot",
           x = "Theoretical Quantiles",
           y = "Standardized Residuals")
  })
  
  output$scale_location_plot <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    fitted_model <- extract_fit_engine(mlr_mod)
    
    # Extract standardized residuals
    standardized_residuals <- rstandard(fitted_model)
    
    ggplot(data.frame(Fitted_Values = fitted(fitted_model), Standardized_Residuals = standardized_residuals), aes(x = Fitted_Values, y = sqrt(abs(Standardized_Residuals)))) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Scale-Location Plot",
           x = "Fitted Values",
           y = "√|Standardized Residuals|")
  })
  
  output$residuals_vs_leverage <- renderPlot({
    data <- data_reactive()
    response_var <- input$response_var
    predictor_vars <- input$predictor_vars
    
    if (is.null(data) || is.null(response_var) || length(predictor_vars) == 0)
      return(NULL)
    
    formula <- reformulate(predictor_vars, response_var)
    
    lm_spec <- linear_reg() %>% 
      set_mode("regression") %>%
      set_engine("lm")
    
    mlr_mod <- lm_spec %>% 
      fit(formula, data = data)
    
    # Calculate residuals and leverage values
    fitted_model <- extract_fit_engine(mlr_mod)
    residuals <- residuals(fitted_model)
    leverage_values <- hatvalues(fitted_model)
    
    # Calculate standardized residuals
    standardized_residuals <- residuals / sqrt(1 - leverage_values)
    
    ggplot(data.frame(Leverage = leverage_values, Standardized_Residuals = standardized_residuals), aes(x = Leverage, y = Standardized_Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(xintercept = 2*mean(leverage_values), linetype = "dotted", color = "blue") +
      geom_vline(xintercept = 3*mean(leverage_values), linetype = "dotted", color = "blue") +
      labs(title = "Residuals vs. Leverage",
           x = "Leverage",
           y = "Standardized Residuals")
  
   
  })
}

shinyApp(ui = ui, server = server)










