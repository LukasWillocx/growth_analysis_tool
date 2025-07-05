# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(rhandsontable)
library(shinycssloaders)

# Source functions
source("functions.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Growth Data Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("table")),
      menuItem("Growth Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Model Diagnostics", tabName = "diagnostics", icon = icon("stethoscope"))
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      # Data Input Tab
      tabItem(tabName = "input",
              fluidRow(
                box(
                  title = "Data Configuration", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(3,
                           numericInput("n_timepoints", "Number of Time Points:", 
                                        value = 10, min = 3, max = 500, step = 1)
                    ),
                    column(3,
                           numericInput("n_replicates", "Number of Replicates:", 
                                        value = 3, min = 1, max = 20, step = 1)
                    ),
                    column(3,
                           selectInput("data_structure", "Data Structure:",
                                       choices = list(
                                         "Wide Format (Time × Replicates)" = "wide",
                                         "Long Format (Time, Replicate, Value)" = "long"
                                       ),
                                       selected = "wide")
                    ),
                    column(3,
                           br(),
                           actionButton("create_table", "Create Data Table", 
                                        class = "btn-primary")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Entry", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  conditionalPanel(
                    condition = "input.create_table > 0",
                    
                    fluidRow(
                      column(6,
                             h4("Instructions:"),
                             tags$ul(
                               tags$li("Click cells to edit - use both . and , for decimals"),
                               tags$li("Press Enter or Tab to confirm cell changes"),
                               tags$li("Right-click for context menu options"),
                               tags$li("Empty cells will be treated as missing values")
                             )
                      ),
                      column(6,
                             div(style = "text-align: right;",
                                 actionButton("add_row", "Add Row", class = "btn-info btn-sm", icon = icon("plus")),
                                 actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", icon = icon("minus")),
                                 actionButton("clear_data", "Clear All", class = "btn-danger btn-sm", icon = icon("trash"))
                             )
                      )
                    ),
                    
                    br(),
                    
                    withSpinner(rHandsontableOutput("data_input_table", height = "400px")),
                    
                    br(),
                    
                    fluidRow(
                      column(6,
                             actionButton("process_data", "Process Data", 
                                          class = "btn-success", icon = icon("check"))
                      ),
                      column(6,
                             downloadButton("download_template", "Download Template", 
                                            class = "btn-info"),
                             fileInput("upload_data", "Upload CSV", 
                                       accept = c(".csv"))
                      )
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "output.data_processed",
                      div(
                        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                        h4("Data Summary", style = "color: #28a745;"),
                        verbatimTextOutput("data_summary")
                      )
                    )
                  )
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Model Configuration", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  
                  selectInput("model_type", "Growth Model:",
                              choices = list(
                                "Logistic" = "logistic",
                                "Gompertz" = "gompertz",
                                "Exponential" = "exponential",
                                "Linear" = "linear"
                              ),
                              selected = "logistic"),
                  
                  conditionalPanel(
                    condition = "input.n_replicates > 1",
                    selectInput("replicate_handling", "Handle Replicates:",
                                choices = list(
                                  "Fit to Mean" = "mean",
                                  "Fit Individual Curves" = "individual",
                                  "Show All Points" = "all"
                                ),
                                selected = "mean")
                  ),
                  
                  checkboxInput("show_confidence", "Show Confidence Intervals", value = TRUE),
                  
                  actionButton("fit_model", "Fit Model", class = "btn-primary", icon = icon("play")),
                  
                  br(), br(),
                  
                  conditionalPanel(
                    condition = "output.model_fitted",
                    div(
                      style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px;",
                      h4("Model Parameters:", style = "color: #155724;"),
                      verbatimTextOutput("model_params")
                    )
                  )
                ),
                
                box(
                  title = "Growth Curve", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 8,
                  withSpinner(plotlyOutput("growth_plot", height = "500px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Growth Characteristics", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::dataTableOutput("growth_characteristics"))
                ),
                
                box(
                  title = "Model Comparison", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::dataTableOutput("model_comparison"))
                )
              )
      ),
      
      # Diagnostics Tab
      tabItem(tabName = "diagnostics",
              fluidRow(
                box(
                  title = "Goodness of Fit", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::dataTableOutput("goodness_of_fit"))
                ),
                
                box(
                  title = "Parameter Confidence Intervals", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::dataTableOutput("confidence_intervals"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Residual Analysis", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotlyOutput("residual_plot", height = "400px"))
                ),
                
                box(
                  title = "Q-Q Plot", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotlyOutput("qq_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Model Predictions vs Observed", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(plotlyOutput("pred_vs_obs_plot", height = "400px"))
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    models = NULL,
    fitted = FALSE
  )
  
  # Create initial data table
  observeEvent(input$create_table, {
    if(input$data_structure == "wide") {
      # Wide format: Time column + Replicate columns
      col_names <- c("Time", paste0("Rep_", 1:input$n_replicates))
      df <- data.frame(matrix(NA, nrow = input$n_timepoints, ncol = length(col_names)))
      colnames(df) <- col_names
      # Create a proper time sequence with empty cells for replicates
      df$Time <- seq(0, input$n_timepoints - 1, by = 1)
      # Set replicate columns to proper NA
      for(i in 2:ncol(df)) {
        df[, i] <- as.numeric(NA)
      }
    } else {
      # Long format: Time, Replicate, Value
      df <- data.frame(
        Time = rep(seq(0, input$n_timepoints - 1, by = 1), each = input$n_replicates),
        Replicate = rep(1:input$n_replicates, times = input$n_timepoints),
        Value = as.numeric(NA)
      )
    }
    
    values$raw_data <- df
    values$processed_data <- NULL
    values$fitted <- FALSE
  })
  
  # Render handsontable with improved settings
  output$data_input_table <- renderRHandsontable({
    req(values$raw_data)
    
    ht <- rhandsontable(values$raw_data, 
                        width = "100%", 
                        height = 400,
                        stretchH = "all",
                        contextMenu = TRUE,
                        manualColumnResize = TRUE,
                        manualRowResize = TRUE,
                        preventOverflow = 'horizontal') %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    
    # Configure column types more explicitly
    if(input$data_structure == "wide") {
      # First column is Time - make it numeric
      ht <- ht %>% hot_col(1, type = "numeric", format = "0.000", allowInvalid = FALSE)
      # Remaining columns are replicates - make them numeric with proper decimal handling
      for(i in 2:ncol(values$raw_data)) {
        ht <- ht %>% hot_col(i, type = "numeric", format = "0.000", allowInvalid = FALSE)
      }
    } else {
      # Long format: Time and Value are numeric, Replicate is integer
      ht <- ht %>% 
        hot_col(1, type = "numeric", format = "0.000", allowInvalid = FALSE) %>%  # Time
        hot_col(2, type = "numeric", format = "0", allowInvalid = FALSE) %>%      # Replicate
        hot_col(3, type = "numeric", format = "0.000", allowInvalid = FALSE)     # Value
    }
    
    ht
  })
  
  # Update raw data when table is edited
  observeEvent(input$data_input_table, {
    if (!is.null(input$data_input_table)) {
      df <- hot_to_r(input$data_input_table)
      
      # Ensure proper numeric conversion
      if(input$data_structure == "wide") {
        # Convert all columns to numeric
        for(i in 1:ncol(df)) {
          df[, i] <- as.numeric(df[, i])
        }
      } else {
        # Convert specific columns
        df$Time <- as.numeric(df$Time)
        df$Replicate <- as.numeric(df$Replicate)
        df$Value <- as.numeric(df$Value)
      }
      
      values$raw_data <- df
      values$processed_data <- NULL
      values$fitted <- FALSE
    }
  })
  
  # Add row functionality
  observeEvent(input$add_row, {
    req(values$raw_data)
    
    if(input$data_structure == "wide") {
      new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(values$raw_data)))
      colnames(new_row) <- colnames(values$raw_data)
      # Set time to next sequential value
      new_row$Time <- max(values$raw_data$Time, na.rm = TRUE) + 1
      # Set replicate columns to proper NA
      for(i in 2:ncol(new_row)) {
        new_row[, i] <- as.numeric(NA)
      }
    } else {
      new_time <- max(values$raw_data$Time, na.rm = TRUE) + 1
      new_row <- data.frame(
        Time = rep(new_time, input$n_replicates),
        Replicate = 1:input$n_replicates,
        Value = as.numeric(NA)
      )
    }
    
    values$raw_data <- rbind(values$raw_data, new_row)
  })
  
  # Remove row functionality
  observeEvent(input$remove_row, {
    req(values$raw_data)
    
    if(input$data_structure == "wide") {
      if(nrow(values$raw_data) > 3) {
        values$raw_data <- values$raw_data[-nrow(values$raw_data), ]
      } else {
        showNotification("Cannot remove row: minimum 3 time points required", type = "warning")
      }
    } else {
      if(length(unique(values$raw_data$Time)) > 3) {
        max_time <- max(values$raw_data$Time, na.rm = TRUE)
        values$raw_data <- values$raw_data[values$raw_data$Time != max_time, ]
      } else {
        showNotification("Cannot remove row: minimum 3 time points required", type = "warning")
      }
    }
  })
  
  # Clear data functionality
  observeEvent(input$clear_data, {
    req(values$raw_data)
    
    if(input$data_structure == "wide") {
      # Keep time column, clear others
      for(i in 2:ncol(values$raw_data)) {
        values$raw_data[, i] <- as.numeric(NA)
      }
    } else {
      values$raw_data$Value <- as.numeric(NA)
    }
  })
  
  # Process data
  observeEvent(input$process_data, {
    req(values$raw_data)
    
    tryCatch({
      processed <- process_growth_data(values$raw_data, input$data_structure)
      values$processed_data <- processed
      values$fitted <- FALSE
      
      showNotification("Data processed successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })
  
  # Data processed flag
  output$data_processed <- reactive({
    !is.null(values$processed_data)
  })
  outputOptions(output, "data_processed", suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderText({
    req(values$processed_data)
    
    data <- values$processed_data
    n_points <- length(unique(data$Time))
    n_reps <- length(unique(data$Replicate))
    time_range <- range(data$Time, na.rm = TRUE)
    value_range <- range(data$Value, na.rm = TRUE)
    missing_count <- sum(is.na(data$Value))
    
    paste(
      paste("Time points:", n_points),
      paste("Replicates:", n_reps),
      paste("Time range:", round(time_range[1], 2), "to", round(time_range[2], 2)),
      paste("Value range:", round(value_range[1], 3), "to", round(value_range[2], 3)),
      paste("Missing values:", missing_count),
      paste("Total observations:", nrow(data)),
      sep = "\n"
    )
  })
  
  # Fit models
  observeEvent(input$fit_model, {
    req(values$processed_data)
    
    withProgress(message = 'Fitting models...', value = 0, {
      
      tryCatch({
        incProgress(0.3, detail = "Fitting growth models")
        
        models <- fit_all_models(values$processed_data, input$replicate_handling)
        values$models <- models
        values$fitted <- TRUE
        
        incProgress(0.7, detail = "Calculating diagnostics")
        
        showNotification("Models fitted successfully!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error fitting models:", e$message), type = "error")
        values$fitted <- FALSE
      })
    })
  })
  
  # Model fitted flag
  output$model_fitted <- reactive({
    values$fitted
  })
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)
  
  # Model parameters
  output$model_params <- renderText({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      params <- coef(model)
      
      param_text <- paste(names(params), "=", round(params, 4), collapse = "\n")
      
      paste("Model:", stringr::str_to_title(input$model_type),
            "Parameters:",
            param_text,
            sep = "\n")
    } else {
      "Model not available"
    }
  })
  
  # Growth plot
  output$growth_plot <- renderPlotly({
    req(values$processed_data)
    
    p <- plot_growth_data(values$processed_data, 
                          values$models, 
                          input$model_type,
                          input$replicate_handling,
                          input$show_confidence)
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>% 
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Growth characteristics
  output$growth_characteristics <- DT::renderDataTable({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      characteristics <- calculate_growth_characteristics(model, values$processed_data, input$model_type)
      
      DT::datatable(characteristics, 
                    options = list(pageLength = 10, dom = 't'),
                    class = 'cell-border stripe') %>%
        DT::formatRound(columns = 2, digits = 4)
    }
  })
  
  # Model comparison
  output$model_comparison <- DT::renderDataTable({
    req(values$models)
    
    comparison <- compare_models(values$models, values$processed_data)
    
    DT::datatable(comparison, 
                  options = list(pageLength = 10, dom = 't'),
                  class = 'cell-border stripe') %>%
      DT::formatRound(columns = 2:5, digits = 4)
  })
  
  # Goodness of fit
  output$goodness_of_fit <- DT::renderDataTable({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      gof <- calculate_goodness_of_fit(model, values$processed_data)
      
      gof_df <- data.frame(
        Metric = names(gof),
        Value = as.numeric(gof)
      )
      
      DT::datatable(gof_df, 
                    options = list(pageLength = 10, dom = 't'),
                    class = 'cell-border stripe') %>%
        DT::formatRound(columns = 2, digits = 4)
    }
  })
  
  # Confidence intervals
  output$confidence_intervals <- DT::renderDataTable({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      ci <- calculate_confidence_intervals(model)
      
      DT::datatable(ci, 
                    options = list(pageLength = 10, dom = 't'),
                    class = 'cell-border stripe') %>%
        DT::formatRound(columns = 2:4, digits = 4)
    }
  })
  
  # Residual plot
  output$residual_plot <- renderPlotly({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      p <- plot_residuals(model, values$processed_data)
      ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE)
    }
  })
  
  # Q-Q plot
  output$qq_plot <- renderPlotly({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      p <- plot_qq(model)
      ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE)
    }
  })
  
  # Predictions vs Observed plot
  output$pred_vs_obs_plot <- renderPlotly({
    req(values$models, input$model_type)
    
    if(input$model_type %in% names(values$models)) {
      model <- values$models[[input$model_type]]
      p <- plot_predictions_vs_observed(model, values$processed_data)
      ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE)
    }
  })
  
  # Download template
  output$download_template <- downloadHandler(
    filename = function() {
      paste0("growth_data_template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(input$data_structure == "wide") {
        template <- data.frame(
          Time = c(0, 1, 2, 3, 4, 5),
          Rep_1 = c(1.0, 1.5, 2.5, 4.0, 6.0, 8.0),
          Rep_2 = c(1.1, 1.6, 2.4, 3.9, 6.1, 7.9),
          Rep_3 = c(0.9, 1.4, 2.6, 4.1, 5.9, 8.1)
        )
      } else {
        template <- data.frame(
          Time = rep(c(0, 1, 2, 3, 4, 5), each = 3),
          Replicate = rep(1:3, times = 6),
          Value = c(1.0, 1.1, 0.9, 1.5, 1.6, 1.4, 2.5, 2.4, 2.6, 
                    4.0, 3.9, 4.1, 6.0, 6.1, 5.9, 8.0, 7.9, 8.1)
        )
      }
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Upload data
  observeEvent(input$upload_data, {
    req(input$upload_data)
    
    tryCatch({
      uploaded <- read.csv(input$upload_data$datapath)
      values$raw_data <- uploaded
      values$processed_data <- NULL
      values$fitted <- FALSE
      
      showNotification("Data uploaded successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error uploading data:", e$message), type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)