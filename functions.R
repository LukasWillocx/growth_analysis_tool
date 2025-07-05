# Enhanced Growth Analysis Functions
# Required libraries
library(ggplot2)
library(minpack.lm)
library(dplyr)
library(stringr)
library(broom)

# Process growth data from wide or long format
process_growth_data <- function(data, data_structure) {
  
  if(data_structure == "wide") {
    # Convert wide to long format
    time_col <- data[, 1, drop = FALSE]
    value_cols <- data[, -1, drop = FALSE]
    
    # Remove rows where Time is NA
    valid_rows <- !is.na(time_col[, 1])
    time_col <- time_col[valid_rows, , drop = FALSE]
    value_cols <- value_cols[valid_rows, , drop = FALSE]
    
    # Create long format
    long_data <- data.frame(
      Time = rep(time_col[, 1], ncol(value_cols)),
      Replicate = rep(1:ncol(value_cols), each = nrow(time_col)),
      Value = as.vector(as.matrix(value_cols))
    )
    
    # Add replicate names
    rep_names <- colnames(value_cols)
    long_data$Replicate <- factor(long_data$Replicate, 
                                  levels = 1:length(rep_names),
                                  labels = rep_names)
    
  } else {
    # Data is already in long format
    long_data <- data
    long_data$Replicate <- factor(long_data$Replicate)
  }
  
  # Remove rows with missing Time or Value
  long_data <- long_data[!is.na(long_data$Time) & !is.na(long_data$Value), ]
  
  # Validate data
  if(nrow(long_data) < 3) {
    stop("Need at least 3 complete data points")
  }
  
  if(length(unique(long_data$Time)) < 3) {
    stop("Need at least 3 different time points")
  }
  
  # Sort by Time and Replicate
  long_data <- long_data[order(long_data$Time, long_data$Replicate), ]
  
  return(long_data)
}

# Fit all growth models
fit_all_models <- function(data, replicate_handling = "mean") {
  
  # Prepare data based on replicate handling
  if(replicate_handling == "mean") {
    # Use mean of replicates
    model_data <- data %>%
      group_by(Time) %>%
      summarise(Growth_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  } else {
    # Use all individual points (ignoring replicates for now)
    model_data <- data.frame(
      Time = data$Time,
      Growth_Value = data$Value
    )
  }
  
  # Define model types to fit
  model_types <- c("linear", "exponential", "logistic", "gompertz")
  models <- list()
  
  for(model_type in model_types) {
    tryCatch({
      model <- fit_growth_model(model_data, model_type)
      models[[model_type]] <- model
    }, error = function(e) {
      warning(paste("Failed to fit", model_type, "model:", e$message))
    })
  }
  
  if(length(models) == 0) {
    stop("No models could be fitted to the data")
  }
  
  return(models)
}

# Enhanced fit growth models
fit_growth_model <- function(data, model_type) {
  
  time <- data$Time
  growth <- data$Growth_Value
  
  # Remove any remaining NA values
  valid_idx <- !is.na(time) & !is.na(growth)
  time <- time[valid_idx]
  growth <- growth[valid_idx]
  
  if(length(time) < 3) {
    stop("Insufficient data points for model fitting")
  }
  
  # Improved starting parameter estimation
  K_start <- max(growth) * 1.1
  r_start <- 0.1
  t0_start <- time[which.max(diff(growth))]  # Inflection point estimate
  
  model <- switch(model_type,
                  "linear" = {
                    # Linear: Y = a + b * t
                    lm(growth ~ time)
                  },
                  
                  "exponential" = {
                    # Exponential: Y = a * exp(r * t)
                    if(any(growth <= 0)) {
                      stop("Exponential models require positive growth values")
                    }
                    
                    # Better starting values for exponential
                    a_start <- min(growth[growth > 0])
                    r_start <- log(max(growth) / a_start) / max(time)
                    
                    nlsLM(growth ~ a * exp(r * time),
                          start = list(a = a_start, r = r_start),
                          control = nls.lm.control(maxiter = 1000),
                          lower = c(a = 0.001, r = -Inf),
                          upper = c(a = Inf, r = Inf))
                  },
                  
                  "logistic" = {
                    # Logistic: Y = K / (1 + exp(-r * (t - t0)))
                    # Better starting values
                    K_start <- max(growth) * 1.05
                    r_start <- 4 * max(diff(growth)) / K_start
                    
                    nlsLM(growth ~ K / (1 + exp(-r * (time - t0))),
                          start = list(K = K_start, r = r_start, t0 = t0_start),
                          control = nls.lm.control(maxiter = 1000),
                          lower = c(K = max(growth), r = 0.001, t0 = -Inf),
                          upper = c(K = Inf, r = Inf, t0 = Inf))
                  },
                  
                  "gompertz" = {
                    # Gompertz: Y = K * exp(-exp(-r * (t - t0)))
                    K_start <- max(growth) * 1.05
                    r_start <- 1 / (max(time) - min(time))
                    
                    nlsLM(growth ~ K * exp(-exp(-r * (time - t0))),
                          start = list(K = K_start, r = r_start, t0 = t0_start),
                          control = nls.lm.control(maxiter = 1000),
                          lower = c(K = max(growth), r = 0.001, t0 = -Inf),
                          upper = c(K = Inf, r = Inf, t0 = Inf))
                  }
  )
  
  return(model)
}

# Enhanced plot function
plot_growth_data <- function(data, models = NULL, selected_model = "logistic", 
                             replicate_handling = "mean", show_confidence = TRUE) {
  
  # Create base plot
  if(replicate_handling == "mean") {
    plot_data <- data %>%
      group_by(Time) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SE = sd(Value, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      )
    
    p <- ggplot(plot_data, aes(x = Time, y = Mean)) +
      geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                    width = 0.2, alpha = 0.7, color = "#2E86AB")
  } else if(replicate_handling == "individual") {
    # Show individual replicate curves
    p <- ggplot(data, aes(x = Time, y = Value, color = Replicate)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_line(alpha = 0.5)
  } else {
    # Show all points
    p <- ggplot(data, aes(x = Time, y = Value)) +
      geom_point(size = 2, alpha = 0.6, color = "#2E86AB")
  }
  
  # Add model curve if models are provided
  if(!is.null(models) && selected_model %in% names(models)) {
    model <- models[[selected_model]]
    
    # Create prediction data
    time_range <- range(data$Time, na.rm = TRUE)
    pred_time <- seq(time_range[1], time_range[2], length.out = 100)
    
    # Generate predictions based on model type
    if(inherits(model, "lm")) {
      # Linear model
      pred_data <- data.frame(time = pred_time)
      predictions <- predict(model, newdata = pred_data, se.fit = show_confidence)
      
      if(show_confidence) {
        pred_df <- data.frame(
          Time = pred_time,
          Predicted = predictions$fit,
          Lower = predictions$fit - 1.96 * predictions$se.fit,
          Upper = predictions$fit + 1.96 * predictions$se.fit
        )
        
        p <- p + 
          geom_ribbon(data = pred_df, aes(x = Time, ymin = Lower, ymax = Upper),
                      alpha = 0.3, fill = "#FF6B6B", inherit.aes = FALSE) +
          geom_line(data = pred_df, aes(x = Time, y = Predicted),
                    color = "#FF6B6B", size = 1, inherit.aes = FALSE)
      } else {
        pred_df <- data.frame(
          Time = pred_time,
          Predicted = predictions$fit
        )
        p <- p + geom_line(data = pred_df, aes(x = Time, y = Predicted),
                           color = "#FF6B6B", size = 1, inherit.aes = FALSE)
      }
    } else {
      # Nonlinear model
      pred_data <- data.frame(time = pred_time)
      predictions <- predict(model, newdata = pred_data)
      
      pred_df <- data.frame(
        Time = pred_time,
        Predicted = predictions
      )
      
      p <- p + geom_line(data = pred_df, aes(x = Time, y = Predicted),
                         color = "#FF6B6B", size = 1, inherit.aes = FALSE)
    }
  }
  
  # Customize plot
  p <- p +
    labs(
      title = paste("Growth Curve Analysis"),
      subtitle = if(!is.null(models) && selected_model %in% names(models)) {
        paste("Model:", stringr::str_to_title(selected_model))
      } else {
        "Raw Data"
      },
      x = "Time",
      y = "Growth Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80")
    )
  
  return(p)
}

# Calculate growth characteristics
calculate_growth_characteristics <- function(model, data, model_type) {
  
  characteristics <- data.frame(
    Characteristic = character(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  if(model_type == "linear") {
    coefs <- coef(model)
    characteristics <- rbind(characteristics, data.frame(
      Characteristic = c("Intercept", "Slope", "Growth Rate"),
      Value = c(coefs[1], coefs[2], coefs[2])
    ))
  } else if(model_type == "exponential") {
    coefs <- coef(model)
    characteristics <- rbind(characteristics, data.frame(
      Characteristic = c("Initial Value (a)", "Growth Rate (r)", "Doubling Time"),
      Value = c(coefs[1], coefs[2], log(2) / coefs[2])
    ))
  } else if(model_type == "logistic") {
    coefs <- coef(model)
    characteristics <- rbind(characteristics, data.frame(
      Characteristic = c("Carrying Capacity (K)", "Growth Rate (r)", "Inflection Point (t0)", "Max Growth Rate"),
      Value = c(coefs[1], coefs[2], coefs[3], coefs[1] * coefs[2] / 4)
    ))
  } else if(model_type == "gompertz") {
    coefs <- coef(model)
    characteristics <- rbind(characteristics, data.frame(
      Characteristic = c("Asymptote (K)", "Growth Rate (r)", "Inflection Point (t0)", "Max Growth Rate"),
      Value = c(coefs[1], coefs[2], coefs[3], coefs[1] * coefs[2] / exp(1))
    ))
  }
  
  return(characteristics)
}

# Compare models
compare_models <- function(models, data) {
  
  # Prepare data for comparison
  model_data <- data %>%
    group_by(Time) %>%
    summarise(Growth_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  
  comparison <- data.frame(
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    RMSE = numeric(),
    R_squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(model_name in names(models)) {
    model <- models[[model_name]]
    
    # Calculate metrics
    if(inherits(model, "lm")) {
      aic_val <- AIC(model)
      bic_val <- BIC(model)
      rmse_val <- sqrt(mean(residuals(model)^2))
      r_squared <- summary(model)$r.squared
    } else {
      # For nls models
      pred_data <- data.frame(time = model_data$Time)
      predictions <- predict(model, newdata = pred_data)
      residuals_val <- model_data$Growth_Value - predictions
      
      n <- length(residuals_val)
      p <- length(coef(model))
      
      aic_val <- n * log(sum(residuals_val^2) / n) + 2 * p
      bic_val <- n * log(sum(residuals_val^2) / n) + p * log(n)
      rmse_val <- sqrt(mean(residuals_val^2))
      
      # R-squared for nonlinear models
      ss_res <- sum(residuals_val^2)
      ss_tot <- sum((model_data$Growth_Value - mean(model_data$Growth_Value))^2)
      r_squared <- 1 - (ss_res / ss_tot)
    }
    
    comparison <- rbind(comparison, data.frame(
      Model = stringr::str_to_title(model_name),
      AIC = aic_val,
      BIC = bic_val,
      RMSE = rmse_val,
      R_squared = r_squared
    ))
  }
  
  # Sort by AIC (lower is better)
  comparison <- comparison[order(comparison$AIC), ]
  
  return(comparison)
}

# Calculate goodness of fit
calculate_goodness_of_fit <- function(model, data) {
  
  # Prepare data
  model_data <- data %>%
    group_by(Time) %>%
    summarise(Growth_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  
  if(inherits(model, "lm")) {
    # Linear model
    predictions <- predict(model)
    residuals_val <- residuals(model)
    
    gof <- list(
      "R-squared" = summary(model)$r.squared,
      "Adjusted R-squared" = summary(model)$adj.r.squared,
      "RMSE" = sqrt(mean(residuals_val^2)),
      "AIC" = AIC(model),
      "BIC" = BIC(model),
      "F-statistic" = summary(model)$fstatistic[1]
    )
  } else {
    # Nonlinear model
    pred_data <- data.frame(time = model_data$Time)
    predictions <- predict(model, newdata = pred_data)
    residuals_val <- model_data$Growth_Value - predictions
    
    n <- length(residuals_val)
    p <- length(coef(model))
    
    ss_res <- sum(residuals_val^2)
    ss_tot <- sum((model_data$Growth_Value - mean(model_data$Growth_Value))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    gof <- list(
      "R-squared" = r_squared,
      "RMSE" = sqrt(mean(residuals_val^2)),
      "AIC" = n * log(ss_res / n) + 2 * p,
      "BIC" = n * log(ss_res / n) + p * log(n),
      "MSE" = mean(residuals_val^2),
      "Sum of Squares" = ss_res
    )
  }
  
  return(gof)
}

# Calculate confidence intervals
calculate_confidence_intervals <- function(model, level = 0.95) {
  
  tryCatch({
    ci <- confint(model, level = level)
    
    ci_df <- data.frame(
      Parameter = rownames(ci),
      Lower = ci[, 1],
      Upper = ci[, 2],
      Estimate = coef(model)[rownames(ci)]
    )
    
    return(ci_df)
  }, error = function(e) {
    # Fallback for models where confint doesn't work
    coefs <- coef(model)
    se <- tryCatch({
      sqrt(diag(vcov(model)))
    }, error = function(e) {
      rep(NA, length(coefs))
    })
    
    alpha <- 1 - level
    t_val <- qt(1 - alpha/2, df = length(fitted(model)) - length(coefs))
    
    ci_df <- data.frame(
      Parameter = names(coefs),
      Lower = coefs - t_val * se,
      Upper = coefs + t_val * se,
      Estimate = coefs
    )
    
    return(ci_df)
  })
}

# Plot residuals
plot_residuals <- function(model, data) {
  
  # Prepare data
  model_data <- data %>%
    group_by(Time) %>%
    summarise(Growth_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  
  if(inherits(model, "lm")) {
    fitted_vals <- fitted(model)
    residuals_val <- residuals(model)
  } else {
    pred_data <- data.frame(time = model_data$Time)
    fitted_vals <- predict(model, newdata = pred_data)
    residuals_val <- model_data$Growth_Value - fitted_vals
  }
  
  plot_data <- data.frame(
    Fitted = fitted_vals,
    Residuals = residuals_val
  )
  
  p <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.7, color = "#2E86AB") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = FALSE, color = "#FF6B6B") +
    labs(
      title = "Residual Plot",
      subtitle = "Residuals vs Fitted Values",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50")
    )
  
  return(p)
}

# Plot Q-Q plot
plot_qq <- function(model) {
  
  if(inherits(model, "lm")) {
    residuals_val <- residuals(model)
  } else {
    # For nls models, we need to calculate residuals manually
    # This is a simplified approach
    residuals_val <- residuals(model)
  }
  
  # Create Q-Q plot data
  n <- length(residuals_val)
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
  sample_quantiles <- sort(scale(residuals_val))
  
  plot_data <- data.frame(
    Theoretical = theoretical_quantiles,
    Sample = sample_quantiles
  )
  
  p <- ggplot(plot_data, aes(x = Theoretical, y = Sample)) +
    geom_point(alpha = 0.7, color = "#2E86AB") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Q-Q Plot",
      subtitle = "Normal Q-Q Plot of Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50")
    )
  
  return(p)
}

# Plot predictions vs observed
plot_predictions_vs_observed <- function(model, data) {
  
  # Prepare data
  model_data <- data %>%
    group_by(Time) %>%
    summarise(Growth_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  
  if(inherits(model, "lm")) {
    predictions <- predict(model)
    observed <- model_data$Growth_Value
  } else {
    pred_data <- data.frame(time = model_data$Time)
    predictions <- predict(model, newdata = pred_data)
    observed <- model_data$Growth_Value
  }
  
  # Calculate R-squared
  ss_res <- sum((observed - predictions)^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  plot_data <- data.frame(
    Observed = observed,
    Predicted = predictions
  )
  
  # Calculate range for 1:1 line
  range_vals <- range(c(observed, predictions), na.rm = TRUE)
  
  p <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.7, color = "#2E86AB", size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "#FF6B6B", alpha = 0.3) +
    labs(
      title = "Predictions vs Observed",
      subtitle = paste("R² =", round(r_squared, 3)),
      x = "Observed Values",
      y = "Predicted Values"
    ) +
    coord_fixed() +
    xlim(range_vals) +
    ylim(range_vals) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50")
    )
  
  return(p)
}