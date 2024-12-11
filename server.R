
library(shiny)
library(lme4)
library(multcomp)
library(ggplot2)
library(grDevices)
library(dplyr)  # Optional, in case you're doing additional data manipulation
server <- function(input, output, session) {
  
  # Reactive values for storing expected values based on logic
  expected_values <- reactive({
    switch(input$logic,
           "OR" = c(0, 1, 1, 1),
           "NOR" = c(1, 1, 1, 0),
           "AND" = c(0, 0, 0, 1),
           "NAND" = c(1, 1, 1, 0),
           "XOR" = c(0, 1, 1, 0),
           "XNOR" = c(1, 0, 0, 1),
           "IMPLY" = c(1, 1, 0, 1),
           "NIMPLY" = c(0, 0, 1, 0))
  })
  
  # Reactive storage for results
  results_input <- reactiveValues(data = NULL)
  
  # Reactive value to track if analysis is run
  analysis_started <- reactiveVal(FALSE)
  
  # Initialize the data frame on "Submit" button click
  observeEvent(input$submit, {
    result_data <- data.frame(
      Group = rep(1:4, each = input$n_samples),
      Sample = rep(1:input$n_samples, times = 4),
      Expected = rep(expected_values(), each = input$n_samples),
      Results = rep(NA, 4 * input$n_samples),
      stringsAsFactors = FALSE
    )
    
    results_input$data <- result_data
    analysis_started(FALSE)  # Reset analysis state
  })
  
  # Render the dynamic table for data entry
  output$dynamic_table <- renderUI({
    req(results_input$data)
    req(!analysis_started())  # Only show table if analysis is not started
    
    group_colors <- c("chocolate1", "chocolate2", "chocolate3", "orange3")
    
    group_colors_hex <- sapply(group_colors, function(color) {
      rgb_vals <- col2rgb(color)
      rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
    })
    
    table_ui <- lapply(1:4, function(group_num) {
      group_data <- subset(results_input$data, Group == group_num)
      
      group_header <- fluidRow(
        column(12, tags$h4(paste("Group ", group_num, ": Expected Value = ", unique(group_data$Expected), sep = "")))
      )
      
      group_rows <- lapply(1:nrow(group_data), function(i) {
        fluidRow(
          column(2, strong(paste("Sample ", group_data$Sample[i], sep = ""))),
          column(2, numericInput(paste("result_", group_num, "_", i, sep = ""), "Result:", value = group_data$Results[i], min = 0, max = 1))
        )
      })
      
      tags$div(
        style = paste("border: 2px solid ", group_colors_hex[group_num], "; padding: 10px; margin-bottom: 20px;"),
        group_header,
        do.call(tagList, group_rows)
      )
    })
    
    do.call(fluidPage, table_ui)
  })
  
  # Perform analysis on "Run Analysis" button click
  observeEvent(input$run_analysis, {
    req(results_input$data)
    
    # Mark analysis as started
    analysis_started(TRUE)
    
    
    # Update Results column with user inputs
    for (group_num in 1:4) {
      group_data <- subset(results_input$data, Group == group_num)
      
      for (i in 1:nrow(group_data)) {
        result_value <- input[[paste("result_", group_num, "_", i, sep = "")]]
        
        if (!is.null(result_value)) {
          results_input$data$Results[results_input$data$Group == group_num & results_input$data$Sample == group_data$Sample[i]] <- as.numeric(result_value)
        }
      }
    }
    
    model_data <- results_input$data
    model_data$Group <- factor(model_data$Group)
    
    # Check for missing results
    if (any(is.na(model_data$Results))) {
      output$analysis_results <- renderText({
        "Error: Some results are missing. Please enter all result values."
      })
      return()
    }
    
    # Statistical modeling and hypothesis testing
    model <- lmer(Results ~ Expected + (1 | Group), data = model_data)
    test <- glht(model, linfct = c("Expected == 0"))
    
    # Capture the console output for the analysis
    test_summary <- capture.output(summary(test))
    conf_intervals <- capture.output(confint(test))
    
    # Combine the test summary and confidence intervals into one output
    full_output <- c(test_summary, conf_intervals)
    
    # Extracting values from the "Test Summary"
    test_summary_values <- strsplit(full_output[8], "\\s+")[[1]]
    estimate <- test_summary_values[4]
    std_error <- test_summary_values[5]
    z_value <- test_summary_values[6]
    p_value <- test_summary_values[7]
    significance <- test_summary_values[8]
    
    # Extracting values from the "Confidence Intervals"
    conf_interval_values <- strsplit(full_output[24], "\\s+")[[1]]
    conf_estimate <- conf_interval_values[4]
    lower_bound <- conf_interval_values[5]
    upper_bound <- conf_interval_values[6]
    
    # Compute averages for each group
    group_avgs <- sapply(1:4, function(group_num) {
      mean(results_input$data$Results[results_input$data$Group == group_num], na.rm = TRUE)
    })
    
    # Rank groups by average
    ranked_groups <- order(group_avgs)
    group_labels <- c("B1", "B2", "B3", "B4")
    ranked_labels <- group_labels[ranked_groups]
    
    # Compute cohesiveness score based on logic
    if (input$logic %in% c("OR", "NAND", "IMPLY")) {
      cohesiveness_score <- (log(group_avgs[ranked_groups[3]] * group_avgs[ranked_groups[2]]) - log(group_avgs[ranked_groups[1]]^2)) /
        (log(group_avgs[ranked_groups[4]]^2) - log(group_avgs[ranked_groups[1]]^2))
    } else if (input$logic %in% c("NOR", "AND", "NIMPLY")) {
      cohesiveness_score <- (log(group_avgs[ranked_groups[4]]^2) - log(group_avgs[ranked_groups[3]] * group_avgs[ranked_groups[2]])) /
        (log(group_avgs[ranked_groups[4]]^2) - log(group_avgs[ranked_groups[1]]^2))
    } else if (input$logic %in% c("XOR", "XNOR")) {
      cohesiveness_score <- (log(group_avgs[ranked_groups[3]]) - log(group_avgs[ranked_groups[2]])) /
        (log(group_avgs[ranked_groups[4]]) - log(group_avgs[ranked_groups[1]]))
    }
    
    # Calculate dynamic range
    dynamic_range <- group_avgs[ranked_groups[4]] - group_avgs[ranked_groups[1]]
    
    # Display parsed results and cohesiveness score
    parsed_results <- paste(
      "=== Analysis Results ===\n\n",
      "Test Summary:\n",
      paste("Estimate/Beta: ", estimate, "\n"),
      paste("Standard Error: ", std_error, "\n"),
      paste("Z Value: ", z_value, "\n"),
      paste("P Value: ", p_value, "\n"),
      paste("Significance: ", significance, "\n"),
      "\n 95% Confidence Intervals:\n",
      paste("Estimate: ", conf_estimate, "\n"),
      paste("Lower Bound: ", lower_bound, "\n"),
      paste("Upper Bound: ", upper_bound, "\n"),
      "\n=== Cohesiveness Score ===\n",
      paste("Cohesiveness Score: ", round(cohesiveness_score, 4), "\n"),
      "\n=== Dynamic Range ===\n",
      paste("Dynamic Range (B4 - B1): ", round(dynamic_range, 4), "\n"),
      sep = ""
    )
    
    output$analysis_results <- renderText({
      parsed_results
    })
    
    # Show the table with the results
    output$display_table <- renderTable({
      model_data
    })
    
    # Plot the group averages and standard deviations in a bar plot
    output$bar_plot <- renderPlot({
      group_sd <- sapply(1:4, function(group_num) {
        sd(results_input$data$Results[results_input$data$Group == group_num], na.rm = TRUE)
      })
      
      ggplot(data.frame(Group = factor(1:4), Average = group_avgs, SD = group_sd), aes(x = Group, y = Average, fill = Group)) +
        geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
        geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD), width = 0.25, color = "black") +
        labs(title = "Group Averages with Error Bars", x = "Group", y = "Average Value") +
        theme_minimal() +
        scale_fill_manual(values = c("chocolate1", "chocolate2", "chocolate3", "orange3"))
    })
    
    # Plot cohesiveness score vs. dynamic range with axes limited from 0 to 1
    output$cohesiveness_vs_dynamic <- renderPlot({
      data_to_plot <- data.frame(
        Cohesiveness = cohesiveness_score,
        DynamicRange = dynamic_range
      )
      
      ggplot(data_to_plot, aes(x = Cohesiveness, y = DynamicRange)) +
        geom_point(color = "orange", size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = "Cohesiveness Score vs Dynamic Range", x = "Cohesiveness Score", y = "Dynamic Range") +
        theme_minimal() +
        scale_x_continuous(limits = c(-0.5, 1.5)) +  # Set x-axis range from 0 to 1
        scale_y_continuous(limits = c(-0.5, 1.5))     # Set y-axis range from 0 to 1
    })
    
  })
}
