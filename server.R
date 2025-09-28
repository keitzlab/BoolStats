
library(shiny)
library(lme4)
library(multcomp)
library(ggplot2)
library(grDevices)
library(dplyr) 

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
    
    group_colors <- c("gray1", "gray5", "gray7", "gray14")
    
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
    
    # Check for singular fit and if so refit model
    Singular <- isSingular(model)
    if (Singular){ # refit without random effects (note this uses lm, not lmer)
      model <- lm(Results ~ Expected, data = model_data)
      singularText <- 'The fitted model is singular.\nThe random effects structure has been dropped and the model refit.\n The test was performed on the refit model.\n\n'
    } else {
      singularText <- 'The fitted model is not singular.\n\n'
    }
    
    
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
    dynamic_range <- 1 - (group_avgs[ranked_groups[1]] / group_avgs[ranked_groups[4]])
    
    # Display parsed results and cohesiveness score
    parsed_results <- paste(
      "=== Analysis Results ===\n\n",
      "Model fit:\n",
      singularText,
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
      paste("Dynamic Range: ", round(dynamic_range, 4), "\n"),
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
        scale_fill_manual(values = c("steelblue", "royalblue", "deepskyblue", "dodgerblue"))
    })
    
    output$cohesiveness_vs_dynamic <- renderPlot({
      
      data_to_plot_1 <- data.frame(
        Cohesiveness = cohesiveness_score,
        DynamicRange = dynamic_range,
        Dataset = "User Gate"  # Label for the first dataset
      )
      
      # New dataset
      data_to_plot_2 <- data.frame(
        Cohesiveness = c(0.63084018, 0.60760225, 0.70867431, 0.76171417, 0.68765895, 0.55506384, 0.67087006, 0.71119724, 
                         0.64567328, 0.77856886, 0.6999246, 0.70810002, 0.63429956, 0.65397232, 0.64081114, 0.70300344, 
                         0.69156204, 0.68616423, 0.7751615, 0.65113182, 0.69943214, 0.6889828, 0.7426313, 0.67668494, 
                         0.67141735, 0.65588511, 0.72586196, 0.71770221, 0.77108294, 0.72778724, 0.65756189, 0.5423235, 
                         0.65412642, 0.69159894, 0.55123382, 0.64198646, 0.58696167, 0.7580417, 0.62129139, 0.87525162, 
                         0.83622562, 0.7346857, 0.77321747, 0.81144076, 0.75147789, 0.9069946, 0.75827261, 0.68348237, 
                         0.84056915, 0.70588235, 0.87626294, 0.89306429, 0.89961408, 0.75075746, 0.61901401, 0.58300109, 
                         0.59443567, 0.53247942, 0.76674738, 0.76842546, 0.67904012, 0.47217649, 0.61892182, 0.5248882, 
                         0.5332618, 0.24955616, 0.21179056, 0.05186908, 0.01196017, 0.06396005, 0.02122554, 0.21381322, 
                         0.30526207, 0.02212403, 0.13453286, 0.14462649, 0.18871238, 0.11356602, 0.29013771, 0.26716445, 
                         0.22214146, 0.89441605, 0.92739511, 0.9080192, 0.81550455, 0.87244165, 0.8459684, 0.81859826, 
                         0.92741988, 0.96387952, 0.96448969, 0.92190572, 0.91607852, 0.78974031, 0.80244389, 0.86834565, 
                         0.84740307, 0.81077597, 0.99775617, 0.82456699, 0.89082194, 0.93053874, 0.88897959, 0.84207298, 
                         0.9121695, 0.91803279, 0.88046882, 0.93932593, 0.86188734, 0.87310207, 0.96169259, 0.80496823, 
                         0.8371772, 0.82742833, 0.87127371, 0.96863501, 0.99281207, 0.86593758, 0.9780938, 0.99847547, 
                         0.97115204, 0.97279882, 0.94036619, 0.95947717, 0.91080387, 0.93263996, 0.91179729, 0.84829338, 
                         0.84345958, 0.82427081, 0.79309996, 0.53213292, 0.49067222, 0.48510095, 0.50391106, 0.43332591, 
                         0.48856436, 0.58779905, 0.41699891, 0.5553002, 0.63292821, 0.58250997, 0.55403532, 0.49208644), 
        DynamicRange = c(0.93617205, 0.95755295, 0.953, 0.936, 0.92426961, 0.913, 0.91378795, 0.83632116, 0.91408597, 
                         0.97910842, 0.86974978, 0.89389441, 0.89634654, 0.90755092, 0.9933802, 0.96419745, 0.97611177, 
                         0.97951844, 0.99572514, 0.86131846, 0.87565949, 0.99697257, 0.89860148, 0.99218328, 0.99400354, 
                         0.97890526, 0.95686535, 0.97273756, 0.90151116, 0.87348491, 0.87084616, 0.97584416, 0.98101698, 
                         0.96713793, 0.97629249, 0.98297147, 0.92350791, 0.94101739, 0.90351831, 0.48914775, 0.73724715, 
                         0.53095118, 0.79895604, 0.76242796, 0.69041065, 0.60602888, 0.73394099, 0.68549049, 0.71918015, 
                         0.66871537, 0.48609559, 0.7461144, 0.6000475, 0.75060483, 0.15744935, 0.44520949, 0.05703153, 
                         0.43618013, -0.0059246, 0.16696948, 0.03239561, 0.16050086, 0.26983898, 0.29899412, 0.30469175, 
                         0.49612769, 0.61138335, 0.66103545, 0.68080696, 0.55153917, 0.52026401, 0.61732638, 0.74227435, 
                         0.62033698, 0.68158299, 0.38853339, 0.7481203, 0.61858638, 0.65866329, 0.68615019, 0.6710011, 
                         0.78838584, 0.97837588, 0.93661669, 0.84740053, 0.83576734, 0.857, 0.96319713, 0.99332046, 
                         0.96693957, 0.92865525, 0.86692288, 0.8820922, 0.85810664, 0.92700676, 0.98994186, 0.99159299, 
                         0.94752884, 0.98785938, 0.99697426, 0.96983187, 0.98393161, 0.94095695, 0.9881787, 0.84332101, 
                         0.98593591, 0.93902647, 0.86143543, 0.9230018, 0.8183386, 0.84033276, 0.85553349, 0.91399065, 
                         0.8922936, 0.99200029, 0.92446059, 0.99833252, 0.99818131, 0.85082867, 0.98920646, 0.98489495, 
                         0.98334106, 0.97622906, 0.93171243, 0.92589975, 0.8457289, 0.89260136, 0.87063312, 0.8686713, 
                         0.95513007, 0.96340958, 0.78603776, 0.72435262, 0.73153036, 0.64290074, 0.82171356, 0.8195816, 
                         0.7699221, 0.72551648, 0.85061244, 0.78293365, 0.79642221, 0.72849402, 0.63270512),
        Dataset = "Gates from Literature"  # Label for the second dataset
      )
      
      # Combine both datasets
      combined_data <- rbind(data_to_plot_2, data_to_plot_1)
      
      # Plot the data
      ggplot(combined_data, aes(x = Cohesiveness, y = DynamicRange, color = Dataset)) +
        geom_point(size = 3) +
        
        labs(title = "Cohesiveness Score vs Dynamic Range", x = "Cohesiveness Score", y = "Dynamic Range") +
        theme_minimal() +
        scale_x_continuous(limits = c(0, 1)) +  # Set x-axis range from 0 to 1
        scale_y_continuous(limits = c(0, 1)) +   # Set y-axis range from 0 to 1
        scale_color_manual(values = c("gray", "dodgerblue"))  # Customize colors
    })
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("analysis_report_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file, width = 8, height = 10)
        
        # Add parsed results
        grid::grid.text(parsed_results, x = 0.5, y = 0.95, just = "center", gp = grid::gpar(fontsize = 12))
        
        # Print the table
        gridExtra::grid.table(model_data)
        
        # Print the plot
        print(ggplot(data.frame(Group = factor(1:4), Average = group_avgs, SD = group_sd), 
                     aes(x = Group, y = Average, fill = Group)) +
                geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
                geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD), width = 0.25, color = "black") +
                labs(title = "Group Averages with Error Bars", x = "Group", y = "Average Value") +
                theme_minimal() +
                scale_fill_manual(values = c("steelblue", "royalblue", "deepskyblue", "dodgerblue")))
        
        # Add the cohesiveness vs dynamic range plot
        print(ggplot(combined_data, aes(x = Cohesiveness, y = DynamicRange, color = Dataset)) +
                geom_point() +
                geom_smooth(method = "lm", se = FALSE, aes(color = Dataset)) +
                labs(title = "Cohesiveness vs. Dynamic Range", x = "Cohesiveness", y = "Dynamic Range") +
                theme_minimal())
        
        dev.off()
      }
    )
    
  })
}
