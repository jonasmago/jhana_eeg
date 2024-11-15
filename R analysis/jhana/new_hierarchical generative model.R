# Set options to prevent scientific notation and control decimal places
options(scipen = 999, digits = 6)

# Load necessary libraries
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(readr)

# List of measures to include in the hierarchical regression
measures <- c("Mean_SCL", "Num_SCRs", "Mean_SCR_Amplitude", "Mean_HR", "SDNN", "RMSSD", "LF", "HF", "LF_HF_Ratio", 
              "Mean_RR", "Mean_Amplitude", "Mean_Cycle_Duration", "RR_Variability", "Amplitude_Variability", 
              "Cycle_Duration_Variability", "lempel_ziv_norm", "permutation_entropy", "spectral_entropy", 
              "sample_entropy", "hjorth_mobility", "hjorth_complexity", "nk_fsi", "nk_lle", "nk_sampen", 
              "nk_pen", "nk_lzc", "delta", "theta", "alpha", "beta", "gamma", "dat")

# Create an empty dataframe to store the results
results_df <- data.frame(
  Measure = character(),
  Beta = numeric(),
  P_Value = numeric(),
  Percent_Change = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each measure
for (measure in measures) {
  # Print a separator line before each measure
  cat("######################\n")
  cat("Running analysis for measure:", measure, "\n")
  cat("######################\n")
  
  # Load the data
  data <- read_csv("../../notebooks/resting state/df_combined.csv", show_col_types = FALSE)
  
  # Convert 'day' column to numeric, handling 'day' values like 'day1', 'day2'
  data$day <- as.numeric(gsub("day", "", data$day))
  
  # Filter data to include only Jhana and Mindfulness conditions (excluding Control)
  filtered_data <- data %>% filter(condition %in% c("jhana", "mindfulness"))
  
  # Relevel 'condition' to make 'Jhana' the reference level
  filtered_data$condition <- relevel(factor(filtered_data$condition), ref = "jhana")
  
  # Fit the linear mixed-effects model
  formula <- as.formula(paste(measure, "~ condition + (1 | sub) + (1 | day)"))
  model <- lmer(formula, data = filtered_data)
  
  # Extract the beta (Estimate) and p-value for the fixed effect 'condition'
  beta_condition <- summary(model)$coefficients["conditionmindfulness", "Estimate"]
  p_value_condition <- summary(model)$coefficients["conditionmindfulness", "Pr(>|t|)"]
  
  # Calculate percentage change based on the means of the two conditions
  means <- filtered_data %>%
    group_by(condition) %>%
    summarise(mean_value = mean(.data[[measure]], na.rm = TRUE)) %>%
    pull(mean_value)
  
  if (length(means) == 2) {
    percent_change <- 100 * (means[1] - means[2]) / means[2]  # Reverse the order for Jhana vs Mindfulness
  } else {
    percent_change <- NA  # In case there's missing data
  }
  
  # Store the results in the results_df
  results_df <- rbind(results_df, data.frame(
    Measure = measure,
    Beta = beta_condition,
    P_Value = p_value_condition,
    Percent_Change = percent_change
  ))
  
  # Optional: Print the model summary for each measure
  print(paste("Model output for", measure, ":"))
  print(summary(model))
  
  # Summarize the measure by condition for plotting
  summary_data <- filtered_data %>%
    group_by(condition) %>%
    summarise(
      mean = mean(.data[[measure]], na.rm = TRUE),
      sd = sd(.data[[measure]], na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      .groups = 'drop'
    )
  
  # Create a shorter title to include the p-value and ensure the text fits
  plot_title <- paste("Comparison of", measure, "\n(p =", format(p_value_condition, digits = 3), ")")
  
  # Create the bar plot and scatter plot (distribution of individual points)
  plot <- ggplot(summary_data, aes(x = condition, y = mean, fill = condition)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.2, position = position_dodge(0.7)) +
    geom_point(data = filtered_data, aes(x = condition, y = .data[[measure]]), 
               color = "black",  # Set the dot color to black or any contrasting color
               position = position_jitter(width = 0.2), alpha = 0.6, size = 2) +  # Scatter plot of individual points
    labs(x = "Condition", y = paste("Mean of", measure), title = plot_title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),  # Adjust title size
      axis.title = element_text(size = 16),  # Adjust axis title size
      axis.text = element_text(size = 14)    # Adjust axis text size
    )
  
  # Explicitly print the plot inside the loop
  print(plot)
}

# Print the final results dataframe
print(results_df)

# Reset options after the analysis, if needed
options(scipen = 0, digits = 7)
