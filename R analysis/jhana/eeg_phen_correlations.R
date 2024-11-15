library(ggplot2)
library(broom)

# Define a function to perform regression, print statistics, and create a plot
run_regression <- function(data, x_var, y_var) {
  # Fit the regression model
  formula <- as.formula(paste(y_var, "~", x_var))
  model <- lm(formula, data = data)
  
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Extract the p-value of the model
  p_value <- summary_model$coefficients[2, 4]
  
  # Print the statistical significance
  print(summary_model)
  
  # Create a plot with a line of best fit and confidence interval
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(
      title = paste("Regression of", y_var, "on", x_var, "\nP-value:", format(p_value, digits = 3)),
      x = x_var,
      y = y_var
    ) +
    theme_minimal()
  
  print(plot)
}

# Run the regressions and create plots for the specified pairs
regressions <- list(
  c("dat", "lempel_ziv_norm"),
  c("dat", "permutation_entropy"),
  c("dat", "gamma"),
  c("jhana_strength_nimitta", "lempel_ziv_norm"),
  c("jhana_stabilit_j1", "lempel_ziv_norm"),
  c("jhana_stabilit_j2", "lempel_ziv_norm"),
  c("jhana_stabilit_j3", "lempel_ziv_norm"),
  c("jhana_stabilit_j4", "lempel_ziv_norm"),
  c("mindfulness_stability_before_beeps", "lempel_ziv_norm"),
  c("jhana_stabilit_j1", "dat"),
  c("jhana_stabilit_j2", "dat"),
  c("jhana_stabilit_j3", "dat"),
  c("jhana_stabilit_j4", "dat"),
  
  
  c("jhana_stabilit_j1", "Mean_HR"), #0.04
  c("jhana_stabilit_j2", "Mean_HR"),
  c("jhana_stabilit_j3", "Mean_HR"),
  c("jhana_stabilit_j4", "Mean_HR"),
  
  c("jhana_stabilit_j1", "SDNN"),
  c("jhana_stabilit_j1", "RMSSD"), #0.04
  c("jhana_stabilit_j2", "RMSSD"), #0.04
  c("jhana_stabilit_j3", "RMSSD"), #0.04
  c("jhana_stabilit_j4", "RMSSD"), #0.04
  c("jhana_stabilit_j1", "LF_HF_Ratio"),

  c("mindfulness_stability_before_beeps", "Mean_HR"),
  c("mindfulness_stability_before_beeps", "SDNN"),
  c("mindfulness_stability_before_beeps", "RMSSD"),
  c("mindfulness_stability_before_beeps", "LF_HF_Ratio"),
  
  c("jhana_stabilit_j1", "jhana_stabilit_j2"),
  c("jhana_stabilit_j1", "jhana_stabilit_j3"),
  c("jhana_stabilit_j1", "jhana_stabilit_j4")
)

for (regression in regressions) {
  run_regression(data_eeg_combined, regression[1], regression[2])
}