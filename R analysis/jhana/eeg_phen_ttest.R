# Load necessary library
library(ggplot2)

# Define a function to perform paired t-test and create a bar plot
plot_ttest <- function(data, variable, group_var = "type", id_var = "ID", run_var = "run", group1 = "jhana", group2 = "mindfulness") {
  # Extract the values for each group
  group1_values <- data[[variable]][data[[group_var]] == group1]
  group2_values <- data[[variable]][data[[group_var]] == group2]
  
  # Perform the paired t-test
  t_test_result <- t.test(group1_values, group2_values, paired = TRUE)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(
    group = c(group1, group2),
    mean_value = c(mean(group1_values, na.rm = TRUE), mean(group2_values, na.rm = TRUE)),
    se_value = c(sd(group1_values, na.rm = TRUE) / sqrt(length(group1_values)), sd(group2_values, na.rm = TRUE) / sqrt(length(group2_values)))
  )
  
  # Create the bar plot
  plot <- ggplot(plot_data, aes(x = group, y = mean_value, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2, position = position_dodge(0.7)) +
    labs(
      title = paste("Mean", variable, "in", group1, "vs.", group2, "\nP-value:", round(t_test_result$p.value, 4)),
      x = "Group",
      y = paste("Mean", variable)
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Print the plot and the t-test result
  print(plot)
  print(t_test_result)
}







# List of variables to analyze
variables <- c("Mean_SCL", "Num_SCRs", "Mean_SCR_Amplitude", "Mean_HR", "SDNN", "RMSSD", "LF", "HF", "LF_HF_Ratio", 
               "Mean_RR", "Mean_Amplitude", "Mean_Cycle_Duration", "RR_Variability", "Amplitude_Variability", 
               "Cycle_Duration_Variability", "lempel_ziv_norm")

# Loop through each variable and call the plot_ttest function
for (variable in variables) {
  plot_ttest(data_eeg_combined, variable)
}


variables <- c("lempel_ziv_norm", "permutation_entropy", "spectral_entropy", 
                          "sample_entropy", "hjorth_mobility", "hjorth_complexity", "average_corr", 
                          "delta", "theta", "alpha", "beta", "gamma")
for (variable in variables) {
  plot_ttest(data_eeg_combined, variable)
}