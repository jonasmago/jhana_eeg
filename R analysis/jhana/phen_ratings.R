library(dplyr)
library(ggplot2)

# Define the columns to compare within the 'jhana' type
columns <- c(
  "jhana_strength_nimitta", "jhana_stabilit_j1", "jhana_stabilit_j2", "jhana_stabilit_j3", 
  "jhana_stabilit_j4", "jhana_stabilit_j4", "jhana_stabilit_jbeeps", "jhana_fading_j1", 
  "jhana_fading_j2", "jhana_fading_j3", "jhana_fading_j4", "jhana_fading_beeps", 
  "jhana_choice_beeps", "jhana_beep_distraction", 
  "mindfulness_stability_before_beeps",
  "mindfulness_stability_during_beeps",
  "mindfulness_fading_before_beeps",
  "mindfulness_fading_during_beeps",
  "mindfulness_jhana_before_beeps",
  "mindfulness_jhana_during_beeps",
  "mindfulness_beep_distraction"
)


# Loop through each column
for (column in columns) {
  print(column)
  
  anova_result <- aov(data[[column]] ~ run, data = data)
  print(summary(anova_result))
  summary(anova_result)
  p <- summary(anova_result)[[1]][['Pr(>F)']][[1]]
  p <- round(p, digits = 3)
  
  
  type <- sub("_.*", "", column)
  type_data <- data[data$type == type, ]
  
  # Convert the column to numeric, non-numeric values will be converted to NA
  type_data[[column]] <- as.numeric(type_data[[column]])
  
  # Remove rows with NA values in the current column
  type_data <- type_data[!is.na(type_data[[column]]), ]
  
  # Create summary data
  summary_data <- type_data %>%
    group_by(run) %>%
    summarise(mean_strength = mean(!!sym(column)),
              se_strength = sd(!!sym(column)) / sqrt(n()))
  
  # Plot the results
  plot <- ggplot(summary_data, aes(x = factor(run), y = mean_strength, fill = factor(run))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_errorbar(aes(ymin = mean_strength - se_strength, ymax = mean_strength + se_strength),
                  width = 0.2, position = position_dodge(0.7)) +
    labs(x = "Run", y = paste("Mean", column), title = paste(column, "by run \n (p-value: ", p,")")) +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 28),  # Adjust title size
      axis.title = element_text(size = 24),  # Adjust axis title size
      axis.text = element_text(size = 24)    # Adjust axis text size
    )
  
  # Print the plot
  print(plot)
}










# Comparing Control VS Mindfulness conditoins

columns <- c(
  "mindfulness_stability_before_beeps",
  "mindfulness_stability_during_beeps",
  "mindfulness_fading_before_beeps",
  "mindfulness_fading_during_beeps",
  "mindfulness_jhana_before_beeps",
  "mindfulness_jhana_during_beeps",
  "mindfulness_beep_distraction"
)


for (column in columns) {
  # Filter your data for the two groups you're interested in and select the current column
  filtered_data <- data %>%
    filter((type == "control") | (type == "mindfulness" & run == 1)) %>%
    mutate(group = ifelse(type == "control", "Control", "Mindfulness Run 1")) %>%
    select(all_of(column), group)  # dynamically keep the current column and group
  
  # Perform t-test between groups for the current column
  t_test_formula <- as.formula(paste(column, "~ group"))  # Create dynamic formula
  t_test_result <- t.test(t_test_formula, data = filtered_data, var.equal = TRUE)
  
  # Get p-value and round it
  p_value <- round(t_test_result$p.value, digits = 3)
  
  # Calculate mean and standard error for each group for the current column
  summary_data <- filtered_data %>%
    group_by(group) %>%
    summarise(
      mean_strength = mean(get(column), na.rm = TRUE),
      se_strength = sd(get(column), na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'  # remove grouping structure after summarising
    )
  
  # Plot the results for the current column
  plot <- ggplot(summary_data, aes(x = group, y = mean_strength, fill = group)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_errorbar(aes(ymin = mean_strength - se_strength, ymax = mean_strength + se_strength), width = 0.25, position = position_dodge(0.7)) +
    labs(x = "Group", y = paste("Mean", column), title = paste("Comparison of", column, "\n(p-value:", p_value, ")")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  # Print the plot
  print(plot)
}

