# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lme4)
library(lmerTest)

##########################################
### Mixed Linear Effects Comparing DAT ###
##########################################

# Load the data
data <- read_csv("../../notebooks/resting state/df_combined.csv")

# Convert 'day' column to numeric
data$day <- as.numeric(gsub("day", "", data$day))

# Filter data to include only Jhana and Mindfulness conditions (excluding Control)
filtered_data <- data %>% filter(condition %in% c("jhana", "mindfulness"))

# Fit the linear mixed-effects model
# The model tests if the condition (Jhana vs Mindfulness) affects 'dat'
# Random effects: (1 | sub) accounts for random variation across subjects
# (1 | day) accounts for random variation across different days
model <- lmer(dat ~ condition + (1 | sub) + (1 | day), data = filtered_data)

# Summarize the model to check significance
summary(model)

# Extract the p-value for the fixed effect 'condition'
anova_model <- anova(model)
p_value_condition <- anova_model$`Pr(>F)`[1]
print(paste("P-value for the fixed effect 'condition':", p_value_condition))

summary_data <- filtered_data %>%
  group_by(condition) %>%
  summarise(
    mean = mean(dat, na.rm = TRUE),
    sd = sd(dat, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'  # Ensure it's not grouped anymore after summarizing
  )

# Create the bar plot
plot <- ggplot(summary_data, aes(x = condition, y = mean, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.7)) +
  labs(x = "Condition", y = "Mean of Dat", title = "Comparison of Mean Dat Values (Jhana vs Mindfulness)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),  # Adjust title size
    axis.title = element_text(size = 16),  # Adjust axis title size
    axis.text = element_text(size = 14)    # Adjust axis text size
  )

# Print the plot
print(plot)


##################################################
### Correlation between DAT and other measures ###
##################################################

# List of variables you want to correlate with 'dat'
variables_to_analyze <- c("lempel_ziv_norm",	"permutation_entropy",	"spectral_entropy",	"sample_entropy",	"hjorth_mobility",	"hjorth_complexity",	"nk_fsi",	"nk_lle",	"nk_sampen",	"nk_pen",	"nk_lzc",	"delta",	"theta",	"alpha",	"beta",	"gamma", "MODTAS_sum_repeated", "meditation_depth_before", "meditation_depth_mean")

# Loop through each variable
for (measure in variables_to_analyze) {
  
  # Fit the linear mixed-effects model
  formula <- as.formula(paste("dat ~", measure, "+ (1 | sub) + (1 | day)"))
  model <- lmer(formula, data = filtered_data)
  
  # Summarize the model to check the significance of the variable
  summary(model)
  
  # Extract the p-value for the fixed effect
  anova_model <- anova(model)
  p_value <- anova_model$`Pr(>F)`[1]
  print(paste("P-value for the fixed effect", measure, ":", p_value))
  
  # Create a scatter plot with regression line and distinct colors for subjects
  plot <- ggplot(filtered_data, aes_string(x = measure, y = "dat", color = "factor(sub)")) +
    geom_point(alpha = 0.6) +  # Scatter plot with distinct color for each subject
    geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line with 95% CI
    labs(x = measure, y = "Dat", 
         title = paste("Correlation between DAT and", measure, "\n (p =", format(p_value, digits = 3), ")")) +
    scale_color_discrete(name = "Subject") +  # Discrete color scale for subjects
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20),  # Adjust title size
      axis.title = element_text(size = 16),  # Adjust axis title size
      axis.text = element_text(size = 14),   # Adjust axis text size
      legend.title = element_text(size = 14),  # Adjust legend title size
      legend.text = element_text(size = 12)    # Adjust legend text size
    )
  
  # Print the plot
  print(plot)
}
