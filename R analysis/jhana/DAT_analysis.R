# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Read the CSV file
df_combined <- read_csv("../../notebooks/resting state/df_combined.csv")


# Compare ORDER
data_order_1 <- data[data$type == "jhana", ]
data_order_2 <- data[data$type == "mindfulness", ]
paired_data <- merge(data_order_1, data_order_2, by = c("subject", "run"))
t_test_paired <- t.test(paired_data$dat.x, paired_data$dat.y, paired = TRUE)
print(t_test_paired)
p_value <- t_test_paired$p.value
print(p_value)


# List of runs to analyze
runs <- c("all", 1, 2, 3, 4)

# Loop through each run value
for (current_run in runs) {
  # Print which run is being analyzed
  print(paste("Analysis for Run:", current_run))
  
  # Filter data based on the 'current_run' variable if not "all"
  if (current_run != "all") {
    filtered_data <- data %>% filter(run == current_run)
  } else {
    filtered_data <- data
  }
  
  # Perform ANOVA and print results
  anova_result <- aov(dat ~ type, data = filtered_data)
  print(summary(anova_result))
  
  # Compare RUNS
  summary_res <- summary(anova_result)
  p_value_run <- summary_res[[1]][['Pr(>F)']][[1]]
  
  # Calculate means and standard errors for each type
  summary_data <- filtered_data %>%
    group_by(type) %>%
    summarise(
      mean = mean(dat, na.rm = TRUE),
      sd = sd(dat, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      .groups = 'drop'  # Ensures the result is an ungrouped tibble
    )
  
  # Create the bar plot with error bars
  plot_types <- ggplot(summary_data, aes(x = type, y = mean, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.2, position = position_dodge(0.7)) +
    ylim(c(0, 9e+15)) +  # Adjust 'max_limit' to the desired maximum y value
    labs(x = "Type of Practice", y = "Mean of Dat", title = ifelse(current_run == "all", "Comparison of DAT for run", paste("Comparison of All Types for Run", current_run, "\n (p =", format(p_value_run, digits = 3),")"))) +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 28),  # Adjust title size
      axis.title = element_text(size = 24),  # Adjust axis title size
      axis.text = element_text(size = 24)    # Adjust axis text size
    )


  # Print the plot
  print(plot_types)

}


# Compare RUNS
res.aov <- aov(dat ~ run + Error(subject/run), data = subset(data, !is.na(run))) #to exclude NaN runs
summary_res <- summary(res.aov)
p_value_run <- summary_res[[2]][[1]][['Pr(>F)']][[1]]
show(p_value_run)


# Comparing all 4 individual runs (only works with pre and post excluded)
pairwise_comparisons <- pairwise.t.test(data$dat, data$run, p.adjust.method = "bonferroni", paired = TRUE)
print(pairwise_comparisons)



# comparing just run 1 and 4
subset_data <- subset(data, run %in% c(1, 4))
subset_data$run <- as.factor(subset_data$run)
pairwise_comparisons_run1_vs_run4 <- pairwise.t.test(subset_data$dat, subset_data$run, 
                                                     p.adjust.method = "bonferroni", paired = TRUE)
print(pairwise_comparisons_run1_vs_run4)


# Comparing pre and post DAT measures
subset_data <- subset(data, type %in% c("ltp-pre", "ltp-post"))
subset_data$type <- as.factor(subset_data$type)
t_test_result <- t.test(dat ~ type, data = subset_data)
print(t_test_result)


# same but this time it's a paired t-test
dat_ltp_post <- data %>% filter(type == "ltp-post") %>% pull(dat)
dat_ltp_pre <- data %>% filter(type == "ltp-pre") %>% pull(dat)
paired_t_test_result <- t.test(dat_ltp_pre, dat_ltp_post, paired = TRUE)
print(paired_t_test_result)


boxplot <- ggplot(subset_data, aes(x = type, y = dat)) +
  geom_boxplot(aes(fill = type)) +
  labs(title = "DAT Scores by Type",
       x = "Type",
       y = "DAT Score") +
  theme_minimal() +
  scale_fill_manual(values = c("ltp-pre" = "#56B4E9", "ltp-post" = "#E69F00"))
print(boxplot)










# Compare RUNS plot
summary_data_runs <- data %>%
  group_by(run) %>%
  summarise(
    mean = mean(dat, na.rm = TRUE),
    sd = sd(dat, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n)
  )

plot_runs <- ggplot(summary_data_runs, aes(x = factor(run), y = mean, fill = factor(run))) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.7)) +
  labs(x = "Run", y = "Mean of Dat", title = paste("Comparison of DAT Scores Across Runs \n (p =", format(p_value_run, digits = 3), ")")) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 28),  # Adjust title size
    axis.title = element_text(size = 24),  # Adjust axis title size
    axis.text = element_text(size = 24)    # Adjust axis text size
  )

print(plot_runs)


# Compare ORDER
data_order_1 <- data[data$order == 1, ]
data_order_2 <- data[data$order == 2, ]
paired_data <- merge(data_order_1, data_order_2, by = c("subject", "run"))
t_test_paired <- t.test(paired_data$dat.x, paired_data$dat.y, paired = TRUE)
print(t_test_paired)
p_value <- t_test_paired$p.value

# Compare ORDER plot
summary_data_runs <- data %>%
  group_by(order) %>%
  summarise(
    mean = mean(dat, na.rm = TRUE),
    sd = sd(dat, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'  # Ensure the result is an ungrouped tibble
  )

plot_order <- ggplot(summary_data_runs, aes(x = factor(order), y = mean, fill = factor(order))) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.7)) +
  labs(x = "Order", y = "Mean of Dat", title = paste("Comparison of DAT Scores between Order \n 1 and 2 (p-value:", format(p_value, digits = 3), ")")) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 28),  # Adjust title size
    axis.title = element_text(size = 24),  # Adjust axis title size
    axis.text = element_text(size = 24)    # Adjust axis text size
  )

print(plot_order)










###########################
###########################
###########################

# Load necessary libraries
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)

# Filter the data to include only "jhana" and "mindfulness" types
data_jhana_mindfulness <- data %>% filter(type %in% c("jhana", "mindfulness"))

# Fit the linear mixed-effects model
model <- lmer(dat ~ type + (1 | subject) + (1 | run), data = data_jhana_mindfulness)

# Summarize the model
summary(model)

# Extract the p-value for the fixed effect 'type' using the lmerTest package
anova_model <- anova(model)
p_value_type <- anova_model$`Pr(>F)`[1]
print(paste("P-value for the fixed effect 'type':", p_value_type))

# Create the summary data for plotting
summary_data <- data_jhana_mindfulness %>%
  group_by(type) %>%
  summarise(
    mean = mean(dat, na.rm = TRUE),
    sd = sd(dat, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'  # Ensures the result is an ungrouped tibble
  )

# Create the bar plot with error bars
plot_types <- ggplot(summary_data, aes(x = type, y = mean, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.7)) +
  ylim(c(0, 9e+15)) +  # Adjust 'max_limit' to the desired maximum y value
  labs(x = "Type of Practice", y = "Mean of Dat", title = paste("Comparison of DAT for Types \n (p =", format(p_value_type, digits = 3), ")")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28),  # Adjust title size
    axis.title = element_text(size = 24),  # Adjust axis title size
    axis.text = element_text(size = 24)    # Adjust axis text size
  )

# Print the plot
print(plot_types)






# Plot residuals to check for outliers
plot(resid(model))
# Use Cook's distance to identify influential data points
cooksd <- cooks.distance(model)
plot(cooksd, type = "h", main = "Cook's Distance")
abline(h = 4/(nrow(data_jhana_mindfulness)-length(fixef(model))), col = "red")



ggplot(data_jhana_mindfulness, aes(x = type, y = dat)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "Boxplot of DAT by Type", x = "Type of Practice", y = "DAT Value")










# Load necessary libraries
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)

# Filter the data to include only "jhana" and "mindfulness" types
data_jhana_mindfulness <- data %>% filter(type %in% c("jhana", "mindfulness"))

# Create a combined covariate
data_jhana_mindfulness <- data_jhana_mindfulness %>%
  mutate(
    stability = ifelse(type == "jhana", "jhana_stability_j1", "mindfulness_stability_before_beeps")
  )

# Fit the linear mixed-effects model with the additional covariate
model_refined <- lmer(dat ~ type + stability + (1 | subject) + (1 | run), data = data_jhana_mindfulness)

# Summarize the model
summary(model_refined)

# Extract the p-value for the fixed effect 'type'
anova_model_refined <- anova(model_refined)
p_value_type_refined <- anova_model_refined$`Pr(>F)`[1]
print(paste("P-value for the fixed effect 'type' in refined model:", p_value_type_refined))

# Create the summary data for plotting
summary_data <- data_jhana_mindfulness %>%
  group_by(type) %>%
  summarise(
    mean = mean(dat, na.rm = TRUE),
    sd = sd(dat, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'  # Ensures the result is an ungrouped tibble
  )

# Create the bar plot with error bars
plot_types <- ggplot(summary_data, aes(x = type, y = mean, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.7)) +
  ylim(c(0, 9e+15)) +  # Adjust 'max_limit' to the desired maximum y value
  labs(x = "Type of Practice", y = "Mean of Dat", title = paste("Comparison of DAT for Types \n (p =", format(p_value_type_refined, digits = 3), ")")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28),  # Adjust title size
    axis.title = element_text(size = 24),  # Adjust axis title size
    axis.text = element_text(size = 24)    # Adjust axis text size
  )

# Print the plot
print(plot_types)





#######################
### regression ###
#######################


# Create the new variable by averaging the specified columns
data$average_stability <- rowMeans(data[, c("jhana_stabilit_j1", 
                                            "jhana_stabilit_j2", 
                                            "jhana_stabilit_j3", 
                                            "jhana_stabilit_j4", 
                                            "jhana_stabilit_jbeeps", 
                                            "mindfulness_stability_before_beeps", 
                                            "mindfulness_stability_during_beeps")], 
                                   na.rm = TRUE)



model <- lm(dat ~ average_stability, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = average_stability, y = dat)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of dat on average stability",
       x = "average stability",
       y = "dat") +
  theme_minimal() # Use a minimal theme for a clean look



# Create the new variable by averaging the specified columns
data$average_fading <- rowMeans(data[, c("jhana_fading_j1", 
                                            "jhana_fading_j2", 
                                            "jhana_fading_j3", 
                                            "jhana_fading_j4", 
                                            "jhana_fading_beeps", 
                                            "mindfulness_fading_before_beeps", 
                                            "mindfulness_fading_during_beeps")], 
                                   na.rm = TRUE)

model <- lm(dat ~ average_fading, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = average_fading, y = dat)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of dat on average fading",
       x = "average fading",
       y = "dat") +
  theme_minimal() # Use a minimal theme for a clean look





model <- lm(dat ~ jhana_strength_nimitta, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = jhana_strength_nimitta, y = dat)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of dat on jhana_strength_nimitta",
       x = "jhana_strength_nimitta",
       y = "dat") +
  theme_minimal() # Use a minimal theme for a clean look



model <- lm(dat ~ MODTAS_sum, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = MODTAS_sum, y = dat)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of dat on MODTAS_sum",
       x = "MODTAS_sum",
       y = "dat") +
  theme_minimal() # Use a minimal theme for a clean look




model <- lm(average_stability ~ average_fading, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = average_stability, y = average_fading)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of average_stability on average_fading",
       x = "average_stability",
       y = "average_fading") +
  theme_minimal() # Use a minimal theme for a clean look



model <- lm(jhana_strength_nimitta ~ average_fading, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = jhana_strength_nimitta, y = average_fading)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of jhana_strength_nimitta on average_fading",
       x = "jhana_strength_nimitta",
       y = "average_fading") +
  theme_minimal() # Use a minimal theme for a clean look




model <- lm(jhana_strength_nimitta ~ average_stability, data = data)
summary(model)

# Create a scatter plot with the regression line
ggplot(data, aes(x = jhana_strength_nimitta, y = average_stability)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", col = "blue") + # Add regression line
  labs(title = "Regression of jhana_strength_nimitta on average_stability",
       x = "jhana_strength_nimitta",
       y = "average_stability") +
  theme_minimal() # Use a minimal theme for a clean look



