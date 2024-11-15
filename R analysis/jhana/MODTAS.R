library(ggplot2)
library(dplyr)

# Assuming 'data' is your dataframe and 'MODTAS_sum' is the column you're interested in
# First, filter the data for the 'jhana' and 'control' groups
jhana_control_data <- data %>%
  filter(type %in% c("jhana", "control")) %>%
  mutate(group = as.factor(type))  # Convert 'type' into a factor for plotting


t_test_result <- t.test(MODTAS_sum ~ group, data = jhana_control_data, var.equal = TRUE)
p_value <- round(t_test_result$p.value, digits = 3)


# Calculate mean and standard error for 'MODTAS_sum' within each group
summary_data <- jhana_control_data %>%
  group_by(group) %>%
  summarise(
    mean_MODTAS_sum = mean(MODTAS_sum, na.rm = TRUE),
    se_MODTAS_sum = sd(MODTAS_sum, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'  # Drop the grouping
  )

# Plot the results with 'ggplot2'
plot <- ggplot(summary_data, aes(x = group, y = mean_MODTAS_sum, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_MODTAS_sum - se_MODTAS_sum, ymax = mean_MODTAS_sum + se_MODTAS_sum),
                width = 0.25, position = position_dodge(0.7)) +
  labs(x = "Group", y = "Mean MODTAS_sum", title = "Comparison of MODTAS_sum Between Jhana and Control Groups") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )+
  annotate("text", x = 1.5, y = max(summary_data$mean_MODTAS_sum + summary_data$se_MODTAS_sum), label = paste("p-value:", p_value), size = 5)
print(plot)






# Perform correlation test between MODTAS_sum and dat
correlation_result <- cor.test(data$MODTAS_sum, data$dat, method = "pearson")  # Assuming normal distribution and linear relationship

correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value

cat("Correlation coefficient (Pearson's r):", correlation_coefficient, "\n")
cat("P-value:", p_value, "\n")

cat("The Pearson correlation between MODTAS_sum and dat is", correlation_coefficient, "with a p-value of", p_value, "\n")





# Plot the scatter plot with a regression line
p_value <- round(t_test_result$p.value, digits = 3)
plot <- ggplot(data, aes(x = MODTAS_sum, y = dat)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line without the confidence interval
  theme_minimal() +
  labs(x = "MODTAS Sum", y = "DAT Variable", title = "Correlation between MODTAS_sum and DAT")+
  theme(
    plot.title = element_text(size = 28),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

print(plot)

