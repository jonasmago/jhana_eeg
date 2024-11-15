# Load necessary library
library(ggplot2)

# Create a data frame with your data
data <- data.frame(
  Group = c("Jhana", "Mindfulness"),
  Value = c(7.459, 6.5),
  Error = c(0.3, 0.3)
)

# Create the bar plot
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  geom_errorbar(aes(ymin = Value - Error, ymax = Value + Error), 
                width = 0.2, position = position_dodge(0.5)) +
  labs(title = "DAT results", x = "", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  scale_fill_manual(values = c("skyblue", "orange"))
