library(readxl)

# Replace the path below with the path to your Excel file
data <- read_excel("../master_phen_dat.xlsx")
head(data)
data$type <- as.factor(data$type)
data$subject <- as.factor(data$subject)
data$run <- as.factor(data$run)


jhana_columns <- c(
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
  "mindfulness_beep_distraction",
  "dat"
)

for (column in jhana_columns) {
  data[[column]] <- as.numeric(data[[column]])
}