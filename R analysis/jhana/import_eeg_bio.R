library(readxl)
library(dplyr)

# Load the data
#data_eeg <- read.csv("../../notebooks/resting state/eeg_metrics_ar.csv")
data_eeg <- read.csv("../../notebooks/resting state/eeg_metrics_hand.csv")

# Perform all the transformations using dplyr
data_eeg <- data_eeg %>%
  # Rename columns
  rename(type = condition, ID = sub, run = day) %>%
  # Modify the ID and type columns based on the condition
  mutate(
    ID = ifelse(grepl("^control_", ID), sub("control_", "", ID), ID),
    type = ifelse(grepl("^control_", ID), "control", type)
  ) %>%
  # Modify the run column to remove "day" prefix and replace "control" with NA
  mutate(
    run = ifelse(grepl("^day", run), sub("day", "", run), run),
    run = ifelse(run == "control", NA, run)
  )

data_eeg_combined <- merge(data, data_eeg, by = c("ID", "run", "type"))


bio_data <- read_excel("../../notebooks/bio/output.xlsx")

bio_data <- bio_data %>%
  mutate(
    ID = as.integer(sub("sub(\\d+).*", "\\1", name)),
    run = as.integer(ifelse(grepl("day(\\d+)", name), sub(".*day(\\d+).*", "\\1", name), NA)),
    type = sub("sub\\d+(-day\\d+)?-(.*)-raw\\.fif", "\\2", name)
  )

data_eeg_combined <- merge(data_eeg_combined, bio_data, by = c("ID", "run", "type"))
