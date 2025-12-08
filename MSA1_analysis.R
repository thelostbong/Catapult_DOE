
library(tidyverse)


msa1_data <- read.csv("MSA1_DataSheet.csv")


reference_distance_cm <- 169.6


mean_reading <- mean(msa1_data$Tape_Reading_cm, na.rm = TRUE)
sigma <- sd(msa1_data$Tape_Reading_cm, na.rm = TRUE)
min_reading <- min(msa1_data$Tape_Reading_cm, na.rm = TRUE)
max_reading <- max(msa1_data$Tape_Reading_cm, na.rm = TRUE)
range_reading <- max_reading - min_reading


bias <- mean_reading - reference_distance_cm

# Calculate tolerance (backwards from target Cg = 2.0)
target_Cg <- 2.0
tolerance <- (target_Cg * 6 * sigma) / 0.2

tolerance <- (target_Cg * 6 * sigma) 


# Calculate capability indices
Cg <- (0.2 * tolerance) / (6 * sigma)
Cgk <- ((0.2 * tolerance) - abs(bias)) / (3 * sigma)

# Pass/Fail decision
Cg_status <- ifelse(Cg >= 1.33, "PASS", "FAIL")
Cgk_status <- ifelse(Cgk >= 1.33, "PASS", "FAIL")
overall_status <- ifelse(Cg >= 1.33 & Cgk >= 1.33, "PASS", "FAIL")

# Create summary table
msa1_summary <- data.frame(
  Metric = c(
    "Number of Trials",
    "Reference Distance (cm)",
    "Mean Tape Reading (cm)",
    "Standard Deviation (cm)",
    "Minimum Reading (cm)",
    "Maximum Reading (cm)",
    "Range (cm)",
    "Bias (cm)",
    "Target Cg",
    "Tolerance (±cm)",
    "Lower Spec Limit (cm)",
    "Upper Spec Limit (cm)",
    "Cg (Precision)",
    "Cg Status",
    "Cgk (Precision + Accuracy)",
    "Cgk Status",
    "Overall MSA Status"
  ),
  Value = c(
    nrow(msa1_data),
    reference_distance_cm,
    round(mean_reading, 3),
    round(sigma, 4),
    round(min_reading, 3),
    round(max_reading, 3),
    round(range_reading, 3),
    round(bias, 4),
    target_Cg,
    round(tolerance/2, 3),
    round(reference_distance_cm - tolerance/2, 3),
    round(reference_distance_cm + tolerance/2, 3),
    round(Cg, 2),
    Cg_status,
    round(Cgk, 2),
    Cgk_status,
    overall_status
  )
)

# Display summary
print(msa1_summary)

# Export summary
write.csv(msa1_summary, "MSA1_Summary.csv", row.names = FALSE)