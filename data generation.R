# --- Data Generation Script for Arthroplasty Registry ---
# Save this as generate_arthroplasty_data.R
# Run this script first to generate the data file

library(tidyverse)
library(lubridate)
library(wakefield)

# --- Generate Comprehensive Mock Data ---
generate_mock_data <- function(n_patients = 600) {
  set.seed(42)
  
  cat("Generating arthroplasty registry data for", n_patients, "patients...\n")
  
  # Generate base patient demographics
  base_patients <- tibble(
    registry_id = 1:n_patients,
    redcap_event_name = "recruitment_arm_1",
    
    # Personal Information
    pt_first = wakefield::name(n = n_patients),
    pt_last = wakefield::name(n = n_patients),
    gender = sample(c(0, 1, 9), n_patients, replace = TRUE, prob = c(0.45, 0.53, 0.02)),
    
    # Dates - Lower age range starting from 15
    enrollment_date = sample(seq(as.Date('2020-01-01'), as.Date('2024-12-31'), by = "day"), n_patients, replace = TRUE),
    dob = enrollment_date - years(sample(15:85, n_patients, replace = TRUE)) - days(sample(0:364, n_patients, replace = TRUE)),
    
    # Demographics
    pat_county = sample(1:47, n_patients, replace = TRUE, prob = c(rep(0.01, 46), 0.54)),
    pat_employment = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
    pat_education = sample(c(1, 2, 3, 4, 5, 6, 8), n_patients, replace = TRUE),
    pat_income = sample(c(1, 2, 3, 4, 5, 6), n_patients, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.08, 0.02)),
    
    # Clinical Information
    pat_joint_problem = sample(c(1, 2, 3, 99), n_patients, replace = TRUE, prob = c(0.5, 0.45, 0.03, 0.02)),
    pat_duration_arthropathy = sample(1:8, n_patients, replace = TRUE),
    
    # ASA Scores - CRITICAL: This must be included
    pat_asa = sample(1:5, n_patients, replace = TRUE, prob = c(0.1, 0.4, 0.3, 0.15, 0.05)),
    
    # Laterality based on joint
    pat_laterality_hip = ifelse(pat_joint_problem == 1, sample(1:5, n_patients, replace = TRUE), NA),
    pat_laterality_knee = ifelse(pat_joint_problem == 2, sample(1:5, n_patients, replace = TRUE), NA),
    pat_laterality_shoulder = ifelse(pat_joint_problem == 3, sample(1:5, n_patients, replace = TRUE), NA),
    pat_laterality_other = ifelse(pat_joint_problem == 99, sample(1:5, n_patients, replace = TRUE), NA),
    
    # Comorbidities (multiple selection simulation)
    pat_comorbidities___1 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.6, 0.4)), # Hypertension
    pat_comorbidities___2 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.8, 0.2)), # Diabetes
    pat_comorbidities___3 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.9, 0.1)), # CKD
    pat_comorbidities___4 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.95, 0.05)), # HIV
    pat_comorbidities___7 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.85, 0.15)), # Asthma
    
    # Medications (multiple selection)
    pat_meds___1 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.7, 0.3)), # Diclofenac
    pat_meds___3 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.6, 0.4)), # Ibuprofen
    pat_meds___7 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.5, 0.5)), # Paracetamol
    pat_meds___9 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.8, 0.2)), # Aspirin
    pat_meds___15 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.85, 0.15)), # Tramadol
    pat_meds___26 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.7, 0.3)), # Metformin
    
    # Surgical details
    op_type = sample(c(1, 2), n_patients, replace = TRUE, prob = c(0.85, 0.15)), # Primary vs Revision
    op_type1 = sample(c(1, 2), n_patients, replace = TRUE, prob = c(0.85, 0.15)),
    
    # Adverse events
    pat_event__1 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.9, 0.1)), # None
    pat_event__2 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.95, 0.05)), # Calcar crack
    pat_event__4 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.97, 0.03)), # Shaft fracture
    pat_event__6 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.98, 0.02)), # Trochanteric fracture
    
    # Scores and measurements
    pat_height = sample(150:190, n_patients, replace = TRUE),
    pat_weight = sample(50:120, n_patients, replace = TRUE),
    pat_pain_scale = sample(0:10, n_patients, replace = TRUE),
    pat_sbp = sample(110:180, n_patients, replace = TRUE),
    pat_dbp = sample(70:110, n_patients, replace = TRUE)
  ) %>%
    mutate(
      # Calculate BMI
      pat_bmi = round(pat_weight / ((pat_height/100)^2), 1),
      
      # Calculate age
      pt_age = as.numeric(difftime(enrollment_date, dob, units = "days") / 365.25),
      
      # Generate operation dates (1-3 months after enrollment)
      operation_days = sample(30:90, n_patients, replace = TRUE),
      pat_op_date = enrollment_date + days(operation_days),
      date_operation = pat_op_date,
      
      # Payment methods
      pat_mod_pay___1 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.7, 0.3)), # Out of pocket
      pat_mod_pay___2 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.5, 0.5)), # NHIF Active
      
      # Diagnosis based on joint problem
      pat_dlag___1 = ifelse(pat_joint_problem %in% c(1, 2), sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.3, 0.7)), 0), # OA
      pat_dlag___2 = ifelse(pat_joint_problem == 1, sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.8, 0.2)), 0), # AVN
      pat_dlag___4 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.9, 0.1)), # RA
      pat_dlag___9 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.85, 0.15)), # Trauma
      pat_dlag___5 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.95, 0.05)), # Infection (Active)
      pat_dlag___10 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.98, 0.02)), # Malignancy
      pat_dlag___99 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.9, 0.1)) # Other
    )
  
  # Generate follow-up data
  generate_followup_data <- function(base_data, event_name, weeks_after, completion_rate) {
    follow_up_patients <- base_data %>%
      sample_frac(completion_rate) %>%
      mutate(
        redcap_event_name = event_name,
        follow_up_date = pat_op_date + days(weeks_after * 7) + days(sample(-14:14, n(), replace = TRUE)),
        # Improved scores at follow-up
        pat_pain_scale = pmax(0, pat_pain_scale - sample(2:5, n(), replace = TRUE)),
        pat_bmi = pat_bmi + rnorm(n(), 0, 0.5)
      )
    
    return(follow_up_patients)
  }
  
  # Generate follow-ups
  follow_up_6wk <- generate_followup_data(base_patients, "follow_up_6wk_arm_1", 6, 0.85)
  follow_up_1yr <- generate_followup_data(base_patients, "follow_up_1yr_arm_1", 52, 0.75)
  
  # Combine all data
  final_data <- bind_rows(
    base_patients,
    follow_up_6wk,
    follow_up_1yr
  ) %>%
    arrange(registry_id, enrollment_date)
  
  return(final_data)
}

# --- Generate and Save Data ---
DATA_FILE_NAME <- "arthroplasty_registry_data.csv"

cat("Starting data generation...\n")
mock_data <- generate_mock_data(600)

cat("Saving data to", DATA_FILE_NAME, "...\n")
write.csv(mock_data, DATA_FILE_NAME, row.names = FALSE)

# Verify the data was created correctly
cat("Data generation complete!\n")
cat("File saved as:", DATA_FILE_NAME, "\n")
cat("Number of records:", nrow(mock_data), "\n")
cat("Variables included:", paste(names(mock_data), collapse = ", "), "\n")

# Check if pat_asa exists
if ("pat_asa" %in% names(mock_data)) {
  cat("✓ pat_asa variable successfully created\n")
  cat("ASA score distribution:\n")
  print(table(mock_data$pat_asa))
} else {
  cat("✗ ERROR: pat_asa variable missing from generated data\n")
}

cat("\nData generation script completed successfully!\n")
cat("You can now run the Shiny app.\n")