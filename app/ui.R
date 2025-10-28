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


# --- Shiny App for Arthroplasty Registry ---
# Save this as app.R
# Run this AFTER generating the data with generate_arthroplasty_data.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(viridis)
library(sf)
library(leaflet)
library(RColorBrewer)

# --- Define File Path ---
DATA_FILE_NAME <- "arthroplasty_registry_data.csv"

# --- Check if Data File Exists ---
if (!file.exists(DATA_FILE_NAME)) {
  stop(paste("Data file not found:", DATA_FILE_NAME, 
             "\nPlease run generate_arthroplasty_data.R first to generate the data."))
}

# --- Load and Prepare Data ---
cat("Loading data from", DATA_FILE_NAME, "...\n")
raw_data <- read.csv(DATA_FILE_NAME, stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), ~na_if(., "")))

# Convert date columns
date_columns <- c("enrollment_date", "dob", "pat_op_date", "date_operation", "follow_up_date")
for (col in date_columns) {
  if (col %in% names(raw_data)) {
    raw_data[[col]] <- as.Date(raw_data[[col]])
  }
}

# Verify pat_asa exists
if (!"pat_asa" %in% names(raw_data)) {
  stop("CRITICAL ERROR: pat_asa variable missing from data file.\nPlease regenerate the data using generate_arthroplasty_data.R")
}

# Create enhanced patient base dataset
patients_base <- raw_data %>%
  filter(redcap_event_name == "recruitment_arm_1") %>%
  mutate(
    # Enhanced demographics
    gender_label = case_when(
      gender == 0 ~ "Male",
      gender == 1 ~ "Female", 
      gender == 9 ~ "Other",
      TRUE ~ "Unknown"
    ),
    
    # Clinical classifications
    joint_label = case_when(
      pat_joint_problem == 1 ~ "Hip",
      pat_joint_problem == 2 ~ "Knee", 
      pat_joint_problem == 3 ~ "Shoulder",
      pat_joint_problem == 99 ~ "Other",
      TRUE ~ "Unknown"
    ),
    
    procedure_stage = ifelse(op_type == 1, "Primary", "Revision"),
    
    # ASA Score labels
    asa_label = case_when(
      pat_asa == 1 ~ "ASA I",
      pat_asa == 2 ~ "ASA II", 
      pat_asa == 3 ~ "ASA III",
      pat_asa == 4 ~ "ASA IV",
      pat_asa == 5 ~ "ASA V",
      TRUE ~ "Not Recorded"
    ),
    
    # Comorbidity count
    comorbidity_count = rowSums(across(c(pat_comorbidities___1, pat_comorbidities___2, pat_comorbidities___3, 
                                         pat_comorbidities___4, pat_comorbidities___7), ~.x), na.rm = TRUE),
    
    # Medication count
    medication_count = rowSums(across(c(pat_meds___1, pat_meds___3, pat_meds___7, pat_meds___9, 
                                        pat_meds___15, pat_meds___26), ~.x), na.rm = TRUE),
    
    # Adverse events
    adverse_event_count = rowSums(across(c(pat_event__2, pat_event__4, pat_event__6), ~.x), na.rm = TRUE),
    has_adverse_event = adverse_event_count > 0,
    
    # Diagnosis labels
    diagnosis_label = case_when(
      pat_dlag___1 == 1 ~ "Osteoarthritis",
      pat_dlag___2 == 1 ~ "Avascular Necrosis",
      pat_dlag___4 == 1 ~ "Rheumatoid Arthritis",
      pat_dlag___9 == 1 ~ "Trauma",
      pat_dlag___5 == 1 ~ "Infection (Active)",
      pat_dlag___10 == 1 ~ "Malignancy", 
      pat_dlag___99 == 1 ~ "Other",
      TRUE ~ "Not Specified"
    ),
    
    # County of origin (simulated for Kenya map)
    county = sample(c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret", "Meru", "Thika", 
                      "Machakos", "Kakamega", "Kisii", "Garissa", "Lamu", "Malindi"), 
                    n(), replace = TRUE),
    
    # Implant manufacturers (simulated data)
    implant_manufacturer = sample(c("Zimmer Biomet", "Stryker", "Johnson & Johnson", "Smith & Nephew",
                                    "Medtronic", "B. Braun", "Arthrex", "CONMED"), 
                                  n(), replace = TRUE, prob = c(0.3, 0.25, 0.15, 0.1, 0.08, 0.06, 0.04, 0.02)),
    
    # Implant brands (simulated data)
    implant_brand = case_when(
      implant_manufacturer == "Zimmer Biomet" ~ sample(c("NexGen", "Persona", "Trabecular Metal"), n(), replace = TRUE),
      implant_manufacturer == "Stryker" ~ sample(c("Triathlon", "Accolade", "Scorpio"), n(), replace = TRUE),
      implant_manufacturer == "Johnson & Johnson" ~ sample(c("Attune", "PFC Sigma", "DePuy Synthes"), n(), replace = TRUE),
      TRUE ~ "Standard Implant"
    )
  )

# Kenya counties data for mapping
kenya_counties <- data.frame(
  county = c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret", "Meru", "Thika", 
             "Machakos", "Kakamega", "Kisii", "Garissa", "Lamu", "Malindi"),
  lat = c(-1.2864, -4.0435, -0.1022, -0.3031, 0.5143, 0.0515, -1.0333, 
          -1.5177, 0.2827, -0.6773, -0.4532, -2.2696, -3.2170),
  lng = c(36.8172, 39.6682, 34.7617, 36.0800, 35.2698, 37.6456, 37.0833, 
          37.2634, 34.7519, 34.7796, 39.6461, 40.9006, 40.1164)
)

cat("Data loaded successfully!\n")
cat("Total patients:", nrow(patients_base), "\n")

# --- UI Definition ---
ui <- dashboardPage(
  dashboardHeader(title = "Kenya Arthroplasty Registry"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Clinical Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Geographic Distribution", tabName = "geography", icon = icon("map")),
      menuItem("Implant Analytics", tabName = "implants", icon = icon("cog")),
      menuItem("Patient Explorer", tabName = "explorer", icon = icon("search"))
    ),
    
    hr(),
    h4("Data Filters"),
    
    selectInput("filter_joint", "Joint Type:", 
                choices = c("All", "Hip", "Knee", "Shoulder", "Other"),
                selected = "All"),
    
    selectInput("filter_procedure", "Procedure Type:",
                choices = c("All", "Primary", "Revision"),
                selected = "All"),
    
    selectInput("filter_diagnosis", "Diagnosis:",
                choices = c("All", unique(patients_base$diagnosis_label)),
                selected = "All"),
    
    selectInput("filter_asa", "ASA Score:",
                choices = c("All", unique(patients_base$asa_label)),
                selected = "All"),
    
    sliderInput("filter_age", "Age Range:",
                min = 15, max = 85, value = c(15, 85)),
    
    selectInput("filter_manufacturer", "Implant Manufacturer:",
                choices = c("All", unique(patients_base$implant_manufacturer)),
                selected = "All")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { margin-bottom: 10px; }
        .value-box { min-height: 100px; }
        .main-header .logo { font-weight: bold; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_patients_box", width = 3),
                valueBoxOutput("avg_age_box", width = 3),
                valueBoxOutput("primary_procedures_box", width = 3),
                valueBoxOutput("revision_procedures_box", width = 3)
              ),
              fluidRow(
                box(plotlyOutput("procedure_type_plot", height = 300), width = 6),
                box(plotlyOutput("asa_distribution_plot", height = 300), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("diagnosis_plot", height = 300), width = 6),
                box(plotlyOutput("gender_plot", height = 300), width = 6)
              )
      ),
      
      # Clinical Analytics Tab
      tabItem(tabName = "analytics",
              fluidRow(
                box(plotlyOutput("comorbidity_heatmap", height = 400), width = 8),
                box(plotlyOutput("age_diagnosis_plot", height = 400), width = 4)
              ),
              fluidRow(
                box(plotlyOutput("outcome_trends", height = 400), width = 12)
              )
      ),
      
      # Geographic Distribution Tab
      tabItem(tabName = "geography",
              fluidRow(
                box(leafletOutput("kenya_map", height = 500), width = 8),
                box(plotlyOutput("county_barplot", height = 500), width = 4)
              )
      ),
      
      # Implant Analytics Tab
      tabItem(tabName = "implants",
              fluidRow(
                valueBoxOutput("total_manufacturers_box", width = 3),
                valueBoxOutput("top_manufacturer_box", width = 3),
                valueBoxOutput("implant_variety_box", width = 3),
                valueBoxOutput("revision_rate_box", width = 3)
              ),
              fluidRow(
                box(plotlyOutput("manufacturer_market_share", height = 400), width = 6),
                box(plotlyOutput("implant_by_joint", height = 400), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("brand_usage", height = 400), width = 12)
              )
      ),
      
      # Patient Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(DTOutput("patient_table"), width = 12)
              )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- patients_base
    
    if (input$filter_joint != "All") {
      data <- data %>% filter(joint_label == input$filter_joint)
    }
    if (input$filter_procedure != "All") {
      data <- data %>% filter(procedure_stage == input$filter_procedure)
    }
    if (input$filter_diagnosis != "All") {
      data <- data %>% filter(diagnosis_label == input$filter_diagnosis)
    }
    if (input$filter_asa != "All") {
      data <- data %>% filter(asa_label == input$filter_asa)
    }
    if (input$filter_manufacturer != "All") {
      data <- data %>% filter(implant_manufacturer == input$filter_manufacturer)
    }
    
    # Age filter
    data <- data %>% filter(pt_age >= input$filter_age[1] & pt_age <= input$filter_age[2])
    
    data
  })
  
  # Overview Tab Outputs
  output$total_patients_box <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total Patients", icon = icon("users"), color = "blue")
  })
  
  output$avg_age_box <- renderValueBox({
    avg_age <- round(mean(filtered_data()$pt_age, na.rm = TRUE), 1)
    valueBox(avg_age, "Average Age", icon = icon("birthday-cake"), color = "green")
  })
  
  output$primary_procedures_box <- renderValueBox({
    count <- filtered_data() %>% filter(procedure_stage == "Primary") %>% nrow()
    valueBox(count, "Primary Procedures", icon = icon("procedures"), color = "purple")
  })
  
  output$revision_procedures_box <- renderValueBox({
    count <- filtered_data() %>% filter(procedure_stage == "Revision") %>% nrow()
    valueBox(count, "Revision Procedures", icon = icon("redo"), color = "orange")
  })
  
  output$procedure_type_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(joint_label, procedure_stage)
    
    p <- ggplot(data, aes(x = joint_label, y = n, fill = procedure_stage, 
                          text = paste("Joint:", joint_label, "<br>Procedure:", procedure_stage, "<br>Count:", n))) +
      geom_col(position = "dodge") +
      labs(x = "Joint Type", y = "Count", fill = "Procedure Type") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$asa_distribution_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(asa_label) %>%
      mutate(asa_label = factor(asa_label, levels = c("ASA I", "ASA II", "ASA III", "ASA IV", "ASA V", "Not Recorded")))
    
    p <- ggplot(data, aes(x = asa_label, y = n, fill = asa_label,
                          text = paste("ASA:", asa_label, "<br>Count:", n))) +
      geom_col() +
      labs(x = "ASA Score", y = "Count") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$diagnosis_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(diagnosis_label) %>%
      arrange(desc(n))
    
    p <- ggplot(data, aes(x = reorder(diagnosis_label, n), y = n, 
                          text = paste("Diagnosis:", diagnosis_label, "<br>Count:", n))) +
      geom_col(fill = "steelblue") +
      labs(x = "Diagnosis", y = "Count") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$gender_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(gender_label)
    
    p <- plot_ly(data, labels = ~gender_label, values = ~n, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial') %>%
      layout(title = "Gender Distribution")
    
    p
  })
  
  # Clinical Analytics Outputs
  output$comorbidity_heatmap <- renderPlotly({
    data <- filtered_data() %>%
      group_by(diagnosis_label, joint_label) %>%
      summarise(
        avg_comorbidities = mean(comorbidity_count, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(data, aes(x = diagnosis_label, y = joint_label, 
                          fill = avg_comorbidities, size = count)) +
      geom_point(shape = 22, color = "white") +
      scale_fill_viridis_c(name = "Avg Comorbidities") +
      scale_size_continuous(range = c(5, 15), name = "Patient Count") +
      labs(x = "Diagnosis", y = "Joint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$age_diagnosis_plot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = diagnosis_label, y = pt_age, fill = diagnosis_label)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      labs(x = "Diagnosis", y = "Age") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d()
    
    ggplotly(p)
  })
  
  output$outcome_trends <- renderPlotly({
    # Simulate monthly trends
    monthly_data <- filtered_data() %>%
      mutate(month = floor_date(date_operation, "month")) %>%
      group_by(month) %>%
      summarise(
        procedures = n(),
        avg_age = mean(pt_age, na.rm = TRUE),
        revision_rate = sum(procedure_stage == "Revision") / n() * 100,
        .groups = 'drop'
      )
    
    p <- plot_ly(monthly_data) %>%
      add_trace(x = ~month, y = ~procedures, type = 'scatter', mode = 'lines+markers',
                name = 'Procedures', line = list(color = 'blue')) %>%
      add_trace(x = ~month, y = ~revision_rate, type = 'scatter', mode = 'lines+markers',
                name = 'Revision Rate %', yaxis = 'y2', line = list(color = 'red')) %>%
      layout(
        title = "Monthly Procedure Trends",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Number of Procedures"),
        yaxis2 = list(title = "Revision Rate %", overlaying = "y", side = "right")
      )
    
    p
  })
  
  # Geographic Outputs
  output$kenya_map <- renderLeaflet({
    county_data <- filtered_data() %>%
      group_by(county) %>%
      summarise(patient_count = n(), .groups = 'drop') %>%
      right_join(kenya_counties, by = "county") %>%
      mutate(patient_count = ifelse(is.na(patient_count), 0, patient_count))
    
    pal <- colorNumeric("viridis", domain = county_data$patient_count)
    
    leaflet(county_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(patient_count) * 2,
        color = ~pal(patient_count),
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste("<b>", county, "</b><br>Patients: ", patient_count)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal, values = ~patient_count,
        title = "Patient Count"
      )
  })
  
  output$county_barplot <- renderPlotly({
    data <- filtered_data() %>%
      count(county) %>%
      arrange(desc(n)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(county, n), y = n, 
                          text = paste("County:", county, "<br>Patients:", n))) +
      geom_col(fill = "darkgreen") +
      labs(x = "County", y = "Number of Patients") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Implant Analytics Outputs
  output$total_manufacturers_box <- renderValueBox({
    count <- n_distinct(filtered_data()$implant_manufacturer)
    valueBox(count, "Manufacturers", icon = icon("industry"), color = "blue")
  })
  
  output$top_manufacturer_box <- renderValueBox({
    top_manuf <- filtered_data() %>%
      count(implant_manufacturer) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(implant_manufacturer)
    
    valueBox(top_manuf, "Top Manufacturer", icon = icon("crown"), color = "yellow")
  })
  
  output$implant_variety_box <- renderValueBox({
    count <- n_distinct(filtered_data()$implant_brand)
    valueBox(count, "Implant Brands", icon = icon("tags"), color = "green")
  })
  
  output$revision_rate_box <- renderValueBox({
    rate <- filtered_data() %>%
      summarise(rate = sum(procedure_stage == "Revision") / n() * 100) %>%
      pull(rate) %>%
      round(1)
    
    valueBox(paste0(rate, "%"), "Revision Rate", icon = icon("chart-line"), color = "red")
  })
  
  output$manufacturer_market_share <- renderPlotly({
    data <- filtered_data() %>%
      count(implant_manufacturer) %>%
      arrange(desc(n))
    
    p <- plot_ly(data, labels = ~implant_manufacturer, values = ~n, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial') %>%
      layout(title = "Manufacturer Market Share")
    
    p
  })
  
  output$implant_by_joint <- renderPlotly({
    data <- filtered_data() %>%
      count(implant_manufacturer, joint_label)
    
    p <- ggplot(data, aes(x = implant_manufacturer, y = n, fill = joint_label,
                          text = paste("Manufacturer:", implant_manufacturer, 
                                       "<br>Joint:", joint_label, 
                                       "<br>Count:", n))) +
      geom_col(position = "stack") +
      labs(x = "Manufacturer", y = "Count", fill = "Joint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$brand_usage <- renderPlotly({
    data <- filtered_data() %>%
      count(implant_manufacturer, implant_brand) %>%
      group_by(implant_manufacturer) %>%
      slice_max(n, n = 5) %>%
      ungroup()
    
    p <- ggplot(data, aes(x = reorder(implant_brand, n), y = n, fill = implant_manufacturer,
                          text = paste("Brand:", implant_brand, 
                                       "<br>Manufacturer:", implant_manufacturer,
                                       "<br>Usage:", n))) +
      geom_col() +
      labs(x = "Implant Brand", y = "Usage Count", fill = "Manufacturer") +
      coord_flip() +
      theme_minimal() +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Patient Explorer Output
  output$patient_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(
          registry_id, pt_first, pt_last, pt_age, gender_label, 
          joint_label, procedure_stage, diagnosis_label, asa_label,
          county, implant_manufacturer, implant_brand
        ),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      filter = 'top'
    )
  })
}

# --- Run App ---
shinyApp(ui, server)