# --- 1. Load Required Libraries ---
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(here)
library(wakefield)
library(janitor)

# --- 2. Define File Paths ---
DATA_FILE_NAME <- here::here("arthroplasty_registry_data.csv")

# --- 3. Generate Comprehensive Mock Data ---
generate_mock_data <- function(n_patients = 600) {
  set.seed(42)
  
  # Generate base patient demographics
  base_patients <- tibble(
    registry_id = 1:n_patients,
    redcap_event_name = "recruitment_arm_1",
    
    # Personal Information
    pt_first = wakefield::name(n = n_patients),
    pt_last = wakefield::name(n = n_patients),
    gender = sample(c(0, 1, 9), n_patients, replace = TRUE, prob = c(0.45, 0.53, 0.02)),
    
    # Dates
    enrollment_date = sample(seq(as.Date('2020-01-01'), as.Date('2024-12-31'), by = "day"), n_patients, replace = TRUE),
    dob = enrollment_date - years(sample(45:85, n_patients, replace = TRUE)) - days(sample(0:364, n_patients, replace = TRUE)),
    
    # Demographics
    pat_county = sample(1:47, n_patients, replace = TRUE, prob = c(rep(0.01, 46), 0.54)),
    pat_employment = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
    pat_education = sample(c(1, 2, 3, 4, 5, 6, 8), n_patients, replace = TRUE),
    pat_income = sample(c(1, 2, 3, 4, 5, 6), n_patients, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.08, 0.02)),
    
    # Clinical Information
    pat_joint_problem = sample(c(1, 2, 3, 99), n_patients, replace = TRUE, prob = c(0.5, 0.45, 0.03, 0.02)),
    pat_duration_arthropathy = sample(1:8, n_patients, replace = TRUE),
    
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
    
    # Surgical details
    op_type = sample(c(1, 2), n_patients, replace = TRUE, prob = c(0.85, 0.15)), # Primary vs Revision
    op_type1 = sample(c(1, 2), n_patients, replace = TRUE, prob = c(0.85, 0.15)),
    
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
      pat_dlag___9 = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.85, 0.15)) # Trauma
    )
  
  # Generate follow-up data with proper date handling
  generate_followup_data <- function(base_data, event_name, weeks_after, completion_rate) {
    follow_up_patients <- base_data %>%
      sample_frac(completion_rate) %>%
      mutate(
        redcap_event_name = event_name,
        # Use days instead of months for more precise control
        follow_up_date = pat_op_date + days(weeks_after * 7) + days(sample(-14:14, n(), replace = TRUE)),
        # Improved scores at follow-up
        pat_pain_scale = pmax(0, pat_pain_scale - sample(2:5, n(), replace = TRUE)),
        pat_bmi = pat_bmi + rnorm(n(), 0, 0.5)
      )
    
    # Add specific follow-up date fields based on event type
    if (event_name == "follow_up_6wk_arm_1") {
      follow_up_patients <- follow_up_patients %>%
        mutate(
          kss_date_preoppost = as.character(follow_up_date),
          hhs_date_preop_post = as.character(follow_up_date)
        )
    } else if (event_name == "follow_up_1yr_arm_1") {
      follow_up_patients <- follow_up_patients %>%
        mutate(
          kss_date_preoppost = as.character(follow_up_date),
          hhs_date_preop_post = as.character(follow_up_date)
        )
    }
    
    return(follow_up_patients)
  }
  
  # Generate follow-ups using weeks instead of months
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

# Generate data if it doesn't exist
if (!file.exists(DATA_FILE_NAME)) {
  cat("Generating comprehensive arthroplasty registry data...\n")
  mock_data <- generate_mock_data(600)
  write.csv(mock_data, DATA_FILE_NAME, row.names = FALSE)
  cat("Data generation complete!\n")
}

# --- 4. Load and Prepare Data ---
raw_data <- read.csv(DATA_FILE_NAME, stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(matches("date|Date|op_date"), as.Date))

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
    
    pat_county_label = case_when(
      pat_county == 1 ~ "Mombasa", pat_county == 2 ~ "Kwale", pat_county == 3 ~ "Kilifi",
      pat_county == 4 ~ "Tana River", pat_county == 5 ~ "Lamu", pat_county == 6 ~ "Taita/Taveta",
      pat_county == 7 ~ "Garissa", pat_county == 8 ~ "Wajir", pat_county == 9 ~ "Mandera",
      pat_county == 10 ~ "Marsabit", pat_county == 11 ~ "Isiolo", pat_county == 12 ~ "Meru",
      pat_county == 13 ~ "Tharaka-Nithi", pat_county == 14 ~ "Embu", pat_county == 15 ~ "Kitui",
      pat_county == 16 ~ "Machakos", pat_county == 17 ~ "Makueni", pat_county == 18 ~ "Nyandarua",
      pat_county == 19 ~ "Nyeri", pat_county == 20 ~ "Kirinyaga", pat_county == 21 ~ "Murang'a",
      pat_county == 22 ~ "Kiambu", pat_county == 23 ~ "Turkana", pat_county == 24 ~ "West Pokot",
      pat_county == 25 ~ "Samburu", pat_county == 26 ~ "Trans Nzoia", pat_county == 27 ~ "Uasin Gishu",
      pat_county == 28 ~ "Elgeyo/Marakwet", pat_county == 29 ~ "Nandi", pat_county == 30 ~ "Baringo",
      pat_county == 31 ~ "Laikipia", pat_county == 32 ~ "Nakuru", pat_county == 33 ~ "Narok",
      pat_county == 34 ~ "Kajiado", pat_county == 35 ~ "Kericho", pat_county == 36 ~ "Bomet",
      pat_county == 37 ~ "Kakamega", pat_county == 38 ~ "Vihiga", pat_county == 39 ~ "Bungoma",
      pat_county == 40 ~ "Busia", pat_county == 41 ~ "Siaya", pat_county == 42 ~ "Kisumu",
      pat_county == 43 ~ "Homa Bay", pat_county == 44 ~ "Migori", pat_county == 45 ~ "Kisii",
      pat_county == 46 ~ "Nyamira", pat_county == 47 ~ "Nairobi City",
      TRUE ~ "Unknown"
    ),
    
    education_label = case_when(
      pat_education == 1 ~ "Primary School",
      pat_education == 2 ~ "High School", 
      pat_education == 3 ~ "Vocational School",
      pat_education == 8 ~ "Some College",
      pat_education == 4 ~ "Bachelor's degree",
      pat_education == 5 ~ "Master's degree", 
      pat_education == 6 ~ "Doctorate",
      TRUE ~ "Not specified"
    ),
    
    income_label = case_when(
      pat_income == 1 ~ "< KSh 20,000",
      pat_income == 2 ~ "KSh 20,001-40,000",
      pat_income == 3 ~ "KSh 40,001-60,000", 
      pat_income == 4 ~ "KSh 60,001-80,000",
      pat_income == 5 ~ "KSh 80,001-100,000",
      pat_income == 6 ~ "> KSh 100,000",
      TRUE ~ "Not specified"
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
    
    # Comorbidity count
    comorbidity_count = pat_comorbidities___1 + pat_comorbidities___2 + pat_comorbidities___3 + 
      pat_comorbidities___4 + pat_comorbidities___7,
    
    comorbidity_category = case_when(
      comorbidity_count == 0 ~ "None",
      comorbidity_count == 1 ~ "1 Condition",
      comorbidity_count == 2 ~ "2 Conditions", 
      comorbidity_count >= 3 ~ "3+ Conditions"
    ),
    
    # BMI categories
    bmi_category = case_when(
      pat_bmi < 18.5 ~ "Underweight",
      pat_bmi >= 18.5 & pat_bmi < 25 ~ "Normal",
      pat_bmi >= 25 & pat_bmi < 30 ~ "Overweight", 
      pat_bmi >= 30 ~ "Obese",
      TRUE ~ "Unknown"
    ),
    
    # Pain severity
    pain_severity = case_when(
      pat_pain_scale <= 3 ~ "Mild",
      pat_pain_scale > 3 & pat_pain_scale <= 7 ~ "Moderate",
      pat_pain_scale > 7 ~ "Severe",
      TRUE ~ "Unknown"
    ),
    
    # Operation date - handle both hip and knee operation date fields
    operation_date = coalesce(as.Date(pat_op_date), as.Date(date_operation))
  )

# --- 5. Define UI ---
ui <- dashboardPage(
  dashboardHeader(
    title = "Arthroplasty Registry Dashboard",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Patient Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("Clinical Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Follow-up Tracker", tabName = "tracker", icon = icon("calendar-check"))
    ),
    
    hr(),
    h4("Data Filters", style = "padding-left: 15px;"),
    
    selectInput("filter_joint", "Joint Type:", 
                choices = c("All", "Hip", "Knee", "Shoulder", "Other"),
                selected = "All"),
    
    selectInput("filter_procedure", "Procedure Type:",
                choices = c("All", "Primary", "Revision"),
                selected = "All"),
    
    selectInput("filter_gender", "Gender:",
                choices = c("All", "Male", "Female", "Other"),
                selected = "All"),
    
    sliderInput("filter_age", "Age Range:",
                min = floor(min(patients_base$pt_age, na.rm = TRUE)),
                max = ceiling(max(patients_base$pt_age, na.rm = TRUE)),
                value = c(floor(min(patients_base$pt_age, na.rm = TRUE)), 
                          ceiling(max(patients_base$pt_age, na.rm = TRUE)))),
    
    selectInput("filter_county", "County:",
                choices = c("All", sort(unique(patients_base$pat_county_label))),
                selected = "All"),
    
    actionButton("reset_filters", "Reset All Filters", icon = icon("refresh"))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .box { border-top: 3px solid #3c8dbc; }
        .info-box { min-height: 100px; }
        .small-box { height: 120px; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_patients_box", width = 3),
                valueBoxOutput("avg_age_box", width = 3),
                valueBoxOutput("avg_bmi_box", width = 3),
                valueBoxOutput("avg_pain_box", width = 3)
              ),
              
              fluidRow(
                valueBoxOutput("hip_procedures_box", width = 3),
                valueBoxOutput("knee_procedures_box", width = 3),
                valueBoxOutput("primary_procedures_box", width = 3),
                valueBoxOutput("revision_procedures_box", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Procedure Distribution by Joint", status = "primary", solidHeader = TRUE,
                  plotlyOutput("procedure_joint_plot", height = 300), width = 6
                ),
                box(
                  title = "Patient Demographics", status = "primary", solidHeader = TRUE,
                  plotlyOutput("demographics_plot", height = 300), width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Comorbidity Distribution", status = "primary", solidHeader = TRUE,
                  plotlyOutput("comorbidity_plot", height = 300), width = 6
                ),
                box(
                  title = "Pain Severity Distribution", status = "primary", solidHeader = TRUE,
                  plotlyOutput("pain_plot", height = 300), width = 6
                )
              )
      ),
      
      # Patient Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Patient Data Table", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("patient_table"),
                  downloadButton("download_data", "Download Filtered Data")
                )
              )
      ),
      
      # Clinical Analytics Tab
      tabItem(tabName = "analytics",
              fluidRow(
                box(
                  title = "Age Distribution by Joint Type", status = "primary", solidHeader = TRUE,
                  plotlyOutput("age_joint_plot", height = 400), width = 6
                ),
                box(
                  title = "BMI vs Pain Score", status = "primary", solidHeader = TRUE,
                  plotlyOutput("bmi_pain_plot", height = 400), width = 6
                )
              )
      ),
      
      # Follow-up Tracker Tab
      tabItem(tabName = "tracker",
              fluidRow(
                valueBoxOutput("due_6wk_box", width = 4),
                valueBoxOutput("due_1yr_box", width = 4),
                valueBoxOutput("completion_rate_box", width = 4)
              ),
              
              fluidRow(
                box(
                  title = "Follow-up Status", status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("followup_table")
                )
              )
      )
    )
  )
)

# --- 6. Define Server Logic ---
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- patients_base
    
    # Apply filters
    if (input$filter_joint != "All") {
      data <- data %>% filter(joint_label == input$filter_joint)
    }
    
    if (input$filter_procedure != "All") {
      data <- data %>% filter(procedure_stage == input$filter_procedure)
    }
    
    if (input$filter_gender != "All") {
      data <- data %>% filter(gender_label == input$filter_gender)
    }
    
    if (input$filter_county != "All") {
      data <- data %>% filter(pat_county_label == input$filter_county)
    }
    
    data <- data %>%
      filter(
        pt_age >= input$filter_age[1] & pt_age <= input$filter_age[2]
      )
    
    return(data)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_joint", selected = "All")
    updateSelectInput(session, "filter_procedure", selected = "All")
    updateSelectInput(session, "filter_gender", selected = "All")
    updateSelectInput(session, "filter_county", selected = "All")
    updateSliderInput(session, "filter_age", 
                      value = c(floor(min(patients_base$pt_age, na.rm = TRUE)), 
                                ceiling(max(patients_base$pt_age, na.rm = TRUE))))
  })
  
  # Value boxes for overview
  output$total_patients_box <- renderValueBox({
    valueBox(
      nrow(filtered_data()), "Total Patients", icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_age_box <- renderValueBox({
    avg_age <- round(mean(filtered_data()$pt_age, na.rm = TRUE), 1)
    valueBox(
      avg_age, "Average Age", icon = icon("birthday-cake"),
      color = "green"
    )
  })
  
  output$avg_bmi_box <- renderValueBox({
    avg_bmi <- round(mean(filtered_data()$pat_bmi, na.rm = TRUE), 1)
    valueBox(
      avg_bmi, "Average BMI", icon = icon("weight"),
      color = "yellow"
    )
  })
  
  output$avg_pain_box <- renderValueBox({
    avg_pain <- round(mean(filtered_data()$pat_pain_scale, na.rm = TRUE), 1)
    valueBox(
      avg_pain, "Average Pain Score", icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$hip_procedures_box <- renderValueBox({
    count <- filtered_data() %>% filter(joint_label == "Hip") %>% nrow()
    valueBox(count, "Hip Procedures", icon = icon("procedures"), color = "purple")
  })
  
  output$knee_procedures_box <- renderValueBox({
    count <- filtered_data() %>% filter(joint_label == "Knee") %>% nrow()
    valueBox(count, "Knee Procedures", icon = icon("procedures"), color = "orange")
  })
  
  output$primary_procedures_box <- renderValueBox({
    count <- filtered_data() %>% filter(procedure_stage == "Primary") %>% nrow()
    valueBox(count, "Primary Procedures", icon = icon("star"), color = "green")
  })
  
  output$revision_procedures_box <- renderValueBox({
    count <- filtered_data() %>% filter(procedure_stage == "Revision") %>% nrow()
    valueBox(count, "Revision Procedures", icon = icon("redo"), color = "red")
  })
  
  # Plots for overview tab
  output$procedure_joint_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(joint_label, procedure_stage)
    
    p <- ggplot(data, aes(x = joint_label, y = n, fill = procedure_stage)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Joint Type", y = "Number of Procedures", fill = "Procedure Type") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
  
  output$demographics_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(gender_label, pat_county_label) %>%
      group_by(pat_county_label) %>%
      top_n(10, n)  # Limit to top 10 counties for readability
    
    p <- ggplot(data, aes(x = pat_county_label, y = n, fill = gender_label)) +
      geom_bar(stat = "identity") +
      labs(x = "County", y = "Number of Patients", fill = "Gender") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  output$comorbidity_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(comorbidity_category)
    
    p <- ggplot(data, aes(x = comorbidity_category, y = n, fill = comorbidity_category)) +
      geom_bar(stat = "identity") +
      labs(x = "Number of Comorbidities", y = "Number of Patients") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p)
  })
  
  output$pain_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(pain_severity)
    
    p <- ggplot(data, aes(x = pain_severity, y = n, fill = pain_severity)) +
      geom_bar(stat = "identity") +
      labs(x = "Pain Severity", y = "Number of Patients") +
      theme_minimal() +
      scale_fill_manual(values = c("Mild" = "green", "Moderate" = "orange", "Severe" = "red"))
    
    ggplotly(p)
  })
  
  # Patient explorer table
  output$patient_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(
          registry_id, pt_first, pt_last, pt_age, gender_label, 
          pat_county_label, joint_label, procedure_stage, 
          pat_bmi, pat_pain_scale, comorbidity_count, operation_date
        ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip'
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("arthroplasty_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Clinical analytics plots
  output$age_joint_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = joint_label, y = pt_age, fill = joint_label)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.2, alpha = 0.5) +
      labs(x = "Joint Type", y = "Age") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
  
  output$bmi_pain_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = pat_bmi, y = pat_pain_scale, color = joint_label)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "BMI", y = "Pain Score", color = "Joint Type") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # Follow-up tracker
  followup_data <- reactive({
    today <- Sys.Date()
    
    patients_base %>%
      mutate(
        due_6wk = operation_date + weeks(6),
        due_1yr = operation_date + weeks(52),
        completed_6wk = registry_id %in% (raw_data %>% filter(redcap_event_name == "follow_up_6wk_arm_1") %>% pull(registry_id)),
        completed_1yr = registry_id %in% (raw_data %>% filter(redcap_event_name == "follow_up_1yr_arm_1") %>% pull(registry_id))
      ) %>%
      mutate(
        status_6wk = case_when(
          completed_6wk ~ "Completed",
          due_6wk > today ~ "Pending",
          due_6wk <= today ~ "Overdue",
          TRUE ~ "Unknown"
        ),
        status_1yr = case_when(
          completed_1yr ~ "Completed", 
          due_1yr > today ~ "Pending",
          due_1yr <= today ~ "Overdue",
          TRUE ~ "Unknown"
        )
      )
  })
  
  output$due_6wk_box <- renderValueBox({
    overdue <- followup_data() %>% filter(status_6wk == "Overdue") %>% nrow()
    valueBox(overdue, "6-Week Follow-ups Overdue", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$due_1yr_box <- renderValueBox({
    overdue <- followup_data() %>% filter(status_1yr == "Overdue") %>% nrow()
    valueBox(overdue, "1-Year Follow-ups Overdue", icon = icon("exclamation-triangle"), color = "orange")
  })
  
  output$completion_rate_box <- renderValueBox({
    total_eligible <- followup_data() %>% 
      filter(operation_date <= Sys.Date() - weeks(6)) %>% 
      nrow()
    completed <- followup_data() %>% 
      filter(completed_6wk) %>% 
      nrow()
    rate <- ifelse(total_eligible > 0, round(completed/total_eligible * 100, 1), 0)
    valueBox(paste0(rate, "%"), "6-Week Completion Rate", icon = icon("percent"), color = "green")
  })
  
  output$followup_table <- renderDT({
    datatable(
      followup_data() %>%
        select(
          registry_id, pt_first, pt_last, joint_label, operation_date,
          due_6wk, status_6wk, due_1yr, status_1yr
        ),
      options = list(pageLength = 15, scrollX = TRUE),
      filter = 'top',
      rownames = FALSE
    )
  })
}

# --- 7. Run the Application ---
shinyApp(ui = ui, server = server)