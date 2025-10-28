# --- 1. Load All Required Libraries ---
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(here)
library(wakefield) # For name generation

# --- 2. Define File Paths ---
DATA_FILE_NAME <- here::here("generated_mock_data.csv")
# This template file MUST be in the same folder as your app.R
TEMPLATE_FILE_NAME <- here::here("ArthroplastyRegistryBackup_ImportTemplate_2025-10-28.csv")

# --- 3. Check if Data is Valid. If Not, Create It ---
NEEDS_REGEN <- TRUE # Assume we need to regenerate
if (file.exists(DATA_FILE_NAME)) {
  suppressMessages({
    df_check <- read.csv(DATA_FILE_NAME, nrows = 1)
    # Check if the file has a key column from the *new* dictionary
    if ("pat_joint_problem" %in% colnames(df_check)) {
      # Also check if it has the correct number of patients
      if(nrow(read.csv(DATA_FILE_NAME)) > 800) { # 600 pts + followups
        NEEDS_REGEN <- FALSE # File is up-to-date
      }
    }
  })
}

if (!file.exists(TEMPLATE_FILE_NAME)) {
  stop(paste("CRITICAL ERROR: The template file '", TEMPLATE_FILE_NAME, "' was not found. Please save it in the same folder as this app.R script."))
}

if (NEEDS_REGEN) {
  
  cat("*************************************************************\n")
  cat("REGENERATING DATA: 'generated_mock_data.csv' with 600 patients.\n")
  cat("Please wait...\n")
  cat("*************************************************************\n")
  
  # --- Start of Data Generation Code ---
  n_patients <- 600 # Set to 600
  set.seed(42)
  
  # Define date constants and randomizers OUTSIDE of the tibble()
  start_date_const <- as.Date("2020-01-01")
  end_date_const <- as.Date("2024-12-31")
  days_diff_const <- as.numeric(end_date_const - start_date_const)
  
  # --- 3a. Generate Base Patient Data ---
  base_patients <- tibble(
    registry_id = 1:n_patients,
    redcap_event_name = "recruitment_arm_1",
    
    # Demographics (NOTE: ALL LINES NOW HAVE COMMAS)
    full_name = wakefield::name(n = n_patients), 
    gender = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.45, 0.55)), 
    
    # Dates & Age
    random_days = sample(0:days_diff_const, n_patients, replace = TRUE),
    demog_coldt = start_date_const + random_days, 
    
    pt_age_generated = sample(45:85, n_patients, replace = TRUE),
    dob = as.character(demog_coldt - years(pt_age_generated) - days(sample(0:364, n_patients, TRUE))),
    date_enrolled = as.character(demog_coldt + days(sample(1:5, n_patients, TRUE))),
    
    # Location
    pat_county = sample(1:47, n_patients, replace = TRUE, prob = c(rep(0.02, 46), 0.08)),
    
    # Clinical Data (Helper Columns)
    helper_joint = sample(c(1, 2), n_patients, replace = TRUE, prob = c(0.6, 0.4)),
    helper_is_revision = sample(c(FALSE, TRUE), n_patients, replace = TRUE, prob = c(0.85, 0.15)),
    helper_op_date = demog_coldt + days(sample(14:60, n_patients, replace = TRUE)),
    helper_diagnosis = sample(c("OA", "AVN", "RA", "SCD", "TRAUMA"), n_patients, TRUE, prob = c(0.7, 0.1, 0.1, 0.05, 0.05)) # LAST ARGUMENT, NO COMMA
    
  ) %>%
    separate(full_name, into = c("pt_first", "pt_last"), sep = " ", extra = "merge") %>%
    
    # --- 3c. Map Helper Columns to REAL REDCap Variables ---
    mutate(
      pat_joint_problem = helper_joint,
      pat_laterality_hip = ifelse(helper_joint == 1, sample(1:5, n(), TRUE), NA),
      pat_laterality_knee = ifelse(helper_joint == 2, sample(1:5, n(), TRUE), NA),
      pat_op_date = ifelse(helper_joint == 1, as.character(helper_op_date), NA),
      date_operation = ifelse(helper_joint == 2, as.character(helper_op_date), NA),
      op_type = ifelse(helper_joint == 2, ifelse(helper_is_revision, 2, 1), NA),
      op_type1 = ifelse(helper_joint == 1, ifelse(helper_is_revision, 2, 1), NA),
      pat_diag___1 = ifelse(helper_diagnosis == "OA", 1, 0),
      pat_diag___2 = ifelse(helper_diagnosis == "AVN", 1, 0),
      pat_diag___3 = ifelse(helper_diagnosis == "SCD", 1, 0),
      pat_diag___4 = ifelse(helper_diagnosis == "RA", 1, 0),
      pat_diag___9 = ifelse(helper_diagnosis == "TRAUMA", 1, 0),
      pat_indic___1 = ifelse(helper_joint == 1 & !helper_is_revision & helper_diagnosis == "OA", 1, 0),
      pat_indic___4 = ifelse(helper_joint == 1 & !helper_is_revision & helper_diagnosis == "AVN", 1, 0),
      pat_indic___5 = ifelse(helper_joint == 1 & !helper_is_revision & helper_diagnosis == "TRAUMA", 1, 0),
      pat_indic2___16 = ifelse(helper_joint == 1 & helper_is_revision, sample(c(1,0), n(), TRUE, prob=c(0.3, 0.7)), 0),
      pat_indic2___17 = ifelse(helper_joint == 1 & helper_is_revision & pat_indic2___16 == 0, 1, 0),
      pat_indic3___1 = ifelse(helper_joint == 2 & !helper_is_revision & helper_diagnosis == "OA", 1, 0),
      pat_indic3___2 = ifelse(helper_joint == 2 & !helper_is_revision & helper_diagnosis == "AVN", 1, 0),
      pat_indic3___8 = ifelse(helper_joint == 2 & !helper_is_revision & helper_diagnosis == "TRAUMA", 1, 0),
      hhs_pain = ifelse(helper_joint == 1, sample(c(10, 20, 30, 40, 44), n(), TRUE, prob=c(0.4, 0.3, 0.2, 0.1, 0)), NA),
      kss_sympt_1 = ifelse(helper_joint == 2, sample(0:5, n(), TRUE), NA) # LAST ARGUMENT, NO COMMA
    )
  
  # --- 3d. Generate Follow-Up Events ---
  
  follow_up_6wk <- base_patients %>%
    filter(!helper_is_revision) %>% 
    sample_frac(0.90) %>% 
    mutate(
      redcap_event_name = "follow_up_6wk_arm_1",
      follow_up_date = helper_op_date + weeks(6) + days(sample(-7:7, n(), TRUE)),
      hhs_date_preop_post = ifelse(helper_joint == 1, as.character(follow_up_date), NA), 
      kss_date_preoppost = ifelse(helper_joint == 2, as.character(follow_up_date), NA),  
      hhs_pain_post = ifelse(helper_joint == 1, hhs_pain + sample(c(10, 20), n(), TRUE), NA), 
      kss_sympt_1post = ifelse(helper_joint == 2, kss_sympt_1 + sample(3:5, n(), TRUE), NA)  
    ) %>%
    select(registry_id, redcap_event_name, hhs_date_preop_post, kss_date_preoppost, hhs_pain_post, kss_sympt_1post)
  
  follow_up_1yr <- base_patients %>%
    filter(!helper_is_revision) %>%
    sample_frac(0.80) %>%
    mutate(
      redcap_event_name = "follow_up_1yr_arm_1",
      follow_up_date = helper_op_date + weeks(52) + days(sample(-14:14, n(), TRUE)),
      hhs_date_preop_post = ifelse(helper_joint == 1, as.character(follow_up_date), NA),
      kss_date_preoppost = ifelse(helper_joint == 2, as.character(follow_up_date), NA),
      hhs_pain_post = ifelse(helper_joint == 1, hhs_pain + sample(c(20, 30), n(), TRUE), NA),
      kss_sympt_1post = ifelse(helper_joint == 2, kss_sympt_1 + sample(5:7, n(), TRUE), NA)
    ) %>%
    select(registry_id, redcap_event_name, hhs_date_preop_post, kss_date_preoppost, hhs_pain_post, kss_sympt_1post)
  
  # --- 3e. Combine and Save ---
  
  final_mock_data <- bind_rows(base_patients, follow_up_6wk, follow_up_1yr) %>%
    arrange(registry_id, demog_coldt)
  
  all_columns_template <- read.csv(TEMPLATE_FILE_NAME, nrows = 0) %>%
    select(-contains("Unnamed"))
  
  final_output <- bind_rows(all_columns_template, final_mock_data)
  
  write.csv(final_output, DATA_FILE_NAME, row.names = FALSE, na = "")
  
  cat("...Data generation complete. Loading app...\n")
}

# --- 4. Load Data (This will now always work) ---
raw_data <- read.csv(DATA_FILE_NAME, na.strings = c("", "NA")) 

# --- 5. Perform Data Cleaning & Feature Engineering ---
patients_base <- raw_data %>%
  filter(redcap_event_name == "recruitment_arm_1") %>% 
  mutate(
    # Dates & Age
    demog_coldt = as_date(demog_coldt),
    dob = as_date(dob),
    pt_age = floor(as.numeric(demog_coldt - dob) / 365.25),
    operation_date = as_date(ifelse(!is.na(pat_op_date), pat_op_date, date_operation)),
    
    # Demographics
    gender_label = case_when(
      gender == 0 ~ "Male",
      gender == 1 ~ "Female",
      gender == 9 ~ "Other",
      TRUE ~ "Unknown"
    ),
    pat_county_label = case_when(
      pat_county == 1 ~ "Mombasa",
      pat_county == 2 ~ "Kwale",
      pat_county == 3 ~ "Kilifi",
      pat_county == 4 ~ "Tana River",
      pat_county == 5 ~ "Lamu",
      pat_county == 6 ~ "Taita/Taveta",
      pat_county == 7 ~ "Garissa",
      pat_county == 8 ~ "Wajir",
      pat_county == 9 ~ "Mandera",
      pat_county == 10 ~ "Marsabit",
      pat_county == 11 ~ "Isiolo",
      pat_county == 12 ~ "Meru",
      pat_county == 13 ~ "Tharaka-Nithi",
      pat_county == 14 ~ "Embu",
      pat_county == 15 ~ "Kitui",
      pat_county == 16 ~ "Machakos",
      pat_county == 17 ~ "Makueni",
      pat_county == 18 ~ "Nyandarua",
      pat_county == 19 ~ "Nyeri",
      pat_county == 20 ~ "Kirinyaga",
      pat_county == 21 ~ "Murang'a",
      pat_county == 22 ~ "Kiambu",
      pat_county == 23 ~ "Turkana",
      pat_county == 24 ~ "West Pokot",
      pat_county == 25 ~ "Samburu",
      pat_county == 26 ~ "Trans Nzoia",
      pat_county == 27 ~ "Uasin Gishu",
      pat_county == 28 ~ "Elgeyo/Marakwet",
      pat_county == 29 ~ "Nandi",
      pat_county == 30 ~ "Baringo",
      pat_county == 31 ~ "Laikipia",
      pat_county == 32 ~ "Nakuru",
      pat_county == 33 ~ "Narok",
      pat_county == 34 ~ "Kajiado",
      pat_county == 35 ~ "Kericho",
      pat_county == 36 ~ "Bomet",
      pat_county == 37 ~ "Kakamega",
      pat_county == 38 ~ "Vihiga",
      pat_county == 39 ~ "Bungoma",
      pat_county == 40 ~ "Busia",
      pat_county == 41 ~ "Siaya",
      pat_county == 42 ~ "Kisumu",
      pat_county == 43 ~ "Homa Bay",
      pat_county == 44 ~ "Migori",
      pat_county == 45 ~ "Kisii",
      pat_county == 46 ~ "Nyamira",
      pat_county == 47 ~ "Nairobi City",
      TRUE ~ "Unknown"
    ),
    
    # Clinical Labels
    procedure_type = case_when(
      pat_joint_problem == 1 ~ "Hip",
      pat_joint_problem == 2 ~ "Knee",
      pat_joint_problem == 3 ~ "Shoulder",
      TRUE ~ "Other"
    ),
    procedure_stage = case_when(
      procedure_type == "Hip" & op_type1 == 1 ~ "Primary",
      procedure_type == "Hip" & op_type1 == 2 ~ "Revision",
      procedure_type == "Knee" & op_type == 1 ~ "Primary",
      procedure_type == "Knee" & op_type == 2 ~ "Revision",
      TRUE ~ "Unknown"
    ),
    
    diagnosis_label = case_when(
      pat_diag___1 == 1 ~ "Osteoarthritis",
      pat_diag___2 == 1 ~ "Avascular Necrosis",
      pat_diag___3 == 1 ~ "Sickle Cell Disease",
      pat_diag___4 == 1 ~ "Rheumatoid Arthritis",
      pat_diag___9 == 1 ~ "Trauma",
      pat_diag___5 == 1 ~ "Infection (Active)",
      pat_diag___6 == 1 ~ "Infection (Previous)",
      pat_diag___10 == 1 ~ "Malignancy",
      pat_diag___99 == 1 ~ "Other",
      TRUE ~ "Unknown"
    ),
    
    indication_label = case_when(
      pat_indic___1 == 1 ~ "Osteoarthritis (Hip)",
      pat_indic___4 == 1 ~ "Avascular Necrosis (Hip)",
      pat_indic___5 == 1 ~ "Trauma (Hip)",
      pat_indic2___16 == 1 ~ "Dislocation/Subluxation (Hip)",
      pat_indic2___17 == 1 ~ "Infection (Hip)",
      pat_indic3___1 == 1 ~ "Osteoarthritis (Knee)",
      pat_indic3___2 == 1 ~ "Avascular Necrosis (Knee)",
      pat_indic3___8 == 1 ~ "Acute Trauma (Knee)",
      TRUE ~ "Other/Unknown"
    )
  ) %>%
  select(
    registry_id, pt_first, pt_last, pt_age, gender_label, 
    pat_county_label, procedure_type, procedure_stage, 
    diagnosis_label, indication_label, operation_date
  )

# This table is just for the tracker tab
follow_ups_completed <- raw_data %>%
  filter(str_detect(redcap_event_name, "follow_up")) %>%
  select(registry_id, redcap_event_name)

# --- 6. Define UI (User Interface) ---
ui <- dashboardPage(
  dashboardHeader(title = "Arthroplasty Registry"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Registry Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Follow-Up Tracker", tabName = "tracker", icon = icon("calendar-check"))
    ),
    
    h4("Dashboard Filters", style = "padding-left: 15px;"),
    
    selectInput("filter_procedure", "Procedure Site:",
                choices = c("All", "Hip", "Knee", "Shoulder"),
                selected = "All"),
    
    selectInput("filter_stage", "Procedure Stage:",
                choices = c("All", "Primary", "Revision"),
                selected = "All"),
    
    selectInput("filter_diagnosis", "Diagnosis:",
                choices = c("All", unique(patients_base$diagnosis_label)),
                selected = "All"),
    
    selectInput("filter_indication", "Indication:",
                choices = c("All", unique(patients_base$indication_label)),
                selected = "All"),
    
    selectInput("filter_county", "County:",
                choices = c("All", sort(unique(patients_base$pat_county_label))),
                selected = "All"),
    
    sliderInput("filter_age", "Age Range:",
                min = min(patients_base$pt_age, na.rm=T),
                max = max(patients_base$pt_age, na.rm=T),
                value = c(min(patients_base$pt_age, na.rm=T), max(patients_base$pt_age, na.rm=T)))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalPatientsBox"),
                valueBoxOutput("avgAgeBox"),
                valueBoxOutput("totalProceduresBox")
              ),
              fluidRow(
                box(title = "Procedure Breakdown", status = "primary", solidHeader = TRUE, plotOutput("procedurePlot")),
                box(title = "Diagnosis Breakdown", status = "primary", solidHeader = TRUE, plotOutput("diagnosisPlot"))
              ),
              fluidRow(
                box(title = "Patient Age Distribution", status = "primary", solidHeader = TRUE, width = 12, plotOutput("agePlot"))
              )
      ),
      tabItem(tabName = "tracker",
              h2("Patient Follow-Up Status"),
              p("This table shows all patients. Filters do not apply to this tab."),
              fluidRow(
                box(
                  title = "Follow-Up Status Tracker",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("trackerTable")
                )
              )
      )
    )
  )
)

# --- 7. Define Server Logic ---
server <- function(input, output) {
  
  # --- Reactive Data Frame for Overview Tab ---
  filtered_data <- reactive({
    
    data_filtered <- patients_base
    
    if (input$filter_procedure != "All") {
      data_filtered <- data_filtered %>%
        filter(procedure_type == input$filter_procedure)
    }
    
    if (input$filter_stage != "All") {
      data_filtered <- data_filtered %>%
        filter(procedure_stage == input$filter_stage)
    }
    
    if (input$filter_diagnosis != "All") {
      data_filtered <- data_filtered %>%
        filter(diagnosis_label == input$filter_diagnosis)
    }
    
    if (input$filter_indication != "All") {
      data_filtered <- data_filtered %>%
        filter(indication_label == input$filter_indication)
    }
    
    if (input$filter_county != "All") {
      data_filtered <- data_filtered %>%
        filter(pat_county_label == input$filter_county)
    }
    
    data_filtered <- data_filtered %>%
      filter(pt_age >= input$filter_age[1] & pt_age <= input$filter_age[2])
    
    return(data_filtered)
  })
  
  # --- Server Logic for Overview Tab ---
  
  output$totalPatientsBox <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total Filtered Patients", icon = icon("users"))
  })
  
  output$avgAgeBox <- renderValueBox({
    avg_age <- filtered_data() %>%
      summarise(mean_age = round(mean(pt_age, na.rm = TRUE), 1)) %>%
      pull(mean_age)
    if(is.nan(avg_age) | nrow(filtered_data()) == 0) { avg_age <- "N/A" }
    valueBox(avg_age, "Average Patient Age", icon = icon("birthday-cake"))
  })
  
  output$totalProceduresBox <- renderValueBox({
    total_proc <- filtered_data() %>% 
      filter(procedure_type %in% c("Hip", "Knee", "Shoulder")) %>% 
      nrow()
    valueBox(total_proc, "Total Procedures", icon = icon("procedures"))
  })
  
  output$procedurePlot <- renderPlot({
    if(nrow(filtered_data()) == 0) return(NULL) 
    
    filtered_data() %>%
      filter(procedure_type %in% c("Hip", "Knee", "Shoulder")) %>%
      count(procedure_type, .drop = FALSE) %>%
      ggplot(aes(x = procedure_type, y = n, fill = procedure_type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      labs(x = "Procedure Type", y = "Total Count") +
      theme_minimal(base_size = 14) + theme(legend.position = "none")
  })
  
  output$diagnosisPlot <- renderPlot({
    if(nrow(filtered_data()) == 0) return(NULL)
    
    top_diagnoses <- filtered_data() %>%
      count(diagnosis_label, sort = TRUE) %>%
      filter(diagnosis_label != "Unknown") %>%
      top_n(5, n) %>%
      pull(diagnosis_label)
    
    filtered_data() %>%
      filter(diagnosis_label %in% top_diagnoses) %>%
      count(diagnosis_label) %>%
      ggplot(aes(x = reorder(diagnosis_label, n), y = n, fill = diagnosis_label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.2, size = 5) +
      labs(x = "Diagnosis", y = "Total Count") +
      coord_flip() +
      theme_minimal(base_size = 14) + theme(legend.position = "none")
  })
  
  output$agePlot <- renderPlot({
    if(nrow(filtered_data()) == 0) return(NULL)
    
    ggplot(filtered_data(), aes(x = pt_age)) +
      geom_histogram(binwidth = 5, fill = "#0072B2", alpha = 0.7, color = "white") +
      labs(x = "Patient Age at Recruitment", y = "Number of Patients") +
      coord_cartesian(xlim = c(min(patients_base$pt_age, na.rm=T), max(patients_base$pt_age, na.rm=T))) +
      theme_minimal(base_size = 14)
  })
  
  # --- UNCHANGED: Server Logic for Tracker Tab ---
  tracker_data <- reactive({
    today <- Sys.Date()
    patients_base %>%
      mutate(
        due_6wk = operation_date + weeks(6),
        due_1yr = operation_date + weeks(52)
      ) %>%
      mutate(
        completed_6wk = registry_id %in% (follow_ups_completed %>% filter(redcap_event_name == "follow_up_6wk_arm_1") %>% pull(registry_id)),
        completed_1yr = registry_id %in% (follow_ups_completed %>% filter(redcap_event_name == "follow_up_1yr_arm_1") %>% pull(registry_id))
      ) %>%
      mutate(
        status_6wk = case_when(
          completed_6wk == TRUE ~ "Completed",
          due_6wk > today ~ "Pending",
          due_6wk <= today & due_6wk > (today - days(30)) ~ "Due Soon / Just Overdue",
          due_6wk < (today - days(30)) ~ "OVERDUE",
          TRUE ~ "Error"
        ),
        status_1yr = case_when(
          completed_1yr == TRUE ~ "Completed",
          due_1yr > (today + days(30)) ~ "Pending",
          due_1yr <= (today + days(30)) & due_1yr > today ~ "Due Soon",
          due_1yr <= today & due_1yr > (today - days(30)) ~ "OVERDUE (Recent)",
          due_1yr < (today - days(30)) ~ "OVERDUE (Old)",
          TRUE ~ "Error"
        )
      ) %>%
      select(
        registry_id, pt_first, pt_last, procedure_type, operation_date,
        status_6wk, due_6wk, status_1yr, due_1yr
      ) %>%
      arrange(desc(due_6wk))
  })
  
  output$trackerTable <- renderDT({
    datatable(
      tracker_data(),
      filter = 'top',
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# --- 8. Run the Application ---
shinyApp(ui = ui, server = server)