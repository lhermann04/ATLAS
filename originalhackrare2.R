# ============================================================
# Rare Disease Trial Readiness Engine
# GLUT1 Demo Version
# ============================================================

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)

# ===================== UI =====================

ui <- fluidPage(
  titlePanel("GLUT1 Deficiency — Community Data Explorer & Trial Readiness Engine"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File (.xlsx)", accept = ".xlsx"),
      hr(),
      uiOutput("variant_selector")
    ),
    
    mainPanel(
      tabsetPanel(
        
        # ----------------------------------------
        tabPanel("📊 Variant Explorer",
                 br(),
                 h4("Variant Distribution"),
                 plotOutput("variant_plot"),
                 br(),
                 h4("Symptom Severity by Variant"),
                 plotOutput("symptom_plot")
        ),
        
        # ----------------------------------------
        tabPanel("🧪 Trial Readiness",
                 br(),
                 fluidRow(
                   column(6,
                          h4("Ranked Endpoint Candidates"),
                          DTOutput("endpoint_ranking")
                   ),
                   column(6,
                          h4("Evidence & Uncertainty"),
                          verbatimTextOutput("endpoint_rationale")
                   )
                 ),
                 br(),
                 h4("Validation Plan"),
                 verbatimTextOutput("validation_plan"),
                 br(),
                 h4("Detectability Simulation"),
                 plotOutput("simulation_plot")
        )
      )
    )
  )
)

# ===================== SERVER =====================

server <- function(input, output) {
  
  # ---------------------------
  # Load Data
  # ---------------------------
  
  raw_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  # ---------------------------
  # Clean Data (DEMO STRUCTURE)
  # Requires columns:
  # patient_id, variant, symptom, severity,
  # date, biomarker, biomarker_value
  # ---------------------------
  
  cleaned_data <- reactive({
    df <- raw_data()
    
    names(df) <- tolower(names(df))
    names(df) <- gsub(" ", "_", names(df))
    
    validate(
      need("patient_id" %in% names(df), "Missing column: patient_id"),
      need("variant" %in% names(df), "Missing column: variant"),
      need("symptom" %in% names(df), "Missing column: symptom"),
      need("severity" %in% names(df), "Missing column: severity"),
      need("date" %in% names(df), "Missing column: date"),
      need("biomarker" %in% names(df), "Missing column: biomarker"),
      need("biomarker_value" %in% names(df), "Missing column: biomarker_value")
    )
    
    df %>%
      mutate(
        patient_id = as.character(patient_id),
        variant = as.character(variant),
        symptom = as.character(symptom),
        severity = as.numeric(severity),
        date = as.Date(date),
        biomarker = as.character(biomarker),
        biomarker_value = as.numeric(biomarker_value)
      )
  })
  
  # ---------------------------
  # Variant Selector
  # ---------------------------
  
  output$variant_selector <- renderUI({
    df <- cleaned_data <- reactive({
      req(input$file)
      
      df <- readxl::read_excel(input$file$datapath)
      
      names(df) <- tolower(names(df))
      names(df) <- gsub(" ", "_", names(df))
      
      # Identify key columns manually from your sheet
      variant_col <- grep("slc2a1", names(df), value=TRUE)[1]
      id_col <- grep("crid|patient", names(df), value=TRUE)[1]
      
      # Select symptom columns (adjust if needed)
      symptom_cols <- names(df)[which(names(df) %in% c(
        "endocrine","cancer_nc","ent","brain","skin",
        "bones","muscles","heart","lungs","digestive",
        "kidneys","blood"
      ))]
      
      df_long <- df %>%
        select(all_of(c(id_col, variant_col, symptom_cols))) %>%
        pivot_longer(cols = all_of(symptom_cols),
                     names_to = "symptom",
                     values_to = "present") %>%
        mutate(
          patient_id = as.character(.data[[id_col]]),
          variant = as.character(.data[[variant_col]]),
          severity = ifelse(present %in% c("Yes", "yes", 1), 3, 0),
          biomarker = symptom,
          biomarker_value = severity + rnorm(n(), 0, 0.5),
          date = Sys.Date()
        ) %>%
        select(patient_id, variant, symptom, severity,
               date, biomarker, biomarker_value)
      
      return(df_long)
    })

  
  filtered_data <- reactive({
    df <- cleaned_data()
    req(input$variant_choice)
    df %>% filter(variant == input$variant_choice)
  })
  
  # ===========================
  # VARIANT EXPLORER TAB
  # ===========================
  
  output$variant_plot <- renderPlot({
    df <- cleaned_data()
    
    df %>%
      count(variant) %>%
      ggplot(aes(x = reorder(variant, n), y = n)) +
      geom_col(fill = "#00274C") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Variant", y = "Count")
  })
  
  output$symptom_plot <- renderPlot({
    df <- filtered_data()
    
    df %>%
      group_by(symptom) %>%
      summarise(mean_severity = mean(severity, na.rm = TRUE),
                .groups = "drop") %>%
      ggplot(aes(x = symptom, y = mean_severity)) +
      geom_col(fill = "#FFCB05") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Mean Severity")
  })
  
  # ===========================
  # TRIAL READINESS ENGINE
  # ===========================
  
  # ---- Endpoint Ranking ----
  
  endpoint_scores <- reactive({
    df <- cleaned_data()
    
    df %>%
      group_by(patient_id, biomarker) %>%
      summarise(within_sd = sd(biomarker_value, na.rm = TRUE),
                .groups = "drop") %>%
      group_by(biomarker) %>%
      summarise(
        mean_within_sd = mean(within_sd, na.rm = TRUE),
        n_patients = n(),
        .groups = "drop"
      ) %>%
      mutate(
        detectability_score = 1 / mean_within_sd
      ) %>%
      arrange(desc(detectability_score))
  })
  
  output$endpoint_ranking <- renderDT({
    scores <- endpoint_scores()
    validate(need(nrow(scores) > 0, "No biomarker data available"))
    datatable(scores)
  })
  
  # ---- Rationale ----
  
  output$endpoint_rationale <- renderPrint({
    scores <- endpoint_scores()
    req(nrow(scores) > 0)
    
    top <- scores$biomarker[1]
    
    cat("Top Proposed Endpoint:", top, "\n\n")
    
    cat("Why this candidate:\n")
    cat("- Lowest within-patient variability\n")
    cat("- Measured across", scores$n_patients[1], "patients\n")
    cat("- Suitable for within-person baseline modeling\n\n")
    
    cat("Uncertainty & Risks:\n")
    cat("- Tiny cohort size limits power\n")
    cat("- Measurement noise may distort SD\n")
    cat("- Missing timepoints reduce longitudinal sensitivity\n\n")
    
    cat("Safety Considerations:\n")
    cat("- Confirm biomarker clinical relevance\n")
    cat("- Evaluate pediatric feasibility\n")
    cat("- Avoid invasive measures unless necessary\n")
  })
  
  # ---- Validation Plan ----
  
  output$validation_plan <- renderPrint({
    cat("Proposed Validation Strategy:\n\n")
    
    cat("1. Within-Patient Baseline Modeling:\n")
    cat("   - Collect 3–6 baseline measures per patient\n")
    cat("   - Detect percent change from baseline mean\n\n")
    
    cat("2. N-of-1 Simulation:\n")
    cat("   - Model treatment signal vs baseline variance\n")
    cat("   - Estimate detectable effect size\n\n")
    
    cat("3. Registry Replication:\n")
    cat("   - Validate signal consistency across variants\n\n")
    
    cat("4. Literature Spot Check:\n")
    cat("   - Confirm biological plausibility\n")
  })
  
  # ---- Detectability Simulation ----
  
  output$simulation_plot <- renderPlot({
    scores <- endpoint_scores()
    req(nrow(scores) > 0)
    
    set.seed(1)
    
    baseline <- rnorm(10, mean = 100, sd = 5)
    treatment <- rnorm(10, mean = 110, sd = 5)
    
    df_sim <- data.frame(
      value = c(baseline, treatment),
      phase = rep(c("Baseline", "Treatment"), each = 10)
    )
    
    ggplot(df_sim, aes(x = phase, y = value)) +
      geom_boxplot(fill = "#9BCBEB") +
      theme_minimal() +
      labs(title = "Simulated Within-Patient Detectability",
           y = "Biomarker Value")
  })
}

# ===================== RUN APP =====================

shinyApp(ui = ui, server = server)