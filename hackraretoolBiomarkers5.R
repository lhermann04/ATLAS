library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# ==========================================================
# PRELOAD DATA (DEMO MODE)
# ==========================================================

demo_data <- read_excel("Biomarkers5.xlsx")

# ==========================================================
# UI
# ==========================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f7fa; font-family: 'Inter', sans-serif; }
      .main-title {
        background: linear-gradient(135deg, #001f3f 0%, #003d7a 100%);
        color: white;
        padding: 25px;
        margin: -15px -15px 20px -15px;
      }
      .card {
        background: white;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }
      .metric-box {
        background: linear-gradient(135deg, #001f3f 0%, #003d7a 100%);
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-weight: 600;
        text-align: center;
      }
    "))
  ),
  
  div(class = "main-title",
      h1("Atlas: Engineering Endpoints for Tiny Cohorts")
  ),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 
                 div(class="card",
                     h4("Upload Community Dataset (Optional Override)"),
                     fileInput("datafile",
                               "Upload Excel (.xlsx)",
                               accept = ".xlsx"),
                     helpText("If no file uploaded, demo dataset is used.")
                 ),
                 
                 div(class="card",
                     h4("Simulation Settings"),
                     numericInput("n_patients","Simulated N",8),
                     sliderInput("effect_size",
                                 "Expected Improvement (%)",
                                 0,50,20),
                     sliderInput("noise_level",
                                 "Noise Multiplier",
                                 0.5,2,1,step=0.1),
                     actionButton("run_sim","Run Detectability Simulation",
                                  class="btn-primary", style="width:100%;")
                 )
    ),
    
    mainPanel(width = 9,
              
              tabsetPanel(
                
                tabPanel("Data Explorer",
                         div(class="card",
                             h4("Active Dataset"),
                             DTOutput("data_table")
                         )
                ),
                
                tabPanel("Genotype–Phenotype",
                         div(class="card",
                             h4("Variant Distribution"),
                             plotOutput("variant_plot", height="300px")
                         ),
                         div(class="card",
                             h4("Symptom Severity by Variant"),
                             plotOutput("symptom_plot", height="400px")
                         )
                ),
                
                tabPanel("Biomarker Discovery",
                         div(class="card",
                             h4("Biomarker Detectability Ranking"),
                             DTOutput("biomarker_ranking")
                         )
                ),
                
                tabPanel("Detectability Simulation",
                         div(class="card",
                             h4("Simulated Endpoint Trajectories"),
                             plotOutput("simulation_plot", height="400px")
                         ),
                         fluidRow(
                           column(4, div(class="metric-box",
                                         textOutput("variance_ratio"))),
                           column(4, div(class="metric-box",
                                         textOutput("detection_prob"))),
                           column(4, div(class="metric-box",
                                         textOutput("endpoint_recommendation")))
                         )
                )
              )
    )
  )
)

# ==========================================================
# SERVER
# ==========================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------
  # DATA SOURCE LOGIC
  # --------------------------------------------------------
  
  observe({
    print(table(cleaned_data()$variant, useNA = "ifany"))
  })
  
  raw_data <- reactive({
    if (is.null(input$datafile)) {
      showNotification("Demo dataset loaded (GLUT1 case study mode)", type="message")
      return(demo_data)
    } else {
      return(read_excel(input$datafile$datapath))
    }
  })
  
  # --------------------------------------------------------
  # CLEANING LAYER
  # --------------------------------------------------------
  
  cleaned_data <- reactive({
    df <- raw_data()
    
    names(df) <- tolower(names(df))
    colnames(df) <- gsub(" ", "_", colnames(df))
    
    if("variant" %in% names(df)){
      df$variant <- trimws(as.character(df$variant))
      df$variant[df$variant == ""] <- NA
    }
    
    if("biomarker" %in% names(df)){
      df$biomarker <- trimws(df$biomarker)
    }
    
    df
  })
  
  # --------------------------------------------------------
  # DATA TABLE
  # --------------------------------------------------------
  
  output$data_table <- renderDT({
    datatable(cleaned_data(), options=list(scrollX=TRUE))
  })
  
  # --------------------------------------------------------
  # GENOTYPE–PHENOTYPE
  # --------------------------------------------------------
  
  output$variant_plot <- renderPlot({
    
    df <- cleaned_data() %>%
      filter(!is.na(variant_type),
             variant_type != "") %>%
      distinct(patient_id, .keep_all = TRUE) %>%
      count(variant_type)
    
    req(nrow(df) > 0)
    
    df <- df %>%
      mutate(percent = round(n / sum(n) * 100, 1),
             label = paste0(percent, "%"))
    
    ggplot(df, aes(x = reorder(variant_type, n), y = n)) +
      geom_col(fill = "#0A1F44", width = 0.7) +
      geom_text(aes(label = label), hjust = -0.2) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(
        x = "Variant Type",
        y = "Number of Patients",
        title = "Distribution of Variant Types"
      ) +
      expand_limits(y = max(df$n) + 0.5)
  })
  
  output$symptom_plot <- renderPlot({
    cleaned_data() %>%
      group_by(variant, symptom) %>%
      summarise(mean_severity = mean(severity, na.rm=TRUE),
                .groups="drop") %>%
      ggplot(aes(x=symptom, y=mean_severity, fill=variant)) +
      geom_col(position="dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  })
  
  # --------------------------------------------------------
  # BIOMARKER RANKING (FIXED)
  # --------------------------------------------------------
  
  biomarker_scores <- reactive({
    
    req("biomarker" %in% names(cleaned_data()))
    
    cleaned_data() %>%
      filter(!is.na(biomarker),
             biomarker != "") %>%
      group_by(biomarker, patient_id) %>%
      summarise(sd_within = sd(biomarker_value, na.rm=TRUE),
                .groups="drop") %>%
      group_by(biomarker) %>%
      summarise(mean_within_sd = mean(sd_within, na.rm=TRUE),
                .groups="drop") %>%
      filter(!is.na(mean_within_sd),
             mean_within_sd > 0) %>%
      mutate(detectability_score = 1/mean_within_sd) %>%
      arrange(desc(detectability_score))
  })
  
  output$biomarker_ranking <- renderDT({
    datatable(biomarker_scores())
  })
  
  # --------------------------------------------------------
  # SIMULATION
  # --------------------------------------------------------
  
  sim_data <- eventReactive(input$run_sim,{
    
    req(nrow(biomarker_scores()) > 0)
    
    top_sd <- biomarker_scores()$mean_within_sd[1]
    
    n <- input$n_patients
    effect <- input$effect_size/100
    noise <- input$noise_level
    
    sim <- data.frame()
    
    for(i in 1:n){
      baseline <- rnorm(6,10,top_sd)
      treatment <- rnorm(6,10*(1-effect),top_sd*noise)
      
      df <- data.frame(
        patient=i,
        week=1:12,
        value=c(baseline,treatment),
        phase=c(rep("Baseline",6),rep("Treatment",6))
      )
      sim <- rbind(sim,df)
    }
    
    sim
  })
  
  output$simulation_plot <- renderPlot({
    req(sim_data())
    
    ggplot(sim_data(),
           aes(x=week,y=value,group=patient,color=phase))+
      geom_line(alpha=0.4)+
      stat_summary(aes(group=phase),
                   fun=mean,
                   geom="line",
                   size=1.5,
                   color="#001f3f")+
      theme_minimal()
  })
  
  output$variance_ratio <- renderText({
    req(sim_data())
    
    df <- sim_data()
    var_treat <- var(df$value[df$phase=="Treatment"])
    var_base <- var(df$value[df$phase=="Baseline"])
    
    paste("Variance Ratio:", round(var_treat/var_base,2))
  })
  
  output$detection_prob <- renderText({
    
    req(nrow(biomarker_scores()) > 0)
    
    top_sd <- biomarker_scores()$mean_within_sd[1]
    n <- input$n_patients
    effect <- input$effect_size/100
    noise <- input$noise_level
    
    sigma_b <- top_sd
    sigma_t <- top_sd * noise
    rho <- 0.6
    
    mean_change <- 10 * effect
    
    sd_diff <- sqrt(sigma_b^2 + sigma_t^2 - 2*rho*sigma_b*sigma_t)
    
    power <- power.t.test(
      n = n,
      delta = mean_change,
      sd = sd_diff,
      sig.level = 0.05,
      type = "paired"
    )$power
    
    paste("Statistical Power:", round(power*100,1), "%")
  })
  
  output$endpoint_recommendation <- renderText({
    
    if(nrow(biomarker_scores()) == 0){
      return("Top Endpoint: None (check data quality)")
    }
    
    paste("Top Endpoint:", biomarker_scores()$biomarker[1])
  })
}

shinyApp(ui, server)