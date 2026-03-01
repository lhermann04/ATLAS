library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# ==========================================================
# PRELOAD DATA (DEMO MODE)
# ==========================================================

demo_data <- read_excel("Hack Rare - Copy.xlsx")

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
      h1("🧬 TinyTrial: Community Genotype–Phenotype Engine (GLUT1 Demo)")
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
                
                tabPanel("📂 Data Explorer",
                         div(class="card",
                             h4("Active Dataset"),
                             DTOutput("data_table")
                         )
                ),
                
                tabPanel("🧬 Genotype–Phenotype",
                         div(class="card",
                             h4("Variant Distribution"),
                             plotOutput("variant_plot", height="300px")
                         ),
                         div(class="card",
                             h4("Symptom Severity by Variant"),
                             plotOutput("symptom_plot", height="400px")
                         )
                ),
                
                tabPanel("🎯 Biomarker Discovery",
                         div(class="card",
                             h4("Biomarker Detectability Ranking"),
                             DTOutput("biomarker_ranking")
                         )
                ),
                
                tabPanel("📈 Detectability Simulation",
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
  # DATA SOURCE LOGIC (UPLOAD OR DEMO)
  # --------------------------------------------------------
  
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
    
    df
  })
  
  # --------------------------------------------------------
  # TABLE
  # --------------------------------------------------------
  
  output$data_table <- renderDT({
    datatable(cleaned_data(), options=list(scrollX=TRUE))
  })
  
  # --------------------------------------------------------
  # GENOTYPE–PHENOTYPE
  # --------------------------------------------------------
  
  output$variant_plot <- renderPlot({
    cleaned_data() %>%
      count(variant) %>%
      ggplot(aes(x=reorder(variant,n), y=n)) +
      geom_col(fill="#001f3f") +
      coord_flip() +
      theme_minimal()
  })
  
  output$symptom_plot <- renderPlot({
    cleaned_data() %>%
      group_by(variant, symptom) %>%
      summarise(mean_severity = mean(severity, na.rm=TRUE)) %>%
      ggplot(aes(x=symptom, y=mean_severity, fill=variant)) +
      geom_col(position="dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  })
  
  # --------------------------------------------------------
  # BIOMARKER RANKING
  # --------------------------------------------------------
  
  biomarker_scores <- reactive({
    cleaned_data() %>%
      group_by(biomarker, patient_id) %>%
      summarise(sd_within = sd(biomarker_value, na.rm=TRUE)) %>%
      group_by(biomarker) %>%
      summarise(
        mean_within_sd = mean(sd_within, na.rm=TRUE)
      ) %>%
      mutate(
        detectability_score = 1/mean_within_sd
      ) %>%
      arrange(desc(detectability_score))
  })
  
  output$biomarker_ranking <- renderDT({
    datatable(biomarker_scores())
  })
  
  # --------------------------------------------------------
  # SIMULATION
  # --------------------------------------------------------
  
  sim_data <- eventReactive(input$run_sim,{
    
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
    df <- sim_data()
    var_treat <- var(df$value[df$phase=="Treatment"])
    var_base <- var(df$value[df$phase=="Baseline"])
    paste("Variance Ratio:", round(var_treat/var_base,2))
  })
  
  output$detection_prob <- renderText({
    improvement <- input$effect_size
    prob <- round(min(95, improvement + 50 - (input$noise_level*20)))
    paste("Detection Probability:", prob,"%")
  })
  
  output$endpoint_recommendation <- renderText({
    paste("Top Endpoint:", biomarker_scores()$biomarker[1])
  })
}

shinyApp(ui, server)
