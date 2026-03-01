library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

# ===============================
# UI
# ===============================

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
      h1("🧪 TinyTrial: Rare Disease Biomarker Readiness Engine")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      div(class="card",
          h4("Step 1️⃣ Disease Context"),
          textInput("disease_name","Disease Name"),
          textInput("gene_name","Gene"),
          selectInput("pediatric","Pediatric Population?",
                      choices=c("Yes","No")),
          numericInput("cost_limit","Max Cost / Patient / Month ($)",500)
      ),
      
      div(class="card",
          h4("Step 2️⃣ Tiny-N Setup"),
          numericInput("n_patients","Number of Patients",8),
          numericInput("baseline_weeks","Baseline Weeks",4),
          numericInput("treatment_weeks","Treatment Weeks",12),
          sliderInput("effect_size",
                      "Expected Improvement (%)",
                      0,50,20),
          sliderInput("noise_level",
                      "Noise Multiplier",
                      0.5,2,1,step=0.1)
      ),
      
      actionButton("run_simulation","Run Simulation",
                   class="btn-primary",style="width:100%;"),
      br(),
      downloadButton("download_report","Download Readiness Report",
                     class="btn-success",style="width:100%;")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        # =====================================
        # Biomarker Designer
        # =====================================
        tabPanel("🧬 Biomarker Designer",
                 
                 div(class="card",
                     h4("Add Biomarker"),
                     textInput("biomarker_name","Biomarker Name"),
                     selectInput("measurement_type","Measurement Type",
                                 c("Home","Clinic","Hybrid")),
                     numericInput("baseline_sd","Estimated Baseline SD",1),
                     selectInput("noise_risk","Noise Risk",
                                 c("Low","Moderate","High")),
                     numericInput("monthly_cost","Monthly Cost ($)",100),
                     textAreaInput("confounders","Confounders"),
                     textAreaInput("safety_risks","Safety Considerations"),
                     actionButton("add_biomarker","Add Biomarker",
                                  class="btn-info")
                 ),
                 
                 DTOutput("biomarker_table")
        ),
        
        # =====================================
        # Feasibility Ranking
        # =====================================
        tabPanel("🏆 Feasibility Ranking",
                 div(class="card",
                     DTOutput("ranking_table"),
                     plotOutput("ranking_plot",height="400px")
                 )
        ),
        
        # =====================================
        # Simulation
        # =====================================
        tabPanel("📈 Longitudinal Simulation",
                 div(class="card",
                     plotOutput("simulation_plot",height="500px")
                 ),
                 
                 fluidRow(
                   column(4,
                          div(class="metric-box",
                              textOutput("variance_ratio"))
                   ),
                   column(4,
                          div(class="metric-box",
                              textOutput("detection_probability"))
                   ),
                   column(4,
                          div(class="metric-box",
                              textOutput("consistency_metric"))
                   )
                 )
        ),
        
        # =====================================
        # Summary
        # =====================================
        tabPanel("📋 Trial Readiness Summary",
                 div(class="card",
                     verbatimTextOutput("readiness_summary")
                 )
        )
      )
    )
  )
)

# ===============================
# SERVER
# ===============================

server <- function(input, output, session) {
  
  # Store biomarkers
  biomarkers <- reactiveVal(data.frame())
  
  # Add biomarker
  observeEvent(input$add_biomarker,{
    
    new_row <- data.frame(
      Name = input$biomarker_name,
      Type = input$measurement_type,
      SD = input$baseline_sd,
      Noise = input$noise_risk,
      Cost = input$monthly_cost,
      stringsAsFactors = FALSE
    )
    
    biomarkers(rbind(biomarkers(), new_row))
  })
  
  # Display biomarker table
  output$biomarker_table <- renderDT({
    datatable(biomarkers(), options=list(pageLength=5))
  })
  
  # Ranking logic
  ranking_data <- reactive({
    df <- biomarkers()
    if(nrow(df)==0) return(NULL)
    
    df <- df %>%
      mutate(
        plausibility = 4,  # placeholder
        burden = ifelse(Type=="Clinic",3,1),
        noise_penalty = case_when(
          Noise=="Low" ~ 1,
          Noise=="Moderate" ~ 2,
          TRUE ~ 3
        ),
        cost_penalty = ifelse(Cost > input$cost_limit,3,1),
        score = (2*plausibility) - (burden + noise_penalty + cost_penalty)
      )
    
    df
  })
  
  output$ranking_table <- renderDT({
    req(ranking_data())
    datatable(ranking_data())
  })
  
  output$ranking_plot <- renderPlot({
    req(ranking_data())
    
    ggplot(ranking_data(),
           aes(x=reorder(Name,score), y=score))+
      geom_col(fill="#001f3f")+
      coord_flip()+
      theme_minimal()+
      labs(x=NULL,y="Feasibility Score")
  })
  
  # ===============================
  # Simulation Engine
  # ===============================
  
  simulate_data <- eventReactive(input$run_simulation,{
    
    n <- input$n_patients
    b_weeks <- input$baseline_weeks
    t_weeks <- input$treatment_weeks
    effect <- input$effect_size/100
    noise <- input$noise_level
    
    sim <- data.frame()
    
    for(i in 1:n){
      
      baseline <- rnorm(b_weeks,10,1)
      treatment <- rnorm(t_weeks,
                         10*(1-effect),
                         1*noise)
      
      df <- data.frame(
        patient=i,
        week=1:(b_weeks+t_weeks),
        value=c(baseline,treatment),
        phase=c(rep("Baseline",b_weeks),
                rep("Treatment",t_weeks))
      )
      
      sim <- rbind(sim,df)
    }
    
    sim
  })
  
  output$simulation_plot <- renderPlot({
    req(simulate_data())
    
    ggplot(simulate_data(),
           aes(x=week,y=value,group=patient,color=phase))+
      geom_line(alpha=0.4)+
      stat_summary(aes(group=phase),
                   fun=mean,
                   geom="line",
                   size=1.5,
                   color="#001f3f")+
      theme_minimal()+
      labs(y="Biomarker Value")
  })
  
  # Metrics
  
  output$variance_ratio <- renderText({
    req(simulate_data())
    df <- simulate_data()
    var_base <- var(df$value[df$phase=="Baseline"])
    var_treat <- var(df$value[df$phase=="Treatment"])
    ratio <- round(var_treat/var_base,2)
    paste("Variance Ratio:",ratio)
  })
  
  output$detection_probability <- renderText({
    req(simulate_data())
    improvement <- input$effect_size
    prob <- round(min(95, improvement + 50 - (input$noise_level*20)))
    paste("Detection Probability:",prob,"%")
  })
  
  output$consistency_metric <- renderText({
    req(simulate_data())
    paste("Patients Simulated:",input$n_patients)
  })
  
  # ===============================
  # Readiness Summary
  # ===============================
  
  output$readiness_summary <- renderPrint({
    
    cat("=== Trial Readiness Summary ===\n\n")
    
    cat("Disease:",input$disease_name,"\n")
    cat("Gene:",input$gene_name,"\n")
    cat("Pediatric:",input$pediatric,"\n\n")
    
    if(!is.null(ranking_data())){
      cat("Top Biomarker Candidate:\n")
      top <- ranking_data() %>% arrange(desc(score)) %>% head(1)
      print(top)
    }
    
    cat("\nSimulation Parameters:\n")
    cat("N =",input$n_patients,"\n")
    cat("Baseline Weeks =",input$baseline_weeks,"\n")
    cat("Treatment Weeks =",input$treatment_weeks,"\n")
    cat("Expected Improvement =",input$effect_size,"%\n")
    
    cat("\nUncertainty Considerations:\n")
    cat("- Simulation assumes normal distribution\n")
    cat("- Noise modeled as SD multiplier\n")
    cat("- Real-world confounders not fully captured\n")
    
  })
  
  # ===============================
  # Download Report
  # ===============================
  
  output$download_report <- downloadHandler(
    filename = function(){
      paste0("TinyTrial_Readiness_",Sys.Date(),".txt")
    },
    content = function(file){
      sink(file)
      print("TinyTrial Readiness Report")
      print(Sys.Date())
      sink()
    }
  )
}

shinyApp(ui, server)

