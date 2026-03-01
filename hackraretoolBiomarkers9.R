library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(officer)

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

  .btn-lg {
    padding: 12px 18px;
    font-size: 11px;
    font-weight: 600;
    border-radius: 10px;
  }

  td.details-control {
    cursor: pointer;
    text-align: center;
    font-weight: bold;
  }

  td.details-control::before {
    content: \"▶\";
    font-size: 14px;
    color: #001f3f;
  }

  tr.shown td.details-control::before {
    content: \"▼\";
  }
"))
  ),
  
  tags$script(HTML("
Shiny.addCustomMessageHandler('injectDetails', function(message) {
  var container = document.getElementById('details-' + message.id);
  if(container){
    container.innerHTML = message.html;
  }
});
")),
  
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
                     actionButton(
                       "run_sim",
                       label = tagList(
                         icon("play"),
                         "Run Simulation"
                       ),
                       class = "btn-primary btn-lg btn-block"
                     )
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
                ),
                
                # ✅ NOW IT'S A PROPER TOP-LEVEL TAB
                tabPanel("Literature",
                         div(class="card",
                             h4("Published Literature & Registry References"),
                             uiOutput("publications_text")
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
  #Publications Tab
  # --------------------------------------------------------
  
  publications_path <- "Publications.docx"
  
  publications_content <- reactive({
    
    if (!file.exists(publications_path)) {
      return(HTML("<p style='color:red;'>Publications.docx not found.</p>"))
    }
    
    tryCatch({
      
      doc <- read_docx(publications_path)
      text_content <- docx_summary(doc)
      
      paragraphs <- text_content[text_content$content_type == "paragraph", ]
      
      html_parts <- sapply(paragraphs$text, function(p) {
        if (nchar(p) > 0) {
          if (nchar(p) < 100 && (toupper(p) == p || grepl(":$", p))) {
            paste0("<h3>", p, "</h3>")
          } else {
            paste0("<p>", p, "</p>")
          }
        } else {
          NULL
        }
      })
      
      html_parts <- html_parts[!sapply(html_parts, is.null)]
      
      HTML(paste(html_parts, collapse = "\n"))
      
    }, error = function(e) {
      HTML(paste0("<p style='color:red;'>Error reading file: ", e$message, "</p>"))
    })
  })
  
  output$publications_text <- renderUI({
    publications_content()
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
    
    if("patient_id" %in% names(df)){
      df$patient_id <- trimws(as.character(df$patient_id))
    }
    
    if("biomarker" %in% names(df)){
      df$biomarker <- trimws(df$biomarker)
    }
    
    df
  })
  
  
  
  patient_summary <- reactive({
    
    cleaned_data() %>%
      distinct(patient_id, .keep_all = TRUE) %>%
      select(
        patient_id,
        gene,
        variant,
        variant_type,
        age
      )
  })
  
  # --------------------------------------------------------
  # DATA TABLE
  # --------------------------------------------------------
  
  output$data_table <- renderDT({
    
    df <- patient_summary()
    
    # Add empty column for expand control
    df$` ` <- ""
    
    df <- df[, c(" ", "patient_id", "gene", "variant", "variant_type", "age")]
    
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        dom = "ft",
        columnDefs = list(
          list(
            className = 'details-control',
            orderable = FALSE,
            targets = 0
          )
        )
      ),
      callback = JS("
      var format = function(patient_id) {
        return '<div id=\"details-' + patient_id + '\"></div>';
      };

      table.on('click', 'td.details-control', function () {
        var tr = $(this).closest('tr');
        var row = table.row(tr);

        if (row.child.isShown()) {
          row.child.hide();
          tr.removeClass('shown');
        } else {
          var patient_id = row.data()[1];
          row.child(format(patient_id)).show();
          tr.addClass('shown');
          Shiny.setInputValue('expand_patient', patient_id, {priority: 'event'});
        }
      });
    ")
    )
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
  
  observeEvent(input$expand_patient, {
    
    selected_id <- input$expand_patient
    
    details_df <- cleaned_data() %>%
      filter(patient_id == selected_id) %>%
      select(
        date,
        symptom,
        severity,
        biomarker,
        biomarker_value,
        unit
      )
    
    details_html <- paste0(
      "<table class='table table-sm table-striped' style='margin-left:40px; width:auto;'>",
      "<thead><tr>",
      paste0("<th>", names(details_df), "</th>", collapse = ""),
      "</tr></thead><tbody>",
      paste0(
        apply(details_df, 1, function(row) {
          paste0("<tr>",
                 paste0("<td>", row, "</td>", collapse = ""),
                 "</tr>")
        }),
        collapse = ""
      ),
      "</tbody></table>"
    )
    
    session$sendCustomMessage(
      type = "injectDetails",
      message = list(
        id = selected_id,
        html = details_html
      )
    )
  })
  
  # --------------------------------------------------------
  # SIMULATION ENGINE
  # --------------------------------------------------------
  
  sim_data <- eventReactive(input$run_sim, {
    
    req(nrow(biomarker_scores()) > 0)
    
    n <- input$n_patients
    effect <- input$effect_size / 100
    noise <- input$noise_level
    
    top_sd <- biomarker_scores()$mean_within_sd[1]
    
    weeks <- 1:12
    baseline_weeks <- 1:6
    treatment_weeks <- 7:12
    
    sim_list <- lapply(1:n, function(i) {
      
      baseline_values <- rnorm(
        length(baseline_weeks),
        mean = 50,
        sd = top_sd
      )
      
      treatment_values <- rnorm(
        length(treatment_weeks),
        mean = 50 * (1 - effect),
        sd = top_sd * noise
      )
      
      data.frame(
        patient = paste0("P", i),
        week = weeks,
        value = c(baseline_values, treatment_values),
        phase = c(
          rep("Baseline", length(baseline_weeks)),
          rep("Treatment", length(treatment_weeks))
        )
      )
    })
    
    bind_rows(sim_list)
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