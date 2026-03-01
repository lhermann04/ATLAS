library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(officer)  # For reading Word documents

# Define UI with improved styling
ui <- fluidPage(
  # Custom CSS for better appearance - NAVY BLUE THEME
  tags$head(
    tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
            
            body {
                font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
                background-color: #f5f7fa;
            }
            
            .main-title {
                background: linear-gradient(135deg, #001f3f 0%, #003d7a 100%);
                color: white;
                padding: 25px;
                margin: -15px -15px 20px -15px;
                border-radius: 0;
                box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            }
            
            .main-title h1 {
                margin: 0;
                font-size: 28px;
                font-weight: 700;
            }
            
            .info-box {
                background: white;
                border-radius: 12px;
                padding: 20px;
                margin-bottom: 20px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                border-left: 4px solid #001f3f;
            }
            
            .info-box h4 {
                color: #2d3748;
                font-weight: 600;
                margin-top: 0;
                margin-bottom: 15px;
                font-size: 16px;
            }
            
            .stat-text {
                color: #4a5568;
                font-size: 14px;
                line-height: 1.8;
            }
            
            .filter-section {
                background: white;
                border-radius: 12px;
                padding: 20px;
                margin-bottom: 15px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.08);
            }
            
            .filter-section h4 {
                color: #2d3748;
                font-weight: 600;
                margin-top: 0;
                margin-bottom: 15px;
                font-size: 16px;
                display: flex;
                align-items: center;
            }
            
            .section-icon {
                margin-right: 8px;
                font-size: 18px;
            }
            
            .btn-custom {
                border-radius: 8px;
                font-weight: 500;
                transition: all 0.2s;
                border: none;
                padding: 10px 20px;
            }
            
            .btn-success {
                background: linear-gradient(135deg, #48bb78 0%, #38a169 100%);
                color: white;
            }
            
            .btn-success:hover {
                background: linear-gradient(135deg, #38a169 0%, #2f855a 100%);
                transform: translateY(-1px);
                box-shadow: 0 4px 12px rgba(72, 187, 120, 0.4);
            }
            
            .btn-warning {
                background: linear-gradient(135deg, #ed8936 0%, #dd6b20 100%);
                color: white;
            }
            
            .btn-warning:hover {
                background: linear-gradient(135deg, #dd6b20 0%, #c05621 100%);
                transform: translateY(-1px);
            }
            
            .btn-primary {
                background: linear-gradient(135deg, #001f3f 0%, #003d7a 100%);
                color: white;
            }
            
            .btn-primary:hover {
                background: linear-gradient(135deg, #003d7a 0%, #005bb5 100%);
                transform: translateY(-1px);
            }
            
            .btn-secondary {
                background: #e2e8f0;
                color: #4a5568;
            }
            
            .btn-secondary:hover {
                background: #cbd5e0;
            }
            
            .btn-info {
                background: linear-gradient(135deg, #001f3f 0%, #003d7a 100%);
                color: white;
            }
            
            .btn-info:hover {
                background: linear-gradient(135deg, #003d7a 0%, #005bb5 100%);
                transform: translateY(-1px);
            }
            
            .dynamic-filter {
                background: #f7fafc;
                border: 1px solid #e2e8f0;
                border-radius: 8px;
                padding: 15px;
                margin-bottom: 12px;
            }
            
            .form-control, .selectize-input {
                border-radius: 6px;
                border: 1px solid #e2e8f0;
                padding: 8px 12px;
            }
            
            .form-control:focus, .selectize-input.focus {
                border-color: #001f3f;
                box-shadow: 0 0 0 3px rgba(0, 31, 63, 0.1);
            }
            
            .checkbox {
                margin-bottom: 12px;
            }
            
            .checkbox label {
                font-weight: 500;
                color: #4a5568;
            }
            
            .well {
                background: white;
                border: none;
                border-radius: 12px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.08);
            }
            
            .nav-tabs {
                border-bottom: 2px solid #e2e8f0;
            }
            
            .nav-tabs > li > a {
                color: #718096;
                border: none;
                font-weight: 500;
                padding: 12px 20px;
            }
            
            .nav-tabs > li.active > a {
                color: #001f3f;
                background: white;
                border: none;
                border-bottom: 3px solid #001f3f;
            }
            
            .records-count {
                background: linear-gradient(135deg, #001f3f 0%, #003d7a 100%);
                color: white;
                padding: 15px 20px;
                border-radius: 8px;
                margin-bottom: 15px;
                font-weight: 600;
            }
            
            .filter-count {
                background: #f56565;
                color: white;
                padding: 15px 20px;
                border-radius: 8px;
                font-weight: 600;
                text-align: right;
            }
            
            .column-selector {
                max-height: 300px;
                overflow-y: auto;
                border: 1px solid #e2e8f0;
                border-radius: 6px;
                padding: 10px;
                background: #f7fafc;
            }
            
            .publications-content {
                background: white;
                border-radius: 12px;
                padding: 30px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                line-height: 1.8;
                color: #2d3748;
            }
            
            .publications-content h3 {
                color: #001f3f;
                font-weight: 600;
                margin-top: 20px;
                margin-bottom: 15px;
                padding-bottom: 10px;
                border-bottom: 2px solid #e2e8f0;
            }
            
            .publications-content p {
                margin-bottom: 15px;
            }
            
            .publications-content ul {
                margin-left: 20px;
                margin-bottom: 20px;
            }
            
            .publications-content li {
                margin-bottom: 10px;
                list-style-type: disc;
            }
            
            hr {
                border: 0;
                height: 1px;
                background: #e2e8f0;
                margin: 20px 0;
            }
        "))
  ),
  
  # Title
  div(class = "main-title",
      h1("🧬 SLC2A1 Variants Database Viewer")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background: transparent; border: none;",
      width = 3,
      
      # Database Info
      div(class = "info-box",
          h4("📊 Database Information"),
          div(class = "stat-text",
              textOutput("db_info")
          )
      ),
      
      # Column Visibility
      div(class = "filter-section",
          h4(span(class = "section-icon", "👁"), "Column Visibility"),
          p(style = "color: #718096; font-size: 13px; margin-bottom: 10px;", 
            "Select columns to display:"),
          fluidRow(
            column(6,
                   actionButton("select_all_cols", "Select All", 
                                class = "btn-custom btn-info btn-sm", 
                                style = "width: 100%;")
            ),
            column(6,
                   actionButton("deselect_all_cols", "Deselect All", 
                                class = "btn-custom btn-secondary btn-sm", 
                                style = "width: 100%;")
            )
          ),
          br(),
          div(class = "column-selector",
              uiOutput("column_checkboxes")
          )
      ),
      
      # Global Keyword Search
      div(class = "filter-section",
          h4(span(class = "section-icon", "🔍"), "Global Keyword Search"),
          textInput("keyword_search",
                    NULL,
                    placeholder = "e.g., seizure, ketogenic, absences"),
          checkboxInput("case_sensitive", "Case sensitive", FALSE),
          checkboxInput("whole_word", "Match whole word only", FALSE),
          actionButton("clear_search", "Clear Search", 
                       class = "btn-custom btn-secondary", 
                       style = "width: 100%;")
      ),
      
      # Column-specific filters
      div(class = "filter-section",
          h4(span(class = "section-icon", "📋"), "Column Filters"),
          p(style = "color: #718096; font-size: 13px; margin-bottom: 15px;", 
            "Add filters for any column:"),
          actionButton("add_filter", "➕ Add Column Filter", 
                       class = "btn-custom btn-success", 
                       style = "width: 100%; margin-bottom: 10px;"),
          uiOutput("dynamic_filters")
      ),
      
      # Quick filters
      div(class = "filter-section",
          h4(span(class = "section-icon", "⚡"), "Quick Filters"),
          selectInput("quick_filter_column",
                      "Select Column:",
                      choices = c("None")),
          conditionalPanel(
            condition = "input.quick_filter_column != 'None'",
            sliderInput("top_n_quick",
                        "Show Top N Values:",
                        min = 5,
                        max = 20,
                        value = 10,
                        step = 5),
            selectInput("quick_filter_value",
                        "Select Value:",
                        choices = c("All"))
          )
      ),
      
      # Action buttons
      actionButton("reset_filters", "🔄 Reset All Filters", 
                   class = "btn-custom btn-warning", 
                   style = "width: 100%; margin-bottom: 10px;"),
      downloadButton("downloadData", "📥 Download Filtered Data", 
                     class = "btn-custom btn-primary",
                     style = "width: 100%;")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel("📋 Data Table",
                 br(),
                 fluidRow(
                   column(6, 
                          div(class = "records-count",
                              textOutput("records_shown")
                          )
                   ),
                   column(6, 
                          div(class = "filter-count",
                              textOutput("active_filters_count")
                          )
                   )
                 ),
                 div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
                     DTOutput("dataTable")
                 )
        ),
        tabPanel("📊 Column Frequencies",
                 br(),
                 div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
                     selectInput("freq_column",
                                 "Select Column to Analyze:",
                                 choices = c("")),
                     sliderInput("freq_top_n",
                                 "Show Top N Values:",
                                 min = 5,
                                 max = 30,
                                 value = 15,
                                 step = 5),
                     plotOutput("frequency_plot", height = "600px"),
                     br(),
                     DTOutput("frequency_table")
                 )
        ),
        tabPanel("📚 Publications",
                 br(),
                 div(class = "publications-content",
                     uiOutput("publications_text")
                 )
        ),
        tabPanel("🔍 Active Filters",
                 br(),
                 div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
                     h4("Currently Active Filters"),
                     verbatimTextOutput("active_filters_summary")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  filter_counter <- reactiveVal(0)
  column_filters <- reactiveValues()
  selected_columns <- reactiveVal(NULL)
  
  # File paths
  file_path <- "Hack Rare - Copy.xlsx"
  publications_path <- "Publications.docx"
  
  # Load data reactively - skip first row and use second row as headers
  data <- reactive({
    
    if (!file.exists(file_path)) {
      showNotification("⚠️ Excel file not found!", 
                       type = "error", 
                       duration = 5)
      return(data.frame())
    }
    
    tryCatch({
      # Read the data, skipping the first row
      df <- read_excel(file_path, skip = 1)
      
      # Get column names
      col_names <- names(df)
      
      # Remove columns that start with "..." (like ...34, ...35, ...36)
      # These are empty columns from Excel
      cols_to_keep <- !grepl("^\\.\\.\\.", col_names)
      df <- df[, cols_to_keep, drop = FALSE]
      
      # Update column names after filtering
      col_names <- names(df)
      
      # Initialize selected columns to all columns if not set
      if (is.null(selected_columns())) {
        selected_columns(col_names)
      }
      
      return(df)
      
    }, error = function(e) {
      showNotification(paste("⚠️ Error reading file:", e$message), 
                       type = "error", 
                       duration = 5)
      return(data.frame())
    })
  })
  
  # Load publications from Word document - FIXED NULL ISSUE
  publications_content <- reactive({
    if (!file.exists(publications_path)) {
      return(HTML("<p style='color: #e53e3e;'>⚠️ Publications.docx file not found. Please make sure the file is in the same folder as the app.</p>"))
    }
    
    tryCatch({
      # Read Word document
      doc <- read_docx(publications_path)
      
      # Extract text from all paragraphs
      text_content <- docx_summary(doc)
      
      # Filter for text content only
      paragraphs <- text_content[text_content$content_type == "paragraph", ]
      
      # Combine into HTML - FIXED to remove NULL values
      html_parts <- sapply(paragraphs$text, function(p) {
        if (nchar(p) > 0) {
          # Check if it looks like a heading (short, all caps, or ends with colon)
          if (nchar(p) < 100 && (toupper(p) == p || grepl(":$", p))) {
            paste0("<h3>", p, "</h3>")
          } else {
            paste0("<p>", p, "</p>")
          }
        } else {
          NULL  # Return NULL for empty paragraphs
        }
      })
      
      # Remove NULL values before combining
      html_parts <- html_parts[!sapply(html_parts, is.null)]
      
      html_content <- paste(html_parts, collapse = "\n")
      
      return(HTML(html_content))
      
    }, error = function(e) {
      return(HTML(paste0(
        "<p style='color: #e53e3e;'>⚠️ Error reading Publications.docx: ", 
        e$message, 
        "</p>",
        "<p>Make sure the 'officer' package is installed:</p>",
        "<pre>install.packages('officer')</pre>"
      )))
    })
  })
  
  # Render publications
  output$publications_text <- renderUI({
    publications_content()
  })
  
  # Generate column checkboxes
  output$column_checkboxes <- renderUI({
    req(nrow(data()) > 0)
    col_names <- names(data())
    
    lapply(col_names, function(col) {
      checkboxInput(
        inputId = paste0("col_", make.names(col)),
        label = col,
        value = TRUE
      )
    })
  })
  
  # Select all columns
  observeEvent(input$select_all_cols, {
    req(nrow(data()) > 0)
    col_names <- names(data())
    
    lapply(col_names, function(col) {
      updateCheckboxInput(session, paste0("col_", make.names(col)), value = TRUE)
    })
  })
  
  # Deselect all columns
  observeEvent(input$deselect_all_cols, {
    req(nrow(data()) > 0)
    col_names <- names(data())
    
    lapply(col_names, function(col) {
      updateCheckboxInput(session, paste0("col_", make.names(col)), value = FALSE)
    })
  })
  
  # Get selected columns
  selected_cols <- reactive({
    req(nrow(data()) > 0)
    col_names <- names(data())
    
    selected <- sapply(col_names, function(col) {
      input_id <- paste0("col_", make.names(col))
      if (!is.null(input[[input_id]])) {
        return(input[[input_id]])
      }
      return(TRUE)  # Default to TRUE if not initialized
    })
    
    col_names[selected]
  })
  
  # Update column choices when data changes
  observe({
    req(nrow(data()) > 0)
    col_names <- names(data())
    
    updateSelectInput(session, "quick_filter_column",
                      choices = c("None", col_names))
    
    # Set default to first column with data
    default_col <- if(length(col_names) > 0) col_names[1] else ""
    
    updateSelectInput(session, "freq_column",
                      choices = col_names,
                      selected = default_col)
  })
  
  # Database info
  output$db_info <- renderText({
    req(nrow(data()) > 0)
    paste0(
      "Total Records: ", nrow(data()), "\n",
      "Total Columns: ", ncol(data()), "\n",
      "Visible Columns: ", length(selected_cols())
    )
  })
  
  # Add filter button
  observeEvent(input$add_filter, {
    req(nrow(data()) > 0)
    
    filter_id <- paste0("filter_", filter_counter())
    filter_counter(filter_counter() + 1)
    
    column_filters[[filter_id]] <- list(
      column = NULL,
      value = ""
    )
  })
  
  # Dynamic filter UI
  output$dynamic_filters <- renderUI({
    req(nrow(data()) > 0)
    
    filter_ids <- names(reactiveValuesToList(column_filters))
    
    if (length(filter_ids) == 0) {
      return(p(style = "color: #a0aec0; font-size: 13px; font-style: italic;", 
               "No filters added yet."))
    }
    
    col_names <- names(data())
    
    lapply(filter_ids, function(filter_id) {
      div(class = "dynamic-filter",
          fluidRow(
            column(10,
                   selectInput(paste0(filter_id, "_col"),
                               "Column:",
                               choices = col_names,
                               selected = col_names[1])
            ),
            column(2,
                   actionButton(paste0(filter_id, "_remove"),
                                "✖",
                                class = "btn-danger btn-sm",
                                style = "margin-top: 25px; width: 100%;")
            )
          ),
          textInput(paste0(filter_id, "_val"),
                    "Filter Value:",
                    placeholder = "Enter search term...")
      )
    })
  })
  
  # Remove filter handlers
  observe({
    filter_ids <- names(reactiveValuesToList(column_filters))
    
    lapply(filter_ids, function(filter_id) {
      observeEvent(input[[paste0(filter_id, "_remove")]], {
        column_filters[[filter_id]] <- NULL
      }, ignoreInit = TRUE)
    })
  })
  
  # Update quick filter values based on selected column
  observe({
    req(input$quick_filter_column != "None")
    req(nrow(data()) > 0)
    req(input$quick_filter_column %in% names(data()))
    
    tryCatch({
      col_data <- data() %>%
        count(!!sym(input$quick_filter_column), sort = TRUE) %>%
        filter(!is.na(!!sym(input$quick_filter_column))) %>%
        head(input$top_n_quick)
      
      if (nrow(col_data) > 0) {
        values <- col_data %>% pull(1)
        counts <- col_data %>% pull(n)
        
        choices <- c("All", setNames(as.character(values), 
                                     paste0(values, " (n=", counts, ")")))
        
        updateSelectInput(session, "quick_filter_value",
                          choices = choices)
      }
    }, error = function(e) {
      # Silently handle errors in quick filter update
    })
  })
  
  # Clear search button
  observeEvent(input$clear_search, {
    updateTextInput(session, "keyword_search", value = "")
  })
  
  # Reset all filters
  observeEvent(input$reset_filters, {
    # Clear column filters
    filter_ids <- names(reactiveValuesToList(column_filters))
    for (filter_id in filter_ids) {
      column_filters[[filter_id]] <- NULL
    }
    
    # Clear other inputs
    updateTextInput(session, "keyword_search", value = "")
    updateSelectInput(session, "quick_filter_column", selected = "None")
    
    showNotification("✅ All filters have been reset!", 
                     type = "message", 
                     duration = 2)
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(nrow(data()) > 0)
    df <- data()
    
    # Global keyword search across all columns
    if (input$keyword_search != "") {
      search_term <- input$keyword_search
      
      if (!input$case_sensitive) {
        search_term <- tolower(search_term)
      }
      
      if (input$whole_word) {
        pattern <- paste0("\\b", search_term, "\\b")
      } else {
        pattern <- search_term
      }
      
      matches <- apply(df, 1, function(row) {
        row_text <- paste(row, collapse = " ")
        if (!input$case_sensitive) {
          row_text <- tolower(row_text)
        }
        grepl(pattern, row_text, ignore.case = !input$case_sensitive)
      })
      
      df <- df[matches, , drop = FALSE]
    }
    
    # Apply column-specific filters
    filter_ids <- names(reactiveValuesToList(column_filters))
    
    for (filter_id in filter_ids) {
      col_name <- input[[paste0(filter_id, "_col")]]
      filter_val <- input[[paste0(filter_id, "_val")]]
      
      if (!is.null(col_name) && !is.null(filter_val) && filter_val != "" &&
          col_name %in% names(df)) {
        df <- df %>%
          filter(grepl(filter_val, as.character(get(col_name)), ignore.case = TRUE))
      }
    }
    
    # Apply quick filter
    if (input$quick_filter_column != "None" && 
        !is.null(input$quick_filter_value) && 
        input$quick_filter_value != "All" &&
        input$quick_filter_column %in% names(df)) {
      df <- df %>%
        filter(as.character(!!sym(input$quick_filter_column)) == input$quick_filter_value)
    }
    
    # Filter columns based on selection
    cols_to_show <- selected_cols()
    if (length(cols_to_show) > 0) {
      df <- df[, cols_to_show, drop = FALSE]
    }
    
    return(df)
  })
  
  # Count active filters
  output$active_filters_count <- renderText({
    filter_count <- 0
    
    if (input$keyword_search != "") filter_count <- filter_count + 1
    
    filter_ids <- names(reactiveValuesToList(column_filters))
    for (filter_id in filter_ids) {
      filter_val <- input[[paste0(filter_id, "_val")]]
      if (!is.null(filter_val) && filter_val != "") {
        filter_count <- filter_count + 1
      }
    }
    
    if (input$quick_filter_column != "None" && 
        !is.null(input$quick_filter_value) && 
        input$quick_filter_value != "All") {
      filter_count <- filter_count + 1
    }
    
    if (filter_count > 0) {
      paste("🔍", filter_count, "Active Filter(s)")
    } else {
      "No Active Filters"
    }
  })
  
  # Records shown text
  output$records_shown <- renderText({
    req(nrow(data()) > 0)
    paste("📊 Showing", nrow(filtered_data()), "of", nrow(data()), "records")
  })
  
  # Render data table
  output$dataTable <- renderDT({
    req(nrow(filtered_data()) > 0)
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        fixedColumns = list(leftColumns = 2),
        searchHighlight = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = 'top',
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  # Frequency analysis for selected column
  column_frequency <- reactive({
    req(nrow(data()) > 0)
    req(input$freq_column != "")
    req(input$freq_column %in% names(data()))
    
    tryCatch({
      data() %>%
        count(!!sym(input$freq_column), sort = TRUE) %>%
        filter(!is.na(!!sym(input$freq_column))) %>%
        mutate(percentage = round(n / sum(n) * 100, 1)) %>%
        head(input$freq_top_n)
    }, error = function(e) {
      data.frame()
    })
  })
  
  # Frequency plot - NAVY BLUE COLOR
  output$frequency_plot <- renderPlot({
    req(nrow(column_frequency()) > 0)
    
    plot_data <- column_frequency()
    col_name <- input$freq_column
    
    ggplot(plot_data, aes(x = reorder(as.character(!!sym(col_name)), n), y = n)) +
      geom_col(fill = "#001f3f", alpha = 0.85) +
      geom_text(aes(label = paste0(n, " (", percentage, "%)")), 
                hjust = -0.2, size = 4, color = "#2d3748", fontface = "bold") +
      coord_flip() +
      labs(title = paste("Top", input$freq_top_n, "Values in", col_name),
           x = NULL,
           y = "Frequency (Count)") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#2d3748", margin = margin(b = 20)),
        axis.text.y = element_text(size = 11, color = "#4a5568"),
        axis.text.x = element_text(size = 11, color = "#4a5568"),
        axis.title.x = element_text(size = 12, color = "#2d3748", face = "bold", margin = margin(t = 10)),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e2e8f0"),
        panel.grid.minor = element_blank()
      ) +
      ylim(0, max(plot_data$n) * 1.15)
  })
  
  # Frequency table
  output$frequency_table <- renderDT({
    req(nrow(column_frequency()) > 0)
    
    datatable(
      column_frequency(),
      options = list(
        pageLength = 20,
        dom = 't'
      ),
      colnames = c(input$freq_column, "Count", "Percentage (%)"),
      rownames = FALSE
    )
  })
  
  # Active filters summary
  output$active_filters_summary <- renderPrint({
    cat("=== Active Filters ===\n\n")
    
    filter_count <- 0
    
    if (input$keyword_search != "") {
      cat("Global Keyword Search:\n")
      cat("  Term: '", input$keyword_search, "'\n", sep = "")
      cat("  Case sensitive:", input$case_sensitive, "\n")
      cat("  Whole word:", input$whole_word, "\n\n")
      filter_count <- filter_count + 1
    }
    
    filter_ids <- names(reactiveValuesToList(column_filters))
    if (length(filter_ids) > 0) {
      cat("Column-Specific Filters:\n")
      for (filter_id in filter_ids) {
        col_name <- input[[paste0(filter_id, "_col")]]
        filter_val <- input[[paste0(filter_id, "_val")]]
        
        if (!is.null(col_name) && !is.null(filter_val) && filter_val != "") {
          cat("  ", col_name, " contains '", filter_val, "'\n", sep = "")
          filter_count <- filter_count + 1
        }
      }
      cat("\n")
    }
    
    if (input$quick_filter_column != "None" && 
        !is.null(input$quick_filter_value) && 
        input$quick_filter_value != "All") {
      cat("Quick Filter:\n")
      cat("  ", input$quick_filter_column, " = '", input$quick_filter_value, "'\n\n", sep = "")
      filter_count <- filter_count + 1
    }
    
    cat("Column Visibility:\n")
    cat("  Showing", length(selected_cols()), "of", ncol(data()), "columns\n\n")
    
    if (filter_count == 0) {
      cat("No active filters.\n")
    } else {
      cat("\n=== Results ===\n")
      cat("Matching records:", nrow(filtered_data()), "of", nrow(data()), "\n")
    }
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("SLC2A1_Variants_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
