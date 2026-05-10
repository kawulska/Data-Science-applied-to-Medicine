library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(rmarkdown)

db_conn <- dbConnect(RSQLite::SQLite(), "biomed_database.sqlite")

dbExecute(db_conn, "
  CREATE TABLE IF NOT EXISTS audit_logs (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    patient_id  TEXT,
    action      TEXT,
    system_user TEXT,
    timestamp   TEXT
  )
")

onStop(function() dbDisconnect(db_conn))

# UI
ui <- navbarPage(
  title = "Biomedical Data Hub",
  
  # Tab 1: Add new patient records
  tabPanel("Record Management",
           fluidPage(
             h2("Patient Data Entry Form"),
             p("Fields marked with * are required. The form checks inputs before saving."),
             
             sidebarLayout(
               sidebarPanel(
                 textInput("patient_id",   "* Patient ID (Format: P-XXXX)",
                           placeholder = "e.g., P-1234"),
                 dateInput("dob",          "* Date of Birth",
                           value = Sys.Date() - 365 * 30, max = Sys.Date()),
                 numericInput("age",       "* Age (years)", value = NA, min = 0, max = 120),
                 selectInput("sex",        "* Sex",
                             choices = c("", "Female", "Male", "Other")),
                 numericInput("weight",    "Weight (kg)", value = NA, min = 1, max = 300),
                 numericInput("height",    "Height (m)",  value = NA, min = 0.3, max = 2.5),
                 selectInput("blood_type", "Blood Type",
                             choices = c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")),
                 textInput("diagnosis",    "Diagnosis Code (ICD-10)",
                           placeholder = "e.g., M54.5"),
                 numericInput("dosage",    "Dosage (mg)", value = NA, min = 0),
                 selectInput("smoker",     "Smoker",
                             choices = c("", "yes", "no")),
                 textInput("doctor_name",  "Attending Doctor",
                           placeholder = "e.g., Dr. Smith"),
                 br(),
                 actionButton("submit_btn", "Save Record",
                              class = "btn-primary", style = "width:100%;")
               ),
               mainPanel(
                 h4("Status:"),
                 verbatimTextOutput("status_msg"),
                 br(),
                 h4("Recent entries (last 10):"),
                 tableOutput("audit_table")
               )
             )
           )
  ),
  
  # Tab 2: Data quality checks
  tabPanel("Quality Indicators",
           fluidPage(
             h2("Data Quality Dashboard"),
             p("Click refresh to recalculate quality metrics from the current database."),
             
             fluidRow(
               column(6,
                      actionButton("refresh_btn", "Refresh Metrics",
                                   icon = icon("sync"), class = "btn-info")
               ),
               column(6,
                      downloadButton("download_report", "Download HTML Report",
                                     class = "btn-success")
               )
             ),
             hr(),
             
             fluidRow(
               # Completeness - how many NAs per column?
               column(4,
                      h4("1. Completeness (Missing Values)"),
                      plotOutput("plot_completeness")
               ),
               # Consistency - are Sex values what we expect?
               column(4,
                      h4("2. Consistency (Sex Variable)"),
                      p("Checking for unexpected values:"),
                      verbatimTextOutput("text_consistency"),
                      uiOutput("consistency_alert")
               ),
               # Accuracy - any extreme dosage values?
               column(4,
                      h4("3. Accuracy (Dosage Outliers)"),
                      plotOutput("plot_accuracy"),
                      verbatimTextOutput("outlier_summary")
               )
             )
           )
  ),
  
  # Tab 3: Explore the data visually
  tabPanel("Data Visualization",
           fluidPage(
             h2("Clinical Data Exploration"),
             
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Chart Options"),
                        selectInput("viz_x", "X-axis variable",
                                    choices = c("Age", "Dosage_mg", "Weight", "Height")),
                        selectInput("viz_color", "Colour by",
                                    choices = c("Sex", "Blood_Type", "Smoker")),
                        selectInput("viz_type", "Chart type",
                                    choices = c("Histogram", "Boxplot", "Scatter (Age vs Dosage)")),
                        actionButton("viz_refresh", "Update Chart",
                                     class = "btn-primary", style = "width:100%;")
                      )
               ),
               column(9,
                      plotOutput("main_plot", height = "420px"),
                      hr(),
                      fluidRow(
                        column(6, plotOutput("age_dist", height = "280px")),
                        column(6, plotOutput("sex_pie",  height = "280px"))
                      )
               )
             )
           )
  ),
  
  # Tab 4: Full audit trail
  tabPanel("Audit Log",
           fluidPage(
             h2("Traceability & Audit Log"),
             p("Every record insertion is logged here with the username and timestamp."),
             actionButton("audit_refresh", "Refresh Log", icon = icon("sync"),
                          class = "btn-info"),
             hr(),
             tableOutput("full_audit_table")
           )
  )
)


# Server

server <- function(input, output, session) {
  
  # Tab 1: Record Management
  status_text <- reactiveVal("Awaiting data entry...")
  
  observeEvent(input$submit_btn, {
    errors <- c()
    
    # Validate inputs before saving anything
    pid <- trimws(input$patient_id)
    
    if (pid == "")
      errors <- c(errors, "- Patient ID is required.")
    if (pid != "" && !grepl("^P-[0-9]{4}$", pid))
      errors <- c(errors, "- Invalid ID format. Use 'P-XXXX' (e.g. P-1234).")
    if (is.na(input$age))
      errors <- c(errors, "- Age is required.")
    if (!is.na(input$age) && (input$age < 0 || input$age > 120))
      errors <- c(errors, "- Age must be between 0 and 120.")
    if (input$sex == "")
      errors <- c(errors, "- Sex is required.")
    if (!is.na(input$dosage) && input$dosage < 0)
      errors <- c(errors, "- Dosage cannot be negative.")
    
    # If there are errors, show them and stop
    if (length(errors) > 0) {
      status_text(paste("VALIDATION FAILED:\n", paste(errors, collapse = "\n")))
      showNotification("Please fix the errors in the form.", type = "error", duration = 5)
      return()
    }
    
    # Insert the new patient record into the database
    tryCatch({
      dbExecute(db_conn,
                "INSERT INTO patients
           (Patient_ID, Date_of_Birth, Age, Sex, Weight, Height,
            Blood_Type, Diagnosis_Code, Dosage_mg, Smoker, Doctor_Name)
         VALUES (?,?,?,?,?,?,?,?,?,?,?)",
                params = list(
                  pid,
                  as.character(input$dob),
                  input$age,
                  input$sex,
                  input$weight,
                  input$height,
                  input$blood_type,
                  input$diagnosis,
                  input$dosage,
                  input$smoker,
                  trimws(input$doctor_name)
                )
      )
      
      # Log the action in the audit table
      dbExecute(db_conn,
                "INSERT INTO audit_logs (patient_id, action, system_user, timestamp)
         VALUES (?,?,?,?)",
                params = list(pid, "INSERT_NEW_PATIENT",
                              Sys.info()[["user"]], as.character(Sys.time()))
      )
      
      status_text(paste("SUCCESS: Record for", pid, "saved."))
      showNotification("Record saved!", type = "message", duration = 3)
      
      # Clear the form after saving
      updateTextInput(session,    "patient_id",  value = "")
      updateNumericInput(session, "age",          value = NA)
      updateSelectInput(session,  "sex",          selected = "")
      updateNumericInput(session, "weight",       value = NA)
      updateNumericInput(session, "height",       value = NA)
      updateSelectInput(session,  "blood_type",   selected = "")
      updateTextInput(session,    "diagnosis",    value = "")
      updateNumericInput(session, "dosage",       value = NA)
      updateSelectInput(session,  "smoker",       selected = "")
      updateTextInput(session,    "doctor_name",  value = "")
      
    }, error = function(e) {
      status_text(paste("DATABASE ERROR:", e$message))
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })
  
  output$status_msg <- renderText({ status_text() })
  
  # Show last 10 audit entries after each submission
  output$audit_table <- renderTable({
    input$submit_btn
    dbGetQuery(db_conn,
               "SELECT patient_id, action, system_user, timestamp
       FROM audit_logs ORDER BY id DESC LIMIT 10")
  })

  # Tab 2: Quality Indicators
  
  
  # Reload data from DB when the user clicks Refresh
  db_data <- eventReactive(input$refresh_btn, {
    dbGetQuery(db_conn, "SELECT * FROM patients")
  }, ignoreNULL = FALSE)
  
  # Completeness: percentage of missing values per column
  output$plot_completeness <- renderPlot({
    df <- db_data()
    req(nrow(df) > 0)
    
    missing_pct <- sapply(df, function(x) sum(is.na(x)) / nrow(df) * 100)
    missing_df  <- data.frame(Variable    = names(missing_pct),
                              MissingPct  = missing_pct)
    
    ggplot(missing_df, aes(x = reorder(Variable, MissingPct), y = MissingPct,
                           fill = MissingPct > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                        guide = "none") +
      theme_minimal(base_size = 12) +
      labs(x = "Variable", y = "Missing (%)")
  })
  
  # Consistency: check if Sex has unexpected values
  output$text_consistency <- renderPrint({
    df <- db_data()
    if ("Sex" %in% colnames(df)) {
      print(table(df$Sex, useNA = "ifany"))
    } else {
      cat("Variable 'Sex' not found.")
    }
  })
  
  output$consistency_alert <- renderUI({
    df <- db_data()
    if (!"Sex" %in% colnames(df)) return(NULL)
    
    allowed <- c("Male", "Female", "Other", "m", "f", "M", "F")
    non_std  <- setdiff(na.omit(unique(df$Sex)), allowed)
    
    if (length(non_std) > 0) {
      div(style = "color:red; font-weight:bold;",
          paste("Non-standard values found:", paste(non_std, collapse = ", ")))
    } else {
      div(style = "color:green;", "All Sex values are standard.")
    }
  })
  
  # Accuracy: detect dosage outliers using IQR method
  output$plot_accuracy <- renderPlot({
    df <- db_data()
    req("Dosage_mg" %in% colnames(df))
    df$Dosage_mg <- suppressWarnings(as.numeric(df$Dosage_mg))
    
    ggplot(df, aes(y = Dosage_mg)) +
      geom_boxplot(fill = "tomato", color = "darkred",
                   outlier.color = "black", outlier.size = 3) +
      theme_minimal(base_size = 12) +
      labs(y = "Dosage (mg)", title = "Dosage Distribution") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  output$outlier_summary <- renderPrint({
    df <- db_data()
    if (!"Dosage_mg" %in% colnames(df)) { cat("Dosage_mg not found."); return() }
    
    d   <- suppressWarnings(as.numeric(df$Dosage_mg))
    q1  <- quantile(d, 0.25, na.rm = TRUE)
    q3  <- quantile(d, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    n_out <- sum(d < q1 - 1.5 * iqr | d > q3 + 1.5 * iqr, na.rm = TRUE)
    cat("Outliers detected (IQR method):", n_out, "\n")
    cat("Lower fence:", round(q1 - 1.5 * iqr, 2), "\n")
    cat("Upper fence:", round(q3 + 1.5 * iqr, 2), "\n")
  })
  
  # Generate and download HTML report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Quality_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      note_id <- showNotification("Generating report, please wait...",
                                  duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(note_id), add = TRUE)
      
      # Copy Rmd to a temp directory (rmarkdown needs write access)
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      current_data <- dbGetQuery(db_conn, "SELECT * FROM patients")
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params       = list(db_data = current_data),
        envir        = new.env(parent = globalenv())
      )
    }
  )
  
  # Tab 3: Data Visualization
  viz_data <- eventReactive(input$viz_refresh, {
    dbGetQuery(db_conn, "SELECT * FROM patients")
  }, ignoreNULL = FALSE)
  
  output$main_plot <- renderPlot({
    df <- viz_data()
    req(nrow(df) > 0)
    
    x_var    <- input$viz_x
    col_var  <- input$viz_color
    df[[x_var]]   <- suppressWarnings(as.numeric(df[[x_var]]))
    df[[col_var]] <- as.factor(df[[col_var]])
    
    if (input$viz_type == "Histogram") {
      ggplot(df, aes_string(x = x_var, fill = col_var)) +
        geom_histogram(bins = 30, alpha = 0.8, position = "stack") +
        theme_minimal(base_size = 13) +
        labs(title = paste("Histogram of", x_var, "by", col_var),
             x = x_var, y = "Count")
      
    } else if (input$viz_type == "Boxplot") {
      ggplot(df, aes_string(x = col_var, y = x_var, fill = col_var)) +
        geom_boxplot(outlier.colour = "red", outlier.size = 2) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Boxplot of", x_var, "by", col_var),
             x = col_var, y = x_var) +
        theme(legend.position = "none")
      
    } else {
      # Scatter: Age vs Dosage
      df$Dosage_mg <- suppressWarnings(as.numeric(df$Dosage_mg))
      ggplot(df, aes_string(x = "Age", y = "Dosage_mg", colour = col_var)) +
        geom_point(alpha = 0.6, size = 2.5) +
        geom_smooth(method = "lm", se = FALSE, colour = "grey40", linetype = "dashed") +
        theme_minimal(base_size = 13) +
        labs(title = "Age vs Dosage (mg)", x = "Age (years)", y = "Dosage (mg)")
    }
  })
  
  # Age distribution histogram
  output$age_dist <- renderPlot({
    df <- viz_data()
    req("Age" %in% colnames(df), nrow(df) > 0)
    df$Age <- suppressWarnings(as.numeric(df$Age))
    ggplot(df, aes(x = Age)) +
      geom_histogram(bins = 25, fill = "steelblue", colour = "white") +
      theme_minimal(base_size = 12) +
      labs(title = "Age Distribution", x = "Age (years)", y = "Count")
  })
  
  # Pie chart of Sex distribution
  output$sex_pie <- renderPlot({
    df <- viz_data()
    req("Sex" %in% colnames(df), nrow(df) > 0)
    
    sex_counts <- df %>%
      mutate(Sex = ifelse(is.na(Sex) | Sex == "", "Unknown", Sex)) %>%
      count(Sex)
    
    ggplot(sex_counts, aes(x = "", y = n, fill = Sex)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void(base_size = 12) +
      labs(title = "Sex Distribution")
  })
  
  # Tab 4: Audit Log
  output$full_audit_table <- renderTable({
    input$audit_refresh
    dbGetQuery(db_conn,
               "SELECT id, patient_id, action, system_user, timestamp
       FROM audit_logs ORDER BY id DESC LIMIT 100")
  })
}

# Run the app
shinyApp(ui = ui, server = server)