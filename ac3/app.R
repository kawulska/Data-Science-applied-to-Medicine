library(shiny)
library(DT)
library(jsonlite)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)

# 1. LOAD AND PROCESS DATA 

load_local_data <- function(dir_path = "json_data") {
  files <- list.files(dir_path, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0) return(list())
  lapply(files, function(f) fromJSON(f, simplifyVector = FALSE))
}

process_data <- function(raw_list) {
  if (length(raw_list) == 0) return(data.frame())
  
  safe_extract <- function(item, key, default = NA) {
    val <- item[[key]]
    if (is.null(val)) return(default)
    return(as.character(val)[1])
  }
  
  df_list <- lapply(raw_list, function(raw) {
    start_dt <- ymd_hms(safe_extract(raw, "startTime"), quiet = TRUE)
    end_dt <- ymd_hms(safe_extract(raw, "endTime"), quiet = TRUE)
    
    size_gb <- 0; sha_val <- ""; seqfu_val <- ""
    if (!is.null(raw$generated) && is.list(raw$generated)) {
      for (item in raw$generated) {
        if (is.list(item) && !is.null(item$label)) {
          if (item$label == "FASTQ Files" && !is.null(item$totalSizeBytes)) size_gb <- round(as.numeric(item$totalSizeBytes) / 1e9, 2)
          if (item$label == "Verificació SHA256" && !is.null(item$value)) sha_val <- as.character(item$value)[1]
          if (item$label == "Verificació Seqfu" && !is.null(item$value)) seqfu_val <- as.character(item$value)[1]
        }
      }
    }
    
    sha_ok <- !grepl("NO coincide|FAIL|MISMATCH", sha_val, ignore.case = TRUE) && nzchar(sha_val)
    seq_ok <- grepl("^OK", trimws(seqfu_val), ignore.case = TRUE)
    
    data.frame(
      id = safe_extract(raw, "@id"),
      sample = safe_extract(raw, "label"),
      node = safe_extract(raw, "executionNode"),
      startTime = start_dt,
      duration = as.numeric(round(as.numeric(difftime(end_dt, start_dt, units = "mins")), 1)),
      size_gb = as.numeric(size_gb),
      status = ifelse(sha_ok && seq_ok, "PASS", "FAIL"),
      stringsAsFactors = FALSE
    )
  })
  bind_rows(df_list)
}

raw_data_list <- load_local_data("json_data")
clean_df <- process_data(raw_data_list)

# 2. USER INTERFACE (UI) 

ui <- fluidPage(
  titlePanel("Genomic Provenance Monitor"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      selectInput("node_f", "Execution Node:", choices = c("All", unique(clean_df$node[!is.na(clean_df$node)]))),
      selectInput("status_f", "Integrity Status:", choices = c("All", "PASS", "FAIL")),
      actionButton("reset", "Reset Filters", class = "btn-block"),
      hr(),
      h4("Current View Stats"),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      width = 9,
      fluidRow(
        column(6, plotlyOutput("line_throughput", height = "300px")),
        column(6, plotOutput("bar_time_static", height = "300px")) 
      ),
      hr(),
      h4("Activity Log (Click a row to view JSON)"),
      DTOutput("dt_table"),
      hr(),
      h4("JSON Schema Viewer"),
      verbatimTextOutput("json_view")
    )
  )
)

# 3. SERVER LOGIC 

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    data <- clean_df
    if (nrow(data) == 0) return(data)
    if (input$node_f != "All") data <- data[data$node == input$node_f, ]
    if (input$status_f != "All") data <- data[data$status == input$status_f, ]
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "node_f", selected = "All")
    updateSelectInput(session, "status_f", selected = "All")
  })
  
  output$summary_stats <- renderText({
    df <- filtered_df()
    if(nrow(df) == 0) return("No data available.")
    paste0(
      "Records Shown: ", nrow(df), "\n",
      "System Health: ", round(mean(df$status == "PASS", na.rm=TRUE) * 100, 1), "% PASS\n",
      "Total Data: ", sum(df$size_gb, na.rm = TRUE), " GB\n",
      "Avg Time: ", round(mean(df$duration, na.rm = TRUE), 1), " min"
    )
  })
 
  output$line_throughput <- renderPlotly({
    df <- filtered_df()
    if(nrow(df) == 0) return(plotly_empty() %>% layout(title="No Data"))
    flow <- df %>% filter(!is.na(startTime)) %>% mutate(date = as.Date(startTime)) %>% 
      group_by(date) %>% summarise(total_gb = sum(size_gb, na.rm = TRUE), .groups="drop")
    plot_ly(flow, x = ~date, y = ~total_gb, type = "scatter", mode = "lines+markers",
            line = list(color = "#5cb85c"), marker = list(color = "#4cae4c")) %>% 
      layout(title = "Daily Throughput (GB)", xaxis = list(title = "Date"), yaxis = list(title = "GB"))
  })
  
  output$bar_time_static <- renderPlot({
    df <- filtered_df()
    if(nrow(df) == 0) return(NULL)
    
    stats <- df %>% 
      filter(!is.na(node)) %>% 
      group_by(node) %>% 
      summarise(avg = mean(duration, na.rm = TRUE), .groups = "drop")
    
    ggplot(stats, aes(x = reorder(node, avg), y = avg)) +
      geom_bar(stat = "identity", fill = "#337ab7") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Avg Processing Time per Node",
           x = "Execution Node",
           y = "Average Minutes") +
      theme(plot.title = element_text(face = "bold", size = 14))
  })
  
  output$dt_table <- renderDT({
    df <- filtered_df()
    cols <- c("sample", "startTime", "node", "duration", "size_gb", "status")
    datatable(df[, intersect(cols, names(df)), drop = FALSE], 
              selection = "single", options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$json_view <- renderText({
    s <- input$dt_table_rows_selected
    df <- filtered_df()
    if (length(s) > 0 && nrow(df) >= s) {
      record <- Filter(function(x) x$`@id` == df$id[s], raw_data_list)
      if (length(record) > 0) toJSON(record[[1]], pretty = TRUE, auto_unbox = TRUE) else "Error loading JSON."
    } else {
      "Select a row in the table above to view JSON."
    }
  })
}

shinyApp(ui, server)