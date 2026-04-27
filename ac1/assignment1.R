library(shiny)
library(ggplot2)
library(dplyr)


titanic_df <- as.data.frame(Titanic)
titanic_df$Freq <- as.integer(titanic_df$Freq)

ui <- fluidPage(
  
  titlePanel("Titanic Passenger Survival Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Filter Dataset"),
      
      selectInput("class_input", "Select Passenger Class:", 
                  choices = unique(titanic_df$Class), 
                  selected = "1st"),
      
      radioButtons("age_input", "Age Category:", 
                   choices = unique(titanic_df$Age), 
                   selected = "Adult"),
      
      hr()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Survival Plot", 
                 plotOutput("survivalPlot")),
        tabPanel("Data Summary", 
                 tableOutput("summaryTable"))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    titanic_df %>%
      filter(Class == input$class_input,
             Age == input$age_input)
  })
  
  output$survivalPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Sex, y = Freq, fill = Survived)) +
      geom_bar(stat = "identity", position = "fill") + 
      scale_fill_manual(values = c("No" = "red", "Yes" = "green")) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste("Percentage of Survivors", input$age_input, "Class:", input$class_input),
           x = "Sex", 
           y = "Percentage") +
      theme_minimal()
  })
  
  output$summaryTable <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
