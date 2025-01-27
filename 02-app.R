library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

# Generate sample ADAE data
set.seed(123)
n_subjects <- 100
TRT01A <- c("Drug A", "Placebo")
AESEV <- c("Mild", "Moderate", "Severe")
AESEQ <- c("Headache", "Nausea", "Dizziness", "Fatigue", "Rash")

# First, create a vector of how many AEs each subject will have
ae_counts <- sample(1:3, n_subjects, replace = TRUE)
total_records <- sum(ae_counts)

adae_data <- data.frame(
  USUBJID = rep(paste0("SUBJ", sprintf("%03d", 1:n_subjects)), ae_counts),
  TRT01P = sample(TRT01A, total_records, replace = TRUE),
  AEDECOD = sample(AESEQ, total_records, replace = TRUE),
  AESEV = sample(AESEV, total_records, replace = TRUE, 
                prob = c(0.6, 0.3, 0.1)),
  ASTDY = sample(1:100, total_records, replace = TRUE),
  AESER = sample(c("Y", "N"), total_records, replace = TRUE, 
                prob = c(0.1, 0.9))
)

ui <- page_sidebar(
  title = "Adverse Event Analysis",
  sidebar = sidebar(
    selectInput("analysis_type", "Select Analysis",
                choices = c("AE Summary Table", 
                          "AE by Severity",
                          "Time to Onset Analysis")),
    checkboxGroupInput("severity_filter", "Filter by Severity",
                      choices = AESEV,
                      selected = AESEV),
    checkboxInput("serious_only", "Show Only Serious AEs", FALSE)
  ),
  
  # Main panel with cards
  card(
    card_header("Analysis Results"),
    card_body(
      conditionalPanel(
        condition = "input.analysis_type == 'AE Summary Table'",
        DTOutput("ae_table")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'AE by Severity'",
        plotOutput("severity_plot")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Time to Onset Analysis'",
        plotOutput("onset_plot")
      )
    )
  )
)

server <- function(input, output) {
  
  # Filtered dataset based on user inputs
  filtered_data <- reactive({
    data <- adae_data
    
    if (!is.null(input$severity_filter)) {
      data <- data %>% filter(AESEV %in% input$severity_filter)
    }
    
    if (input$serious_only) {
      data <- data %>% filter(AESER == "Y")
    }
    
    data
  })
  
  # AE Summary Table
  output$ae_table <- renderDT({
    filtered_data() %>%
      group_by(AEDECOD, TRT01P) %>%
      summarise(
        n = n(),
        n_serious = sum(AESER == "Y"),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = TRT01P,
        values_from = c(n, n_serious),
        values_fill = 0
      ) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # AE by Severity Plot
  output$severity_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = AEDECOD, fill = AESEV)) +
      geom_bar(position = "dodge") +
      facet_wrap(~TRT01P) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Adverse Event", y = "Count", fill = "Severity") +
      scale_fill_brewer(palette = "YlOrRd")
  })
  
  # Time to Onset Plot
  output$onset_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = ASTDY, fill = TRT01P)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~AEDECOD) +
      theme_minimal() +
      labs(x = "Study Day of Onset", y = "Density", fill = "Treatment") +
      scale_fill_brewer(palette = "Set2")
  })
}

shinyApp(ui, server)
