library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(viridis)
library(forestplot)
library(scales)

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
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    selectInput("analysis_type", "Select Analysis",
                choices = c("AE Summary Table", 
                          "AE by Severity",
                          "Time to Onset Analysis",
                          "AE Heatmap",
                          "Risk Ratio Forest Plot")),
    checkboxGroupInput("severity_filter", "Filter by Severity",
                      choices = AESEV,
                      selected = AESEV),
    checkboxInput("serious_only", "Show Only Serious AEs", FALSE)
  ),
  
  # Main panel with cards
  layout_columns(
    col_widths = 12,
    card(
      card_header("Analysis Results"),
      card_body(
        conditionalPanel(
          condition = "input.analysis_type == 'AE Summary Table'",
          DTOutput("ae_table")
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'AE by Severity'",
          plotOutput("severity_plot", height = "600px")
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'Time to Onset Analysis'",
          plotOutput("onset_plot", height = "600px")
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'AE Heatmap'",
          plotOutput("heatmap_plot", height = "600px")
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'Risk Ratio Forest Plot'",
          plotOutput("forest_plot", height = "600px")
        )
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
  
  # Enhanced AE by Severity Plot
  output$severity_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = AEDECOD, fill = AESEV)) +
      geom_bar(position = "fill") +
      facet_wrap(~TRT01P) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        title = "Adverse Event Severity Distribution by Treatment",
        x = "Adverse Event",
        y = "Proportion",
        fill = "Severity"
      ) +
      scale_fill_viridis_d() +
      scale_y_continuous(labels = percent)
  })
  
  # Enhanced Time to Onset Plot
  output$onset_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = ASTDY, fill = TRT01P)) +
      geom_density(alpha = 0.7) +
      facet_wrap(~AEDECOD) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        title = "Time to Onset Distribution by Adverse Event",
        x = "Study Day of Onset",
        y = "Density",
        fill = "Treatment"
      ) +
      scale_fill_viridis_d()
  })
  
  # New Heatmap Plot
  output$heatmap_plot <- renderPlot({
    event_counts <- filtered_data() %>%
      group_by(AEDECOD, TRT01P, AESEV) %>%
      summarise(count = n(), .groups = "drop")
    
    ggplot(event_counts, aes(x = AEDECOD, y = AESEV, fill = count)) +
      geom_tile() +
      facet_wrap(~TRT01P) +
      scale_fill_viridis() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      labs(
        title = "Adverse Event Frequency Heatmap",
        x = "Adverse Event",
        y = "Severity",
        fill = "Count"
      )
  })
  
  # New Forest Plot
  output$forest_plot <- renderPlot({
    # Calculate risk ratios
    risk_ratios <- filtered_data() %>%
      group_by(AEDECOD, TRT01P) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = TRT01P, values_from = n, values_fill = 0) %>%
      mutate(
        risk_ratio = (`Drug A` / sum(`Drug A`)) / (Placebo / sum(Placebo)),
        log_rr = log(risk_ratio),
        se = sqrt(1/`Drug A` + 1/Placebo),
        lower = exp(log_rr - 1.96 * se),
        upper = exp(log_rr + 1.96 * se)
      )
    
    ggplot(risk_ratios, aes(y = reorder(AEDECOD, risk_ratio))) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
      geom_point(aes(x = risk_ratio), size = 3, color = "darkblue") +
      scale_x_log10() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      labs(
        title = "Risk Ratios of Adverse Events (Drug A vs Placebo)",
        x = "Risk Ratio (log scale)",
        y = "Adverse Event"
      )
  })
}

shinyApp(ui, server)
