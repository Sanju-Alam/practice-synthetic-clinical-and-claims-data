library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(here)

# Load app data
kpi_overview <- readr::read_csv(here("data", "appdata", "kpi_overview.csv"), show_col_types = FALSE)
cohort_summary <- readr::read_csv(here("data", "appdata", "cohort_summary.csv"), show_col_types = FALSE)
utilization_by_type <- readr::read_csv(here("data", "appdata", "utilization_by_type.csv"), show_col_types = FALSE)
payment_by_type <- readr::read_csv(here("data", "appdata", "payment_by_type.csv"), show_col_types = FALSE)
top_dx <- readr::read_csv(here("data", "appdata", "top_dx.csv"), show_col_types = FALSE)
patient_drilldown_sample <- readr::read_csv(here("data", "appdata", "patient_drilldown_sample.csv"), show_col_types = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Synthetic Claims Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Cohort Explorer", tabName = "cohort"),
      menuItem("Payments", tabName = "payments"),
      menuItem("Patient Drilldown", tabName = "patient")
    ),
    hr(),
    selectInput("year", "Year", choices = c("All", sort(unique(cohort_summary$file_year))), selected = "All"),
    selectInput("sex", "Sex", choices = c("All", sort(unique(cohort_summary$sex))), selected = "All"),
    selectInput("age_group", "Age group", choices = c("All", sort(unique(cohort_summary$age_group))), selected = "All")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb1"),
          valueBoxOutput("vb2"),
          valueBoxOutput("vb3")
        ),
        fluidRow(
          box(width = 6, title = "Top diagnosis codes", DTOutput("dx_table")),
          box(width = 6, title = "Project note",
              p("This dashboard uses CMS synthetic Medicare claims data processed into smaller app-ready summary files.")
          )
        )
      ),
      tabItem(
        tabName = "cohort",
        fluidRow(
          box(width = 12, title = "Cohort summary", plotlyOutput("cohort_plot")),
          box(width = 12, title = "Utilization by claim type", plotlyOutput("util_plot"))
        )
      ),
      tabItem(
        tabName = "payments",
        fluidRow(
          box(width = 12, title = "Mean payment by claim type", plotlyOutput("payment_plot"))
        )
      ),
      tabItem(
        tabName = "patient",
        fluidRow(
          box(width = 4, title = "Select beneficiary",
              selectInput("patient_id", "Beneficiary ID", choices = sort(unique(patient_drilldown_sample$desynpuf_id)))
          ),
          box(width = 8, title = "Patient event history", DTOutput("patient_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_cohort <- reactive({
    df <- cohort_summary
    if (input$year != "All") df <- df |> filter(file_year == as.numeric(input$year))
    if (input$sex != "All") df <- df |> filter(sex == input$sex)
    if (input$age_group != "All") df <- df |> filter(age_group == input$age_group)
    df
  })
  
  filtered_util <- reactive({
    df <- utilization_by_type
    if (input$year != "All") df <- df |> filter(file_year == as.numeric(input$year))
    if (input$sex != "All") df <- df |> filter(sex == input$sex)
    if (input$age_group != "All") df <- df |> filter(age_group == input$age_group)
    df
  })
  
  filtered_pay <- reactive({
    df <- payment_by_type
    if (input$year != "All") df <- df |> filter(file_year == as.numeric(input$year))
    if (input$sex != "All") df <- df |> filter(sex == input$sex)
    if (input$age_group != "All") df <- df |> filter(age_group == input$age_group)
    df
  })
  
  output$vb1 <- renderValueBox({
    valueBox(
      value = kpi_overview$value[kpi_overview$metric == "Beneficiaries"],
      subtitle = "Beneficiaries",
      icon = icon("users")
    )
  })
  
  output$vb2 <- renderValueBox({
    valueBox(
      value = kpi_overview$value[kpi_overview$metric == "Inpatient claims"],
      subtitle = "Inpatient claims",
      icon = icon("hospital")
    )
  })
  
  output$vb3 <- renderValueBox({
    valueBox(
      value = kpi_overview$value[kpi_overview$metric == "Prescription drug events"],
      subtitle = "PDE events",
      icon = icon("pills")
    )
  })
  
  output$dx_table <- renderDT({
    datatable(top_dx, options = list(pageLength = 10))
  })
  
  output$cohort_plot <- renderPlotly({
    p <- filtered_cohort() |>
      group_by(age_group) |>
      summarise(beneficiaries = sum(beneficiaries), .groups = "drop") |>
      ggplot(aes(x = age_group, y = beneficiaries)) +
      geom_col() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Beneficiaries")
    ggplotly(p)
  })
  
  output$util_plot <- renderPlotly({
    p <- filtered_util() |>
      group_by(claim_type) |>
      summarise(total_events = sum(total_events), .groups = "drop") |>
      ggplot(aes(x = claim_type, y = total_events)) +
      geom_col() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Total events")
    ggplotly(p)
  })
  
  output$payment_plot <- renderPlotly({
    p <- filtered_pay() |>
      group_by(claim_type) |>
      summarise(mean_payment = mean(mean_payment, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = claim_type, y = mean_payment)) +
      geom_col() +
      scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Mean payment")
    ggplotly(p)
  })
  
  output$patient_table <- renderDT({
    patient_drilldown_sample |>
      filter(desynpuf_id == input$patient_id) |>
      datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui, server)