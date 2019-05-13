library(shiny)

navbarPage(
  "FCDS",
  id = "fcds_page",
  inverse = TRUE,
  theme = "moffitt-cosmo.min.css",
  tabPanel(
    "Select Cancer Type",
    id = "select-cancer-type",
    value = "select-cancer-type",
    fluidRow(
      class = "container col-sm-12 col-lg-8 col-lg-offset-2 col-md-10 col-md-offset-1",
      tags$h2("Select Cancer Type"),
      helpText(
        "Choose filters for cancer type below. Remove or delete all groups to select default or all values.",
        uiOutput("c_filters_match")
      )
    ),
    fluidRow(
      class = "container col-sm-12 col-lg-8 col-lg-offset-2 col-md-10 col-md-offset-1",
      tags$h3("Cancer Site, Grade, and Status"),
      column(
        width = 4,
        select_multiple("c_cancer_site_group", "Site Group", choices = c_opt$cancer_site_group),
        helpText("FCDS Site Group")
      ),
      column(
        width = 4,
        select_multiple("c_cancer_grade", "Grade", choices = c_opt$cancer_grade),
        helpText("Grade, Differentiation, or Cell Lineage Indicator")
      ),
      column(
        width = 4,
        select_multiple("c_cancer_laterality", "Laterality", choices = c_opt$cancer_laterality),
        helpText("Laterality at Diagnosis")
      )
    ),
    fluidRow(
      class = "container col-sm-12 col-lg-8 col-lg-offset-2 col-md-10 col-md-offset-1",
      tags$h3("Histology, Behavior, and Morphology"),
      # cancer_ICDO3_histology, cancer_ICD03_behavior, cancer_ICDO3_morphology
      column(
        width = 4,
        select_multiple("c_cancer_ICDO3_histology", "Histology", choices = c_opt$cancer_ICDO3_histology),
        helpText("Histologic Type ICD-O-3")
      ),
      column(
        width = 4,
        select_multiple("c_cancer_ICDO3_behavior", "Behavior", choices = c_opt$cancer_ICDO3_behavior),
        helpText("Behavior Code ICD-O-3")
      ),
      column(
        width = 4,
        select_multiple("c_cancer_ICDO3_morphology", "Morphology", choices = c_opt$cancer_ICDO3_morphology),
        helpText("Morphology Code ICD-O-3 (Type and Behavior)")
      )
    ),
    fluidRow(
      class = "container col-sm-12 col-lg-8 col-lg-offset-2 col-md-10 col-md-offset-1",
      tags$h3("Reporting Source and Status"),
      # cancer_status, cancer_confirmation, cancer_reporting_source
      column(
        width = 4,
        select_multiple("c_cancer_reporting_source", "Reporting Source", choices = c_opt$cancer_reporting_source),
        helpText("Type of Reporting Source")
      ),
      column(
        width = 4,
        select_single("c_cancer_status", "Status", choice = c_opt$cancer_status),
        helpText("Cancer Status at time abstract was completed")
      ),
      column(
        width = 4,
        select_multiple("c_cancer_confirmation", "Confirmation", choice = c_opt$cancer_confirmation),
        helpText("Diagnostic Confirmation at first diagnosis")
      )
    )
  ),
  tabPanel(
    "Explore",
    id = "tab-explore",
    value = "tab-explore",
    fluidRow(
      class = "well",
      style = "margin-top: -21px;",
      column(
        width = 2,
        offset = 1,
        selectInput(
          "m_year_group", "Year",
          choices = opt_year_group,
          selected = "2011-2015"
        )
      ),
      column(
        width = 2,
        selectizeInput(
          "m_sex", "Sex",
          choices = opt_sex,
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        )
      ),
      column(
        width = 2,
        selectizeInput(
          "m_race", "Race",
          choices = opt_race,
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        )
      ),
      column(
        width = 2,
        selectizeInput(
          "m_origin", "Hispanic Origin",
          choices = opt_origin,
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        )
      ),
      column(
        width = 2,
        selectizeInput(
          "m_age_group", "Age Group",
          choices = opt_age_group,
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        )
      )
    ),
    fluidRow(
      tabsetPanel(
        tabPanel(
          "Map",
          tags$head(
            tags$style(HTML("#explore_map { height: calc(100vh - 235px) !important; }"))
          ),
          withSpinner(
            leafletOutput("explore_map", width = "100%"),
            type = 8, color = "#00589A", color.background = "#bec3c3"
          )
        ),
        tabPanel(
          "Table",
          tags$p(),
          DT::dataTableOutput("explore_table")
        )
      )
    )
  ),
  navbarMenu(
    "More",
    tabPanel(
      "ICD-O-3",
      id = "tab-icdo3",
      value = "tab-icdo3",
      DT::dataTableOutput("seer_icd_o_3")
    )
  )
)