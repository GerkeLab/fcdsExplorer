fcds_app_server <- function(input, output, session) {
  
  
  # Cancer Type Filter Observers --------------------------------------------
  
  # morphology limited by selected histology
  observe({
    req(input$c_cancer_ICDO3_histology)
    
    s_morphology <- isolate(input$c_cancer_ICDO3_morphology)
    s_histology <- isolate(input$c_cancer_ICDO3_histology)
    
    regex_hist <- paste0("^(", fcds:::collapse(s_histology, "|"), ")")
    
    c_morphology <-
      c_opt$cancer_ICDO3_morphology %>%
      purrr::map(grep, pattern = regex_hist, value = TRUE) %>%
      keep(~ length(.) > 0)
    
    s_morphology <- intersect(s_morphology, unlist(c_morphology))
    
    updateSelectizeInput(
      session,
      "c_cancer_ICDO3_morphology",
      selected = s_morphology,
      choices = add_all(c_morphology)
    )
  })
  
  # reset morphology choices if histology choice is NULL
  observe({
    if (is.null(input$c_cancer_ICDO3_histology)) {
      if (is.null(isolate(input$c_cancer_ICDO3_morphology))) {
        updateSelectizeInput(
          session,
          "c_cancer_ICDO3_morphology",
          choices = add_all(c_opt$cancer_ICDO3_morphology)
        )
      }
    }
  })
  
  fcds_cancer_type <- reactive({
    # fcds:::cat_line("Re-calculating fcds_cancer_type()")
    
    # Get inputs with `c_cancer` prefix,
    # but exclude selectize inputs that get -selectize suffix
    c_filter_inputs <- paste0("c_", cancer_vars_include)
    c_filter_selected <- c_filter_inputs %>%
      set_names() %>%
      map(~ input[[.x]]) %>%
      compact() %>%
      discard(~ { length(.x) == 1 && .x %in% c("All", "") })
    
    
    fcds_cancer <- fcds_cancer_types
    if (length(c_filter_selected) > 0) {
      
      for (flt_var in names(c_filter_selected)) {
        fcds_var <- sub("^c_", "", flt_var)
        if (!fcds_var %in% names(fcds_cancer)) next
        if (fcds_var == "cancer_ICDO3_histology") {
          fcds_cancer <- filter_cancer_icdo3_histology(
            fcds_cancer, c_filter_selected[[flt_var]]
          )
        } else {
          fcds_cancer <- fcds_cancer[fcds_cancer[[fcds_var]] %in% c_filter_selected[[flt_var]], ]
        }
      }
    }
    
    fcds_cancer
  })
  
  output$c_filters_match <- renderUI({
    n_full <- sum(fcds_cancer_types$n)
    n_filtered <- sum(fcds_cancer_type()$n)
    
    tags$p(HTML(glue(
      "{tags$strong(format(n_filtered, big.mark = ','))} of ",
      "{tags$strong(format(n_full, big.mark = ','))} records match ",
      "selected cancer type filters."
    )))
  })
  
  
  fcds <- reactive({
    ret <- fcds_base %>%
      group_by(county_fips, county_name)
    
    if (isTruthy(input$m_year_group)) {
      ret <- ret %>% filter(year_group %in% input$m_year_group) %>%
        group_by(year_group, year, add = TRUE)
    }
    if (isTruthy(input$m_county_name)) {
      ret <- if (input$m_county_name == "moffitt_catchment") {
        ret %>% filter(county_name %in% fcds_const("moffitt")) %>%
          group_by(county_fips, county_name, add = TRUE)
      } else {
        ret %>% filter(county_name %in% input$m_county_name) %>%
          group_by(county_fips, county_name, add = TRUE)
      }
    }
    if (isTruthy(input$m_sex)) {
      ret <- ret %>% filter(sex %in% input$m_sex) %>%
        group_by(sex, add = TRUE)
    }
    if (isTruthy(input$m_race)) {
      ret <- ret %>% filter(race %in% input$m_race) %>%
        group_by(race, add = TRUE)
    }
    if (isTruthy(input$m_origin)) {
      ret <- ret %>% filter(origin %in% input$m_origin) %>%
        group_by(origin, add = TRUE)
    }
    if (isTruthy(input$m_cancer_site_group)) {
      ret <- ret %>% filter(cancer_site_group %in% input$m_cancer_site_group) %>%
        group_by(cancer_site_group, add = TRUE)
    }
    if (isTruthy(input$m_morphology)) {
      ret <- ret %>% filter(cancer_ICDO3_morphology %in% input$m_morphology) %>%
        group_by(cancer_ICDO3_morphology, add = TRUE)
    }
    if (isTruthy(input$m_age_group)) {
      ret <- ret %>% filter(age_group %in% input$m_age_group) %>%
        group_by(age_group, add = TRUE)
    }
    
    if (is.null(input$m_age_group)) {
      cat_ts("Age adjusting")
      ret <- ret %>%
        group_by(year_group, year, age_group, add = TRUE) %>%
        with_retain_groups(~ summarize(., n = sum(n))) %>%
        complete_age_groups() %>%
        age_adjust() %>%
        select(dplyr::matches("year"), dplyr::matches("county"), everything())
    } else {
      ret <- ret %>%
        summarize(n = sum(n))
    }
    
    ret
  })
  
  
  # Leaflet Map -------------------------------------------------------------
  
  # observe({
  #   cat_ts(fcds:::collapse(names(input)))
  # })
  
  fcds_df_map <- reactive({
    # req(input$fcds_page == "tab-explore")
    cat_ts("map: data prep")
    base_groups <- c("year", "year_group", "age_group", "county_name", "county_fips")
    
    fcds_df_map_pre <- fcds:::quiet_semi_join(
      fcds_base, fcds_cancer_type(), by = cancer_vars_include
    ) %>%
      group_by(!!!rlang::syms(base_groups))
    
    cat_ts("map: data future age-adjust")
    future({
      complete_age_groups(fcds_df_map_pre, county_name) %>%
        summarize(n = sum(n)) %>%
        age_adjust() %>%
        fcds:::join_boundaries_fl()
    })
  })
  
  base_map_exists <- reactiveVal(FALSE)
  output$explore_map <- renderLeaflet({
    cat_ts("map: base map")
    base_map_exists(TRUE)
    
    leaflet::leaflet() %>%
      fitBounds(-87.6349029541016, 31.0009689331055,
                -79.9743041992188, 24.3963069915771)
  })
  
  # Update data on the map
  observe({
    req(base_map_exists())
    cat_ts("map: fcds_map()")
    
    leaflet::leafletProxy("explore_map") %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls()
    
    fcds_df_map() %...>%
      fcds:::fcds_map_leaflet(palette = moffitt_red(5), proxy_id = "explore_map") %...>%
      leaflet::addLayersControl(
        baseGroups = rev(fcds_const("year_group")),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  })
  
  # switch map year group layer
  observe({
    req(input$m_year_group)
    leaflet::leafletProxy("explore_map") %>%
      leaflet::hideGroup(fcds_const("year_group")) %>%
      leaflet::showGroup(input$m_year_group)
  })
  
  map_last_clicked_county <- reactive({
    req(base_map_exists())
    req(input$explore_map_shape_click$lng)
    # input$explore_map_shape_click$id
    counties_clicked <- contains_point(fl_county_boundaries, input$explore_map_shape_click)
    if (nrow(counties_clicked)) {
      counties_clicked$county_fips[[1]]
    }
  })
  
  observe({
    req(map_last_clicked_county())
    cat_ts(map_last_clicked_county())
  })
  
  output$explore_table <- DT::renderDataTable({
    DT::datatable(
      fcds(),
      filter = "top",
      options = list(scrollX = TRUE)
    )
  }, server = TRUE)
  
  output$seer_icd_o_3 <- DT::renderDataTable({
    DT::datatable(fcds::seer_icd_o_3,
                  filter = "top")
  })
  
}
