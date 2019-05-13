library(shiny)
library(shinycssloaders)
library(promises)
library(future)
library(leaflet)
library(DT)
library(dplyr)
library(purrr)
library(glue)
library(fcds)

future::plan(multiprocess)


# Setup -------------------------------------------------------------------

cancer_vars_ignore <- c("cancer_site_specific", "cancer_ICDO3_conversion")
cancer_vars_include <- setdiff(fcds_vars("cancer"), cancer_vars_ignore)

if (!exists("fcds_base")) {
  fcds_shiny_cache <- file.path(fcds_default_data_path(), "fcds-shiny.rds")
  if (!file.exists(fcds_shiny_cache)) {
    fcds_base <- fcds_load() %>%
      mutate(cancer_ICDO3_histology = sub("\\d$", "", cancer_ICDO3_histology)) %>%
      group_by(year_group, year, county_name, county_fips, age_group, sex, race, origin,
               !!!rlang::syms(cancer_vars_include)) %>%
      count_fcds() %>%
      ungroup()
    
    message("Caching base shiny data to ", fcds_shiny_cache)
    saveRDS(fcds_base, fcds_shiny_cache, compress = FALSE)
  } else {
    fcds_base <- readRDS(fcds_shiny_cache)
  }
}


add_all <- function(x, ...) UseMethod("add_all")
add_all.character <- function(x, all = "") c("All" = all, x)
add_all.list <- function(x, all = "") c(list("All" = all), x)
add_all.NULL <- function(x, ...) NULL

# Initial Cancer Selection Options ----
c_opt_vars <- setdiff(fcds_vars("cancer"), cancer_vars_ignore)

fcds_cancer_types <- fcds_base %>%
  select(c_opt_vars, n) %>%
  group_by(!!!rlang::syms(c_opt_vars)) %>%
  summarize(n = sum(n)) %>%
  ungroup()

c_opt <- c_opt_vars %>%
  set_names() %>%
  map(possibly(fcds_const, NULL)) %>%
  map(sort)

c_opt$cancer_ICDO3_morphology <-
  fcds::seer_icd_o_3 %>%
  distinct(histology_description, morphology, morphology_description) %>%
  mutate_at(quos(starts_with("morphology")), as.character) %>%
  split(.$histology_description) %>%
  map(~ {
    .x$morphology_description <- glue_data(.x, "{morphology} - {morphology_description}")
    .x <- .x %>% arrange(morphology_description)
    set_names(.x$morphology, paste(.x$morphology_description))
  })

c_opt$cancer_ICDO3_histology <-
  fcds::seer_icd_o_3 %>%
  mutate_all(as.character) %>%
  mutate(histology = sub("\\d$", "", histology)) %>%
  distinct(histology, histology_description) %>%
  mutate(choice = glue("{histology} {histology_description}")) %>%
  arrange(choice) %>%
  {set_names(.$histology, .$choice)}


filter_cancer_icdo3_histology <- function(data, values = NULL) {
  if (is.null(values)) return(values)
  values <- paste0("^(", paste(values, collapse = "|"), ")")
  data[grepl(values, data$cancer_ICDO3_histology), ]
}

# Initial Demographic Exploration Options ----

opt_year_group <- fcds_const("year_group") %>% rev() %>% add_all()

opt_cancer_site_group <- fcds_const("cancer_site_group", full = TRUE) %>%
  { set_names(.$label, glue_data(., "{label} ({value})")) } %>%
  sort() %>%
  add_all()

opt_sex <- fcds_const("sex") %>% add_all()
opt_race <- fcds_const("race") %>% add_all()
opt_age_group <- c("Age-Adjusted" = "", fcds_const("age_group"))
opt_origin <- fcds_const("origin") %>% add_all()
opt_county_name <- c("Moffitt Catchment Area" = "moffitt_catchment",
                     fcds_const("county_name")) %>% add_all()


# Helpers -----------------------------------------------------------------

select_multiple <- function(choices, ...) {
  choices <- add_all(choices, all = "")
  selectizeInput(..., choices = choices, multiple = TRUE, options = list(plugins = list('remove_button')))
}

select_single <- function(choices, ...) {
  choices <- add_all(choices, all = "All")
  selectizeInput(..., choices = choices, multiple = FALSE)
}

cat_ts <- function(...) {
  cat("\n", strftime(Sys.time(), "[%F %T] "), ..., sep = "")
}

fl_county_boundaries <- fcds::county_fips_fl %>%
  fcds:::join_boundaries_fl()


points2sf <- function(lng, lat) {
  map2(lng, lat, ~ sf::st_point(c(.x, .y))) %>%
    sf::st_sfc(crs = 4326)
}

contains_point <- function(data, pt) {
  if (is.null(pt)) return(data[0, ])
  
  quiet_st_within <- fcds:::quietly(sf::st_within)
  
  rows_containing <-
    quiet_st_within(points2sf(pt$lng, pt$lat), data) %>%
    map(as.numeric) %>%
    reduce(union)
  
  data[rows_containing, ]
}

moffitt_blue <- function(n = 3) {
  colorspace::sequential_hcl(n, 250, 64, c(37, 90), power = 1, rev = TRUE)
}

moffitt_red <- function(n = 3) {
  colorspace::sequential_hcl(n, 4.350477, 142.1399, c(50.48059, 90), power = 1, rev = TRUE)
}
