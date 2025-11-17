# app.R ---------------------------------------------------------------
# Shiny app to check whether fish observations fall within
# expected MEOW realms based on MERMAID fish attributes

library(shiny)
library(tidyverse)
library(sf)
library(DT)
library(stringr)
library(mermaidr)
library(rlang)

# --- Global setup: MEOW realms + MERMAID fish attributes -------------

# Turn off s2 to avoid edge-crossing errors (same as in your HTML doc)
sf::sf_use_s2(FALSE)

# Load precomputed MEOW realms
realm_only <- readRDS("data/realm_only.rds")

# Get fish attribute data from MERMAID (species + regions)
mermaidFishSppTBL <- mermaidr::mermaid_get_reference("fishspecies")

# --- UI --------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Check fish observation realms vs MERMAID ranges"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "obs_file",
        "Upload fish observations CSV",
        accept = c(".csv")
      ),
      helpText(
        "Your CSV should include at least:",
        tags$ul(
          tags$li("A fish name / species column"),
          tags$li("Latitude"),
          tags$li("Longitude")
        )
      ),
      hr(),
      uiOutput("column_ui"),   # dynamic selectors for columns
      hr(),
      actionButton("run_checks", "Run realm checks"),
      br(), br(),
      downloadButton("download_results", "Download results CSV")
    ),
    mainPanel(
      h4("Preview of input data"),
      DTOutput("obs_preview"),
      hr(),
      h4("Realm check results"),
      DTOutput("results_table")
    )
  )
)

# --- Server ----------------------------------------------------------

server <- function(input, output, session) {
  
  # Read uploaded data -----------------------------------------------
  obs_data <- reactive({
    req(input$obs_file)
    readr::read_csv(input$obs_file$datapath, show_col_types = FALSE)
  })
  
  output$obs_preview <- renderDT({
    req(obs_data())
    DT::datatable(head(obs_data(), 20))
  })
  
  # Dynamic UI: let the user map columns ------------------------------
  output$column_ui <- renderUI({
    req(obs_data())
    cols <- names(obs_data())
    
    # Cheap heuristics for defaults
    guess_fish <- cols[
      which.max(stringr::str_detect(tolower(cols), "fish|species|name"))
    ]
    guess_lat  <- cols[
      which.max(stringr::str_detect(tolower(cols), "lat"))
    ]
    guess_lon  <- cols[
      which.max(stringr::str_detect(tolower(cols), "lon|long"))
    ]
    
    tagList(
      selectInput(
        "fish_col",
        "Column with fish name / taxon",
        choices = cols,
        selected = if (!is.na(guess_fish)) guess_fish else cols[1]
      ),
      selectInput(
        "lat_col",
        "Latitude column",
        choices = cols,
        selected = if (!is.na(guess_lat)) guess_lat else cols[2]
      ),
      selectInput(
        "lon_col",
        "Longitude column",
        choices = cols,
        selected = if (!is.na(guess_lon)) guess_lon else cols[3]
      )
    )
  })
  
  # Main realm-check pipeline, triggered by button -------------------
  results <- eventReactive(input$run_checks, {
    df <- obs_data()
    req(input$fish_col, input$lat_col, input$lon_col)
    
    # Standardize column names to match your original script
    obs_df <- df %>%
      dplyr::rename(
        `Fish name` = !!rlang::sym(input$fish_col),
        Latitude    = !!rlang::sym(input$lat_col),
        Longitude   = !!rlang::sym(input$lon_col)
      ) %>%
      dplyr::select(`Fish name`, Latitude, Longitude, dplyr::everything())
    
    # Drop rows with missing coordinates
    obs_df <- obs_df %>%
      dplyr::filter(!is.na(Latitude), !is.na(Longitude))
    
    validate(
      need(nrow(obs_df) > 0, "No rows with non-missing Latitude and Longitude.")
    )
    
    # Convert to sf (coords = c(x = Longitude, y = Latitude))
    fish_sf <- obs_df %>%
      sf::st_as_sf(
        coords  = c("Longitude", "Latitude"),
        crs     = 4326,
        remove  = FALSE
      )
    
    # Spatial join: assign realm to each point
    fish_with_realm <- sf::st_join(fish_sf, realm_only) %>%
      sf::st_drop_geometry() %>%
      dplyr::rename(realm_obs = realm)
    
    # Merge with MERMAID fish attribute realms
    fish_check_realm <- fish_with_realm %>%
      dplyr::left_join(
        mermaidFishSppTBL %>%
          dplyr::select(species, regions) %>%
          dplyr::rename(
            `Fish name` = species,
            realm_exp   = regions
          ),
        by = "Fish name"
      ) %>%
      # Does observed realm appear in expected realms string?
      dplyr::mutate(
        match_realm = dplyr::case_when(
          is.na(realm_obs) | is.na(realm_exp) ~ NA_character_,
          stringr::str_detect(realm_exp, fixed(realm_obs)) ~ "yes",
          TRUE ~ "no"
        )
      )
    
    fish_check_realm
  })
  
  # Show results table ------------------------------------------------
  output$results_table <- renderDT({
    req(results())
    DT::datatable(
      results() %>%
        dplyr::select(
          `Fish name`,
          Latitude,
          Longitude,
          realm_obs,
          realm_exp,
          match_realm
        ),
      options = list(pageLength = 20)
    )
  })
  
  # Download handler --------------------------------------------------
  output$download_results <- downloadHandler(
    filename = function() {
      if (is.null(input$obs_file)) {
        "fish_realm_check.csv"
      } else {
        paste0(
          tools::file_path_sans_ext(input$obs_file$name),
          "_realm_check.csv"
        )
      }
    },
    content = function(file) {
      readr::write_csv(results(), file)
    }
  )
}

shinyApp(ui, server)
