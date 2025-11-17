# app.R ---------------------------------------------------------------
# Shiny app to check whether fish observations fall within
# expected MEOW realms based on MERMAID fish attributes,
# with Leaflet map + progress + status label.

library(shiny)
library(tidyverse)
library(sf)
library(DT)
library(stringr)
library(mermaidr)
library(rlang)
library(leaflet)

# --- Global setup: MEOW realms + MERMAID fish attributes -------------

# Turn off s2 to avoid edge-crossing errors
sf::sf_use_s2(FALSE)

# Load precomputed MEOW realms
realm_only <- readRDS("data/realm_only.rds")

# Make sure they're in WGS84 lon/lat
if (is.na(sf::st_crs(realm_only))) {
  sf::st_crs(realm_only) <- 4326
} else {
  realm_only <- sf::st_transform(realm_only, 4326)
}

# Get fish attribute data from MERMAID (species + regions)
# (you can replace this with readRDS() if you cache it)
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
      downloadButton("download_results", "Download results CSV"),
      br(), br(),
      strong("Status:"),
      textOutput("run_status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Input preview",
          h4("Preview of input data"),
          DTOutput("obs_preview")
        ),
        tabPanel(
          "Results table",
          h4("Realm check results"),
          DTOutput("results_table")
        ),
        tabPanel(
          "Map",
          h4("Map of observations and realms"),
          leafletOutput("map", height = 600)
        )
      )
    )
  )
)

# --- Server ----------------------------------------------------------

server <- function(input, output, session) {
  
  # Persistent status message
  run_status <- reactiveVal("Realm checks not run yet.")
  
  output$run_status <- renderText({
    run_status()
  })
  
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
    
    withProgress(message = "Running realm checks...", value = 0, {
      
      # 1. Read + prepare data
      df <- obs_data()
      incProgress(0.2, detail = "Reading uploaded data...")
      req(input$fish_col, input$lat_col, input$lon_col)
      
      obs_df <- df %>%
        dplyr::rename(
          `Fish name` = !!rlang::sym(input$fish_col),
          Latitude    = !!rlang::sym(input$lat_col),
          Longitude   = !!rlang::sym(input$lon_col)
        ) %>%
        dplyr::select(`Fish name`, Latitude, Longitude, dplyr::everything()) %>%
        dplyr::filter(!is.na(Latitude), !is.na(Longitude))
      
      validate(
        need(nrow(obs_df) > 0, "No rows with non-missing Latitude and Longitude.")
      )
      
      incProgress(0.4, detail = "Converting to sf and joining realms...")
      
      # 2. SF + realm join
      fish_sf <- obs_df %>%
        sf::st_as_sf(
          coords  = c("Longitude", "Latitude"),
          crs     = 4326,
          remove  = FALSE
        )
      
      fish_with_realm <- sf::st_join(fish_sf, realm_only) %>%
        sf::st_drop_geometry() %>%
        dplyr::rename(realm_obs = realm)
      
      incProgress(0.7, detail = "Joining with MERMAID species table...")
      
      # 3. MERMAID join + match flag
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
        dplyr::mutate(
          match_realm = dplyr::case_when(
            is.na(realm_obs) | is.na(realm_exp) ~ NA_character_,
            stringr::str_detect(realm_exp, fixed(realm_obs)) ~ "yes",
            TRUE ~ "no"
          )
        )
      
      incProgress(1, detail = "Done.")
      
      # Update persistent status text
      run_status(
        paste0(
          "Realm checks finished for ",
          nrow(fish_check_realm),
          " rows at ",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )
      
      # Optional pop-up notification
      showNotification(
        paste("Realm checks finished for", nrow(fish_check_realm), "rows."),
        type = "message",
        duration = 5
      )
      
      fish_check_realm
    })
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
  
  # Leaflet map -------------------------------------------------------
  
  # Initial empty map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # Update map whenever results() changes
  observeEvent(results(), {
    res <- results()
    
    # Points sf
    pts_sf <- res %>%
      dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>%
      sf::st_as_sf(
        coords = c("Longitude", "Latitude"),
        crs    = 4326,
        remove = FALSE
      )
    
    # Palette for match_realm
    pal <- colorFactor(
      palette = c("green", "red", "gray50"),
      domain  = c("yes", "no"),
      na.color = "gray50"
    )
    
    m <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      # Realms polygons
      addPolygons(
        data = realm_only,
        group = "Realms",
        weight = 0.5,
        fillOpacity = 0.2,
        color = "blue",
        popup = ~realm
      ) %>%
      # Observation points
      addCircleMarkers(
        data = pts_sf,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.8,
        color = ~pal(match_realm),
        popup = ~paste0(
          "<b>", `Fish name`, "</b><br/>",
          "Realm (obs): ", realm_obs, "<br/>",
          "Realm (MERMAID): ", realm_exp, "<br/>",
          "Match: ", match_realm
        ),
        group = "Observations"
      ) %>%
      addLayersControl(
        overlayGroups = c("Realms", "Observations"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # Zoom to points if any
    if (nrow(pts_sf) > 0) {
      bb <- sf::st_bbox(pts_sf)
      m <- m %>%
        fitBounds(
          lng1 = bb["xmin"], lat1 = bb["ymin"],
          lng2 = bb["xmax"], lat2 = bb["ymax"]
        )
    }
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


