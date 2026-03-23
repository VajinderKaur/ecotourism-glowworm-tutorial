# encoding: UTF-8
suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(plotly)
})
# sourcing the files so that app.R remains easy to read.
source("utils.R")
source("text.R") 

# UI
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=1200"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    )
  ),
  
  # Header
  tags$div(id = "app-header",
           tags$div(id = "app-header-inner",
                    # Left column
                    tags$div(id = "app-title-block",
                             tags$h1(HTML(APP_TITLE)),
                             tags$p(class = "subtitle", APP_SUBTITLE),
                             tags$p(class = "data-note", APP_DESC),
                             tags$div(id = "pkg-facts",
                                      tags$span(class = "pkg-chip", tags$strong(PKG_TOTAL),   " total records"),
                                      tags$span(class = "pkg-chip", tags$strong(PKG_SPECIES), " species"),
                                      tags$span(class = "pkg-chip", tags$strong(PKG_STATES),  " states"),
                                      tags$span(class = "pkg-chip", tags$strong(PKG_YEARS),   " years of data")
                             ),
                             tags$div(id = "header-links",
                                      tags$span(class = "res-label", "Data & code"),
                                      tags$a(class = "res-link", target = "_blank",
                                             href = "https://github.com/vahdatjavad/ecotourism",
                                             tags$i(class = "fab fa-github"), "ecotourism package"),
                                      tags$a(class = "res-link", target = "_blank",
                                             href = "https://www.ala.org.au",
                                             tags$i(class = "fas fa-globe"), "Atlas of Living Australia"),
                                      tags$a(class = "res-link", target = "_blank",
                                             href = "https://www.gbif.org",
                                             tags$i(class = "fas fa-database"), "GBIF")
                             ),
                             tags$details(class = "about-details",
                                          tags$summary(tags$i(class = "fas fa-info-circle"), " More about this data"),
                                          tags$div(class = "about-body",
                                                   tags$div(class = "about-body-inner",
                                                            tags$div(class = "about-grid",
                                                                     tags$div(class = "about-section",
                                                                              tags$h4(ABOUT_SIGHTING_TITLE),
                                                                              ABOUT_SIGHTING
                                                                     ),
                                                                     tags$div(class = "about-section",
                                                                              tags$h4(ABOUT_CAVEATS_TITLE),
                                                                              ABOUT_CAVEATS
                                                                     ),
                                                                     tags$div(class = "about-section",
                                                                              tags$h4(ABOUT_WEATHER_TITLE),
                                                                              ABOUT_WEATHER,
                                                                              tags$div(class = "bias-box", ABOUT_BIAS)
                                                                     )
                                                            )
                                                   )
                                          )
                             )
                    ),
                    
                    # Right column
                    tags$div(id = "org-cards",
                             lapply(ORG_CARDS, function(card) {
                               tags$div(class = "org-card",
                                        tags$span(class = "oc-name", card$icon),
                                        card$desc,
                                        tags$span(class = "oc-badge", card$badge)
                               )
                             })
                    )
           )
  ),
  
  # Filter bar
  tags$div(id = "filter-bar",
           tags$div(id = "filter-bar-inner",
                    tags$span(class = "filter-label", "Organism"),
                    selectInput("organism", label = NULL,
                                choices  = names(ORGANISM_COLOURS),
                                selected = "Glowworms", width = "200px"),
                    tags$span(class = "filter-label", "Month"),
                    selectInput("month", label = NULL,
                                choices  = c("All months" = 0,
                                             setNames(1:12, MONTH_NAMES)),
                                selected = 0, width = "170px"),
                    uiOutput("active_chip_ui")
           )
  ),
  
  # Navigation tab
  tags$div(id = "tab-nav",
           tags$div(id = "tab-nav-inner",
                    tags$button(class = "tab-btn active", `data-tab` = "overview",
                                tags$i(class = "fas fa-map"), TAB_OVERVIEW),
                    tags$button(class = "tab-btn", `data-tab` = "sightings",
                                tags$i(class = "fas fa-binoculars"), TAB_SIGHTINGS),
                    tags$button(class = "tab-btn", `data-tab` = "bystate",
                                tags$i(class = "fas fa-chart-bar"), TAB_BYSTATE),
                    tags$button(class = "tab-btn", `data-tab` = "weather",
                                tags$i(class = "fas fa-cloud-sun"), TAB_WEATHER)
           )
  ),
  
  # Content Wrapper
  tags$div(id = "main-content-wrapper",
           tags$div(id = "main-content",
                    
                    # Overview panel
                    tags$div(id = "panel-overview", class = "tab-panel active",
                             tags$div(class = "section-header",
                                      "Overview  \u00B7  Where are they spotted?"),
                             tags$div(id = "stat-boxes",
                                      tags$div(class = "stat-box sb-green",
                                               tags$div(class = "sb-val", uiOutput("vb_total_val")),
                                               tags$div(class = "sb-lbl", STAT_SIGHTINGS),
                                               tags$i(class = "fas fa-eye sb-icon")),
                                      tags$div(class = "stat-box sb-olive",
                                               tags$div(class = "sb-val", uiOutput("vb_states_val")),
                                               tags$div(class = "sb-lbl", STAT_STATES),
                                               tags$i(class = "fas fa-map-marker-alt sb-icon")),
                                      tags$div(class = "stat-box sb-teal",
                                               tags$div(class = "sb-val", uiOutput("vb_years_val")),
                                               tags$div(class = "sb-lbl", STAT_YEARS),
                                               tags$i(class = "fas fa-calendar sb-icon")),
                                      tags$div(class = "stat-box sb-lime",
                                               tags$div(class = "sb-val", uiOutput("vb_species_val")),
                                               tags$div(class = "sb-lbl", STAT_SPECIES),
                                               tags$i(class = "fas fa-dna sb-icon"))
                             ),
                             tags$div(class = "content-card",
                                      tags$div(class = "card-header",
                                               tags$span(class = "ch-title", uiOutput("map_title")),
                                               downloadButton("dl_map_csv",
                                                              label = tagList(tags$i(class = "fas fa-download"), " Export CSV"),
                                                              class = "dl-btn")
                                      ),
                                      leafletOutput("map", height = "500px")
                             )
                    ),
                    
                    # Sightings panel
                    tags$div(id = "panel-sightings", class = "tab-panel",
                             tags$div(class = "section-header",
                                      "Sightings  \u00B7  Trends over time"),
                             tags$div(class = "two-col",
                                      tags$div(class = "content-card",
                                               tags$div(class = "card-header",
                                                        tags$span(class = "ch-title", CARD_MONTH_CHART),
                                                        tags$button(class = "dl-btn", onclick = "downloadPlotly('month_chart')",
                                                                    tags$i(class = "fas fa-download"), " PNG")
                                               ),
                                               tags$div(class = "card-body", plotlyOutput("month_chart", height = "300px"))
                                      ),
                                      tags$div(class = "content-card",
                                               tags$div(class = "card-header",
                                                        tags$span(class = "ch-title", CARD_YEAR_CHART),
                                                        tags$button(class = "dl-btn", onclick = "downloadPlotly('year_chart')",
                                                                    tags$i(class = "fas fa-download"), " PNG")
                                               ),
                                               tags$div(class = "card-body", plotlyOutput("year_chart", height = "300px"))
                                      )
                             ),
                             # Hour-of-day chart -- machine observations excluded automatically
                             # in build_hour_chart(). Directly connected to the tutorial finding
                             # that glowworm sightings peak after dark in November Queensland.
                             tags$div(class = "content-card",
                                      tags$div(class = "card-header",
                                               tags$span(class = "ch-title", CARD_HOUR_CHART),
                                               tags$button(class = "dl-btn", onclick = "downloadPlotly('hour_chart')",
                                                           tags$i(class = "fas fa-download"), " PNG")
                                      ),
                                      tags$div(class = "card-body", plotlyOutput("hour_chart", height = "280px"))
                             ),
                             tags$div(class = "content-card warning",
                                      tags$div(class = "card-header",
                                               tags$span(class = "ch-title", CARD_ACTIVITY)),
                                      tags$div(class = "card-body", uiOutput("activity_card"))
                             )
                    ),
                    
                    # By State panel
                    tags$div(id = "panel-bystate", class = "tab-panel",
                             tags$div(class = "section-header",
                                      "By State  \u00B7  Where do they live?"),
                             tags$div(class = "col-8-4",
                                      tags$div(class = "content-card",
                                               tags$div(class = "card-header",
                                                        tags$span(class = "ch-title", CARD_STATE_BAR),
                                                        tags$button(class = "dl-btn", onclick = "downloadPlotly('state_bar')",
                                                                    tags$i(class = "fas fa-download"), " PNG")
                                               ),
                                               tags$div(class = "card-body",
                                                        plotlyOutput("state_bar", height = "400px"))),
                                      tags$div(class = "content-card warning",
                                               tags$div(class = "card-header",
                                                        tags$span(class = "ch-title", CARD_STATE_TABLE),
                                                        downloadButton("dl_state_csv",
                                                                       label = tagList(tags$i(class = "fas fa-download"), " CSV"),
                                                                       class = "dl-btn")
                                               ),
                                               tags$div(class = "card-body", tableOutput("state_table")))
                             )
                    ),
                    
                    # Weather panel
                    tags$div(id = "panel-weather", class = "tab-panel",
                             tags$div(class = "section-header", SEC_WEATHER),
                             tags$div(class = "col-7-5",
                                      tags$div(class = "content-card",
                                               tags$div(class = "card-header",
                                                        tags$span(class = "ch-title", CARD_WEATHER_CHART),
                                                        tags$button(class = "dl-btn", onclick = "downloadPlotly('weather_chart')",
                                                                    tags$i(class = "fas fa-download"), " PNG")
                                               ),
                                               tags$div(class = "card-body",
                                                        plotlyOutput("weather_chart", height = "380px"))),
                                      tags$div(class = "content-card warning",
                                               tags$div(class = "card-header",
                                                        tags$span(class = "ch-title", CARD_WEATHER_PIE),
                                                        tags$button(class = "dl-btn", onclick = "downloadPlotly('weather_pie')",
                                                                    tags$i(class = "fas fa-download"), " PNG")
                                               ),
                                               tags$div(class = "card-body",
                                                        plotlyOutput("weather_pie", height = "380px")))
                             )
                    )
                    
           ) # /main-content
  ), # /main-content-wrapper
  
  # Chart PNG download via Plotly.downloadImage
  tags$script(HTML("
    function downloadPlotly(outputId) {
      var el = document.getElementById(outputId);
      if (!el) return;
      Plotly.downloadImage(el, {
        format:   'png',
        width:    1200,
        height:   700,
        filename: outputId + '_wildlife_explorer'
      });
    }
  ")),
  
  # Tab switching: panels use visibility:hidden rather than display:none
  # so Plotly charts retain their dimensions and render correctly.
  tags$script(HTML("
    $(document).on('click', '.tab-btn', function () {
      var tab = $(this).data('tab');

      // Update button state
      $('.tab-btn').removeClass('active');
      $(this).addClass('active');

      // Show/hide panels
      $('.tab-panel').removeClass('active');
      $('#panel-' + tab).addClass('active');

      // Scroll page back to top on tab switch
      window.scrollTo(0, 0);

      // Resize any Plotly charts in the newly active panel
      setTimeout(function () {
        var plots = document.querySelectorAll('#panel-' + tab + ' .js-plotly-plot');
        plots.forEach(function (el) {
          try { Plotly.Plots.resize(el); } catch (e) {}
        });

        // Refresh Leaflet map if switching to the overview tab
        if (tab === 'overview') {
          var mapEl = document.getElementById('map');
          if (mapEl && mapEl._leaflet_id) {
            try {
              L.map(mapEl).invalidateSize({ animate: false });
            } catch (e) {}
          }
        }
      }, 80);
    });
  "))
)

# SERVER
server <- function(input, output, session) {
  
  # Filtered data reacts to organism + month selectors
  filtered <- reactive({
    d <- dplyr::filter(ALL_ORGANISMS, organism == input$organism)
    if (as.integer(input$month) != 0)
      d <- dplyr::filter(d, month == as.integer(input$month))
    d
  })
  
  # Unfiltered organism data for trend charts (month filter excluded)
  org_data <- reactive({
    dplyr::filter(ALL_ORGANISMS, organism == input$organism)
  })
  
  # Sighting data joined to weather
  with_weather <- reactive({
    dplyr::left_join(
      filtered(),
      dplyr::select(weather, ws_id, date, temp),
      by = c("ws_id", "date")
    )
  })
  
  colour <- reactive({ ORGANISM_COLOURS[[input$organism]] })
  
  output$active_chip_ui <- renderUI({
    mo <- if (as.integer(input$month) == 0) NULL
    else paste0(" . ", MONTH_NAMES[as.integer(input$month)])
    tags$span(class = "active-chip", input$organism, mo)
  })
  
  # Stat boxes
  output$vb_total_val  <- renderUI(
    tags$span(format(nrow(filtered()), big.mark = ",")))
  output$vb_states_val <- renderUI(
    tags$span(length(unique(filtered()$obs_state))))
  output$vb_years_val  <- renderUI({
    d <- org_data()
    tags$span(paste(min(d$year), "-", max(d$year)))
  })
  output$vb_species_val <- renderUI(
    tags$span(length(unique(filtered()$sci_name))))
  
  output$map_title <- renderUI({
    col <- colour()
    mo  <- if (as.integer(input$month) == 0) "All Year"
    else MONTH_NAMES[as.integer(input$month)]
    tags$span(
      tags$span(input$organism,
                style = paste0("color:", col, ";font-weight:700;")),
      tags$small(" . ", mo, style = "color:#9A9A8A;font-weight:400;")
    )
  })
  
  # Base map -- markers updated separately via leafletProxy
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 13)) |>
      addProviderTiles("Esri.WorldTopoMap") |>
      setView(lng = 134, lat = -27, zoom = 4) |>
      setMaxBounds(lng1 = 100, lat1 = -50, lng2 = 165, lat2 = -5)
  })
  
  observe({
    d   <- filtered()
    col <- colour()
    leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters() |>
      addCircleMarkers(
        data        = d,
        lng         = ~obs_lon,
        lat         = ~obs_lat,
        radius      = 5,
        color       = col, fillColor = col,
        fillOpacity = 0.75, weight = 1.2,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 50, spiderfyOnMaxZoom = TRUE),
        popup = ~paste0(
          "<div style='font-family:Georgia,serif;font-size:0.83rem;line-height:1.6;'>",
          "<strong><i>", sci_name, "</i></strong><br>",
          format(as.Date(date), "%d %b %Y"), "<br>",
          "<span style='color:#7A8A72;'>", obs_state, "</span></div>")
      )
  })
  
  # Trend charts use full organism data so month filter does not distort the series
  output$month_chart <- renderPlotly({ build_month_chart(org_data(), colour()) })
  output$year_chart  <- renderPlotly({ build_year_chart(org_data(), colour()) })
  
  # Hour-of-day chart -- machine observations excluded inside build_hour_chart()
  output$hour_chart  <- renderPlotly({ build_hour_chart(org_data(), colour()) })
  
  # Seasonal activity card
  output$activity_card <- renderUI({
    d <- org_data()
    
    mc <- d |>
      dplyr::count(month) |>
      dplyr::arrange(month)
    
    all_months <- data.frame(month = 1:12)
    mc <- dplyr::left_join(all_months, mc, by = "month") |>
      dplyr::mutate(n = ifelse(is.na(n), 0L, n))
    
    max_n     <- max(mc$n, 1)
    ranks     <- rank(-mc$n, ties.method = "first")
    tier      <- dplyr::case_when(ranks <= 3 ~ "peak", ranks <= 6 ~ "high", TRUE ~ "normal")
    peak_idx  <- which(tier == "peak")
    short_mon <- c("J","F","M","A","M","J","J","A","S","O","N","D")
    
    aus_season <- function(m) {
      dplyr::case_when(
        m %in% c(12, 1, 2) ~ "summer",
        m %in% c(3, 4, 5)  ~ "autumn",
        m %in% c(6, 7, 8)  ~ "winter",
        TRUE                ~ "spring"
      )
    }
    peak_seasons  <- unique(aus_season(peak_idx))
    peak_names    <- MONTH_NAMES[peak_idx]
    top_state     <- d |> dplyr::count(obs_state, sort = TRUE) |>
      dplyr::slice_head(n = 1) |> dplyr::pull(obs_state)
    
    season_str <- if (length(peak_seasons) == 1) peak_seasons
    else paste(peak_seasons[-length(peak_seasons)], collapse = ", ") |>
      paste("and", peak_seasons[length(peak_seasons)])
    month_str  <- paste(peak_names, collapse = ", ")
    
    bars <- lapply(seq_len(12), function(i) {
      ht  <- max(4, round(mc$n[i] / max_n * 44))
      tags$div(
        class = paste0("ac-spark-bar ", tier[i]),
        style = paste0("height:", ht, "px;")
      )
    })
    
    ticks <- lapply(seq_len(12), function(i) {
      cls <- if (tier[i] == "peak") "ac-month-lbl peak" else "ac-month-lbl"
      tags$span(class = cls, short_mon[i])
    })
    
    tags$div(class = "activity-card",
             tags$span(class = "ac-label", ACTIVITY_LABEL),
             tags$div(class = "ac-spark",     bars),
             tags$div(class = "ac-month-row", ticks),
             tags$div(class = "ac-narrative",
                      ACTIVITY_NARRATIVE(input$organism, month_str, season_str, top_state))
    )
  })
  
  output$state_bar <- renderPlotly({ build_state_bar(filtered(), colour()) })
  
  output$state_table <- renderTable({
    filtered() |>
      dplyr::count(obs_state, sort = TRUE) |>
      dplyr::rename(State = obs_state, Sightings = n) |>
      dplyr::mutate(Sightings = format(Sightings, big.mark = ","))
  }, striped = TRUE, hover = TRUE, bordered = FALSE,
  spacing = "s", width = "100%", align = "lr")
  
  # CSV exports
  output$dl_map_csv <- downloadHandler(
    filename = function() paste0(tolower(gsub(" ", "_", input$organism)), "_sightings_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(
        filtered() |>
          dplyr::select(date, sci_name, obs_lat, obs_lon, obs_state, year, month),
        file, row.names = FALSE
      )
    }
  )
  
  output$dl_state_csv <- downloadHandler(
    filename = function() paste0(tolower(gsub(" ", "_", input$organism)), "_by_state_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(
        filtered() |>
          dplyr::count(obs_state, sort = TRUE) |>
          dplyr::rename(state = obs_state, sightings = n),
        file, row.names = FALSE
      )
    }
  )
  
  output$weather_chart <- renderPlotly({ build_weather_chart(with_weather()) })
  output$weather_pie   <- renderPlotly({ build_weather_pie(with_weather()) })
}

shinyApp(ui = ui, server = server)