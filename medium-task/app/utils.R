# utils.R  --  shared constants, data preparation, and plot builders
# Sourced by app.R at startup

suppressPackageStartupMessages({
  library(dplyr)
  library(plotly)
  library(ecotourism)
})

# ── Constants ─────────────────────────────────────────────────────────────────

ORGANISM_COLOURS <- c(
  "Glowworms"      = "#7AB648",
  "Gouldian Finch" = "#E07B39",
  "Manta Rays"     = "#3A86C8",
  "Orchids"        = "#C84B8A"
)

MONTH_NAMES <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

PLOT_BG  <- "#FDFAF4"
GRID_COL <- "#E8E0D0"

# ── Data preparation ──────────────────────────────────────────────────────────

data(glowworms,      package = "ecotourism")
data(gouldian_finch, package = "ecotourism")
data(manta_rays,     package = "ecotourism")
data(orchids,        package = "ecotourism")
data(weather,        package = "ecotourism")

ALL_ORGANISMS <- dplyr::bind_rows(
  glowworms      |> dplyr::mutate(organism = "Glowworms"),
  gouldian_finch |> dplyr::mutate(organism = "Gouldian Finch"),
  manta_rays     |> dplyr::mutate(organism = "Manta Rays"),
  orchids        |> dplyr::mutate(organism = "Orchids")
) |> dplyr::filter(!is.na(obs_lat), !is.na(obs_lon))

# Summary stats used in the header chips
PKG_TOTAL   <- format(nrow(ALL_ORGANISMS), big.mark = ",")
PKG_SPECIES <- length(unique(ALL_ORGANISMS$sci_name))
PKG_STATES  <- length(unique(ALL_ORGANISMS$obs_state))
PKG_YEARS   <- paste(min(ALL_ORGANISMS$year),
                     max(ALL_ORGANISMS$year), sep = "-")

# ── Plot builders ─────────────────────────────────────────────────────────────
# Each builder is a plain function returning a plotly object.
# The same function is reused for both renderPlotly() and download handlers,
# avoiding duplicated logic and ensuring exports match what the user sees.

build_month_chart <- function(org_data, colour) {
  d <- org_data |>
    dplyr::count(month) |>
    dplyr::mutate(month_name = factor(MONTH_NAMES[month], levels = MONTH_NAMES))
  plot_ly(d, x = ~month_name, y = ~n, type = "bar",
          marker = list(color = colour,
                        line  = list(color = colour, width = 1)),
          hovertemplate = "%{x}: %{y} sightings<extra></extra>") |>
    layout(
      xaxis  = list(title = "", tickfont = list(size = 10)),
      yaxis  = list(title = "Sightings", gridcolor = GRID_COL),
      paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG,
      margin = list(t = 10, b = 40)
    )
}

build_year_chart <- function(org_data, colour) {
  d <- org_data |> dplyr::count(year)
  plot_ly(d, x = ~year, y = ~n,
          type = "scatter", mode = "lines+markers",
          line      = list(color = colour, width = 2),
          marker    = list(color = colour, size = 7),
          fill      = "tozeroy",
          fillcolor = "rgba(45,90,39,0.12)",
          hovertemplate = "%{x}: %{y} sightings<extra></extra>") |>
    layout(
      xaxis  = list(title = "", gridcolor = GRID_COL,
                    dtick = 1, tickangle = -45,
                    tickfont = list(size = 9)),
      yaxis  = list(title = "Sightings", gridcolor = GRID_COL),
      paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG,
      margin = list(t = 10, b = 60, l = 50)
    )
}

# Hour-of-day chart — shows when sightings are recorded across the day.
# Machine observations (hour == 0 artefact) are excluded so the chart
# reflects genuine human behaviour, not data collection defaults.
# For glowworms this reveals the strong after-dark signal that motivates
# the tutorial's focus on November nights in Queensland.
build_hour_chart <- function(org_data, colour) {
  d <- org_data |>
    dplyr::filter(
      hour != 0,
      record_type != "MACHINE_OBSERVATION"
    ) |>
    dplyr::count(hour) |>
    dplyr::mutate(
      period = dplyr::case_when(
        hour < 6              ~ "Night",
        hour < 12             ~ "Morning",
        hour < 17             ~ "Afternoon",
        hour < 20             ~ "Evening",
        TRUE                  ~ "Night"
      )
    )

  if (nrow(d) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text      = "No time-of-day data after excluding hour = 0",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE,
            font      = list(size = 13, color = "#9A9A8A")
          )),
          paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG
        )
    )
  }

  plot_ly(d, x = ~hour, y = ~n, type = "bar",
          marker = list(color = colour,
                        line  = list(color = colour, width = 1)),
          hovertemplate = "%{x}:00 — %{y} sightings<extra></extra>") |>
    layout(
      xaxis  = list(
        title      = "Hour of day (24h, machine observations excluded)",
        tickmode   = "linear",
        tick0      = 1,
        dtick      = 1,
        tickfont   = list(size = 9),
        gridcolor  = GRID_COL
      ),
      yaxis  = list(title = "Sightings", gridcolor = GRID_COL),
      paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG,
      margin = list(t = 10, b = 50)
    )
}

build_state_bar <- function(filtered_data, colour) {
  d <- filtered_data |>
    dplyr::count(obs_state, sort = TRUE) |>
    dplyr::slice_head(n = 8) |>
    dplyr::mutate(obs_state = reorder(obs_state, n))
  plot_ly(d, x = ~n, y = ~obs_state,
          type = "bar", orientation = "h",
          marker = list(color = colour,
                        line  = list(color = colour, width = 1)),
          hovertemplate = "%{y}: %{x} sightings<extra></extra>") |>
    layout(
      xaxis  = list(title = "Sightings", gridcolor = GRID_COL),
      yaxis  = list(title = ""),
      paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG,
      margin = list(l = 160, t = 10)
    )
}

build_weather_chart <- function(weather_data) {
  d <- weather_data |>
    dplyr::filter(!is.na(temp)) |>
    dplyr::mutate(month_name = factor(MONTH_NAMES[month], levels = MONTH_NAMES))

  if (nrow(d) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(list(
            text = "No weather data for this selection",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "#9A9A8A")
          )),
          paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG
        )
    )
  }

  plot_ly(d, x = ~month_name, y = ~temp, type = "box",
          marker    = list(color = "#5C8A4A", size = 3, opacity = 0.4),
          line      = list(color = "#2D5A27"),
          fillcolor = "rgba(92,138,74,0.3)",
          hovertemplate = "%{x}<br>Temp: %{y}C<extra></extra>") |>
    layout(
      xaxis  = list(title = "", tickfont = list(size = 10)),
      yaxis  = list(title = "Temperature (C)", gridcolor = GRID_COL),
      paper_bgcolor = PLOT_BG, plot_bgcolor = PLOT_BG,
      margin = list(t = 10)
    )
}

build_weather_pie <- function(weather_data) {
  n_with    <- sum(!is.na(weather_data$temp))
  n_without <- sum(is.na(weather_data$temp))

  plot_ly(
    labels   = c("With weather data", "No weather data"),
    values   = c(n_with, n_without),
    type     = "pie",
    marker   = list(colors = c("#2D5A27", "#D8CEB8"),
                    line   = list(color = "#FDFAF4", width = 2)),
    textinfo          = "percent",
    textposition      = "inside",
    insidetextfont    = list(color = "#FFFFFF", size = 13),
    hovertemplate     = "<b>%{label}</b><br>%{value} sightings (%{percent})<extra></extra>",
    showlegend        = TRUE,
    pull              = c(0.04, 0),
    direction         = "clockwise",
    rotation          = -90,
    domain            = list(x = c(0, 1), y = c(0.05, 0.95))
  ) |>
    layout(
      legend = list(
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = -0.08,
        font = list(size = 11, color = "#4A5A42"),
        bgcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = PLOT_BG,
      margin = list(t = 20, b = 50, l = 20, r = 20),
      transition = list(duration = 600, easing = "cubic-in-out")
    )
}
