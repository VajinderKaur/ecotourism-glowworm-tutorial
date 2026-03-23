# text.R  all user-facing texts for the Australian Wildlife Explorer
# this file is for all the labels, descriptions, or copy in the app so that app.R remains easy to read.
# Sourced by app.R at startup, after utils.R.

# App header
APP_TITLE    <- "\U0001F33F Australian Wildlife Explorer"
APP_SUBTITLE <- "Where do glowworms glow, finches fly, rays glide, and orchids bloom?"
APP_DESC     <- tagList(
  "Explore verified occurrence records for four iconic Australian species,",
  " drawn from the ", tags$strong("Atlas of Living Australia"),
  " \u00B7 10 years of citizen science data, 2014-2024."
)

# Organism cards (header, right column)
ORG_CARDS <- list(
  list(
    icon  = "\u2728 Glowworms",
    desc  = "Bioluminescent larvae found in caves and rainforests of eastern Australia. Best spotted at night in spring and autumn.",
    badge = "Insects"
  ),
  list(
    icon  = "\U0001F99C Gouldian Finch",
    desc  = "One of Australia's most endangered birds, found in tropical savannas of the north. Known for extraordinary colour variation.",
    badge = "Critically endangered"
  ),
  list(
    icon  = "\U0001F30A Manta Rays",
    desc  = "Reef and oceanic manta rays patrol the warm waters of the Queensland coast, making seasonal appearances near dive sites.",
    badge = "Marine"
  ),
  list(
    icon  = "\U0001F338 Orchids",
    desc  = "Australia hosts over 1,500 native orchid species across every state \u00B7 from tropical epiphytes to alpine terrestrials.",
    badge = "1,500+ species"
  )
)

# About panel (expands from header)
ABOUT_SIGHTING_TITLE <- "What a sighting record means"
ABOUT_SIGHTING <- tagList(
  tags$p(
    "A record is created when someone \u00B7 a researcher,",
    " a park ranger, or a citizen scientist \u00B7 logs an",
    " observation and submits it to the ",
    tags$strong("Atlas of Living Australia (ALA)."),
    " Each record has a species, a location, and a date."
  ),
  tags$p(
    "Records include ", tags$strong("human observations"),
    " (most common), ", tags$strong("preserved specimens"),
    " from museum collections, and occasional ",
    tags$strong("machine detections"),
    " from acoustic or camera sensors."
  )
)

ABOUT_CAVEATS_TITLE <- "Per-organism caveats"
ABOUT_CAVEATS <- tagList(
  tags$p(tags$strong("Glowworms:"),
         " records skew toward accessible tourist caves,",
         " not the full population range."),
  tags$p(tags$strong("Gouldian Finch:"),
         " sighting density reflects conservation survey",
         " effort as much as actual abundance."),
  tags$p(tags$strong("Manta Rays:"),
         " records cluster around popular dive sites,",
         " not necessarily peak abundance zones."),
  tags$p(tags$strong("Orchids:"),
         " multiple species aggregated \u00B7 patterns reflect",
         " the group, not any single species.")
)

ABOUT_WEATHER_TITLE <- "Weather & reporting bias"
ABOUT_WEATHER <- tags$p(
  "Each sighting is matched to the nearest Bureau of",
  " Meteorology station via the ",
  tags$code("ws_id"), " field.",
  " Temperature reflects station conditions on the",
  " sighting date, not the exact location."
)

ABOUT_BIAS <- tagList(
  tags$strong("(!) Reporting bias:"),
  " sighting density shows where people look,",
  " not just where animals are. Urban areas and",
  " tourist sites are over-represented. Absence",
  " of records does not mean absence of the organism."
)

# Tab labels
TAB_OVERVIEW  <- "Overview"
TAB_SIGHTINGS <- "Sightings"
TAB_BYSTATE   <- "By State"
TAB_WEATHER   <- "Weather"

# Section headers
SEC_OVERVIEW  <- "Overview \u00B7 Where are they spotted?"
SEC_SIGHTINGS <- "Sightings \u00B7 Trends over time"
SEC_BYSTATE   <- "By State \u00B7 Where do they live?"
SEC_WEATHER   <- "Weather \u00B7 Conditions at sighting time"

# Stat box labels
STAT_SIGHTINGS <- "Sightings"
STAT_STATES    <- "States"
STAT_YEARS     <- "Years of data"
STAT_SPECIES   <- "Species"

# Card titles
CARD_MONTH_CHART   <- "Sightings by Month"
CARD_YEAR_CHART    <- "Sightings Over the Years"
CARD_HOUR_CHART    <- "Time of Day — When are sightings recorded? (machine observations excluded)"
CARD_ACTIVITY      <- "Seasonal Activity Pattern"
CARD_STATE_BAR     <- "Top States by Sightings"
CARD_STATE_TABLE   <- "State Summary"
CARD_WEATHER_CHART <- "Temperature by Month"
CARD_WEATHER_PIE   <- "Weather Coverage"

# Seasonal activity card narrative
ACTIVITY_LABEL     <- "Monthly activity rhythm \u00B7 all years combined"
ACTIVITY_NARRATIVE <- function(organism, month_str, season_str, top_state) {
  tagList(
    "Records for ", tags$strong(organism),
    " cluster most strongly in ", tags$strong(month_str), " \u00B7 ",
    "the ", tags$strong(season_str), " months in Australia. ",
    "The concentration of sightings in ", tags$strong(top_state),
    " suggests this is a key habitat corridor. ",
    "Quieter months still hold records, reflecting year-round presence",
    " at lower densities."
  )
}
