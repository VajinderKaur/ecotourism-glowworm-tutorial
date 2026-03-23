predict_best_spotting_times <- function(occurrence_data,
                                        weather_data,
                                        top_stations,
                                        organism_name,
                                        n      = 5,
                                        conf_k = 5) {
  
  #1. Remove machine observations (explained earlier)
  n_total   <- nrow(occurrence_data)
  n_machine <- sum(occurrence_data$record_type == "MACHINE_OBSERVATION",
                   na.rm = TRUE)
  
  occ <- occurrence_data %>%
    filter(record_type != "MACHINE_OBSERVATION")
  
  if (n_machine > 0) {
    message("Excluded ", n_machine, "/", n_total,
            " MACHINE_OBSERVATION records (all recorded at hour 0 by default)")
  }
  
  #Flag hour = 0 in human observations
  n_zero <- sum(occ$hour == 0, na.rm = TRUE)
  occ    <- occ %>% filter(!is.na(hour))
  
  if (n_zero > 0) {
    message(n_zero, " human observation record(s) have hour = 0. ",
            "These are kept. For nocturnal organisms this may be a real ",
            "sighting; the plausibility score will weight it accordingly.")
  }
  
  #Derive ecological plausibility window from the data 
  hour_dist <- occ %>%
    count(hour) %>%
    arrange(hour) %>%
    mutate(cum_pct = cumsum(n) / sum(n))
  
  plausible_hours <- hour_dist %>%
    filter(cum_pct >= 0.10, cum_pct <= 0.90) %>%
    pull(hour)
  
  plaus_min <- min(plausible_hours)
  plaus_max <- max(plausible_hours)
  
  message("Ecological plausibility window (central 80% of sightings): ",
          sprintf("%02d:00", plaus_min), " - ", sprintf("%02d:00", plaus_max))
  
  plausibility_score <- function(h) {
    dist <- pmax(0, pmax(plaus_min - h, h - plaus_max))
    ifelse(h >= plaus_min & h <= plaus_max,
           1.0,
           pmax(0.2, cos(dist / 3 * pi / 2)))
  }
  
  #Get weather for top stations, averaged per date
  org_stations <- top_stations %>%
    filter(organism == organism_name)
  
  if (nrow(org_stations) == 0) {
    stop("No top stations found for organism: ", organism_name)
  }
  
  weather_org <- weather_data %>%
    filter(ws_id %in% org_stations$ws_id) %>%
    group_by(date, month, dayofyear) %>%
    summarise(
      temp       = mean(temp,       na.rm = TRUE),
      rh         = mean(rh,         na.rm = TRUE),
      prcp       = mean(prcp,       na.rm = TRUE),
      rainy      = mean(rainy,      na.rm = TRUE),
      wind_speed = mean(wind_speed, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    # mean(na.rm = TRUE) on all-NA groups produces NaN, converted to NA
    mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)))
  
  #Join occurrences to weather
  occ_weather <- occ %>%
    select(date, hour, month, weekday, dayofyear) %>%
    inner_join(weather_org, by = "date")
  
  #Weather weights via Cohen's d 
  cohen_d <- function(x_obs, x_all) {
    x_obs <- x_obs[!is.na(x_obs)]
    x_all <- x_all[!is.na(x_all)]
    if (length(x_obs) < 3 || length(x_all) < 3) return(0)
    s <- sd(x_all)
    if (is.na(s) || s == 0) return(0)
    abs(mean(x_obs) - mean(x_all)) / s
  }
  
  d_temp  <- cohen_d(occ_weather$temp,       weather_org$temp)
  d_rain  <- cohen_d(occ_weather$rainy,      weather_org$rainy)
  d_wind  <- cohen_d(occ_weather$wind_speed, weather_org$wind_speed)
  d_rh    <- cohen_d(occ_weather$rh,         weather_org$rh)
  total_d <- d_temp + d_rain + d_wind + d_rh + 1e-6
  
  ideal_temp  <- mean(occ_weather$temp,       na.rm = TRUE)
  ideal_rain  <- mean(occ_weather$rainy,      na.rm = TRUE)
  ideal_wind  <- mean(occ_weather$wind_speed, na.rm = TRUE)
  ideal_rh    <- mean(occ_weather$rh,         na.rm = TRUE)
  
  # Score each historical weather day by closeness to ideal conditions
  weather_scored <- weather_org %>%
    mutate(
      weather_score = 1 - (
        (d_temp / total_d) * pmin(abs(temp       - ideal_temp) / 10,  1) +
          (d_rain / total_d) * pmin(abs(rainy       - ideal_rain),       1) +
          (d_wind / total_d) * pmin(abs(wind_speed  - ideal_wind) / 10,  1) +
          (d_rh   / total_d) * pmin(abs(rh          - ideal_rh)   / 100, 1)
      ),
      weather_score = ifelse(is.na(weather_score), 0.5, weather_score)
    ) %>%
    select(date, month, weather_score)
  
  monthly_weather <- weather_scored %>%
    group_by(month) %>%
    summarise(avg_weather_score = mean(weather_score, na.rm = TRUE),
              .groups = "drop")
  
  #Frequency scores
  month_scores <- occ %>%
    count(month, name = "n_month") %>%
    mutate(month_score = n_month / sum(n_month))
  
  hour_scores <- occ %>%
    count(hour, name = "n_hour") %>%
    mutate(hour_score = n_hour / sum(n_hour))
  
  #Confidence: sightings per month x hour cell
  month_hour_counts <- occ %>%
    count(month, hour, name = "n_cell")
  
  #Build month x hour grid and score every combination
  grid <- merge(data.frame(month = 1:12), data.frame(hour = 1:23))
  
  results <- grid %>%
    left_join(month_scores,      by = "month") %>%
    left_join(monthly_weather,   by = "month") %>%
    left_join(hour_scores,       by = "hour")  %>%
    left_join(month_hour_counts, by = c("month", "hour")) %>%
    replace_na(list(
      month_score       = 0,
      avg_weather_score = 0.5,
      hour_score        = 0,
      n_cell            = 0
    )) %>%
    mutate(
      # Base composite score
      composite_score = 0.40 * month_score +
        0.40 * hour_score  +
        0.20 * avg_weather_score,
      
      # Confidence: exponential saturation curve
      confidence   = 1 - exp(-n_cell / conf_k),
      
      # Ecological plausibility
      plausibility = plausibility_score(hour),
      
      # Final score: composite scaled by confidence-plausibility blend
      confidence_weight = 0.6 * confidence + 0.4 * plausibility,
      final_score       = composite_score * confidence_weight,
      
      # Labels
      month_name   = month.name[month],
      time_label   = sprintf("%02d:00", hour),
      period       = case_when(
        hour < 12 ~ "Morning",
        hour < 17 ~ "Afternoon",
        TRUE      ~ "Evening/Night"
      ),
      confidence_tag = case_when(
        confidence >= 0.63 & plausibility >= 0.8 ~
          "High: strong data, ecologically expected",
        confidence >= 0.18 & plausibility >= 0.8 ~
          "Moderate: sparse data but ecologically plausible",
        confidence >= 0.63 & plausibility <  0.8 ~
          "Caution: data present but unusual time",
        TRUE ~
          "Low: sparse data and atypical time"
      )
    ) %>%
    arrange(desc(final_score))
  
  top_results <- results %>% slice_head(n = n)
  
  #Print results
  cat("\n")
  cat("  Best times to spot:", organism_name, "\n")
  cat(rep("-", 60), "\n", sep = "")
  cat(sprintf("  %-22s  %-6s  %-6s  %-6s  %s\n",
              "When", "Score", "Conf.", "Plaus.", "Tag"))
  cat(rep("-", 60), "\n", sep = "")
  
  for (i in seq_len(nrow(top_results))) {
    row <- top_results[i, ]
    cat(sprintf("  %d. %-10s %s  %.3f  %.2f    %.2f    %s\n",
                i,
                row$month_name,
                row$time_label,
                row$final_score,
                row$confidence,
                row$plausibility,
                row$confidence_tag))
  }
  
  cat("\n")
  cat("  Plausibility window:",
      sprintf("%02d:00", plaus_min), "-", sprintf("%02d:00", plaus_max), "\n")
  cat("  Cohen's d, temp:", round(d_temp, 3),
      "| rain:", round(d_rain, 3),
      "| wind:", round(d_wind, 3),
      "| rh:", round(d_rh, 3), "\n")
  cat("  Confidence k =", conf_k,
      "(1 sighting = 0.18, 5 = 0.63, 10 = 0.86, 20+ ~ 0.98)\n")
  
  invisible(list(
    recommendations     = top_results,
    full_grid           = results,
    plausibility_window = c(plaus_min, plaus_max),
    cohen_d = c(temp = d_temp, rain = d_rain, wind = d_wind, rh = d_rh)
  ))
}