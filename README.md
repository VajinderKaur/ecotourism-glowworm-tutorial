# Ecotourism GSoC Evaluation Tasks

This repository contains my submission for the Ecotourism GSoC project evaluation tasks.

The tasks focus on working with the `ecotourism` R package to:
- Practice real-world data integration
- Build an interactive visualization tool
- Develop a method to recommend best wildlife spotting times

---

## Tasks Overview

### Easy Task — Teaching Questions

Created a tutorial with three structured questions that guide users through:

- Joining wildlife occurrence data with:
  - Weather data
  - Tourism data
- Working with:
  - One region (Queensland)
  - One organism (Glowworms)
  - One month (November)

The tutorial emphasizes:
- Understanding joins (`ws_id`, `date`, `quarter`)
- Handling missing data
- Interpreting real-world datasets

**File:**
- `ecotourism_tutorial.qmd`

---

### Medium Task — Shiny App

Built an interactive Shiny app to explore wildlife sightings across Australia.

**Features:**
- Map of Australia with occurrence points (Leaflet)
- Dropdown to select organism
- Month filter
- Interactive visualizations:
  - Sightings over time
  - Distribution by state
  - Hour-of-day activity patterns
  - Weather summaries

**Goal:**
Help users visually explore where and when wildlife is observed.

**Files:**
- `app.R`
- `utils.R`
- `text.R`
- `styles.css`

---

### Hard Task — Prediction Function

Implemented:

```r
predict_best_spotting_times()
```

### Hard Task — Prediction Function

This function recommends the top times to spot a given organism by combining:

- Sightings frequency (month + hour)  
- Weather similarity (Cohen’s d weighting)  
- Ecological plausibility (based on observed activity hours)  
- Confidence (based on data density)  

**Output:**
- Top N recommended month–hour combinations  
- Confidence levels and interpretability tags  

**Key idea:**  
Balances data-driven patterns with biological realism.

**File:**
- `predict_best_spotting_times.R`

---

## How to Run

### Install dependencies
```r
install.packages(c("shiny", "tidyverse", "leaflet", "plotly", "lubridate"))
```

### Install ecotourism package
```r
# install.packages("pak")
pak::pak("vahdatjavad/ecotourism")
```

### Run Shiny App
```r
shiny::runApp()
```

### Run Prediction Function
```r
predict_best_spotting_times(
  occurrence_data,
  weather_data,
  top_stations,
  organism_name = "Glowworms"
)
```

### What This Submission Focuses On
Working with messy, real-world data
Understanding data joins and structure
Building interactive tools for exploration
Designing interpretable scoring methods
