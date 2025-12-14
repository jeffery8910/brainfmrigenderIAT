# Script to generate Pilot Insight Reports

# Ensure working directory is project root (Behavior)
if (!file.exists("index.html")) {
  start_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:6) {
    cand <- normalizePath(
      file.path(start_dir, paste(rep("..", i), collapse = "/")),
      winslash = "/",
      mustWork = FALSE
    )
    if (file.exists(file.path(cand, "index.html"))) {
      setwd(cand)
      break
    }
  }
}

library(rmarkdown)
library(tidyverse)

# Template path
pilot_template <- file.path("Base", "Pilot_Insight_Report.Rmd")

# Scenarios to run (Standard ones usually sufficient for Pilot insight)
scenarios <- list(
  list(
    name = "Base",
    output_dir = "Base",
    output_file = "Pilot_Insight_Report.html",
    params = list(
      exclude_ids = c(),
      missing_as_error = FALSE,
      keep_outliers = FALSE,
      data_source_mode = "auto"
    )
  ),
  list(
    name = "Exclude_S5S10",
    output_dir = "Exclude_S5S10",
    output_file = "Pilot_Insight_Report.html",
    params = list(
      exclude_ids = c("S5", "S05", "S005", "S10", "S010"),
      missing_as_error = FALSE,
      keep_outliers = FALSE,
      data_source_mode = "auto"
    )
  ),
  list(
    name = "MissingAsError",
    output_dir = "MissingAsError",
    output_file = "Pilot_Insight_Report.html",
    params = list(
      exclude_ids = c(),
      missing_as_error = TRUE,
      keep_outliers = FALSE,
      data_source_mode = "raw"
    )
  ),
  list(
    name = "NoOutlier_Base",
    output_dir = "NoOutlierExclusion",
    output_file = "Pilot_Insight_Report_Base_NoOutlier.html",
    params = list(
      exclude_ids = c(),
      missing_as_error = FALSE,
      keep_outliers = TRUE,
      data_source_mode = "raw"
    )
  ),
  list(
    name = "NoOutlier_Exclude_S5S10",
    output_dir = "NoOutlierExclusion",
    output_file = "Pilot_Insight_Report_Exclude_NoOutlier.html",
    params = list(
      exclude_ids = c("S5", "S05", "S005", "S10", "S010"),
      missing_as_error = FALSE,
      keep_outliers = TRUE,
      data_source_mode = "raw"
    )
  ),
  list(
    name = "NoOutlier_MissingAsError",
    output_dir = "NoOutlierExclusion",
    output_file = "Pilot_Insight_Report_Missing_NoOutlier.html",
    params = list(
      exclude_ids = c(),
      missing_as_error = TRUE,
      keep_outliers = TRUE,
      data_source_mode = "raw"
    )
  )
)

message("=== Starting Pilot Insight Analysis Pipeline ===")

for (scen in scenarios) {
  message(paste("Processing Scenario:", scen$name))
  
  tryCatch({
    render(
      input = pilot_template,
      output_dir = scen$output_dir,
      output_file = scen$output_file,
      params = scen$params,
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message(paste("  [OK] Pilot Report:", file.path(scen$output_dir, scen$output_file)))
  }, error = function(e) {
    message(paste("  [FAIL] Pilot Report:", e$message))
  })
}

message("=== Pipeline Complete ===")
