# Script to generate ALL Non-Parametric Analysis Reports

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

# Define the master analysis template (The Engine)
# It is located in Base/ directory
analysis_template <- file.path("Base", "Master_Report_NonParametric.Rmd")

# Define the interpretation template (The Summary)
# It is located in Root directory
interpretation_template <- "Interpretation_Template_NonParametric.Rmd"

# Define Scenarios
scenarios <- list(
  # 1. Base (Standard)
  list(
    name = "Base",
    output_dir = "Base",
    output_file = "Master_Report_NonParametric.html",
    xlsx_name = "Master_Statistics_NonParametric.xlsx",
    params = list(
      exclude_ids = c(),
      missing_as_error = FALSE,
      keep_outliers = FALSE,
      data_source_mode = "auto",
      xlsx_output_name = "Master_Statistics_NonParametric.xlsx"
    ),
    title = "無母數分析：基礎 (Base)"
  ),
  
  # 2. Exclude S5/S10
  list(
    name = "Exclude_S5S10",
    output_dir = "Exclude_S5S10",
    output_file = "Master_Report_NonParametric.html",
    xlsx_name = "Master_Statistics_NonParametric.xlsx",
    params = list(
      exclude_ids = c("S5", "S05", "S005", "S10", "S010"),
      missing_as_error = FALSE,
      keep_outliers = FALSE,
      data_source_mode = "auto",
      xlsx_output_name = "Master_Statistics_NonParametric.xlsx"
    ),
    title = "無母數分析：排除 S5/S10"
  ),
  
  # 3. Missing As Error
  list(
    name = "MissingAsError",
    output_dir = "MissingAsError",
    output_file = "Master_Report_NonParametric.html",
    xlsx_name = "Master_Statistics_NonParametric.xlsx",
    params = list(
      exclude_ids = c(),
      missing_as_error = TRUE,
      keep_outliers = FALSE, # Usually missing as error implies keeping NAs in ACC, but here we filter RT ranges.
                             # If we want to strictly follow "MissingAsError" logic for RT, we just don't use the NAs for RT tests.
      data_source_mode = "raw_only",
      xlsx_output_name = "Master_Statistics_NonParametric.xlsx"
    ),
    title = "無母數分析：漏答視為錯誤"
  ),
  
  # 4. No Outlier - Base
  list(
    name = "NoOutlier_Base",
    output_dir = "NoOutlierExclusion",
    output_file = "Report_Base_NonParametric_NoOutlier.html",
    xlsx_name = "Stats_Base_NonParametric_NoOutlier.xlsx",
    params = list(
      exclude_ids = c(),
      missing_as_error = FALSE,
      keep_outliers = TRUE,
      data_source_mode = "raw_only",
      xlsx_output_name = "Stats_Base_NonParametric_NoOutlier.xlsx"
    ),
    title = "無母數分析 (保留離群值)：基礎"
  ),
  
  # 5. No Outlier - Exclude S5/S10
  list(
    name = "NoOutlier_Exclude",
    output_dir = "NoOutlierExclusion",
    output_file = "Report_Exclude_NonParametric_NoOutlier.html",
    xlsx_name = "Stats_Exclude_NonParametric_NoOutlier.xlsx",
    params = list(
      exclude_ids = c("S5", "S05", "S005", "S10", "S010"),
      missing_as_error = FALSE,
      keep_outliers = TRUE,
      data_source_mode = "raw_only",
      xlsx_output_name = "Stats_Exclude_NonParametric_NoOutlier.xlsx"
    ),
    title = "無母數分析 (保留離群值)：排除 S5/S10"
  ),
  
  # 6. No Outlier - Missing As Error
  list(
    name = "NoOutlier_Missing",
    output_dir = "NoOutlierExclusion",
    output_file = "Report_Missing_NonParametric_NoOutlier.html",
    xlsx_name = "Stats_Missing_NonParametric_NoOutlier.xlsx",
    params = list(
      exclude_ids = c(),
      missing_as_error = TRUE,
      keep_outliers = TRUE,
      data_source_mode = "raw_only",
      xlsx_output_name = "Stats_Missing_NonParametric_NoOutlier.xlsx"
    ),
    title = "無母數分析 (保留離群值)：漏答視為錯誤"
  )
)

message("=== Starting Non-Parametric Analysis Pipeline ===")

for (scen in scenarios) {
  message(paste("Processing Scenario:", scen$name))
  
  # Calculate correct relative path for XLSX output (Analysis Rmd runs in Base/)
  if (scen$output_dir == "Base") {
    xlsx_out_for_analysis <- scen$xlsx_name
  } else {
    xlsx_out_for_analysis <- file.path("..", scen$output_dir, scen$xlsx_name)
  }
  
  # Update params
  current_params <- scen$params
  current_params$xlsx_output_name <- xlsx_out_for_analysis
  
  # 1. Run Analysis (Generate XLSX and Full Report)
  tryCatch({
    render(
      input = analysis_template,
      output_dir = scen$output_dir,
      output_file = scen$output_file,
      params = current_params,
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message(paste("  [OK] Analysis Report:", file.path(scen$output_dir, scen$output_file)))
  }, error = function(e) {
    message(paste("  [FAIL] Analysis Report:", e$message))
  })
  
  # 2. Run Interpretation (Generate Summary HTML)
  # Interpretation template needs to know where the XLSX is.
  # Since we are rendering INTO the output_dir, the relative path to XLSX is just the filename.
  
  interp_output_filename <- paste0("Interpretation_", scen$name, ".html")
  # Simplify standard names
  if(scen$name == "Base") interp_output_filename <- "Interpretation_NonParametric.html"
  if(scen$name == "Exclude_S5S10") interp_output_filename <- "Interpretation_NonParametric.html"
  if(scen$name == "MissingAsError") interp_output_filename <- "Interpretation_NonParametric.html"
  
  tryCatch({
    render(
      input = interpretation_template,
      output_dir = scen$output_dir,
      output_file = interp_output_filename,
      params = list(
        title_text = scen$title,
        xlsx_path = file.path(scen$output_dir, scen$xlsx_name),
        exclude_ids = scen$params$exclude_ids,
        missing_as_error = scen$params$missing_as_error,
        keep_outliers = scen$params$keep_outliers
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message(paste("  [OK] Interpretation:", file.path(scen$output_dir, interp_output_filename)))
  }, error = function(e) {
    message(paste("  [FAIL] Interpretation:", e$message))
  })
}

message("=== Pipeline Complete ===")
