# Script to generate ALL interpretation pages

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

# --- 1. Individual Reports ---
subjects <- c("S001", "S002", "S003", "S004", "S005")
indiv_template <- "Individual_Reports/Interpretation_Template_Individual.Rmd"
indiv_output_dir <- "Individual_Reports/Output"

message("Starting Individual Interpretations...")

for (subj in subjects) {
  # The XLSX is in Individual_Reports/ (root of template execution context usually)
  # But we are running from root.
  # The Rmd expects xlsx_path relative to itself or absolute.
  # Let's use absolute or careful relative paths.
  
  # XLSX was saved in Individual_Reports/Full_Stats_Sxxx.xlsx
  xlsx_path <- paste0("Full_Stats_", subj, ".xlsx") 
  # Note: Rmd will run in Individual_Reports/ directory if not specified otherwise, 
  # but render input is relative to CWD.
  
  # We will render inside the directory to match file paths easily
  output_filename <- paste0("Interpretation_", subj, ".html")
  
  tryCatch({
    render(
      input = indiv_template,
      output_file = output_filename,
      output_dir = indiv_output_dir,
      params = list(
        subject_id = subj,
        xlsx_path = xlsx_path
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("  -> Generated: ", output_filename)
  }, error = function(e) {
    message("  -> Failed ", subj, ": ", e$message)
  })
}

# --- 2. Master Reports ---
# Define the 6 scenarios
# Structure: list(Folder, Title, ExcelName)
scenarios <- list(
  list(dir="Base", title="基礎分析 (Base)", xlsx="Master_Statistics_Base.xlsx"),
  list(dir="Exclude_S5S10", title="排除 S5/S10", xlsx="Master_Statistics_Exclude_S5S10.xlsx"),
  list(dir="MissingAsError", title="漏答視為錯誤", xlsx="Master_Statistics_MissingAsError.xlsx"),
  
  # No Outlier versions (Excel names are same but in NoOutlierExclusion folder? 
  # Wait, previous step saved them as specific names in root of NoOutlierExclusion?)
  # Let's check the previous step's save paths. 
  # Base/Master_Report_Base.Rmd saved to "Master_Statistics_Base.xlsx" (relative to Rmd -> in Base/)
  # NoOutlierExclusion/Report_Base_NoOutlier.Rmd saved to "Master_Statistics_Base.xlsx" (in NoOutlierExclusion/)
  # Wait, filenames might collide if I'm not careful about directory context.
  
  # Let's assume they are inside their respective folders.
  
  list(dir="NoOutlierExclusion", title="基礎分析 (保留離群值)", xlsx="Master_Statistics_Base.xlsx", out_name="Interpretation_Base.html"),
  list(dir="NoOutlierExclusion", title="排除 S5/S10 (保留離群值)", xlsx="Master_Statistics_Exclude_S5S10.xlsx", out_name="Interpretation_Exclude.html"),
  list(dir="NoOutlierExclusion", title="漏答視為錯誤 (保留離群值)", xlsx="Master_Statistics_MissingAsError.xlsx", out_name="Interpretation_Missing.html")
)

# For the first 3, they are in separate folders.
scenarios_standard <- list(
  list(dir="Base", title="基礎分析 (Base)", xlsx="Master_Statistics_Base.xlsx",
       exclude_ids = c(), missing_as_error = FALSE, keep_outliers = FALSE),
  list(dir="Exclude_S5S10", title="排除 S5/S10", xlsx="Master_Statistics_Exclude_S5S10.xlsx",
       exclude_ids = c("S5", "S05", "S005", "S10", "S010"), missing_as_error = FALSE, keep_outliers = FALSE),
  list(dir="MissingAsError", title="漏答視為錯誤", xlsx="Master_Statistics_MissingAsError.xlsx",
       exclude_ids = c(), missing_as_error = TRUE, keep_outliers = FALSE)
)

master_template <- "Interpretation_Template_Master.Rmd"

message("Starting Master Interpretations...")

# Render Standard Scenarios
for (scen in scenarios_standard) {
  # XLSX is in the specific dir. Use absolute path.
  xlsx_full_path <- file.path(getwd(), scen$dir, scen$xlsx)
  
  tryCatch({
    render(
      input = master_template,
      output_file = "Interpretation.html",
      output_dir = scen$dir,
      params = list(
        title_text = scen$title,
        xlsx_path = xlsx_full_path,
        exclude_ids = scen$exclude_ids,
        missing_as_error = scen$missing_as_error,
        keep_outliers = scen$keep_outliers
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("  -> Generated: ", scen$dir, "/Interpretation.html")
  }, error = function(e) {
    message("  -> Failed ", scen$dir, ": ", e$message)
  })
}

# Render NoOutlier Scenarios
no_outlier_scens <- list(
  list(xlsx="Master_Statistics_Base.xlsx", title="基礎分析 (保留離群值)", out="Interpretation_Base.html",
       exclude_ids = c(), missing_as_error = FALSE, keep_outliers = TRUE),
  list(xlsx="Master_Statistics_Exclude_S5S10.xlsx", title="排除 S5/S10 (保留離群值)", out="Interpretation_Exclude.html",
       exclude_ids = c("S5", "S05", "S005", "S10", "S010"), missing_as_error = FALSE, keep_outliers = TRUE),
  list(xlsx="Master_Statistics_MissingAsError.xlsx", title="漏答視為錯誤 (保留離群值)", out="Interpretation_Missing.html",
       exclude_ids = c(), missing_as_error = TRUE, keep_outliers = TRUE)
)

for (scen in no_outlier_scens) {
  dir <- "NoOutlierExclusion"
  # xlsx is in NoOutlierExclusion/
  xlsx_full_path <- file.path(getwd(), dir, scen$xlsx)
  
  tryCatch({
    render(
      input = master_template,
      output_file = scen$out,
      output_dir = dir,
      params = list(
        title_text = scen$title,
        xlsx_path = xlsx_full_path,
        exclude_ids = scen$exclude_ids,
        missing_as_error = scen$missing_as_error,
        keep_outliers = scen$keep_outliers
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("  -> Generated: ", dir, "/", scen$out)
  }, error = function(e) {
    message("  -> Failed ", scen$out, ": ", e$message)
  })
}

message("All Interpretations Generated.")
