# Script to generate individual reports for each subject

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

# 1. Read Data (use analysis_data.csv: includes practice/formal + event rows)
find_first <- function(vec) {
  for (p in vec) if (!is.na(p) && file.exists(p)) return(p)
  return(NULL)
}

analysis_path <- find_first(c(
  file.path("..","..","Thesis_Analysis_Output","analysis_data.csv"),
  file.path("..","Thesis_Analysis_Output","analysis_data.csv"),
  file.path("Thesis_Analysis_Output","analysis_data.csv")
))
if (is.null(analysis_path)) {
  stop("找不到 analysis_data.csv（Thesis_Analysis_Output）。")
}

raw <- read.csv(analysis_path, stringsAsFactors = FALSE)
trials <- raw %>% filter(is.na(event) | event == "")
subjects <- sort(unique(trials$subject))
print(subjects)

# 2. Output Directory
output_dir <- "Individual_Reports/Output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 3. Template Path
template_path <- "Individual_Reports/Individual_Report_Template.Rmd"

# 4. Loop and Render
for (subj in subjects) {
  message("Generating report for: ", subj)
  
  output_file <- paste0("Subject_Report_", subj, ".html")
  
  tryCatch({
    render(
      input = template_path,
      output_file = output_file,
      output_dir = output_dir,
      params = list(subject_id = subj),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("  -> Success: ", output_file)
  }, error = function(e) {
    message("  -> Failed: ", subj, " Error: ", e$message)
  })
}

message("All done.")
