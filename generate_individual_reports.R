# Script to generate individual reports for each subject

library(rmarkdown)
library(tidyverse)

# 1. Read Data
data_path <- "../../Statistical_Analysis_Implementation/cleaned_trial_level_data.csv"
if (!file.exists(data_path)) {
  stop("Data file not found at: ", data_path)
}

df <- read.csv(data_path)
subjects <- unique(df$subject)
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
      quiet = TRUE
    )
    message("  -> Success: ", output_file)
  }, error = function(e) {
    message("  -> Failed: ", subj, " Error: ", e$message)
  })
}

message("All done.")
