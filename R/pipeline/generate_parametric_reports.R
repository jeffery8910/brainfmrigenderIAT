# Script to generate parametric (t-test / ANOVA) master reports

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

scenarios <- list(
  list(
    name = "Base",
    input = file.path("Base", "Master_Report_Base.Rmd"),
    output_dir = "Base",
    output_file = "Master_Report_Base.html"
  ),
  list(
    name = "Exclude_S5S10",
    input = file.path("Exclude_S5S10", "Master_Report_Exclude_S5S10.Rmd"),
    output_dir = "Exclude_S5S10",
    output_file = "Master_Report_Exclude_S5S10.html"
  ),
  list(
    name = "MissingAsError",
    input = file.path("MissingAsError", "Master_Report_MissingAsError.Rmd"),
    output_dir = "MissingAsError",
    output_file = "Master_Report_MissingAsError.html"
  ),
  list(
    name = "NoOutlier_Base",
    input = file.path("NoOutlierExclusion", "Report_Base_NoOutlier.Rmd"),
    output_dir = "NoOutlierExclusion",
    output_file = "Report_Base_NoOutlier.html"
  ),
  list(
    name = "NoOutlier_Exclude",
    input = file.path("NoOutlierExclusion", "Report_Exclude_S5S10_NoOutlier.Rmd"),
    output_dir = "NoOutlierExclusion",
    output_file = "Report_Exclude_S5S10_NoOutlier.html"
  ),
  list(
    name = "NoOutlier_Missing",
    input = file.path("NoOutlierExclusion", "Report_MissingAsError_NoOutlier.Rmd"),
    output_dir = "NoOutlierExclusion",
    output_file = "Report_MissingAsError_NoOutlier.html"
  )
)

message("=== Starting Parametric Analysis Pipeline ===")

for (scen in scenarios) {
  message("Processing Scenario: ", scen$name)

  tryCatch({
    render(
      input = scen$input,
      output_dir = scen$output_dir,
      output_file = scen$output_file,
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("  [OK] Report: ", file.path(scen$output_dir, scen$output_file))
  }, error = function(e) {
    message("  [FAIL] ", scen$name, ": ", e$message)
  })
}

message("=== Pipeline Complete ===")
