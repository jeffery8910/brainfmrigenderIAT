# One-click build: regenerate figures + all HTML outputs

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

message("=== (1/6) Static PNG Figures ===")
invisible(system2("Rscript", args = c("make_behavior_plots.R"), stdout = TRUE, stderr = TRUE))

message("=== (2/6) Parametric Reports ===")
source(file.path("R", "pipeline", "generate_parametric_reports.R"))

message("=== (3/6) Non-Parametric Reports + Interpretations ===")
source(file.path("R", "pipeline", "generate_nonparametric_reports.R"))

message("=== (4/6) Pilot Reports ===")
source(file.path("R", "pipeline", "generate_pilot_reports.R"))

message("=== (5/6) Dashboards / Individual / Interpretations / QC ===")
source(file.path("R", "pipeline", "generate_behavior_dashboards.R"))
source(file.path("R", "pipeline", "generate_individual_reports.R"))
source(file.path("R", "pipeline", "generate_all_interpretations.R"))
source(file.path("R", "pipeline", "generate_review_report.R"))

message("=== (6/6) Consolidated Report Book ===")
source(file.path("R", "pipeline", "generate_behavior_report_book.R"))

message("=== (7/7) Thesis-style Printable Report ===")
source(file.path("R", "pipeline", "generate_behavior_thesis_report.R"))

message("=== All Outputs Updated ===")
