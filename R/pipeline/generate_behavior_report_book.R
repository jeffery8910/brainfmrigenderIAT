# Generate the consolidated "Behavior_Report_Book.html"

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

message("=== Generating Static PNG Figures (all scenarios) ===")
invisible(system2("Rscript", args = c("make_behavior_plots.R"), stdout = TRUE, stderr = TRUE))

message("=== Rendering Behavior_Report_Book.Rmd ===")
render(
  input = "Behavior_Report_Book.Rmd",
  output_file = "Behavior_Report_Book.html",
  envir = new.env(parent = globalenv()),
  quiet = TRUE
)

message("  [OK] Behavior_Report_Book.html")
