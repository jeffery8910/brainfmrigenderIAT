# Generate QC/Review report (HTML)

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

message("=== Generating QC Review Report ===")

message("=== Running QC scripts (python) ===")
tryCatch({
  system("python qc_compare_rt.py", intern = FALSE)
}, error = function(e) {
  message("  [WARN] qc_compare_rt.py failed: ", e$message)
})

tryCatch({
  system("python qc_compare_right_rt.py", intern = FALSE)
}, error = function(e) {
  message("  [WARN] qc_compare_right_rt.py failed: ", e$message)
})

tryCatch({
  system("python review_behavior_logs.py", intern = FALSE)
}, error = function(e) {
  message("  [WARN] review_behavior_logs.py failed: ", e$message)
})

tryCatch({
  render(
    input = file.path("QC", "Review_Report.Rmd"),
    output_dir = "QC",
    output_file = "Review_Report.html",
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  message("  [OK] QC/Review_Report.html")
}, error = function(e) {
  message("  [FAIL] QC review: ", e$message)
})

message("=== Done ===")
