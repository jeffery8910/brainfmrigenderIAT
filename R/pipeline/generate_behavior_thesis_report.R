# Generate thesis-style printable report (Word)

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

message("=== Generating Thesis-style Behavior Report (docx) ===")

tryCatch({
  render(
    input = "Behavior_Thesis_Report.Rmd",
    output_dir = ".",
    output_file = "Behavior_Thesis_Report.docx",
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  message("  [OK] Behavior_Thesis_Report.docx")
}, error = function(e) {
  message("  [FAIL] Thesis report: ", e$message)
})

message("=== Done ===")
