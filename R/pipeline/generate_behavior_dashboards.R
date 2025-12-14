# Generate Behavior Dashboards for all scenarios

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

dashboard_template <- "Behavior_Dashboard.Rmd"

scenarios_standard <- list(
  list(
    name = "Base",
    dir = "Base",
    out = "Behavior_Dashboard.html",
    title = "Dashboard：基礎 (Base)",
    exclude_ids = c(),
    missing_as_error = FALSE,
    keep_outliers = FALSE
  ),
  list(
    name = "Exclude_S5S10",
    dir = "Exclude_S5S10",
    out = "Behavior_Dashboard.html",
    title = "Dashboard：排除 S5/S10",
    exclude_ids = c("S5", "S05", "S005", "S10", "S010"),
    missing_as_error = FALSE,
    keep_outliers = FALSE
  ),
  list(
    name = "MissingAsError",
    dir = "MissingAsError",
    out = "Behavior_Dashboard.html",
    title = "Dashboard：漏答視為錯誤",
    exclude_ids = c(),
    missing_as_error = TRUE,
    keep_outliers = FALSE
  )
)

scenarios_no_outlier <- list(
  list(
    name = "NoOutlier_Base",
    dir = "NoOutlierExclusion",
    out = "Behavior_Dashboard_Base.html",
    title = "Dashboard：基礎 (保留離群值)",
    exclude_ids = c(),
    missing_as_error = FALSE,
    keep_outliers = TRUE
  ),
  list(
    name = "NoOutlier_Exclude",
    dir = "NoOutlierExclusion",
    out = "Behavior_Dashboard_Exclude.html",
    title = "Dashboard：排除 S5/S10 (保留離群值)",
    exclude_ids = c("S5", "S05", "S005", "S10", "S010"),
    missing_as_error = FALSE,
    keep_outliers = TRUE
  ),
  list(
    name = "NoOutlier_Missing",
    dir = "NoOutlierExclusion",
    out = "Behavior_Dashboard_Missing.html",
    title = "Dashboard：漏答視為錯誤 (保留離群值)",
    exclude_ids = c(),
    missing_as_error = TRUE,
    keep_outliers = TRUE
  )
)

message("=== Starting Behavior Dashboard Pipeline ===")

all_scens <- c(scenarios_standard, scenarios_no_outlier)

for (scen in all_scens) {
  message("Rendering: ", scen$name)
  tryCatch({
    render(
      input = dashboard_template,
      output_dir = scen$dir,
      output_file = scen$out,
      params = list(
        title_text = scen$title,
        exclude_ids = scen$exclude_ids,
        missing_as_error = scen$missing_as_error,
        keep_outliers = scen$keep_outliers
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("  [OK] ", file.path(scen$dir, scen$out))
  }, error = function(e) {
    message("  [FAIL] ", scen$name, ": ", e$message)
  })
}

message("=== Pipeline Complete ===")
