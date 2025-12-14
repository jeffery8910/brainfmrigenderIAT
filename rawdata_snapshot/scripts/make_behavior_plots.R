# Static figures export for behavioral data (single-file script)
# Location: make_behavior_plots.R (project root)
# Data source (preferred): raw csv files matching S*_fMRI_*.csv
# - Default location: rawdata/S*_fMRI_*.csv  (will auto-map S004/S006/S007/S008/S010 -> S001~S005)
# - Override location: set env var BEHAVIOR_RAW_DIR to ".", "rawdata", or any folder path
# Fallback: Thesis_Analysis_Output/analysis_data.csv
# Outputs: Behavior_Figures/<Scenario>/*.png

# Working directory policy:
# - If you run this script inside a RAW snapshot folder that already contains S*_fMRI_*.csv,
#   we assume you want outputs generated *in that folder* (no auto setwd to project root).
# - Otherwise, we try to locate the project root (the folder containing index.html).
if (!file.exists("index.html")) {
  has_raw_here <- length(list.files(".", pattern = "^S.*_fMRI_.*\\.csv$", full.names = TRUE)) > 0
  has_raw_in_rawdata <- dir.exists("rawdata") &&
    length(list.files("rawdata", pattern = "^S.*_fMRI_.*\\.csv$", full.names = TRUE)) > 0

  if (!(has_raw_here || has_raw_in_rawdata)) {
    start_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    for (i in 1:8) {
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
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(cowplot)
})

# === Color settings (edit here) ===
# If you want to change the figure theme/colors, edit the hex codes below.
COL_RT <- "#1f77b4"  # bars for mean RT
COL_ACC <- "#2ca02c" # bars for accuracy

# Demo-style Congruent/Incongruent bars
COL_COND_DEMO <- c(
  "Congruent" = "#d73027",
  "Incongruent" = "#223b87"
)

# Effect direction colors
COL_EFFECT_DIR <- c(
  "Incongruent slower (+)" = "#FC4E07",
  "Incongruent faster (-)" = "#00AFBB"
)

find_first <- function(vec) {
  for (p in vec) if (!is.na(p) && file.exists(p)) return(p)
  return(NULL)
}

out_root <- Sys.getenv("BEHAVIOR_OUTPUT_DIR", unset = "Behavior_Figures")
if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

# === Data loading (prefer RAW csv) ===
# RAW: Behavior/rawdata/S*_fMRI_*.csv
# Fallback: Thesis_Analysis_Output/analysis_data.csv
SUBJECT_MAP <- c(
  "S004" = "S001",
  "S006" = "S002",
  "S007" = "S003",
  "S008" = "S004",
  "S010" = "S005"
)

normalize_from_stim_class <- function(stim_class) {
  sc <- as.character(stim_class)

  modality <- case_when(
    str_detect(sc, "圖片") ~ "Image",
    str_detect(sc, "文字") ~ "Text",
    TRUE ~ NA_character_
  )

  image_type <- if_else(modality == "Image", str_extract(sc, "^[A-Za-z]+"), NA_character_)

  tokens <- str_split(sc, "\\s+", simplify = TRUE)
  arousal_raw <- if_else(modality == "Text" & ncol(tokens) >= 1, tokens[, 1], NA_character_)
  valence_raw <- if_else(modality == "Text" & ncol(tokens) >= 2, tokens[, 2], NA_character_)

  arousal <- recode(arousal_raw, "高" = "High", "中" = "Mid", "低" = "Low", .default = NA_character_)
  valence <- recode(valence_raw, "正向" = "Positive", "負向" = "Negative", .default = NA_character_)

  tibble(
    Modality = modality,
    Image_Type = image_type,
    Valence = valence,
    Arousal = arousal
  )
}

load_from_raw_csv <- function(raw_dir = "rawdata") {
  files <- list.files(raw_dir, pattern = "^S.*_fMRI_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)

  rows <- lapply(files, function(path) {
    d <- read.csv(path, stringsAsFactors = FALSE)

    if (!"subject" %in% names(d)) {
      subj_orig <- str_split(basename(path), "_", simplify = TRUE)[1, 1]
      d$subject <- subj_orig
    }

    d <- d %>%
      mutate(
        subject = recode(subject, !!!SUBJECT_MAP),
        rt = suppressWarnings(as.numeric(rt)),
        acc = suppressWarnings(as.numeric(acc))
      )

    if ("stim_class" %in% names(d)) {
      extra <- normalize_from_stim_class(d$stim_class)
      d <- bind_cols(d, extra)
    } else {
      d <- d %>% mutate(Modality = NA_character_, Image_Type = NA_character_, Valence = NA_character_, Arousal = NA_character_)
    }

    d %>%
      transmute(
        subject,
        phase,
        block_type,
        trial_global,
        trial_block,
        condition,
        event,
        rt,
        acc,
        Modality,
        Image_Type,
        Valence,
        Arousal
      )
  })

  bind_rows(rows)
}

load_from_processed_csv <- function() {
  data_path <- find_first(c(
    file.path("..", "..", "Thesis_Analysis_Output", "analysis_data.csv"),
    file.path("..", "Thesis_Analysis_Output", "analysis_data.csv"),
    file.path("Thesis_Analysis_Output", "analysis_data.csv")
  ))
  if (is.null(data_path)) return(NULL)
  read.csv(data_path, stringsAsFactors = FALSE)
}

pick_raw_dir <- function() {
  # Priority:
  # 1) env var (BEHAVIOR_RAW_DIR) if it contains raw csv
  # 2) project root "." if it contains raw csv
  # 3) rawdata/ if it contains raw csv
  env_dir <- Sys.getenv("BEHAVIOR_RAW_DIR", unset = NA_character_)
  candidates <- c(env_dir, ".", "rawdata")
  for (cand in candidates) {
    if (is.na(cand) || !dir.exists(cand)) next
    has_files <- length(list.files(cand, pattern = "^S.*_fMRI_.*\\.csv$", full.names = TRUE)) > 0
    if (has_files) return(cand)
  }
  return(NULL)
}

raw_dir <- pick_raw_dir()
raw_all <- if (!is.null(raw_dir)) load_from_raw_csv(raw_dir) else NULL
if (is.null(raw_all)) {
  raw_all <- load_from_processed_csv()
  if (is.null(raw_all)) stop("找不到 S*_fMRI_*.csv（Root/rawdata）或 Thesis_Analysis_Output/analysis_data.csv。")
  message("Using data source: Thesis_Analysis_Output/analysis_data.csv")
} else {
  message("Using data source: ", raw_dir, "/S*_fMRI_*.csv")
}

trials_all <- raw_all %>% filter(is.na(event) | event == "")

rt_min <- 0.2
rt_max <- 1.5

scenarios <- list(
  list(id = "Base", title = "Base", exclude_ids = character(0), missing_as_error = FALSE, keep_outliers = FALSE),
  list(id = "Exclude_S5S10", title = "Exclude S5/S10", exclude_ids = c("S5", "S05", "S005", "S10", "S010"), missing_as_error = FALSE, keep_outliers = FALSE),
  list(id = "MissingAsError", title = "Missing as Error", exclude_ids = character(0), missing_as_error = TRUE, keep_outliers = FALSE),
  list(id = "NoOutlier_Base", title = "NoOutlier Base", exclude_ids = character(0), missing_as_error = FALSE, keep_outliers = TRUE),
  list(id = "NoOutlier_Exclude_S5S10", title = "NoOutlier Exclude S5/S10", exclude_ids = c("S5", "S05", "S005", "S10", "S010"), missing_as_error = FALSE, keep_outliers = TRUE),
  list(id = "NoOutlier_MissingAsError", title = "NoOutlier Missing as Error", exclude_ids = character(0), missing_as_error = TRUE, keep_outliers = TRUE)
)

apply_scenario_rules <- function(trials, exclude_ids, missing_as_error, keep_outliers, rt_min, rt_max) {
  d <- trials %>%
    mutate(
      rt = suppressWarnings(as.numeric(rt)),
      acc = suppressWarnings(as.numeric(acc))
    )

  if (length(exclude_ids) > 0) d <- d %>% filter(!subject %in% exclude_ids)

  if (missing_as_error) {
    d <- d %>% mutate(acc_used = ifelse(is.na(acc), 0, acc))
  } else {
    d <- d %>% mutate(acc_used = acc)
  }

  if (!keep_outliers) {
    if (missing_as_error) {
      d <- d %>% filter(is.na(rt) | (rt >= rt_min & rt <= rt_max))
    } else {
      d <- d %>% filter(!is.na(rt), !is.na(acc_used), rt >= rt_min, rt <= rt_max)
    }
  } else {
    if (!missing_as_error) {
      d <- d %>% filter(!is.na(rt), !is.na(acc_used))
    }
  }

  d %>%
    mutate(
      condition = factor(condition, levels = c("Congruent", "Incongruent")),
      Modality = factor(Modality, levels = c("Image", "Text")),
      subject = factor(subject)
    )
}

save_plot <- function(p, out_dir, name, width = 8, height = 5) {
  ggsave(filename = file.path(out_dir, name), plot = p, width = width, height = height, dpi = 300)
}

make_dual_rt_acc_static <- function(trials_used, scenario_title) {
  subj_dual <- trials_used %>%
    group_by(subject) %>%
    summarise(
      acc_mean = mean(acc_used, na.rm = TRUE),
      rt_mean_correct = mean(rt[acc_used == 1], na.rm = TRUE),
      n = n(),
      n_correct_rt = sum(acc_used == 1 & !is.na(rt)),
      .groups = "drop"
    ) %>%
    filter(is.finite(acc_mean), is.finite(rt_mean_correct))

  if (nrow(subj_dual) == 0) return(NULL)

  rt_scale <- max(subj_dual$rt_mean_correct, na.rm = TRUE)
  if (!is.finite(rt_scale) || rt_scale <= 0) rt_scale <- 1

  plot_df <- subj_dual %>%
    mutate(
      acc_scaled = acc_mean * rt_scale
    ) %>%
    select(subject, rt_mean_correct, acc_mean, acc_scaled, n, n_correct_rt) %>%
    pivot_longer(cols = c(rt_mean_correct, acc_scaled), names_to = "metric", values_to = "value") %>%
    mutate(
      metric = recode(metric,
                      rt_mean_correct = "Mean RT (correct)",
                      acc_scaled = "Accuracy"),
      label = if_else(metric == "Mean RT (correct)",
                      sprintf("%.3f s", value),
                      sprintf("%.1f%%", (value / rt_scale) * 100))
    )

  plot_df$metric <- factor(plot_df$metric, levels = c("Mean RT (correct)", "Accuracy"))

  ggplot(plot_df, aes(x = subject, y = value, fill = metric)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8) +
    geom_text(
      aes(label = label),
      position = position_dodge(width = 0.85),
      vjust = -0.35,
      size = 3
    ) +
    scale_fill_manual(values = c("Mean RT (correct)" = COL_RT, "Accuracy" = COL_ACC)) +
    scale_y_continuous(
      name = "Mean RT (s)",
      sec.axis = sec_axis(~ . / rt_scale, name = "Accuracy", labels = percent_format(accuracy = 1)),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = paste0("Per-Subject: Mean RT (Correct) & Accuracy (Dual-axis) - ", scenario_title),
      x = "Subject",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom"
    )
}

make_rt_acc_side_by_side_static <- function(trials_used, scenario_title) {
  subj_dual <- trials_used %>%
    group_by(subject) %>%
    summarise(
      acc_mean = mean(acc_used, na.rm = TRUE),
      rt_mean_correct = mean(rt[acc_used == 1], na.rm = TRUE),
      n = n(),
      n_correct_rt = sum(acc_used == 1 & !is.na(rt)),
      .groups = "drop"
    ) %>%
    filter(is.finite(acc_mean), is.finite(rt_mean_correct))

  if (nrow(subj_dual) == 0) return(NULL)

  p_rt <- ggplot(subj_dual, aes(x = subject, y = rt_mean_correct)) +
    geom_col(fill = COL_RT, width = 0.75) +
    geom_text(aes(label = sprintf("%.3f s", rt_mean_correct)), vjust = -0.35, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Mean RT (Correct)",
      x = "Subject",
      y = "Mean RT (s)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  p_acc <- ggplot(subj_dual, aes(x = subject, y = acc_mean)) +
    geom_col(fill = COL_ACC, width = 0.75) +
    geom_text(aes(label = sprintf("%.1f%%", acc_mean * 100)), vjust = -0.35, size = 3) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Accuracy",
      x = "Subject",
      y = "Accuracy"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  body <- cowplot::plot_grid(p_rt, p_acc, ncol = 2, rel_widths = c(1, 1))

  cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Per-Subject: Mean RT (Correct) & Accuracy (Side-by-side) - ", scenario_title),
      fontface = "bold",
      x = 0,
      hjust = 0,
      size = 14
    ) +
    cowplot::draw_plot(body, y = 0, height = 0.92)
}

make_subject_rt_cond_mod_static <- function(trials_used, scenario_title) {
  subj_rt <- trials_used %>%
    filter(acc_used == 1, !is.na(rt), !is.na(Modality), !is.na(condition)) %>%
    group_by(subject, Modality, condition) %>%
    summarise(mean_rt = mean(rt, na.rm = TRUE), n = n(), .groups = "drop") %>%
    mutate(cond_mod = paste0(Modality, " × ", condition))

  if (nrow(subj_rt) == 0) return(NULL)

  subj_rt$cond_mod <- factor(subj_rt$cond_mod,
                             levels = c("Image × Congruent", "Image × Incongruent",
                                        "Text × Congruent", "Text × Incongruent"))

  ggplot(subj_rt, aes(x = subject, y = mean_rt, fill = cond_mod)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.82) +
    geom_text(
      aes(label = sprintf("%.3f", mean_rt)),
      position = position_dodge(width = 0.9),
      vjust = -0.35,
      size = 3
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = paste0("Per-Subject Mean RT (Correct Trials; Condition × Modality) - ", scenario_title),
      x = "Subject",
      y = "Mean RT (s)",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom"
    )
}

make_demo_rt_congruency_subject <- function(trials_used, scenario_title) {
  d <- trials_used %>%
    filter(acc_used == 1, !is.na(rt), is.finite(rt)) %>%
    mutate(condition = factor(condition, levels = c("Congruent", "Incongruent")))

  if (nrow(d) == 0) return(NULL)

  subj_stats <- d %>%
    group_by(subject, condition) %>%
    summarise(
      mean_rt = mean(rt, na.rm = TRUE),
      sd_rt = sd(rt, na.rm = TRUE),
      n = n(),
      se_rt = sd_rt / sqrt(n),
      .groups = "drop"
    )

  pvals <- d %>%
    group_by(subject) %>%
    summarise(
      p = tryCatch(
        wilcox.test(rt ~ condition, exact = FALSE)$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(
      label = case_when(
        is.na(p) ~ "p=NA",
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        TRUE ~ sprintf("p=%.2f", p)
      )
    )

  ann <- subj_stats %>%
    group_by(subject) %>%
    summarise(y = max(mean_rt + se_rt, na.rm = TRUE), .groups = "drop") %>%
    left_join(pvals, by = "subject") %>%
    mutate(y = y + 0.06)

  ggplot(subj_stats, aes(x = subject, y = mean_rt, fill = condition)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68, color = "white") +
    geom_errorbar(
      aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
      position = position_dodge(width = 0.75),
      width = 0.18,
      linewidth = 0.5
    ) +
    geom_text(
      data = ann,
      aes(x = subject, y = y, label = label),
      inherit.aes = FALSE,
      size = 3.2
    ) +
    scale_fill_manual(values = COL_COND_DEMO, guide = "none") +
    labs(
      title = paste0("Per-Subject RT (Congruent vs Incongruent) - ", scenario_title),
      x = "Subject",
      y = "RT (s)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}

make_demo_rt_congruency_group <- function(trials_used, scenario_title) {
  d <- trials_used %>%
    filter(acc_used == 1, !is.na(rt), is.finite(rt)) %>%
    mutate(condition = factor(condition, levels = c("Congruent", "Incongruent")))

  if (nrow(d) == 0) return(NULL)

  subj_means <- d %>%
    group_by(subject, condition) %>%
    summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = condition, values_from = mean_rt)

  if (!all(c("Congruent", "Incongruent") %in% names(subj_means))) return(NULL)

  n_subj <- nrow(subj_means)

  wt <- tryCatch(
    wilcox.test(subj_means$Incongruent, subj_means$Congruent, paired = TRUE, exact = FALSE),
    error = function(e) NULL
  )

  p <- if (is.null(wt)) NA_real_ else wt$p.value
  label <- case_when(
    is.na(p) ~ "p=NA",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ sprintf("p=%.3f", p)
  )

  grp <- tibble(
    condition = factor(c("Congruent", "Incongruent"), levels = c("Congruent", "Incongruent")),
    mean_rt = c(
      mean(subj_means$Congruent, na.rm = TRUE),
      mean(subj_means$Incongruent, na.rm = TRUE)
    ),
    se = c(
      sd(subj_means$Congruent, na.rm = TRUE) / sqrt(n_subj),
      sd(subj_means$Incongruent, na.rm = TRUE) / sqrt(n_subj)
    )
  )

  y_top <- max(grp$mean_rt + grp$se, na.rm = TRUE) + 0.08

  ggplot(grp, aes(x = condition, y = mean_rt, fill = condition)) +
    geom_col(width = 0.6, color = "white") +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.18, linewidth = 0.5) +
    geom_segment(aes(x = 1, xend = 2, y = y_top, yend = y_top), inherit.aes = FALSE, linewidth = 0.7) +
    geom_segment(aes(x = 1, xend = 1, y = y_top, yend = y_top - 0.03), inherit.aes = FALSE, linewidth = 0.7) +
    geom_segment(aes(x = 2, xend = 2, y = y_top, yend = y_top - 0.03), inherit.aes = FALSE, linewidth = 0.7) +
    geom_text(aes(x = 1.5, y = y_top + 0.02, label = paste0(label, "\nN=", n_subj)),
              inherit.aes = FALSE, size = 3.2) +
    scale_fill_manual(values = COL_COND_DEMO, guide = "none") +
    labs(
      title = paste0("Group Mean RT (Congruent vs Incongruent) - ", scenario_title),
      x = NULL,
      y = "RT (s)"
    ) +
    theme_minimal(base_size = 12)
}

make_demo_rt_congruency_combined <- function(trials_used, scenario_title) {
  p1 <- make_demo_rt_congruency_subject(trials_used, scenario_title)
  p2 <- make_demo_rt_congruency_group(trials_used, scenario_title)
  if (is.null(p1) || is.null(p2)) return(NULL)

  cowplot::plot_grid(
    p1, p2,
    nrow = 1,
    rel_widths = c(1.55, 1),
    align = "hv",
    axis = "tb"
  )
}

for (scen in scenarios) {
  message("Generating static figures: ", scen$id)

  out_dir <- file.path(out_root, scen$id)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  trials_used <- apply_scenario_rules(
    trials = trials_all,
    exclude_ids = scen$exclude_ids,
    missing_as_error = scen$missing_as_error,
    keep_outliers = scen$keep_outliers,
    rt_min = rt_min,
    rt_max = rt_max
  )

  # 1) Accuracy by Condition × Modality
  acc_summary <- trials_used %>%
    group_by(condition, Modality) %>%
    summarise(acc_mean = mean(acc_used, na.rm = TRUE), n = n(), .groups = "drop")

  if (nrow(acc_summary) > 0) {
    p_acc <- ggplot(acc_summary, aes(x = condition, y = acc_mean, fill = Modality)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = percent(acc_mean, accuracy = 0.1)),
        position = position_dodge(width = 0.8),
        vjust = -0.4,
        size = 3
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(title = paste0("Accuracy: Condition × Modality - ", scen$title), x = "Condition", y = "Accuracy") +
      theme_minimal() +
      theme(legend.position = "bottom")

    save_plot(p_acc, out_dir, "accuracy_by_condition_modality.png")
  }

  # 2) RT distribution (violin+box)
  rt_ok <- trials_used %>% filter(!is.na(rt), rt > 0)
  if (nrow(rt_ok) > 0) {
    p_rt <- ggplot(rt_ok, aes(x = condition, y = rt, fill = condition)) +
      geom_violin(trim = FALSE, alpha = 0.3) +
      geom_boxplot(width = 0.2, outlier.alpha = 0.2) +
      labs(title = paste0("RT Distribution (Violin + Box) - ", scen$title), x = "Condition", y = "RT (s)") +
      theme_minimal() +
      theme(legend.position = "none")
    save_plot(p_rt, out_dir, "rt_violin_box.png")

    # 3) RT density by Modality
    if (any(!is.na(rt_ok$Modality))) {
      p_density <- ggplot(rt_ok %>% filter(!is.na(Modality)), aes(x = rt, fill = Modality)) +
        geom_density(alpha = 0.35) +
        labs(title = paste0("RT Density (colored by Modality) - ", scen$title), x = "RT (s)", y = "Density") +
        theme_minimal() +
        theme(legend.position = "bottom")
      save_plot(p_density, out_dir, "rt_density_modality.png")
    }

    # 6) RT histogram overall
    p_hist <- ggplot(rt_ok, aes(x = rt)) +
      geom_histogram(bins = 40, fill = "#00AFBB", alpha = 0.7, color = "white") +
      labs(title = paste0("RT Histogram (All Trials) - ", scen$title), x = "RT (s)", y = "Count") +
      theme_minimal()
    save_plot(p_hist, out_dir, "rt_histogram.png")
  }

  # 4) Subject-level congruency effect (Incongruent - Congruent mean RT)
  subj_eff <- trials_used %>%
    filter(!is.na(rt), acc_used == 1) %>%
    group_by(subject, condition) %>%
    summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = condition, values_from = mean_rt) %>%
    { if (!"Incongruent" %in% names(.)) mutate(., Incongruent = NA_real_) else . } %>%
    { if (!"Congruent" %in% names(.)) mutate(., Congruent = NA_real_) else . } %>%
    mutate(effect = Incongruent - Congruent) %>%
    arrange(desc(effect))

  if (nrow(subj_eff) > 0) {
    p_effect <- ggplot(subj_eff, aes(x = reorder(subject, effect), y = effect, fill = effect > 0)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.3f", effect)), hjust = ifelse(subj_eff$effect > 0, -0.1, 1.1), size = 3) +
      coord_flip() +
      scale_fill_manual(values = c("#00AFBB", "#FC4E07"), guide = "none") +
      labs(title = paste0("Congruency Effect (Incongruent - Congruent RT) - ", scen$title), x = "Subject", y = "RT Diff (s)") +
      theme_minimal()
    save_plot(p_effect, out_dir, "subject_congruency_effect.png", width = 6, height = 4)
  }

  # 5) Missing RT count per subject (Stored in used set)
  miss_rt <- trials_used %>% summarise(missing_rt = sum(is.na(rt)), .by = subject)
  if (nrow(miss_rt) > 0) {
    p_missing <- ggplot(miss_rt, aes(x = reorder(subject, missing_rt), y = missing_rt)) +
      geom_col(fill = "#999999") +
      geom_text(aes(label = missing_rt), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(title = paste0("Missing RT Count (Per Subject) - ", scen$title), x = "Subject", y = "Missing RT count") +
      theme_minimal()
    save_plot(p_missing, out_dir, "missing_rt_per_subject.png", width = 6, height = 4)
  }

  # 7) Subject mean RT by Condition × Modality (static, labeled)
  p_subj_rt <- make_subject_rt_cond_mod_static(trials_used, scen$title)
  if (!is.null(p_subj_rt)) {
    save_plot(p_subj_rt, out_dir, "subject_mean_rt_condition_modality.png", width = 9, height = 5.5)
  }

  # 8) Dual-axis RT + Accuracy (static, labeled)
  p_dual <- make_dual_rt_acc_static(trials_used, scen$title)
  if (!is.null(p_dual)) {
    save_plot(p_dual, out_dir, "dual_rt_accuracy_by_subject.png", width = 9, height = 5.5)
  }

  # 8b) Side-by-side RT + Accuracy (static, labeled)
  p_side <- make_rt_acc_side_by_side_static(trials_used, scen$title)
  if (!is.null(p_side)) {
    save_plot(p_side, out_dir, "rt_accuracy_side_by_side.png", width = 11, height = 5.2)
  }

  # 9) Demo-style: subject + group RT bars (split)
  p_demo_subj <- make_demo_rt_congruency_subject(trials_used, scen$title)
  if (!is.null(p_demo_subj)) {
    save_plot(p_demo_subj, out_dir, "demo_rt_congruency_subject.png", width = 9, height = 4.6)
  }

  p_demo_group <- make_demo_rt_congruency_group(trials_used, scen$title)
  if (!is.null(p_demo_group)) {
    save_plot(p_demo_group, out_dir, "demo_rt_congruency_group.png", width = 6.6, height = 4.6)
  }

  # (Optional) keep a combined version for quick preview
  p_demo <- make_demo_rt_congruency_combined(trials_used, scen$title)
  if (!is.null(p_demo)) {
    save_plot(p_demo, out_dir, "demo_rt_congruency_combined.png", width = 11, height = 4.4)
  }
}

# Backward-compatible shortcuts:
# Also copy Base scenario figures to Behavior_Figures/*.png
base_dir <- file.path(out_root, "Base")
if (dir.exists(base_dir)) {
  base_files <- c(
    "accuracy_by_condition_modality.png",
    "rt_violin_box.png",
    "rt_density_modality.png",
    "rt_histogram.png",
    "subject_congruency_effect.png",
    "missing_rt_per_subject.png",
    "subject_mean_rt_condition_modality.png",
    "dual_rt_accuracy_by_subject.png",
    "rt_accuracy_side_by_side.png",
    "demo_rt_congruency_subject.png",
    "demo_rt_congruency_group.png",
    "demo_rt_congruency_combined.png"
  )
  for (f in base_files) {
    src <- file.path(base_dir, f)
    dst <- file.path(out_root, f)
    if (file.exists(src)) file.copy(src, dst, overwrite = TRUE)
  }
}

message("Static figures saved to ", normalizePath(out_root, winslash = "/", mustWork = TRUE))
