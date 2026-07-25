# Extracted from test-diagnostics.R:374

# prequel ----------------------------------------------------------------------
tryCatch(
  source(file.path(dir_r, "saomnk-diagnostics.R"), local = FALSE),
  error = function(e) {
    message("Could not source saomnk-diagnostics.R: ", e$message)
  }
)
make_sai_grid <- function() {
  data.frame(
    effect = rep(c("reciprocity", "transitivity", "homophily"), each = 5),
    specification = rep(paste0("spec_", 1:5), 3),
    estimate = c(
      0.8, 0.9, 0.7, 0.85, 0.75,      # reciprocity: all positive
      0.3, -0.1, 0.2, 0.4, -0.05,      # transitivity: mixed sign
      0.5, 0.6, 0.55, 0.45, 0.52       # homophily: all positive
    ),
    std_error = rep(0.15, 15),
    stringsAsFactors = FALSE
  )
}

# test -------------------------------------------------------------------------
result <- suppressMessages(
    saomnk_map_effects("nonexistent_saom", "nonexistent_tergm")
  )
