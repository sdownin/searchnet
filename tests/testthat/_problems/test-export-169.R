# Extracted from test-export.R:169

# prequel ----------------------------------------------------------------------
tryCatch(
  source(file.path(dir_r, "searchnet-export.R"), local = FALSE),
  error = function(e) {
    message("Could not source searchnet-export.R: ", e$message)
  }
)

# test -------------------------------------------------------------------------
env <- SaomNkRSienaBiEnv$new(make_small_environ_params())
expect_error(
    searchnet_export_snapshots(env),
    "NULL"
  )
