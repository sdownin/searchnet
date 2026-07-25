# Extracted from test-reproducibility.R:195

# test -------------------------------------------------------------------------
env <- SaomNkRSienaBiEnv_base$new(list(
    M = 6, N = 10, BI_PROB = 0.5, name = "rb_test", rand_seed = 1
  ))
mats <- replicate(5, env$random_bipartite_matrix(seed = 42), simplify = FALSE)
