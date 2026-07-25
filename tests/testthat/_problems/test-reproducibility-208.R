# Extracted from test-reproducibility.R:208

# test -------------------------------------------------------------------------
env <- SaomNkRSienaBiEnv_base$new(list(
    M = 6, N = 10, BI_PROB = 0.5, name = "rb_test", rand_seed = 1
  ))
mat1 <- env$random_bipartite_matrix(seed = 10)
