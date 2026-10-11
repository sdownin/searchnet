#' @import R6 igraph RSiena
#' @importFrom plyr ddply
#' @importFrom dplyr mutate filter select group_by summarize arrange bind_rows
#' @importFrom Matrix Matrix
#' @importFrom network network
#' @importFrom visNetwork visNetwork visEdges visNodes
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
#' @importFrom grid grid.text
#' @importFrom gridExtra grid.arrange
#' @importFrom cowplot plot_grid
#' @importFrom ggraph ggraph
#' @importFrom ggpubr ggarrange
#' @importFrom texreg screenreg
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom uuid UUIDgenerate



## Helpers of get_struct_mod_stats_mat_from_bi_mat(), kept at package level
## rather than rebuilt at every call (see the note there).
##
## RSiena centers a covariate on its mean unless it was created with
## centered = FALSE, and every covariate effect reads the centered value.
## Using the raw value adds mean(v) times a degree term to the statistic,
## so the egoX, altX, outActX and X columns were off by that much until
## 2026-10-04 (tests/testthat/test-structural-stats-vs-rsiena.R).
.searchnet_rsiena_centered <- function(covar) {
  v <- as.numeric(covar)
  if (isFALSE(attr(covar, 'centered'))) v else v - mean(v)
}
## Effect i's covariate: fetched once per chain by prepare_struct_mod_stats()
## when `.prep` is given, otherwise from the environment.
.searchnet_stats_covar <- function(self, .prep, i, item) {
  if (is.null(.prep)) self$get_cov_data(item) else .prep$covars[[i]]
}

##
##
##
SaomNkRSienaBiEnv_base <- R6Class( 
  ##
  "SaomNkRSienaBiEnv_base",
  ##
  
  public = list(
    #
    ITERATION = 0,
    TIMESTAMP = NULL,
    SIM_NAME = NULL,
    UUID = NULL,
    DIR_OUTPUT = NULL,
    #
    M = NULL,                # Number of actors
    N = NULL,                # Number of components
    BI_PROB = NULL,          # probability of tie in bipartite network dyads (determines bipartite density)
    #
    config_environ_params = list(),   ## environment of simulation (DGP) **TODO** add shocks or landscape changes **
    config_structure_model = list(),  ## list of SAOM model DVs and effects
    ## config_payoff_formulas = list(),  ## list of formulae for each DV's network statistics predictors to be used in 
    ## K = NULL,             # Avg. degree of component-[actor]-component interactions
    bipartite_igraph = NULL, # Bipartite network object 
    social_igraph = NULL,   # Social space projection (network object)
    search_igraph = NULL, # Search space projection (network object)
    #
    bipartite_matrix_init = NULL, ## cache the init matrix for future reference and plotting
    #
    bipartite_matrix = NULL,
    social_matrix = NULL,
    search_matrix = NULL,
    #
    bipartite_matrix_waves = list(), ## list of simulated networks for extended multi-wave simulation; implements dynamic strategy choice/changes
    #
    bi_env_arr = array(), ## bipartite matrix array from chain of ministeps
    ## Memory-efficient diff-based storage (Task 4 optimization):
    ## Instead of full M x N x nchains array, store initial matrix + per-step changes.
    ## Reduces memory from O(M*N*nchains) to O(nchains + M*N).
    bi_env_arr_initial = NULL,   ## M x N initial bipartite matrix
    bi_env_changes = NULL,       ## nchains x 3 matrix: (step, actor_i, comp_j); NA row = stability step
    #
    bipartite_rsienaDV = NULL,
    social_rsienaDV = NULL,
    search_rsienaDV = NULL,
    behavior_rsienaDV = NULL,   ## behavior / performance DV coevolving with the bipartite net
    behavior_values = NULL,     ## the M x waves matrix the behavior DV was built from
    #
    theta_shocks = NULL,
    theta_matrix = NULL,
    #
    ## State-carrying simulation (0.11.0), see R/searchnet-path.R.
    searchnet_path_kind = NULL,  ## "genuine" or "independent_draws" (legacy_replay)
    path_start_matrix = NULL,    ## the state the simulated path actually started from
    path_segments = NULL,        ## one row per simulated segment (rows, rate, seed, ministeps)
    behavior_state = NULL,       ## behavior vector at the end of the last run
    covariate_centering = NULL,  ## the centering RSiena applied to each covariate
    #
    markets = list(),
    #
    ##----- COVARIATES (expanded slots for multi-W-matrix support) -----------
    strat_1_coCovar = NULL,        ## constant strategy covariate  (M-vector)
    strat_2_coCovar = NULL,
    strat_3_coCovar = NULL,
    strat_4_coCovar = NULL,
    strat_5_coCovar = NULL,
    strat_6_coCovar = NULL,
    strat_7_coCovar = NULL,
    strat_8_coCovar = NULL,
    strat_9_coCovar = NULL,
    strat_10_coCovar = NULL,
    #
    strat_1_varCovar = NULL,        ## time-varying strategy covariate  (MxT matrix) for T periods
    strat_2_varCovar = NULL,
    strat_3_varCovar = NULL,
    strat_4_varCovar = NULL,
    #
    strat_1_coDyadCovar = NULL,    ## constant social network covariate (MxM matrix)
    strat_2_coDyadCovar = NULL,
    strat_3_coDyadCovar = NULL,
    strat_4_coDyadCovar = NULL,
    strat_5_coDyadCovar = NULL,
    strat_6_coDyadCovar = NULL,
    strat_7_coDyadCovar = NULL,
    strat_8_coDyadCovar = NULL,
    strat_9_coDyadCovar = NULL,
    strat_10_coDyadCovar = NULL,
    #
    strat_1_varDyadCovar = NULL,    ## time varying social network covariate (MxMxT array) for T periods
    strat_2_varDyadCovar = NULL,
    strat_3_varDyadCovar = NULL,
    strat_4_varDyadCovar = NULL,
    #
    strat_1_interaction = NULL,
    strat_2_interaction = NULL,
    strat_3_interaction = NULL,
    strat_4_interaction = NULL,
    #
    component_1_coCovar = NULL,     ## constant component covariate  (N-vector)
    component_2_coCovar = NULL,
    component_3_coCovar = NULL,
    component_4_coCovar = NULL,
    component_5_coCovar = NULL,
    component_6_coCovar = NULL,
    component_7_coCovar = NULL,
    component_8_coCovar = NULL,
    component_9_coCovar = NULL,
    component_10_coCovar = NULL,
    #
    component_1_varCovar = NULL,     ## time varying component covariate  (NxT matrix) for T periods
    component_2_varCovar = NULL,
    component_3_varCovar = NULL,
    component_4_varCovar = NULL,
    #
    component_1_coDyadCovar = NULL,  ## constant influence matrix covariate (NxN matrix)
    component_2_coDyadCovar = NULL,
    component_3_coDyadCovar = NULL,
    component_4_coDyadCovar = NULL,
    component_5_coDyadCovar = NULL,
    component_6_coDyadCovar = NULL,
    component_7_coDyadCovar = NULL,
    component_8_coDyadCovar = NULL,
    component_9_coDyadCovar = NULL,
    component_10_coDyadCovar = NULL,
    component_11_coDyadCovar = NULL,
    component_12_coDyadCovar = NULL,
    component_13_coDyadCovar = NULL,
    component_14_coDyadCovar = NULL,
    component_15_coDyadCovar = NULL,
    component_16_coDyadCovar = NULL,
    component_17_coDyadCovar = NULL,
    component_18_coDyadCovar = NULL,
    component_19_coDyadCovar = NULL,
    component_20_coDyadCovar = NULL,
    #
    component_1_varDyadCovar = NULL,  ## time-varying influence matrix covariate (NxNxT array) for T periods
    component_2_varDyadCovar = NULL,
    component_3_varDyadCovar = NULL,
    component_4_varDyadCovar = NULL,
    ## Slots 5-20 added 2026-08-23 to match the static `component_*_coDyadCovar`
    ## ladder, which has run to 20 since 0.6.0. A multi-W horserace enters each
    ## coupling as its own XWX term, and four is below what such a design needs.
    ## These are R6 public fields and MUST be declared: the engine assigns by
    ## `self[[sprintf('component_%s_varDyadCovar', i)]] <- ...`, and R6 errors on
    ## assignment to an undeclared field rather than creating it.
    component_5_varDyadCovar = NULL,
    component_6_varDyadCovar = NULL,
    component_7_varDyadCovar = NULL,
    component_8_varDyadCovar = NULL,
    component_9_varDyadCovar = NULL,
    component_10_varDyadCovar = NULL,
    component_11_varDyadCovar = NULL,
    component_12_varDyadCovar = NULL,
    component_13_varDyadCovar = NULL,
    component_14_varDyadCovar = NULL,
    component_15_varDyadCovar = NULL,
    component_16_varDyadCovar = NULL,
    component_17_varDyadCovar = NULL,
    component_18_varDyadCovar = NULL,
    component_19_varDyadCovar = NULL,
    component_20_varDyadCovar = NULL,
    #
    component_1_interaction = NULL,
    component_2_interaction = NULL,
    component_3_interaction = NULL,
    component_4_interaction = NULL,
    #
    interaction_1 = NULL,
    interaction_2 = NULL,
    interaction_3 = NULL,
    interaction_4 = NULL,
    interaction_5 = NULL,
    interaction_6 = NULL,
    #
    ##------/end covariates ------------
    #
    plots = list(),
    multiwave_plots = list(),
    mc_results = list(),  ## Monte Carlo replication results from search_rsiena_monte_carlo()
    #
    fitness_landscape = NULL, # Fitness landscape matrix
    progress_scores = c(),  # Track scores over iterations
    P_change = NULL,          # Probability of changing the component influence matrix
    #
    chain_stats = NULL,  ##
    #
    actor_stats_df = NULL,
    actor_util_df = NULL,
    actor_util_diff_df = NULL,
    #
    component_stats_df = NULL,
    actor_proj_stats_df = NULL,
    component_proj_stats_df = NULL,
    #
    actor_wave_stats = NULL,
    actor_wave_util = NULL,
    actor_wave_util_diff = NULL,
    #
    component_wave_stats =  NULL,
    #
    actor_proj_wave_stats = list(),
    component_proj_wave_stats = list(),

    ## Coupled Degree Statistics
    K_AA_df = NULL, ## degree distribution statistics of Actor projected social network (common components)
    K_AC_df = NULL, ## degree distribution statistics of Bipartite social network - [1]Actors
    K_CA_df = NULL, ## degree distribution statistics of Bipartite social network - [2]Components
    K_CC_df = NULL, ## degree distribution statistics of Component projected network (common actors)
    #
    ###
    K_AA_NEW_df = NULL,
    K_AC_NEW_df = NULL,
    K_CA_NEW_df = NULL,
    K_CC_NEW_df = NULL,
    #
    K_AA_OLD_df = NULL,
    K_AC_OLD_df = NULL,
    K_CA_OLD_df = NULL,
    K_CC_OLD_df = NULL,
    #
    K_wave_A = NULL,
    K_wave_B1 = NULL,
    K_wave_B2 = NULL,
    K_wave_C = NULL,
    ##
    sims_stats = NULL,
    merge_log = list(),
    alliance_log = list(),
    did_analysis_results = NULL,
    #   'K_AA'=list(), ##**TODO** Actor social space [degree = central position in social network]
    #   'K_B'=list(), #  Bipartite space  [ degree = resource/affiliation density ]
    #   'K_CC'=list()  #  Component space  [ degree = interdependence/structuration : epistatic interactions ]
    #   ),
    # #
    rsiena_model = NULL,
    rsiena_data = NULL,
    rsiena_effects = NULL,
    rsiena_algorithm = NULL,
    #
    rsiena_model_waves = list(), ## list of models from multiwave simulation; implements dynamic strategy choice/changes
    #
    rsiena_run_seed = NULL,
    rsiena_env_seed = NULL,
    ## Run provenance (versions, RNG kind, seed, call), written by
    ## saomnk_run() / saomnk_monte_carlo(); see searchnet_provenance().
    provenance = NULL,
    #
    experiments = list(),
    
    
    
    # Constructor to initialize the SAOM-NK model
    initialize = function(config_environ_params, verbose=FALSE) {
      if(verbose) cat('\nCALLED _BASE_ INIT\n')
      ## ----- prevent clashes with sna package-----------
      sna_err_check <- tryCatch(expr = { detach('package:sna') }, error=function(e)e )
      ## -------------------------------------------------
      ##**TODO:  LOAD DEPENDENCY FUNCTIONS ETC**
      ##
      self$config_environ_params = config_environ_params
      self$M <- config_environ_params[['M']]
      self$N <- config_environ_params[['N']]
      self$BI_PROB <- config_environ_params[['BI_PROB']]
      self$UUID <- UUIDgenerate(use.time = TRUE)
      ## Default to the session temp directory, not getwd(): RSiena writes a report
      ## .txt per run into DIR_OUTPUT, so a getwd() default dropped them wherever
      ## the caller happened to be. Running the vignettes left them in vignettes/,
      ## where R CMD build carries them into the tarball.
      self$DIR_OUTPUT <- ifelse(is.null(config_environ_params[['dir_output']]),
                                tempdir(),
                                config_environ_params[['dir_output']])
      # self$P_change <- config_environ_params[['P_change']]
      #
      # self$bipartite_igraph <- self$generate_bipartite_igraph()
      # self$social_network <- self$project_social_space()
      # self$search_landscape <- self$project_search_space()
      
      ##--------- INIT BIPARTITE MATRIX STRUCTURE --------------------
      default_seed <- 123
      self$rsiena_env_seed <- ifelse(is.null(config_environ_params[['rand_seed']]), 
                                     default_seed,
                                     config_environ_params[['rand_seed']])
      
      start_bipartite_matrix <-  if ( !is.null(config_environ_params[['init_matrix']]) ) {
        self$BI_PROB <- sum(c(config_environ_params[['init_matrix']])) / (self$M * self$N) ## update null prob with empirical density
        config_environ_params[['init_matrix']]
      } else {
        self$random_bipartite_matrix(self$rsiena_env_seed)
      }
      # #
      # if ( component_mat_init_type  == 'rand'  ) {
      #   default_seed <- 123
      #   self$rsiena_env_seed <- ifelse(is.null(config_environ_params[['rand_seed']]), 
      #                                  default_seed,
      #                                  config_environ_params[['rand_seed']])
      #   ## 
      #   start_bipartite_matrix <- self$random_bipartite_matrix(self$rsiena_env_seed)
      #   
      #   # } else if (component_mat_init_type == 'modular') {
      #   
      #   # } else if (component_mat_init_type == 'triangular') {  ## 
      #     
      # } else {
      #   
      #   stop(sprintf('Component matrix init type not implemented: %s', component_mat_init_type))
      # 
      # }
      
      ## Keep init matrix
      self$bipartite_matrix_init <- start_bipartite_matrix
      ## SET BIPARTITE NETWORK SYSTEM FROM INIT matrix
      self$set_system_from_bipartite_matrix( start_bipartite_matrix )
      ##
      self$TIMESTAMP <- round( as.numeric(Sys.time())*100 )
      self$SIM_NAME <- config_environ_params[['name']]
      ##
      self$markets <- config_environ_params[['markets']]
    },
    
    
    
    # # Clone method (needed for proper deep copying)
    # clone = function(deep = TRUE) {
    #   # Use R6's built-in clone function
    #   new_instance <- super$clone(deep)
    #   
    #   # If deep copy is requested, clone complex objects
    #   if (deep) {
    #     # Make deep copies of complex objects (lists, data frames, arrays)
    #     if (!is.null(new_instance$config_environ_params)) {
    #       new_instance$config_environ_params <- utils::modifyList(list(), new_instance$config_environ_params)
    #     }
    #     
    #     if (!is.null(new_instance$bi_env_arr)) {
    #       # Arrays need special handling
    #       new_instance$bi_env_arr <- array(new_instance$bi_env_arr, dim = dim(new_instance$bi_env_arr))
    #     }
    #     
    #     if (!is.null(new_instance$actor_util_df)) {
    #       new_instance$actor_util_df <- data.frame(new_instance$actor_util_df)
    #     }
    #   }
    #   
    #   return(new_instance)
    # },
    
    
    
    #
    set_system_from_bipartite_matrix = function(bipartite_matrix) {
      bipartite_igraph <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = FALSE, mode = 'all')
      self$set_system_from_bipartite_igraph(bipartite_igraph)
    },
    
    #
    set_system_from_bipartite_igraph = function(bipartite_igraph) {
      if( ! 'igraph' %in% class(bipartite_igraph))
        stop(sprintf('\nbipartite_igraph is not an igraph object; class %s\n', class(bipartite_igraph)))
      if(!length(E(bipartite_igraph)$weight))
        E(bipartite_igraph)$weight <- 1
      #
      self$bipartite_igraph <- bipartite_igraph
      ## OPTIMIZED: call bipartite_projection() once instead of twice (was called
      ## separately in project_social_space and project_search_space)
      projs <- self$get_bipartite_projections(bipartite_igraph)
      social_ig  <- projs$proj1
      search_ig  <- projs$proj2
      if(!length(E(social_ig)$weight))  E(social_ig)$weight  <- 1
      if(!length(E(search_ig)$weight))  E(search_ig)$weight  <- 1
      self$social_igraph <- social_ig
      self$search_igraph <- search_ig
      #
      self$bipartite_matrix <- igraph::as_biadjacency_matrix(bipartite_igraph,attr = 'weight', sparse = FALSE)
      self$social_matrix <- igraph::as_adjacency_matrix(self$social_igraph, attr = 'weight', sparse = FALSE)
      self$search_matrix <- igraph::as_adjacency_matrix(self$search_igraph, attr = 'weight', sparse = FALSE)
    },
    
    # Generate a random bipartite network matrix
    random_bipartite_matrix = function(rand_seed = 123) {
      ## If BI_PROB==1 or 0, return equivalent matrix (full or empty) 
      if (self$BI_PROB %in% c(0, 1))
        return(matrix(self$BI_PROB, nrow = self$M, ncol = self$N))
      # Else sample random matrix
      set.seed(rand_seed)  # For reproducibility
      probs <- c( 1 - self$BI_PROB, self$BI_PROB )
      bipartite_matrix <- matrix(sample(0:1, self$M * self$N, replace = TRUE, prob = probs ),
                                 nrow = self$M, ncol = self$N)
      return(bipartite_matrix)
      # # bipartite_net <- network(bipartite_matrix, bipartite = TRUE, directed = TRUE)
      # bipartite_igraph <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = F, mode = 'all')
      # return(bipartite_igraph)
    },
    
    # Generate a random bipartite network object
    random_bipartite_igraph = function(rand_seed = 123) {
      bipartite_matrix <- self$random_bipartite_matrix(rand_seed)
      # bipartite_net <- network(bipartite_matrix, bipartite = TRUE, directed = TRUE)
      bipartite_igraph <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = FALSE, mode = 'all')
      return(bipartite_igraph)
    },
    
    ##
    get_bipartite_projections = function(ig_bipartite) {
      projs <- igraph::bipartite_projection(ig_bipartite, multiplicity = TRUE, which = 'both')
      return(projs)    
    },

    # Project the social space (actor network) from the bipartite network
    project_social_space = function(ig_bipartite) {
      projs <- self$get_bipartite_projections(ig_bipartite)
      social_igraph <- projs$proj1
      if(!length(E(social_igraph)$weight)) 
        E(social_igraph)$weight <- 1
      return(social_igraph)
    },

    # Project the search landscape space (component interaction network)
    project_search_space = function(ig_bipartite) {
      projs <- self$get_bipartite_projections(ig_bipartite)
      search_igraph <- projs$proj2
      if(!length(E(search_igraph)$weight)) 
        E(search_igraph)$weight <- 1
      return(search_igraph)
    },
    
    # Update Iteration progress
    increment_sim_iter = function(val = 1) {
      self$ITERATION <- self$ITERATION + val
    }, 
    
    
    ##------ Helper functions ---------------

    ## Reconstruct the bipartite matrix at any chain step from diff-based storage.
    ## Replays changes from bi_env_arr_initial up to the requested step.
    ## This is O(step) per call but uses O(nchains + M*N) total memory
    ## instead of O(M*N*nchains) for the full 3D array.
    get_bipartite_at_step = function(step) {
      if (is.null(self$bi_env_arr_initial) || is.null(self$bi_env_changes))
        stop("Diff-based bi_env storage not initialized. Run simulation first.")
      mat <- self$bi_env_arr_initial
      if (step < 1) return(mat)
      nsteps <- min(step, nrow(self$bi_env_changes))
      for (s in seq_len(nsteps)) {
        ci <- self$bi_env_changes[s, 2]
        cj <- self$bi_env_changes[s, 3]
        if (!is.na(ci) && !is.na(cj)) {
          mat[ci, cj] <- 1L - mat[ci, cj]
        }
      }
      mat
    },

    # Define the bipartite (2-mode) network matrix self$toggle function:
    toggleBiMat = function(m,i,j){
      if ( i >= 1 && i <= dim(m)[1] && j >= 1 && j <= dim(m)[2] ) {
        m[i,j] <-   1 - m[i,j] 
      }
      return(m)
    },
    
    ##
    ##
    ##
    get_jaccard_index = function(m0, m1) {
      diffvec <-  c(m1 - m0)   ## new m1 - old m0
      cnt_maintain <- sum( m1 * m0 )
      cnt_change <- sum( diffvec != 0 )  ## sum = count true cases (added + dropped)
      return( cnt_maintain / (cnt_maintain + cnt_change) )
    },
    
    ##
    exists = function(x){
      return(!is.null(x) && !is.na(x) && !is.nan(x))
    },
    
    # Define the one-mode network matrix self$toggle function:
    toggle = function(m,i,j){
      if (i != j) {
        # print(sprintf('test i %s != j %s',i,j))
        m[i,j] <-  ( 1 - m[i,j] )
      }
      return(m)
    },
    
    
    ##-----/helper-------------------------
 
 
    
    
    ##
    include_rsiena_effect_from_eff_list = function(eff, verbose=FALSE, fix=TRUE) {
      
      # ##---------- 2+ Effects Combination --------------------
      # if (length(eff$effect)>1)
      # {
      #   if (all(eff$effect %in% c('egoX', 'inPopX'))) {
      #     self$rsiena_effects <- includeEffects(self$rsiena_effects,  egoX, inPopX, ## get network statistic function from effect name (character)
      #                                           name = eff$dv_name, 
      #                                           interaction1 = eff$interaction1,
      #                                           fix = fix)
      #     self$rsiena_effects <- setEffect(self$rsiena_effects,  egoX, inPopX, 
      #                                      interaction1 = eff$interaction1,
      #                                      name = eff$dv_name, parameter = eff$parameter,  fix = fix)
      #   }
      #   else 
      #   {
      #     stop('Effect combination not yet implemented.')
      #   }
      #   
      #   return(NULL)
      # }

      ## A declared effect that cannot be included is a model silently missing
      ## that effect (the B1 defect class): every result would be reported for a
      ## specification the user did not ask for. All branches below route their
      ## failures here, which stops by default; the generic fallback's opt-out
      ## options(saomnk.skip_missing_effects = TRUE) also covers them.
      .effect_unavailable <- function(msg) {
        if (isTRUE(getOption("saomnk.skip_missing_effects", FALSE))) {
          warning(paste(msg, "(skipping: saomnk.skip_missing_effects = TRUE)"), call. = FALSE)
        } else {
          stop(paste0(msg, "\n  The model would otherwise run WITHOUT this effect. ",
                      "Fix the specification, or set ",
                      "options(saomnk.skip_missing_effects = TRUE) to skip it."),
               call. = FALSE)
        }
        invisible(NULL)
      }

      ## ---- Theta-storage convention (2026-08-23) ---------------------------
      ## The coefficient (theta) is carried in the effects table's
      ## `initialValue` column, which get_theta_matrix() reads and hands to
      ## siena07(thetaValues=). RSiena's `setEffect(parameter=)` writes the
      ## `parm` column -- the INTERNAL effect parameter, i.e. the `#`
      ## substitution in effect and function names (a root exponent for
      ## cycle4, inPopX, outActX, and ~100 other effects). Writing a
      ## coefficient there does not set a coefficient: it changes WHICH
      ## statistic is computed (cycle4 at "coefficient" 0.3 becomes
      ## count^(1/0.3) = count^3.33). `parameter=` is therefore passed ONLY
      ## when the caller explicitly requests it via the `internal_parameter`
      ## key of the effect entry (e.g. cycle4 with internal_parameter = 2 for
      ## the square-root form). The public structure-model key `parameter`
      ## keeps meaning the coefficient; it lands in `initialValue`.
      ##
      ## `.set_theta()` is the single place this convention is applied, so the
      ## nineteen effect branches below cannot drift apart again.
      .set_theta <- function(short, ..., type = NULL, required = TRUE) {
        th <- eff$parameter %||% eff$initialValue
        if (is.null(th) && required)
          stop(sprintf(paste0("Effect '%s' declares no coefficient. Set `parameter=` ",
                              "in its structure-model entry (use `internal_parameter=` ",
                              "only for RSiena's internal '#' parameter)."),
                       eff$effect), call. = FALSE)
        args <- list(self$rsiena_effects, shortName = short, character = TRUE,
                     name = eff$dv_name, fix = fix, verbose = verbose, ...)
        if (!is.null(type))                    args$type         <- type
        if (!is.null(th))                      args$initialValue <- th
        if (!is.null(eff$internal_parameter))  args$parameter    <- eff$internal_parameter
        do.call(setEffect, args)
      }

      ##---------- 1 Efect --------------------------
      if (eff$effect == 'Rate')
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  Rate, ## get network statistic function from effect name (character)
                                              name = eff$dv_name,  # interaction1 = eff$interaction1,
                                              fix = fix,
                                              type='rate', verbose = verbose)
        self$rsiena_effects <- .set_theta('Rate', type = 'rate')
      }
      ## HETEROGENEOUS / STRUCTURAL RATE EFFECTS
      ## RateX  : rate depends on an actor covariate  (RSiena group `covarBipartiteRate`)
      ## outRate*/inRate* : rate depends on the actor's own degree (`bipartiteRate`)
      ## These make the FREQUENCY of change actor-specific, as distinct from the evaluation
      ## function, which governs WHICH change is preferred. Required for modeling
      ## heterogeneous adjustment / repositioning costs.
      else if (eff$effect %in% c('RateX', 'outRate', 'outRateInv', 'outRateLog',
                                 'inRateInv', 'inRateLog'))
      {
        .needs_cov <- identical(eff$effect, 'RateX')
        if (.needs_cov && (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1)))) {
          .effect_unavailable("'RateX' effect cannot be included: no interaction1 (actor covariate) specified. Declare a covariate first.")
        } else {
          tryCatch({
            .args_inc <- list(self$rsiena_effects, eff$effect, character = TRUE,
                              name = eff$dv_name, type = 'rate',
                              fix = fix, verbose = verbose)
            if (.needs_cov) .args_inc$interaction1 <- eff$interaction1
            self$rsiena_effects <- do.call(includeEffects, .args_inc)

            ## Coefficient -> `initialValue` (see the theta-storage note above).
            self$rsiena_effects <- if (.needs_cov) {
              .set_theta(eff$effect, type = 'rate', interaction1 = eff$interaction1)
            } else {
              .set_theta(eff$effect, type = 'rate')
            }
          }, error = function(e) {
            .effect_unavailable(sprintf("'%s' rate effect failed: %s (is covariate '%s' registered?)",
                            eff$effect, e$message,
                            if (is.null(eff$interaction1)) '<none>' else eff$interaction1))
          })
        }
      }
      else if (eff$effect == 'density')
      {
        ## RSiena's effects table carries a 'density' (outdegree) row for
        ## one-mode AND bipartite dependent variables: in RSiena 1.5.0,
        ## getEffects() on bipartite data lists density eval/endow/creation,
        ## with eval included by default. An earlier version of this branch
        ## claimed bipartite had no density and silently substituted 'outAct'
        ## (the sum of squared outdegrees, a different statistic). That
        ## substitution is removed: without a density row, stop rather than
        ## include an effect the user did not ask for.
        eff_tbl <- as.data.frame(self$rsiena_effects)
        has_density_row <- any(
          eff_tbl$name == eff$dv_name & eff_tbl$shortName == 'density'
        )
        if (!has_density_row) {
          stop(sprintf(paste0(
            "The effects table has no 'density' row for dependent variable '%s', ",
            "so the requested 'density' effect cannot be included. RSiena 1.5.0 ",
            "provides 'density' for one-mode and bipartite networks alike; check ",
            "the dependent variable name and the RSiena version (found %s). ",
            "searchnet no longer substitutes 'outAct', which is a different statistic."),
            eff$dv_name, as.character(utils::packageVersion("RSiena"))), call. = FALSE)
        }
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  density,
                                             name = eff$dv_name,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('density')
      }
      else if (eff$effect == 'inPop')
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, inPop, ## get network statistic function from effect name (character)
                                             name = eff$dv_name, # interaction1 = eff$interaction1,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('inPop')
      }
      else if (eff$effect == 'outAct')
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, outAct, ## get network statistic function from effect name (character)
                                             name = eff$dv_name, # interaction1 = eff$interaction1,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('outAct')
      }
      else if (eff$effect == 'outActSqrt')
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, outActSqrt, ## get network statistic function from effect name (character)
                                              name = eff$dv_name, # interaction1 = eff$interaction1,
                                              fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('outActSqrt')
      }
      else if (eff$effect == 'cycle4')
      {
        ## cycle4 is a '#'-carrying effect: its `parm` is a ROOT EXPONENT
        ## ((4-cycle count)^(1/parm)), NOT a coefficient. The coefficient goes
        ## to `initialValue` via .set_theta(); a caller who wants the
        ## square-root form asks for it with `internal_parameter = 2`.
        self$rsiena_effects <- includeEffects(self$rsiena_effects, cycle4, ## get network statistic function from effect name (character)
                                             name = eff$dv_name, # interaction1 = eff$interaction1,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('cycle4')
      }
      else if (eff$effect == 'transTriads')
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  transTriads, ## get network statistic function from effect name (character)
                                              name = eff$dv_name, # interaction1 = eff$interaction1,
                                              fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('transTriads')
      }
      # else if (eff$effect == 'cycle4ND') 
      # {
      #   self$rsiena_effects <- includeEffects(self$rsiena_effects, cycle4ND, ## get network statistic function from effect name (character)
      #                                         name = eff$dv_name, # interaction1 = eff$interaction1,
      #                                         fix = fix)
      #   self$rsiena_effects <- setEffect(self$rsiena_effects,  cycle4ND, 
      #                                    name = eff$dv_name, parameter = eff$parameter,  fix = fix)
      # }
      
      else if (eff$effect == 'totInDist2') 
      {
        ##**Takes an interaction (but confusing because no specified X in name to indicate covariate)**
        
        # activity_covar <- varCovar(activity_data)
        # effects <- includeEffects(effects, egoX, interaction1 = "activity_covar")
        
        seteffs <- self$get_rsiena_effects_theta_df()
        resuse_int_eff <- seteffs[which(seteffs$interaction1 == eff$reuse_interaction1), ]
        
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  totInDist2, ## get network statistic function from effect name (character)
                                              name = eff$dv_name,
                                              interaction1 = resuse_int_eff$interaction1,
                                              fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('totInDist2',
                                          interaction1 = resuse_int_eff$interaction1)
      }
      
      else if (eff$effect == 'simEgoInDist2') 
      {
        ##**Takes an interaction (but confusing because no specified X in name to indicate covariate)**
        
        # activity_covar <- varCovar(activity_data)
        # effects <- includeEffects(effects, egoX, interaction1 = "activity_covar")
        
        seteffs <- self$get_rsiena_effects_theta_df()
        resuse_int_eff <- seteffs[which(seteffs$interaction1 == eff$reuse_interaction1), ]
        
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  simEgoInDist2, ## get network statistic function from effect name (character)
                                              name = eff$dv_name,
                                              interaction1 = resuse_int_eff$interaction1,
                                              fix = fix, verbose = verbose)
        self$rsiena_effects <- .set_theta('simEgoInDist2',
                                          interaction1 = resuse_int_eff$interaction1)
      }
      
      else if (eff$effect %in% c('egoX', 'altX', 'outActX', 'altXOutAct', 'homXOutAct', 'inPopX'))
      {
        ## All covariate-dependent effects require interaction1 (a registered covariate name)
        if (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1))) {
          .effect_unavailable(sprintf("'%s' effect cannot be included: no interaction1 (covariate) specified. Declare a covariate first.", eff$effect))
        } else {
          tryCatch({
            ## NOTE: `shortName` is NOT a formal of includeEffects(); passing it there puts
            ## it in `...`, where RSiena deparses the unevaluated expression and looks for an
            ## effect literally named "eff$effect". Pass the name as the first `...` argument
            ## with character=TRUE instead. setEffect() DOES take shortName, but likewise
            ## deparses it unless character=TRUE.
            ## NOTE 2: the coefficient goes to `initialValue` (see the theta-storage
            ## note at the top of this function). This matters doubly here because
            ## inPopX, outActX and homXOutAct are '#'-carrying effects: writing the
            ## coefficient into `parm` would not only misroute theta, it would
            ## change the statistic itself (e.g. inPopX becomes
            ## indegree-pop.^(1/parm)).
            self$rsiena_effects <- includeEffects(self$rsiena_effects,
                                                  eff$effect,
                                                  character = TRUE,
                                                  name = eff$dv_name,
                                                  interaction1 = eff$interaction1,
                                                  fix = fix, verbose = verbose)
            self$rsiena_effects <- .set_theta(eff$effect,
                                              interaction1 = eff$interaction1)
          }, error = function(e) {
            .effect_unavailable(sprintf("'%s' effect failed: %s (is covariate '%s' registered?)",
                            eff$effect, e$message, eff$interaction1))
          })
        }
      }
      ## DYADIC COVARIATE EFFECT
      else if (eff$effect == 'XWX')
      {
        ## XWX requires a coDyadCovar registered as interaction1
        if (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1))) {
          .effect_unavailable("XWX effect cannot be included: no interaction1 (W-matrix covariate) specified. Configure W-matrix first.")
        } else {
          tryCatch({
            self$rsiena_effects <- includeEffects(self$rsiena_effects,  XWX,
                                                  name = eff$dv_name,
                                                  interaction1 = eff$interaction1,
                                                  fix = fix, verbose = verbose)
            self$rsiena_effects <- .set_theta('XWX',
                                              interaction1 = eff$interaction1)
          }, error = function(e) {
            .effect_unavailable(sprintf("XWX effect failed: %s (is the W-matrix registered as a coDyadCovar?)", e$message))
          })
        }
      }
      else if (eff$effect == 'X')
      {
        if (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1))) {
          .effect_unavailable("X effect cannot be included: no interaction1 covariate specified.")
        } else {
          tryCatch({
            self$rsiena_effects <- includeEffects(self$rsiena_effects,  X,
                                                  name = eff$dv_name,
                                                  interaction1 = eff$interaction1,
                                                  fix = fix, verbose = verbose)
            self$rsiena_effects <- .set_theta('X',
                                              interaction1 = eff$interaction1)
          }, error = function(e) {
            .effect_unavailable(sprintf("X effect failed: %s", e$message))
          })
        }
      }
      # else if (eff$effect == 'XWX1')
      # {
      #   self$rsiena_effects <- includeEffects(self$rsiena_effects,  XWX1, ## get network statistic function from effect name (character)
      #                                         name = eff$dv_name, 
      #                                         interaction1 = eff$interaction1,
      #                                         fix = fix)
      #   self$rsiena_effects <- setEffect(self$rsiena_effects,  XWX1, 
      #                                    interaction1 = eff$interaction1,
      #                                    name = eff$dv_name, parameter = eff$parameter,  fix = fix)
      # }
      # else if (eff$effect == 'XWX2')
      # {
      #   self$rsiena_effects <- includeEffects(self$rsiena_effects,  XWX2, ## get network statistic function from effect name (character)
      #                                         name = eff$dv_name, 
      #                                         interaction1 = eff$interaction1,
      #                                         fix = fix)
      #   self$rsiena_effects <- setEffect(self$rsiena_effects,  XWX2, 
      #                                    interaction1 = eff$interaction1,
      #                                    name = eff$dv_name, parameter = eff$parameter,  fix = fix)
      # }
      else
      {
        ## Generic fallback: try to include the effect by shortName directly
        ## This handles effects not in the explicit if/else chain above.
        ## See the covariate branch above for why `character = TRUE` is required on both
        ## calls: includeEffects() has no `shortName` formal, and setEffect() deparses
        ## `shortName` unless told the value is a character string.
        tryCatch({
          .type <- if (grepl('rate', eff$effect, ignore.case = TRUE)) 'rate' else 'eval'
          .args_inc <- list(self$rsiena_effects, eff$effect, character = TRUE,
                            name = eff$dv_name, type = .type,
                            fix = fix, verbose = verbose)
          if (!is.null(eff$interaction1) && nzchar(as.character(eff$interaction1)))
            .args_inc$interaction1 <- eff$interaction1
          ## Two-slot effects. Behavior effects such as `avXAlt` / `totXAlt`
          ## and the covariate distance-2 family (`avXInAltDist2`, ...) are
          ## identified by BOTH a covariate (interaction1) and the network
          ## through which it reaches ego (interaction2); without
          ## interaction2 RSiena cannot resolve them. No structure model
          ## predating behavior coevolution sets interaction2 on this path,
          ## so this is inert for them.
          if (!is.null(eff$interaction2) && nzchar(as.character(eff$interaction2)))
            .args_inc$interaction2 <- eff$interaction2
          self$rsiena_effects <- do.call(includeEffects, .args_inc)
          if (!is.null(eff$parameter) || !is.null(eff$initialValue) ||
              !is.null(eff$internal_parameter)) {
            ## Coefficient -> `initialValue`; `internal_parameter` (if any) ->
            ## RSiena's `parm`. See the theta-storage note at the top of this
            ## function: many generic effects carry '#' in their functionName,
            ## for which `parm` selects the statistic rather than scaling it.
            .args_extra <- list(type = .type, required = FALSE)
            if (!is.null(eff$interaction1) && nzchar(as.character(eff$interaction1)))
              .args_extra$interaction1 <- eff$interaction1
            if (!is.null(eff$interaction2) && nzchar(as.character(eff$interaction2)))
              .args_extra$interaction2 <- eff$interaction2
            self$rsiena_effects <- do.call(.set_theta, c(list(eff$effect), .args_extra))
          }
          if (verbose) cat(sprintf("  [generic] Included effect '%s'\n", eff$effect))
        }, error = function(e2) {
          ## Surface useful diagnostic: which shortNames ARE available for this DV
          eff_tbl <- tryCatch(as.data.frame(self$rsiena_effects), error = function(e) NULL)
          available_msg <- ""
          if (!is.null(eff_tbl)) {
            avail <- sort(unique(eff_tbl$shortName[eff_tbl$name == eff$dv_name]))
            if (length(avail) > 0) {
              available_msg <- sprintf(" Available shortNames for DV '%s': %s.",
                                       eff$dv_name, paste(avail, collapse = ", "))
            }
          }
          ## A skipped effect is a silently mis-specified model: the run
          ## proceeds without the effect the user asked for, and nothing
          ## downstream shows that it is missing. Effect names are also not
          ## portable across dependent-variable types -- egoXaltX is a one-mode
          ## effect and does not exist for a bipartite DV, where the
          ## dyadic-covariate effect is X -- so this is easy to hit by
          ## following one-mode examples.
          ##
          ## Default is therefore to STOP. Set
          ##   options(saomnk.skip_missing_effects = TRUE)
          ## to restore the old permissive behavior for exploratory work.
          msg <- sprintf("Effect '%s' could not be included: %s%s",
                         eff$effect, e2$message, available_msg)
          if (isTRUE(getOption("saomnk.skip_missing_effects", FALSE))) {
            warning(paste(msg, "(skipping)"))
          } else {
            stop(paste0(msg,
              "
  The model would otherwise run WITHOUT this effect. ",
              "Fix the effect name, or set ",
              "options(saomnk.skip_missing_effects = TRUE) to skip it."),
              call. = FALSE)
          }
        })
      }
      
    },
    
    
    
    
    # ##**altX**
    # component_cov <- self$rsiena_data$cCovars[[ interact$interaction1 ]]
    # if (attr(component_cov, 'centered')) {
    #   ## return to uncentered original data
    #   component_cov <-  component_cov + attr(component_cov, 'mean')
    # }
    
    # ##**X**
    # dyad_cov <- self$rsiena_data$dycCovars[[ interact$interaction1 ]]
    # if (attr(component_cov, 'centered')) {
    #   ## return to uncentered original data
    #   dyad_cov <-  dyad_cov + attr(dyad_cov, 'mean')
    # }
    # ##**XWX**
    # component_dyad_cov <- self$rsiena_data$dycCovars[[ interact$interaction2 ]]
    # if ( all(diag(component_dyad_cov)==0)  &  all(c(component_dyad_cov) %in% c(0,1)) ) {
    #   diag(component_dyad_cov) <- 1
    # }
    # ##
    # inter_mat <- outer(component_cov, component_cov, "*") * component_dyad_cov
    #
    
    include_rsiena_effect_from_eff_list_static = function(rsiena_effects, eff, unfix_all=TRUE, verbose=FALSE) {
      
      if (grepl('rate',eff$effect, ignore.case = TRUE)) {
        cat(sprintf('**NOTE** skipping rate effect `%s`',eff$effect))
        return(rsiena_effects)
      }
      #
      dv_name <- gsub('self\\$','',eff$dv_name, ignore.case = TRUE)
      #
      ## Theta-storage convention (2026-08-23): the declared coefficient goes
      ## to `initialValue` -- here, on the ESTIMATION path, it serves as the
      ## warm-start value for siena07. `parameter=` (RSiena's internal '#'
      ## parameter, which for '#'-carrying effects such as cycle4 / inPopX /
      ## outActX selects WHICH statistic is computed) is passed only when the
      ## caller explicitly sets `internal_parameter`. The previous code wrote
      ## the coefficient into `parameter=`, which for those effects estimated
      ## a transformed statistic (e.g. count^(1/coefficient)) without saying so.
      .fix_arg <- ifelse(unfix_all, FALSE, eff$fix)
      .args_set <- list(rsiena_effects, shortName = eff$effect, character = TRUE,
                        name = dv_name, fix = .fix_arg, verbose = verbose)
      if (!is.null(eff$parameter %||% eff$initialValue))
        .args_set$initialValue <- eff$parameter %||% eff$initialValue
      if (!is.null(eff$internal_parameter))
        .args_set$parameter <- eff$internal_parameter
      if (is.null(eff$interaction1)) {
        rsiena_effects <- includeEffects(rsiena_effects,  eff$effect,
                                         name = dv_name,  # interaction1 = eff$interaction1,
                                         fix = .fix_arg,
                                         character = TRUE, verbose=verbose)
      } else {
        interact1 <- gsub('self\\$','',eff$interaction1, ignore.case = TRUE)
        rsiena_effects <- includeEffects(rsiena_effects,  eff$effect,
                                         name = dv_name,  # interaction1 = eff$interaction1,
                                         fix = .fix_arg,
                                         interaction1 = interact1,
                                         character = TRUE, verbose=verbose)
        .args_set$interaction1 <- interact1
      }
      if (!is.null(.args_set$initialValue) || !is.null(.args_set$parameter)) {
        .args_set[[1]] <- rsiena_effects
        rsiena_effects <- do.call(setEffect, .args_set)
      }

      return(rsiena_effects)
    },
    
    
    
    
    include_rsiena_interaction_from_eff_list_static = function(rsiena_effects, interact, unfix_all=TRUE, verbose=FALSE) {
      #
      if ( ! 'manual_interaction' %in% names(rsiena_effects) ) {
        rsiena_effects$manual_interaction <- NA
      }
      #
      dv_name <- gsub('self\\$','',interact$dv_name, ignore.case = TRUE)
      #
      if (is.null(interact$interaction1)) {
        rsiena_effects <- includeInteraction(rsiena_effects, interact$effects[1], interact$effects[2],  ## get network statistic function from effect name (character)
                                           name = dv_name, # interaction1 = eff$interaction1,
                                           include=TRUE,
                                           initialValue = interact$parameter,
                                           fix = ifelse(unfix_all, FALSE, interact$fix),
                                           character = TRUE, verbose = verbose)
      } else {
        interact1 <- c(
          gsub('self\\$','',eff$interaction1, ignore.case = TRUE),
          gsub('self\\$','',eff$interaction2, ignore.case = TRUE)
        )
        rsiena_effects <- includeInteraction(rsiena_effects, interact$effects[1], interact$effects[2],  ## get network statistic function from effect name (character)
                                             name = dv_name, # interaction1 = eff$interaction1,
                                             include=TRUE,
                                             initialValue = interact$parameter,
                                             fix = ifelse(unfix_all, FALSE, interact$fix),
                                             interaction1 = interact1, ##**interaction**
                                             character = TRUE, verbose = verbose)
      }

      ## This workaround adjusts parameter of interaction without name
      ##**TODO: Check for RSiena official implementation**
      ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
      rsiena_effects[rsiena_effects$include,][sum(rsiena_effects$include),'manual_interaction'] <- interact$effect
      ##
      return(rsiena_effects)
    },
    
    
    
    
    include_rsiena_interaction_from_eff_list = function(interact, verbose=FALSE, fix=TRUE) {
      #
      # interact$effect
      #
      cat('\n===DEBUG===\n'); print(interact)
      #
      if ( ! 'manual_interaction' %in% names(self$rsiena_effects) ) {
        self$rsiena_effects$manual_interaction <- NA
      }
      #
      if(all(c('totInDist2','X') %in% interact$effects))  {
        #
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, totInDist2, X,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix,
                                                  interaction1 = c(interact$interaction1, interact$interaction2), 
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
      
      } else if(all(c('inPop','altX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, inPop, altX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, 
                                                  interaction1 =interact$interaction1, 
                                                  # interaction2 =interact$interaction2, 
                                                  include=TRUE,  # parameter = interact$parameter,
                                                  initialValue = interact$parameter,
                                                  fix = fix, 
                                                  verbose=verbose)
        #########################
        # myeff_rows <- self$rsiena_effects[ self$rsiena_effects$effect1!=0 | self$rsiena_effects$effect2!=0, ]
        # if (nrow(myeff_rows) > 1) 
        #   cat('\n======= WARNING ======\n multiple interactions included; need to double check the theta_matrix parameters and shocks')
        # #
        # self$rsiena_effects <- setEffect(self$rsiena_effects, 
        #                                  shortName=NULL, 
        #                                  effect1 = myeff_rows$effect1[1], ## just use first interaction
        #                                  effect2 = myeff_rows$effect2[1],
        #                                  # effect3 = ,
        #                                  name = interact$dv_name,
        #                                  interaction1 = interact$interaction1,
        #                                  parameter = interact$parameter,
        #                                  fix = fix, verbose = verbose)
        #######################
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
      } else if(all(c('outAct','egoX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, outAct, egoX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, 
                                                  interaction1 =interact$interaction1, 
                                                  # interaction2 =interact$interaction2, 
                                                  include=TRUE,  # parameter = interact$parameter,
                                                  initialValue = interact$parameter,
                                                  fix = fix, 
                                                  verbose=verbose)
        #########################
        # myeff_rows <- self$rsiena_effects[ self$rsiena_effects$effect1!=0 | self$rsiena_effects$effect2!=0, ]
        # if (nrow(myeff_rows) > 1) 
        #   cat('\n======= WARNING ======\n multiple interactions included; need to double check the theta_matrix parameters and shocks')
        # #
        # self$rsiena_effects <- setEffect(self$rsiena_effects, 
        #                                  shortName=NULL, 
        #                                  effect1 = myeff_rows$effect1[1], ## just use first interaction
        #                                  effect2 = myeff_rows$effect2[1],
        #                                  # effect3 = ,
        #                                  name = interact$dv_name,
        #                                  interaction1 = interact$interaction1,
        #                                  parameter = interact$parameter,
        #                                  fix = fix, verbose = verbose)
        #######################
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
        
      } else if(all(c('outActSqrt','egoX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, outActSqrt, egoX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, 
                                                  interaction1 =interact$interaction1, 
                                                  # interaction2 =interact$interaction2, 
                                                  include=TRUE,  # parameter = interact$parameter,
                                                  initialValue = interact$parameter,
                                                  fix = fix, 
                                                  verbose=verbose)
        #########################
        # myeff_rows <- self$rsiena_effects[ self$rsiena_effects$effect1!=0 | self$rsiena_effects$effect2!=0, ]
        # if (nrow(myeff_rows) > 1) 
        #   cat('\n======= WARNING ======\n multiple interactions included; need to double check the theta_matrix parameters and shocks')
        # #
        # self$rsiena_effects <- setEffect(self$rsiena_effects, 
        #                                  shortName=NULL, 
        #                                  effect1 = myeff_rows$effect1[1], ## just use first interaction
        #                                  effect2 = myeff_rows$effect2[1],
        #                                  # effect3 = ,
        #                                  name = interact$dv_name,
        #                                  interaction1 = interact$interaction1,
        #                                  parameter = interact$parameter,
        #                                  fix = fix, verbose = verbose)
        #######################
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
        
      } else if(all(c('outAct','inPop') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, outAct, inPop,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix, 
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
        
      } else if(all(c('egoX','XWX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, egoX, XWX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix,
                                                  interaction1 = interact$interaction1,
                                                  # interaction2 = interact$interaction2,
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
      } else if(all(c('inPopX','X') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, inPopX, X,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix,
                                                  interaction1 = c(interact$interaction1, interact$interaction2),
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
     
      } else if(all(c('egoX','altX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, egoX, altX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix,
                                                  interaction1 = c(interact$interaction1, interact$interaction2),
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = interact$fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
           
      } else if(all(c('inPopX','egoX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, inPopX, egoX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix,
                                                  interaction1 = c(interact$interaction1, interact$interaction2),
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
      } else if(all(c('inPopX','altX') %in% interact$effects))  {
        
        self$rsiena_effects <- includeInteraction(self$rsiena_effects, inPopX, altX,  ## get network statistic function from effect name (character)
                                                  name = interact$dv_name, # interaction1 = eff$interaction1,
                                                  # parameter = interact$parameter,
                                                  include=TRUE,
                                                  initialValue = interact$parameter,
                                                  fix = fix,
                                                  interaction1 = c(interact$interaction1, interact$interaction2),
                                                  verbose=verbose)
        ## This workaround adjusts parameter of interaction without name
        ##**TODO: Check for RSiena official implementation**
        ## NEED TO SET INTERACTNG VARIABLE NAMES HERE FOR USE IN LATER COMPUTATIONS (theta_matrix)
        self$rsiena_effects[self$rsiena_effects$include,][sum(self$rsiena_effects$include),'manual_interaction'] <- interact$effect
        # self$rsiena_effects <- setEffect(self$rsiena_effects, unspInt,
        #                                  name = interact$dv_name,
        #                                  parameter = interact$parameter,
        #                                  fix = fix,
        #                                  interaction1 = c(interact$interaction1, interact$interaction2))  #,
        
      }  else {
        
        stop(sprintf('Interaction `%s*%s` not yet implemented.', interact$effects[1], interact$effects[2]))
        
      }


    },
    
    
    
    # ##-----------------
    getTotInDist2 = function(bipartite_matrix, actor_covariate, interaction_type = "absdiff") {
      # Ensure input is a matrix
      if (!is.matrix(bipartite_matrix)) {
        stop("Bipartite network must be an M C N matrix.")
      }

      # Ensure actor covariate is a vector of length M
      M <- nrow(bipartite_matrix)
      if (length(actor_covariate) != M) {
        stop("Actor covariate must be a vector of length M (number of actors).")
      }

      # Step 1: Compute the N C N projection (component adjacency matrix)
      A <- t(bipartite_matrix) %*% bipartite_matrix  # Project bipartite network onto components
      diag(A) <- 0  # Remove self-loops

      # Step 2: Compute the dyadic transformation of the actor covariate
      actor_matrix <- outer(actor_covariate, actor_covariate, FUN=switch(
        interaction_type,
        "absdiff" = function(x, y) abs(x - y),
        "product" = function(x, y) x * y,
        "sum" = function(x, y) x + y,
        stop("Invalid interaction type. Choose 'absdiff', 'product', or 'sum'.")
      ))

      # Step 3: Apply the transformed covariate to weight second-degree paths
      A_squared <- A %*% A  # A^2 counts 2-step walks
      weighted_A2 <- A_squared * (t(bipartite_matrix) %*% actor_matrix %*% bipartite_matrix)

      # Step 4: Compute totInDist2 statistic as the sum of incoming 2-step weighted paths
      totInDist2_values <- rowSums(weighted_A2)

      # Return as named vector
      names(totInDist2_values) <- paste0("Component_", seq_along(totInDist2_values))

      return(totInDist2_values)
    },
    
    ## Number of dependent variables in the current RSiena data object.
    ## RSiena estimates CONDITIONALLY with exactly one DV (which deletes the
    ## conditioning DV's basic rate from theta) and UNCONDITIONALLY with two or
    ## more (which keeps every basic rate in theta). The theta matrix width
    ## follows from this, so several call sites need to ask.
    get_n_rsiena_depvars = function() {
      if (is.null(self$rsiena_data) || is.null(self$rsiena_data$depvars))
        return(1L)
      length(self$rsiena_data$depvars)
    },

    ## The subset of theta columns that belong to the BIPARTITE network's
    ## evaluation function: the effects whose per-actor statistics the utility
    ## and K-4 decompositions know how to compute. Excludes basic rates and,
    ## when a behavior DV coevolves, that DV's effects -- `linear`, `quad`,
    ## `avInSimDist2` and the rest are statistics of the behavior, not of the
    ## bipartite matrix, and have no decomposition on this path.
    ##
    ## For a single-DV model this returns exactly what
    ## get_rsiena_effects_theta_df(no_rates = TRUE) has always returned.
    get_bipartite_effects_theta_df = function() {
      df <- self$get_rsiena_effects_theta_df(
        no_rates = !(self$get_n_rsiena_depvars() > 1L))
      df[ df$name == 'self$bipartite_rsienaDV' &
            !(df$shortName == 'Rate' & df$type == 'rate'), , drop = FALSE ]
    },

    ##
    get_rsiena_effects_theta_df = function(no_rates=TRUE) {
      if (is.null(self$rsiena_effects))
        stop('self$rsiena_effects is not yet set.')
      ##
      theta_df <- as.data.frame( self$rsiena_effects[self$rsiena_effects$include, ] )
      ## Effect key
      theta_df$effect_key <- sapply(1:nrow(theta_df), function(i){
        paste(c(theta_df$name[i], ## DV name
                theta_df$shortName[i], ## effect name
                ifelse(theta_df$interaction1[i]!='', theta_df$interaction1[i], NA), ## interactions make unique effect key
                ifelse(theta_df$interaction2[i]!='', theta_df$interaction2[i], NA)  ## interactions make unique effect key
                ),collapse = '::')
      })
      ### Effect level names for facet plotting (handle multiple of same effect)
      theta_df$effect_level <- theta_df$shortName
      dups <- plyr::count(theta_df$shortName) %>% filter(freq > 1)
      if (nrow(dups)) {
        for (i_row in 1:nrow(dups)) {
          rename_ids <- which(theta_df$shortName == dups$x[i_row] )
          theta_df$effect_level[ rename_ids ] <- paste(dups$x[i_row], 1:dups$freq[i_row], sep='_')
        }
      }
      ## Handle Interactions
      theta_df$effect_level_orig <- theta_df$effect_level
      for (i in 1:nrow(theta_df)) {
        if ( grepl('.*unspInt.*',  theta_df$effect_level[i], ignore.case = TRUE) ) {
          theta_df$effect_level[i] <- theta_df$manual_interaction[i]
        }
      }
      ##
      ## Drop ONLY the basic rate parameter(s), which RSiena handles separately and which
      ## are therefore absent from the theta / thetaValues vector. Non-basic rate effects
      ## (RateX, outRate, outRateInv, outRateLog, inRateInv, inRateLog) DO occupy theta
      ## columns, so a blanket /rate/i filter under-counts the columns and RSiena rejects
      ## the thetaValues matrix ("should have N columns").
      ## Backward-compatible: for a model whose only rate effect is the basic `Rate`, this
      ## removes exactly the same row the old regex did.
      if (no_rates) {
        .is_basic_rate <- theta_df$shortName == 'Rate'
        if ('type' %in% names(theta_df))
          .is_basic_rate <- .is_basic_rate & theta_df$type == 'rate'
        theta_df <- theta_df[ ! .is_basic_rate , , drop = FALSE ]
      }
      #
      return(theta_df)
    },
    
    # get_cov_type_from_structure_model = function(rsiena_effect_item, cov_type, structure_model=NULL) {
    #   
    # },
    
    get_cov_data = function(rsiena_effect_item, structure_model=NULL) {
      if (is.null(structure_model)) {
        struture_model <- self$config_structure_model
      }
      dv <- rsiena_effect_item$name
      #
      interact1 <- gsub('self\\$', '', rsiena_effect_item$interaction1)
      interact2 <- gsub('self\\$', '', rsiena_effect_item$interaction2)
      #
      if(is.null(self[[interact1]])) 
        stop(sprintf('Covariate not set for interaction1 = `self$%s`.', interact1))
      if( interact2 != "" || grepl('[|]', interact2) ) 
        stop(sprintf('DEBUG handling of 2 interactions: interaction1=`%s` and interaction2=`%s`.', interact1, interact2))
      #
      covar <- self[[interact1]]
      #
      return( covar )
      
    },
    
    # type <- switch(type,
    #                effects = ,
    #                coCovars = ,
    #                varCovars = NA
    #                )
    
    # struture_model[[dv]][[]]
    
    # for (i in 1:length(structure_model)) {
    #   if (length(structure_model[[ i ]]$effects)) {
    #     for (j in 1:length(structrue_model[[ i ]]$effects))}{
    #       if 
    #     }
    #   }
    # }
    
    ##
    ## The effects table get_struct_mod_stats_mat_from_bi_mat() reads, and its
    ## rows and its empty statistics matrix, built once. They depend only on
    ## the model and M, not on the state.
    prepare_struct_mod_stats = function() {
      theta_df_norates <- self$get_bipartite_effects_theta_df()
      theta_df_norates$effect <-  theta_df_norates$shortName
      neffs <- nrow(theta_df_norates)
      mat <- matrix(rep(0, self$M * neffs ), nrow=self$M, ncol=neffs )
      colnames(mat) <- theta_df_norates$effect_level
      rownames(mat) <- 1:self$M
      items <- lapply(seq_len(neffs), function(i) theta_df_norates[ i , ])
      ## Each covariate effect's covariate, and for XWX its numeric W and
      ## diag(W): fixed for the model, so fetched once rather than at every
      ## ministep (2026-10-10). The statistics use the same values either way.
      .cov_effects <- c('egoX', 'altX', 'outActX', 'inPopX', 'XWX', 'X',
                        'totInDist2', 'simEgoInDist2')
      covars <- lapply(items, function(item)
        if (item$effect %in% .cov_effects) self$get_cov_data(item) else NULL)
      xwx_W <- lapply(seq_len(neffs), function(i) {
        if (items[[i]]$effect != 'XWX') return(NULL)
        covar <- covars[[i]]
        W <- matrix(as.numeric(covar), nrow = nrow(covar), ncol = ncol(covar))
        list(W = W, diag = diag(W))
      })
      list(theta_df = theta_df_norates,
           items = items,
           covars = covars,
           xwx_W = xwx_W,
           mat_template = mat)
    },

    ## `.prep`: the value of prepare_struct_mod_stats(), for callers that
    ## evaluate many states of one model (the ministep-chain replay). It only
    ## skips rebuilding the effects table, its rows and the empty statistics
    ## matrix on every call (with `.prep = NULL` they are built here); the
    ## statistics are computed by the same expressions either way.
    get_struct_mod_stats_mat_from_bi_mat = function(bi_env_mat, type='all', .cache=NULL, .prep=NULL) {
      #
      ## Bipartite-network effects only: this function computes statistics OF
      ## bi_env_mat, and a coevolving behavior DV's effects are not statistics
      ## of it. Identical to the previous call for single-DV models.
      ## Without `.prep` the table is built here, as it always was (callers
      ## and tests that supply a minimal `self` rely on that).
      if (is.null(.prep)) {
        theta_df_norates <- self$get_bipartite_effects_theta_df()
        theta_df_norates$effect <-  theta_df_norates$shortName
      } else {
        theta_df_norates <- .prep$theta_df
      }
      #
      ## --- Intermediate result cache (Task 2 optimization) ---
      ## Compute commonly needed matrices once; reuse across effect computations.
      ## Caller may supply .cache for repeated calls with the same bi_env_mat.
      if (is.null(.cache)) {
        .cache <- list(
          row_sums = rowSums(bi_env_mat, na.rm = TRUE),
          col_sums = colSums(bi_env_mat, na.rm = TRUE),
          M = nrow(bi_env_mat),
          N = ncol(bi_env_mat)
        )
        ## Lazy-compute expensive matrices only when needed (set to NULL as sentinel)
        .cache$social   <- NULL  # bi_env_mat %*% t(bi_env_mat)  -- M x M
        .cache$epistasis <- NULL # t(bi_env_mat) %*% bi_env_mat  -- N x N
      }
      xActorDegree      <- .cache$row_sums
      xComponentDegree  <- .cache$col_sums
      #
      # eff <- m1$rsiena_effects[m1$rsiena_effects$include, ]
      # efflist <- c(
      #   self$config_structure_model$dv_bipartite$effects,
      #   self$config_structure_model$dv_bipartite$coCovars,
      #   self$config_structure_model$dv_bipartite$varCovars,
      #   self$config_structure_model$dv_bipartite$coDyadCovars,
      #   self$config_structure_model$dv_bipartite$varDyadCovars,
      #   self$config_structure_model$dv_bipartite$interactions
      # )
      #
      neffs <- nrow(theta_df_norates)
      ### empty matrix to hold actor network statistics
      effnames <- theta_df_norates$shortName ## sapply(efflist, function(x) x$effect, simplify = T)
      ## Theta-storage convention (2026-08-23): coefficients live in
      ## `initialValue`, never in `parm` (RSiena's internal '#' parameter).
      effparams <- theta_df_norates$initialValue ##sapply(efflist, function(x) x$parameter, simplify = T)
      #
      ## The empty statistics matrix, with its dimnames, depends only on the
      ## model and M: built once by prepare_struct_mod_stats().
      if (!is.null(.prep)) {
        mat <- .prep$mat_template
      } else {
        mat <- matrix(rep(0, self$M * neffs ), nrow=self$M, ncol=neffs )
        colnames(mat) <- theta_df_norates$effect_level
        rownames(mat) <- 1:self$M
      }
      #
      ## The helpers are package-level functions (2026-10-10) rather than
      ## closures rebuilt at every call, which the ministep-chain replay makes
      ## once per ministep: .searchnet_rsiena_centered() and
      ## .searchnet_stats_covar(). The social projection for cycle4 is
      ## computed in place (lazily, as before).
      .rsiena_centered <- .searchnet_rsiena_centered
      #
      for (i in 1:neffs)
      {
        # print(i)
        item <- if (is.null(.prep)) theta_df_norates[ i , ] else .prep$items[[ i ]]
        # item <- efflist[[ i ]]
        # print('DEBUG  get_struct_mod_stats_mat_from_bi_mat() ')
        # print(item)
        
        ## network statistics dataframe
        #
        if (item$effect == 'density' )  {
          
          mat[ , i] <- c( xActorDegree )
          
        } else if (item$effect == 'outAct' )  {
          
          mat[ , i] <- c( xActorDegree^2 )
          
        } else if (item$effect == 'outActSqrt' )  {
          
          mat[ , i] <- c( xActorDegree * sqrt(xActorDegree) )  ## ~10x more efficient than c( xActorDegree^1.5 )
          
        } else if (item$effect == 'inPop' ) {
          
          ## s_i = sum_j x_ij x_+j. xComponentDegree is colSums(bi_env_mat), which
          ## already counts ego, so the former `+ 1` counted ego twice. Sum over
          ## actors equals RSiena 1.5.0's siena07 target sum_j x_+j^2 (2026-09-15,
          ## tests/testthat/test-structural-stats-vs-rsiena.R).
          mat[ , i] <- c( bi_env_mat %*% xComponentDegree )

          # else if (item$effect == 'transTriads' ) {
          #   stat <-
        } else if (item$effect == 'inPopSqrt' ) {

          ## s_i = sum_j x_ij sqrt(x_+j), ego counted in x_+j (RSiena 1.5.0 target
          ## sum_j x_+j^1.5). Previously fell through to "not yet implemented" and
          ## left the column at 0.
          mat[ , i] <- rowSums( bi_env_mat * rep(sqrt(xComponentDegree), each = self$M) )

        } else if (item$effect == 'cycle4' ) {

            ## s_i = (1/2) sum_{k != i} choose(ov_ik, 2), ov = B %*% t(B) with the
            ## diagonal zeroed. Summed over actors this is the number of bipartite
            ## four-cycles, RSiena 1.5.0's cycle4 target (parameter 1). The former
            ## rowSums((BB')^2 * BB') / 2 kept the diagonal and counted degenerate
            ## closed walks.
            if (is.null(.cache$social)) .cache$social <- bi_env_mat %*% t(bi_env_mat)
            ov <- .cache$social   ## cached; the local copy is modified, not the cache
            diag(ov) <- 0
            mat[ , i] <- rowSums( choose(ov, 2) ) / 2

        } else if (item$effect == 'egoX') {
          
          # covar <- item$x
          covar <- .searchnet_stats_covar(self, .prep, i, item)
          checkConform <-  all(
            (  ## A or B
              class(covar) %in% c('array','matrix') & nrow(covar)==self$M
            ) | ( 
              length(covar)==self$M  ## array, matrix; vector
            )
          )
          if( ! checkConform )
            stop('egoX covar not conformable for multiplication given number of actors')
          ## s_i = x_i+ (v_i - vbar), the covariate centered as RSiena centers it;
          ## sums to RSiena 1.5.0's egoX target.
          mat[ , i] <- c( .rsiena_centered(covar) * xActorDegree )

        } else if (item$effect == 'altX') {
          
          # covar <- item$x
          covar <- .searchnet_stats_covar(self, .prep, i, item)
          checkConform <-  all(
            (  ## A or B
              class(covar) %in% c('array','matrix') & nrow(covar)==self$M
            ) | ( 
              length(covar)==self$N  ## COMPONENT array, for ACTOR statistic 
            )
          )
          if( ! checkConform )
            stop('altX covar not conformable for multiplication given number of components or actors')
          ## s_i = sum_j x_ij (v_j - vbar), the component covariate centered;
          ## sums to RSiena 1.5.0's altX target.
          covarComponentMat <- matrix(rep(.rsiena_centered(covar), self$M), nrow=self$M, ncol=self$N, byrow = TRUE)
          mat[ , i] <- rowSums( covarComponentMat * bi_env_mat, na.rm=TRUE ) ##**vector element-wise multiplication by rows of covar matrix, or elements of covar array
          
        } else if (item$effect == 'outActX') { ## interaction1 component_coCovar
          
          ## N-vector of component covariate
          # covar <- item$x
          covar <- .searchnet_stats_covar(self, .prep, i, item)
          # MxN matrix of row-stacked component covariate (repeated for each actor)
          covarComponentMat <- matrix(rep(.rsiena_centered(covar), self$M), nrow=self$M, ncol=self$N, byrow = TRUE)
          ## s_i = x_i+ sum_j x_ij (v_j - vbar); sums to RSiena 1.5.0's outActX
          ## target for a component covariate at internal parameter 1.
          mat[ , i] <- xActorDegree * rowSums( covarComponentMat * bi_env_mat, na.rm = TRUE)
        
        } else if (item$effect == 'inPopX') { #M-vector of actor strategy covars

          ## RSiena 1.5.0's two-mode inPopX with an actor covariate v at
          ## internal parameter 1 ("ind. pop.^(1/#) weighted v"):
          ##   s_i = sum_j x_ij w_j,  w_j = sum_h x_hj v~_h  (ego counted),
          ## v~ centered as RSiena centers it (unless declared uncentered).
          ## Until 2026-10-08 this column computed rowSums(B * w), which
          ## recycles the N-vector w down B's columns (column-major), so actor
          ## i's row was weighted by the wrong components' w. RSiena 1.5.0's
          ## own siena07 target for this effect is not deterministic: on
          ## repeated identical calls it returns either the sum above or the
          ## sum with the LAST component's term omitted
          ## (tests/testthat/test-structural-stats-vs-rsiena.R records this).
          ## Only the internal parameter 1 (no root) is implemented here.
          covar <- .searchnet_stats_covar(self, .prep, i, item)
          w_j <- colSums(bi_env_mat * .rsiena_centered(covar), na.rm = TRUE)
          mat[ , i] <- as.numeric(bi_env_mat %*% w_j)
        
        } else if (item$effect == 'XWX') { ## NxN
          
          ## W and diag(W) as numeric matrices; built once per chain with `.prep`.
          if (is.null(.prep)) {
            covar <- .searchnet_stats_covar(self, .prep, i, item)
            W <- matrix(as.numeric(covar), nrow = nrow(covar), ncol = ncol(covar))
            W_diag <- diag(W)
          } else {
            W <- .prep$xwx_W[[i]]$W
            W_diag <- .prep$xwx_W[[i]]$diag
          }
          ## Within-ego: s_i = sum_{j != h} x_ij x_ih w_hj
          ##           = sum_j x_ij (B W)_ij - sum_j x_ij w_jj.
          ## The former rowSums(B W B') summed over every actor k, not ego alone.
          ## Summed over actors this equals RSiena 1.5.0's XWX siena07 target,
          ## including for asymmetric W with a nonzero diagonal. The raw W is used:
          ## coDyadCovar's `centered` flag does not change RSiena's XWX target
          ## (verified 2026-09-15), although it can affect other effects that
          ## read the same covariate.
          mat[ , i] <- rowSums( (bi_env_mat %*% W) * bi_env_mat, na.rm=TRUE ) -
            c( bi_env_mat %*% W_diag )
          
        }  else if (item$effect == 'X') { ## MxN
          
          # covar <- item$x
          covar <- .searchnet_stats_covar(self, .prep, i, item)
          ## s_i = sum_j x_ij (w_ij - wbar) for an actor x component dyadic
          ## covariate centered on its overall mean, as RSiena centers it; sums to
          ## RSiena 1.5.0's X target. The uncentered form, marked TODO, stood here
          ## until 2026-10-04.
          W <- matrix(.rsiena_centered(covar), nrow = nrow(covar), ncol = ncol(covar))
          mat[ , i] <- rowSums( bi_env_mat * W, na.rm=TRUE )

          
        }   else if (item$effect == 'totInDist2') { 
          
          ## s_i = sum_j x_ij sum_{h != i} x_hj (v_h - vbar): the centered
          ## covariate summed over the OTHER holders of each component ego holds,
          ## once per shared component. Sums to RSiena 1.5.0's totInDist2 target.
          ## Until 2026-10-04 this used the raw covariate and counted ego among
          ## the holders.
          v <- .rsiena_centered(.searchnet_stats_covar(self, .prep, i, item))
          holder_sums <- c( v %*% bi_env_mat )   ## sum_h x_hj v_h, ego included
          mat[ , i ] <- c( bi_env_mat %*% holder_sums ) - xActorDegree * v


        }   else if (item$effect == 'simEgoInDist2') {

          ## RSiena 1.5.0's two-mode simEgoInDist2 (actor covariate v):
          ##   s_i = sum_j x_ij [ 1 - |v_i - vbar_j^(-i)| / R - simMean ],
          ## vbar_j^(-i) the mean of v over the OTHER holders of j, or the mean of
          ## v over all actors when nobody else holds j; R = max(v) - min(v);
          ## simMean the mean of 1 - |v_a - v_b| / R over ordered pairs a != b.
          ## Translation invariant, so centering does not matter. Sums to the
          ## siena07 target (tests/testthat/test-structural-stats-vs-rsiena.R).
          ## Until 2026-10-04 this column held a different statistic: similarity
          ## to the mean of distance-2 alters in the actor projection, with no
          ## simMean centering.
          ##
          ## saomnk_coholder_similarity() (R/searchnet-imitation.R, formerly
          ## saomnk_sim_ego_indist2()) is NOT this statistic either.
          v <- as.numeric(.searchnet_stats_covar(self, .prep, i, item))
          xRange <- max(v) - min(v)
          if (xRange == 0) {
            ## A constant covariate leaves the similarity undefined (R = 0).
            mat[ , i ] <- 0
          } else {
            pair_sim <- 1 - abs(outer(v, v, '-')) / xRange
            diag(pair_sim) <- NA
            simMean <- mean(pair_sim, na.rm = TRUE)
            n_other <- matrix(xComponentDegree, self$M, self$N, byrow = TRUE) - bi_env_mat
            sum_other <- matrix(c( v %*% bi_env_mat ), self$M, self$N, byrow = TRUE) -
              bi_env_mat * v
            vbar <- ifelse(n_other > 0, sum_other / pmax(n_other, 1), mean(v))
            mat[ , i ] <- rowSums( bi_env_mat * (1 - abs(v - vbar) / xRange - simMean) )
          }

        }  else if (item$effect %in% .SEARCHNET_BOUND_STAT_EFFECTS) {

          ## Degree-bound penalty effects (R/searchnet-degree-bounds.R): RSiena's
          ## statistic up to a constant, zero on every state within the bounds,
          ## so the change for any toggle is RSiena's and a feasible path's
          ## reported utility carries no penalty term.
          mat[ , i ] <- .searchnet_bound_stat(item$effect, item$parm,
                                              item$initialValue, bi_env_mat)

        }  else if(grepl('[|]', item$effect) | item$effect == 'unspInt' )  {
          
          # cat(sprintf('\n skipping %s interaction to handle via post-hoc multiplication\n', item$effect))
          next ## Skip interactions; in processing chain_stats, add interactions to the stat matrix from the interacting effects
          
        } else {
          
          cat(sprintf('\n\nEffect not yet implemented: `%s\n\n`', item$effect))
          next
          
        }
        
        ####
        # else if (item$effect == 'altX|XWX') ## interaction1 strat_coCovar
        # {
        #   # covar <- item$x  ## NxN matrix
        #   # ## MxM matrix of inter-actor connections weighted by component covarite matrix
        #   # interactor_cov_w <- bi_env_mat %*% covar %*% t(bi_env_mat)
        #   # ## covert to M-vector of actor attributes
        #   # stat <- rowSums( interactor_cov_w, na.rm=T ) ##**TODO: CHECK**
        #   stop('implement altX|XWX .')
        # }
        # else if (item$effect == 'totInDist2|X') ## interaction1 strat_coCovar
        # {
        #   # covar <- item$x  ## NxN matrix
        #   # ## MxM matrix of inter-actor connections weighted by component covarite matrix
        #   # interactor_cov_w <- bi_env_mat %*% covar %*% t(bi_env_mat)
        #   # ## covert to M-vector of actor attributes
        #   # stat <- rowSums( interactor_cov_w, na.rm=T ) ##**TODO: CHECK**
        #   stop('implement altX|XWX .')
        # }
        
        #
        # mat[ , i] <- stat
        
      }
      
      return(mat)
    }



  ),

  private = list(

    # R6's clone(deep = TRUE) recurses only into fields that are themselves R6
    # objects. A data.table is not one, so the clone and the original end up
    # bound to the SAME data.table -- verified: identical addresses, and a
    # `:=` update on the clone adds a column to the original.
    #
    # That matters because `:=` deliberately bypasses R's copy-on-modify. Every
    # other field here is a value type (matrix, list, plain data.frame) or an
    # igraph object whose API returns new graphs, so all of those isolate
    # correctly on their own; the data.tables were the single exception.
    #
    # No package code currently trips this: results are installed by assignment
    # (`self$actor_stats_df <- ...`), never by reference update, so the clone in
    # plot-markets.R is safe as written. This closes the trap rather than fixing
    # a live defect -- it is armed the moment anyone writes `:=` against an
    # env's data.table, and the symptom would be cross-contaminated runs in a
    # seed batch, which is expensive to diagnose and easy to prevent here.
    deep_clone = function(name, value) {
      if (data.table::is.data.table(value)) return(data.table::copy(value))
      value
    }

  )


)




# # ##-----------------
# compute_totInDist2 <- function(bipartite_matrix, actor_covariate, interaction_type = "absdiff") {
#   # Ensure input is a matrix
#   if (!is.matrix(bipartite_matrix)) {
#     stop("Bipartite network must be an M C N matrix.")
#   }
#   
#   # Ensure actor covariate is a vector of length M
#   M <- nrow(bipartite_matrix)
#   if (length(actor_covariate) != M) {
#     stop("Actor covariate must be a vector of length M (number of actors).")
#   }
#   
#   # Step 1: Compute the N C N projection (component adjacency matrix)
#   A <- t(bipartite_matrix) %*% bipartite_matrix  # Project bipartite network onto components
#   diag(A) <- 0  # Remove self-loops
#   
#   # Step 2: Compute the dyadic transformation of the actor covariate
#   actor_matrix <- outer(actor_covariate, actor_covariate, FUN=switch(
#     interaction_type,
#     "absdiff" = function(x, y) abs(x - y),
#     "product" = function(x, y) x * y,
#     "sum" = function(x, y) x + y,
#     stop("Invalid interaction type. Choose 'absdiff', 'product', or 'sum'.")
#   ))
#   
#   # Step 3: Apply the transformed covariate to weight second-degree paths
#   A_squared <- A %*% A  # A^2 counts 2-step walks
#   weighted_A2 <- A_squared * (t(bipartite_matrix) %*% actor_matrix %*% bipartite_matrix)
#   
#   # Step 4: Compute totInDist2 statistic as the sum of incoming 2-step weighted paths
#   totInDist2_values <- rowSums(weighted_A2)
#   
#   # Return as named vector
#   names(totInDist2_values) <- paste0("Component_", seq_along(totInDist2_values))
#   
#   return(totInDist2_values)
# }

# # ===========================
# # p Example Usage
# # ===========================
# 
# # Set parameters
# M <- 10  # Number of actors
# N <- 15  # Number of components
# 
# # Generate a random M x N bipartite adjacency matrix (undirected)
# set.seed(123)
# bipartite_matrix <- matrix(sample(0:1, M * N, replace=TRUE, prob=c(0.7, 0.3)), nrow=M, ncol=N)
# 
# # Generate a random actor covariate (M C 1 vector)
# actor_covariate <- runif(M, 0, 1)
# 
# # Compute the totInDist2 statistic with actor-based interaction
# totInDist2_results <- compute_totInDist2(bipartite_matrix, actor_covariate, interaction_type="absdiff")
# 
# # Print results
# print(totInDist2_results)



# ##
# get_struct_mod_net_stats_list_from_bi_mat = function(bi_env_mat, type='all') {
#   # eff <- m1$rsiena_effects[m1$rsiena_effects$include, ]
#   efflist <- self$config_structure_model$dv_bipartite$effects
#   # set the bipartite environment network matrix
#   # bi_env_mat <- self$bipartite_matrix
#   ### empty matrix to hold actor network statistics
#   df <- data.frame() #nrow=self$M, ncol=length(efflist)
#   effnames <- sapply(efflist, function(x) x$effect, simplify = T)
#   effparams <- sapply(efflist, function(x) x$parameter, simplify = T)
#   mat <- matrix(rep(0, m1$M * length(efflist) ), nrow=self$M, ncol=length(efflist) )
#   colnames(mat) <- effnames
#   rownames(mat) <- 1:self$M
#   #
#   for (i in 1:length(efflist))
#   {
#     eff_name <- efflist[[ i ]]$effect
#     #
#     xActorDegree  <- rowSums(bi_env_mat, na.rm=T)
#     xComponentDegree  <- colSums(bi_env_mat, na.rm=T)
#     ## network statistics dataframe
#     #
#     if (eff_name == 'density' )
#     {
#       stat <- c( xActorDegree )
#     }
#     else if (eff_name == 'outAct' )
#     {
#       stat <- c( xActorDegree^2 )
#     }
#     else if (eff_name == 'inPop' )
#     {
#       stat <- c( bi_env_mat %*% (xComponentDegree + 1) )
#     }
#     # else if (eff_name == 'transTriads' )
#     # {
#     #   stat <- 
#     # }
#     # else if (eff_name == 'cycle4' )
#     # {
#     #   stat <- 
#     # }
#     else
#     {
#       cat(sprintf('\n\nEffect not yet implemented: `%s\n\n`', eff_name))
#     }
#     #
#     mat[ , i] <- stat
#     #
#     df <- rbind(df, data.frame(
#       statistic = stat, 
#       actor_id = factor(1:self$M), 
#       effect_id = factor(i), 
#       effect_name = effnames[i] 
#     ))
#   }
#   #
#   if (type %in% c('df','data.frame'))   return(df)
#   if (type %in% c('mat','matrix'))      return(mat)
#   if (type %in% c('all','both','list',NA)) return(list(df=df, mat=mat))
#   cat(sprintf('specified return type %s not found', type))
# }, 



