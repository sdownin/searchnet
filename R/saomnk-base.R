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
    behavior_rsienaDV = NULL,   ## behaviour / performance DV coevolving with the bipartite net
    behavior_values = NULL,     ## the M x waves matrix the behaviour DV was built from
    #
    theta_shocks = NULL,
    theta_matrix = NULL,
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
    component_1_coDyadCovar = NULL,  ## constant interaction matrix covariate (NxN matrix)
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
    component_1_varDyadCovar = NULL,  ## time varying interaction matrix covariate (NxNxT array) for T periods
    component_2_varDyadCovar = NULL,
    component_3_varDyadCovar = NULL,
    component_4_varDyadCovar = NULL,
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
    P_change = NULL,          # Probability of changing the component interaction matrix
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
    #
    experiments = list(),
    
    
    
    # Constructor to initialize the SAOM-NK model
    initialize = function(config_environ_params, verbose=FALSE) {
      if(verbose) cat('\nCALLED _BASE_ INIT\n')
      ## ----- prevent clashes with sna package-----------
      sna_err_check <-tryCatch(expr = { detach('package:sna') }, error=function(e)e )
      ## -------------------------------------------------
      ##**TODO:  LOAD DEPENDENCY FUNCTIONS ETC**
      ##
      self$config_environ_params = config_environ_params
      self$M <- config_environ_params[['M']]
      self$N <- config_environ_params[['N']]
      self$BI_PROB <- config_environ_params[['BI_PROB']]
      self$UUID <- UUIDgenerate(use.time = T)
      self$DIR_OUTPUT <- ifelse(is.null(config_environ_params[['dir_output']]),
                                getwd(),
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
      bipartite_igraph <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = F, mode = 'all')
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
      self$bipartite_matrix <- igraph::as_biadjacency_matrix(bipartite_igraph,attr = 'weight', sparse = F)
      self$social_matrix <- igraph::as_adjacency_matrix(self$social_igraph, attr = 'weight', sparse = F)
      self$search_matrix <- igraph::as_adjacency_matrix(self$search_igraph, attr = 'weight', sparse = F)
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
      bipartite_igraph <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = F, mode = 'all')
      return(bipartite_igraph)
    },
    
    ##
    get_bipartite_projections = function(ig_bipartite) {
      projs <- igraph::bipartite_projection(ig_bipartite, multiplicity = T, which = 'both')
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
      ##---------- 1 Efect --------------------------
      if (eff$effect == 'Rate') 
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  Rate, ## get network statistic function from effect name (character)
                                              name = eff$dv_name,  # interaction1 = eff$interaction1,
                                              fix = fix, 
                                              type='rate', verbose = verbose)
        self$rsiena_effects <- setEffect(self$rsiena_effects,  Rate,
                                         name = eff$dv_name, parameter = eff$parameter,  fix = fix, type='rate', verbose = verbose)
      }
      ## HETEROGENEOUS / STRUCTURAL RATE EFFECTS
      ## RateX  : rate depends on an actor covariate  (RSiena group `covarBipartiteRate`)
      ## outRate*/inRate* : rate depends on the actor's own degree (`bipartiteRate`)
      ## These make the FREQUENCY of change actor-specific, as distinct from the evaluation
      ## function, which governs WHICH change is preferred. Required for modelling
      ## heterogeneous adjustment / repositioning costs.
      else if (eff$effect %in% c('RateX', 'outRate', 'outRateInv', 'outRateLog',
                                 'inRateInv', 'inRateLog'))
      {
        .needs_cov <- identical(eff$effect, 'RateX')
        if (.needs_cov && (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1)))) {
          warning("'RateX' effect skipped: no interaction1 (actor covariate) specified. Declare a covariate first.")
        } else {
          tryCatch({
            .args_inc <- list(self$rsiena_effects, eff$effect, character = TRUE,
                              name = eff$dv_name, type = 'rate',
                              fix = fix, verbose = verbose)
            if (.needs_cov) .args_inc$interaction1 <- eff$interaction1
            self$rsiena_effects <- do.call(includeEffects, .args_inc)

            ## `parameter=` populates the `parm` column that get_theta_matrix() reads.
            .args_set <- list(self$rsiena_effects, shortName = eff$effect, character = TRUE,
                              name = eff$dv_name, type = 'rate',
                              parameter = eff$parameter,
                              fix = fix, verbose = verbose)
            if (!is.null(eff$initialValue)) .args_set$initialValue <- eff$initialValue
            if (.needs_cov) .args_set$interaction1 <- eff$interaction1
            self$rsiena_effects <- do.call(setEffect, .args_set)
          }, error = function(e) {
            warning(sprintf("'%s' rate effect failed: %s (is covariate '%s' registered?)",
                            eff$effect, e$message,
                            if (is.null(eff$interaction1)) '<none>' else eff$interaction1))
          })
        }
      }
      else if (eff$effect == 'density')
      {
        ## RSiena's bipartite effects table has NO 'density' shortName.
        ## (Confirmed by inspecting getEffects(<bipartite data>): only
        ## 'Rate' is auto-included; the structural baseline / intercept-like
        ## effect for bipartite is 'outAct' — outdegree activity.)
        ## For one-mode networks 'density' exists. Detect by querying the
        ## current effects table for a row matching this dv_name.
        eff_tbl <- as.data.frame(self$rsiena_effects)
        has_density_row <- any(
          eff_tbl$name == eff$dv_name & eff_tbl$shortName == 'density'
        )
        if (has_density_row) {
          ## one-mode case — original RSiena 'density' effect
          self$rsiena_effects <- includeEffects(self$rsiena_effects,  density,
                                               name = eff$dv_name,
                                               fix = fix, verbose = verbose)
          self$rsiena_effects <- setEffect(self$rsiena_effects,  density,
                                           name = eff$dv_name, parameter = eff$parameter,  fix = fix, verbose = verbose)
        } else {
          ## bipartite case — substitute 'outAct' (outdegree activity), which
          ## plays the same role as the structural intercept in bipartite SAOMs.
          if (verbose) {
            message(sprintf("[SaoMNK] DV '%s' is bipartite: routing user-friendly 'density' effect to RSiena 'outAct' (the bipartite-equivalent baseline structural effect).", eff$dv_name))
          }
          self$rsiena_effects <- includeEffects(self$rsiena_effects,  outAct,
                                               name = eff$dv_name,
                                               fix = fix, verbose = verbose)
          self$rsiena_effects <- setEffect(self$rsiena_effects,  outAct,
                                           name = eff$dv_name, parameter = eff$parameter,  fix = fix, verbose = verbose)
        }
      }
      else if (eff$effect == 'inPop') 
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, inPop, ## get network statistic function from effect name (character)
                                             name = eff$dv_name, # interaction1 = eff$interaction1,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- setEffect(self$rsiena_effects,  inPop, 
                                          name = eff$dv_name, parameter = eff$parameter,  fix = fix, verbose = verbose)
      }
      else if (eff$effect == 'outAct') 
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, outAct, ## get network statistic function from effect name (character)
                                             name = eff$dv_name, # interaction1 = eff$interaction1,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- setEffect(self$rsiena_effects,  outAct, 
                                          name = eff$dv_name, parameter = eff$parameter,  fix = fix, verbose = verbose)
      }
      else if (eff$effect == 'outActSqrt') 
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, outActSqrt, ## get network statistic function from effect name (character)
                                              name = eff$dv_name, # interaction1 = eff$interaction1,
                                              fix = fix, verbose = verbose)
        self$rsiena_effects <- setEffect(self$rsiena_effects,  outActSqrt, 
                                         name = eff$dv_name, parameter = eff$parameter,  fix = fix, verbose = verbose)
      }
      else if (eff$effect == 'cycle4') 
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects, cycle4, ## get network statistic function from effect name (character)
                                             name = eff$dv_name, # interaction1 = eff$interaction1,
                                             # parameter = eff$parameter,  
                                             # initialValue = eff$parameter,
                                             fix = fix, verbose = verbose)
        self$rsiena_effects <- setEffect(self$rsiena_effects,  cycle4,
                                          name = eff$dv_name, 
                                         initialValue = eff$initialValue,
                                         # parameter = eff$parameter, 
                                         fix = fix, verbose = verbose)
      }
      else if (eff$effect == 'transTriads') 
      {
        self$rsiena_effects <- includeEffects(self$rsiena_effects,  transTriads, ## get network statistic function from effect name (character)
                                              name = eff$dv_name, # interaction1 = eff$interaction1,
                                              fix = fix, verbose = verbose)
        self$rsiena_effects <- setEffect(self$rsiena_effects,  transTriads, 
                                         name = eff$dv_name, parameter = eff$parameter,  fix = fix, verbose = verbose)
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
        self$rsiena_effects <- setEffect(self$rsiena_effects,  totInDist2, 
                                         name = eff$dv_name, 
                                         interaction1 = resuse_int_eff$interaction1,
                                         parameter = eff$parameter,  
                                         fix = fix, verbose = verbose)
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
        self$rsiena_effects <- setEffect(self$rsiena_effects,  simEgoInDist2, 
                                         name = eff$dv_name, 
                                         interaction1 = resuse_int_eff$interaction1,
                                         parameter = eff$parameter,  
                                         fix = fix, verbose = verbose)
      }
      
      else if (eff$effect %in% c('egoX', 'altX', 'outActX', 'altXOutAct', 'homXOutAct', 'inPopX'))
      {
        ## All covariate-dependent effects require interaction1 (a registered covariate name)
        if (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1))) {
          warning(sprintf("'%s' effect skipped: no interaction1 (covariate) specified. Declare a covariate first.", eff$effect))
        } else {
          tryCatch({
            ## NOTE: `shortName` is NOT a formal of includeEffects(); passing it there puts
            ## it in `...`, where RSiena deparses the unevaluated expression and looks for an
            ## effect literally named "eff$effect". Pass the name as the first `...` argument
            ## with character=TRUE instead. setEffect() DOES take shortName, but likewise
            ## deparses it unless character=TRUE.
            ## NOTE 2: the theta values that drive the simulation are read from the `parm`
            ## column (`get_theta_matrix()`: `theta_in <- effs$parm`), populated by
            ## setEffect(parameter=). Writing `initialValue=` instead registers the effect
            ## but leaves its coefficient out of the theta matrix, so the effect is INERT.
            ## `parameter` and `initialValue` are different things -- pass both when supplied.
            self$rsiena_effects <- includeEffects(self$rsiena_effects,
                                                  eff$effect,
                                                  character = TRUE,
                                                  name = eff$dv_name,
                                                  interaction1 = eff$interaction1,
                                                  fix = fix, verbose = verbose)
            .args_set <- list(self$rsiena_effects,
                              shortName = eff$effect, character = TRUE,
                              interaction1 = eff$interaction1,
                              name = eff$dv_name,
                              parameter = eff$parameter,
                              fix = fix, verbose = verbose)
            if (!is.null(eff$initialValue)) .args_set$initialValue <- eff$initialValue
            self$rsiena_effects <- do.call(setEffect, .args_set)
          }, error = function(e) {
            warning(sprintf("'%s' effect failed: %s (is covariate '%s' registered?)",
                            eff$effect, e$message, eff$interaction1))
          })
        }
      }
      ## DYADIC COVARIATE EFFECT
      else if (eff$effect == 'XWX')
      {
        ## XWX requires a coDyadCovar registered as interaction1
        if (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1))) {
          warning("XWX effect skipped: no interaction1 (W-matrix covariate) specified. Configure W-matrix first.")
        } else {
          tryCatch({
            self$rsiena_effects <- includeEffects(self$rsiena_effects,  XWX,
                                                  name = eff$dv_name,
                                                  interaction1 = eff$interaction1,
                                                  fix = fix, verbose = verbose)
            self$rsiena_effects <- setEffect(self$rsiena_effects,  XWX,
                                             interaction1 = eff$interaction1,
                                             name = eff$dv_name,
                                             initialValue = eff$initialValue %||% eff$parameter,
                                             fix = fix, verbose = verbose)
          }, error = function(e) {
            warning(sprintf("XWX effect failed: %s (is the W-matrix registered as a coDyadCovar?)", e$message))
          })
        }
      }
      else if (eff$effect == 'X')
      {
        if (is.null(eff$interaction1) || !nzchar(as.character(eff$interaction1))) {
          warning("X effect skipped: no interaction1 covariate specified.")
        } else {
          tryCatch({
            self$rsiena_effects <- includeEffects(self$rsiena_effects,  X,
                                                  name = eff$dv_name,
                                                  interaction1 = eff$interaction1,
                                                  fix = fix, verbose = verbose)
            self$rsiena_effects <- setEffect(self$rsiena_effects,  X,
                                             interaction1 = eff$interaction1,
                                             name = eff$dv_name,
                                             initialValue = eff$initialValue %||% eff$parameter,
                                             fix = fix, verbose = verbose)
          }, error = function(e) {
            warning(sprintf("X effect failed: %s", e$message))
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
          ## Two-slot effects. Behaviour effects such as `avXAlt` / `totXAlt`
          ## and the covariate distance-2 family (`avXInAltDist2`, ...) are
          ## identified by BOTH a covariate (interaction1) and the network
          ## through which it reaches ego (interaction2); without
          ## interaction2 RSiena cannot resolve them. No structure model
          ## predating behaviour coevolution sets interaction2 on this path,
          ## so this is inert for them.
          if (!is.null(eff$interaction2) && nzchar(as.character(eff$interaction2)))
            .args_inc$interaction2 <- eff$interaction2
          self$rsiena_effects <- do.call(includeEffects, .args_inc)
          if (!is.null(eff$parameter) || !is.null(eff$initialValue)) {
            ## `parameter=` populates the `parm` column that get_theta_matrix() reads;
            ## `initialValue=` is RSiena's estimation start value. They are not the same.
            .args_set <- list(self$rsiena_effects, shortName = eff$effect, character = TRUE,
                              name = eff$dv_name, type = .type,
                              fix = fix, verbose = verbose)
            if (!is.null(eff$parameter))    .args_set$parameter    <- eff$parameter
            if (!is.null(eff$initialValue)) .args_set$initialValue <- eff$initialValue
            if (!is.null(eff$interaction1) && nzchar(as.character(eff$interaction1)))
              .args_set$interaction1 <- eff$interaction1
            if (!is.null(eff$interaction2) && nzchar(as.character(eff$interaction2)))
              .args_set$interaction2 <- eff$interaction2
            self$rsiena_effects <- do.call(setEffect, .args_set)
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
          warning(sprintf("Effect '%s' could not be included: %s (skipping).%s",
                          eff$effect, e2$message, available_msg))
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
      
      if (grepl('rate',eff$effect, ignore.case = T)) {
        cat(sprintf('**NOTE** skipping rate effect `%s`',eff$effect))
        return(rsiena_effects)
      }
      #
      dv_name <- gsub('self\\$','',eff$dv_name, ignore.case = T)
      #
      if (is.null(eff$interaction1)) {
        rsiena_effects <- includeEffects(rsiena_effects,  eff$effect, 
                                         name = dv_name,  # interaction1 = eff$interaction1,
                                         fix = ifelse(unfix_all, FALSE, eff$fix), 
                                         character = TRUE, verbose=verbose)
        rsiena_effects <- setEffect(rsiena_effects,  eff$effect,
                                    name = dv_name, 
                                    parameter = eff$parameter,  
                                    fix = ifelse(unfix_all, FALSE, eff$fix), 
                                    character = TRUE, verbose=verbose)
      } else {
        interact1 <- gsub('self\\$','',eff$interaction1, ignore.case = T)
        rsiena_effects <- includeEffects(rsiena_effects,  eff$effect, 
                                         name = dv_name,  # interaction1 = eff$interaction1,
                                         fix = ifelse(unfix_all, FALSE, eff$fix), 
                                         interaction1 = interact1,
                                         character = TRUE, verbose=verbose)
        rsiena_effects <- setEffect(rsiena_effects,  eff$effect,
                                    name = dv_name, 
                                    parameter = eff$parameter,  
                                    fix = ifelse(unfix_all, FALSE, eff$fix),
                                    interaction1 = interact1,
                                    character = TRUE, verbose=verbose)
      }

      return(rsiena_effects)
    },
    
    
    
    
    include_rsiena_interaction_from_eff_list_static = function(rsiena_effects, interact, unfix_all=TRUE, verbose=FALSE) {
      #
      if ( ! 'manual_interaction' %in% names(rsiena_effects) ) {
        rsiena_effects$manual_interaction <- NA
      }
      #
      dv_name <- gsub('self\\$','',interact$dv_name, ignore.case = T)
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
          gsub('self\\$','',eff$interaction1, ignore.case = T),
          gsub('self\\$','',eff$interaction2, ignore.case = T)
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
    ## when a behaviour DV coevolves, that DV's effects -- `linear`, `quad`,
    ## `avInSimDist2` and the rest are statistics of the behaviour, not of the
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
        if ( grepl('.*unspInt.*',  theta_df$effect_level[i], ignore.case = T) ) {
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
    get_struct_mod_stats_mat_from_bi_mat = function(bi_env_mat, type='all', .cache=NULL) {
      #
      ## Bipartite-network effects only: this function computes statistics OF
      ## bi_env_mat, and a coevolving behaviour DV's effects are not statistics
      ## of it. Identical to the previous call for single-DV models.
      theta_df_norates <- self$get_bipartite_effects_theta_df()
      theta_df_norates$effect <-  theta_df_norates$shortName
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
      effparams <- theta_df_norates$parm ##sapply(efflist, function(x) x$parameter, simplify = T)
      #
      mat <- matrix(rep(0, self$M * neffs ), nrow=self$M, ncol=neffs )
      colnames(mat) <- theta_df_norates$effect_level
      rownames(mat) <- 1:self$M
      #
      ## Lazy-compute helpers: only materialize social/epistasis when first needed
      .get_social <- function() {
        if (is.null(.cache$social)) .cache$social <<- bi_env_mat %*% t(bi_env_mat)
        .cache$social
      }
      .get_epistasis <- function() {
        if (is.null(.cache$epistasis)) .cache$epistasis <<- t(bi_env_mat) %*% bi_env_mat
        .cache$epistasis
      }
      #
      for (i in 1:neffs)
      {
        # print(i)
        item <- theta_df_norates[ i , ]
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
          
          mat[ , i] <- c( bi_env_mat %*% (xComponentDegree + 1) )
          
          # else if (item$effect == 'transTriads' ) {
          #   stat <- 
        } else if (item$effect == 'cycle4' ) {

            ## OPTIMIZED: reuse cached social projection (B %*% t(B)) instead of recomputing
            XXt <- .get_social()
            XXt_squared <- XXt %*% XXt
            # For each actor, count paths of length 3 that return to the actor
            # Divide by 2 because each cycle is counted twice for each actor
            # rowSums of element-wise product extracts diagonal without allocating full product
            actor_cycles <- rowSums(XXt_squared * XXt) / 2
            #
            mat[ , i] <- actor_cycles

        } else if (item$effect == 'egoX') {
          
          # covar <- item$x
          covar <- self$get_cov_data(item)
          checkConform <-  all(
            (  ## A or B
              class(covar) %in% c('array','matrix') & nrow(covar)==self$M
            ) | ( 
              length(covar)==self$M  ## array, matrix; vector
            )
          )
          if( ! checkConform )  
            stop('egoX covar not conformable for multiplication given number of actors')
          mat[ , i] <- c( covar * xActorDegree ) ##**vector element-wise multiplication by rows of covar matrix, or elements of covar array
            
        } else if (item$effect == 'altX') {
          
          # covar <- item$x
          covar <- self$get_cov_data(item)
          checkConform <-  all(
            (  ## A or B
              class(covar) %in% c('array','matrix') & nrow(covar)==self$M
            ) | ( 
              length(covar)==self$N  ## COMPONENT array, for ACTOR statistic 
            )
          )
          if( ! checkConform )
            stop('altX covar not conformable for multiplication given number of components or actors')
          covarComponentMat <- matrix(rep(covar, self$M), nrow=self$M, ncol=self$N, byrow = TRUE)
          mat[ , i] <- rowSums( covarComponentMat * bi_env_mat, na.rm=T ) ##**vector element-wise multiplication by rows of covar matrix, or elements of covar array
          
        } else if (item$effect == 'outActX') { ## interaction1 component_coCovar
          
          ## N-vector of component covariate
          # covar <- item$x
          covar <- self$get_cov_data(item)
          # MxN matrix of row-stacked component covariate (repeated for each actor)
          covarComponentMat <- matrix(rep(covar, self$M), nrow=self$M, ncol=self$N, byrow = TRUE)
          ## M-vector of actor's squared sum of component-covariate-weighted component connections (weighted version of the squared degree)
          mat[ , i] <- xActorDegree * rowSums( covarComponentMat * bi_env_mat, na.rm = T)
        
        } else if (item$effect == 'inPopX') { #M-vector of actor strategy covars

          # covar <- item$x
          covar <- self$get_cov_data(item)
          ## MxN matrix holding actor strategy covariate as columns stacked for each component
          covarActorMat <- matrix(rep(covar, self$N), nrow=self$M, ncol=self$N,  byrow = FALSE)
          ## N-vector of square roots of component weights (sum of actor covariate for the component's connected actors)
          component_weights_from_actor_stats <-  colSums(covarActorMat * bi_env_mat, na.rm=T)
          ## M-vector of actor sum of it's connected component weights (which are computed as the sum of the connected actor covariates)
          mat[ , i] <- rowSums( bi_env_mat * component_weights_from_actor_stats, na.rm=T ) ##**vector element-wise multiplication by rows of covar matrix, or elements of covar array
        
        } else if (item$effect == 'XWX') { ## NxN
          
          # covar <- item$x
          covar <- self$get_cov_data(item)
          ## MxM matrix of inter-actor connections weighted by component covarite matrix
          interactor_cov_w <- bi_env_mat %*% covar %*% t(bi_env_mat)
          ## covert to M-vector of actor attributes
          mat[ , i] <- rowSums( interactor_cov_w, na.rm=T ) ##**TODO: CHECK**
          # mat[ , i] <- colSums( interactor_cov_w, na.rm=T ) ##**TODO: CHECK**
          
        }  else if (item$effect == 'X') { ## MxN
          
          # covar <- item$x
          covar <- self$get_cov_data(item)
          ## MxM matrix of inter-actor connections weighted by component covarite matrix
          # interactor_cov_w <- bi_env_mat * (covar - mean(c(covar, na.rm=T)) )
          interactor_cov_w <- bi_env_mat * covar ##**TODO** Not Centered
          ## covert to M-vector of actor attributes
          mat[ , i] <- rowSums( interactor_cov_w, na.rm=T ) ##**TODO: CHECK**
          # stop('implement altX|XWX .')

          
        }   else if (item$effect == 'totInDist2') { 
          
          ## M-vector
          covar <- self$get_cov_data(item)
          # 1xN matrix
          component_sums_w_by_actor_covar <-  covar %*% bi_env_mat 
          #
          compo_w_stacked_mat <- matrix(rep(component_sums_w_by_actor_covar, self$M), byrow=T, ncol=self$N)
          ## covert to M-vector of actor attributes
          mat[ , i ] <-  rowSums( compo_w_stacked_mat * bi_env_mat, na.rm=T )
          # mat[ , i] <- self$getTotInDist2(bi_env_mat, covar, interaction_type = 'absdiff') ##**TODO: CHECK**
          
          
        }   else if (item$effect == 'simEgoInDist2') {

          ## M-vector
          covar <- self$get_cov_data(item)
          #
          M <- self$M
          # Range for similarity normalization
          xRange <- max(covar) - min(covar)

          if (xRange == 0) {
            similarityScores <- numeric(M)
          } else {
            ## VECTORIZED: replace per-actor loop with matrix algebra
            ## Social projection S = B %*% t(B): S[i,j] = # shared components between actors i,j
            S <- .get_social()
            ## dist1[i,j] = 1 iff actors i,j share at least one component (distance-1 neighbors)
            dist1 <- (S > 0)
            diag(dist1) <- FALSE  # exclude self
            ## dist2_reach[i,j] = 1 iff actor j is reachable from i in exactly 2 hops through
            ## the social projection (i.e., i shares a component with k, k shares a component with j)
            S2 <- dist1 %*% dist1  # 2-hop reachability counts in actor space
            ## dist2_only: reachable in 2 hops but NOT in 1 hop (and not self)
            dist2_only <- (S2 > 0) & (!dist1)
            diag(dist2_only) <- FALSE
            ## For each actor i, compute mean covariate of distance-2 alters
            ## n_dist2[i] = number of distance-2 alters for actor i
            n_dist2 <- rowSums(dist2_only)
            ## sum of covar for dist-2 alters (matrix-vector product)
            sum_cov_dist2 <- dist2_only %*% covar
            ## mean covariate of dist-2 alters (avoid div-by-zero for isolated actors)
            has_dist2 <- (n_dist2 > 0)
            avgCovAlter <- ifelse(has_dist2, sum_cov_dist2 / n_dist2, 0)
            ## Similarity: 1 - |ego_cov - avg_alter_cov| / range
            similarityScores <- ifelse(has_dist2,
                                       1 - abs(covar - avgCovAlter) / xRange,
                                       0)
          }
          ## covert to M-vector of actor attributes
          mat[ , i ] <-  similarityScores
          
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



