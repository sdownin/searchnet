#' @import R6 igraph RSiena ggplot2 dplyr tidyr cowplot ggraph Matrix network reshape2 texreg grid gridExtra ggpubr plyr
#' @importFrom rvest html_text
#' @importFrom knitr kable
#' @importFrom broom tidy
#' @importFrom stringr str_detect str_replace
#' @importFrom scales percent_format
#' @importFrom RColorBrewer brewer.pal
#' @importFrom MatchIt matchit


###############   DEPENDENCIES ###############################


###############################################################################
###############################################################################

# Define the SAOM_NK_Enhanced class with local search, complex search strategies, network metrics, and visualization
SaomNkRSienaBiEnv <- R6Class( 
  "SaomNkRSienaBiEnv",
  inherit = SaomNkRSienaBiEnv_base,
  
  public = list(

    # Constructor to initialize the SAOM-NK model
    initialize = function(config_environ_params, verbose=FALSE) {
      if(verbose) cat('\nTEST FROM CALLED CLASS: *BEFORE* BASE INIT\n')
      super$initialize(config_environ_params)
      if(verbose) cat('\nTEST FROM CALLED CLASS: *AFTER* BASE INIT\n')
      plot_init <- ifelse(is.null(config_environ_params[['plot_init']]), 
                               FALSE, ##default to not show on init
                               config_environ_params[['plot_init']])
      if (plot_init) {
        self$plot_bipartite_system_from_mat(self$bipartite_matrix_init, RSIENA_ITERATION=0, return_plot=FALSE)
      }
    },


    ## ------------------------------------------------------------------------
    ## set_behavior_rsienaDV()
    ## ------------------------------------------------------------------------
    ## Build self$behavior_rsienaDV from structure_model$dv_behavior, or clear it
    ## when the structure model declares no behavior DV. Clearing matters: an
    ## environment reused across models must not silently carry a behavior DV
    ## from a previous run into a model that does not declare one.
    ## No-op in effect for every structure model that has no dv_behavior block,
    ## which is every structure model built before this feature existed.
    set_behavior_rsienaDV = function(structure_model, verbose = FALSE) {
      if (!.searchnet_has_behavior(structure_model)) {
        self$behavior_rsienaDV <- NULL
        self$behavior_values   <- NULL
        return(invisible(NULL))
      }
      dvb <- structure_model$dv_behavior
      self$behavior_rsienaDV <- .searchnet_build_behavior_dv(self, dvb)
      self$behavior_values   <- dvb$values
      if (verbose)
        cat(sprintf("behavior DV '%s' set: %d nodes x %d waves, values in [%d, %d]\n",
                    dvb$name, nrow(self$behavior_values), ncol(self$behavior_values),
                    min(self$behavior_values), max(self$behavior_values)))
      invisible(self$behavior_rsienaDV)
    },

    get_rsiena_data_from_structure_model = function(structure_model, verbose=FALSE) {
      ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
      COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
      ## Behavior coevolution DV, added to input_varlist alongside the
      ## bipartite DV in both construction branches below. NULL when the
      ## structure model declares no dv_behavior block, in which case every
      ## line touching it is inert.
      .behavior_varlist <- list()
      if (.searchnet_has_behavior(structure_model)) {
        if (is.null(self$behavior_rsienaDV))
          self$set_behavior_rsienaDV(structure_model, verbose = verbose)
        .behavior_varlist[[ structure_model$dv_behavior$name ]] <- self$behavior_rsienaDV
      }
      ## Check if any covariates are actually provided (not just empty lists).
      ##
      ## The TIME-VARYING kinds must be counted here too. This gate previously
      ## tested only `coCovars` and `coDyadCovars`, so a structure model
      ## declaring ONLY `varCovars` or ONLY `varDyadCovars` took the early
      ## return below and registered nothing -- silently. That is exactly the
      ## shape of a multi-W horserace built from `influence_arrays` with no
      ## static `influence_matrix`: every coupling would be dropped and the
      ## model would estimate without them, reporting nothing amiss.
      .has <- function(k) k %in% names(structure_model$dv_bipartite) &&
        length(structure_model$dv_bipartite[[k]]) > 0
      has_coCovars      <- .has('coCovars')
      has_coDyadCovars  <- .has('coDyadCovars')
      has_varCovars     <- .has('varCovars')
      has_varDyadCovars <- .has('varDyadCovars')
      if (!has_coCovars && !has_coDyadCovars &&
          !has_varCovars && !has_varDyadCovars) {
        ## No covariates -- return simple RSiena data with only bipartite DV.
        ## CRITICAL: sienaDataCreate() picks up the dependent-variable name from
        ## the NAMES OF THE `...` ARGUMENTS, not from the names of a list passed
        ## as the first positional argument (which gives a name of "NULL"). Use
        ## do.call to splat the named list into `...` so the DV is registered
        ## under "self$bipartite_rsienaDV", matching dv_name used downstream by
        ## includeEffects()/setEffect().
        input_varlist <- c(list(`self$bipartite_rsienaDV` = self$bipartite_rsienaDV),
                           .behavior_varlist)
        rsiena_data <- do.call(
          sienaDataCreate,
          c(input_varlist, list(nodeSets = list(ACTORS, COMPONENTS)))
        )
        return(rsiena_data)
      }
      dv_name <- structure_model$dv_bipartite$name
      
      input_effs <- self$get_input_from_structure_model(structure_model)
      
      
      structeffs <- sapply(structure_model$dv_bipartite$effects, function(x)x$parameter)
      names(structeffs) <- sapply(structure_model$dv_bipartite$effects, function(x)x$effect)
      
      coCovars      <- sapply(structure_model$dv_bipartite$coCovars, function(x)x$effect)
      varCovars     <- sapply(structure_model$dv_bipartite$varCovars, function(x)x$effect)
      coDyadCovars  <- sapply(structure_model$dv_bipartite$coDyadCovars, function(x)x$effect)
      varDyadCovars <- sapply(structure_model$dv_bipartite$varDyadCovars, function(x)x$effect)

      coCovarTypes      <- sapply(structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
      varCovarTypes     <- sapply(structure_model$dv_bipartite$varCovars, function(x)x$interaction1)
      coDyadCovarTypes  <- sapply(structure_model$dv_bipartite$coDyadCovars, function(x)x$interaction1)
      varDyadCovarTypes <- sapply(structure_model$dv_bipartite$varDyadCovars, function(x)x$interaction1)

      component_coCovar_ids     <- grep('self\\$component.+_coCovar', coCovarTypes) ## ex: "self$component_1_coCovar"
      component_varCovar_ids    <- grep('self\\$component.+_varCovar', varCovarTypes) ## ex: "self$component_1_coCovar"
      component_coDyadCovar_ids  <- grep('self\\$component.+_coDyadCovar', coDyadCovarTypes) ## ex: "self$component_1_coCovar"
      component_varDyadCovar_ids <- grep('self\\$component.+_varDyadCovar', varDyadCovarTypes) ## ex: "self$component_1_coCovar"

      strat_coCovar_ids     <- grep('self\\$strat.+_coCovar', coCovarTypes) ## ex: "self$component_1_coCovar"
      strat_varCovar_ids    <- grep('self\\$strat.+_varCovar', varCovarTypes) ## ex: "self$component_1_coCovar"
      strat_coDyadCovar_ids  <- grep('self\\$strat.+_coDyadCovar', coDyadCovarTypes) ## ex: "self$component_1_coCovar"
      strat_varDyadCovar_ids <- grep('self\\$strat.+_varDyadCovar', varDyadCovarTypes) ## ex: "self$component_1_coCovar"
      
      ncompo_coCovar     <- length( component_coCovar_ids )
      ncompo_varCovar    <- length( component_varCovar_ids )
      ncompo_coDyadCovar  <- length( component_coDyadCovar_ids )
      ncompo_varDyadCovar <- length( component_varDyadCovar_ids )

      nstrat_coCovar     <- length( strat_coCovar_ids )
      nstrat_varCovar    <- length( strat_varCovar_ids )
      nstrat_coDyadCovar  <- length( strat_coDyadCovar_ids )
      nstrat_varDyadCovar <- length( strat_varDyadCovar_ids )
      
      
      ## Get struture model effects to extract interactions
      struct_model_params <- self$get_structure_model_params()
      interaction_ids <- struct_model_params$interact_type_ids
      ninteraction <- length( interaction_ids )
      interaction_effects <- ifelse(ninteraction>0 & !is.null(struct_model_params$covs[interaction_ids]), 
                                    struct_model_params$covs[interaction_ids],
                                    0)
      
      
      ## input list of variable for RSiena model
      input_varlist <- c(list(`self$bipartite_rsienaDV`=self$bipartite_rsienaDV),
                         .behavior_varlist)
      ##-------------------------------------------------------------
      ## COMPONENTS ##
      if (ncompo_coCovar) {
        for (i in 1:ncompo_coCovar) {
          property <- sprintf('component_%s_coCovar', i)
          eff <- structure_model$dv_bipartite$coCovars[[ component_coCovar_ids[i] ]]
          self[[property]] <- coCovar(eff$x, nodeSet = c('COMPONENTS'))
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      if (ncompo_varCovar) {
        for (i in 1:ncompo_varCovar) {
          ## Slot name must carry the _varCovar suffix: it is what the R6 field is
          ## called in saomnk-base.R, and it is what an effect's interaction1
          ## ("self$component_1_varCovar") addresses. Writing _coCovar here both
          ## clobbered the coCovar slot and left the varCovar reference unresolved.
          property <- sprintf('component_%s_varCovar', i)
          eff <- structure_model$dv_bipartite$varCovars[[ component_varCovar_ids[i] ]]
          self[[property]] <- varCovar(eff$x, nodeSet = c('COMPONENTS'))
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      if (ncompo_coDyadCovar) {
        for (i in 1:ncompo_coDyadCovar) {
          property <- sprintf('component_%s_coDyadCovar', i)
          ## Exact [[ ]] rather than $coDyadCovar: `$` partial-matches, so the
          ## singular name silently resolved to the plural key producers emit.
          ## Adding a real `coDyadCovar` key would flip the match, and an
          ## ambiguous prefix returns NULL -- after which NULL[[1]] is NULL, not
          ## an error, and the failure surfaces far away as "argument is of
          ## length zero". [[ ]] fails here instead, at the line that is wrong.
          eff <- structure_model$dv_bipartite[["coDyadCovars"]][[ component_coDyadCovar_ids[i] ]]
          eff_dim <- dim(eff$x)
          if ( !is.null(eff$nodeSet) ) {
            ## A. user provides nodeSet
            nodeSet <- eff$nodeSet
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$M  &&  eff_dim[2] == self$N ) {
            ## B. M x N with M != N: an actor-by-component covariate
            nodeSet <- c('ACTORS','COMPONENTS')
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$N  &&  eff_dim[2] == self$N ) {
            ## C. N x N with M != N: a component-by-component covariate
            nodeSet <- c('COMPONENTS','COMPONENTS')
          } else {
            ## D. M == N, or dimensions matching neither shape. When M == N the
            ## two shapes above are identical and genuinely ambiguous.
            stop(sprintf('Cannot distinguish actors from components for dimensions M=%s,N=%s; provide nodeSet for effect %s.',
                         self$M, self$N, eff$effect))
          }
          self[[property]] <- coDyadCovar(eff$x, nodeSet = nodeSet)
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      if (ncompo_varDyadCovar) {
        for (i in 1:ncompo_varDyadCovar) {
          property <- sprintf('component_%s_varDyadCovar', i)
          ## Exact [[ ]] for the same reason as coDyadCovars above.
          eff <- structure_model$dv_bipartite[["varDyadCovars"]][[ component_varDyadCovar_ids[i] ]]
          self[[property]] <- varDyadCovar(eff$x, nodeSet = c('COMPONENTS','COMPONENTS'))
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      ##-------------------------------------------------------------
      if (nstrat_coCovar) {
        for (i in 1:nstrat_coCovar) {
          property <- sprintf('strat_%s_coCovar', i)
          eff <- structure_model$dv_bipartite$coCovars[[ strat_coCovar_ids[i] ]]
          ## Guard: if x (covariate values) is missing, generate default zeros
          cov_values <- if (!is.null(eff$x) && length(eff$x) > 0 && !all(is.na(eff$x))) {
            as.numeric(eff$x)
          } else {
            warning(sprintf("Covariate '%s' has no x values, using zeros for M=%d actors", property, self$M))
            rep(0, self$M)
          }
          self[[property]] <- coCovar(cov_values, nodeSet = c('ACTORS'))
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      if (nstrat_varCovar) {
        for (i in 1:nstrat_varCovar) {
          ## _varCovar suffix, not _coCovar -- see the component varCovar loop above.
          property <- sprintf('strat_%s_varCovar', i)
          eff <- structure_model$dv_bipartite$varCovars[[ strat_varCovar_ids[i] ]]
          self[[property]] <- varCovar(eff$x, nodeSet = c('ACTORS'))
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      if (nstrat_coDyadCovar) {
        for (i in 1:nstrat_coDyadCovar) {
          property <- sprintf('strat_%s_coDyadCovar', i)
          ## strat_, not component_: indexing the component id vector here made a
          ## strategy dyadic covariate fetch the component one whenever both were
          ## present. Exact [[ ]] for the reason given in the component loop above.
          eff <- structure_model$dv_bipartite[["coDyadCovars"]][[ strat_coDyadCovar_ids[i] ]]
          eff_dim <- dim(eff$x)
          if ( !is.null(eff$nodeSet) ) {
            ## A. user provides nodeSet
            nodeSet <- eff$nodeSet
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$M  &&  eff_dim[2] == self$N ) {
            ## B. M x N with M != N: an actor-by-component covariate
            nodeSet <- c('ACTORS','COMPONENTS')
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$M  &&  eff_dim[2] == self$M ) {
            ## C. M x M with M != N: an actor-by-actor covariate
            nodeSet <- c('ACTORS','ACTORS')
          } else {
            ## D. M == N, or dimensions matching neither shape.
            stop(sprintf('Cannot distinguish actors from components for dimensions M=%s,N=%s; provide nodeSet for effect %s.',
                         self$M, self$N, eff$effect))
          }
          self[[property]] <- coDyadCovar(eff$x, nodeSet = nodeSet)
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      if (nstrat_varDyadCovar) {
        for (i in 1:nstrat_varDyadCovar) {
          property <- sprintf('strat_%s_varDyadCovar', i)
          ## Exact [[ ]] for the same reason as coDyadCovars above.
          eff <- structure_model$dv_bipartite[["varDyadCovars"]][[ strat_varDyadCovar_ids[i] ]]
          self[[property]] <- varDyadCovar(eff$x, nodeSet = c('ACTORS','ACTORS'))
          input_varlist[[sprintf('self$%s',property)]] <-  self[[property]]
        }
      }
      ##------------------------------------------------------------
      if (ninteraction) {
        COUNTER <- 1
        for (i in seq_len(ninteraction)){
          property <- sprintf('interaction_%s', COUNTER)
          eff <- structure_model$dv_bipartite$interactions[[ i ]]
          interact_name <- gsub('self\\$','', eff$effect )
          interact_effects <- strsplit(interact_name, split = '[|]')[[1]]
          int1s <- gsub('self\\$','', eff$interaction1 )

          for (ii in seq_along(int1s) ) {
            if ( int1s[ii] != '' ) {
              input_varlist[[ sprintf('self$%s',property) ]]  <- self[[ int1s[ii] ]]
              COUNTER <- COUNTER + 1
            }
          }
        }
      }
      ##-------------------------------------------------------------
      
      if(verbose) print(input_varlist)
      
      sienaDataCreate_args <- c( list(nodeSets=list(ACTORS, COMPONENTS)), input_varlist ) 
      rsiena_data <- do.call(sienaDataCreate, sienaDataCreate_args)
      return(rsiena_data)
    },
    
    
    ## nonself function does not affect object 'self'
    get_rsiena_data_static = function(structure_model, input_varlist) {
      ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
      COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
  
      dv_name <- structure_model$dv_bipartite$name
  
      structeffs <- sapply(structure_model$dv_bipartite$effects, function(x)x$parameter)
      names(structeffs) <- sapply(structure_model$dv_bipartite$effects, function(x)x$effect)
      
      coCovars      <- sapply(structure_model$dv_bipartite$coCovars, function(x)x$effect)
      varCovars     <- sapply(structure_model$dv_bipartite$varCovars, function(x)x$effect)
      coDyadCovars  <- sapply(structure_model$dv_bipartite$coDyadCovars, function(x)x$effect)
      varDyadCovars <- sapply(structure_model$dv_bipartite$varDyadCovars, function(x)x$effect)
      interactions  <- sapply(structure_model$dv_bipartite$interactions, function(x)x$effect)
      
      coCovarTypes      <- sapply(structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
      varCovarTypes     <- sapply(structure_model$dv_bipartite$varCovars, function(x)x$interaction1)
      coDyadCovarTypes  <- sapply(structure_model$dv_bipartite$coDyadCovars, function(x)x$interaction1)
      varDyadCovarTypes <- sapply(structure_model$dv_bipartite$varDyadCovars, function(x)x$interaction1)
      interactionTypes  <- sapply(structure_model$dv_bipartite$interactions, function(x)paste(c(x$interaction1,x$interaction2), collapse = '|'))
      
      component_coCovar_ids     <- grep('self\\$component.+_coCovar', coCovarTypes) ## ex: "self$component_1_coCovar"
      component_varCovar_ids    <- grep('self\\$component.+_varCovar', varCovarTypes) ## ex: "self$component_1_coCovar"
      component_coDyadCovar_ids  <- grep('self\\$component.+_coDyadCovar', coDyadCovarTypes) ## ex: "self$component_1_coCovar"
      component_varDyadCovar_ids <- grep('self\\$component.+_varDyadCovar', varDyadCovarTypes) ## ex: "self$component_1_coCovar"
      
      strat_coCovar_ids     <- grep('self\\$strat.+_coCovar', coCovarTypes) ## ex: "self$component_1_coCovar"
      strat_varCovar_ids    <- grep('self\\$strat.+_varCovar', varCovarTypes) ## ex: "self$component_1_coCovar"
      strat_coDyadCovar_ids  <- grep('self\\$strat.+_coDyadCovar', coDyadCovarTypes) ## ex: "self$component_1_coCovar"
      strat_varDyadCovar_ids <- grep('self\\$strat.+_varDyadCovar', varDyadCovarTypes) ## ex: "self$component_1_coCovar"
      
      interaction_ids <- grep('(self\\$strat.+_coCovar|self\\$strat.+_coDyadCovar|self\\$component.+_coCovar|self\\$component.+_coDyadCovar)', interactionTypes) ## ex: "self$component_1_coCovar"
      
      ncompo_coCovar     <- length( component_coCovar_ids )
      ncompo_varCovar    <- length( component_varCovar_ids )
      ncompo_coDyadCovar  <- length( component_coDyadCovar_ids )
      ncompo_varDyadCovar <- length( component_varDyadCovar_ids )
      
      nstrat_coCovar     <- length( strat_coCovar_ids )
      nstrat_varCovar    <- length( strat_varCovar_ids )
      nstrat_coDyadCovar  <- length( strat_coDyadCovar_ids )
      nstrat_varDyadCovar <- length( strat_varDyadCovar_ids )
      
      ninteraction <- length( interaction_ids )
      
      ##-------------------------------------------------------------
      ## COMPONENTS ##
      if (ncompo_coCovar) {
        for (i in 1:ncompo_coCovar) {
          property <- sprintf('component_%s_coCovar', i)
          eff <- structure_model$dv_bipartite$coCovars[[ component_coCovar_ids[i] ]]
          input_varlist[[sprintf('%s',property)]] <-  coCovar(eff$x, nodeSet = c('COMPONENTS'))
        }
      }
      if (ncompo_varCovar) {
        for (i in 1:ncompo_varCovar) {
          ## _varCovar suffix, not _coCovar: with _coCovar a varCovar overwrote the
          ## coCovar entry of the same index in input_varlist, and no variable
          ## answered to interaction1 = "self$component_i_varCovar".
          property <- sprintf('component_%s_varCovar', i)
          eff <- structure_model$dv_bipartite$varCovars[[ component_varCovar_ids[i] ]]
          input_varlist[[sprintf('%s',property)]] <- varCovar(eff$x, nodeSet = c('COMPONENTS'))
        }
      }
      if (ncompo_coDyadCovar) {
        for (i in 1:ncompo_coDyadCovar) {
          property <- sprintf('component_%s_coDyadCovar', i)
          ## Exact [[ ]] rather than $coDyadCovar: `$` partial-matches, so the
          ## singular name silently resolved to the plural key producers emit.
          ## Adding a real `coDyadCovar` key would flip the match, and an
          ## ambiguous prefix returns NULL -- after which NULL[[1]] is NULL, not
          ## an error, and the failure surfaces far away as "argument is of
          ## length zero". [[ ]] fails here instead, at the line that is wrong.
          eff <- structure_model$dv_bipartite[["coDyadCovars"]][[ component_coDyadCovar_ids[i] ]]
          eff_dim <- dim(eff$x)
          if ( !is.null(eff$nodeSet) ) {
            ## A. user provides nodeSet
            nodeSet <- eff$nodeSet
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$M  &&  eff_dim[2] == self$N ) {
            ## B. M x N with M != N: an actor-by-component covariate
            nodeSet <- c('ACTORS','COMPONENTS')
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$N  &&  eff_dim[2] == self$N ) {
            ## C. N x N with M != N: a component-by-component covariate
            nodeSet <- c('COMPONENTS','COMPONENTS')
          } else {
            ## D. M == N, or dimensions matching neither shape. When M == N the
            ## two shapes above are identical and genuinely ambiguous.
            stop(sprintf('Cannot distinguish actors from components for dimensions M=%s,N=%s; provide nodeSet for effect %s.',
                         self$M, self$N, eff$effect))
          }
          input_varlist[[sprintf('%s',property)]] <-  coDyadCovar(eff$x, nodeSet = nodeSet)
        }
      }
      if (ncompo_varDyadCovar) {
        for (i in 1:ncompo_varDyadCovar) {
          property <- sprintf('component_%s_varDyadCovar', i)
          ## Exact [[ ]] for the same reason as coDyadCovars above.
          eff <- structure_model$dv_bipartite[["varDyadCovars"]][[ component_varDyadCovar_ids[i] ]]
          input_varlist[[sprintf('%s',property)]] <-  varDyadCovar(eff$x, nodeSet = c('COMPONENTS','COMPONENTS'))
        }
      }
      ##-------------------------------------------------------------
      if (nstrat_coCovar) {
        for (i in 1:nstrat_coCovar) {
          property <- sprintf('strat_%s_coCovar', i)
          eff <- structure_model$dv_bipartite$coCovars[[ strat_coCovar_ids[i] ]]
          input_varlist[[sprintf('%s',property)]] <-  coCovar(eff$x, nodeSet = c('ACTORS'))
        }
      }
      if (nstrat_varCovar) {
        for (i in 1:nstrat_varCovar) {
          ## _varCovar suffix, not _coCovar -- see the component varCovar loop above.
          property <- sprintf('strat_%s_varCovar', i)
          eff <- structure_model$dv_bipartite$varCovars[[ strat_varCovar_ids[i] ]]
          input_varlist[[sprintf('%s',property)]] <- varCovar(eff$x, nodeSet = c('ACTORS'))
        }
      }
      if (nstrat_coDyadCovar) {
        for (i in 1:nstrat_coDyadCovar) {
          property <- sprintf('strat_%s_coDyadCovar', i)
          ## strat_, not component_: indexing the component id vector here made a
          ## strategy dyadic covariate fetch the component one whenever both were
          ## present. Exact [[ ]] for the reason given in the component loop above.
          eff <- structure_model$dv_bipartite[["coDyadCovars"]][[ strat_coDyadCovar_ids[i] ]]
          eff_dim <- dim(eff$x)
          if ( !is.null(eff$nodeSet) ) {
            ## A. user provides nodeSet
            nodeSet <- eff$nodeSet
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$M  &&  eff_dim[2] == self$N ) {
            ## B. M x N with M != N: an actor-by-component covariate
            nodeSet <- c('ACTORS','COMPONENTS')
          } else if ( self$M != self$N  &&  length(eff_dim) == 2L  &&
                      eff_dim[1] == self$M  &&  eff_dim[2] == self$M ) {
            ## C. M x M with M != N: an actor-by-actor covariate
            nodeSet <- c('ACTORS','ACTORS')
          } else {
            ## D. M == N, or dimensions matching neither shape.
            stop(sprintf('Cannot distinguish actors from components for dimensions M=%s,N=%s; provide nodeSet for effect %s.',
                         self$M, self$N, eff$effect))
          }
          input_varlist[[sprintf('%s',property)]] <-  coDyadCovar(eff$x, nodeSet = nodeSet)
        }
      }
      if (nstrat_varDyadCovar) {
        for (i in 1:nstrat_varDyadCovar) {
          property <- sprintf('strat_%s_varDyadCovar', i)
          ## Exact [[ ]] for the same reason as coDyadCovars above.
          eff <- structure_model$dv_bipartite[["varDyadCovars"]][[ strat_varDyadCovar_ids[i] ]]
          input_varlist[[sprintf('%s',property)]] <-  varDyadCovar(eff$x, nodeSet = c('ACTORS','ACTORS'))
        }
      }
      ##------------------------------------------------------------
      if (ninteraction) {
        for (i in 1:ninteraction){
          property <- sprintf('interaction_%s', i)
          eff <- structure_model$dv_bipartite$interactions[[ ninteraction[i] ]]
          int1 <- gsub('self\\$','', eff$interaction1 )
          int2 <- gsub('self\\$','', eff$interaction2 )
          interact_item <- list(self[[int1]],  self[[int2]])
          names(interact_item) <- c(int1, int2)
          input_varlist[[ property ]]  <- interact_item
        }
      }
      ##-------------------------------------------------------------
      sienaDataCreate_args <- c( list(nodeSets=list(ACTORS, COMPONENTS)), input_varlist ) 
      rsiena_data <- do.call(sienaDataCreate, sienaDataCreate_args)
      
      
      return(rsiena_data)
    },
    
    
    init_rsiena_model_from_structure_model_bipartite_matrix = function(structure_model, bipartite_matrix, 
                                                                       rand_seed=123) {
      set.seed(rand_seed)
      ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
      COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
      self$config_structure_model <- structure_model
      structure_model_dvs <- names(structure_model)

      ## Simulation baseline nets should not be same; make one small change (self$toggle one dyad)
      ## @see https://www.stats.ox.ac.uk/~snijders/siena/NetworkSimulation.R
      bipartite_matrix1 <- bipartite_matrix
      bipartite_matrix2 <- bipartite_matrix
      # .i <- sample(1:self$M, 1)
      social_matrix1 <- bipartite_matrix1 %*% t(bipartite_matrix1)
      search_matrix1 <- t(bipartite_matrix1) %*% bipartite_matrix1
      social_matrix2 <- bipartite_matrix2 %*% t(bipartite_matrix2)
      search_matrix2 <- t(bipartite_matrix2) %*% bipartite_matrix2
      
      ## init networks to duplicate for the init arrays (two network waves)
      array_bi_net <- array(c(bipartite_matrix1, bipartite_matrix2), dim=c(self$M, self$N, 2) )
      array_social <- array(c(social_matrix1, social_matrix2), dim=c(self$M, self$M, 2) )
      array_search <- array(c(search_matrix1, search_matrix2), dim=c(self$N, self$N, 2) )
      ## Drop information above binary ties for RSiena DVs
      ##**TODO** Simulate potential influence / bias from this information loss
      array_bi_net[ array_bi_net > 1 ] <- 1
      array_social[ array_social > 1 ] <- 1
      array_search[ array_search > 1 ] <- 1
    
      
      if ('dv_social' %in% structure_model_dvs ) {
        self$social_rsienaDV <- sienaDependent(array_social, type='oneMode', nodeSet = 'ACTORS', allowOnly = FALSE)
      }
      if ('dv_search' %in% structure_model_dvs) {
        self$search_rsienaDV <- sienaDependent(array_search, type='oneMode', nodeSet = 'COMPONENTS', allowOnly = FALSE)
      }
      if ('dv_bipartite' %in% structure_model_dvs) {
        self$bipartite_rsienaDV <- sienaDependent(array_bi_net, type='bipartite', nodeSet =c('ACTORS', 'COMPONENTS'), allowOnly = FALSE)
      }
      ## Behavior co-evolution DV; no-op when structure_model has no dv_behavior.
      self$set_behavior_rsienaDV(structure_model)
      ##---------------------------------------------

      ##---------------------------------------------
      ## dv_bipartite remains required: the searchnet engine is built around a
      ## bipartite actor-component DV, and the chain post-processing reads that
      ## DV's ministeps to reconstruct the state trajectory. dv_behavior is an
      ## ADDITIONAL DV that coevolves with it, handled by
      ## get_rsiena_data_from_structure_model() above; dv_social and dv_search
      ## are still not wired into sienaDataCreate().
      if ('dv_bipartite' %in% structure_model_dvs)
      {
        self$rsiena_data <- self$get_rsiena_data_from_structure_model(structure_model)
      }
      else 
      {
        Stop('structural model has no dependent variables.')
      }
      ##---------------------------------------------
      ###
      ###
      
    },
    
    init_multiwave_rsiena_model_from_structure_model_bipartite_matrix = function(structure_model, 
                                                                                 bipartite_matrix1, 
                                                                                 bipartite_matrix2,
                                                                                 rand_seed=123) {
      set.seed(rand_seed)
      ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
      COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
      self$config_structure_model <- structure_model
      structure_model_dvs <- names(structure_model)
      hasCoCovars <- 'coCovars' %in% names(structure_model$dv_bipartite)
      print('DEBUG:  hasCoCovars: ')
      print(hasCoCovars)
      ## Simulation baseline nets should not be same; make one small change (self$toggle one dyad)
      ## @see https://www.stats.ox.ac.uk/~snijders/siena/NetworkSimulation.R
      social_matrix1 <- bipartite_matrix1 %*% t(bipartite_matrix1)
      search_matrix1 <- t(bipartite_matrix1) %*% bipartite_matrix1
      social_matrix2 <- bipartite_matrix2 %*% t(bipartite_matrix2)
      search_matrix2 <- t(bipartite_matrix2) %*% bipartite_matrix2
      ## init networks to duplicate for the init arrays (two network waves)
      array_bi_net <- array(c(bipartite_matrix1, bipartite_matrix2), dim=c(self$M, self$N, 2) )
      array_social <- array(c(social_matrix1, social_matrix2), dim=c(self$M, self$M, 2) )
      array_search <- array(c(search_matrix1, search_matrix2), dim=c(self$N, self$N, 2) )
      ## Drop information above binary ties for RSiena DVs
      array_bi_net[ array_bi_net > 1 ] <- 1
      array_social[ array_social > 1 ] <- 1
      array_search[ array_search > 1 ] <- 1
      if ('dv_social' %in% structure_model_dvs ) {
        self$social_rsienaDV <- sienaDependent(array_social, type='oneMode', nodeSet = 'ACTORS', allowOnly = FALSE)
      }
      if ('dv_search' %in% structure_model_dvs) {
        self$search_rsienaDV <- sienaDependent(array_search, type='oneMode', nodeSet = 'COMPONENTS', allowOnly = FALSE)
      }
      if ('dv_bipartite' %in% structure_model_dvs) {
        self$bipartite_rsienaDV <- sienaDependent(array_bi_net, type='bipartite', nodeSet =c('ACTORS', 'COMPONENTS'), allowOnly = FALSE)
      }
      ##---------------------------------------------
      self$rsiena_data <- self$get_rsiena_data_from_structure_model(structure_model)
    },
    
    
    ##**TODO**
    ##**Create custom RSiena interaction functions for only bipartite DV, **
    ##**but objective function includes statistics of the projections (social net, search landscape)**
    add_rsiena_effects = function(structure_model, verbose=FALSE) {
      
      if (is.null(self$rsiena_effects))
        stop('initiate self$rsiena_effects before adding effects.')

      
      for (i in seq_along(structure_model)) {

        dv <- structure_model[[ i ]]

        if (length(dv$rates)) {
          for (j in seq_along(dv$rates)) {
            if (verbose) cat(sprintf('\n Rate effects i=%s, j=%s\n', i, j))

            eff <- dv$rates[[ j ]]

            if (verbose) print(eff)

            self$include_rsiena_effect_from_eff_list(eff, verbose=verbose)
          }
        }

        if (length(dv$effects)) {
          for (j in seq_along(dv$effects)) {
            if (verbose) cat(sprintf('\n structural effects i=%s, j=%s\n', i, j))

            eff <- dv$effects[[ j ]]

            if (verbose) print(eff)

            self$include_rsiena_effect_from_eff_list(eff, verbose=verbose)
          }
        }

        if (length(dv$coCovars)) {
          for (j in seq_along(dv$coCovars)) {
            if (verbose) cat(sprintf('\n coCovars i=%s, j=%s\n', i, j))

            eff <- dv$coCovars[[ j ]]

            if (verbose) print(eff)

            self$include_rsiena_effect_from_eff_list(eff, verbose=verbose)
          }
        }


        #   ##**TODO** Implement variable covariates(?)

        if (length(dv$coDyadCovars)) {
          for (j in seq_along(dv$coDyadCovars)) {
            if (verbose) cat(sprintf('\n coDyadCovars i=%s, j=%s\n', i, j))

            eff <- dv$coDyadCovars[[ j ]]

            if (verbose) print(eff)

            self$include_rsiena_effect_from_eff_list(eff, verbose=verbose)
          }
        }


        #   ##**TODO** Implement variable covariates(?)
        
        if (length(dv$interactions)) {
          for (j in seq_along(dv$interactions)) {
            if (verbose) cat(sprintf('\n interactions i=%s, j=%s\n', i, j))

            interact <- dv$interactions[[ j ]]

            interact$effects <- strsplit(interact$effect, '[|]')[[1]]

            for (eff_id in seq_along(interact$effects)) {
              interact$effects[ eff_id ] <- strsplit(interact$effects[ eff_id ], '_')[[1]][1]
            }

            if (verbose) print(interact)

            self$include_rsiena_interaction_from_eff_list(interact, verbose=verbose)

          }
        }


      }

    },


    add_rsiena_effects_static = function(rsiena_effects, structure_model, theta_shock=NULL, verbose=FALSE) {

      for (i in seq_along(structure_model)) {

        dv <- structure_model[[ i ]]

        if (length(dv$rates)) {
          for (j in seq_along(dv$rates)) {
            stop('Rate effects not yet supported.')
          }
        }

        if (length(dv$effects)) {
          for (j in seq_along(dv$effects)) {
            if (verbose) cat(sprintf('\n structural effects i=%s, j=%s\n', i, j))
            
            eff <- dv$effects[[ j ]]
            
            if (verbose) print(eff)
            
            if (!is.null(theta_shock) && isTRUE(theta_shock$shock_on == 1) ) {
              shock_eff_id <- which( theta_shock$effect == eff$effect & theta_shock$interaction1 == eff$interaction1 )
              if (length(shock_eff_id)) 
                eff$parameter <- theta_shock$parameter[ shock_eff_id ]
            }
            
            rsiena_effects <- self$include_rsiena_effect_from_eff_list_static(rsiena_effects, eff, verbose=verbose)
          }
        }
        
        if (length(dv$coCovars)) {
          for (j in seq_along(dv$coCovars)) {
            if (verbose) cat(sprintf('\n coCovars i=%s, j=%s\n', i, j))

            eff <- dv$coCovars[[ j ]]

            if (verbose) print(eff)

            if (!is.null(theta_shock) && isTRUE(theta_shock$shock_on == 1) ) {
              shock_eff_id <- which( theta_shock$effect == eff$effect & theta_shock$interaction1 == eff$interaction1 )
              if (length(shock_eff_id))
                eff$parameter <- theta_shock$parameter[ shock_eff_id ]
            }


            rsiena_effects <- self$include_rsiena_effect_from_eff_list_static(rsiena_effects, eff, verbose=verbose)
          }
        }


        #   ##**TODO** Implement variable covariates(?)

        if (length(dv$coDyadCovars)) {
          for (j in seq_along(dv$coDyadCovars)) {
            if (verbose) cat(sprintf('\n coDyadCovars i=%s, j=%s\n', i, j))

            eff <- dv$coDyadCovars[[ j ]]

            if (verbose) print(eff)

            if (!is.null(theta_shock) && isTRUE(theta_shock$shock_on == 1) ) {
              shock_eff_id <- which( theta_shock$effect == eff$effect & theta_shock$interaction1 == eff$interaction1 )
              if (length(shock_eff_id))
                eff$parameter <- theta_shock$parameter[ shock_eff_id ]
            }


            rsiena_effects <- self$include_rsiena_effect_from_eff_list_static(rsiena_effects, eff, verbose=verbose)
          }
        }


        #   ##**TODO** Implement variable covariates(?)

        if (length(dv$interactions)) {
          for (j in seq_along(dv$interactions)) {
            if (verbose) cat(sprintf('\n interactions i=%s, j=%s\n', i, j))

            interact <- dv$interactions[[ j ]]

            interact$effects <- strsplit(interact$effect, '[|]')[[1]]

            if (verbose) print(interact)

            if (!is.null(theta_shock) && isTRUE(theta_shock$shock_on == 1) ) {
              shock_eff_id <- which( theta_shock$effect == interact$effect & theta_shock$interaction1 == interact$interaction1 )
              if (length(shock_eff_id))
                interact$parameter <- theta_shock$parameter[ shock_eff_id ]
            }


            rsiena_effects <- self$include_rsiena_interaction_from_eff_list_static(rsiena_effects, interact, verbose=verbose)

          }
        }
        
        
      }
      
      return(rsiena_effects)
      
    },
    
    
    preview_effects = function(structure_model, filter=TRUE, verbose=FALSE) {
      self$config_structure_model <- structure_model
      
      ##--------- I. SET BIPARTITE NETWORK DV ARRAY IF NULL -----------------
      ## Set up bipartite matrix RSiena dependent variable 
      if(is.null(self$bipartite_matrix)) 
          stop('bipartite_matrix is not set and no array_bi_net provided.')
      array_bi_net <- array(c(self$bipartite_matrix, self$bipartite_matrix), 
                            dim = c(self$M, self$N, 2))

      self$bipartite_rsienaDV <- sienaDependent(array_bi_net,
                                                type='bipartite',
                                                nodeSet =c('ACTORS', 'COMPONENTS'),
                                                allowOnly = FALSE)
      ## Behavior co-evolution DV (structure_model$dv_behavior). Also CLEARS a
      ## behavior DV left over from a previous model when none is declared, so
      ## an environment reused across models cannot carry one over silently.
      self$set_behavior_rsienaDV(structure_model, verbose = verbose)

      ##----------- II. SET RSIENA DATA AND EFFECTS ---------------------------
      ##  1. RSiena data object
      self$rsiena_data <- self$get_rsiena_data_from_structure_model(structure_model)
      ##  2. Init effects
      self$rsiena_effects <- getEffects(self$rsiena_data)
      
      eff_filename <- file.path(self$DIR_OUTPUT, '_rsiena_effects_doc_')
      effectsDocumentation(self$rsiena_effects, type = 'html', display = FALSE, filename = eff_filename)
      
      # ##--2. NETWORK: STRUCTURE EVOLUTION (structure_Model)-----
      # ##  2.1. Add effects from model objective function list
      
      # Parse the HTML
      html <- read_html(sprintf('%s.html', eff_filename))
      
      
      # Extract tables (returns a list of data.frames)
      efftab_list <- html %>% html_table(fill = TRUE)
      efftab <- efftab_list[[1]] # effect table is first in list
      
      if ( ! filter )
        return(efftab)
      
      efftable_dt <- datatable(
        efftab,               
        filter = "top",       # Adds filter boxes at the top
        options = list(
          pageLength = 10,   # Number of rows per page
          autoWidth = TRUE,   # Auto-adjust column width
          dom = 'lfrtip',     # Layout controls (search box, filters, etc.)
          scrollX = TRUE      # Horizontal scrolling if needed
        ),
        class = "display"
      )   # Apply default styling
      
      return(efftable_dt)
      
    },
    
    
    ##**TODO: Check if this can be removed**
    search_rsiena_v1 = function(structure_model, 
                                 get_eff_doc=FALSE,
                                 rsiena_phase2_nsub=1,
                                 rsiena_n2start_scale=1, 
                                 iterations=1000, 
                                 run_seed=123, 
                                 digits=3,
                                 plot_save=FALSE,
                                 return_plot=FALSE
                                 ) {
      if(is.null(self$bipartite_matrix))
        stop('bipartite_matrix is not set.')

      
      ##**self$rsiena_data property** SET rsiena_data FROM 1st 2 bipartite_matrix and structure_model
      self$init_multiwave_rsiena_model_from_structure_model_bipartite_matrix(
        structure_model,
        self$bipartite_matrix, 
        self$bipartite_matrix, ## one tie will be randomly toggled because RSiena sim instructions suggest not using identical networks
        self$rsiena_env_seed
      )
      
      print('self$rsiena_data : ')
      print(self$rsiena_data)
      
      ##  INIT effects list in simulation model (in RSiena model)
      self$rsiena_effects <- getEffects(self$rsiena_data)
      
      ##--2. NETWORK: STRUCTURE EVOLUTION (structure_Model)-----
      ##  2.1. Add effects from model objective function list
      self$add_rsiena_effects(structure_model)
    
      # Effects Documentation
      if(get_eff_doc)
        effectsDocumentation(self$rsiena_effects, type = 'html', display = TRUE)
      ##-----------------------------
      
      
      ## set fix to FALSE for all parameters to be simulated
      self$rsiena_effects$fix <- rep(FALSE, length(self$rsiena_effects$fix) )
      
      ##-----------------------------
      ## RSiena Algorithm
      self$rsiena_run_seed <- run_seed
      self$rsiena_algorithm <- sienaAlgorithmCreate(projname=file.path(self$DIR_OUTPUT,
                                                              sprintf('%s_%s',self$SIM_NAME,self$TIMESTAMP)),
                                                    simOnly = TRUE,
                                                    nsub = 0,
                                                    n3 = iterations,
                                                    seed = run_seed)
      
      # Run RSiena simulation
      self$rsiena_model <- siena07(self$rsiena_algorithm,
                                   data = self$rsiena_data,
                                   effects = self$rsiena_effects,
                                   batch = TRUE,
                                   returnDeps = TRUE,
                                   returnChains = TRUE,
                                   returnThetas = TRUE,
                                   returnDataFrame = TRUE, ##**TODO** CHECK
                                   returnLoglik = TRUE     ##**TODO** CHECK
      )   # returnChains = returnChains
      
      # Summarize and plot results
      mod_summary <- summary(self$rsiena_model)
      if(!is.null(mod_summary))
        print(mod_summary)
      
      
      print(screenreg(list(self$rsiena_model), single.row = TRUE, digits = digits))
      
      ## update simulation object environment from RSiena simulation model
      new_bi_env_igraph <- self$get_bipartite_igraph_from_rsiena_model()
      self$set_system_from_bipartite_igraph( new_bi_env_igraph )
    },
    
    
    get_step = function() {
      if (is.null(self$bi_env_arr)) {
        cat('\nNote: bi_env_arr is empty. Returning iter=0.\n')
        return(0)
      }
      return( dim(self$bi_env_arr)[3] )
    },
    
    get_input_from_structure_model = function(structure_model) {
      if( ! 'dv_bipartite' %in% names(structure_model) ) 
        stop('structure_model does not contain dv_bipartite.')
      rates <- if (length(structure_model$dv_bipartite$rates) > 0) {
        structure_model$dv_bipartite$rates %>% ldply(as.data.frame) %>% mutate(interaction1=NA, interaction2=NA)
      } else {
        data.frame(effect=character(0), parameter=numeric(0), dv_name=character(0), fix=logical(0), interaction1=character(0), interaction2=character(0), stringsAsFactors=FALSE)
      }
      structeffs <- if (length(structure_model$dv_bipartite$effects) > 0) {
        structure_model$dv_bipartite$effects %>% ldply(as.data.frame) %>% mutate(interaction1=NA, interaction2=NA)
      } else {
        data.frame(effect=character(0), parameter=numeric(0), dv_name=character(0), fix=logical(0), interaction1=character(0), interaction2=character(0), stringsAsFactors=FALSE)
      }
      coCovars <- if (length(structure_model$dv_bipartite$coCovars) > 0) {
        structure_model$dv_bipartite$coCovars %>% ldply(function(x) {
          as.data.frame(x[! names(x)%in%c('x')])
        }) %>% mutate(interaction2=NA)
      } else { data.frame() }
      coDyadCovars <- if (length(structure_model$dv_bipartite$coDyadCovars) > 0) {
        structure_model$dv_bipartite$coDyadCovars %>% ldply(function(x) {
          as.data.frame(x[! names(x)%in%c('x','nodeSet')])
        }) %>% mutate(interaction2=NA)
      } else { data.frame() }
      interact_list <- structure_model$dv_bipartite$interactions
      interactions <- if (length(interact_list) == 0) {
        data.frame()
        } else {
          interact_list %>% ldply(function(x) as.data.frame( x ) ) %>% 
            group_by(effect) %>%
            dplyr::summarize(
              effect = unique(effect) ,
              parameter = unique(parameter) ,
              dv_name = unique(dv_name) ,
              interaction1 = paste(interaction1, collapse  = '|')
            )
      }
      ## ---- Behavior co-evolution DV -------------------------------------
      ## get_theta_matrix() filters the RSiena effects table down to effects
      ## whose shortName appears in THIS data frame. A behavior DV's effects
      ## are included in rsiena_effects but absent here, so without these rows
      ## the theta matrix would be built too narrow and siena07() would reject
      ## it ("thetaValues should have N columns"). Empty for every structure
      ## model with no dv_behavior block.
      behavior_rows <- data.frame()
      if (.searchnet_has_behavior(structure_model)) {
        .beh_df <- function(lst) {
          if (!length(lst)) return(data.frame())
          do.call(rbind, lapply(lst, function(x) data.frame(
            effect       = as.character(x$effect),
            parameter    = as.numeric(if (is.null(x$parameter)) 0 else x$parameter),
            dv_name      = as.character(if (is.null(x$dv_name)) structure_model$dv_behavior$name else x$dv_name),
            fix          = as.logical(if (is.null(x$fix)) TRUE else x$fix),
            interaction1 = as.character(if (is.null(x$interaction1)) '' else x$interaction1),
            interaction2 = as.character(if (is.null(x$interaction2)) '' else x$interaction2),
            stringsAsFactors = FALSE
          )))
        }
        behavior_rows <- bind_rows(.beh_df(structure_model$dv_behavior$rates),
                                    .beh_df(structure_model$dv_behavior$effects))
      }

      effects <- as.data.frame(rates %>%
        bind_rows(structeffs) %>%
        bind_rows(coCovars) %>%
        bind_rows(coDyadCovars) %>%
        bind_rows(interactions) %>%
        bind_rows(behavior_rows), stringsAsFactors = FALSE)
      effects$effect_key <-  sapply(1:nrow(effects), function(i){
        paste(c(effects$dv_name[i], ## DV name
                effects$effect[i],  ## effect name
                ifelse(effects$interaction1[i]!='', effects$interaction1[i], NA), ## interactions make unique effect key
                ifelse(effects$interaction2[i]!='', effects$interaction2[i], NA)  ## interactions make unique effect key
        ),collapse = '::')
      })
      ## Effect Names (handle duplicates)
      effects$effect_level <- effects$effect
      dups <- plyr::count(effects$effect) %>% filter(freq > 1)
      if (nrow(dups)) {
        for (i_row in 1:nrow(dups)) {
          rename_ids <- which( effects$effect == dups$x[i_row] )
          effects$effect_level[ rename_ids ] <- paste(dups$x[i_row], 1:dups$freq[i_row], sep='_')
        }
      }
        
      return(effects)
    },
    
    get_theta_matrix = function(input_effs, iterations, verbose=FALSE) {
      ## ---- Conditional vs unconditional estimation sets the theta WIDTH ----
      ## RSiena estimates CONDITIONALLY when the data has exactly one dependent
      ## variable and UNCONDITIONALLY when it has two or more (the `cond = NA`
      ## rule in sienaAlgorithmCreate()). Under conditional estimation
      ## initializeFRAN() DELETES the conditioning variable's basic rate from
      ## the parameter vector, so theta is one column narrower than the
      ## included-effects table -- which is why dropping basic rates has always
      ## been correct here. Under unconditional estimation every basic rate IS
      ## a theta column. siena07() rejects a thetaValues matrix of the wrong
      ## width outright ("should have N columns"), so this must be derived, not
      ## assumed.
      .uncond <- self$get_n_rsiena_depvars() > 1L
      effs <- self$get_rsiena_effects_theta_df(no_rates = !.uncond)
      ## add short name for convenience as effect name
      effs$effect <- sapply(1:nrow(effs), function(i) {
        ifelse(effs$shortName[i]=='unspInt'|is.na(effs$shortName[i]),
               effs$manual_interaction[i],
               effs$shortName[i])
      })
      ## EXCLUDE RATES PARAMETERS NOT INCLUDED IN INPUT MODEL
      .keep <- effs$effect %in% input_effs$effect
      if (.uncond) {
        ## Basic rates are structural under unconditional estimation: RSiena
        ## allocates a theta column for each one whether or not the structure
        ## model bothered to declare it, so the column must exist here too.
        .keep <- .keep | (effs$shortName == 'Rate' & effs$type == 'rate')
      }
      effs <- effs[ .keep , ]
      ## ---- Theta-storage convention (2026-08-23) --------------------------
      ## The coefficient lives in `initialValue`, which every branch of
      ## include_rsiena_effect_from_eff_list() writes via setEffect(). It must
      ## NOT be read from `parm`: `parm` is RSiena's internal effect parameter
      ## (the '#' substitution -- a root exponent for cycle4/inPopX/outActX),
      ## and reading it here is the defect that simulated cycle4 at its parm
      ## default (1) and XWX/X at 0 regardless of the declared coefficient.
      theta_in        <- effs$initialValue
      names(theta_in) <- effs$effect_level
      if (.uncond) {
        ## A basic rate of 0 freezes its dependent variable for the whole
        ## simulation -- no ministeps, no change, ever. That is never what a
        ## caller means. Two cases get the substitute value 1, with a message:
        ##   (a) a rate the structure model never DECLARED -- its initialValue
        ##       is whatever getEffects() derived from the (degenerate,
        ##       two-identical-wave) data, not a caller's choice; before the
        ##       theta-storage repair this row read parm = 0 and was
        ##       substituted, so forcing 1 preserves that behavior exactly;
        ##   (b) a declared rate of NA or <= 0, which would freeze the DV.
        .basic <- (effs$shortName == 'Rate' & effs$type == 'rate')
        .declared <- rep(TRUE, nrow(effs))
        if (any(.basic)) {
          .declared[.basic] <- vapply(which(.basic), function(i) {
            if ('dv_name' %in% names(input_effs))
              any(input_effs$effect == 'Rate' & input_effs$dv_name == effs$name[i])
            else
              'Rate' %in% input_effs$effect
          }, logical(1))
        }
        .subst <- .basic & (!.declared | is.na(theta_in) | theta_in <= 0)
        if (any(.subst)) {
          message(sprintf(
            "searchnet: basic rate for %s was %s; using 1.0. Declare a `rates` entry to control it.",
            paste(effs$name[.subst], collapse = ', '),
            paste(ifelse(!.declared[.subst], 'undeclared',
                         ifelse(is.na(theta_in[.subst]), 'NA', '<= 0')), collapse = ', ')))
          theta_in[.subst] <- 1
        }
      }
      nthetas <- length(theta_in)
      ## Number of decision chain steps to simulate
      theta_matrix <- matrix(NA, nrow = iterations, ncol = nthetas)
      for (j in seq_along(theta_in)) {
        if(verbose) print(names(theta_in)[j])
        theta_matrix[, j] <- rep(theta_in[j], iterations)
      }
      colnames(theta_matrix) <- names(theta_in)
      rownames(theta_matrix) <- seq_len(nrow(theta_matrix))

      #---- Interactions -----
      struct_model_params <- self$get_structure_model_params()
      if (length(struct_model_params$interact_type_ids) ) {
        for (i in seq_along(struct_model_params$interact_type_ids)) {
          int_param <- struct_model_params$covs[ struct_model_params$interact_type_ids[i] ]
          int_eff   <- names(int_param)
          if ( int_eff %in% colnames(theta_matrix) ) {
            theta_matrix[ , int_eff ] <- int_param
          }
        }
      }
        
      return(theta_matrix)
    },

    ## ------------------------------------------------------------------------
    ## prepare_theta_scaffold()
    ## ------------------------------------------------------------------------
    ## Build the RSiena data + effects objects for `structure_model` and return
    ## the DEFAULT theta matrix (iterations x n_parameters) that
    ## `search_rsiena()` would build internally, WITHOUT running the simulation.
    ##
    ## This exists so that per-ministep parameter trajectories (ramps, drifts,
    ## arbitrary user-supplied schedules) can be constructed against a matrix
    ## that is guaranteed to have the right shape and the right column names
    ## in RSiena's own parameter order. Hand-building that matrix is the one
    ## thing a caller cannot reliably do, because the column order is decided
    ## by RSiena's effects table, not by the structure model.
    ##
    ## Side effects: sets self$config_structure_model, self$bipartite_rsienaDV,
    ## self$rsiena_data and self$rsiena_effects -- exactly the same fields
    ## search_rsiena() sets in its steps I and II, and which search_rsiena()
    ## unconditionally rebuilds on its next call. Calling this before
    ## search_rsiena() is therefore safe.
    prepare_theta_scaffold = function(structure_model, iterations, verbose = FALSE) {

      if (self$M < 2)
        stop("prepare_theta_scaffold() requires M >= 2 actors (RSiena's ",
             "sienaDataCreate() does not support single-actor bipartite networks).")
      if (!is.numeric(iterations) || length(iterations) != 1 || iterations < 1)
        stop("`iterations` must be a single positive integer.")
      iterations <- as.integer(iterations)

      bi_mat <- if (!is.null(self$bipartite_matrix)) self$bipartite_matrix else self$bipartite_matrix_init
      if (is.null(bi_mat))
        stop("bipartite_matrix is not set on this environment.")

      array_bi_net <- array(c(bi_mat, bi_mat), dim = c(self$M, self$N, 2))
      self$bipartite_rsienaDV <- sienaDependent(array_bi_net,
                                                type = 'bipartite',
                                                nodeSet = c('ACTORS', 'COMPONENTS'),
                                                allowOnly = FALSE)
      ## Behavior co-evolution DV, when the structure model declares one.
      self$set_behavior_rsienaDV(structure_model, verbose = verbose)

      self$config_structure_model <- structure_model
      input_effs <- self$get_input_from_structure_model(structure_model)

      self$rsiena_data    <- self$get_rsiena_data_from_structure_model(structure_model)
      self$rsiena_effects <- getEffects(self$rsiena_data)
      self$add_rsiena_effects(structure_model, verbose = verbose)

      self$get_theta_matrix(input_effs, iterations, verbose = verbose)
    },

    preprocess_theta_shocks = function(theta_shocks, iterations) {
      if (!length(theta_shocks))
        stop('theta_shocks list is empty. No shocks to process.')
      if (is.null(theta_shocks[[1]]$effect) && is.null(theta_shocks[[1]]$effect_level))
        stop('theta_shocks must have either `effect` or `effect_level`')
      effectvar <- ifelse(
        all(sapply(theta_shocks, function(shock_item) !is.null(shock_item$effect_level) )),
        'effect_level', ## User can provide effect_level "egoX_1","egoX_2",..., if multiple of that type of effect are shocked 
        'effect'        ## User can provide just the effect "egoX" if only 1 of that type of effect is shocked
      )
      dv_name <- self$config_structure_model[[1]]$name
      portions <- sum(sapply(theta_shocks, function(x)ifelse(is.null(x$portion),1,x$portion)))
      chunksteps <- floor(iterations / portions)
      remainder <- iterations - (chunksteps * portions)
      counter <- 0
      for (ii in seq_along(theta_shocks)) {
        
        if ( effectvar == 'effect_level' ) {
          ##---------------- A. user provides `effect_level` ----------------
          effects <- c()
          for (jj in 1:length(theta_shocks[[ii]]$effect_level)) {
            effects[jj] <- strsplit( theta_shocks[[ii]]$effect_level[ jj ], '_')[[1]][1]
          }
          theta_shocks[[ii]]$effect <- effects
          ##-----------------------------------------------------------
        } else {
          ##----------------- B. User provides `effect` -----------------------------
          effect_levels <- c()
          dups <- plyr::count(theta_shocks[[ii]]$effect) %>% filter(freq > 1)
          for (jj in 1:length(theta_shocks[[ii]]$effect)) {
            effect_levels[jj] <- ifelse(nrow(dups), 
                                        paste(theta_shocks[[ ii ]]$effect[ jj ],  jj, sep = '_'), 
                                        theta_shocks[[ ii ]]$effect[ jj ] )
          }
          theta_shocks[[ii]]$effect_level <- effect_levels
          ##-----------------------------------------------------------
        }
        
        prev_counter <-  counter
        counter <- counter + (theta_shocks[[ii]]$portion * chunksteps)
        start <- prev_counter + 1
        end   <- counter
        theta_shocks[[ii]]$chain_step_ids <- (start:end)
        theta_shocks[[ii]]$shock_on <- 1

        if(ii == length(theta_shocks) && remainder>0){
          maxid <- max(theta_shocks[[ii]]$chain_step_ids, na.rm = TRUE)
          theta_shocks[[ii]]$chain_step_ids <- c( theta_shocks[[ii]]$chain_step_ids , (maxid+1):(maxid+remainder) )
        }
          
      }

      return(theta_shocks)
    },
    
    shock_theta_matrix = function(theta_matrix, ## matrix[iterations, n_parameters]
                                  theta_shocks  ## list(shock1list, shock2list, ...)
                                  ) {
      if(is.null(theta_shocks[[1]]$effect_level))
        stop('effect_level not set in theta_shocks. First run preprocess_theta_shocks().')
      if (is.null(theta_shocks[[1]]$chain_step_ids)) {
        theta_shocks <- self$preprocess_theta_shocks(theta_shocks, nrow(theta_matrix))
      }
      portions <- sum(sapply(theta_shocks, function(x)ifelse(is.null(x$portion),1,x$portion)))
      chunksteps <- floor(nrow(theta_matrix) / portions)
      remainder <- nrow(theta_matrix) - (chunksteps * portions)
      for (i_shock in seq_along(theta_shocks)) {
        n_params <- length( theta_shocks[[ i_shock ]]$effect_level )
        for (j_effect in 1:n_params) {
          shock_rows  <- theta_shocks[[ i_shock ]]$chain_step_ids
          theta_matrix_names <- colnames(theta_matrix)
          effect_col <- which(  theta_matrix_names  == theta_shocks[[ i_shock ]]$effect_level[ j_effect ] )
          if (length(shock_rows) && length(effect_col))
            theta_matrix[shock_rows, effect_col] <- theta_shocks[[ i_shock ]]$parameter[ j_effect ]
        }
      }
      ## if remainder, fill matrix remainder rows with the last set row
      if (remainder) {
        last_set_row <- nrow(theta_matrix) - remainder
        last_set_params <- theta_matrix[last_set_row, ]
        fill_rows <- (1+last_set_row):nrow(theta_matrix)
        theta_matrix[fill_rows, ] <- matrix(rep(last_set_params, length(fill_rows)), 
                                            byrow=TRUE, nrow=length(fill_rows))
      }
      return(theta_matrix)
    },
    
    
    ############################################################################
    search_rsiena = function(structure_model, 
                             array_bi_net=NULL, ## starting matrices for the simulation
                             theta_matrix=NULL, ## variable theta matrix replaces parameter values in structure_model
                             theta_shocks=NULL, ## list of parameter shocks (effects, parameter values, portions of simulation)
                             iterations=NULL,   ## replaced by nrow(theta_matrix) if theta_matrix is not null
                             iterations_per_actor=NULL, ## Overrides "iterations" if not NULL
                             run_seed=123, 
                             process_chain=TRUE,
                             get_eff_doc=FALSE,
                             plot_save=FALSE,
                             return_plot=FALSE,
                             verbose=FALSE,
                             digits=3,
                             restart=TRUE
    ) {

      ## RSiena's sienaDataCreate() cannot handle a single-actor (M=1)
      ## bipartite dependent variable: its internal validation drops the
      ## singleton actor dimension and fails with "'x' must be an array of
      ## at least two dimensions". Landscape analysis supports M=1; SAOM
      ## simulation requires M >= 2.
      if (self$M < 2)
        stop("search_rsiena() requires M >= 2 actors: RSiena's ",
             "sienaDataCreate() does not support single-actor bipartite ",
             "networks. Landscape methods (compute_fitness_landscape, ",
             "verify_nk_equivalence) remain available for M = 1.")

      ##system restart to reset init on search
      if(restart)
        self$set_system_from_bipartite_matrix(self$bipartite_matrix_init)
      
      ## Set iterations (steps in simulated decision chain; rows of theta_matrix)
      iterations <- if (!is.null(theta_matrix)) {
        nrow(theta_matrix)
      } else if (!is.null(iterations_per_actor)) {
        (iterations_per_actor * self$M)
      } else if (!is.null(iterations)) {
        iterations
      } else {
        (self$M * self$N) ## default to one component dimensionality-scaled decision round
      }

      self$config_structure_model <- structure_model
      input_effs <- self$get_input_from_structure_model(structure_model)

      ##--------- I. SET BIPARTITE NETWORK DV ARRAY IF NULL -----------------
      ## Set up bipartite matrix RSiena dependent variable 
      if (any(is.null(array_bi_net))) {
        if(is.null(self$bipartite_matrix)) 
          stop('bipartite_matrix is not set and no array_bi_net provided.')
        array_bi_net <- array(c(self$bipartite_matrix, self$bipartite_matrix), 
                              dim = c(self$M, self$N, 2))
      }
      self$bipartite_rsienaDV <- sienaDependent(array_bi_net,
                                                type='bipartite',
                                                nodeSet =c('ACTORS', 'COMPONENTS'),
                                                allowOnly = FALSE)
      ## Behavior co-evolution DV (structure_model$dv_behavior). Also CLEARS a
      ## behavior DV left over from a previous model when none is declared, so
      ## an environment reused across models cannot carry one over silently.
      self$set_behavior_rsienaDV(structure_model, verbose = verbose)

      ##----------- II. SET RSIENA DATA AND EFFECTS ---------------------------
      ##  1. RSiena data object
      self$rsiena_data <- self$get_rsiena_data_from_structure_model(structure_model)
      if(verbose) {
        cat('\n\nself$rsiena_data : \n\n')
        print(self$rsiena_data)
      }
      ##  2.1 RSiena Effects: Init 
      self$rsiena_effects <- getEffects(self$rsiena_data)
      ## UPDATE PARAMETERS TO FIXED=FALSE so we can estimate them
      ##  2.2 RSiena Effects: Add effects from model objective function list
      self$add_rsiena_effects(structure_model, verbose=verbose)
      ## set fix to FALSE for all parameters to be simulated
      if(verbose) {
        cat('\n\nself$rsiena_effects : \n\n')
        print(self$rsiena_effects)
      }
      ##---------- III. SET PARAMETERS (THETA) ------------------------------
      if(any(is.null(theta_matrix))) {
        theta_matrix <- self$get_theta_matrix(input_effs, iterations, verbose=verbose)
      }
      # HANDLE SHOCKS IF APPLICABLE
      if(!is.null(theta_shocks) && length(theta_shocks)) {
        theta_shocks <- self$preprocess_theta_shocks(theta_shocks, iterations)
        self$theta_shocks <- theta_shocks
        theta_matrix <- self$shock_theta_matrix(theta_matrix, theta_shocks)
      }
      self$theta_matrix <- theta_matrix
      if(verbose) {
        cat('\n\n theta_matrix summary (first, middle, last 5 rows) : \n\n')
        print_rows <- c(1:5, NA, (floor(iterations/2)-2):(floor(iterations/2)+2), NA,  (iterations-4):iterations )
        print(theta_matrix[print_rows, ])
      }
      
      ##---------- IV. RUN SIMULATION  -----------------------------
      ##  4. RSiena Algorithm
      self$rsiena_run_seed <- run_seed
      ## `cond` is left at RSiena's default (NA -> TRUE for one dependent
      ## variable) for every single-DV model, i.e. every model that existed
      ## before behavior coevolution. With two DVs RSiena resolves NA to FALSE
      ## anyway; stating it explicitly keeps the theta width computed in
      ## get_theta_matrix() and the width siena07() demands provably in step.
      .cond_arg <- if (self$get_n_rsiena_depvars() > 1L) list(cond = FALSE) else list()
      self$rsiena_algorithm <- if (verbose) {
        do.call(sienaAlgorithmCreate, c(list(
          projname=file.path(self$DIR_OUTPUT, sprintf('%s_%s',self$SIM_NAME,self$TIMESTAMP)),
          simOnly = TRUE,  # nsub = rsiena_phase2_nsub * 1,
          nsub = 0, # n2start = rsiena_n2start_scale * 2.52 * (7+sum(self$rsiena_effects$include)),
          n3 = nrow(theta_matrix), seed = run_seed
        ), .cond_arg))
      } else {
        timestat <- as.numeric(Sys.time()) * 100
        ## Use DIR_OUTPUT for sink file (CWD may not be writable in async workers)
        sink_dir <- if (!is.null(self$DIR_OUTPUT) && dir.exists(self$DIR_OUTPUT)) self$DIR_OUTPUT else tempdir()
        sink_file <- file.path(sink_dir, sprintf("sink_%s.txt", timestat))
        suppressMessages(
          suppressWarnings({
            while (sink.number() > 0) sink(NULL)
            sink(file = sink_file)
            on.exit(sink(), add = TRUE)
            do.call(sienaAlgorithmCreate, c(list(
              projname=file.path(sink_dir, sprintf('%s_%s',self$SIM_NAME,self$TIMESTAMP)),
              simOnly = TRUE,
              nsub = 0,
              n3 = nrow(theta_matrix), seed = run_seed
            ), .cond_arg))
          })
        )
      }
      ## 5. Run RSiena simulation
      self$rsiena_model <- siena07(self$rsiena_algorithm,
                                   data = self$rsiena_data, 
                                   effects = self$rsiena_effects,
                                   thetaValues = theta_matrix,
                                   batch = TRUE,
                                   returnDeps = TRUE, 
                                   returnChains = TRUE,
                                   returnThetas = TRUE,
                                   returnDataFrame = TRUE, ##**TODO** CHECK
                                   returnLoglik = TRUE,   ##**TODO** CHECK
                                   verbose = verbose
      )   
      # 6. Summary of fitted model
      mod_summary <- summary( self$rsiena_model )
      if(verbose &  !is.null(mod_summary))
        print(mod_summary)
      
      
      ##----------- V. POST-PROCESSING OF SIMULATION RESULTS  ------------------
      if(process_chain){
        ## Process the decision ministep chain
        ##  (C++ output to R data.frame: reindexing C++'s 0-index Actor IDs to R's 1-index)
        self$search_rsiena_process_ministep_chain(verbose)
        ## Compute actor and component statistics (may fail gracefully for minimal models)
        tryCatch(
          self$search_rsiena_process_stats(),
          error = function(e) {
            if(verbose) message("Note: chain stats processing skipped: ", e$message)
          }
        )
        if (!is.null(self$bi_env_arr) && length(dim(self$bi_env_arr)) == 3) {
          new_bi_env_matrix <- self$bi_env_arr[,, dim(self$bi_env_arr)[3] ]
          self$set_system_from_bipartite_matrix( new_bi_env_matrix )
        }
      }
      
      ## Show RSiena model screenreg after chain stats (show users what they expect to see at bottom)
      if(verbose)
        print(screenreg(list(self$rsiena_model), single.row = TRUE, digits = digits))
      
    },
    ############################################################################
    
    
    ##_________________GOF_____________
    add_gof_to_rsiena_shocks = function(theta_shocks=NULL) {
      if (is.null(theta_shocks))
        theta_shocks <- self$theta_shocks
      graphics::par(mfrow=c(2,2))
      for (i in 1:length(theta_shocks)) {
        rsiena_model <- theta_shocks[[ i ]]$rsiena_model
        gof.od <- RSiena::sienaGOF(rsiena_model, OutdegreeDistribution, levls=0:(self$N-1), varName = 'bipartite_rsienaDV')
        print(plot(gof.od, main=sprintf('GOF: Outdegree Distribution, i=%s',i)))
        gof.id <- RSiena::sienaGOF(rsiena_model, IndegreeDistribution, levls=0:(self$M-1), varName = 'bipartite_rsienaDV')
        print(plot(gof.id, main=sprintf('GOF: Indegree Distribution, i=%s',i)))
        theta_shocks[[ i ]]$convergence <- list(
          tconv = rsiena_model$tconv, 
          tconv_max = rsiena_model$tconv.max[1],
          check_tconv_lt10= all( abs(rsiena_model$tconv) <  0.1 ),
          check_tconv_max_lt25 = abs( rsiena_model$tconv.max[1] ) < 0.25,
          checK_AAll = ( abs(rsiena_model$tconv) <  0.1 && abs(rsiena_model$tconv.max[1]) < 0.25 )
        )
        print( theta_shocks[[ i ]]$convergence )
        theta_shocks[[ i ]]$rsiena_gof <- list(OutdegreeDistribution = gof.od, IndegreeDistribution  = gof.id)
        ##-----------------------------------
      }
      return( theta_shocks )
    },
    
    
    get_K4_df = function(type='all') {
      
      if (grepl('new', type, ignore.case = TRUE)) {
        
        Kdf <- self$K_AC_NEW_df %>% mutate(effect='K_AC', node_type='Actor', dyad_type='2-mode  (bipartite)')  %>% 
          bind_rows(self$K_AA_NEW_df %>% mutate(effect='K_AA', node_type='Actor', dyad_type='1-mode  (projected)') ) %>%
          bind_rows( self$K_CA_NEW_df %>% mutate(effect='K_CA', node_type='Component', dyad_type='2-mode  (bipartite)') ) %>% 
          bind_rows( self$K_CC_NEW_df %>% mutate(effect='K_CC', node_type='Component', dyad_type='1-mode  (projected)')  ) 
          
        
      } else if (grepl('old', type, ignore.case = TRUE)) {
        
        Kdf <- self$K_AC_OLD_df %>% mutate(effect='K_AC', node_type='Actor', dyad_type='2-mode  (bipartite)')  %>% 
          bind_rows(self$K_AA_OLD_df %>% mutate(effect='K_AA', node_type='Actor', dyad_type='1-mode  (projected)') ) %>%
          bind_rows( self$K_CA_OLD_df %>% mutate(effect='K_CA', node_type='Component', dyad_type='2-mode  (bipartite)') ) %>% 
          bind_rows( self$K_CC_OLD_df %>% mutate(effect='K_CC', node_type='Component', dyad_type='1-mode  (projected)')  ) 
        
      } else { ## 'all','any','NA, etc.
        
        Kdf <- self$K_AC_df %>% mutate(effect='K_AC', node_type='Actor', dyad_type='2-mode  (bipartite)')  %>% 
          bind_rows( self$K_AA_df %>% mutate(effect='K_AA', node_type='Actor', dyad_type='1-mode  (projected)')  ) %>%
          bind_rows( self$K_CA_df %>% mutate(effect='K_CA', node_type='Component', dyad_type='2-mode  (bipartite)') ) %>% 
          bind_rows( self$K_CC_df %>% mutate(effect='K_CC', node_type='Component', dyad_type='1-mode  (projected)')  ) 
      }

      Kdf$dyad_type <- factor(Kdf$dyad_type, levels=c('2-mode  (bipartite)','1-mode  (projected)'))
      ## fill in node_id for actor_id or component_id depending upon node type
      Kdf$node_id <- apply(Kdf[,c('actor_id','component_id')], 1, function(x)na.omit(x)[1])
      Kdf$node_id <- factor(Kdf$node_id, levels=sort(unique(as.numeric(Kdf$node_id))))
      Kdf$component_id <- as.numeric(Kdf$component_id)
      ###----------------
      new_components <- which( colSums(self$bipartite_matrix_init) == 0 )
      component_actor_strats <- sapply(1:self$N, function(x)ifelse(x %in% new_components, "NEW", "OLD"))
      ####----------------
      ##**TODO: CHANGE COMPONENT GROUPING TO COMPONENT INFLUENCE MATRIX BLOCKS (or clusters if not block diagonal)**
      ## clustering is generalization of (full) modularity
      ## - Blondel et al 2008 Louvain clustering is modularity maximzed as the objective of the clustering algorithm 
      ## user actor strategy for color group loess curve
      Kdf$node_group <- 'NA'
      if(length(component_actor_strats)) {
        Kdf$node_group[which(Kdf$node_type=='Component')] <-  sapply( Kdf$component_id[which(Kdf$node_type=='Component')], function(id){
          as.character(component_actor_strats[ id ]) ## fixes factor issue (returning factor level id if not converted to character)
        })
      }
      Kdf$node_group[which(Kdf$node_type=='Actor')] <- as.character(Kdf$strategy[which(Kdf$node_type=='Actor')])
      Kdf$panel_label <- NA
      Kdf$panel_label[which(Kdf$node_type == 'Actor' & Kdf$dyad_type == '1-mode  (projected)' )]      <- 'K_AA'
      Kdf$panel_label[which(Kdf$node_type == 'Actor'  & Kdf$dyad_type == '2-mode  (DGP)' )]           <- 'K_AC'
      Kdf$panel_label[which(Kdf$node_type == 'Component'  & Kdf$dyad_type == '2-mode  (DGP)' )]       <- 'K_CA'
      Kdf$panel_label[which(Kdf$node_type == 'Component'  & Kdf$dyad_type == '1-mode  (projected)' )] <- 'K_CC'
      
      Kdf$panel_label_text <- apply(Kdf[,c('node_type','dyad_type')], 1, function(x)paste(x,collapse = '_'))
      return(Kdf)
    },
  
    
    plot_shocks = function(verbose=FALSE) {
      if(is.null(self$theta_shocks))
        stop('self$theta_shocks is missing.')
      sim_title_str <- self$get_structure_model_param_str()
      theta_shocks <- self$theta_shocks
      actor_strats <- as.factor(self$get_actor_strategies())
      nsteps <- max(unlist(lapply(theta_shocks, function(x)x$chain_step_ids)))
      util <- self$actor_util_df
      util$shock_id <- NA
      util$shock_label <- NA
      util$shock_on <- NA
      util$treatment_group <- 0
      Kdf <- self$get_K4_df()
      Kdf$shock_id <- NA
      Kdf$shock_label <- NA
      Kdf$shock_on <- NA
      Kdf$treatment_group <- 0
      statdf <- self$actor_stats_df
      statdf$shock_id <- NA
      statdf$shock_label <- NA
      statdf$shock_on <- NA
      statdf$treatment_group <- 0
      for (i in 1:length(theta_shocks)) {
        
        shock <- theta_shocks[[ i ]]
        
        util_idx <- which(util$chain_step_id %in% shock$chain_step_ids)
        Kdf_idx  <- which(Kdf$chain_step_id %in% shock$chain_step_ids)
        statdf_idx  <- which(statdf$chain_step_id %in% shock$chain_step_ids)
        
        util$shock_id[ util_idx ]      <- i
        Kdf$shock_id[ Kdf_idx ]        <- i
        statdf$shock_id[ statdf_idx ]  <- i
        
        util$shock_on[ util_idx ]      <- shock$shock_on
        Kdf$shock_on[ Kdf_idx ]        <- shock$shock_on
        statdf$shock_on[ statdf_idx ]  <- shock$shock_on
        
        util$shock_label[ util_idx ]      <- ifelse(is.null(shock$label), as.character(i), shock$label)
        Kdf$shock_label[ Kdf_idx ]        <- ifelse(is.null(shock$label), as.character(i), shock$label)
        statdf$shock_label[ statdf_idx ]  <- ifelse(is.null(shock$label), as.character(i), shock$label)
        
        strat_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% filter(grepl('(self\\$)?strat_\\d{1,2}',effect_key,ignore.case = TRUE))
        ## LOOP EFFECTS j IN SHOCK i
        for (j in 1:nrow(strat_effs)) {
          
          strat_eff_j <- strat_effs[j,]
          strat_eff_shock_eff_id <- which(shock$effect_level == strat_eff_j$effect_level )
          
          is_treated <- FALSE
          if(length(strat_eff_shock_eff_id)) {
            is_treated <- (shock$parameter[ strat_eff_shock_eff_id ] != 0 )
          }
          if(is_treated){
            strat_cov_attr <- gsub('self\\$','', strat_eff_j$interaction1)
            strat_treated_ids <- which( self[[ strat_cov_attr ]] != 0 )
            util_treat_idx <- which( util$actor_id %in% strat_treated_ids )
            Kdf_treat_idx  <- which( Kdf$actor_id %in% strat_treated_ids )
            statdf_treat_idx  <- which( statdf$actor_id %in% strat_treated_ids )
            util$treatment_group[ util_treat_idx ]      <- min(shock$chain_step_ids)
            Kdf$treatment_group[ Kdf_treat_idx ]        <- min(shock$chain_step_ids)
            statdf$treatment_group[ statdf_treat_idx ]  <- min(shock$chain_step_ids)
          }
          
        }##/end j effect loop in shock i
        
      }##/end i shock loop
      
      
      util <- util %>% mutate(value = utility, utility=NULL) ## Swap in utility for the 'value' to be computed
      statdf <- statdf %>% mutate(value = value_contributions, value_contributions=NULL) ## USE VALUE CONTRIBUTIONS
      
      
      pu <- util %>% 
        ggplot(aes(x=shock_id, y=value, color=strategy,fill=strategy)) + 
        geom_point(position='jitter', alpha=.1) + geom_boxplot(aes(shape=shock_label), alpha=.2) + 
        ggtitle(sprintf('Utility by Exogenous Shock\n%s', sim_title_str)) +
        theme_bw()
      
      pk <- Kdf %>% filter(effect %in% c('K_AC','K_AA')) %>%
        ggplot(aes(x=shock_id, y=value, color=node_group,fill=node_group)) + 
        geom_point(position='jitter', alpha=.1) + geom_boxplot(aes(shape=shock_label), alpha=.2) + 
        facet_grid(effect ~ ., scales='free_y') +
        ggtitle(sprintf('Actor and Component Degrees by Exogenous Shock\n%s', sim_title_str)) +
        theme_bw()
      
      ps <- statdf %>% 
        ggplot(aes(x=shock_id, y=value, color=strategy,fill=strategy)) + 
        geom_point(position='jitter', alpha=.1) + geom_boxplot(aes(shape=shock_label), alpha=.2) + 
        facet_grid(effect_level ~ ., scales='free_y') +
        ggtitle(sprintf('Actor Utility Contribution (statistic * theta) by Exogenous Shock\n%s', sim_title_str)) +
        theme_bw()
      
      
      return(list(pu=pu, pk=pk, ps=ps))
      
    },
  
    
    ############################################
    
    compute_K_attribute_shocks = function(K_type='K_AC', ## c('K_AA', 'K_AC')
                                          new_components = NULL,  ## specify which components are "new"
                                          verbose=FALSE) {

      # Helper function to safely convert to numeric
      safe_numeric <- function(x) {
        if (is.numeric(x)) return(x)
        if (is.factor(x)) return(as.numeric(as.character(x)))
        if (is.character(x)) return(as.numeric(x))
        return(x)
      }
      
      if(is.null(self$theta_shocks))
        stop('self$theta_shocks is missing.')
      if ( ! K_type %in% c('K_AA','K_AC')) ## 'K_CA','K_CC' not yet supported
        stop('K_type not supported.')
      
      # Identify new components if not specified
      if (is.null(new_components)) {
        # Components with no initial connections (columns 9-16 based on your data)
        new_components <- which(colSums(self$bipartite_matrix_init) == 0)
        if (verbose) {
          cat("Identified new components:", new_components, "\n")
        }
      }
      
      sim_title_str <- self$get_structure_model_param_str()
      theta_shocks <- self$theta_shocks
      actor_strats <- as.factor(self$get_actor_strategies())
      nsteps <- max(unlist(lapply(theta_shocks, function(x)x$chain_step_ids)))
      
      # Get the K4_df but we'll need to recalculate values for new components only
      # First, let's get the structure from the original K4_df
      Kdf_orig <- self$get_K4_df()
      
      # Create a modified K dataframe focusing only on new components
      if (K_type == 'K_AC') {
        # For K_AC: Calculate actor degrees based only on connections to new components
        
        # Initialize the dataframe with the same structure
        Kdf <- Kdf_orig %>% 
          filter(effect == K_type)
        
        # Remove value column if it exists
        if ("value" %in% names(Kdf)) {
          Kdf <- Kdf %>% select(-value)
        }
        
        # Get unique combinations of chain_step_id and actor_id
        all_steps <- sort(unique(Kdf$chain_step_id))
        all_actors <- sort(unique(safe_numeric(Kdf$actor_id)))  # Use helper function
        
        # Create a complete grid to ensure all actor-step combinations exist
        complete_grid <- expand.grid(
          chain_step_id = all_steps,
          actor_id = all_actors,
          stringsAsFactors = FALSE
        )
        
        # Initialize values vector
        values_new <- numeric(nrow(complete_grid))
        
        # Recalculate degrees for each time step, considering only new components
        for (i in 1:nrow(complete_grid)) {
          step <- complete_grid$chain_step_id[i]
          actor <- complete_grid$actor_id[i]
          
          # Ensure actor is numeric
          if (is.factor(actor) || is.character(actor)) {
            actor <- as.numeric(as.character(actor))
          }
          
          # Get the bipartite matrix at this time step
          if (!is.null(self$bi_env_arr) && dim(self$bi_env_arr)[3] >= step) {
            bi_mat <- self$bi_env_arr[,,step]
            
            # Calculate degree only for new components for this specific actor
            if (!is.na(actor) && actor <= nrow(bi_mat)) {
              values_new[i] <- sum(bi_mat[actor, new_components])
            } else {
              values_new[i] <- 0
            }
          } else {
            values_new[i] <- 0
          }
        }
        
        # Add the calculated values
        complete_grid$value <- values_new
        
        # Merge back with original Kdf to preserve other columns
        if ("value" %in% names(Kdf)) {
          Kdf <- Kdf %>% select(-value)  # Remove any existing value column
        }
        
        Kdf <- Kdf %>%
          mutate(actor_id = safe_numeric(actor_id)) %>%  # Use helper function
          left_join(complete_grid, by = c("chain_step_id", "actor_id")) %>%
          arrange(chain_step_id, actor_id) %>%
          # Add new_components as a column for convenience
          mutate(new_components = paste(new_components, collapse = ","))
        
      } else if (K_type == 'K_AA') {
        # For K_AA: This would need to be calculated based on shared connections to new components
        # This is more complex as it involves actor-actor relationships through new components
        
        stop("K_AA calculation for new components not yet implemented. Use K_AC for now.")
        
        # Implementation would involve:
        # 1. For each pair of actors, count shared connections to new components only
        # 2. Create the actor-actor adjacency matrix based on these shared new component connections
        # 3. Calculate degrees from this filtered adjacency matrix
      }
      
      # Now apply shock information (same as original compute_K_shocks)
      Kdf <- Kdf %>%
        mutate(
          shock_id = NA,
          shock_label = NA,
          shock_on = NA,
          treatment_group = 0
        )
      
      # LOOP SHOCKS i
      for (i in 1:length(theta_shocks)) {
        
        shock <- theta_shocks[[ i ]]
        
        Kdf_idx  <- which(Kdf$chain_step_id %in% shock$chain_step_ids)
        
        Kdf$shock_id[ Kdf_idx ]    <- i
        Kdf$shock_on[ Kdf_idx ]    <- shock$shock_on
        Kdf$shock_label[ Kdf_idx ] <- ifelse(is.null(shock$label), as.character(i), shock$label)
        
        
        if (shock$shock_on == 1) {
          
          shock_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% 
            filter(grepl('(self\\$)?shockable[|]', effect_key, ignore.case = TRUE))
          
          ## Apply the shock information based on shock specification  
          ## THIS ASSIGNS "treatment_group" -- Chain step when treatment starts
          ## 0 means control group (never treated)
          
          if (nrow(shock_effs) > 0) {
            # Loop through shockable effects
            for (j in 1:nrow(shock_effs)) {
              eff <- shock_effs[j,]
              
              # Determine which actors are affected based on effect type
              if (grepl('K_AC', eff$effectName) && K_type == 'K_AC') {
                # For K_AC effects
                k_ac_idx <- which(Kdf$effect == 'K_AC')
                Kdf$treatment_group[k_ac_idx[Kdf$chain_step_id[k_ac_idx] %in% shock$chain_step_ids]] <- min(shock$chain_step_ids)
              }
              
              # Strategy-based treatment assignment
              if (grepl('strat_100', eff$effectName) || grepl('strat_100', eff$effect_key)) {
                strat_actor_idx <- which(Kdf$strategy == '100')
                if (length(strat_actor_idx) > 0) {
                  strat_chain_idx <- which(Kdf$actor_id %in% Kdf$actor_id[strat_actor_idx] & 
                                             Kdf$chain_step_id >= min(shock$chain_step_ids))
                  if (length(strat_chain_idx) > 0) {
                    Kdf$treatment_group[strat_chain_idx] <- min(shock$chain_step_ids)
                  }
                }
              }
              if (grepl('strat_0', eff$effectName) || grepl('strat_0', eff$effect_key)) {
                strat_actor_idx <- which(Kdf$strategy == '0')
                if (length(strat_actor_idx) > 0) {
                  strat_chain_idx <- which(Kdf$actor_id %in% Kdf$actor_id[strat_actor_idx] & 
                                             Kdf$chain_step_id >= min(shock$chain_step_ids))
                  if (length(strat_chain_idx) > 0) {
                    Kdf$treatment_group[strat_chain_idx] <- min(shock$chain_step_ids)
                  }
                }
              }
            }
          } else {
            # Fallback: assign treatment based on strategy
            treated_actors <- unique(Kdf$actor_id[Kdf$strategy == "100"])
            for (actor in treated_actors) {
              actor_idx <- which(Kdf$actor_id == actor & Kdf$chain_step_id >= min(shock$chain_step_ids))
              if (length(actor_idx) > 0) {
                Kdf$treatment_group[actor_idx] <- min(shock$chain_step_ids)
              }
            }
          }
        }
      }
      
      if (verbose) {
        cat("\n=== K_attribute_shocks Summary ===\n")
        cat("K_type:", K_type, "\n")
        cat("New components considered:", new_components, "\n")
        cat("Number of shocks:", length(theta_shocks), "\n")
        cat("Treatment groups:", sort(unique(Kdf$treatment_group)), "\n")
        cat("Data dimensions:", nrow(Kdf), "rows\n")
        cat("Actor IDs:", paste(sort(unique(Kdf$actor_id)), collapse=", "), "\n")
        cat("Value range:", range(Kdf$value, na.rm = TRUE), "\n")
        cat("New components stored in data:", unique(Kdf$new_components)[1], "\n")
        cat("Average degrees to new components:\n")
        summary_df <- Kdf %>% 
          group_by(strategy, shock_id) %>% 
          summarise(
            mean_degree_new = mean(value, na.rm = TRUE),
            n_obs = n(),
            .groups = 'drop'
          )
        print(summary_df)
      }
      
      # Return the modified Kdf focused on new components
      return(Kdf)
    },
    
    test_shocks_new_components = function(model_type = 'did', K_type = 'K_AC', new_components = NULL, verbose = FALSE) {
      
      # Helper function to safely convert to numeric
      safe_numeric <- function(x) {
        if (is.numeric(x)) return(x)
        if (is.factor(x)) return(as.numeric(as.character(x)))
        if (is.character(x)) return(as.numeric(x))
        return(x)
      }
      
      # Get K data for new components
      Kdf_new <- self$compute_K_attribute_shocks(K_type = K_type, 
                                                 new_components = new_components,
                                                 verbose = FALSE)
      
      # Extract new_components from the dataframe if needed (for reporting)
      if (is.null(new_components)) {
        new_comp_str <- unique(Kdf_new$new_components)[1]
        if (!is.na(new_comp_str) && new_comp_str != "") {
          new_components <- as.numeric(unlist(strsplit(new_comp_str, ",")))
        }
      }
      
      # Prepare data for DiD analysis (following compute_K_shocks pattern)
      test_list <- list()
      
      # Get unique treatment/control strategies
      treatment_strategies <- unique(Kdf_new$strategy[Kdf_new$treatment_group > 0])
      control_strategies <- unique(Kdf_new$strategy[Kdf_new$treatment_group == 0])
      
      # Run DiD for each treatment-control pair
      for (trt_lvl in treatment_strategies) {
        for (ctrl_lvl in control_strategies) {
          
          # Prepare data
          did_dat <- Kdf_new %>%
            filter(strategy %in% c(trt_lvl, ctrl_lvl)) %>%
            group_by(chain_step_id, actor_id) %>%
            summarize(
              value_mean = mean(value, na.rm = TRUE),
              strategy = first(strategy),
              treatment_group = first(treatment_group),
              .groups = 'drop'
            ) %>%
            mutate(
              actor_id = as.integer(safe_numeric(actor_id)),
              chain_step_id = as.integer(chain_step_id),
              treatment_group = as.integer(treatment_group)
            ) %>%
            as.data.frame()
          
          if (model_type == 'did') {
            # Define test_key early so it's available throughout
            test_key <- sprintf('treatment_%s__control_%s', trt_lvl, ctrl_lvl)
            
            # FIXED: Calculate first_treated_step BEFORE any operations that use it
            treated_groups <- did_dat$treatment_group[did_dat$treatment_group > 0]
            
            if (length(treated_groups) == 0) {
              if (verbose) {
                cat("\nNo treated units found for", trt_lvl, "vs", ctrl_lvl, "\n")
              }
              next  # Skip to next iteration
            }
            
            first_treated_step <- min(treated_groups)
            
            if (!is.finite(first_treated_step)) {
              if (verbose) {
                cat("\nInvalid first_treated_step for", trt_lvl, "vs", ctrl_lvl, "\n")
              }
              next
            }
            
            # NOW we can use first_treated_step in filters
            # Check if there's sufficient pre-treatment data
            pre_treatment_data <- did_dat %>%
              filter(treatment_group == 0 | chain_step_id < first_treated_step)
            
            pre_periods <- pre_treatment_data %>%
              pull(chain_step_id) %>%
              unique() %>%
              length()
            
            if (pre_periods < 2) {
              if (verbose) {
                cat("\nInsufficient pre-treatment periods (", pre_periods, ") for", trt_lvl, "vs", ctrl_lvl, ". Need at least 2.\n")
              }
              next
            }
            
            # Check if there's variation in the data
            variation_check <- did_dat %>%
              group_by(strategy) %>%
              summarise(
                var = var(value_mean, na.rm = TRUE),
                mean = mean(value_mean, na.rm = TRUE),
                n_unique = n_distinct(value_mean),
                n_obs = n(),
                .groups = 'drop'
              )
            
            if (verbose) {
              cat("\n=== Processing:", trt_lvl, "vs", ctrl_lvl, "===\n")
              cat("First treated step:", first_treated_step, "\n")
              cat("Pre-treatment periods:", pre_periods, "\n")
              cat("Variation check:\n")
              print(variation_check)
            }
            
            # Check if both groups have zero variance (all zeros)
            if (all(variation_check$var == 0 | is.na(variation_check$var))) {
              if (verbose) {
                cat("\nNo variation in either group. All values appear to be zero.\n")
              }
              
              # Create simple result
              did_group <- list(
                overall.att = 0,
                overall.se = NA,
                note = "No variation - all actors have zero connections to new components"
              )
              did_dyna <- list(
                egt = numeric(0),
                att.egt = numeric(0),
                se.egt = numeric(0),
                note = "No variation for dynamic estimation"
              )
              
              test_list[[test_key]] <- list(
                treatment_strategy = trt_lvl,
                control_strategy = ctrl_lvl,
                test_key = test_key,
                first_treated_step = first_treated_step,
                stat_type = paste0(K_type, "_NEW_C", min(new_components), "-C", max(new_components)),
                new_components = new_components,
                did = list(
                  group = did_group,
                  dynamic = did_dyna
                )
              )
              
              next
            }
            
            # Run att_gt with error handling
            did_attgt <- NULL
            did_group <- NULL
            did_dyna <- NULL
            
            tryCatch({
              did_attgt <- did::att_gt(
                yname = 'value_mean',
                tname = 'chain_step_id',
                idname = 'actor_id',
                gname = 'treatment_group',
                data = did_dat,
                panel = TRUE,
                allow_unbalanced_panel = TRUE,
                control_group = 'notyettreated',
                anticipation = 0,
                weightsname = NULL,
                alp = 0.05,
                bstrap = TRUE,
                cband = TRUE,
                biters = 2000,
                clustervars = NULL,
                est_method = "dr",
                base_period = 'universal',
                print_details = FALSE,
                pl = TRUE,
                cores = 4
              )
              
              # Aggregate results with na.rm = TRUE
              did_group <- did::aggte(did_attgt, type = 'group', na.rm = TRUE, cband = FALSE)
              did_dyna <- did::aggte(did_attgt, type = 'dynamic', na.rm = TRUE)
              
            }, error = function(e) {
              if (verbose) {
                cat("\nError in DiD estimation for", test_key, ":\n")
                cat(e$message, "\n")
              }
              
              # Try simple before/after comparison as fallback
              simple_did <- did_dat %>%
                mutate(post = chain_step_id >= first_treated_step) %>%
                group_by(strategy, post) %>%
                summarise(mean_value = mean(value_mean, na.rm = TRUE), .groups = 'drop') %>%
                pivot_wider(names_from = post, values_from = mean_value, names_prefix = "period_")
              
              if (nrow(simple_did) == 2 && all(c("period_FALSE", "period_TRUE") %in% names(simple_did))) {
                treatment_effect <- (simple_did$period_TRUE[simple_did$strategy == trt_lvl] - 
                                       simple_did$period_FALSE[simple_did$strategy == trt_lvl]) -
                  (simple_did$period_TRUE[simple_did$strategy == ctrl_lvl] - 
                     simple_did$period_FALSE[simple_did$strategy == ctrl_lvl])
              } else {
                treatment_effect <- NA
              }
              
              did_group <- list(
                overall.att = treatment_effect,
                overall.se = NA,
                note = "Simple DiD calculation due to estimation error",
                error = e$message
              )
              did_dyna <- list(
                egt = numeric(0),
                att.egt = numeric(0),
                se.egt = numeric(0),
                note = "Insufficient variation for dynamic DiD estimation"
              )
            })
            
            # test_key already defined above
            
            test_list[[test_key]] <- list(
              treatment_strategy = trt_lvl,
              control_strategy = ctrl_lvl,
              test_key = test_key,
              first_treated_step = first_treated_step,
              stat_type = paste0(K_type, "_NEW_C", min(new_components), "-C", max(new_components)),
              new_components = new_components,
              did = list(
                group = did_group,
                dynamic = did_dyna
              )
            )
          }
        }
      }
      
      return(test_list)
    },
    
    plot_K_attribute_shocks = function(K_type = 'K_AC', new_components = NULL, 
                                       plot_type = c("raw", "did", "both")) {

      plot_type <- match.arg(plot_type)
      
      # Get the K data for new components
      Kdf_new <- self$compute_K_attribute_shocks(K_type = K_type, 
                                                 new_components = new_components,
                                                 verbose = TRUE)
      
      # Extract new_components from the dataframe if not provided
      if (is.null(new_components)) {
        # Get from the dataframe column
        new_comp_str <- unique(Kdf_new$new_components)[1]
        if (!is.na(new_comp_str) && new_comp_str != "") {
          new_components <- as.numeric(unlist(strsplit(new_comp_str, ",")))
        } else {
          # Fallback to detecting from bipartite matrix
          new_components <- which(colSums(self$bipartite_matrix_init) == 0)
        }
      }
      
      # Identify shock time
      shock_times <- Kdf_new %>% 
        filter(treatment_group > 0) %>% 
        pull(treatment_group) %>% 
        unique()
      
      if (length(shock_times) == 0) {
        warning("No treatment groups found. Using midpoint of simulation.")
        shock_times <- max(Kdf_new$chain_step_id) / 2
      } else {
        shock_times <- min(shock_times)
      }
      
      # Prepare data
      plot_data <- Kdf_new %>%
        mutate(
          period = factor(ifelse(chain_step_id < shock_times, "Pre-shock", "Post-shock"), levels = c("Pre-shock", "Post-shock")),
          treatment = ifelse(strategy == "100", "Treated", "Control")
        ) %>%
        group_by(chain_step_id, treatment) %>%
        summarise(
          mean_degree = mean(value, na.rm = TRUE),
          se = sd(value, na.rm = TRUE) / sqrt(n()),
          n = n(),
          .groups = 'drop'
        )
      
      # Calculate diff-in-diff data
      did_data <- plot_data %>%
        select(chain_step_id, treatment, mean_degree) %>%
        pivot_wider(names_from = treatment, values_from = mean_degree) %>%
        mutate(
          diff = Treated - Control,  # Treatment effect (negative means treated decreases relative to control)
          # Calculate baseline (pre-treatment average difference)
          pre_control = ifelse(chain_step_id < shock_times, Control, NA),
          pre_treated = ifelse(chain_step_id < shock_times, Treated, NA)
        )
      
      # Calculate pre-treatment average difference
      pre_diff <- mean(did_data$diff[did_data$chain_step_id < shock_times], na.rm = TRUE)
      
      # Adjust for pre-treatment difference
      did_data <- did_data %>%
        mutate(
          diff_adjusted = diff - pre_diff,
          # Normalize control to 0 at shock time for visualization
          control_normalized = Control - Control[which.min(abs(chain_step_id - shock_times))],
          treated_normalized = Treated - Control[which.min(abs(chain_step_id - shock_times))]
        )
      
      if (plot_type == "raw") {
        # Original raw values plot
        p <- plot_data %>%
          ggplot(aes(x = chain_step_id, y = mean_degree, color = treatment)) +
          geom_line(size = 1.2) +
          geom_ribbon(aes(ymin = mean_degree - se, ymax = mean_degree + se, fill = treatment), 
                      alpha = 0.2, color = NA) +
          geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 1) +
          labs(
            title = sprintf("%s for New Components Only (C%d-C%d)", 
                            K_type, 
                            min(new_components), 
                            max(new_components)),
            x = "Chain Step",
            y = "Average Degree (New Components)",
            color = "Group",
            fill = "Group"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom"
          ) +
          scale_y_continuous(limits = c(0, NA))
        
      } else if (plot_type == "did") {
        # Diff-in-diff visualization with control as baseline
        p <- did_data %>%
          pivot_longer(cols = c(control_normalized, treated_normalized), 
                       names_to = "group", 
                       values_to = "value") %>%
          mutate(group = ifelse(group == "control_normalized", "Control (baseline)", "Treated")) %>%
          ggplot(aes(x = chain_step_id, y = value, color = group)) +
          geom_line(size = 1.2) +
          geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
          geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 1) +
          annotate("text", x = shock_times + 10, y = min(did_data$treated_normalized, na.rm = TRUE) * 0.5,
                   label = "Treatment effect\n(negative = less exploration)", 
                   hjust = 0, vjust = 0.5, size = 3) +
          labs(
            title = sprintf("Diff-in-Diff: %s for New Components (C%d-C%d)", 
                            K_type, 
                            min(new_components), 
                            max(new_components)),
            subtitle = "Relative to control group baseline at treatment time",
            x = "Chain Step",
            y = "Degree Difference from Control Baseline",
            color = "Group"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
            legend.position = "bottom"
          )
        
      } else {  # plot_type == "both"
        # Create both plots
        if (!requireNamespace("patchwork", quietly = TRUE)) {
          warning("Package 'patchwork' needed for combined plots. Returning raw plot only.")
          plot_type <- "raw"
          return(self$plot_K_attribute_shocks(K_type = K_type, 
                                              new_components = new_components, 
                                              plot_type = "raw"))
        }

        p1 <- plot_data %>%
          ggplot(aes(x = chain_step_id, y = mean_degree, color = treatment)) +
          geom_line(size = 1) +
          geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 0.8) +
          labs(
            title = "Raw Values",
            x = "Chain Step",
            y = "Average Degree",
            color = "Group"
          ) +
          theme_minimal() +
          scale_y_continuous(limits = c(0, NA))
        
        p2 <- did_data %>%
          ggplot(aes(x = chain_step_id)) +
          geom_line(aes(y = diff_adjusted), size = 1.2, color = "darkblue") +
          geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
          geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 0.8) +
          labs(
            title = "Treatment Effect (Treated - Control)",
            x = "Chain Step",
            y = "Difference in Degrees"
          ) +
          theme_minimal()
        
        p <- p1 / p2 + 
          patchwork::plot_annotation(
            title = sprintf("%s for New Components Only (C%d-C%d)", 
                            K_type, min(new_components), max(new_components)),
            theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          )
      }
      
      return(list(
        data = Kdf_new, 
        plot = p, 
        new_components = new_components,
        did_data = if (exists("did_data")) did_data else NULL
      ))
    },
    
    plot_K_AC_NEW_shocks = function(verbose = FALSE) {

      # Get K data for new components
      Kdf_new <- self$compute_K_attribute_shocks(K_type = 'K_AC', verbose = verbose)
      
      # Get test results for DiD
      test_results <- self$test_shocks_new_components(model_type = 'did', K_type = 'K_AC', verbose = verbose)
      
      # Extract shock information
      theta_shocks <- self$theta_shocks
      shock_idx <- which(sapply(theta_shocks, function(x) x$shock_on == 1))[1]
      shock_time <- min(theta_shocks[[shock_idx]]$chain_step_ids)
      
      # Get environment parameters for title
      sim_title_str <- sprintf("Environment: Actors (M) = %d, Components (N) = %d, Init.P. = %.2f",
                               self$M, self$N, self$p_bipartite_init)
      
      # Check if all values are zero (common for new components)
      all_zero <- all(Kdf_new$value == 0, na.rm = TRUE)
      if (all_zero && verbose) {
        cat("\nNote: All actors have zero connections to new components throughout the simulation.\n")
        cat("This is expected if the subsidy effectively prevents exploration of new activities.\n")
      }
      
      # Create data for top panel (subsidy effect)
      top_data <- Kdf_new %>%
        filter(strategy == "100") %>%  # Only show treated group
        group_by(chain_step_id) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
      
      # Create data for bottom panel (DiD)
      # Calculate the difference between treated and control
      bottom_data <- Kdf_new %>%
        filter(strategy %in% c("100", "0")) %>%
        group_by(chain_step_id) %>%
        summarise(
          treated = mean(value[strategy == "100"], na.rm = TRUE),
          control = mean(value[strategy == "0"], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(diff = treated - control)
      
      # Check if there's variation for meaningful visualization
      has_variation <- var(bottom_data$diff, na.rm = TRUE) > 0
      
      # Create the plots with exact styling
      
      # Top panel
      p_top <- ggplot(top_data, aes(x = chain_step_id, y = value)) +
        geom_rect(aes(xmin = shock_time, xmax = Inf, ymin = -Inf, ymax = Inf),
                  fill = "pink", alpha = 0.3) +
        geom_line(color = "cyan3", size = 1) +
        geom_vline(xintercept = shock_time, linetype = "dashed", 
                   color = "orange2", size = 0.8) +
        geom_hline(yintercept = 0, color = "black", size = 0.3) +
        annotate("text", x = 5, y = max(c(top_data$value, 0.1)) * 0.8, 
                 label = "subsidy", hjust = 0, vjust = 1, size = 3) +
        labs(title = "K_AC_NEW", x = "", y = "") +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major = element_line(color = "gray90", size = 0.3),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.margin = margin(5, 5, 5, 5)
        ) +
        scale_y_continuous(expand = c(0.02, 0), limits = c(0, max(c(top_data$value, 1))))
      
      # Bottom panel
      y_range <- if (has_variation) range(bottom_data$diff) else c(-0.5, 0.5)
      
      p_bottom <- ggplot(bottom_data, aes(x = chain_step_id, y = diff)) +
        geom_hline(yintercept = 0, color = "black", size = 0.5) +
        geom_line(color = "cyan3", size = 1) +
        geom_vline(xintercept = shock_time, linetype = "dashed", 
                   color = "orange2", size = 0.8) +
        annotate("text", x = 5, y = min(y_range) * 0.8, 
                 label = "treatment_100__control_0", hjust = 0, vjust = 1, size = 3) +
        labs(title = "K_AC_NEW", x = "", y = "") +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = "black", size = 0.5),
          panel.grid.major = element_line(color = "gray90", size = 0.3),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.margin = margin(5, 5, 5, 5)
        ) +
        scale_y_continuous(limits = y_range)
      
      # Add note if no variation
      if (!has_variation) {
        p_bottom <- p_bottom +
          annotate("text", x = mean(range(bottom_data$chain_step_id)), y = 0,
                   label = "No variation: Treatment and control have identical outcomes",
                   hjust = 0.5, vjust = 0.5, size = 3, color = "red", fontface = "italic")
      }
      
      # Y-axis labels
      y_lab_top <- textGrob("Avg Degree (New Components)", 
                            rot = 90, gp = gpar(fontsize = 9))
      y_lab_bottom <- textGrob("Avg Treatment Effect on Treated (ATT)", 
                               rot = 90, gp = gpar(fontsize = 9))
      
      # Combine plots
      plots <- arrangeGrob(
        p_top, p_bottom,
        ncol = 1,
        heights = c(1, 1),
        left = y_lab_top  # This will be positioned on the left
      )
      
      # Add main title and subtitle
      title <- textGrob("Degrees to New Components (C9-C16) Only",
                        gp = gpar(fontsize = 12, fontface = "bold"))
      subtitle <- textGrob(sim_title_str,
                           gp = gpar(fontsize = 10))
      
      # Add x-axis label
      x_label <- textGrob(sprintf("Event Time\n(Shock Starts at Simulated Decision Chain Step %d)", 
                                  shock_time),
                          gp = gpar(fontsize = 9))
      
      # Strategy comparison label
      strat_label <- textGrob("Intervention Comparison:    treatment_100__control_0",
                              gp = gpar(fontsize = 9))
      
      # Final assembly
      final_plot <- arrangeGrob(
        title,
        subtitle,
        plots,
        x_label,
        strat_label,
        ncol = 1,
        heights = c(0.06, 0.04, 0.8, 0.05, 0.05)
      )
      
      # Return plot and data
      return(list(
        plot = final_plot,
        data = list(
          top = top_data,
          bottom = bottom_data,
          test_results = test_results
        )
      ))
    },
    
    ############################################
    
    
    # Usage examples:
    compute_K_NEW_shocks = function(K_type='K_AC', ## c('K_AA', 'K_AC')
                                    verbose=FALSE #,experiment = ''
    ) {
      if(is.null(self$theta_shocks))
        stop('self$theta_shocks is missing.')
      if ( ! K_type %in% c('K_AA_NEW','K_AC_NEW')) ## 'K_CA','K_CC' not yet supported
        stop('K_type not supported.')
      sim_title_str <- self$get_structure_model_param_str()
      theta_shocks <- self$theta_shocks
      actor_strats <- as.factor(self$get_actor_strategies())
      nsteps <- max(unlist(lapply(theta_shocks, function(x)x$chain_step_ids)))
      
      new_components <- which(colSums(self$bipartite_matrix_init) == 0)
      
      ###
      Kdf <- self$get_K4_df(type='NEW')
      
      Kdf <- Kdf %>% filter(effect == K_type) %>%
        mutate(
          shock_id = NA,
          shock_label = NA,
          shock_on = NA,
          treatment_group = 0
        )
      
      # LOOP SHOCKS i
      for (i in 1:length(theta_shocks)) {
        
        shock <- theta_shocks[[ i ]]
        
        Kdf_idx  <- which(Kdf$chain_step_id %in% shock$chain_step_ids)
        
        Kdf$shock_id[ Kdf_idx ]    <- i
        Kdf$shock_on[ Kdf_idx ]    <- shock$shock_on
        Kdf$shock_label[ Kdf_idx ] <- ifelse(is.null(shock$label), as.character(i), shock$label)
        
        
        shockable_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% 
          filter(grepl('(strat|component)_\\d{1,2}',effect_key,ignore.case = TRUE))
        ## LOOP EFFECTS j IN SHOCK i
        for (j in 1:nrow(shockable_effs)) {
          
          shock_eff_j <- shockable_effs[j,]
          shock_eff_j_shock_eff_id <- which(sapply(shock$effect_level, function(x){
            any( strsplit(x,'[|]')[[1]]  %in% shock_eff_j )
          }))
          
          is_treated <- FALSE
          if(length(shock_eff_j_shock_eff_id)) {
            is_treated <-  any(shock$parameter[ shock_eff_j_shock_eff_id ] != 0  &  shock$shock_on==1 )
            
          }
          if(is_treated){
            control_strat <-  paste(rep('0', self$get_actor_strategy_set_effects_length()), collapse = '_')
            shock_treated_ids <-  which( self$get_actor_strategies() != control_strat )
            Kdf_treat_idx  <- which( Kdf$actor_id %in% shock_treated_ids )
            if (all( Kdf$treatment_group[ Kdf_treat_idx ] == 0 )) {
              Kdf$treatment_group[ Kdf_treat_idx ]  <- min(shock$chain_step_ids)
            }
            
          }
          
        }##/end j effect loop in shock i
        
      }##/end i shock loop
      
      
      ##------------------------------------------------
      ## TEST STRATEGIES
      ##------------------------------------------------
      actor_strat_lvls <- levels(actor_strats)
      trt_lvl_ids <- which(sapply(actor_strat_lvls, function(x) ! all(as.numeric(strsplit(x, '_')[[1]]) %in% c(0,'0')) ))
      ctrl_lvl_id <-  which(sapply(actor_strat_lvls, function(x)  all(as.numeric(strsplit(x, '_')[[1]]) %in% c(0,'0')) ))
      ctrl_lvl <- actor_strat_lvls[ ctrl_lvl_id]
      
      
      test_list <- list()
      for (ii in 1:length(trt_lvl_ids)) {
        
        trt_lvl <- actor_strat_lvls[ trt_lvl_ids[ii] ]
        
        
        ##---------- K Degrees --------------------
        
        ###
        did_Kdf_dat <- Kdf %>% 
          filter(strategy %in% c(trt_lvl, ctrl_lvl) ) %>%
          group_by( chain_step_id, actor_id) %>% 
          dplyr::summarize(value_mean=mean(value), 
                           strategy = as.factor(first(strategy)), 
                           treatment_group=paste(unique(treatment_group, collapse='|')) ) %>%
          mutate(actor_id = as.numeric(as.character(actor_id)))
        
        if (any(grepl('[|]',did_Kdf_dat$treatment_group))){
          stop('treatment_group error: concatenated multiple groups `|` in summarize function call.')
        }
        
        did_Kdf_dat$treatment_group <- as.numeric( did_Kdf_dat$treatment_group )
        
        
        did_Kdf_attgt <- did::att_gt(
          yname = 'value_mean',
          tname = 'chain_step_id',
          idname = 'actor_id',
          gname = 'treatment_group',
          data = did_Kdf_dat,
          panel = TRUE,
          allow_unbalanced_panel = TRUE,
          control_group = 'notyettreated', # c("nevertreated", "notyettreated"),
          anticipation = 0,
          weightsname = NULL,
          alp = 0.05,
          bstrap = TRUE,
          cband = TRUE,
          biters = 2000,
          clustervars = NULL,
          est_method = "dr", ## "reg", "dr", "ipw"
          base_period = 'universal',#"varying",
          print_details = verbose,
          pl = TRUE,
          cores = 4
        )
        
        
        did_Kdf_group <- did::aggte( did_Kdf_attgt, type = 'group', cband = FALSE)
        # did_stat
        
        did_Kdf_dyna <- did::aggte( did_Kdf_attgt, type = 'dynamic')
        # did_dyna
        
        
        first_treated_step <- min(did_Kdf_dat$treatment_group[ ! did_Kdf_dat$treatment_group %in% c(0,'0') ])
        
        ##----------output list-------------------
        
        test_key <- sprintf('treatment_%s__control_%s',trt_lvl, ctrl_lvl)
        test_list[[ test_key ]] <- list(
          treatment_strategy = trt_lvl,
          control_strategy = ctrl_lvl,
          test_key = test_key,
          first_treated_step = first_treated_step,
          stat_type = K_type,
          did =  list(
            group   = did_Kdf_group,
            dynamic = did_Kdf_dyna
          )
        )
        
      }
      
      
      return(test_list)
      
    },
    
    
    compute_K_shocks = function(K_type='K_AC', ## c('K_AA', 'K_AC')
                                component_type='all', ## 'all', 'old', 'new' (exploration)
                                verbose=FALSE #,experiment = ''
                                ) {
      if(is.null(self$theta_shocks))
        stop('self$theta_shocks is missing.')
      if ( ! grepl('K_A.+', K_type)) ## 'K_CA','K_CC' not yet supported
        stop('K_type not supported.')
      sim_title_str <- self$get_structure_model_param_str()
      theta_shocks <- self$theta_shocks
      actor_strats <- as.factor(self$get_actor_strategies())
      nsteps <- max(unlist(lapply(theta_shocks, function(x)x$chain_step_ids)))
      
      ###
      Kdf <- self$get_K4_df(component_type)
      
      Kdf <- Kdf %>% filter(effect == K_type) %>%
        mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
      
      # LOOP SHOCKS i
      for (i in 1:length(theta_shocks)) {
        
        shock <- theta_shocks[[ i ]]
        
        Kdf_idx  <- which(Kdf$chain_step_id %in% shock$chain_step_ids)
        
        Kdf$shock_id[ Kdf_idx ]    <- i
        Kdf$shock_on[ Kdf_idx ]    <- shock$shock_on
        Kdf$shock_label[ Kdf_idx ] <- ifelse(is.null(shock$label), as.character(i), shock$label)
        
        
        shockable_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% 
          filter(grepl('(strat|component)_\\d{1,2}',effect_key,ignore.case = TRUE))
        ## LOOP EFFECTS j IN SHOCK i
        for (j in 1:nrow(shockable_effs)) {
          
          shock_eff_j <- shockable_effs[j,]
          shock_eff_j_shock_eff_id <- which(sapply(shock$effect_level, function(x){
            any( strsplit(x,'[|]')[[1]]  %in% shock_eff_j )
          }))
          
          is_treated <- FALSE
          if(length(shock_eff_j_shock_eff_id)) {
            is_treated <-  any(shock$parameter[ shock_eff_j_shock_eff_id ] != 0  &  shock$shock_on==1 )
            
          }
          if(is_treated){
            control_strat <-  paste(rep('0', self$get_actor_strategy_set_effects_length()), collapse = '_')
            shock_treated_ids <-  which( self$get_actor_strategies() != control_strat )
            Kdf_treat_idx  <- which( Kdf$actor_id %in% shock_treated_ids )
            if (all( Kdf$treatment_group[ Kdf_treat_idx ] == 0 )) {
              Kdf$treatment_group[ Kdf_treat_idx ]  <- min(shock$chain_step_ids)
            }
            
          }
          
        }##/end j effect loop in shock i
        
      }##/end i shock loop
      
      
      ##------------------------------------------------
      ## TEST STRATEGIES
      ##------------------------------------------------
      actor_strat_lvls <- levels(actor_strats)
      trt_lvl_ids <- which(sapply(actor_strat_lvls, function(x) ! all(as.numeric(strsplit(x, '_')[[1]]) %in% c(0,'0')) ))
      ctrl_lvl_id <-  which(sapply(actor_strat_lvls, function(x)  all(as.numeric(strsplit(x, '_')[[1]]) %in% c(0,'0')) ))
      ctrl_lvl <- actor_strat_lvls[ ctrl_lvl_id]
      
      
      test_list <- list()
      for (ii in 1:length(trt_lvl_ids)) {
        
        trt_lvl <- actor_strat_lvls[ trt_lvl_ids[ii] ]
        
        
        ##---------- K Degrees --------------------
        
        ###
        did_Kdf_dat <- Kdf %>% 
          filter(strategy %in% c(trt_lvl, ctrl_lvl) ) %>%
          group_by( chain_step_id, actor_id) %>% 
          dplyr::summarize(value_mean=mean(value), 
                    strategy = as.factor(first(strategy)), 
                    treatment_group=paste(unique(treatment_group, collapse='|')) ) %>%
          mutate(actor_id = as.numeric(as.character(actor_id)))
        
        if (any(grepl('[|]',did_Kdf_dat$treatment_group))){
          stop('treatment_group error: concatenated multiple groups `|` in summarize function call.')
        }
        
        did_Kdf_dat$treatment_group <- as.numeric( did_Kdf_dat$treatment_group )
        
        
        did_Kdf_attgt <- did::att_gt(
          yname = 'value_mean',
          tname = 'chain_step_id',
          idname = 'actor_id',
          gname = 'treatment_group',
          data = did_Kdf_dat,
          panel = TRUE,
          allow_unbalanced_panel = TRUE,
          control_group = 'notyettreated', # c("nevertreated", "notyettreated"),
          anticipation = 0,
          weightsname = NULL,
          alp = 0.05,
          bstrap = TRUE,
          cband = TRUE,
          biters = 2000,
          clustervars = NULL,
          est_method = "dr", ## "reg", "dr", "ipw"
          base_period = 'universal',#"varying",
          print_details = verbose,
          pl = TRUE,
          cores = 4
        )
        
        
        did_Kdf_group <- did::aggte( did_Kdf_attgt, type = 'group', cband = FALSE)
        # did_stat
        
        did_Kdf_dyna <- did::aggte( did_Kdf_attgt, type = 'dynamic')
        # did_dyna
        

        first_treated_step <- min(did_Kdf_dat$treatment_group[ ! did_Kdf_dat$treatment_group %in% c(0,'0') ])
        
        ##----------output list-------------------
        
        test_key <- sprintf('treatment_%s__control_%s',trt_lvl, ctrl_lvl)
        test_list[[ test_key ]] <- list(
          treatment_strategy = trt_lvl,
          control_strategy = ctrl_lvl,
          test_key = test_key,
          first_treated_step = first_treated_step,
          stat_type = K_type,
          did =  list(
            group   = did_Kdf_group,
            dynamic = did_Kdf_dyna
          )
        )
        
      }
      
      
      return(test_list)
      
    },
    
   
    compute_utility_shocks = function(scale_center=FALSE, scale_scale=FALSE, verbose=FALSE) {  ## experiment=''
      if(is.null(self$theta_shocks))
        stop('self$theta_shocks is missing.')
      sim_title_str <- self$get_structure_model_param_str()
      theta_shocks <- self$theta_shocks
      actor_strats <- as.factor(self$get_actor_strategies())
      nsteps <- max(unlist(lapply(theta_shocks, function(x)x$chain_step_ids)))
      
      util <- self$actor_util_df
      
      if (any(c(scale_center, scale_scale))){
        util$utility <- scale(util$utility, center = scale_center, scale = scale_scale)
        scaled_center <- attr(util$utility, 'scaled:center')
        scaled_scale  <- attr(util$utility, 'scaled:scale')
      }
      
      util <- util %>% mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
      
      # LOOP SHOCKS i
      for (i in 1:length(theta_shocks)) {
        
        shock <- theta_shocks[[ i ]]
        
        util_idx <- which(util$chain_step_id %in% shock$chain_step_ids)
        
        util$shock_id[ util_idx ]    <- i
        util$shock_on[ util_idx ]    <- shock$shock_on
        util$shock_label[ util_idx ] <- ifelse(is.null(shock$label), as.character(i), shock$label)
       
        ## ## Changed from strategy effects to shockable effects include any covariate terms 
        shockable_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% filter(grepl('(self\\$)?(strat|component)_\\d{1,2}',effect_key,ignore.case = TRUE))
        ## LOOP EFFECTS j IN SHOCK i
        for (j in 1:nrow(shockable_effs)) {
          
          shock_eff_j <- shockable_effs[j,]
          shock_eff_j_shock_eff_id <- which(sapply(shock$effect_level, function(x){
            any( strsplit(x,'[|]')[[1]]  %in% shock_eff_j )
          }))
          
          is_treated <- FALSE
          if(length(shock_eff_j_shock_eff_id)) {
            is_treated <-  any(shock$parameter[ shock_eff_j_shock_eff_id ] != 0  &  shock$shock_on==1  )
          }
          if(is_treated){
            shocK_CCov_attr <- gsub('self\\$','', shock_eff_j$interaction1)
            
            control_strat <-  paste(rep('0', self$get_actor_strategy_set_effects_length()), collapse = '_')
            shock_treated_ids <-  which( self$get_actor_strategies() != control_strat )
            util_treat_idx <- which( util$actor_id %in% shock_treated_ids )
            if (all( util$treatment_group[ util_treat_idx ] == 0 )){
              util$treatment_group[ util_treat_idx ]      <- min(shock$chain_step_ids)
            }
          }
          
        }##/end j effect loop in shock i
        
      }##/end i shock loop
      
      
      util <- util %>% mutate(value = utility, utility=NULL) ## Swap in utility for the 'value' to be computed
      
      ##------------------------------------------------
      ## TEST STRATEGIES
      ##------------------------------------------------
      actor_strat_lvls <- levels(actor_strats)
      trt_lvl_ids <- which(sapply(actor_strat_lvls, function(x) ! all(as.numeric(strsplit(x, '_')[[1]]) %in% c(0,'0')) ))
      ctrl_lvl_id <-  which(sapply(actor_strat_lvls, function(x)  all(as.numeric(strsplit(x, '_')[[1]]) %in% c(0,'0')) ))
      ctrl_lvl <- actor_strat_lvls[ ctrl_lvl_id]
      
      test_list <- list()
      for (ii in 1:length(trt_lvl_ids)) {
        
        trt_lvl <- actor_strat_lvls[ trt_lvl_ids[ii] ]
        
        
        ##---------- UTILITY --------------------
        
        ###
        did_util_dat <- util %>% filter(strategy %in% c(trt_lvl, ctrl_lvl) ) %>%
          group_by( chain_step_id, actor_id) %>% 
          dplyr::summarize(value_mean=mean(value), 
                    strategy = as.factor(first(strategy)), 
                    treatment_group=paste(unique(treatment_group, collapse='|')) ) %>%
          mutate(actor_id = as.numeric(as.character(actor_id)))
        
        if (any(grepl('[|]',did_util_dat$treatment_group))){
          stop('treatment_group error: concatenated multiple groups `|` in summarize function call.')
        }
        
        did_util_dat$treatment_group <- as.numeric( did_util_dat$treatment_group )
        
        
        did_util_attgt <- did::att_gt(
          yname = 'value_mean',
          tname = 'chain_step_id',
          idname = 'actor_id',
          gname = 'treatment_group',
          data = did_util_dat,
          panel = TRUE,
          allow_unbalanced_panel = TRUE,
          control_group = 'notyettreated', # c("nevertreated", "notyettreated"),
          anticipation = 0,
          weightsname = NULL,
          alp = 0.05,
          bstrap = TRUE,
          cband = TRUE,
          biters = 2000,
          clustervars = NULL,
          est_method = "dr", ## "reg", "dr", "ipw"
          base_period = 'universal',#"varying",
          print_details = verbose,
          pl = TRUE,
          cores = 4
        )
        
        
        did_util_group <- did::aggte( did_util_attgt, type = 'group', cband = FALSE)
        # did_stat
        
        did_util_dyna <- did::aggte( did_util_attgt, type = 'dynamic')
        # did_dyna
        
        # did_cal
        
        
        first_treated_step <- min(did_util_dat$treatment_group[ ! did_util_dat$treatment_group %in% c(0,'0') ])
        
        
        ##----------output list-------------------
        
        test_key <- sprintf('treatment_%s__control_%s',trt_lvl, ctrl_lvl)
        test_list[[ test_key ]] <- list(
          treatment_strategy = trt_lvl,
          control_strategy = ctrl_lvl,
          test_key = test_key,
          first_treated_step = first_treated_step,
          stat_type = 'utility',
          did =  list(
            group    = did_util_group,
            dynamic  = did_util_dyna
          )
        )
        
      }
      
      
      return(test_list)
      
    },
    
    
    test_shocks_K_AC = function(model_type='did',
                                include_group_test=FALSE,
                                verbose=FALSE) {
      
      if(is.null(self$theta_shocks))
        stop('No shocks to test. Run search_rsiena with theta_shocks list.')
      
      theta_shocks <- self$theta_shocks
      
      shock_starts <- sapply(theta_shocks,function(x) min(x$chain_step_ids) )
      shock_ends <- sapply(theta_shocks,function(x) max(x$chain_step_ids) )
      theta_shock_on_id1 <- min(which( sapply(theta_shocks,function(x)x$shock_on == 1) ))
      
      ## Event (Intervention) Period 
      # min(theta_shocks[[2]]$chain_step_ids) ## generalized in case 2+ off periods before shock_on=1
      first_shock_on_step <- shock_starts[ theta_shock_on_id1 ]
      
      first_shock_on <- theta_shocks[[ theta_shock_on_id1 ]]
      nsteps_first_shock_on <- length(first_shock_on$chain_step_ids)
      
      if (model_type == 'did') {
        
        
        dfgrp <- NULL
        if (include_group_test) {
          ##---------------------
          kb1g     <- data.table::rbindlist( self$test_shocks_did(test_type='group',   stat_type='K_AC', component_type='all'), idcol = 'test_id' )
          kb1g.new <- data.table::rbindlist( self$test_shocks_did(test_type='group',   stat_type='K_AC', component_type='new'), idcol = 'test_id' )
          kb1g.old <- data.table::rbindlist( self$test_shocks_did(test_type='group',   stat_type='K_AC', component_type='old'), idcol = 'test_id' )
          kb1g     <- kb1g %>% mutate(test_type='group', stat_type='K_AC', component_type='C1-C16. All')
          kb1g.new <- kb1g.new %>% mutate(test_type='group', stat_type='K_AC', component_type='C9-C16. New')
          kb1g.old <- kb1g.old %>% mutate(test_type='group', stat_type='K_AC', component_type='C1-C8. Old')
          
          dfgrp <-  bind_rows( kb1g ) %>%
            bind_rows( kb1g.new ) %>%
            bind_rows( kb1g.old ) 
        }

        ##--------------------- 
        kb1d <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type='all', verbose=verbose), idcol = 'test_id' )
        kb1d.new <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type='new', verbose=verbose), idcol = 'test_id' )
        kb1d.old <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type='old', verbose=verbose), idcol = 'test_id' )
        kb1d <- kb1d %>% mutate(test_type='dynamic', stat_type='K_AC', component_type='C1-C16. All')
        kb1d.new  <- kb1d.new %>% mutate(test_type='dynamic', stat_type='K_AC', component_type='C9-C16. New')
        
        kb1d.old  <- kb1d.old %>% mutate(test_type='dynamic', stat_type='K_AC', component_type='C1-C8. Old')
        
        stat_type_levels <- c('K_AC')
        
        dfdyn <-  bind_rows( kb1d ) %>%
          bind_rows( kb1d.new ) %>%
          bind_rows( kb1d.old ) %>%
          filter(event.time >= -1) %>%
          mutate(combined_comparison = 'Common Treatment Scale',
                 stat_type = factor(stat_type, levels=stat_type_levels))
        
        sim_title_str <- self$get_structure_model_param_str()
        
        nstrats <- length(levels(self$get_actor_strategies()))
        color_manual <- scales::hue_pal()(nstrats)[2:nstrats]
        
        
        ##------------------- Plot 1 --------------------------------------------
        plt1 <- ggplot(dfdyn %>% filter(component_type == 'C1-C16. All'), 
                       aes(x=event.time, y=estimate, color=test_id, fill=test_id)) + 
          geom_point(pch=1, size=1.1, alpha=0.6) + 
          geom_line(alpha=.5) +
          geom_ribbon(aes(ymin=point.conf.low, ymax=point.conf.high), alpha=.09) +
          geom_hline(yintercept = 0, linetype=1, color='black') + 
          geom_vline(xintercept = 0, linetype=2, color='black') + 
          facet_wrap( component_type ~ .) +
          # ) +
          # scale_color_brewer(palette = 'Set1') +
          # scale_fill_brewer(palette = 'Set1') +
          scale_color_manual(values = color_manual) +
          scale_fill_manual(values = color_manual) +
          # scale_color_grey() +  ##, direction = -1 ## reverse 
          theme_bw() + theme(legend.position = 'none') +
          labs(
            title = sprintf('%s',sim_title_str), ##'Multiperiod Diff-in-Diff\nPointwise Estimates and Bootstrapped 95CI ',
            y = 'Avg. Treatment Effect on Treated (ATT)',
            x = '', 
            color = 'Intervention Comparison:  ',
            fill = 'Intervention Comparison:  '
          )
        ##------------------ Plot 2 --------------------------------------------
        plt2 <- ggplot(dfdyn %>% filter(component_type != 'C1-C16. All'), 
                       aes(x=event.time, y=estimate)) + 
          geom_point(aes(color=test_id, fill=test_id), pch=1, size=1.1, alpha=0.6) + 
          geom_line(alpha=.5) +
          geom_ribbon(aes(ymin=point.conf.low, ymax=point.conf.high, color=test_id, fill=test_id), alpha=.09) +
          geom_hline(yintercept = 0, linetype=1, color='black') + 
          geom_vline(xintercept = 0, linetype=2, color='black') +
          # scale_color_brewer(palette = 'Set1') +
          # scale_fill_brewer(palette = 'Set1') +
          scale_color_manual(values = color_manual) +
          scale_fill_manual(values = color_manual) +
          # facet_grid(test_id ~ stat_type, scales='free') +
          facet_grid(  test_id ~ component_type , scales = 'free_y') +
          theme_bw() + theme(legend.position = 'bottom') +
          labs(
            y = 'Avg. Treatment Effect on Treated (ATT)',
            x = sprintf('Event Time\n(Shock Starts at Simulated Decision Chain Step %s)',  first_shock_on_step),
            color = 'Intervention Comparison:  ',
            fill = 'Intervention Comparison:  '
          )
        
        ##------------------ Shocks Panels--------------------------------------
        if (!is.null(self$theta_shocks)) {
          
          ##--- plot 1 shock panels --------
          shock_rects1 <- self$get_theta_shock_rects_df(self$theta_shocks)%>%
            mutate(estimate=0, chain_step_id=0, test_id=NA, event.time=0, 
                   start_orig=start, 
                   end_orig=end, 
                   start=start-first_shock_on_step, 
                   end=end-first_shock_on_step)
          suppressMessages({  layout1 <- ggplot_build(plt1)$layout  })
          y_maxs1 <- unlist(lapply(layout1$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects1)) ))
          plt1 <- plt1 + geom_rect(data=shock_rects1, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                                   fill='darkorange', color='orange',linetype=2,  alpha=.05)
          plt1 <- plt1 + geom_text(data = shock_rects1, aes(x = (start + end) / 2, y = y_maxs1, label = label),
                                   vjust = 0, size = 2.7, color='black') #fontface = "bold"
          
          ##--- plot 2 shock panels ---------
          shock_rects2 <- self$get_theta_shock_rects_df(self$theta_shocks)%>%
            mutate(estimate=0, chain_step_id=0, event.time=0, 
                   start_orig=start, 
                   end_orig=end, 
                   start=start-first_shock_on_step, 
                   end=end-first_shock_on_step)
          suppressMessages({  layout2 <- ggplot_build(plt2)$layout  })
          y_maxs2 <- unlist(lapply(layout2$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects2)) ))
          plt2 <- plt2 + geom_rect(data=shock_rects2, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                                   fill='darkorange', color='orange',linetype=2,  alpha=.05)
          plt2 <- plt2 + geom_text(data = shock_rects2, aes(x = (start + end) / 2, y = y_maxs2, label = label),
                                   vjust = 0, size = 2.7, color='black') #fontface = "bold"
        }
        
        
        ##------------------- Combined Plot ------------------------------------
        n_test_ids <- length(unique(as.character(kb1d$test_id)))
        combined_plot <- ggarrange(plotlist=list(plt1, plt2), nrow=2, heights = c(2.5, 1.5+n_test_ids ) )
        
        # Add common title
        combined_title_str <- "Multiperiod Diff-in-Diff Tests of Firm Scope (K_AC): Combined and Grouped by Activity (Old v. New)"
        combined_plot <- ggpubr::annotate_figure(combined_plot, 
                                         top = ggpubr::text_grob(combined_title_str, face = "bold", size = 14)) ##face = "bold"
        
        return(list(
          plot = combined_plot,
          data = list(
            dynamic=dfdyn,
            group = dfgrp
          )
        ))
        
        
      }  else {
        stop('model_type not supported.')
      }
    },
    
    test_shocks_by_component_type = function(model_type='did', 
                                             verbose=FALSE) {
      if(is.null(self$theta_shocks))
        stop('No shocks to test. Run search_rsiena with theta_shocks list.')
        
      theta_shocks <- self$theta_shocks
      
      shock_starts <- sapply(theta_shocks,function(x) min(x$chain_step_ids) )
      shock_ends <- sapply(theta_shocks,function(x) max(x$chain_step_ids) )
      theta_shock_on_id1 <- min(which( sapply(theta_shocks,function(x)x$shock_on == 1) ))
      
      ## Event (Intervention) Period 
      # min(theta_shocks[[2]]$chain_step_ids) ## generalized in case 2+ off periods before shock_on=1
      first_shock_on_step <- shock_starts[ theta_shock_on_id1 ]
      
      first_shock_on <- theta_shocks[[ theta_shock_on_id1 ]]
      nsteps_first_shock_on <- length(first_shock_on$chain_step_ids)
      
      if (model_type == 'did') {
        
        ##--------------------- 
        kb1d <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type='all', verbose=verbose), idcol = 'test_id' )
        kad  <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AA', component_type='all', verbose=verbose), idcol = 'test_id' )
        kb1d.new <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type='new', verbose=verbose), idcol = 'test_id' )
        kad.new  <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AA', component_type='new', verbose=verbose), idcol = 'test_id' )
        kb1d.old <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type='old', verbose=verbose), idcol = 'test_id' )
        kad.old  <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AA', component_type='old', verbose=verbose), idcol = 'test_id' )
        
        kb1d <- kb1d %>% mutate(test_type='dynamic', stat_type='K_AC', component_type='C1-C16. All')
        kad  <- kad  %>% mutate(test_type='dynamic', stat_type='K_AA', component_type='C1-C16. All')
        kb1d.new  <- kb1d.new %>% mutate(test_type='dynamic', stat_type='K_AC', component_type='C9-C16. New')
        kad.new   <- kad.new  %>% mutate(test_type='dynamic', stat_type='K_AA', component_type='C9-C16. New')
        
        kb1d.old  <- kb1d.old %>% mutate(test_type='dynamic', stat_type='K_AC', component_type='C1-C8. Old')
        kad.old   <- kad.old  %>% mutate(test_type='dynamic', stat_type='K_AA', component_type='C1-C8. Old')
        
        stat_type_levels <- c('UTILITY','K_AC','K_AA')
        
        dfdyn <-  bind_rows( kad ) %>% bind_rows( kb1d ) %>%
            bind_rows( kad.new ) %>% bind_rows( kb1d.new ) %>%
            bind_rows( kad.old ) %>% bind_rows( kb1d.old ) %>%
          filter(event.time >= -1) %>%
          mutate(combined_comparison = 'Common Treatment Scale',
                 stat_type = factor(stat_type, levels=stat_type_levels))
        
        sim_title_str <- self$get_structure_model_param_str()
        
        nstrats <- length(levels(self$get_actor_strategies()))
        color_manual <- scales::hue_pal()(nstrats)[2:nstrats]
        
        
        ##------------------- Plot 1 --------------------------------------------
        plt1 <- ggplot(dfdyn %>% filter(component_type == 'C1-C16. All'), 
                       aes(x=event.time, y=estimate, color=test_id, fill=test_id)) + 
          geom_point(pch=1, size=1.1, alpha=0.6) + 
          geom_line(alpha=.5) +
          geom_ribbon(aes(ymin=point.conf.low, ymax=point.conf.high), alpha=.09) +
          geom_hline(yintercept = 0, linetype=1, color='black') + 
          geom_vline(xintercept = 0, linetype=2, color='black') + 
          facet_grid(  stat_type ~ component_type) +
          # ) +
          # scale_color_brewer(palette = 'Set1') +
          # scale_fill_brewer(palette = 'Set1') +
          scale_color_manual(values = color_manual) +
          scale_fill_manual(values = color_manual) +
          # scale_color_grey() +  ##, direction = -1 ## reverse 
          theme_bw() + theme(legend.position = 'none') +
          labs(
            title = sprintf('%s',sim_title_str), ##'Multiperiod Diff-in-Diff\nPointwise Estimates and Bootstrapped 95CI ',
            y = 'Avg. Treatment Effect on Treated (ATT)',
            x = '', 
            color = 'Intervention Comparison:  ',
            fill = 'Intervention Comparison:  '
          )
        ##------------------ Plot 2 --------------------------------------------
        plt2 <- ggplot(dfdyn %>% filter(component_type != 'C1-C16. All'), 
                       aes(x=event.time, y=estimate)) + 
          geom_point(aes(color=test_id, fill=test_id), pch=1, size=1.1, alpha=0.6) + 
          geom_line(alpha=.5) +
          geom_ribbon(aes(ymin=point.conf.low, ymax=point.conf.high, color=test_id, fill=test_id), alpha=.09) +
          geom_hline(yintercept = 0, linetype=1, color='black') + 
          geom_vline(xintercept = 0, linetype=2, color='black') +
          # scale_color_brewer(palette = 'Set1') +
          # scale_fill_brewer(palette = 'Set1') +
          scale_color_manual(values = color_manual) +
          scale_fill_manual(values = color_manual) +
          # facet_grid(test_id ~ stat_type, scales='free') +
          facet_wrap(stat_type ~ component_type, scales = 'free_y') +
          theme_bw() + theme(legend.position = 'bottom') +
          labs(
            y = 'Avg. Treatment Effect on Treated (ATT)',
            x = sprintf('Event Time\n(Shock Starts at Simulated Decision Chain Step %s)',  first_shock_on_step),
            color = 'Intervention Comparison:  ',
            fill = 'Intervention Comparison:  '
          )
        
        ##------------------ Shocks Panels--------------------------------------
        if (!is.null(self$theta_shocks)) {
          
          ##--- plot 1 shock panels --------
          shock_rects1 <- self$get_theta_shock_rects_df(self$theta_shocks)%>%
            mutate(estimate=0, chain_step_id=0, test_id=NA, event.time=0, 
                   start_orig=start, 
                   end_orig=end, 
                   start=start-first_shock_on_step, 
                   end=end-first_shock_on_step)
          suppressMessages({  layout1 <- ggplot_build(plt1)$layout  })
          y_maxs1 <- unlist(lapply(layout1$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects1)) ))
          plt1 <- plt1 + geom_rect(data=shock_rects1, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                                   fill='darkorange', color='orange',linetype=2,  alpha=.05)
          plt1 <- plt1 + geom_text(data = shock_rects1, aes(x = (start + end) / 2, y = y_maxs1, label = label),
                                   vjust = 0, size = 2.7, color='black') #fontface = "bold"
          
          ##--- plot 2 shock panels ---------
          shock_rects2 <- self$get_theta_shock_rects_df(self$theta_shocks)%>%
            mutate(estimate=0, chain_step_id=0, event.time=0, 
                   start_orig=start, 
                   end_orig=end, 
                   start=start-first_shock_on_step, 
                   end=end-first_shock_on_step)
          suppressMessages({  layout2 <- ggplot_build(plt2)$layout  })
          y_maxs2 <- unlist(lapply(layout2$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects2)) ))
          plt2 <- plt2 + geom_rect(data=shock_rects2, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                                   fill='darkorange', color='orange',linetype=2,  alpha=.05)
          plt2 <- plt2 + geom_text(data = shock_rects2, aes(x = (start + end) / 2, y = y_maxs2, label = label),
                                   vjust = 0, size = 2.7, color='black') #fontface = "bold"
        }
        
        
        ##------------------- Combined Plot ------------------------------------
        combined_plot <- ggarrange(plotlist=list(plt1, plt2), nrow=2, heights = c(2,2) )
        
        # Add common title
        combined_title_str <- "Multiperiod Diff-in-Diff Tests of Degrees (K_AC, K_AA): Combined and Grouped by Activity (Old, New)"
        combined_plot <- ggpubr::annotate_figure(combined_plot, 
                                         top = ggpubr::text_grob(combined_title_str, face = "bold", size = 14)) ##face = "bold"
        
        return(list(
          plot = combined_plot,
          data = dfdyn
        ))
        
        
      }  else {
        stop('model_type not supported.')
      }
    },
    
    
    test_shocks = function(model_type='did', 
                           component_type='all', ## 'all', 'old', 'new' (exploration proxy)
                           verbose=FALSE) {
      if(is.null(self$theta_shocks))
        stop('No shocks to test. Run search_rsiena with theta_shocks list.')
      
      theta_shocks <- self$theta_shocks
      
      shock_starts <- sapply(theta_shocks,function(x) min(x$chain_step_ids) )
      shock_ends <- sapply(theta_shocks,function(x) max(x$chain_step_ids) )
      theta_shock_on_id1 <- min(which( sapply(theta_shocks,function(x)x$shock_on == 1) ))
      
      ## Event (Intervention) Period 
      # min(theta_shocks[[2]]$chain_step_ids) ## generalized in case 2+ off periods before shock_on=1
      first_shock_on_step <- shock_starts[ theta_shock_on_id1 ]
      
      first_shock_on <- theta_shocks[[ theta_shock_on_id1 ]]
      nsteps_first_shock_on <- length(first_shock_on$chain_step_ids)
      
      if (model_type == 'did') {
        
        ##---------------------
        ##---------------------
        ud   <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='UTILITY', verbose=verbose), idcol = 'test_id' )
        kb1d <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AC', component_type=component_type, verbose=verbose), idcol = 'test_id' )
        kad  <- data.table::rbindlist( self$test_shocks_did(test_type='dynamic', stat_type='K_AA', component_type=component_type, verbose=verbose), idcol = 'test_id' )
        ud   <- ud   %>% mutate(test_type='dynamic', stat_type='UTILITY')
        kb1d <- kb1d %>% mutate(test_type='dynamic', stat_type='K_AC')
        kad  <- kad  %>% mutate(test_type='dynamic', stat_type='K_AA')
        
        stat_type_levels <- c('UTILITY','K_AC','K_AA')
        
        dfdyn <-  ud %>% bind_rows( kad ) %>% bind_rows( kb1d ) %>%
          filter(event.time >= -1) %>%
          mutate(combined_comparison = 'Common Treatment Scale',
                 stat_type = factor(stat_type, levels=stat_type_levels))
        
        sim_title_str <- self$get_structure_model_param_str()
        
        nstrats <- length(levels(self$get_actor_strategies()))
        color_manual <- scales::hue_pal()(nstrats)[2:nstrats]
        
        
        ##------------------- Plot 1 --------------------------------------------
        plt1 <- ggplot(dfdyn, aes(x=event.time, y=estimate, color=test_id, fill=test_id)) + 
          geom_point(pch=1, size=1.1, alpha=0.6) + 
          geom_line(alpha=.5) +
          geom_ribbon(aes(ymin=point.conf.low, ymax=point.conf.high), alpha=.09) +
          geom_hline(yintercept = 0, linetype=1, color='black') + 
          geom_vline(xintercept = 0, linetype=2, color='black') + 
          facet_grid(combined_comparison ~ stat_type) +
          # scale_color_brewer(palette = 'Set1') +
          # scale_fill_brewer(palette = 'Set1') +
          scale_color_manual(values = color_manual) +
          scale_fill_manual(values = color_manual) +
          # scale_color_grey() +  ##, direction = -1 ## reverse 
          theme_bw() + theme(legend.position = 'none') +
          labs(
            title = sprintf('%s',sim_title_str), ##'Multiperiod Diff-in-Diff\nPointwise Estimates and Bootstrapped 95CI ',
            y = 'Avg. Treatment Effect on Treated (ATT)',
            x = '', 
            color = 'Intervention Comparison:  ',
            fill = 'Intervention Comparison:  '
          )
        ##------------------ Plot 2 --------------------------------------------
        plt2 <- ggplot(dfdyn, aes(x=event.time, y=estimate)) + 
          geom_point(aes(color=test_id, fill=test_id), pch=1, size=1.1, alpha=0.6) + 
          geom_line(alpha=.5) +
          geom_ribbon(aes(ymin=point.conf.low, ymax=point.conf.high, color=test_id, fill=test_id), alpha=.09) +
          geom_hline(yintercept = 0, linetype=1, color='black') + 
          geom_vline(xintercept = 0, linetype=2, color='black') +
          # scale_color_brewer(palette = 'Set1') +
          # scale_fill_brewer(palette = 'Set1') +
          scale_color_manual(values = color_manual) +
          scale_fill_manual(values = color_manual) +
          # facet_grid(test_id ~ stat_type, scales='free') +
          facet_wrap(test_id ~ stat_type, scales = 'free_y') +
          theme_bw() + theme(legend.position = 'bottom') +
          labs(
            y = 'Avg. Treatment Effect on Treated (ATT)',
            x = sprintf('Event Time\n(Shock Starts at Simulated Decision Chain Step %s)',  first_shock_on_step),
            color = 'Intervention Comparison:  ',
            fill = 'Intervention Comparison:  '
          )
        
        ##------------------ Shocks Panels--------------------------------------
        if (!is.null(self$theta_shocks)) {
          
          ##--- plot 1 shock panels --------
          shock_rects1 <- self$get_theta_shock_rects_df(self$theta_shocks)%>%
            mutate(estimate=0, chain_step_id=0, test_id=NA, event.time=0, 
                   start_orig=start, 
                   end_orig=end, 
                   start=start-first_shock_on_step, 
                   end=end-first_shock_on_step)
          suppressMessages({  layout1 <- ggplot_build(plt1)$layout  })
          y_maxs1 <- unlist(lapply(layout1$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects1)) ))
          plt1 <- plt1 + geom_rect(data=shock_rects1, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                                   fill='darkorange', color='orange',linetype=2,  alpha=.05)
          plt1 <- plt1 + geom_text(data = shock_rects1, aes(x = (start + end) / 2, y = y_maxs1, label = label),
                                   vjust = 0, size = 2.7, color='black') #fontface = "bold"
          
          ##--- plot 2 shock panels ---------
          shock_rects2 <- self$get_theta_shock_rects_df(self$theta_shocks)%>%
            mutate(estimate=0, chain_step_id=0, event.time=0, 
                   start_orig=start, 
                   end_orig=end, 
                   start=start-first_shock_on_step, 
                   end=end-first_shock_on_step)
          suppressMessages({  layout2 <- ggplot_build(plt2)$layout  })
          y_maxs2 <- unlist(lapply(layout2$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects2)) ))
          plt2 <- plt2 + geom_rect(data=shock_rects2, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                                   fill='darkorange', color='orange',linetype=2,  alpha=.05)
          plt2 <- plt2 + geom_text(data = shock_rects2, aes(x = (start + end) / 2, y = y_maxs2, label = label),
                                   vjust = 0, size = 2.7, color='black') #fontface = "bold"
        }
        
        
        ##------------------- Combined Plot ------------------------------------
        combined_plot <- ggarrange(plotlist=list(plt1, plt2), nrow=2)
        
        # Add common title
        combined_title_str <- if(grepl('new', component_type, ignore.case = TRUE)) {
          "Multiperiod Diff-in-Diff Tests of Actor Utility and Degrees (K_AC_NEW, K_AA_NEW)"
        } else {
          "Multiperiod Diff-in-Diff Tests of Actor Utility and Degrees (K_AC, K_AA)"
        }
        combined_plot <- ggpubr::annotate_figure(combined_plot, 
                                         top = ggpubr::text_grob(combined_title_str, face = "bold", size = 14)) ##face = "bold"
        
        return(list(
          plot = combined_plot,
          data = dfdyn
        ))
        
        
      }  else {
        stop('model_type not supported.')
      }
    },
    
    
    test_shocks_did = function(test_type = 'dynamic', ## c('dynamic','group')
                               stat_type = 'utility',  ## c('utility','K_AA','K_AC','K_CA','K_CC') ##,'utility_contributions, 'net_stats')
                               component_type = 'all',
                               scale_center=FALSE,
                               scale_scale=FALSE,
                               verbose = FALSE
                               ) {
      
      test_list <- if(grepl('K_', stat_type)) {
        self$compute_K_shocks(K_type = stat_type, 
                              component_type = component_type, 
                              verbose = verbose)
      } else {
        self$compute_utility_shocks(scale_center=scale_center, scale_scale=scale_scale, verbose = verbose)
      }
      
      test_key <- switch(test_type, 
                         group   = 'group',
                         dynamic = 'dynamic')
      if (is.null(test_key))
        stop(sprintf('test_type=`%s` not supported.', test_type))
      
      reg_table_list <- lapply(test_list, function(test) {
        model_test <- test$did[[ test_key ]]
        reg_df <- tidy(model_test) %>% 
          mutate(`(sig.)`= ifelse( (point.conf.low * point.conf.high) < 0 | is.na(point.conf.low), '', ' * ')) %>% 
          select( !c('type') ) %>% mutate(first_treated_step = test$first_treated_step)
        reg_df
      })
      
      return(reg_table_list)
      
    },
    
    
    fit_rsiena_shocks = function(n_obs=8, digits=3, add_gof=TRUE, verbose=FALSE) {
      if (is.null(self$theta_shocks))
        stop('theta_shocks is not set.')
      if (is.null(self$theta_shocks[[1]]$chain_step_ids))
        stop('theta_shocks missing chain_step_ids.')
      
      limit_obs <- 30
      max_obs <- ifelse( dim(self$bi_env_arr)[3] <= limit_obs, dim(self$bi_env_arr)[3],  limit_obs )
        
      theta_shocks <- self$theta_shocks
      
      
      for (i in 1:length(theta_shocks)) {
        theta_shock <- theta_shocks[[ i ]]
        ## all step ids in this shock period
        shock_step_ids <- theta_shock$chain_step_ids
        ## observation step ids to use for RSiena estimation of the model
        obs_step_ids <- round(seq(min(shock_step_ids), max(shock_step_ids), length.out=n_obs))
        if (length(obs_step_ids) > max_obs)
          obs_step_ids <- obs_step_ids[ 1:max_obs ]
        bi_env_obs <- self$bi_env_arr[ , , obs_step_ids ]
        rsiena_model <- self$fit_rsiena_static(bi_env_obs, theta_shock, verbose=verbose)
        # #
        theta_shocks[[ i ]]$rsiena_model <- rsiena_model
        # #
      }
      
      if (add_gof)
        theta_shocks <- self$add_gof_to_rsiena_shocks(theta_shocks)
      
      
      ## update self$theta_shocks
      self$theta_shocks <- theta_shocks
      
      return(theta_shocks)
    },


    ###
    ###
    ###
    ## Uses self$config_structure_model. Before 0.10.0 both call sites below
    ## referenced a bare `structure_model` that is neither a formal of this
    ## method nor a field, so the function could only raise "object not
    ## found". Its sole caller, fit_rsiena_shocks(), was broken with it, and
    ## no test names either function.
    fit_rsiena_static = function(bi_env_arr, theta_shock=NULL, 
                                  digits=3, iterations_multiplier=4, 
                                  verbose=FALSE) {

      if (is.null(self$rsiena_model))
        stop('rsiena_model is missing. Run search_rsiena() before fit_rsiena')
      if(is.null(self$rsiena_model$thetaUsed))
        stop('rsiena_model$thetaUsed is missing. Set thetaValues=theta_matrix in siena07().')

      ##---------- DATA AND EFFECTS ------------------------------
      # 1. DV
      bipartite_rsienaDV <- sienaDependent(bi_env_arr,
                                          type='bipartite',
                                          nodeSet =c('ACTORS', 'COMPONENTS'),
                                          allowOnly = FALSE)

      ## input list of variable for RSiena model
      input_varlist <- list(bipartite_rsienaDV=bipartite_rsienaDV)
      ## 2. Data
      rsiena_data <- self$get_rsiena_data_static(self$config_structure_model, input_varlist) ## does not affect self$... properties
      ## 3.1 Effects: init
      rsiena_effects <- RSiena::getEffects(rsiena_data)
      ## 3.1 Effects: Add from structure model
      rsiena_effects <- self$add_rsiena_effects_static(rsiena_effects, self$config_structure_model, theta_shock)

      if(verbose) {
        cat('\n\nself$rsiena_data : \n\n')
        print(rsiena_effects)
      }

      ##---------- ESTIMATION  -----------------------------
      iterations <-  self$rsiena_model$n3 * iterations_multiplier
      ##  4. RSiena Algorithm
      rsiena_fit_algorithm <- sienaAlgorithmCreate(projname=file.path(self$DIR_OUTPUT, sprintf('%s_%s',self$SIM_NAME,as.numeric(Sys.time())*100)),
                                                    simOnly = FALSE,  # nsub = rsiena_phase2_nsub * 1,
                                                    n3 = iterations,
                                                    seed = self$rsiena_run_seed)
      ## 5. Run RSiena simulation
      rsiena_model <- siena07(rsiena_fit_algorithm,
                             data = rsiena_data,
                             effects = rsiena_effects,# thetaValues = theta_matrix,
                             batch = TRUE,
                             returnDeps = TRUE,
                             returnThetas = TRUE,
                             returnDataFrame = TRUE, ##**TODO** CHECK
                             returnLoglik = TRUE,   ##**TODO** CHECK
                             verbose = verbose
      )   # returnChains = returnChains


      ##---------- POST PROCESSING  -----------------------------
      ## 6. Print Summary and Regression Table
      mod_summary <- summary( rsiena_model )
      if(verbose &  !is.null(mod_summary))
        print(mod_summary)

    
      return(rsiena_model)   
    },
    
    
    plot_snapshots = function(snapshot_ids=c(), include_init=TRUE) {
      if(!length(snapshot_ids)) 
        snapshot_ids <- c(1, 2, dim(self$bi_env_arr)[3]  )
      if(include_init)
        snapshot_ids <- c(0, snapshot_ids)
      for (i in 1:length(snapshot_ids)) {
        step <- snapshot_ids[ i ]
        mat <- if (step == 0) {
          self$bipartite_matrix_init
        } else {
          self$bi_env_arr[,,step]
        }
        self$plot_bipartite_system_from_mat(mat, step)
      }
    },
    
    
    animate_snapshots = function(snapshot_ids=c()) {
      ##########################
      ##**TODO**
      ##*      ##**TODO**
      ##*            ##**TODO**
      ##*                  ##**TODO**
      ##*                        ##**TODO**
      ##*                              ##**TODO**
      ##*                                    ##**TODO**
      ##*                                          ##**TODO**
      ##*                                                ##**TODO**
      ##*                                                      ##**TODO**
      ###########################
        
        if(!length(snapshot_ids)) 
          snapshot_ids <- c(1, 2, dim(self$bi_env_arr)[3]  )
        for (i in 1:length(snapshot_ids)) {
          step <- snapshot_ids[ i ]
          mat <- self$bi_env_arr[,,step]
          self$plot_bipartite_system_from_mat(mat, step)
        }
        
        
        # Add animation elements
        anim <- p + 
          # Add actor nodes that move
          geom_point(data = all_actor_positions,
                     aes(x = x, y = y, color = strategy, group = actor_id),
                     shape = 1,
                     size = actor_size * 3) +
          # Add actor labels that move with the actors
          geom_text(data = all_actor_positions,
                    aes(x = x, y = y, label = actor_id, group = actor_id),
                    vjust = -1.5, size = 3) +
          # Add edges that change at each step
          geom_segment(data = all_edges,
                       aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                           alpha = weight, group = paste(from, to, step)),
                       color = rgb(.5,.5,.5, .4) ) + #scale_size_continuous(range = c(0.5, 2)) +
          # Add path segments that appear over time
          geom_segment(data = path_animation_data,
                       aes(x = x, y = y, xend = xend, yend = yend, 
                           color = strategy, group = paste(actor, step)),
                       arrow = arrow(type = "closed", length = unit(path_arrow_size, "inches")),
                       linewidth = path_linewidth) +
          # Define the transition
          gganimate::transition_states(
            states = step,
            transition_length = 2,
            state_length = 3
          ) +
          # Add view_follow to keep focus on the moving actors
          # gganimate::view_follow(fixed_y = TRUE) +
          gganimate::ease_aes('linear') ##+ gganimate::shadow_trail(past = FALSE, future = FALSE)  # Show the path traveled
        
        # Render the animation
        animated_plot <- gganimate::animate(
          anim,
          nframes = S * 3,  # 5 frames per step
          fps = animation_fps,
          duration = animation_duration,
          width = 800,
          height = 800,
          renderer = gganimate::gifski_renderer()
        )
    
    },
    
    
    ## The output is a 3D array with:
    ##  The first dimension NK[a, :, :] is the number of an iteration (from 0 to n_landscapes)
    ##  The second dimension NK[:, b, :] is the location on a given landscape, where b ranges from 0 to 2^N
    ##  The third dimension NK[:, :, c] captures:
    ##   - the first N columns are for the combinations of N decision variables DV
    ##   - the second N columns are for the contribution values of each DV
    ##   - the next value (index=2*N) is for the total fit (avg of N contributions)
    ##   - the last one (index=2*N+1 is to find out whether it is the local peak (0 or 1)
    #' Compute the NK fitness landscape for all 2^N configurations.
    #'
    #' @description
    #' Enumerates (or samples) the full fitness landscape implied by the
    #' current influence matrix. The landscape has 2^N configurations,
    #' so computational cost is exponential in N. For N > 20 the full
    #' enumeration requires over one million configurations per landscape
    #' and becomes prohibitively slow; use the \code{sample_size} argument
    #' to obtain an MCMC-style random sample instead.
    #'
    #' @param n_landscapes Integer. Number of independent landscapes to
    #'   generate (default 30).
    #' @param component_coCovar Character or NULL. Name of a component
    #'   covariate stored in the object. If NULL, uniform random values
    #'   are used.
    #' @param normalize_int_mat Logical. Whether to normalize and binarize
    #'   the influence matrix (default TRUE).
    #' @param project_int_mat Logical. Use the projected search matrix
    #'   instead of the exogenous covariate matrix (default FALSE).
    #' @param component_value_sd Numeric. Standard deviation for Gaussian
    #'   noise around component covariate values (default 0.1).
    #' @param verbose Logical. Print summary statistics (default TRUE).
    #' @param max_N Integer. Safety cap on N for full enumeration
    #'   (default 20). If N exceeds this value and \code{sample_size} is
    #'   NULL, the method stops with an error. Set to \code{Inf} to
    #'   override at your own risk.
    #' @param sample_size Integer or NULL. When non-NULL, instead of
    #'   enumerating all 2^N configurations the method randomly samples
    #'   this many configurations per landscape. This provides an
    #'   approximate landscape that scales linearly in \code{sample_size}
    #'   rather than exponentially in N. Local-peak detection is
    #'   performed only among the sampled configurations.
    #'
    #' @return Invisibly, the 3D array stored in
    #'   \code{self$fitness_landscape}. Dimensions are
    #'   \code{[n_landscapes, n_configs, 2*N+2]}.
    compute_fitness_landscape = function(n_landscapes=30,
                                         component_coCovar=NULL, ## if not exists, then just use random
                                         normalize_int_mat=TRUE,
                                         project_int_mat=FALSE,
                                         component_value_sd=0.1,
                                         verbose=TRUE,
                                         max_N=20,
                                         sample_size=NULL) {
      # SYSTEM INPUT
      N <- self$N

      ## -- Scalability guard ------------------------------------------
      ## WARNING: Full enumeration creates 2^N rows per landscape.
      ##   N = 20 -->      1,048,576 rows  (feasible)
      ##   N = 25 -->     33,554,432 rows  (very slow)
      ##   N = 30 --> ~1,073,741,824 rows  (out of memory on most machines)
      ## Use the sample_size argument to avoid full enumeration for large N.
      if (N > 20 && is.null(sample_size)) {
        warning(
          sprintf(
            paste0("N = %d implies 2^N = %s configurations per landscape. ",
                   "Full enumeration is exponentially expensive. ",
                   "Consider setting sample_size (e.g., sample_size = 10000) ",
                   "for an MCMC-style approximation."),
            N, format(2^N, big.mark = ",")
          ),
          immediate. = TRUE
        )
      }
      if (N > max_N && is.null(sample_size)) {
        stop(
          sprintf(
            paste0("N = %d exceeds max_N = %d. Full enumeration of 2^%d = %s ",
                   "configurations is not feasible. Either:\n",
                   "  (1) set sample_size (e.g., sample_size = 10000) for a ",
                   "random-sample approximation, or\n",
                   "  (2) increase max_N (e.g., max_N = 25) if you have ",
                   "sufficient memory."),
            N, max_N, N, format(2^N, big.mark = ",")
          )
        )
      }
      ## --------------------------------------------------------------

      has_coDyadCovar <- !is.null(self$component_1_coDyadCovar) & all(dim(self$component_1_coDyadCovar) == self$N)
      
      ##**TODO - Use projected search matrix or exogenous covariate matrix? **
      ## use projected matrix
      if (project_int_mat || !has_coDyadCovar) {
        Int_matrix <- self$search_matrix
      } else if ( has_coDyadCovar ) {
        Int_matrix <- as.matrix( self$component_1_coDyadCovar )
      } else {
        stop('project_int_mat=FALSE but no component_x_coDyadCovar variable included in RSiena data.')
      }
      # Must remove negative values for computing
      Int_matrix[ Int_matrix < 0 ] <- 0
      if (normalize_int_mat) {
        Int_matrix <- sapply(1:nrow(Int_matrix), function(i){
          if(max(Int_matrix[i, ])==0) 
            return(Int_matrix[i, ])
          return( Int_matrix[i, ] / max(Int_matrix) )
        })
        ## make binary: the binary influence pattern (support of W), as in the classical NK influence matrix
        Int_matrix[ Int_matrix < 0.5 ] <- 0
        Int_matrix[ Int_matrix >=0.5 ] <- 1
      }
      powerkey <- function(N) {
        # Compute the powers of 2 for index location
        return(2^((N-1):0))
      }
      nkland <- function(N, component_cov=NULL, component_value_sd=0.1) {
        if(!self$exists(component_cov)) {
          mat <- matrix(runif(2^N * N), nrow=2^N, ncol=N)
          # ## Use covariate values
          return(mat)
        }
        ##**Use component covariates in fitness computation**
        vals <- self[[sprintf('component_%s_coCovar', component_coCovar)]]
        ## Gaussian noise centered at the component value ~ U(0,1), with sd=component_value_sd
        rnorm_mat <- sapply(1:N, function(i) rnorm(n = 2^N, mean = vals[i], sd=component_value_sd))
        return( rnorm_mat )
      }
      calc_fit <- function(N, NK_land, inter_m, Current_position, Power_key) {
        # Compute fitness contributions for each decision variable
        Fit_vector <- numeric(N)
        for (ad1 in 1:N) {
          Fit_vector[ad1] <- NK_land[sum(Current_position * inter_m[ad1, ] * Power_key) + 1, ad1]
        }
        return(Fit_vector)
      }
      comb_and_values <- function(N, NK_land, Power_key, inter_m) {
        # Compute fitness for all combinations
        Comb_and_value <- matrix(0, nrow=2^N, ncol=2*N+2)
        combinations <- expand.grid(replicate(N, 0:1, simplify = FALSE))
        
        for (c1 in 1:(2^N)) {
          Combination1 <- as.integer(combinations[c1,])
          fit_1 <- calc_fit(N, NK_land, inter_m, Combination1, Power_key)
          Comb_and_value[c1, 1:N] <- Combination1
          Comb_and_value[c1, (N+1):(2*N)] <- fit_1
          Comb_and_value[c1, (2*N+1)] <- mean(fit_1)
        }
        
        # Row index convention for Comb_and_value follows expand.grid(), in
        # which the FIRST variable varies fastest (bit 1 = least significant).
        # The MSB-first Power_key (2^((N-1):0)) used by calc_fit() to mask the
        # influence matrix does NOT match this row ordering, so neighbour
        # lookups must use the matching LSB-first key. Using Power_key here was
        # a bug: it retrieved the wrong row, corrupting the local-peak flags
        # (e.g. a K=0 additive landscape reported ~1000 peaks instead of 1).
        Grid_key <- rev(Power_key)  # 2^(0:(N-1)); row(b) = sum(b * Grid_key) + 1

        # Check for local peaks
        for (c3 in 1:(2^N)) {
          loc_p <- 1  # Assume it is a peak
          for (c4 in 1:N) {
            new_comb <- Comb_and_value[c3, 1:N]
            new_comb[c4] <- abs(new_comb[c4] - 1)
            index <- sum(new_comb * Grid_key) + 1
            if (Comb_and_value[c3, 2*N+1] < Comb_and_value[index, 2*N+1]) {
              loc_p <- 0  # Not a peak
            }
          }
          Comb_and_value[c3, 2*N+2] <- loc_p
        }
        
        return(Comb_and_value)
      }
      ##___________________________
      ## Sampled-configurations helper: evaluates a random subset of
      ## the 2^N landscape (MCMC-style approximation for large N).
      comb_and_values_sampled <- function(N, NK_land, Power_key, inter_m,
                                          sample_size) {
        Comb_and_value <- matrix(0, nrow = sample_size, ncol = 2 * N + 2)

        ## Draw sample_size random binary configurations
        sampled_configs <- matrix(
          sample(0:1, size = N * sample_size, replace = TRUE),
          nrow = sample_size, ncol = N
        )

        ## Compute fitness for each sampled configuration
        for (c1 in seq_len(sample_size)) {
          Combination1 <- as.integer(sampled_configs[c1, ])
          fit_1 <- calc_fit(N, NK_land, inter_m, Combination1, Power_key)
          Comb_and_value[c1, 1:N] <- Combination1
          Comb_and_value[c1, (N + 1):(2 * N)] <- fit_1
          Comb_and_value[c1, (2 * N + 1)] <- mean(fit_1)
        }

        ## Approximate local-peak detection among sampled configs only:
        ## for each sampled config, check its N Hamming-1 neighbours.
        ## A config is labeled a peak if no neighbour (computed on the
        ## fly from NK_land) has strictly higher fitness.
        for (c3 in seq_len(sample_size)) {
          loc_p <- 1L
          current_fit <- Comb_and_value[c3, 2 * N + 1]
          for (c4 in seq_len(N)) {
            new_comb <- Comb_and_value[c3, 1:N]
            new_comb[c4] <- abs(new_comb[c4] - 1)
            neighbour_fit <- mean(
              calc_fit(N, NK_land, inter_m, as.integer(new_comb), Power_key)
            )
            if (current_fit < neighbour_fit) {
              loc_p <- 0L
              break
            }
          }
          Comb_and_value[c3, 2 * N + 2] <- loc_p
        }

        return(Comb_and_value)
      }

      ##___________________________

      # *** GENERAL VARIABLES AND OBJECTS ***
      Power_key <- powerkey(N)

      use_sampling <- !is.null(sample_size)
      n_configs <- if (use_sampling) sample_size else 2^N

      NK <- array(0, dim = c(n_landscapes, n_configs, 2 * N + 2))

      start_time <- Sys.time()
      for (i_1 in 1:n_landscapes) {
        nkland_i1 <- nkland(N, component_coCovar, component_value_sd)
        if (use_sampling) {
          NK[i_1, , ] <- comb_and_values_sampled(
            N, nkland_i1, Power_key, Int_matrix, sample_size
          )
        } else {
          NK[i_1, , ] <- comb_and_values(N, nkland_i1, Power_key, Int_matrix)
        }
      }
      end_time <- Sys.time()
      run_time <-  end_time - start_time

      self$fitness_landscape <- NK

      npeaks_vec <- sapply(1:n_landscapes, function(i) sum(NK[i,,(2*N+2)]) )
      peaks <- psych::describe(npeaks_vec)
      if (verbose) {
        if (use_sampling) {
          cat(sprintf(
            "\n[Sampled mode] %s of 2^%d = %s configurations sampled per landscape.\n",
            format(sample_size, big.mark = ","), N, format(2^N, big.mark = ",")
          ))
        }
        cat("\nThe mean number of peaks per landscape is:", peaks$mean, "\n")
        cat("\nThe std. deviation of the number of peaks per landscape is:", peaks$sd, "\n")
        cat("\nThe skewness of the number of peaks per landscape is:", peaks$skew, "\n")
        cat("\nThe kurtosis of the number of peaks per landscape is:", peaks$kurtosis, "\n")
        cat("\nElapsed time:", round(run_time, 2), "sec\n")
      }

    },


    # =========================================================================
    # Formal SAOM-NK-Logit / RUM Mathematical Methods
    # (aligned with AMR Mathematical Appendix canonical framework)
    # =========================================================================

    #' Compute the NK power key index for epistasis masking
    #'
    #' @description
    #' Implements the index function \eqn{r_d(x) = \sum_{j=1}^{N} x_j E_{dj} PK_j + 1}
    #' where \eqn{PK = (2^{N-1}, 2^{N-2}, \ldots, 2^0)} is the power-key vector
    #' and \eqn{E} is the epistasis (interaction) matrix.
    #'
    #' @param b_i Binary vector of length N (actor's activity configuration).
    #' @param d   Integer, the focal dimension (1..N).
    #' @param E   Optional N x N influence matrix (binary influence pattern).
    #'   Defaults to the stored influence matrix (\code{component_1_coDyadCovar})
    #'   or identity.
    #' @return Integer row index (1-based) into the NK fitness table.
    power_key_index = function(b_i, d, E = NULL) {
      if (is.null(E)) {
        E <- if (!is.null(self$component_1_coDyadCovar) &&
                 all(dim(as.matrix(self$component_1_coDyadCovar)) == self$N)) {
          as.matrix(self$component_1_coDyadCovar)
        } else if (!is.null(self$search_matrix) &&
                   all(dim(as.matrix(self$search_matrix)) == self$N)) {
          as.matrix(self$search_matrix)
        } else {
          diag(self$N)
        }
      }
      PK <- 2^((self$N - 1):0)
      masked <- b_i * E[d, ]
      as.integer(sum(masked * PK) + 1)
    },


    #' Compute the full 10-component utility function from the AMR Mathematical Appendix
    #'
    #' @description
    #' Evaluates the canonical utility decomposition:
    #' \deqn{
    #'   U_i(B_i) = \beta_F F_i(B_i) - \beta_s (|B_i|/N)^2
    #'              + \beta_w (B_i' W B_i) / N^2
    #'              + \beta_h \bar{o}_i - \beta_{cong} \text{congestion}_i
    #'              - \beta_{disp} \text{displacement}_i
    #'              + \beta_{comp} \text{complementarity}_i
    #'              + \beta_{clos} \text{closure}_i
    #'              + \beta_{riv} \text{rivalry}_i
    #'              + \beta_{leg} \text{legitimacy}_i
    #' }
    #'
    #' @param actor_id  Integer vector of actor IDs (1-indexed), or NULL for all.
    #' @param beta_F    Weight on NK fitness component.
    #' @param beta_s    Weight on scope-cost (quadratic).
    #' @param beta_w    Weight on synergy (bilinear form through W).
    #' @param beta_h    Weight on herding (mean overlap with others).
    #' @param beta_cong Weight on congestion penalty.
    #' @param beta_disp Weight on displacement penalty.
    #' @param beta_comp Weight on complementarity bonus.
    #' @param beta_clos Weight on triadic closure in social projection.
    #' @param beta_riv  Weight on rivalry (inverse overlap with neighbors).
    #' @param beta_leg  Weight on legitimacy (Hill-function adoption signal).
    #' @param h_hill    Hill coefficient for legitimacy (steepness).
    #' @param K_half    Half-saturation constant for legitimacy Hill function.
    #' @param step      Integer time step (index into bi_env_arr), or NULL for
    #'   the current bipartite matrix.
    #' @param landscape_id Integer, which landscape replicate to use from
    #'   \code{fitness_landscape} (default 1).
    #' @return A \code{data.frame} with one row per actor and columns for each
    #'   utility component plus the total.
    compute_formal_utility = function(actor_id = NULL,
                                      beta_F = 1, beta_s = 0.5, beta_w = 0.3,
                                      beta_h = 0, beta_cong = 0, beta_disp = 0,
                                      beta_comp = 0, beta_clos = 0, beta_riv = 0,
                                      beta_leg = 0, h_hill = 2, K_half = 0.4,
                                      step = NULL, landscape_id = 1) {
      # -- Retrieve bipartite matrix at the requested step --
      if (is.null(step)) {
        bi_mat <- self$bipartite_matrix
      } else {
        bi_mat <- self$bi_env_arr[,,step]
      }

      # -- Influence matrix W --
      W <- if (!is.null(self$component_1_coDyadCovar) &&
               all(dim(as.matrix(self$component_1_coDyadCovar)) == self$N)) {
        as.matrix(self$component_1_coDyadCovar)
      } else if (!is.null(self$search_matrix) &&
                 all(dim(as.matrix(self$search_matrix)) == self$N)) {
        as.matrix(self$search_matrix)
      } else {
        diag(self$N)
      }

      # -- Reconstruct the raw NK_land table (2^N x N) from fitness_landscape --
      #    fitness_landscape[landscape, config, cols]:
      #      cols 1:N          = binary config
      #      cols (N+1):(2*N)  = per-dimension fitness contributions
      has_nk <- !is.null(self$fitness_landscape)
      if (has_nk) {
        NK_land_raw <- self$fitness_landscape[landscape_id, , (self$N + 1):(2 * self$N)]
        # NK_land_raw is a matrix: rows = configs (2^N or sample), cols = N dims
      }

      actors <- if (is.null(actor_id)) 1:self$M else actor_id
      n_actors <- length(actors)

      # Pre-allocate result
      utilities <- data.frame(
        actor_id        = actors,
        nk_fitness      = numeric(n_actors),
        scope_cost      = numeric(n_actors),
        synergy         = numeric(n_actors),
        herding         = numeric(n_actors),
        congestion      = numeric(n_actors),
        displacement    = numeric(n_actors),
        complementarity = numeric(n_actors),
        closure         = numeric(n_actors),
        rivalry         = numeric(n_actors),
        legitimacy      = numeric(n_actors),
        total           = numeric(n_actors),
        stringsAsFactors = FALSE
      )

      # Activity popularity vector (used in several components)
      n_j <- colSums(bi_mat)

      # Social projection (actor-by-actor co-membership matrix)
      S <- bi_mat %*% t(bi_mat)
      diag(S) <- 0

      for (idx in seq_along(actors)) {
        i <- actors[idx]
        b_i <- bi_mat[i, ]
        H_i <- which(b_i == 1)
        scope <- sum(b_i)

        # 1. NK Fitness
        nk_fit <- if (has_nk) {
          ## NK_land_raw is rows = configurations, cols = the N per-dimension
          ## fitness contributions of the configuration on that row, so it is
          ## indexed by configuration, not by a per-dimension power key. The
          ## power_key_index() as a row index and clamped it with min(), which
          ## silently returned another configuration's fitness rather than failing:
          ## for N = 4, K = 1 and b_i = (0,1,0,0) it returned 0.4879, the value
          ## stored for the EMPTY portfolio, where the landscape stores 0.2825.
          ## Rows enumerate configurations least-significant-bit first, so the row
          ## is computable directly for a full enumeration; a sampled landscape is
          ## matched instead, and reports NA rather than a neighbouring row's value.
          row_idx <- if (nrow(NK_land_raw) == 2^self$N) {
            1L + sum(as.integer(b_i) * 2L^(seq_len(self$N) - 1L))
          } else {
            cfg <- self$fitness_landscape[landscape_id, , seq_len(self$N)]
            hit <- which(apply(cfg, 1L, function(r) all(r == b_i)))
            if (length(hit)) hit[1L] else NA_integer_
          }
          if (is.na(row_idx)) NA_real_
          else mean(NK_land_raw[row_idx, ])
        } else 0

        # 2. Scope cost  (|B_i| / N)^2
        scope_cost <- (scope / self$N)^2

        # 3. Synergy  b_i' W b_i / N^2
        synergy <- if (length(H_i) >= 2) {
          as.numeric(t(b_i) %*% W %*% b_i) / self$N^2
        } else 0

        # 4. Herding  mean overlap with other actors / N
        overlaps <- apply(bi_mat[-i, , drop = FALSE], 1, function(b_j) sum(b_i & b_j))
        ## With a single actor there is nobody to overlap with, so herding is 0,
        ## not NaN. mean(numeric(0)) is NaN, and because $total adds
        ## beta_h * herding, one NaN made EVERY M = 1 utility call return NaN,
        ## which in turn made compute_choice_probabilities() return all-NaN.
        herding <- if (length(overlaps)) mean(overlaps) / self$N else 0

        # 5. Congestion  sum_j b_{ij} n_j / (M * N)
        congestion <- sum(b_i * n_j) / (self$M * self$N)

        # 6. Displacement  sum_j b_{ij} (n_j / M)^2 / N
        displacement <- sum(b_i * (n_j / self$M)^2) / self$N

        # 7. Complementarity (avg pairwise W among held activities)
        comp <- if (length(H_i) >= 2) {
          sum(W[H_i, H_i][upper.tri(W[H_i, H_i])]) / choose(length(H_i), 2)
        } else 0

        # 8. Closure (triadic closure in social projection)
        neighbors <- which(S[i, ] > 0)
        closure_val <- if (length(neighbors) >= 2) {
          sub_S <- S[neighbors, neighbors]
          sum(sub_S[upper.tri(sub_S)] > 0) / choose(length(neighbors), 2)
        } else 0

        # 9. Rivalry (negative mean co-membership with direct neighbors)
        rivalry_val <- if (length(neighbors) > 0) {
          -mean(S[i, neighbors]) / self$N
        } else 0

        # 10. Legitimacy (Hill function of adoption popularity)
        legitimacy_val <- if (scope > 0) {
          mean(b_i * n_j^h_hill / (K_half^h_hill * self$M^h_hill + n_j^h_hill))
        } else 0

        # -- Assemble weighted components --
        utilities$nk_fitness[idx]      <- beta_F    * nk_fit
        utilities$scope_cost[idx]      <- -beta_s   * scope_cost
        utilities$synergy[idx]         <- beta_w    * synergy
        utilities$herding[idx]         <- beta_h    * herding
        utilities$congestion[idx]      <- -beta_cong * congestion
        utilities$displacement[idx]    <- -beta_disp * displacement
        utilities$complementarity[idx] <- beta_comp * comp
        utilities$closure[idx]         <- beta_clos * closure_val
        utilities$rivalry[idx]         <- beta_riv  * rivalry_val
        utilities$legitimacy[idx]      <- beta_leg  * legitimacy_val
        utilities$total[idx]           <- sum(utilities[idx, 2:11])
      }

      return(utilities)
    },


    #' Compute McFadden conditional logit choice probabilities for each actor
    #'
    #' @description
    #' For each actor \eqn{i}, evaluates the probability of flipping each
    #' activity \eqn{j \in \{1, \ldots, N\}} versus maintaining the status quo,
    #' following the Random Utility Model (RUM) / conditional logit:
    #' \deqn{
    #'   P(\text{flip } j \mid i) = \frac{\exp(\beta \, \Delta U_{ij})}
    #'         {1 + \sum_{k=1}^{N} \exp(\beta \, \Delta U_{ik})}
    #' }
    #' where \eqn{\Delta U_{ij}} is the change in formal utility from toggling
    #' activity \eqn{j}.
    #'
    #' @param beta  Logit sensitivity / inverse temperature parameter.
    #' @param step  Integer time step, or NULL for current bipartite matrix.
    #' @param ...   Additional arguments passed to \code{compute_formal_utility}.
    #' @return A list of length M, each element containing
    #'   \code{actor_id}, \code{delta_u} (N-vector of utility changes),
    #'   \code{probabilities} (named vector of N flip probabilities plus
    #'   the pass/status-quo probability), and \code{beta}.
    compute_choice_probabilities = function(beta = 1, step = NULL, ...) {
      if (is.null(step)) {
        bi_mat <- self$bipartite_matrix
      } else {
        bi_mat <- self$bi_env_arr[,,step]
      }

      result <- vector("list", self$M)
      for (i in 1:self$M) {
        b_i <- bi_mat[i, ]
        u_current <- self$compute_formal_utility(actor_id = i, step = step, ...)$total

        delta_u <- numeric(self$N)
        for (j in 1:self$N) {
          # Create counterfactual: flip activity j
          b_flipped <- b_i
          b_flipped[j] <- 1 - b_flipped[j]

          # Temporarily install the flipped row to compute utility
          bi_mat_saved <- bi_mat[i, ]
          bi_mat[i, ] <- b_flipped
          # Write into the object temporarily for correct social-projection
          if (is.null(step)) {
            self$bipartite_matrix[i, ] <- b_flipped
          } else {
            self$bi_env_arr[i, , step] <- b_flipped
          }

          u_flipped <- self$compute_formal_utility(actor_id = i, step = step, ...)$total
          delta_u[j] <- u_flipped - u_current

          # Restore original
          bi_mat[i, ] <- bi_mat_saved
          if (is.null(step)) {
            self$bipartite_matrix[i, ] <- bi_mat_saved
          } else {
            self$bi_env_arr[i, , step] <- bi_mat_saved
          }
        }

        # Logit probabilities (status quo gets exp(0) = 1 in denominator)
        exp_vals <- exp(beta * delta_u)
        denom <- 1 + sum(exp_vals)
        probs <- c(exp_vals / denom, 1 / denom)
        names(probs) <- c(paste0("flip_", 1:self$N), "pass")

        result[[i]] <- list(
          actor_id     = i,
          delta_u      = delta_u,
          probabilities = probs,
          beta         = beta
        )
      }
      return(result)
    },


    #' Verify Theorem 1 (Reduction): SaoMNK with M=1, theta=0, beta->inf recovers NK
    #'
    #' @description
    #' Exhaustively enumerates all \eqn{2^N} binary configurations and compares
    #' the NK fitness (from the stored landscape) to the single-actor SaoMNK
    #' utility.  Under the reduction conditions (one actor, no social/strategic
    #' effects), these must be identical up to floating-point tolerance.
    #'
    #' @param max_N  Safety guard: refuse to run if \code{N > max_N} (default 12).
    #' @param landscape_id Which landscape replicate to use (default 1).
    #' @return A \code{data.frame} with columns \code{config_id}, \code{nk_fitness},
    #'   \code{saomnk_utility}, and \code{difference}.  Also prints a summary.
    verify_nk_equivalence = function(max_N = 12, landscape_id = 1) {
      if (self$N > max_N) {
        stop(sprintf("N=%d too large (max %d for exhaustive verification)", self$N, max_N))
      }
      if (is.null(self$fitness_landscape)) {
        stop("NK landscape not computed.  Run compute_fitness_landscape() first.")
      }

      N <- self$N
      n_configs <- 2^N
      PK <- 2^((N - 1):0)

      # Influence matrix (binary influence pattern E)
      E <- if (!is.null(self$component_1_coDyadCovar) &&
               all(dim(as.matrix(self$component_1_coDyadCovar)) == N)) {
        as.matrix(self$component_1_coDyadCovar)
      } else if (!is.null(self$search_matrix) &&
                 all(dim(as.matrix(self$search_matrix)) == N)) {
        as.matrix(self$search_matrix)
      } else {
        diag(N)
      }

      # Stored landscape, rows in expand.grid() order (row r holds the config
      # whose bits b satisfy sum(b * 2^(0:(N-1))) == r - 1):
      #   cols 1:N        = configuration bits
      #   cols (N+1):(2N) = per-dimension NK fitness contributions f_d
      #   col  2N+1       = mean_d f_d = the configuration's NK fitness (ground
      #                     truth, produced by compute_fitness_landscape()).
      bits     <- matrix(self$fitness_landscape[landscape_id, , 1:N,
                                                drop = FALSE], n_configs, N)
      contrib  <- matrix(self$fitness_landscape[landscape_id, , (N + 1):(2 * N),
                                                drop = FALSE], n_configs, N)
      f_stored <- self$fitness_landscape[landscape_id, , 2 * N + 1]

      # --- Independent NK reconstruction -----------------------------------
      # The defining NK property is that contribution f_d depends ONLY on the
      # epistatic loci of d. calc_fit() encodes those loci through the masked
      # code  code_d(b) = sum(b * E[d,] * PK), so any two configurations with
      # the same code MUST carry the same f_d. We rebuild each dimension's
      # payoff table keyed by that code (value taken from the FIRST config seen
      # for each code) and recompute fitness as the mean of these look-ups,
      # then compare to the engine's stored fitness. If masking or row-indexing
      # were inconsistent (e.g. an endianness bug), configs sharing a code would
      # carry different contributions and the difference would become non-zero.
      # With theta = 0 and no social terms the single-actor SaoMNK utility
      # equals this NK fitness exactly -- there is NO multiplication by x[d]
      # (averaging the x[d] factor in was a bug that made the check meaningless).
      nk_indep <- numeric(n_configs)
      for (d in 1:N) {
        codes <- as.vector(bits %*% (E[d, ] * PK))  # code_d(b) for every config
        first <- match(codes, codes)                # first config index per code
        nk_indep <- nk_indep + contrib[cbind(first, d)]
      }
      nk_indep <- nk_indep / N

      results <- data.frame(
        config_id      = 1:n_configs,
        nk_fitness     = f_stored,   # engine-stored NK fitness (ground truth)
        saomnk_utility = nk_indep,   # independent M=1, theta=0 reduction
        difference     = abs(nk_indep - f_stored)
      )

      cat(sprintf("NK-SaoMNK equivalence check: N=%d, max difference = %.2e\n",
                  N, max(results$difference)))
      return(results)
    },


    process_fitness_landscape = function(actor_ids=c(), step_ids=1:6) {
      
      if (is.null(self$theta_matrix)) 
        stop('missing self$theta_matrix')
      
      if (is.null(self$fitness_landscape)) 
        self$compute_fitness_landscape(
          n_landscapes=30, 
          component_coCovar=NULL, ## if not exists, then just use random
          normalize_int_mat=TRUE,
          project_int_mat=FALSE,
          component_value_sd=0.1,
          verbose=TRUE
        )
      
      theta_matrix <- self$theta_matrix
      
      pltlist <- list()
      statsl <- list()
      utilist <- list()
      
      if(length(actor_ids)==0)
        actor_ids <- 1:self$M  ## default to all actors
      
      ## step in decision chain loop
      for (loop_id_step in 1:length(step_ids)) {
        
        step_id <- step_ids[ loop_id_step ]
        
        cat(sprintf('\nstep %s: actors ', step_id))
        
        bi_env_mat_step <- self$bi_env_arr[, , step_id]
        
        act_counterfacts <- list()
        
        
        for (loop_id_actor in 1:length(actor_ids)) {
          
          actor_id <- actor_ids[ loop_id_actor ]
          
          
          cat(sprintf(' %s ', actor_id))
          
          
          ##**ACTOR i DECISION PERSPECTIVE** 0000000000000000000000000000000000
          
          ##**TODO**
          ## ALL actor-component counterfactual configuations for Actor i  (2^N rows by N cols)
          iland <- expand.grid(lapply(1:self$N, function(x) 0:1 ))
          iland_config_step_row_id <- which(apply(iland, 1, function(x) all(x == bi_env_mat_step[actor_id,]) ))
          
          ## fitness
          fitness_land1 <- self$fitness_landscape[1, , 1:self$N]
          if ( ! all( iland == fitness_land1) ) {
            stop('fitness landscape configurations do not match iland configurations')
          }
          
          
          ##**TODO**
          ##**TODO** COMBINE utility and 'fitness' measures in dataframe by configuration with distance from actor's current configuration
          ##*##**TODO**
          ##**TODO**
          ##**TODO  CHANGE TO ACTOR-SPECIFIC LANDSCAPES **
          tmpmat <- bi_env_mat_step
          ## ifit dimensions [ M, 2^N ]
          ifit <- apply(iland, 1, function(x){
            tmpmat[actor_id,] <- x  ## set counterfactual actor-component configuration
            self$get_struct_mod_stats_mat_from_bi_mat( tmpmat ) %*% theta_matrix[step_id,] ##variable thetas
          }) 
          
          
          ids.max <- which(ifit == max(ifit), arr.ind = TRUE)
          fits.max <- ifit[ ids.max ]
          nmax <- length(fits.max)
          
          # ifit
          # ids.max[1,]
          
          act_counterfacts[[as.character(actor_id)]] <- list(
            ifit = ifit, #Given all other ties, Actor i's configurations applied to utility func for all actors
            ids.max = ids.max,
            fits.max = fits.max,
            nmax = nmax
          )
          
          ## distances of each counterfactual configuration 
          dist_counterfac <- iland - bi_env_mat_step[actor_id, ]
          z <- sapply( 1:nrow(dist_counterfac), function(id_i) {
            x <- dist_counterfac[id_i,]
            fit <- fitness_land1[id_i, ncol(fitness_land1)-1 ]
            c(dist=sum(x!=0),
              drop=sum(x==-1), 
              nochange=sum(x==0),
              add=sum(x==1), 
              counterfac=paste(x, collapse = '|'), 
              start=paste(bi_env_mat_step[actor_id, ], collapse = '|'),
              fitness=fit
            )
          })
          
          ##**TODO**
          ##**INFORMATION LOSS**
          ##  This uses only ego's own counterfactual fits (landscape)
          ##  but the corresponding other actor's affected fits (landscapes are not currently used )
          z <- rbind(z, utility_ego=ifit[ actor_id , ] )
          z <- rbind(z, utility_alter_mean= colMeans(ifit[ -actor_id , ], na.rm = TRUE) )
          z <- rbind(z, utility_alter_sd  = apply(ifit[ -actor_id , ], 2, function(x) sd(x, na.rm = TRUE)) )
          
          wdf <- as.data.frame( t(z) )
          
          actfit_long <- wdf %>% 
            mutate(
              dist = as.numeric(dist),
              drop = as.numeric(drop),
              nochange = as.numeric(nochange),
              add = as.numeric(add) ,
              utility_ego=as.numeric(utility_ego),
              utility_alter_mean = as.numeric(utility_alter_mean),
              utility_alter_sd = as.numeric(utility_alter_sd)
            ) %>%
            pivot_longer(cols = c( drop:add, utility_ego:utility_alter_sd )) 
          
          plt <- actfit_long %>% ggplot(aes(x=value))+ # geom_density() + 
            geom_histogram() +
            facet_grid(dist ~ name, scales='free_x') + theme_bw() + 
            ggtitle(sprintf('Utility Transition Paths: Actor %s, Step %s', actor_id, step_id))
          
          pltlist[[ length(pltlist)+1 ]] <- plt
          
          step_actor_key <- sprintf('%s|%s',step_id, actor_id )
          
          statsl[[step_actor_key]] <- list(
            actfit_long = actfit_long,
            act_counterfacts = act_counterfacts,
            plt = plt,
            actor_id=actor_id, 
            chain_step_id = step_id,
            strategy=ifelse(is.null(self$strat_1_coCovar), NA, as.factor(self$strat_1_coCovar[actor_id]))
          )
          
          utilist[[step_actor_key]] <- actfit_long %>% 
            filter(name %in% c('utility_ego','utility_alter_mean')) %>%
            mutate(actor_id=actor_id, chain_step_id = step_id, 
                   strategy=ifelse(is.null(self$strat_1_coCovar),NA,as.factor(self$get_actor_strategies()[actor_id])))
          
          
        }##/end actor loop in step
      
        
      } ##/end step loop of the decision chain
      
      ##plot
      plt_grid <- data.table::rbindlist(utilist, idcol = 'step_actor_key') %>% 
        mutate(chain_step_id = paste('step',chain_step_id,sep=''), actor_id = paste('A',actor_id,sep='')) %>%
        ggplot(aes(x=value, fill=factor(dist), color=factor(dist))) + 
        geom_density(alpha=.3, size=1) + 
        facet_grid(chain_step_id ~ actor_id, scales = 'free') +
        ggtitle('Fitness Values by Distance (# tie changes) from Current Configuration') +
        theme_bw()
      
      return(list(
        utilist = utilist,
        statsl = statsl,
        pltlist = pltlist,
        plt_step_actor = plt_grid
      ))
      
    },
    
    
    # RSIENA
    search_rsiena_init = function(structure_model, get_eff_doc = FALSE) {
      ##--1. RSiena Model
      ##  1.1. INIT: bipartite matrix --> RSiena model
      self$init_rsiena_model_from_structure_model_bipartite_matrix(self$bipartite_matrix, structure_model, self$rsiena_env_seed)
      
      print('self$rsiena_data : ')
      print(self$rsiena_data)
      
      ##  1.2. INIT effects list in simulation model (in RSiena model)
      self$rsiena_effects <- getEffects(self$rsiena_data)
      
      # Effects Documentation
      if(get_eff_doc)
        effectsDocumentation(self$rsiena_effects)
      ##-----------------------------
      
      ##--2. NETWORK: STRUCTURE EVOLUTION (structure_Model)-----
      ##  2.1. Add effects from model objective function list
      self$add_rsiena_effects(structure_model)
      
      # ##--3. SEARCH (FITNESS): PAYOFFS (payoff_formulas) ----------
      # ##  3.1. set payoff rules (formulas)
    },
    
    
    search_rsiena_execute_sim = function(iterations,
                                         returnDeps=TRUE,
                                         returnChains=TRUE,
                                         rsiena_phase2_nsub=1, rsiena_n2start_scale=1,
                                         digits=3,
                                         seed=123) {
      if (is.null(self$rsiena_effects))
        stop('Set rsiena_effects before running simulation.')


      ##-----------------------------
      ## RSiena Algorithm
      self$rsiena_algorithm <- sienaAlgorithmCreate(projname=file.path(self$DIR_OUTPUT,
                                                              sprintf('%s_%s',self$SIM_NAME,self$TIMESTAMP)),
                                                    simOnly = TRUE,
                                                    nsub = rsiena_phase2_nsub,
                                                    n2start = rsiena_n2start_scale * 2.52 * (7+sum(self$rsiena_effects$include)),
                                                    n3 = iterations,
                                                    seed = seed)


      # Run RSiena simulation
      self$rsiena_model <- siena07(self$rsiena_algorithm,
                                   data = self$rsiena_data,
                                   effects = self$rsiena_effects,
                                   batch = TRUE,
                                   returnDeps = returnDeps,
                                   returnChains = returnChains,
                                   returnDataFrame = TRUE, ##**TODO** CHECK
                                   returnLoglik = TRUE #,  ##**TODO** CHECK
                                   )   # returnChains = returnChains

      # Summarize and plot results
      mod_summary <- summary(self$rsiena_model)
      if(!is.null(mod_summary))
        print(mod_summary)


      print(screenreg(list(self$rsiena_model), single.row = TRUE, digits = digits))

      ## update simulation object environment from RSiena simulation model
      new_bi_env_igraph <- self$get_bipartite_igraph_from_rsiena_model()
      self$set_system_from_bipartite_igraph( new_bi_env_igraph )
      sim_iterations <- self$rsiena_model$n3 ##**TODO** CHECK
      self$plot_bipartite_system_from_mat(self$bipartite_matrix, sim_iterations, 
                                          plot_save = FALSE, return_plot=TRUE)

    },
    
    
    ## Single Simulation Run
    search_rsiena_run = function(structure_model,
                                 iterations=1000,
                                 returnDeps=TRUE, returnChains=TRUE, ## TRUE=simulation only
                                 plot_save = TRUE,
                                 rsiena_phase2_nsub=1, rsiena_n2start_scale=1,
                                 get_eff_doc = FALSE, digits=3,
                                 run_seed=123
                                 ) {
      set.seed(run_seed)
      self$rsiena_run_seed <- run_seed
      if( overwrite | is.null(self$rsiena_model) ) {
        ## 1. Init simulation
        self$search_rsiena_init(structure_model, get_eff_doc)
        ## 2. Execute simulation
        self$search_rsiena_execute_sim(iterations,
                                       returnDeps=returnDeps,
                                       returnChains=returnChains,
                                       rsiena_phase2_nsub=rsiena_phase2_nsub,
                                       rsiena_n2start_scale=rsiena_n2start_scale,
                                       digits=digits,
                                       rand_seed=run_seed)
        ## 3. Process chain of simulation ministeps
        self$search_rsiena_process_ministep_chain()
        ## 4. Process actor statistics (e.g., utility)
        self$search_rsiena_process_stats()
      } else {
        stop('_extend() method not yet implemented.')
      }
    },
    
    
    search_rsiena_multiwave_run = function(structure_model,
                                           waves=2,
                                           iterations=1000,
                                           returnDeps=TRUE,
                                           returnChains=TRUE,
                                           rsiena_phase2_nsub=1, rsiena_n2start_scale=1,
                                           digits=3,
                                           rand_seed=123,
                                           dir_output=NA,
                                           file_output=NA) {
      bipartite_matrix_0 <- self$bipartite_matrix
      ##--1. INIT RSiena Model: set $rsiena_data --------
      self$init_rsiena_model_from_structure_model_bipartite_matrix(structure_model, bipartite_matrix_0, self$rsiena_env_seed)
      print('self$rsiena_data : ')
      print(self$rsiena_data)
      ##  2. Init effects
      self$rsiena_effects <- getEffects(self$rsiena_data)
      ##  3. Add effects from structure_model list
      self$add_rsiena_effects(structure_model)
      ##  4. RSiena Algorithm 
      .projdir <- ifelse(is.na(dir_output), getwd(), dir_output)
      .projname <-  ifelse(!is.na(file_output), file_output, as.character(as.numeric(Sys.time())))
      self$rsiena_algorithm <- sienaAlgorithmCreate(projname=file.path( .projdir,  sprintf('%s.log', .projname) ), ## rsiena project log filename
                                                    simOnly = TRUE,
                                                    nsub = rsiena_phase2_nsub,
                                                    n3 = iterations,
                                                    seed = rand_seed)
      ## 5. Run RSiena simulation
      self$rsiena_model <- siena07(self$rsiena_algorithm,
                                   data = self$rsiena_data, 
                                   effects = self$rsiena_effects,
                                   batch = TRUE,
                                   returnDeps = returnDeps, 
                                   returnChains = returnChains,
                                   returnDataFrame = TRUE, ##**TODO** CHECK
                                   returnLoglik = TRUE #,  ##**TODO** CHECK
      )   # returnChains = returnChains
      
      # Summarize and plot results
      mod_summary <- summary(self$rsiena_model)
      if(!is.null(mod_summary))
        print(mod_summary)
      print(screenreg(list(self$rsiena_model), single.row = TRUE, digits = digits))
      
      # 6. Update System
      ## update simulation object environment from RSiena simulation model
      new_bi_env_igraph <- self$get_bipartite_igraph_from_rsiena_model()
      self$set_system_from_bipartite_igraph( new_bi_env_igraph )
      
      # sink() ## write output text to file
      for (w in 1:waves){
        bipartite_matrix_previous <- if(w == 1){ bipartite_matrix_0 }else{ self$bipartite_matrix_waves[[w-1]] }
        ##--1. INIT RSiena Model--------
        self$init_multiwave_rsiena_model_from_structure_model_bipartite_matrix(structure_model,
                                                                               bipartite_matrix_previous, ## matrix1 (previous)
                                                                               self$bipartite_matrix,     ## matrix2 (latest)
                                                                               self$rsiena_env_seed)
        print('self$rsiena_data : ')
        print(self$rsiena_data)
        ##  2. Init effects
        self$rsiena_effects <- getEffects(self$rsiena_data)
        ##  3. Add effects from model objective function list
        self$add_rsiena_effects(structure_model)
        ##  4. RSiena Algorithm
        self$rsiena_algorithm <- sienaAlgorithmCreate(projname=file.path(self$DIR_OUTPUT,
                                                              sprintf('%s_%s',self$SIM_NAME,self$TIMESTAMP)),
                                                      simOnly = TRUE,
                                                      nsub = rsiena_phase2_nsub,
                                                      n3 = iterations,
                                                      seed = rand_seed)
        ## 5. Run RSiena simulation
        self$rsiena_model <- siena07(self$rsiena_algorithm,
                                     data = self$rsiena_data, 
                                     effects = self$rsiena_effects,
                                     batch = TRUE,
                                     returnDeps = returnDeps, 
                                     returnChains = returnChains,
                                     returnDataFrame = TRUE, ##**TODO** CHECK
                                     returnLoglik = TRUE #,  ##**TODO** CHECK
        )   # returnChains = returnChains
        
        # Summarize and plot results
        mod_summary <- summary(self$rsiena_model)
        if(!is.null(mod_summary))
          print(mod_summary)
        print(screenreg(list(self$rsiena_model), single.row = TRUE, digits = digits))
        
        ## 6. Update System
        ## update simulation object environment from RSiena simulation model
        new_bi_env_igraph <- self$get_bipartite_igraph_from_rsiena_model()
        self$set_system_from_bipartite_igraph( new_bi_env_igraph )
        
        ##---------- 2. Waves 2,3,4,... in Multiwave Simulation -------------------
        
        self$rsiena_model_waves[[w]] <- self$rsiena_model
        self$bipartite_matrix_waves[[w]] <- self$bipartite_matrix
        
      }
      
    },


    ## ---- Parallel Monte Carlo replications --------------------------------
    ##
    ## Each replication starts from the same initial bipartite matrix and
    ## structure model but uses a different random seed. Results are collected
    ## into self$mc_results as a list. Requires {future} and {future.apply}.
    search_rsiena_monte_carlo = function(structure_model,
                                          replications = 10,
                                          waves = 2,
                                          iterations = 1000,
                                          rand_seed = 123,
                                          parallel = FALSE,
                                          workers = NULL) {
      init_matrix <- self$bipartite_matrix
      init_params <- list(
        M = self$M, N = self$N,
        BI_PROB = 0,
        rand_seed = self$rsiena_env_seed
      )
      run_one_rep <- function(rep_id) {
        env_rep <- SaomNkRSienaBiEnv$new(init_params)
        env_rep$bipartite_matrix <- init_matrix
        env_rep$search_rsiena_multiwave_run(
          structure_model,
          waves       = waves,
          iterations  = iterations,
          rand_seed   = rand_seed + rep_id
        )
        list(
          rep_id          = rep_id,
          seed            = rand_seed + rep_id,
          bipartite_final = env_rep$bipartite_matrix,
          bipartite_waves = env_rep$bipartite_matrix_waves,
          rsiena_model    = tryCatch(env_rep$rsiena_model, error = function(e) NULL)
        )
      }
      if (parallel && replications > 1) {
        if (!requireNamespace("future", quietly = TRUE) ||
            !requireNamespace("future.apply", quietly = TRUE)) {
          stop("Packages 'future' and 'future.apply' are required for parallel Monte Carlo.")
        }
        old_plan <- NULL
        if (!is.null(workers)) {
          old_plan <- future::plan()
          future::plan(future::multisession, workers = workers)
          on.exit(future::plan(old_plan), add = TRUE)
        }
        message(sprintf("Running %d Monte Carlo replications in parallel...", replications))
        self$mc_results <- future.apply::future_lapply(
          seq_len(replications), run_one_rep, future.seed = TRUE
        )
      } else {
        message(sprintf("Running %d Monte Carlo replications sequentially...", replications))
        self$mc_results <- lapply(seq_len(replications), function(rep_id) {
          if (rep_id %% max(1, replications %/% 10) == 0)
            message(sprintf("  Replication %d / %d", rep_id, replications))
          run_one_rep(rep_id)
        })
      }
      message("Monte Carlo replications complete.")
      invisible(self)
    },


    #**TODO**
    search_rsiena_multiwave_extend = function(waves=1,
                                               iterations=1000,
                                               returnDeps=TRUE,
                                               returnChains=TRUE,
                                               rsiena_phase2_nsub=1, rsiena_n2start_scale=1, 
                                               digits=3,
                                               rand_seed=123) {
      ##  4. RSiena Algorithm 
      self$rsiena_algorithm <- sienaAlgorithmCreate(projname=file.path(self$DIR_OUTPUT,
                                                              sprintf('%s_%s',self$SIM_NAME,self$TIMESTAMP)),
                                                    simOnly = TRUE,
                                                    nsub = rsiena_phase2_nsub,
                                                    n3 = iterations,
                                                    seed = rand_seed)
      ## 5. Run RSiena simulation
      self$rsiena_model <- siena07(self$rsiena_algorithm,
                                   data = self$rsiena_data, 
                                   effects = self$rsiena_effects,
                                   batch = TRUE,
                                   returnDeps = returnDeps, 
                                   returnChains = returnChains,
                                   returnDataFrame = TRUE, ##**TODO** CHECK
                                   returnLoglik = TRUE ,  ##**TODO** CHECK
                                   prevAns= self$rsiena_model
      )   # returnChains = returnChains
      
      # Summarize and plot results
      mod_summary <- summary(self$rsiena_model)
      if(!is.null(mod_summary))
        print(mod_summary)
      print(screenreg(list(self$rsiena_model), single.row = TRUE, digits = digits))
      
      # 6. Update System
      ## update simulation object environment from RSiena simulation model
      new_bi_env_igraph <- self$get_bipartite_igraph_from_rsiena_model()
      self$set_system_from_bipartite_igraph( new_bi_env_igraph )
      
      #                                returnDataFrame = TRUE, ##**TODO** CHECK
      #                                returnLoglik = TRUE #,  ##**TODO** CHECK
      
    },
    
    
    search_rsiena_multiwave_process_results = function(progress_callback = NULL) {
      actor_wave_stats <- list()
      actor_wave_util <- list()
      actor_wave_util_diff <- list()
      K_wave_A <- list()
      K_wave_B1 <- list()
      K_wave_B2 <- list()
      K_wave_C <- list()
      for (w in 1:length(self$rsiena_model_waves)) {
        rsiena_model_w <- self$rsiena_model_waves[[ w ]]
        bipartite_igraph_w <- self$get_bipartite_igraph_from_rsiena_model(rsiena_model_w)
        self$set_system_from_bipartite_igraph( bipartite_igraph_w )
        ## 3. Process chain of simulation ministeps
        self$search_rsiena_process_ministep_chain()
        ## 4. Process actor statistics (e.g., utility)
        self$search_rsiena_process_stats(progress_callback = progress_callback)
        ##**TODO**
        actor_wave_stats[[w]]     <- self$actor_stats_df %>% mutate(wave_id=w)
        actor_wave_util[[w]]      <- self$actor_util_df %>% mutate(wave_id=w)
        actor_wave_util_diff[[w]] <- self$actor_util_diff_df %>% mutate(wave_id=w)
        
        K_wave_A[[w]]  <- self$K_AA_df  %>% mutate(wave_id=w)
        K_wave_B1[[w]] <- self$K_AC_df %>% mutate(wave_id=w)
        K_wave_B2[[w]] <- self$K_CA_df %>% mutate(wave_id=w)
        K_wave_C[[w]]  <- self$K_CC_df  %>% mutate(wave_id=w)
      }
      
      ##**TODO**
      self$actor_wave_stats     <- data.table::rbindlist( actor_wave_stats )
      self$actor_wave_util      <- data.table::rbindlist( actor_wave_util )
      self$actor_wave_util_diff <- data.table::rbindlist( actor_wave_util_diff )
      
      # ##
      
      self$K_wave_A  <- data.table::rbindlist( K_wave_A ) 
      self$K_wave_B1 <- data.table::rbindlist( K_wave_B1 ) 
      self$K_wave_B2 <- data.table::rbindlist( K_wave_B2 ) 
      self$K_wave_C  <- data.table::rbindlist( K_wave_C ) 
    },
    
    
    get_bipartite_igraph_from_rsiena_model = function(rsiena_model = NULL, sim_iteration = NULL) {
      rsiena_model <- if(is.null(rsiena_model)) { self$rsiena_model } else { rsiena_model }
      sim_iteration <- ifelse( is.null(sim_iteration), rsiena_model$n3 , sim_iteration)
      new_bi_env_mat <- self$get_bipartite_matrix_from_rsiena_model(rsiena_model, sim_iteration)
      new_bi_env_igraph <- igraph::graph_from_biadjacency_matrix(new_bi_env_mat, directed = FALSE, weighted = TRUE, mode = 'out') ##**TODO: CHECK** all vs. out
      return(new_bi_env_igraph)
    },
    
    get_bipartite_matrix_from_rsiena_model = function(rsiena_model = NULL, sim_iteration = NULL, wave_id=1) {
      rsiena_model <- if(is.null(rsiena_model)) { self$rsiena_model } else { rsiena_model }
      ######### UPDATE SYSTEM ENVIRONMENT SNAPSHOT FROM EVOLVED Bipartite Network DV #####################
      if (! length(rsiena_model$sims) )
        stop('Run RSiena simulations to set rsiena_model$sims before get_bipartite_matrix_from_rsiena_model.')
      sim_id <- ifelse(is.null(sim_iteration), 
                       length(rsiena_model$chain), ## default current state is the last simulation in sims list
                       sim_iteration)
      bi_env_dv_id <- which( names(self$config_structure_model) == 'dv_bipartite' )
      MplusN <- self$M + self$N
      ## Get DV (bi-partite network) from simulation iteration=sim_id
      el_bi_env <- rsiena_model$sims[[ sim_id ]][[ wave_id ]][[ bi_env_dv_id ]]$`1`
      ## update numbering of second mode (the N component integer names shift upward by the number of actors M to match bipartite naming)
      el_bi_env[,2] <- el_bi_env[,2] + self$M
      ### get networks from other prjected space ties 
      #############
      bi_env_mat_sp <- sparseMatrix(i = el_bi_env[,1],
                                    j = el_bi_env[,2],
                                    x = el_bi_env[,3],
                                    dims = c(MplusN, MplusN))
      ## Undirected network --> only uses 'Upper right' rectangle of full bipartite matrix
      ##   M N
      new_bi_env_mat <- as.matrix(bi_env_mat_sp)[ 1:self$M, (self$M+1):(MplusN) ]
      return( new_bi_env_mat )
    },
    
    get_chain_stats_list = function(progress_callback = NULL) { ## 'all','statdf','bi_env_arr', 'util', 'util_diff')
      if ( is.null(self$rsiena_model) )
        stop('rsiena_model missing; run simulation to compute actor utility')
      if ( is.null(self$chain_stats) )
        stop('chain_stats missing; run simulation with returnChains=TRUE in order to compute actor utility')
      interaction_effnames <- sapply(self$config_structure_model$dv_bipartite$interactions, function(x) x$effect)
      bi_env_mat <- self$bipartite_matrix_init
      new_components <- which(colSums(self$bipartite_matrix_init) == 0)
      old_components <- which(colSums(self$bipartite_matrix_init) >  0)
      
      ## remove chain entries where no change was made (keep if !stability )
      ## Keep all steps (including no-change for forbearance measures)
      tiechdf <- self$chain_stats
      
      ## get matrix timeseries and network statistics timeseries
      nchains <- nrow(tiechdf)

      ## Progress callback reporting interval (~20 updates over full loop)
      report_interval <- max(1L, floor(nchains / 20))

      ## paramters
      ## The columns of rsiena_model$thetaUsed correspond one-for-one with the
      ## rows of the theta data frame built at the SAME width get_theta_matrix()
      ## used -- which includes basic rates when RSiena is estimating
      ## unconditionally (two or more dependent variables). Build at that width,
      ## then keep only the BIPARTITE network's non-rate effects.
      ##
      ## The utility decomposition below attributes each ministep to per-actor
      ## contributions of the bipartite evaluation function. A behavior DV's
      ## effects (`linear`, `quad`, `avInSimDist2`, ...) are not statistics of
      ## the bipartite matrix and have no such decomposition here, so they are
      ## dropped rather than fabricated. Behavior trajectories are recovered
      ## from the chain and the simulated behavior arrays instead. For a
      ## single-DV model this keeps every column it kept before.
      .theta_df_all <- self$get_rsiena_effects_theta_df(
        no_rates = !(self$get_n_rsiena_depvars() > 1L))
      .net_cols <- which(.theta_df_all$name == 'self$bipartite_rsienaDV' &
                           !(.theta_df_all$shortName == 'Rate' & .theta_df_all$type == 'rate'))
      theta_df_norates <- self$get_bipartite_effects_theta_df()
      theta_names_norate <-  theta_df_norates$shortName
      theta_levels_norates <- theta_df_norates$effect_level
      if(is.null(self$rsiena_model$thetaUsed)){
        ## Get theta in the order of rsiena_effect object.
        ## Theta-storage convention (2026-08-23): the coefficient lives in
        ## `initialValue`, never in `parm` (RSiena's internal '#' parameter).
        theta <- theta_df_norates$initialValue
        names(theta) <- theta_df_norates$shortName
        ## simOnly mode: theta is constant across all ministeps -- replicate for nchains rows
        theta_mat <- matrix(rep(theta, nchains), byrow = TRUE, nrow = nchains)
      }else{
        ## thetaUsed has n3 rows (one per simulation run) -- expand to nchains rows
        ## by mapping each ministep to its simulation run
        theta_used <- self$rsiena_model$thetaUsed
        ## Keep only the bipartite non-rate columns (identity for single-DV models)
        if (ncol(theta_used) == nrow(.theta_df_all))
          theta_used <- theta_used[, .net_cols, drop = FALSE]
        if (nrow(theta_used) == nchains) {
          theta_mat <- theta_used
        } else {
          ## Replicate each run's theta for all ministeps in that run
          simChain <- self$rsiena_model$chain
          run_lengths <- sapply(seq_along(simChain), function(iter) {
            length(simChain[[iter]][[1]][[1]])
          })
          run_ids <- rep(seq_along(run_lengths), run_lengths)
          theta_mat <- theta_used[run_ids, , drop = FALSE]
        }
      }
      colnames(theta_mat) <- theta_names_norate
      ntheta <-  dim(theta_mat)[2]

      if (nrow(theta_mat) != nchains) {
        warning(sprintf('theta_mat rows (%d) != chain rows (%d). Truncating/padding theta_mat.', nrow(theta_mat), nchains))
        if (nrow(theta_mat) > nchains) {
          theta_mat <- theta_mat[1:nchains, , drop = FALSE]
        } else {
          ## Pad by repeating last row
          n_pad <- nchains - nrow(theta_mat)
          theta_mat <- rbind(theta_mat, theta_mat[rep(nrow(theta_mat), n_pad), , drop = FALSE])
        }
      }
      
      bi_env_arr <- array(NA, dim=c(self$M, self$N, nchains))
      ## Diff-based storage (Task 4 memory optimization)
      bi_env_changes <- matrix(NA_integer_, nrow = nchains, ncol = 3)
      colnames(bi_env_changes) <- c("step", "actor_i", "comp_j")
      stats_li <- list()
      util_li  <- list()
      util_diff_li <- list()
      K_AA_li <- list()
      K_AC_li <- list()
      K_CA_li <- list()
      K_CC_li <- list()
      K_AA_NEW_li <- list()
      K_AC_NEW_li <- list()
      K_AA_OLD_li <- list()
      K_AC_OLD_li <- list()
      K_CA_NEW_li <- list()
      K_CC_NEW_li <- list()
      K_CA_OLD_li <- list()
      K_CC_OLD_li <- list()

      ## ========================================================================
      ## RANK-1 INCREMENTAL UPDATE OPTIMIZATION
      ## ========================================================================
      ## Instead of recomputing O(M*N*M) and O(M*N*N) matrix products at every
      ## chain step, we maintain projection matrices incrementally. Since only ONE
      ## dyad (actor_i, comp_j) changes per ministep, we apply rank-1 updates:
      ##
      ##   Social projection  S = B %*% t(B):  S_new = S_old + delta*(e_i * b_j^T + b_j * e_i^T)
      ##   Epistasis projection E = t(B) %*% B: E_new = E_old + delta*(e_j * b_i^T + b_i * e_j^T)
      ##
      ## where delta = +1 (tie creation) or -1 (tie dissolution), b_j = B[,j]
      ## is the j-th column of B (AFTER the toggle), and b_i = B[i,] is the i-th
      ## row of B (AFTER the toggle). This reduces per-step cost from O(M^2*N) to
      ## O(M) for social and O(N) for epistasis projections.
      ##
      ## Row/column sums are also maintained incrementally: O(1) per step instead
      ## of O(M*N).
      ##
      ## K-stat degree counts (number of positive entries per row/col of projection)
      ## are maintained via sign-counting: when a projection entry crosses zero
      ## (from 0 to positive, or from positive to 0), the degree count is adjusted.
      ## ========================================================================

      M <- self$M
      N <- self$N
      has_new <- length(new_components) > 0
      has_old <- length(old_components) > 0
      n_new <- length(new_components)
      n_old <- length(old_components)
      ## Boolean lookup: is component j in new_components?
      is_new_comp <- rep(FALSE, N)
      if (has_new) is_new_comp[new_components] <- TRUE
      ## For NEW/OLD subsets, map global column index to local index
      new_local_idx <- integer(N)  # new_local_idx[global_j] = local index in new_components
      old_local_idx <- integer(N)
      if (has_new) new_local_idx[new_components] <- seq_along(new_components)
      if (has_old) old_local_idx[old_components] <- seq_along(old_components)

      ## --- Pre-compute initial projection matrices and sums from bi_env_mat ---
      ## Social projection (MxM): S = B %*% t(B), diagonal zeroed
      social_mat <- bi_env_mat %*% t(bi_env_mat)
      diag(social_mat) <- 0
      ## Epistasis projection (NxN): E = t(B) %*% B, diagonal zeroed
      search_mat <- t(bi_env_mat) %*% bi_env_mat
      diag(search_mat) <- 0
      ## Row and column sums of B
      row_sums <- rowSums(bi_env_mat)
      col_sums <- colSums(bi_env_mat)
      ## K-stat degree counts: number of positive off-diagonal entries per row
      ## K_AA: for each actor, count of other actors sharing at least one component
      K_AA_vec <- rowSums(social_mat > 0)
      ## K_CC: for each component, count of other components sharing at least one actor
      K_CC_vec <- colSums(search_mat > 0)
      ## K_AC: for each actor, count of components it connects to = row_sums
      ## K_CA: for each component, count of actors connecting to it = col_sums

      ## --- NEW/OLD subset projections and K-stats ---
      if (has_new) {
        bi_new <- bi_env_mat[, new_components, drop = FALSE]
        social_new <- bi_new %*% t(bi_new)
        diag(social_new) <- 0
        search_new <- t(bi_new) %*% bi_new
        diag(search_new) <- 0
        row_sums_new <- rowSums(bi_new)
        col_sums_new <- colSums(bi_new)
        K_AA_NEW_vec <- rowSums(social_new > 0)
        K_CC_NEW_vec <- colSums(search_new > 0)
      }
      if (has_old) {
        bi_old <- bi_env_mat[, old_components, drop = FALSE]
        social_old <- bi_old %*% t(bi_old)
        diag(social_old) <- 0
        search_old <- t(bi_old) %*% bi_old
        diag(search_old) <- 0
        row_sums_old <- rowSums(bi_old)
        col_sums_old <- colSums(bi_old)
        K_AA_OLD_vec <- rowSums(social_old > 0)
        K_CC_OLD_vec <- colSums(search_old > 0)
      }

      ## --- Pre-allocate template data frames (created once, filled per step) ---
      ## stat grid: M actors x ntheta effects
      tpl_statgrid <- data.frame(
        chain_step_id = rep(0L, M * ntheta),
        actor_id      = rep(1:M, times = ntheta),
        effect_level  = rep(theta_levels_norates, each = M),
        stringsAsFactors = FALSE
      )
      ## Map effect_level -> effect_name (computed once)
      eff_level_to_name <- theta_df_norates$shortName
      names(eff_level_to_name) <- theta_df_norates$effect_level
      tpl_statgrid$effect_name <- eff_level_to_name[tpl_statgrid$effect_level]
      ## util grids: M actors
      tpl_utilgrid <- data.frame(
        chain_step_id = rep(0L, M),
        actor_id      = 1:M,
        stringsAsFactors = FALSE
      )
      ## K_AA/K_AC grids: M actors
      tpl_actor_grid <- data.frame(
        chain_step_id = rep(0L, M),
        actor_id      = 1:M,
        stringsAsFactors = FALSE
      )
      ## K_CA/K_CC grids: N components
      tpl_comp_grid <- data.frame(
        chain_step_id = rep(0L, N),
        component_id  = 1:N,
        stringsAsFactors = FALSE
      )
      ## NEW/OLD component grids
      if (has_new) {
        tpl_comp_new_grid <- data.frame(
          chain_step_id = rep(0L, n_new),
          component_id  = 1:n_new,
          stringsAsFactors = FALSE
        )
      }
      if (has_old) {
        tpl_comp_old_grid <- data.frame(
          chain_step_id = rep(0L, n_old),
          component_id  = 1:n_old,
          stringsAsFactors = FALSE
        )
      }

      ## --- Helper: rank-1 update of a symmetric projection matrix + K-degree vector ---
      ## Updates proj_mat (symmetric, zero diagonal) and k_vec (positive-entry count per row)
      ## in place (by reference via parent environment). Returns nothing.
      ## actor_i: the row that was toggled in B
      ## comp_j: the column that was toggled in B
      ## delta: +1 or -1
      ## b_col: the column of B that was toggled (B[, comp_j] AFTER toggle)
      ## dim_size: nrow (=ncol) of proj_mat

      for (i in 1:nchains) {

        mstep <- tiechdf[i,]

        if(i %% 100 == 0 | i == nchains) cat(sprintf('\n %.2f%s', 100*i/nchains,'%'))
        ## Fire progress callback periodically (~20 times over full loop)
        if (!is.null(progress_callback) && (i %% report_interval == 0 || i == nchains)) {
          progress_callback(i, nchains)
        }
        ## update bipartite environment matrix for one step (toggle one dyad)

        ## Behavior-DV ministeps change an actor attribute, not a tie: their
        ## id_to is a behavior value and must never be toggled as a component.
        ## No-op for chains without a behavior DV.
        if ( ! mstep$stability &&
             ! identical(as.character(mstep$dv_varname), .SEARCHNET_BEHAVIOR_DV_NAME) ) {
          actor_i <- mstep$id_from
          comp_j  <- mstep$id_to
          ## Record change for diff-based storage
          bi_env_changes[i, ] <- c(i, actor_i, comp_j)
          ## Determine delta BEFORE toggling
          delta <- if (bi_env_mat[actor_i, comp_j] == 0) 1L else -1L
          ## Toggle the cell
          bi_env_mat[actor_i, comp_j] <- 1L - bi_env_mat[actor_i, comp_j]

          ## ----- Rank-1 update: SOCIAL projection S = B %*% t(B) -----
          ## S[actor_i, k] += delta * B[k, comp_j] for all k (and symmetrically)
          ## But we must also track K_AA degree counts (# positive off-diag entries)
          b_col_j <- bi_env_mat[, comp_j]  # column j of B AFTER toggle
          update_vec_s <- delta * b_col_j   # change to social_mat[actor_i, ] and social_mat[, actor_i]
          ## Before updating, record which entries were positive (for K_AA tracking)
          old_row_pos <- social_mat[actor_i, ] > 0  # logical M-vector
          old_col_pos <- social_mat[, actor_i] > 0   # same by symmetry, but track separately for safety
          ## Apply rank-1 update
          social_mat[actor_i, ] <- social_mat[actor_i, ] + update_vec_s
          social_mat[, actor_i] <- social_mat[, actor_i] + update_vec_s
          social_mat[actor_i, actor_i] <- 0  # keep diagonal zero
          ## Update K_AA degree counts for actor_i (its entire row changed)
          new_row_pos <- social_mat[actor_i, ] > 0
          K_AA_vec[actor_i] <- sum(new_row_pos)
          ## For all OTHER actors k: only social_mat[k, actor_i] changed
          ## Check if sign changed for column actor_i across all rows k
          changed_k <- which(old_col_pos != (social_mat[, actor_i] > 0))
          changed_k <- changed_k[changed_k != actor_i]  # exclude diagonal
          for (k in changed_k) {
            if (social_mat[k, actor_i] > 0) {
              K_AA_vec[k] <- K_AA_vec[k] + 1L
            } else {
              K_AA_vec[k] <- K_AA_vec[k] - 1L
            }
          }

          ## ----- Rank-1 update: EPISTASIS projection E = t(B) %*% B -----
          b_row_i <- bi_env_mat[actor_i, ]  # row i of B AFTER toggle
          update_vec_e <- delta * b_row_i
          old_row_pos_e <- search_mat[comp_j, ] > 0
          old_col_pos_e <- search_mat[, comp_j] > 0
          search_mat[comp_j, ] <- search_mat[comp_j, ] + update_vec_e
          search_mat[, comp_j] <- search_mat[, comp_j] + update_vec_e
          search_mat[comp_j, comp_j] <- 0
          new_row_pos_e <- search_mat[comp_j, ] > 0
          K_CC_vec[comp_j] <- sum(new_row_pos_e)
          changed_l <- which(old_col_pos_e != (search_mat[, comp_j] > 0))
          changed_l <- changed_l[changed_l != comp_j]
          for (l in changed_l) {
            if (search_mat[l, comp_j] > 0) {
              K_CC_vec[l] <- K_CC_vec[l] + 1L
            } else {
              K_CC_vec[l] <- K_CC_vec[l] - 1L
            }
          }

          ## ----- Incremental row/col sums -----
          row_sums[actor_i] <- row_sums[actor_i] + delta
          col_sums[comp_j]  <- col_sums[comp_j]  + delta

          ## ----- Rank-1 updates for NEW/OLD subset projections -----
          if (has_new && is_new_comp[comp_j]) {
            ## comp_j is a NEW component -- update NEW projections
            local_j <- new_local_idx[comp_j]
            ## Social NEW: S_new = B_new %*% t(B_new)
            b_col_j_new <- bi_env_mat[, comp_j]  # same as b_col_j
            update_s_new <- delta * b_col_j_new
            old_pos_sn <- social_new[actor_i, ] > 0
            old_cpos_sn <- social_new[, actor_i] > 0
            social_new[actor_i, ] <- social_new[actor_i, ] + update_s_new
            social_new[, actor_i] <- social_new[, actor_i] + update_s_new
            social_new[actor_i, actor_i] <- 0
            K_AA_NEW_vec[actor_i] <- sum(social_new[actor_i, ] > 0)
            changed_kn <- which(old_cpos_sn != (social_new[, actor_i] > 0))
            changed_kn <- changed_kn[changed_kn != actor_i]
            for (k in changed_kn) {
              if (social_new[k, actor_i] > 0) {
                K_AA_NEW_vec[k] <- K_AA_NEW_vec[k] + 1L
              } else {
                K_AA_NEW_vec[k] <- K_AA_NEW_vec[k] - 1L
              }
            }
            ## Epistasis NEW: E_new = t(B_new) %*% B_new
            b_row_i_new <- bi_env_mat[actor_i, new_components]  # row i restricted to new cols
            update_e_new <- delta * b_row_i_new
            old_pos_en <- search_new[local_j, ] > 0
            old_cpos_en <- search_new[, local_j] > 0
            search_new[local_j, ] <- search_new[local_j, ] + update_e_new
            search_new[, local_j] <- search_new[, local_j] + update_e_new
            search_new[local_j, local_j] <- 0
            K_CC_NEW_vec[local_j] <- sum(search_new[local_j, ] > 0)
            changed_ln <- which(old_cpos_en != (search_new[, local_j] > 0))
            changed_ln <- changed_ln[changed_ln != local_j]
            for (l in changed_ln) {
              if (search_new[l, local_j] > 0) {
                K_CC_NEW_vec[l] <- K_CC_NEW_vec[l] + 1L
              } else {
                K_CC_NEW_vec[l] <- K_CC_NEW_vec[l] - 1L
              }
            }
            ## row/col sums for NEW subset
            row_sums_new[actor_i] <- row_sums_new[actor_i] + delta
            col_sums_new[local_j] <- col_sums_new[local_j] + delta
          }
          if (has_old && !is_new_comp[comp_j]) {
            ## comp_j is an OLD component -- update OLD projections
            local_j <- old_local_idx[comp_j]
            b_col_j_old <- bi_env_mat[, comp_j]
            update_s_old <- delta * b_col_j_old
            old_pos_so <- social_old[actor_i, ] > 0
            old_cpos_so <- social_old[, actor_i] > 0
            social_old[actor_i, ] <- social_old[actor_i, ] + update_s_old
            social_old[, actor_i] <- social_old[, actor_i] + update_s_old
            social_old[actor_i, actor_i] <- 0
            K_AA_OLD_vec[actor_i] <- sum(social_old[actor_i, ] > 0)
            changed_ko <- which(old_cpos_so != (social_old[, actor_i] > 0))
            changed_ko <- changed_ko[changed_ko != actor_i]
            for (k in changed_ko) {
              if (social_old[k, actor_i] > 0) {
                K_AA_OLD_vec[k] <- K_AA_OLD_vec[k] + 1L
              } else {
                K_AA_OLD_vec[k] <- K_AA_OLD_vec[k] - 1L
              }
            }
            ## Epistasis OLD
            b_row_i_old <- bi_env_mat[actor_i, old_components]
            update_e_old <- delta * b_row_i_old
            old_pos_eo <- search_old[local_j, ] > 0
            old_cpos_eo <- search_old[, local_j] > 0
            search_old[local_j, ] <- search_old[local_j, ] + update_e_old
            search_old[, local_j] <- search_old[, local_j] + update_e_old
            search_old[local_j, local_j] <- 0
            K_CC_OLD_vec[local_j] <- sum(search_old[local_j, ] > 0)
            changed_lo <- which(old_cpos_eo != (search_old[, local_j] > 0))
            changed_lo <- changed_lo[changed_lo != local_j]
            for (l in changed_lo) {
              if (search_old[l, local_j] > 0) {
                K_CC_OLD_vec[l] <- K_CC_OLD_vec[l] + 1L
              } else {
                K_CC_OLD_vec[l] <- K_CC_OLD_vec[l] - 1L
              }
            }
            row_sums_old[actor_i] <- row_sums_old[actor_i] + delta
            col_sums_old[local_j] <- col_sums_old[local_j] + delta
          }
        } ## end if (!stability)

        ## --- Compute statmat using pre-computed sums (avoids full recompute) ---
        ## Pass pre-computed row_sums and col_sums to avoid O(M*N) recomputation
        statmat <- self$get_struct_mod_stats_mat_from_bi_mat( bi_env_mat )
        ### ADD INTERACTIONS
        if (length(interaction_effnames)) {
          for (int_i in 1:length(interaction_effnames)) {
            vars <- strsplit(interaction_effnames[int_i],'[|]')[[1]]
            statmat[ , interaction_effnames[int_i] ] <- statmat[ , vars[1] ] *  statmat[ , vars[2] ]
          }
        }

        ## --- Fill pre-allocated data frames instead of expand.grid per step ---
        step_statgrid <- tpl_statgrid
        step_statgrid$chain_step_id <- i
        step_statgrid$value <- c( statmat )
        step_statgrid$value_contributions <- c(sweep(statmat, 2, theta_mat[i, ], "*"))
        step_statgrid$stability <- mstep$stability
        stats_li[[i]] <- step_statgrid
        bi_env_arr[ , , i]  <- bi_env_mat
        ## Add utilities to array
        util <- c( statmat %*% theta_mat[i, ] )
        step_utilgrid <- tpl_utilgrid
        step_utilgrid$chain_step_id <- i
        step_utilgrid$utility <- util
        step_utilgrid$stability <- mstep$stability
        util_li[[i]] <- step_utilgrid
        step_util_diffgrid <- tpl_utilgrid
        step_util_diffgrid$chain_step_id <- i
        step_util_diffgrid$utility <- if(i == 1){ NA } else { util - util_lag }
        step_util_diffgrid$stability <- mstep$stability
        util_diff_li[[i]] <- step_util_diffgrid
        ######
        ## update utility lag for next period difference (applies to i>1)
        util_lag <- util
        ######

        ## --- K-stats from incrementally maintained projection matrices ---
        ## NOTE: The original code counts ALL positive entries per row/col of the
        ## projection including the diagonal. The diagonal of B%*%t(B) at [i,i] is
        ## row_sums[i], and of t(B)%*%B at [j,j] is col_sums[j]. Our projection
        ## matrices have zeroed diagonals for clean rank-1 updates, so we add the
        ## diagonal contribution back: +1 if the sum is positive.
        ##
        ## K_AA: actor-actor degree from social projection (already maintained)
        K_AA_grid <- tpl_actor_grid
        K_AA_grid$chain_step_id <- i
        K_AA_grid$value <- K_AA_vec + as.integer(row_sums > 0)
        K_AA_grid$stability <- mstep$stability
        K_AA_li[[i]] <- K_AA_grid
        ## K_AC: actor-component degree = row_sums (# components per actor)
        K_AC_grid <- tpl_actor_grid
        K_AC_grid$chain_step_id <- i
        K_AC_grid$value <- as.integer(row_sums)
        K_AC_grid$stability <- mstep$stability
        K_AC_li[[i]] <- K_AC_grid
        ## K_CA: component-actor degree = col_sums (# actors per component)
        K_CA_grid <- tpl_comp_grid
        K_CA_grid$chain_step_id <- i
        K_CA_grid$value <- as.integer(col_sums)
        K_CA_grid$stability <- mstep$stability
        K_CA_li[[i]] <- K_CA_grid
        ## K_CC: component-component degree from the bipartite projection
        K_CC_grid <- tpl_comp_grid
        K_CC_grid$chain_step_id <- i
        K_CC_grid$value <- K_CC_vec + as.integer(col_sums > 0)
        K_CC_grid$stability <- mstep$stability
        K_CC_li[[i]] <- K_CC_grid
        #---

        ## --- NEW component K stats (guard against empty new_components) ---
        if (has_new) {
          K_AA_NEW_grid <- tpl_actor_grid
          K_AA_NEW_grid$chain_step_id <- i
          K_AA_NEW_grid$value <- K_AA_NEW_vec + as.integer(row_sums_new > 0)
          K_AA_NEW_grid$stability <- mstep$stability
          K_AA_NEW_li[[i]] <- K_AA_NEW_grid
          K_AC_NEW_grid <- tpl_actor_grid
          K_AC_NEW_grid$chain_step_id <- i
          K_AC_NEW_grid$value <- as.integer(row_sums_new)
          K_AC_NEW_grid$stability <- mstep$stability
          K_AC_NEW_li[[i]] <- K_AC_NEW_grid
          K_CA_NEW_grid <- tpl_comp_new_grid
          K_CA_NEW_grid$chain_step_id <- i
          ## Original: apply(t(B_new)%*%B_new, 1, sum(x>0)) = search_new positive per row (incl diag)
          K_CA_NEW_grid$value <- K_CC_NEW_vec + as.integer(col_sums_new > 0)
          K_CA_NEW_grid$stability <- mstep$stability
          K_CA_NEW_li[[i]] <- K_CA_NEW_grid
          K_CC_NEW_grid <- tpl_comp_new_grid
          K_CC_NEW_grid$chain_step_id <- i
          ## Original: apply(B_new, 2, sum(x>0)) = col_sums_new
          K_CC_NEW_grid$value <- as.integer(col_sums_new)
          K_CC_NEW_grid$stability <- mstep$stability
          K_CC_NEW_li[[i]] <- K_CC_NEW_grid
        }
        ## --- OLD component K stats (guard against empty old_components) ---
        if (has_old) {
          K_AA_OLD_grid <- tpl_actor_grid
          K_AA_OLD_grid$chain_step_id <- i
          K_AA_OLD_grid$value <- K_AA_OLD_vec + as.integer(row_sums_old > 0)
          K_AA_OLD_grid$stability <- mstep$stability
          K_AA_OLD_li[[i]] <- K_AA_OLD_grid
          K_AC_OLD_grid <- tpl_actor_grid
          K_AC_OLD_grid$chain_step_id <- i
          K_AC_OLD_grid$value <- as.integer(row_sums_old)
          K_AC_OLD_grid$stability <- mstep$stability
          K_AC_OLD_li[[i]] <- K_AC_OLD_grid
          K_CA_OLD_grid <- tpl_comp_old_grid
          K_CA_OLD_grid$chain_step_id <- i
          ## Original: apply(t(B_old)%*%B_old, 1, sum(x>0)) = search_old positive per row (incl diag)
          K_CA_OLD_grid$value <- K_CC_OLD_vec + as.integer(col_sums_old > 0)
          K_CA_OLD_grid$stability <- mstep$stability
          K_CA_OLD_li[[i]] <- K_CA_OLD_grid
          K_CC_OLD_grid <- tpl_comp_old_grid
          K_CC_OLD_grid$chain_step_id <- i
          ## Original: apply(B_old, 2, sum(x>0)) = col_sums_old
          K_CC_OLD_grid$value <- as.integer(col_sums_old)
          K_CC_OLD_grid$stability <- mstep$stability
          K_CC_OLD_li[[i]] <- K_CC_OLD_grid
        }

      }
      
      actor_strats <- self$get_actor_strategies()
      ##-----------------
      ## Actor Network Statistics long dataframe
      stats_df <- data.table::rbindlist( stats_li )
      stats_df$actor_id <- as.factor(stats_df$actor_id)
      stats_df$strategy <- as.factor( actor_strats[ stats_df$actor_id ] )
      ## Actor Utility  long dataframe
      util_df <- data.table::rbindlist( util_li ) 
      util_df$actor_id <- as.factor(util_df$actor_id)
      util_df$strategy <- as.factor( actor_strats[ util_df$actor_id ] )
      ## Actor Utility Difference long dataframe
      util_diff_df <- data.table::rbindlist( util_diff_li ) 
      util_diff_df$actor_id <- as.factor(util_diff_df$actor_id)
      util_diff_df$strategy <- as.factor( actor_strats[ util_diff_df$actor_id ] )
      ##---
      
      K_AA_df <- data.table::rbindlist( K_AA_li ) 
      K_AA_df$actor_id <- as.factor(K_AA_df$actor_id)
      K_AA_df$component_id <- as.factor( NA )
      K_AA_df$strategy <- as.factor( actor_strats[ K_AA_df$actor_id ] )
      K_AC_df <- data.table::rbindlist( K_AC_li ) 
      K_AC_df$actor_id <- as.factor(K_AC_df$actor_id)
      K_AC_df$component_id <- as.factor( NA )
      K_AC_df$strategy <- as.factor( actor_strats[ K_AC_df$actor_id ] )
      K_AA_NEW_df <- if (length(K_AA_NEW_li)) data.table::rbindlist(K_AA_NEW_li) else data.frame(chain_step_id=integer(0), actor_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_AA_NEW_df) > 0) { K_AA_NEW_df$actor_id <- as.factor(K_AA_NEW_df$actor_id); K_AA_NEW_df$component_id <- as.factor(NA); K_AA_NEW_df$strategy <- as.factor(actor_strats[K_AA_NEW_df$actor_id]) }
      K_AC_NEW_df <- if (length(K_AC_NEW_li)) data.table::rbindlist(K_AC_NEW_li) else data.frame(chain_step_id=integer(0), actor_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_AC_NEW_df) > 0) { K_AC_NEW_df$actor_id <- as.factor(K_AC_NEW_df$actor_id); K_AC_NEW_df$component_id <- as.factor(NA); K_AC_NEW_df$strategy <- as.factor(actor_strats[K_AC_NEW_df$actor_id]) }
      K_AA_OLD_df <- if (length(K_AA_OLD_li)) data.table::rbindlist(K_AA_OLD_li) else data.frame(chain_step_id=integer(0), actor_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_AA_OLD_df) > 0) { K_AA_OLD_df$actor_id <- as.factor(K_AA_OLD_df$actor_id); K_AA_OLD_df$component_id <- as.factor(NA); K_AA_OLD_df$strategy <- as.factor(actor_strats[K_AA_OLD_df$actor_id]) }
      K_AC_OLD_df <- if (length(K_AC_OLD_li)) data.table::rbindlist(K_AC_OLD_li) else data.frame(chain_step_id=integer(0), actor_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_AC_OLD_df) > 0) { K_AC_OLD_df$actor_id <- as.factor(K_AC_OLD_df$actor_id); K_AC_OLD_df$component_id <- as.factor(NA); K_AC_OLD_df$strategy <- as.factor(actor_strats[K_AC_OLD_df$actor_id]) }
      
      ##---
      K_CA_df <- data.table::rbindlist( K_CA_li ) 
      K_CA_df$strategy <- as.factor(sapply(K_CA_df$component_id, function(x) ifelse( x %in% new_components, "NEW", "OLD") ))
      K_CA_df$component_id <- as.factor(K_CA_df$component_id)
      K_CA_df$actor_id <- as.factor( NA )
      K_CC_df <- data.table::rbindlist( K_CC_li ) 
      K_CA_df$strategy <- as.factor(sapply(K_CC_df$component_id, function(x) ifelse( x %in% new_components, "NEW", "OLD") ))
      K_CC_df$component_id <- as.factor(K_CC_df$component_id)
      K_CC_df$actor_id <- as.factor( NA )
      K_CA_NEW_df <- if (length(K_CA_NEW_li)) data.table::rbindlist(K_CA_NEW_li) else data.frame(chain_step_id=integer(0), component_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_CA_NEW_df) > 0) { K_CA_NEW_df$component_id <- as.factor(K_CA_NEW_df$component_id); K_CA_NEW_df$actor_id <- as.factor(NA); K_CA_NEW_df$strategy <- as.factor("NEW") }
      K_CC_NEW_df <- if (length(K_CC_NEW_li)) data.table::rbindlist(K_CC_NEW_li) else data.frame(chain_step_id=integer(0), component_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_CC_NEW_df) > 0) { K_CC_NEW_df$component_id <- as.factor(K_CC_NEW_df$component_id); K_CC_NEW_df$actor_id <- as.factor(NA); K_CC_NEW_df$strategy <- as.factor("NEW") }
      K_CA_OLD_df <- if (length(K_CA_OLD_li)) data.table::rbindlist(K_CA_OLD_li) else data.frame(chain_step_id=integer(0), component_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_CA_OLD_df) > 0) { K_CA_OLD_df$component_id <- as.factor(K_CA_OLD_df$component_id); K_CA_OLD_df$actor_id <- as.factor(NA); K_CA_OLD_df$strategy <- as.factor("OLD") }
      K_CC_OLD_df <- if (length(K_CC_OLD_li)) data.table::rbindlist(K_CC_OLD_li) else data.frame(chain_step_id=integer(0), component_id=integer(0), value=numeric(0), stability=logical(0))
      if (nrow(K_CC_OLD_df) > 0) { K_CC_OLD_df$component_id <- as.factor(K_CC_OLD_df$component_id); K_CC_OLD_df$actor_id <- as.factor(NA); K_CC_OLD_df$strategy <- as.factor("OLD") }
      
      
      ##---
      
      #####---------------
      return(list(
        stats_df = stats_df,
        util_df = util_df,
        util_diff_df = util_diff_df,
        K_AA_df = K_AA_df,
        K_AC_df = K_AC_df,
        K_CA_df = K_CA_df,
        K_CC_df = K_CC_df,
        K_AA_NEW_df = K_AA_NEW_df,
        K_AC_NEW_df = K_AC_NEW_df,
        K_AA_OLD_df = K_AA_OLD_df,
        K_AC_OLD_df = K_AC_OLD_df,
        K_CA_NEW_df = K_CA_NEW_df,
        K_CC_NEW_df = K_CC_NEW_df,
        K_CA_OLD_df = K_CA_OLD_df,
        K_CC_OLD_df = K_CC_OLD_df,
        bi_env_arr = bi_env_arr,
        bi_env_arr_initial = self$bipartite_matrix_init,
        bi_env_changes = bi_env_changes
      ))
    },
    
    
    ## ## @see https://www.stats.ox.ac.uk/~snijders/siena/WorkOnChains.r
    # For the meaning of the 13 fields of a ministep:
    # From siena07utilities.cpp:
    #               difference in dependent behavior variable: 0 if nothing changes
    # This is from C++ code; note that in C++, numbering starts at 0.
    # Therefore, for a one-mode network the values of Ego and Alter run
    # from 0 to n-1, where n is the number of actors.
    # If the mini-step is a network mini-step for a one-mode network,
    # the change made can be inferred by comparing ego and alter.
    # If ego and alter are the same node, then no change has occurred;
    # if ego and alter are different nodes, then the value for the tie
    # from ego to alter is self$toggled (1 -> 0; 0 -> 1).
    # For a two-mode network the values of Alter run from 0 to m,
    # where m is the number of nodes in the second mode;
    # here the value Alter=m means that no change has occurred.
    ### Interpretation of the columns on the outputted df - 
    # 1 - network or behavior function 
    # 2 - same as 1, denoted as 0/1 
    # 3 - same as 1/2, denoted using varname 
    # 4 - ego ID (starting @0) 
    # 5 - if network function - alter ID (starting @0) for changes 
    # ego ID for no change 
    # 0 if behavior function 
    # 6 - 0 if network 
    # -1, 0, 1 for change in behavior level behavior 
    # Our aims don't make use of columns 7-10
    # 11 - Designates stability (TRUE = no change, FALSE = change)
    search_rsiena_process_ministep_chain = function(verbose=TRUE) {
      
      if (is.null(self$rsiena_model$chain)) {
        stop("Chain not available. Ensure returnChains=TRUE was set in the siena07 call.")
      }
      simChain <- self$rsiena_model$chain
      depvar <- 1
      period <- 1

      ###--------
      ## Bipatite network chain --> value Alter=m means that no change has occurred.
      ##** "chain[[run]][[depvar]][[period]][[ministep]]"**
      chainDatZeroIndex <- ldply(seq_along(simChain), function(iter){
        ncolsIter <- length(simChain[[iter]][[depvar]][[period]])
        t(matrix(unlist(simChain[[iter]][[depvar]][[period]]), nc=ncolsIter))
      })
      ### one-index chain as new object
      chainDat <- chainDatZeroIndex
      chainDat[,2] <- as.integer(chainDat[,2]) ## 0=Network; 1=Behavior
      chainDat[,4] <- as.numeric(chainDat[,4]) + 1 ## Ego (from) 1-indexing from C++ 0-index
      chainDat[,5] <- as.numeric(chainDat[,5]) + 1 ## Alter (to) 1-indexing from C++ 0-index
      chainDat[,6] <- as.numeric(chainDat[,6]) ## Behavior difference
      chainDat[,7] <- as.numeric(chainDat[,7]) ##
      chainDat[,8] <- as.numeric(chainDat[,8]) ##
      chainDat[,9] <- as.numeric(chainDat[,9]) ##
      chainDat[,11] <- as.logical(chainDat[,11]) ## Stability
      # Set dataframe Names
      
      
      ###--------
      .getTieChangeAfterOneindexing <- function(x, N) {
        ## already re-indexed id_from and id_to -- changing from C++ 0-index to R 1-index
        dv_name <- x[3]
        id_from <- x[4]
        id_to   <- x[5]
        if(dv_name == 'self$bipartite_rsienaDV') {
          ## bipartite network after oneIndexing the node ids:  id_to==(N+1) means no tie
          return(ifelse(id_to == (N+1), FALSE, TRUE))
        } else if (dv_name %in% c('self$social_rsienaDV','self$search_rsienaDV')) {
          ## bipartite network after oneIndexing the node ids:  id_to==id_from means no tie
          return(ifelse(id_from == id_to, FALSE, TRUE))
        } else if (dv_name == .SEARCHNET_BEHAVIOR_DV_NAME) {
          ## A behavior ministep changes an actor's ATTRIBUTE, never a tie.
          ## Its `id_to` column carries no node id at all, so the bipartite
          ## rules above would misread it. tie_change is unambiguously FALSE;
          ## the magnitude of the behavior change lives in `beh_difference`.
          return(FALSE)
        } else {
          stop(sprintf('dv_name %s not implemented in .getTieChange()', dv_name))
        }
      }
      
      names(chainDat) <- c('dv_type','dv_type_bin','dv_varname','id_from','id_to','beh_difference',
                           'reciprocal_rate','LogOptionSetProb', 'LogChoiceProb', 'diagonal','stability')
      # Set tie change by rules
      chainDat$tie_change <- apply(chainDat, 1, function(x) .getTieChangeAfterOneindexing(x, N=self$N) )
      chainDat$chain_step_id <- 1:nrow(chainDat)
      chainDat$chain_change_id <- NA
      chainDat$chain_change_id[ !chainDat$stability ] <- 1:sum(!chainDat$stability)
      chainDat <- chainDat %>% tidyr::fill(chain_change_id, .direction = "down") ## a no-change takes previous chain_change_id
      
      ## set to simulation self
      self$chain_stats <- chainDat

      ## Build bi_env_arr (3D array of bipartite matrices across chain steps)
      ## This is essential for plotting and is computed here so it's always available
      ## even if search_rsiena_process_stats() fails downstream
      nchains <- nrow(chainDat)
      bi_env_mat <- self$bipartite_matrix_init
      bi_env_arr <- array(NA, dim = c(self$M, self$N, nchains))
      ## Also build diff-based storage (Task 4 memory optimization):
      ## Store initial matrix + per-step (step, i, j) changes instead of full 3D array.
      ## Reduces memory from O(M*N*nchains) to O(nchains + M*N).
      bi_env_changes <- matrix(NA_integer_, nrow = nchains, ncol = 3)
      colnames(bi_env_changes) <- c("step", "actor_i", "comp_j")
      ## A behavior ministep is not a tie toggle. Its `id_to` is a behavior
      ## value, not a component id, so toggling on it would corrupt the state
      ## trajectory. Skip those rows. Identical to the previous behavior for
      ## every chain that contains only bipartite ministeps.
      .is_beh_step <- chainDat$dv_varname == .SEARCHNET_BEHAVIOR_DV_NAME
      for (.i in 1:nchains) {
        if (!chainDat$stability[.i] && !.is_beh_step[.i]) {
          bi_env_changes[.i, ] <- c(.i, chainDat$id_from[.i], chainDat$id_to[.i])
          bi_env_mat <- self$toggleBiMat(bi_env_mat, chainDat$id_from[.i], chainDat$id_to[.i])
        } else {
          bi_env_changes[.i, ] <- c(.i, NA_integer_, NA_integer_)
        }
        bi_env_arr[, , .i] <- bi_env_mat
      }
      self$bi_env_arr <- bi_env_arr
      self$bi_env_arr_initial <- self$bipartite_matrix_init
      self$bi_env_changes <- bi_env_changes

      if (verbose) {
        cat('\n\n\nSimulated Decision Chain Header:\n\n')
        print(dim(self$chain_stats))
        print(self$chain_stats[1:5,])
        cat('\n...\n\n')
      }

    },
    
    search_rsiena_process_stats = function(progress_callback = NULL) {
      chain_stats_list <- self$get_chain_stats_list(progress_callback = progress_callback)
      self$bi_env_arr <- chain_stats_list$bi_env_arr  ## chain array of bipartite matrix from chain of decision steps
      self$bi_env_arr_initial <- chain_stats_list$bi_env_arr_initial
      self$bi_env_changes <- chain_stats_list$bi_env_changes
      self$actor_stats_df      <- chain_stats_list$stats_df
      self$actor_util_df       <- chain_stats_list$util_df
      self$actor_util_diff_df  <- chain_stats_list$util_diff_df
      ###
      ###
      self$K_AA_df  <- chain_stats_list$K_AA_df
      self$K_AC_df  <- chain_stats_list$K_AC_df
      self$K_CA_df  <- chain_stats_list$K_CA_df
      self$K_CC_df  <- chain_stats_list$K_CC_df
      ###
      self$K_AA_NEW_df  <- chain_stats_list$K_AA_NEW_df
      self$K_AC_NEW_df  <- chain_stats_list$K_AC_NEW_df
      self$K_CA_NEW_df  <- chain_stats_list$K_CA_NEW_df
      self$K_CC_NEW_df  <- chain_stats_list$K_CC_NEW_df
      self$K_AA_OLD_df  <- chain_stats_list$K_AA_OLD_df
      self$K_AC_OLD_df  <- chain_stats_list$K_AC_OLD_df
      self$K_CA_OLD_df  <- chain_stats_list$K_CA_OLD_df
      self$K_CC_OLD_df  <- chain_stats_list$K_CC_OLD_df
    },
    
    
    #   #   'K_AA'=list(), ##**TODO** Actor social space [degree = central position in social network]
    
    
    ##===================== PLOTTING ============================

    
    search_rsiena_plot_stability = function(tol=1e-5, step_size=1, wave_id=1) {
      
      sims = self$rsiena_model$sims
      n <- length(sims)
      bi_env_dv_id <- which( names(self$config_structure_model) == 'dv_bipartite' )
      if (!length(bi_env_dv_id)) stop('dv_bipartite is missing from self$config_structure_model.')
      outlist <- list()
      difflist <- list()
      jaccardlist <- list()
      K_soc_list <- list()
      K_env_list <- list()
      sim_ids_plot <- seq(1, n, by=step_size) ##**TODO CHECK**
      # ##**TODO: CHECK** if not plotting every simulation, then drop first sim ( (nsims/step_size) - 1)
      ###
      for(i in 1:length(sim_ids_plot)) {
        sim_id <- sim_ids_plot[ i ]
        cat(sprintf(' %s ', i))
        el_bi_env <- sims[[ sim_id ]][[ wave_id ]][[ bi_env_dv_id ]]$`1`
        ## update numbering of second mode (the comonent integer names shift upward by the number of actors)
        el_bi_env[,2] <- el_bi_env[,2] + self$M
        MplusN <- self$M + self$N
        #############
        ## Bipartite matrix space (N+M by N+M)
        ## Undirected --> Upper right rectangle of full bipartite matrix
        ##   M N
        bi_env_mat_sp <- sparseMatrix(i = el_bi_env[,1],
                                      j = el_bi_env[,2],
                                      x = el_bi_env[,3],
                                      dims = c(MplusN, MplusN))
        bi_env_mat_new  <- as.matrix(bi_env_mat_sp)[ 1:self$M, (self$M+1):(MplusN) ]
        ## Add ACTOR NAMES on rows
        rownames(bi_env_mat_new) <- as.character( 1:self$M )
        ## Add COMPONENT NAMES on columns (N+1 ... N+M)
        colnames(bi_env_mat_new) <- as.character( (1:self$N) + self$M  )
        outlist[[ sprintf('sim%d',i) ]] <- bi_env_mat_new
        
        if (i == 1) {
          difflist[[ sprintf('diff%d-%d', i-1, i) ]] <- 0
          jaccardlist[[sprintf('jac%d-%d', i-1, i)]] <- 0
        } else {
          difflist[[ sprintf('diff%d-%d', i-1, i) ]] <-  bi_env_mat_new - outlist[[ (i-1) ]] 
          jaccardlist[[sprintf('jac%d-%d', i-1, i)]] <- self$get_jaccard_index(m0 = outlist[[ (i-1) ]], m1 = bi_env_mat_new )
        }
        
        ## `multiple` and `weighted` are mutually exclusive in igraph, and this
        ## call passed both, so search_rsiena_plot_stability() could never run.
        ## `weighted` is the right one to keep: the other three biadjacency calls
        ## in this package use weighted = T and none uses multiple, and the
        ## sibling at get_bipartite_igraph_from_matrix() builds the same graph
        ## for the same K_soc/K_env degree projections. The matrix is binary
        ## here in any case, so the two would agree on degree().
        new_bi_g <- igraph::graph_from_biadjacency_matrix(bi_env_mat_new,
                                                          directed = FALSE, mode = 'all',
                                                          weighted = TRUE)
        ## add.names dropped: igraph wants a character vertex-attribute name or
        ## NULL there, not a logical, and TRUE raised "`name` must be a single
        ## string". The matrix already carries dimnames, which is where the
        ## vertex names come from, so the argument was doing nothing anyway.
        projections <- igraph::bipartite_projection(new_bi_g, multiplicity = TRUE, which = 'both')
        K_soc_list[[i]] <- igraph::degree(projections$proj1)
        K_env_list[[i]] <- igraph::degree(projections$proj2)
        
      }
      
      ##_-----------------------------------------
      ## K interdependencies (K_E, K_S)
      
      degree_summary <- do.call(rbind, lapply(1:length(K_soc_list), function(iter) {
         data.frame(
           Iteration = iter,
           Mean_K_S = mean(K_soc_list[[iter]]),
           Q25_K_S = quantile(K_soc_list[[iter]], 0.25),
           Q75_K_S = quantile(K_soc_list[[iter]], 0.75),
           Mean_K_E = mean(K_env_list[[iter]]),
           Q25_K_E = quantile(K_env_list[[iter]], 0.25),
           Q75_K_E = quantile(K_env_list[[iter]], 0.75)
         )
      }))
      K_plt <- ggplot(degree_summary, aes(x = Iteration)) +
         geom_line(aes(y = Mean_K_S, color = "Mean K_S")) +
         geom_ribbon(aes(ymin = Q25_K_S, ymax = Q75_K_S, fill = "K_S"), alpha = 0.05) +
         geom_line(aes(y = Mean_K_E, color = "Mean K_E")) +
         geom_ribbon(aes(ymin = Q25_K_E, ymax = Q75_K_E, fill = "K_E"), alpha = 0.05) +
         scale_color_manual(values = c("Mean K_S" = "blue", "Mean K_E" = "red")) +
         scale_fill_manual(values = c("K_S" = "blue", "K_E" = "red")) +
         labs(title = "Degree Progress Over Iterations",
                   x = "Iteration",
                   y = "Degree",
                   color = "Mean Degree",
                   fill = "IQR (Mid-50%)") +
         theme_minimal()
      
      print(K_plt)
      
      
      #-------------------------------------------
      ## par(mfrow=) is global device state. This method set it and never
      ## restored it, so every subsequent plot in the session stayed split 1x3.
      op <- graphics::par(mfrow = c(1,3))
      on.exit(graphics::par(op), add = TRUE)
      ##------------------------------------------
      jaccard_vec <- plyr::ldply(jaccardlist)[sim_ids_plot[-1], 2] ## skip first period (no change yet)
      n_changes <- length(jaccard_vec)
      sim_ids_plot_steps <- sim_ids_plot[-1] 
      stability_vec <- cumsum(jaccard_vec) / (1:n_changes)
      stability_delta <- c(0, abs(diff(stability_vec))) / abs(stability_vec)
      plot(x=sim_ids_plot_steps, y=jaccard_vec, 
           type='l', ylab='Jaccard Index [t-1, t]', xlab='Simulation Iteration',
           main='Inter-Sim Distance\n(Jaccard Index between same-wave sims)')
      plot(x=sim_ids_plot_steps, y=stability_vec , 
           type='l' , ylab='Jaccard Index [t-1, t]', xlab='Simulation Iteration',
           ylim=c( .9, 1),
           main='Stabiliation by Iteration\n(Inter-Sim Distance Moving Average)'
           ); abline(h=1, col='gray', lty=2)
      plot(x = sim_ids_plot_steps, y=stability_delta, 
           type='l', log='y', xlab='Simulation Iteration', 
           ylab='Ln Stability Change [t-1, t]', 
           main='Sufficient Iterations?\n(Inter-Sim Distance Moving Average Change)' 
           ); abline(h = tol, col='pink', lty=2)

      ## Return the computed series invisibly. The method drew three plots and
      ## then threw away the numbers behind them, so a caller could look at the
      ## stability trace but could not test or reuse it -- and the return was
      ## NULL, which is what test-plotting.R asserts against. invisible(), so
      ## callers that ignore the value are unaffected.
      invisible(list(
        degree_plot     = K_plt,
        sim_ids         = sim_ids_plot_steps,
        jaccard         = jaccard_vec,
        stability       = stability_vec,
        stability_delta = stability_delta,
        tol             = tol
      ))
    },
    
    # Convenience function for plotting all relevant plots 
    # or one plot by specifying plot 
    search_rsiena_multiwave_plot = function(type=c(), 
                                            rolling_window = 10, 
                                            actor_ids=c(),
                                            component_ids=c(),
                                            wave_ids=c(),
                                            thin_factor=1, 
                                            thin_wave_factor=1,
                                            smooth_method='loess',
                                            show_utility_points=TRUE,
                                            show_strategy_means=TRUE,
                                            append_plot=FALSE,
                                            histogram_position='identity',
                                            scale_utility=TRUE,
                                            return_plot=TRUE,
                                            plot_file=NA, plot_dir=NA,
                                            loess_span=0.4
    ) {
      plist <- list()
      if (length(type)==0 |  'K_4panel' %in% type)
        plist[['K_4panel']] <- self$search_rsiena_multiwave_plot_K_4panel(actor_ids, component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
      
      if (length(type)==0 |  'K_AA_strategy_summary' %in% type)
        plist[['K_AA_strategy_summary']] <- self$search_rsiena_multiwave_plot_K_AA_strategy_summary(actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
      
      if (length(type)==0 |  'K_AC_strategy_summary' %in% type)
        plist[['K_AC_strategy_summary']] <- self$search_rsiena_multiwave_plot_K_AC_strategy_summary(actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
      
      if (length(type)==0 |  'K_CA_strategy_summary' %in% type)
        plist[['K_CA_strategy_summary']] <- self$search_rsiena_multiwave_plot_K_CA_strategy_summary(component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
      
      if (length(type)==0 |  'K_CC_strategy_summary' %in% type)
        plist[['K_CC_strategy_summary']] <- self$search_rsiena_multiwave_plot_K_CC_strategy_summary(component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
      
      
      if (length(type)==0 |  'utility_strategy_summary' %in% type)
        plist[['utility_strategy_summary']] <- self$search_rsiena_multiwave_plot_actor_utility_strategy_summary(actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, scale_utility, return_plot=TRUE, plot_file=plot_file, loess_span=loess_span )
      
      if (length(type)==0 |  'utility_by_strategy' %in% type)
        plist[['utility_by_strategy']] <- self$search_rsiena_multiwave_plot_actor_utility_by_strategy(actor_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
      if (length(type)==0 |  'utility_density_by_strategy' %in% type)
        plist[['utility_density_by_strategy']] <- self$search_rsiena_multiwave_plot_actor_utility_density_by_strategy(thin_wave_factor, return_plot=TRUE, plot_file=plot_file )
      #   plist[['stability']] <- self$search_rsiena_plot_stability() ##**TODO** Fix return plot
      if (length(type)==0 |  'utility_ridge_density_by_strategy' %in% type)
        plist[['utility_ridge_density_by_strategy']] <- self$search_rsiena_multiwave_plot_utility_ridge_density_by_strategy(actor_ids, wave_ids, thin_factor, thin_wave_factor, show_utility_points, show_strategy_means, return_plot=TRUE, plot_file=plot_file )
      #  SET plots 
      self$multiwave_plots <- if(append_plot) { append(self$multiwave_plots, plist) } else { plist }
      
      if(return_plot)
        return(plist)
    },
    
    
    search_rsiena_multiwave_plot_utility_ridge_density_by_strategy = function(actor_ids=c(),
                                                                              wave_ids=c(),
                                                                              thin_factor=1,
                                                                              thin_wave_factor=1,
                                                                              show_utility_points=TRUE,
                                                                              show_strategy_means=TRUE,
                                                                              scale_utility=TRUE,
                                                                              return_plot=TRUE,
                                                                              plot_file=NA,
                                                                              plot_dir=NA,
                                                                              plot_periods=4) {
      actor_strat <- self$get_actor_strategies() 
      nstep <- sum(!self$chain_stats$stability)
      coveffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      covparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      covfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      covDvTypes <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
      componentDV_ids <- grep('self\\$component_\\d{1,2}_coCovar', covDvTypes) ## ex: "self$component_1_coCovar"
      stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes ) ## ex: "self$strat_1_coCovar" 
      compoeffs   <- coveffs[ componentDV_ids ]
      compoparams <- covparams[ componentDV_ids ]
      compofixs   <- covfixs[ componentDV_ids ]
      strateffs   <- coveffs[ stratDV_ids ]
      stratparams <- covparams[ stratDV_ids ]
      stratfixs   <- covfixs[ stratDV_ids ]
      actor_component_period <- self$M * self$N
      density_ridges_rel_min_height = 1e-07  ## prevents density ridges colored lines from covering full x-axis (clarifies group separation)
      ## Compare 2 actors utilty
      dat <- self$actor_wave_util %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          strategy = actor_strat[ actor_id ],
          chain_below_med =  chain_step_id < median(chain_step_id),
          actor_component_period = 1 + floor( chain_step_id / actor_component_period )
        ) %>% 
        mutate(
          stabilization_summary_period = ifelse(actor_component_period <= (plot_periods - 1), 
                                                actor_component_period, 
                                                sprintf('%s+\n(%s-%s)',plot_periods,plot_periods,max(actor_component_period)))
        )
      util_lab <- 'Actor Utility'
      if(scale_utility) {
        util_sc <- scale(dat$utility)
        util_lab <- sprintf('Actor Utility\n(Standardized Center = %.2f; Scale = %.2f)',
                            attr(util_sc, 'scaled:center'), 
                            attr(util_sc, 'scaled:scale'))
        if (!all(dat$utility == 0))
          dat <- dat %>% mutate(utility = c(scale(utility)))
      }
      density_rng <- range(dat$utility, na.rm=TRUE)
      density_absdiff_scale <- abs(diff(density_rng)) * 0.15
      util_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
      point_size <- 10 / log( nstep )
      point_alpha <- min( 1,  1/log10( nstep ) )
      if(length(actor_ids))
        dat <- dat %>% filter(actor_id %in% actor_ids)
      if(length(wave_ids))
        dat <- dat %>% filter(wave_id %in% wave_ids)
      dat_acp_stabil_means <- dat %>% group_by(stabilization_summary_period, strategy) %>% 
        dplyr::summarize(mean=mean(utility, na.rm=TRUE)) %>%
        mutate(PeriodFct = forcats::fct_rev(as.factor(stabilization_summary_period)))
      ##==============================================
      strat_legend_title <- sprintf("Strategy (%s) :  ", paste(strateffs, collapse = '_'))
      strat_break <- levels(actor_strat) 
      strat_labs <- sapply(1:length(levels(actor_strat)), function(i) {
        a <- levels(actor_strat)[i]
        names(a) <- a ## # names(a) <- sprintf('%s: %s', i, a)
        return(a)
      }) 
      dat_dens_rigde <- dat %>%
        mutate(PeriodFct = forcats::fct_rev(as.factor(stabilization_summary_period))) 
      group_dens_means <- dat_dens_rigde %>% ungroup() %>% group_by(strategy) %>% 
        dplyr::summarize(n=n(),mean=mean(utility,na.rm=TRUE))
      ##---------------------
      ## Start Plot
      plt.dr <- ggplot(dat_dens_rigde, aes(y = PeriodFct, x = utility, color=strategy, fill=strategy)) +
        ggridges::stat_density_ridges(aes(point_color = strategy, point_fill = strategy, point_shape = strategy),
                            quantile_lines = TRUE, alpha = .3, rel_min_height = density_ridges_rel_min_height,
                            point_size=.4,
                            jittered_points = TRUE, 
                            position = ggridges::position_raincloud(adjust_vlines = FALSE, ygap = -.1, height = .15),# "raincloud",
                            quantiles = c(0.5), linewidth=.75 ) +
        scale_y_discrete(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        ggridges::scale_fill_cyclical(
          breaks = strat_break,
          labels = strat_labs,
          values = scales::hue_pal()(length(levels(actor_strat))), # c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
          guide = "legend"
        ) +
        labs(
          x = util_lab,
          y = sprintf(" Time Period \n(Actor-Component-Period = %s decision steps)", actor_component_period ),
          title = "Actor Utility Stabilization Paths",
          subtitle = sprintf("(Decision Chain Iterations: %s )", nstep), 
          color = strat_legend_title,
          fill = strat_legend_title,
          point_color = strat_legend_title,
          point_fill = strat_legend_title,
          point_shape = strat_legend_title
        ) +
        geom_vline(xintercept = 0, linetype=1) +
        coord_cartesian(clip = "off") +
        ggridges::theme_ridges(grid = TRUE, center=TRUE) + 
        theme(legend.position = 'bottom')
      if(show_strategy_means) {
        plt.dr <- plt.dr +  
          geom_vline(data = group_dens_means,  aes(xintercept = mean, color=strategy), 
                     linetype=3, linewidth=1.3) +
          geom_text(data = group_dens_means,
                    aes(x = mean, y = 1+length(unique(dat_dens_rigde$stabilization_summary_period)), label = round(mean, 2), color=strategy),
                    inherit.aes = FALSE, size = 5, nudge_x=-.1, nudge_y=.35  ) 
      }

      if(return_plot)
        return(plt.dr)
    },
    
    
    search_rsiena_multiwave_plot_K_4panel = function(actor_ids=c(), 
                                                     component_ids=c(), 
                                                     wave_ids=c(), 
                                                     thin_factor=1, 
                                                     thin_wave_factor=1, 
                                                     smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                     show_utility_points=TRUE, 
                                                     return_plot=TRUE,
                                                     plot_file=NA, plot_dir=NA
                                                     ) {
      K_AA  <- self$search_rsiena_multiwave_plot_K_AA_strategy_summary(actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=TRUE, show_title=FALSE, return_plot=TRUE)
      K_AC <- self$search_rsiena_multiwave_plot_K_AC_strategy_summary(actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=FALSE, show_title=FALSE, return_plot=TRUE)
      K_CA <- self$search_rsiena_multiwave_plot_K_CA_strategy_summary(component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=FALSE, show_title=FALSE, return_plot=TRUE)
      K_CC  <- self$search_rsiena_multiwave_plot_K_CC_strategy_summary(component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=TRUE, show_title=FALSE, return_plot=TRUE)
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      covDvTypes <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
      componentDV_ids <- grep('self\\$component_\\d{1,2}_coCovar', covDvTypes) ## ex: "self$component_1_coCovar"
      stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes ) ## ex: "self$strat_1_coCovar" 
      
      maintitle <- sprintf('Environment: Actors (M) = %s, Components (N) = %s, Init.Prob. = %.2f\nActor Strategy:  %s\nComponent Payoff:  %s\nStructure:  %s', 
                           self$M, self$N, self$BI_PROB,
                           paste( paste(paste(strateffs[stratDV_ids], stratparams[stratDV_ids], sep='= '), stratfixs[stratDV_ids], sep='' ), collapse = ';  '),
                           paste( paste(paste(strateffs[componentDV_ids], stratparams[componentDV_ids], sep='= '), stratfixs[componentDV_ids], sep=''), collapse = ';  '),
                           paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
      )
      
      combined_plot_notitle <- ggarrange(
        K_AC, K_CA, 
        K_AA, K_CC,
        nrow = 2, ncol = 2, 
        common.legend = TRUE, # Share a common legend if needed
        legend = "bottom"#,     # Place legend at the bottom
      ) 
      
      combined_plot <- ggpubr::annotate_figure(
        combined_plot_notitle,
        top = ggpubr::text_grob(maintitle, color = "black", size = 14) ## face = "bold", 
      )
      
      self$multiwave_plots <- list(combined_plot=combined_plot)
      
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               combined_plot, 
               width = 10, height = 8, units = 'in', dpi = 600)
      
      if(return_plot)
        return(combined_plot)
    },
    
    search_rsiena_multiwave_plot_K_CC_strategy_summary = function(component_ids=c(), 
                                                                  wave_ids=c(),
                                                                  thin_factor=1, 
                                                                  thin_wave_factor=1,
                                                                  smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                                  show_utility_points=TRUE,
                                                                  show_legend=TRUE,
                                                                  show_title=TRUE,
                                                                  return_plot=TRUE,
                                                                  plot_file=NA, plot_dir=NA
                                                                 ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar are not  set.")
      if ( !identical(attr(self$component_1_coCovar, 'nodeSet'), 'COMPONENTS') )
        stop("Component payoff values in self$component_1_coCovar are not set.")
      range_midpoint <- min(self$component_1_coCovar, na.rm=TRUE) + ( abs(diff(range(self$component_1_coCovar, na.rm = TRUE))) / 2 )
      component_types <- as.factor( ifelse(self$component_1_coCovar > range_midpoint, 'High', 'Low') )
      nstep <- sum(!self$chain_stats$stability)
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      ## Compare 2 actors utilty
      dat <- self$K_wave_C %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          component_type = component_types[ component_id ],
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      y_lab <- 'K_CC: Component Epistasis Degree'
      density_rng <- range(dat$value, na.rm=TRUE)
      density_absdiff_scale <- abs(diff(density_rng)) * 0.15
      y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
      point_size <- 10 / log( nstep )
      point_alpha <- min( 1,  .5/log10( nstep ) )
      if(length(component_ids))
        dat <- dat %>% filter(component_id %in% component_ids)
      if(length(wave_ids))
        dat <- dat %>% filter(wave_id %in% wave_ids)
      dat_wave_means <- dat %>% group_by(wave_id) %>% 
        dplyr::summarize(mean=mean(value, na.rm=TRUE))
      plt <- ggplot(dat, aes(x=chain_step_id, y=value)) + 
        geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
        facet_grid(wave_id ~ .) 
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=component_id), alpha=point_alpha, shape=1, size=point_size, show.legend = FALSE)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(color=component_id, fill=component_id), method = smooth_method, linewidth=.5, alpha=.09, show.legend = FALSE, se=FALSE)
      plt <- plt + theme_bw() + 
        # scale_linetype_manual(values = rep(1:8, length.out = length(unique(dat$component_id)))) +
        ylim(y_lim) + 
        ylab(y_lab) +
        xlab('Actor Decision Chain Ministep') +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom"#,
        )
      if (show_title)
        plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s', 
                                     paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                     paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
        ))
      plt <- plt +  guides(color = guide_legend(nrow = 1))
      
      #### Density
      stratmeans <- dat %>% group_by(component_id, wave_id) %>% 
        dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt2 <- ggplot(dat, aes(x=value, color=component_id, fill=component_id)) + ##linetype=chain_half
        geom_density(alpha=.01, linewidth=.5, show.legend = FALSE)  +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=component_id), linetype=2, linewidth=.5, show.legend = FALSE) +
        geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
        labs(y='', x='') +
        # xlim(c(ggplot_build(plt)$layout$panel_params[[1]]$y.range)) + 
        xlim(y_lim) +
        coord_flip() +
        facet_grid(wave_id ~ .) +
        ylab('K_CC Density') +
        # labs(color='component_type', fill='component_type') +
        theme_bw() + theme(
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'none', 
          plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank()#,
        ) 
      if (show_title)
        plt2 <- plt2 + ggtitle('\n\n\n')
      
      
      combined_plot <- ggarrange(
        plt, plt2, 
        ncol = 2, 
        widths = c(4.1,0.9), # Adjust column widths
        common.legend = TRUE, # Share a common legend if needed
        legend = ifelse(show_legend, "bottom", "none")#,     # Place legend at the bottom
      ) 
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               combined_plot, 
               width = 10, height = 8, units = 'in', dpi = 600)
      if(return_plot)
        return(combined_plot)
    },
    
    search_rsiena_multiwave_plot_K_CA_strategy_summary = function(component_ids=c(), 
                                                                  wave_ids=c(),
                                                                  thin_factor=1, 
                                                                  thin_wave_factor=1,
                                                                  smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                                  show_utility_points=TRUE,
                                                                  show_legend=TRUE,
                                                                  show_title=TRUE,
                                                                  return_plot=TRUE,
                                                                  plot_file=NA, plot_dir=NA
    ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar are not set.")
      if ( !identical(attr(self$component_1_coCovar, 'nodeSet'), 'COMPONENTS') )
        stop("Component payoff values in self$component_1_coCovar are not set.")
      range_midpoint <- min(self$component_1_coCovar, na.rm=TRUE) + ( abs(diff(range(self$component_1_coCovar, na.rm = TRUE))) / 2 )
      component_types <- as.factor( ifelse(self$component_1_coCovar > range_midpoint, 'High', 'Low') )
      nstep <- sum(!self$chain_stats$stability)
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      ## Compare 2 actors utilty
      dat <- self$K_wave_B2 %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          component_type = component_types[ component_id ],
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      y_lab <- 'K_CA: Component-Actor Degree'
      density_rng <- range(dat$value, na.rm=TRUE)
      density_absdiff_scale <- abs(diff(density_rng)) * 0.15
      y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
      point_size <- 10 / log( nstep )
      point_alpha <- min( 1,  .5/log10( nstep ) )
      if(length(component_ids))
        dat <- dat %>% filter(component_id %in% component_ids)
      if(length(wave_ids))
        dat <- dat %>% filter(wave_id %in% wave_ids)
      dat_wave_means <- dat %>% group_by(wave_id) %>% 
        dplyr::summarize(mean=mean(value, na.rm=TRUE))
      plt <- ggplot(dat, aes(x=chain_step_id, y=value)) + 
        geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
        facet_grid(wave_id ~ .) 
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=component_id), alpha=point_alpha, shape=1, size=point_size, show.legend = FALSE)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(color=component_id, fill=component_id), method = smooth_method, linewidth=.5, alpha=.09, se=FALSE, show.legend = FALSE)
      plt <- plt + theme_bw() + 
        # scale_linetype_manual(values = rep(1:8, length.out = length(unique(dat$component_id)))) +
        ylim(y_lim) + 
        ylab(y_lab) +
        xlab('Actor Decision Chain Ministep') +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "center",
          legend.key.width = unit(0.8, "cm"),  # Adjust legend key width
          legend.spacing.x = unit(0.3, "cm")   # Adjust spacing between keys
        )
      if(show_title)
        plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s', 
                                     paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                     paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
        ))
      plt <- plt +  guides(color = guide_legend(nrow = 1))
      
      #### Density
      stratmeans <- dat %>% group_by(component_id, wave_id) %>% 
        dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt2 <- ggplot(dat, aes(x=value, color=component_id, fill=component_id)) + ##linetype=chain_half
        geom_density(alpha=.01, linewidth=.5, show.legend = FALSE)  +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=component_id), linetype=2, linewidth=.5, show.legend = FALSE) +
        geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
        labs(y='', x='') +
        # xlim(c(ggplot_build(plt)$layout$panel_params[[1]]$y.range)) + 
        xlim(y_lim) +
        coord_flip() +
        facet_grid(wave_id ~ .) +
        ylab('K_CA Density') +
        theme_bw() + theme(
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'none', 
          plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank()#,
        ) 
      if (show_title)
        plt2 <- plt2 + ggtitle('\n\n\n')
      
      
      combined_plot <- ggarrange(
        plt, plt2, 
        ncol = 2, 
        widths = c(4.1,0.9), # Adjust column widths
        common.legend = TRUE, # Share a common legend if needed
        legend = ifelse(show_legend, "bottom", "none")#,     # Place legend at the bottom
      ) 
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               combined_plot, 
               width = 10, height = 8, units = 'in', dpi = 600)
      if(return_plot)
        return(combined_plot)
    },
    
    
    search_rsiena_multiwave_plot_K_AC_strategy_summary = function(actor_ids=c(), 
                                                                 wave_ids=c(),
                                                                 thin_factor=1, 
                                                                 thin_wave_factor=1,
                                                                 smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                                 show_utility_points=TRUE,
                                                                 show_legend=TRUE,
                                                                 show_title=TRUE,
                                                                 return_plot=TRUE,
                                                                 plot_file=NA, plot_dir=NA
    ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <-  self$get_actor_strategies() 
      nstep <- sum(!self$chain_stats$stability)
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      ## Compare 2 actors utilty
      dat <- self$K_wave_B1 %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          strategy = actor_strat[ actor_id ],
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      y_lab <- 'K_AC: Actor-Component Degree'
      density_rng <- range(dat$value, na.rm=TRUE)
      density_absdiff_scale <- abs(diff(density_rng)) * 0.15
      y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
      point_size <- 10 / log( nstep )
      point_alpha <- min( 1,  1/log10( nstep ) )
      if(length(actor_ids))
        dat <- dat %>% filter(actor_id %in% actor_ids)
      if(length(wave_ids))
        dat <- dat %>% filter(wave_id %in% wave_ids)
      dat_wave_means <- dat %>% group_by(wave_id) %>% 
        dplyr::summarize(mean=mean(value, na.rm=TRUE))
      plt <- ggplot(dat, aes(x=chain_step_id, y=value)) + 
        geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
        facet_grid(wave_id ~ .) 
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy, fill=strategy), method = smooth_method, linewidth=1, alpha=.09)
      plt <- plt + theme_bw() + 
        ylim(y_lim) + 
        ylab(y_lab) +
        xlab('Actor Decision Chain Ministep') +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "center",
          legend.key.width = unit(0.8, "cm"),  # Adjust legend key width
          legend.spacing.x = unit(0.3, "cm")   # Adjust spacing between keys
        )
      if(show_title)
        plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s', 
                                     paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                     paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
        ))
      plt <- plt +  guides(color = guide_legend(nrow = 1))
      
      #### Density
      stratmeans <- dat %>% group_by(strategy, wave_id) %>% 
        dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt2 <- ggplot(dat, aes(x=value, color=strategy, fill=strategy)) + ##linetype=chain_half
        geom_density(alpha=.1, linewidth=1)  +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
        geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
        labs(y='', x='') +
        # xlim(c(ggplot_build(plt)$layout$panel_params[[1]]$y.range)) + 
        xlim(y_lim) +
        coord_flip() +
        facet_grid(wave_id ~ .) +
        ylab('K_AC Density') +
        theme_bw() + theme(
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'none', 
          plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank()#,
        ) 
      if (show_title)
        plt2 <- plt2 + ggtitle('\n\n\n')
      
      
      combined_plot <- ggarrange(
        plt, plt2, 
        ncol = 2, 
        widths = c(4.1,0.9), # Adjust column widths
        common.legend = TRUE, # Share a common legend if needed
        legend = ifelse(show_legend, "bottom", "none")#,     # Place legend at the bottom
      ) 
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               combined_plot, 
               width = 10, height = 8, units = 'in', dpi = 600)
      if(return_plot)
        return(combined_plot)
    },

    
    search_rsiena_multiwave_plot_K_AA_strategy_summary = function(actor_ids=c(), 
                                                                 wave_ids=c(),
                                                                 thin_factor=1, 
                                                                 thin_wave_factor=1,
                                                                 smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                                 show_utility_points=TRUE,
                                                                 show_legend=TRUE,
                                                                 show_title=TRUE,
                                                                 return_plot=TRUE,
                                                                 plot_file=NA, plot_dir=NA
    ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <-  self$get_actor_strategies() 
      nstep <- sum(!self$chain_stats$stability)
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      ## Compare 2 actors utilty
      dat <- self$K_wave_A %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          strategy = actor_strat[ actor_id ],
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      y_lab <- 'K_AA: Social Degree'
      density_rng <- range(dat$value, na.rm=TRUE)
      density_absdiff_scale <- abs(diff(density_rng)) * 0.15
      y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
      point_size <- 10 / log( nstep )
      point_alpha <- min( 1,  1/log10( nstep ) )
      if(length(actor_ids))
        dat <- dat %>% filter(actor_id %in% actor_ids)
      if(length(wave_ids))
        dat <- dat %>% filter(wave_id %in% wave_ids)
      dat_wave_means <- dat %>% group_by(wave_id) %>% 
        dplyr::summarize(mean=mean(value, na.rm=TRUE))
      plt <- ggplot(dat, aes(x=chain_step_id, y=value)) + 
        geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
        facet_grid(wave_id ~ .) 
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy, fill=strategy), method = smooth_method, linewidth=1, alpha=.09)
      plt <- plt + theme_bw() + 
        ylim(y_lim) + 
        ylab(y_lab) +
        xlab('Actor Decision Chain Ministep') +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "center",
          legend.key.width = unit(0.8, "cm"),  # Adjust legend key width
          legend.spacing.x = unit(0.3, "cm")   # Adjust spacing between keys
        )
      if(show_title)
        plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s', 
                                     paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                     paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
        ))
      plt <- plt +  guides(color = guide_legend(nrow = 1))
      
      #### Density
      stratmeans <- dat %>% group_by(strategy, wave_id) %>% 
        dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt2 <- ggplot(dat, aes(x=value, color=strategy, fill=strategy)) + ##linetype=chain_half
        geom_density(alpha=.1, linewidth=1)  +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
        geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
        labs(y='', x='') +
        # xlim(c(ggplot_build(plt)$layout$panel_params[[1]]$y.range)) + 
        xlim(y_lim) +
        coord_flip() +
        facet_grid(wave_id ~ .) +
        ylab('K_AA Density') +
        theme_bw() + theme(
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'none', 
          plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank()#,
        )  
      if (show_title)
        plt2 <- plt2 + ggtitle('\n\n\n')
      
      
      combined_plot <- ggarrange(
        plt, plt2, 
        ncol = 2, 
        widths = c(4.1,0.9), # Adjust column widths
        common.legend = TRUE, # Share a common legend if needed
        legend = ifelse(show_legend, "bottom", "none")#,     # Place legend at the bottom
      ) 
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               combined_plot, 
               width = 10, height = 8, units = 'in', dpi = 600)
      if(return_plot)
        return(combined_plot)
    },
    
    get_actor_strategy_set_effects = function() {
      coveffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      covDvTypes <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
      stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes ) ## ex: "self$strat_1_coCovar" 
      strat_effs <- coveffs[ stratDV_ids ]
      return(strat_effs)
    },
    
    get_actor_strategy_set_effects_length = function() {
      strat_effs <- self$get_actor_strategy_set_effects()
      return(length(strat_effs))
    },
    
    get_actor_strategies = function(as_factor = TRUE) {
      nstrat <- self$get_actor_strategy_set_effects_length()
      strats <- if (nstrat == 0) {
        rep("none", self$M)
      } else if(nstrat == 1) {
        as.character(self$strat_1_coCovar )
      } else if   (nstrat == 2) {
        paste(self$strat_1_coCovar,
              self$strat_2_coCovar, sep='_')
      } else if   (nstrat == 3) {
        paste(self$strat_1_coCovar,
              self$strat_2_coCovar,
              self$strat_3_coCovar, sep='_')
      } else if   (nstrat == 4) {
        paste(self$strat_1_coCovar,
              self$strat_2_coCovar,
              self$strat_3_coCovar,
              self$strat_4_coCovar, sep='_')
      } else if   (nstrat == 5) {
        paste(self$strat_1_coCovar,
              self$strat_2_coCovar,
              self$strat_3_coCovar,
              self$strat_4_coCovar,
              self$strat_5_coCovar, sep='_')
      }
      if (as_factor)
        strats <- as.factor( strats )
      return(strats)
    },
    
    
    search_rsiena_multiwave_plot_actor_utility_strategy_summary = function(actor_ids=c(), 
                                                                           wave_ids=c(),
                                                                            thin_factor=1, 
                                                                            thin_wave_factor=1,
                                                                            smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                                            show_utility_points=TRUE,
                                                                            scale_utility=TRUE,
                                                                            return_plot=TRUE,
                                                                            plot_file=NA, plot_dir=NA,
                                                                           loess_span=0.4
    ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <- self$get_actor_strategies() 
      nstep <- sum(!self$chain_stats$stability)
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      covDvTypes <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
      componentDV_ids <- grep('self\\$component_\\d{1,2}_coCovar', covDvTypes) ## ex: "self$component_1_coCovar"
      stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes ) ## ex: "self$strat_1_coCovar" 
      ## Compare 2 actors utilty
      dat <- self$actor_wave_util %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          strategy = actor_strat[ actor_id ],
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      util_lab <- 'Actor Utility'
      if(scale_utility) {
        util_sc <- scale(dat$utility)
        util_lab <- sprintf('Actor Utility\n(Standardized Center = %.2f; Scale = %.2f)',
                            attr(util_sc, 'scaled:center'), 
                            attr(util_sc, 'scaled:scale'))
        if (!all(dat$utility == 0))
          dat <- dat %>% mutate(utility = c(scale(utility)))
      }
      density_rng <- range(dat$utility, na.rm=TRUE)
      density_absdiff_scale <- abs(diff(density_rng)) * 0.15
      util_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
      point_size <- 10 / log( nstep )
      point_alpha <- min( 1,  1/log10( nstep ) )
      if(length(actor_ids))
        dat <- dat %>% filter(actor_id %in% actor_ids)
      if(length(wave_ids))
        dat <- dat %>% filter(wave_id %in% wave_ids)
      dat_wave_means <- dat %>% group_by(wave_id) %>% 
        dplyr::summarize(mean=mean(utility, na.rm=TRUE))
      plt <- ggplot(dat, aes(x=chain_step_id, y=utility)) + 
        geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
        facet_grid(wave_id ~ .) 
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(color=strategy, fill=strategy, shape=actor_id), 
                                 method = smooth_method, span=loess_span, linewidth=1, alpha=.09)
      # Population Mean in black
      plt <- plt + geom_smooth(aes(x=chain_step_id, y=utility_mean), method = smooth_method, span=loess_span, 
                               data=dat %>% group_by(chain_step_id)%>%dplyr::summarize(utility_mean=mean(utility)),
                               color='black', linetype=1, alpha=.1) 
      plt <- plt + theme_bw() + 
        ylim(util_lim) + 
        ylab(util_lab) +
        xlab('Actor Decision Chain Ministep') +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "center",
          legend.key.width = unit(0.8, "cm"),  # Adjust legend key width
          legend.spacing.x = unit(0.3, "cm")   # Adjust spacing between keys
        )
      
      ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
      COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
      if (! 'coCovars' %in% names(self$config_structure_model$dv_bipartite) ) {
        rsiena_data <- sienaDataCreate(list(self$bipartite_rsienaDV), nodeSets = list(ACTORS, COMPONENTS))
        return(rsiena_data)
      }
      
      plt <- plt + ggtitle(sprintf('Environment: Actors (M) = %s, Components (N) = %s, Init.Prob. = %.2f\nActor Strategy:  %s\nComponent Payoff:  %s\nStructure:  %s', 
                                   self$M, self$N, self$BI_PROB,
                                   paste( paste(paste(strateffs[stratDV_ids], stratparams[stratDV_ids], sep='= '), stratfixs[stratDV_ids], sep='' ), collapse = ';  '),
                                   paste( paste(paste(strateffs[componentDV_ids], stratparams[componentDV_ids], sep='= '), stratfixs[componentDV_ids], sep=''), collapse = ';  '),
                                   paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
      ))
      plt <- plt +  guides(color = guide_legend(nrow = 1))
     
      #### Density
      stratmeans <- dat %>% group_by(strategy, wave_id) %>% 
        dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt2 <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) + ##linetype=chain_half
        geom_density(alpha=.1, linewidth=1)  +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
        geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
        labs(y='', x='') +
        # xlim(c(ggplot_build(plt)$layout$panel_params[[1]]$y.range)) + 
        xlim(util_lim) +
        coord_flip() +
        facet_grid(wave_id ~ .) +
        ylab('Actor Utility Density') +
        theme_bw() + theme(
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'none', 
          plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank()#,
          ) + ggtitle('\n\n\n')
      
      
      combined_plot <- ggarrange(
        plt, plt2, 
        ncol = 2, 
        widths = c(4.1,0.9), # Adjust column widths
        common.legend = TRUE, # Share a common legend if needed
        legend = "bottom"#,     # Place legend at the bottom
      ) 
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               combined_plot, 
               width = 10, height = 8, units = 'in', dpi = 600)
        
      if(return_plot)
        return(combined_plot)
    },
    
    search_rsiena_multiwave_plot_actor_utility_by_strategy = function(actor_ids=c(), 
                                                                      thin_factor=1, 
                                                                      thin_wave_factor=1,
                                                                      smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                                      show_utility_points=TRUE,
                                                                      return_plot=TRUE,
                                                                      plot_file=NA, plot_dir=NA
    ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <- self$get_actor_strategies() 
      ## Compare 2 actors utilty
      dat <- self$actor_wave_util %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(strategy = actor_strat[ actor_id ] )
      if(length(actor_ids))
        dat <- dat %>% filter(actor_id %in% actor_ids)
      plt <- ggplot(dat, aes(x=chain_step_id, y=utility)) + 
        geom_hline(data=dat%>%group_by(wave_id)%>%dplyr::summarize(mean=mean(utility, na.rm=TRUE)), aes(yintercept=mean), linetype=2, col='black' ) +
        facet_wrap( ~ wave_id)
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=strategy), alpha=.25, shape=1, size=2)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy), method = smooth_method, linewidth=1, alpha=.15)
      plt <- plt + theme_bw()
      # Add marginal density plots
      plt <- ggExtra::ggMarginal(plt, type = "density", margins = "y")
      
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               plt, 
               width = 10, height = 8, units = 'in', dpi = 600)
      if(return_plot)
        return(plt)
    },
    
    
    search_rsiena_multiwave_plot_actor_utility_density_by_strategy = function(thin_wave_factor=1, 
                                                                              return_plot=TRUE, 
                                                                              plot_file=NA, 
                                                                              plot_dir=NA) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <-  self$get_actor_strategies() 
      strateffs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
      stratparams <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
      stratfixs   <- sapply(self$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
      structeffs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$effect)
      structparams <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
      structfixs   <- sapply(self$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
      ## Compare 2 actors utilty
      dat <- self$actor_wave_util %>% 
        filter(wave_id %% thin_wave_factor == 0 ) %>% 
        mutate(
          strategy = actor_strat[ actor_id ] ,
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      ##filter(chain_step_id %% thin_factor == 0) %>% 
      stratmeans <- dat %>% group_by(strategy, chain_half, wave_id) %>% 
        dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) + ##linetype=chain_half
        geom_density(alpha=.1, linewidth=1)  +
        facet_grid( wave_id ~ chain_half) +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
        theme_bw() + 
        ggtitle(sprintf('Strategy: %s\nStructure: %s', 
                        paste( paste(paste(strateffs, stratparams, sep='='), stratfixs, sep='' ), collapse = '; '),
                        paste( paste(paste(structeffs, structparams, sep='='), structfixs, sep=''), collapse = '; ')
                        ))
      
      if(!is.na(plot_file))
        ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", self$config_environ_params$name, plot_file)), 
               plt, 
               width = 10, height = 8, units = 'in', dpi = 600)
      
      if(return_plot)
        return(plt)
    },
    
   
    # Convenience function for plotting all relevant plots 
    # or one plot by specifying plot 
    search_rsiena_plot = function(type=NA, 
                                  rolling_window = 10, 
                                  actor_ids=c(), 
                                  thin_factor=1, 
                                  smooth_method='loess',
                                  show_utility_points=TRUE,
                                  return_plot=TRUE,
                                  append_plot=FALSE,
                                  histogram_position='identity'
                                  ) {
      plist <- list()
      if (all(is.na(type)) |  'utility' %in% type)
        plist[['utility']] <- self$search_rsiena_plot_actor_utility(actor_ids, thin_factor, smooth_method, show_utility_points, return_plot=TRUE)
      
      if (all(is.na(type)) |  'utility_by_strategy' %in% type)
        plist[['utility_by_strategy']] <- self$search_rsiena_plot_actor_utility_by_strategy(actor_ids, thin_factor, smooth_method, show_utility_points, return_plot=TRUE)
      
      if (all(is.na(type)) |  'utility_density' %in% type)
        plist[['utility_density']] <- self$search_rsiena_plot_actor_utility_density(return_plot=TRUE)
      
      if (all(is.na(type)) |  'utility_density_by_strategy' %in% type)
        plist[['utility_density_by_strategy']] <- self$search_rsiena_plot_actor_utility_density_by_strategy(return_plot=TRUE)
      
      if (all(is.na(type)) |  'utility_histogram_by_strategy' %in% type)
        plist[['utility_histogram_by_strategy']] <- self$search_rsiena_plot_actor_utility_histogram_by_strategy(histogram_position, return_plot=TRUE)
      
      if (all(is.na(type)) |  'stability' %in% type)
        plist[['stability']] <- self$search_rsiena_plot_stability() ##**TODO** Fix return plot
      
      #  SET plots 
      self$plots <- if(append_plot) { append(self$plots, plist) } else { plist }
      
      if(return_plot)
        return(plist)
    },
    # ##**TODO** Action count data frames (explore, exploit) 
    search_rsiena_plot_actor_utility = function(actor_ids=c(), thin_factor=1, 
                                                smooth_method='loess', ##"lm", "glm", "gam", "loess","auto"
                                                show_utility_points=TRUE,
                                                return_plot=FALSE
                                                ) {
      ## Compare 2 actors utilty
      dat <- self$actor_util_df %>% filter(chain_step_id %% thin_factor == 0)
      if(length(actor_ids))
        dat <- dat%>%filter(actor_id %in% actor_ids)
      plt <- ggplot(dat, aes(x=chain_step_id, y=utility, color=actor_id)) + 
        geom_hline(yintercept = mean(dat$utility, na.rm=TRUE), linetype=2, col='black' )
      if(show_utility_points)
        plt <- plt + geom_point(alpha=.25, shape=1, size=2)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(linetype=actor_id), method = smooth_method, linewidth=1, alpha=.05)
      plt <- plt + theme_bw()
      print(plt)
      if(return_plot)
        return(plt)
    },
    
    search_rsiena_plot_actor_utility_by_strategy = function(actor_ids=c(), thin_factor=1, 
                                                        smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                        show_utility_points=TRUE,
                                                        return_plot=TRUE
                                                        ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <-  self$get_actor_strategies() 
      ## Compare 2 actors utilty
      dat <- self$actor_util_df %>% 
        filter(chain_step_id %% thin_factor == 0) %>% 
        mutate(strategy = actor_strat[ actor_id ] )
      if(length(actor_ids))
        dat <- dat %>% filter(actor_id %in% actor_ids)
      plt <- ggplot(dat, aes(x=chain_step_id, y=utility)) + 
        geom_hline(yintercept = mean(dat$utility, na.rm=TRUE), linetype=2, col='black' )
      if(show_utility_points)
        plt <- plt + geom_point(aes(color=strategy), alpha=.25, shape=1, size=2)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
      if(self$exists(smooth_method))
        plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy), method = smooth_method, linewidth=1, alpha=.15)
      plt <- plt + theme_bw()
      print(plt)
      if(return_plot)
        return(plt)
    },
    
    search_rsiena_plot_actor_utility_density = function(return_plot=TRUE) {
      dat <- self$actor_util_df ##%>% filter(chain_step_id %% thin_factor == 0)
      ## Actor density fact plots comparing H1 to H2 utility distribution
      dat$chain_half <- factor(1 + 1*(dat$chain_step_id >= median(dat$chain_step_id)), levels = c(2,1)) ## reverse order for linetype_1 used for Half_2
      plt <- ggplot(dat, aes(x=utility, color=actor_id, fill=actor_id, linetype=chain_half)) + 
         geom_density(alpha=.1, linewidth=1)  + 
         facet_wrap( ~ actor_id)+
         theme_bw()
      print(plt)
      if(return_plot)
        return(plt)
    },
    
    search_rsiena_plot_actor_utility_density_by_strategy = function(return_plot=TRUE) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <-  self$get_actor_strategies() 
      ## Compare 2 actors utilty
      dat <- self$actor_util_df %>%  
        mutate(
          strategy = actor_strat[ actor_id ] ,
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      ##filter(chain_step_id %% thin_factor == 0) %>% 
      stratmeans <- dat %>% group_by(strategy, chain_half) %>% 
        dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) + ##linetype=chain_half
        geom_density(alpha=.1, linewidth=1)  +
        facet_grid( chain_half ~ .) +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
        theme_bw() 
      print(plt)
      if(return_plot)
        return(plt)
    },
    
    search_rsiena_plot_actor_utility_histogram_by_strategy = function(histogram_position='identity', 
                                                                      return_plot=TRUE
                                                                      ) {
      ## actor strategy
      if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
        stop("Actor Strategy self$strat_1_coCovar not set.")
      actor_strat <-  self$get_actor_strategies() 
      ## Compare 2 actors utilty
      dat <- self$actor_util_df %>%  
        mutate(
          strategy = actor_strat[ actor_id ] ,
          chain_below_med =  chain_step_id < median(chain_step_id)
        )
      dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
      stratmeans <- dat %>% group_by(strategy, chain_half) %>% 
        dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
      ## Actor density fact plots comparing H1 to H2 utility distribution
      plt <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) + ##linetype=chain_half
        geom_histogram(alpha=.1, position = histogram_position) +
        facet_grid( chain_half ~ strategy ) +
        geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
        theme_bw()
      print(plt)
      if(return_plot)
        return(plt)
    },
    
    search_rsiena_plot_actor_ministep_count = function(rolling_window = 20) {
      if (is.null(self$chain_stats))
        stop("Run RSiena to set chain before plotting chain stats.")
      ## Get Actor-period count of ministep decisions
      step_summary_df <- self$chain_stats %>% mutate(chain_step_id = row_number()) %>%
        filter( dv_varname =='self$bipartite_rsienaDV') %>% 
        group_by(id_from)
      iters_with_change <- unique(step_summary_df$chain_step_id)
      cnt_mat <- matrix(0, nrow=self$M, ncol=length(self$rsiena_model$chain) )
      for (iter in iters_with_change) {
        cat(sprintf(' %s ', iter))
        iter_df <- step_summary_df %>% filter(chain_step_id == iter) ##%>% mutate(X__id_from=X__id_from+1)
        actor_cnts <- iter_df %>% group_by(id_from) %>% count()
        cnt_mat[ actor_cnts$id_from, iter ] <- actor_cnts$n
      }
      
      cnt_df <- as.data.frame(cnt_mat)
      colnames(cnt_df) <- paste0("Period_", 1:length(self$rsiena_model$chain) )
      cnt_df$Actor <- 1:self$M
      rownames(cnt_df) <- paste0("Actor_", cnt_df$Actor)
      
      long_data <- cnt_df %>% 
        pivot_longer(cols = starts_with("Period_"),
                     names_to = "period",
                     values_to = "count") %>% 
        mutate(
          period = as.numeric(gsub("Period_", "", period)),
          actor = as.numeric(gsub("Actor_", "", Actor))
        ) %>%
        select(actor, period, count)  # Reorder columns
      
      ## plot actor event sequences
      plt <- ggplot(long_data, aes(y = factor(actor), x=period, fill = count)) + 
        geom_tile() + 
        scale_fill_gradient(low = "white", high = "blue", name = "Ministeps Count") +
        theme_bw() + labs('y' = 'Actor')
      
      print(plt)
      
      return(plt)
    },
    

    ##**TODO**
    ##**CREATE VISUALIZE_NETWORKS plots for the RSiena approach search_rsiena_batchrun **
    # -------------------------------------------------------------------
    #   -------------------------------------------------------------------
    ####
    ####
  
  
    # Plot time series of mean and 95% quantile of degrees for K_S and K_E
    plot_degree_progress_from_sims = function(K_soc_list, K_env_list, plot_save = FALSE) {
      
      n <- length(K_soc_list)
      
      degree_summary <- do.call(rbind, lapply(1:n, function(iter) {
        data.frame(
          Iteration = iter,
          Mean_K_S = mean(K_soc_list[[iter]]),
          Q25_K_S = quantile(K_soc_list[[iter]], 0.25),
          Q75_K_S = quantile(K_soc_list[[iter]], 0.75),
          Mean_K_E = mean(K_env_list[[iter]]),
          Q25_K_E = quantile(K_env_list[[iter]], 0.25),
          Q75_K_E = quantile(K_env_list[[iter]], 0.75)
        )
      }))
      
      (plt <- ggplot(degree_summary, aes(x = Iteration)) +
          geom_line(aes(y = Mean_K_S, color = "Mean K_S")) +
          geom_ribbon(aes(ymin = Q25_K_S, ymax = Q75_K_S, fill = "K_S"), alpha = 0.1) +
          geom_line(aes(y = Mean_K_E, color = "Mean K_E")) +
          geom_ribbon(aes(ymin = Q25_K_E, ymax = Q75_K_E, fill = "K_E"), alpha = 0.1) +
          scale_color_manual(values = c("Mean K_S" = "blue", "Mean K_E" = "red")) +
          scale_fill_manual(values = c("K_S" = "blue", "K_E" = "red")) +
          labs(title = "Degree Progress Over Iterations",
               x = "Iteration",
               y = "Degree",
               color = "Mean Degree",
               fill = "IQR (Mid-50%)") +
          theme_minimal()
      )
      
      # Calculate average degree (K) for the social space and component interaction space
      avg_degree_social <- mean(degree_summary$Mean_K_S)
      avg_degree_component <- mean(degree_summary$Mean_K_E)
      
      if (plot_save) {
        keystring <- sprintf("K_S_%.2f_K_CC_%.2f", 
                              avg_degree_social, avg_degree_component)
        ggsave(filename = sprintf('Ks_degree_SAOM-NK_networks_%s_%s.png',
                                  keystring, round(as.numeric(Sys.time())*100)), 
               plot = plt, height = 4.5, width = 9, units = 'in', dpi = 300)
      }
    },
    
    
    # Plot time series of mean and 95% quantile of degrees for K_S and K_E
    plot_degree_progress = function(plot_save = FALSE) {
      if (length(self$degree_history_K_S) == 0 || length(self$degree_history_K_E) == 0) {
        stop("No degree history recorded. Run the simulation first.")
      }
      
      degree_summary <- do.call(rbind, lapply(1:self$ITERATION, function(iter) {
        data.frame(
          Iteration = iter,
          Mean_K_S = mean(self$degree_history_K_S[[iter]]),
          Q25_K_S = quantile(self$degree_history_K_S[[iter]], 0.25),
          Q75_K_S = quantile(self$degree_history_K_S[[iter]], 0.75),
          Mean_K_E = mean(self$degree_history_K_E[[iter]]),
          Q25_K_E = quantile(self$degree_history_K_E[[iter]], 0.25),
          Q75_K_E = quantile(self$degree_history_K_E[[iter]], 0.75)
        )
      }))
      
      (plt <- ggplot(degree_summary, aes(x = Iteration)) +
        geom_line(aes(y = Mean_K_S, color = "Mean K_S")) +
        geom_ribbon(aes(ymin = Q25_K_S, ymax = Q75_K_S, fill = "K_S"), alpha = 0.1) +
        geom_line(aes(y = Mean_K_E, color = "Mean K_E")) +
        geom_ribbon(aes(ymin = Q25_K_E, ymax = Q75_K_E, fill = "K_E"), alpha = 0.1) +
        scale_color_manual(values = c("Mean K_S" = "blue", "Mean K_E" = "red")) +
        scale_fill_manual(values = c("K_S" = "blue", "K_E" = "red")) +
        labs(title = "Degree Progress Over Iterations",
             x = "Iteration",
             y = "Degree",
             color = "Mean Degree",
             fill = "IQR (Mid-50%)") +
        theme_minimal()
      )
      
      # Calculate average degree (K) for the social space and component interaction space
      avg_degree_social <- mean(degree(self$get_social_igraph()))
      avg_degree_component <- mean(degree(self$get_component_igraph()))
      
      if (plot_save) {
        keystring <- sprintf("%s_sim%.0f_iter%.0f_N%d_M%d_BI_PROB_%.2f_K_S_%.2f_K_CC_%.2f", 
                             self$SIM_NAME, self$TIMESTAMP, self$ITERATION, self$N, self$M, self$BI_PROB, avg_degree_social, avg_degree_component)
        ggsave(filename = sprintf('Ks_degree_SAOM-NK_networks_%s_%s.png', keystring, self$TIMESTAMP), 
               plot = plt, height = 4.5, width = 9, units = 'in', dpi = 300)
      }
    },
    
    
    plot_bipartite_system_from_mat = function(bipartite_matrix, RSIENA_ITERATION, 
                                              plot_save = FALSE, return_plot=TRUE,
                                              normalize_degree = FALSE,
                                              initial_bipartite_matrix = NULL) {
      
      N <- ncol(bipartite_matrix)
      M <- nrow(bipartite_matrix)
      
      TS <- round(as.numeric(Sys.time()) * 100)
      
      # Generate labels for components in letter-number sequence
      generate_component_labels <- function(n) {
        letters <- LETTERS  # Uppercase alphabet letters
        if (n <= 26)
          return(letters[1:n])
        labels <- c()
        repeat_count <- ceiling(n / length(letters))
        for (i in 1:repeat_count) {
          labels <- c(labels, paste0(letters, i))
        }
        return(labels[1:n])  # Return only as many as needed
      }
      
      component_labels <- generate_component_labels(self$N)
      
      # Determine which components are "new" (initially isolated) vs "old" (initially connected)
      if (!is.null(initial_bipartite_matrix)) {
        # Use provided initial matrix
        component_initial_degrees <- colSums(initial_bipartite_matrix)
        component_is_new <- component_initial_degrees == 0
      } else if (!is.null(self$bipartite_matrix_init)) {
        # Use stored initial matrix from self
        component_initial_degrees <- colSums(self$bipartite_matrix_init)
        component_is_new <- component_initial_degrees == 0
      } else {
        # Default: all components are "old" if we can't determine
        component_is_new <- rep(FALSE, N)
        warning("No initial bipartite matrix found - unable to determine new vs old components")
      }
      
      # Get actor strategies for coloring
      actor_strategies <- self$get_actor_strategies()  # Assuming this contains the strategy levels

      n_strat_levels <- length(levels(actor_strategies))
      if (n_strat_levels == 0) n_strat_levels <- 1
      actor_colors <- scales::hue_pal()(n_strat_levels)
      
      # Map normalized strategies to colors
      
      # 1. Bipartite network plot using ggraph with vertex labels
      ig_bipartite <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = FALSE, weighted = TRUE)
      
      projs <- igraph::bipartite_projection(ig_bipartite, multiplicity = TRUE, which = 'both')
      ig_social <- projs$proj1
      ig_component <- projs$proj2
      
      # Set node attributes for shape and label
      V(ig_bipartite)$shape <- ifelse(V(ig_bipartite)$type, "square", "circle")
      
      # Create color vector for all vertices
      vertex_colors <- character(vcount(ig_bipartite))
      
      # Assign actor colors based on strategy (first M vertices)
      vertex_colors[1:M] <- actor_colors
      
      # Assign component colors based on new/old status (next N vertices)
      component_colors <- ifelse(component_is_new, 'darkgreen', 'tan')  # Bright green for new, sky blue for old
      vertex_colors[(M+1):(M+N)] <- component_colors
      
      # Assign colors to vertices
      V(ig_bipartite)$color <- vertex_colors
      
      # Labels
      V(ig_bipartite)$label <- c(as.character(1:self$M), as.character(component_labels))
      
      # Count new and old components for subtitle
      n_new <- sum(component_is_new)
      n_old <- sum(!component_is_new)
      
      # Create subtitle with strategy info
      strategy_range_text <- paste(levels(self$get_actor_strategies()), collapse = ', ')
      
      bipartite_plot <- ggraph(ig_bipartite, layout = "fr") +
        geom_edge_link(color = "gray") +
        geom_node_point(aes(shape = shape, color = color), size = 6) +  # Increased size for visibility
        geom_node_text(aes(label = label), vjust = 0.5, hjust = 0.5, size = 3, color = "white") +  # White text for better contrast
        scale_shape_manual(values = c("circle" = 16, "square" = 15)) +
        scale_color_identity() +  # This uses the exact colors we assigned
        labs(title = "[DGP] Bipartite Environment",
             subtitle = sprintf("Actors (strategy: %s) and Components (%d old, %d new)", 
                                strategy_range_text, n_old, n_new)) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          # Add black border around panel
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          plot.margin = margin(5, 5, 5, 5, "pt"),
          plot.subtitle = element_text(size = 9, color = "gray40")
        )
      node_size <- igraph::degree(ig_social)  # Degree centrality for node size
      node_color <- igraph::eigen_centrality(ig_social)$vector  # Eigenvector centrality for node color
      node_text <- 1:self$M
      
      social_plot <- ggraph(ig_social, layout = "fr") +
        geom_edge_link(color = "gray") +
        geom_node_point(aes(size = node_size, color = node_color)) +
        geom_node_text(aes(label = node_text), vjust = 0.5, hjust = 0.5, size = 3, color='white') +
        scale_color_gradient(low = "green", high = "red") +
        labs(title = "[Proj1] Actor Social Network\n(common components)", 
             color = "Eigenvector\nCentrality", size = "Degree\nCentrality") +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
        ) +
        guides(
          color = guide_legend(order = 1, nrow = 2),
          size = guide_legend(order = 2, nrow = 2)
        )
      
      # 3. Component influence matrix heatmap
      component_matrix <- igraph::as_adjacency_matrix(ig_component, type = 'both', sparse = FALSE, attr = 'weight')
      
      component_df <- melt(component_matrix)
      colnames(component_df) <- c("Component1", "Component2", "Weight")
      
      # Replace component numbers with labels in the heatmap data frame
      component_df$Component1 <- factor(component_df$Component1, labels = component_labels)
      component_df$Component2 <- factor(component_df$Component2, labels = component_labels)
      
      heatmap_plot <- ggplot(component_df, aes(x = Component1, y = forcats::fct_rev(Component2), fill = Weight)) +
        geom_tile() +
        labs(title = "[Proj2] Component Heatmap\n(common actors)", x = "Component 1", y = "Component 2") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      if ( sum(component_df$Weight) == 0 ) {
        heatmap_plot <- heatmap_plot + scale_fill_gradient(low = "white", high = "white")
      } else {
        heatmap_plot <- heatmap_plot + scale_fill_gradient(low = "white", high = "red")
      }
      
      # Calculate average degree (K) for the social space and component interaction space
      avg_degree_social <- mean(igraph::degree(ig_social, mode = 'all', loops = FALSE, normalized = normalize_degree))
      avg_degree_component <- mean(igraph::degree(ig_component, mode = 'all', loops = FALSE, normalized = normalize_degree))
      density_current <- igraph::edge_density(ig_bipartite, loops = FALSE)
      
      # Create a main title using sprintf with simulation parameters
      main_title <- sprintf("Environment M=%s, N=%s:   Step = %s,  Density = %.2f, K_AA = %.2f, K_CC = %.2f", 
                            M, N,
                            RSIENA_ITERATION, density_current, avg_degree_social, avg_degree_component)
      
      # Arrange plots with a main title
      plt <- grid.arrange(
        social_plot, bipartite_plot, heatmap_plot, 
        ncol = 3,
        top = textGrob(main_title, gp = gpar(fontsize = 16, fontface = "bold"))
      )
      
      if(plot_save){
        keystring <- sprintf("%s_sim%.0f_iter%.0f_N%d_M%d_BI_PROB_%.2f_K_S_%.2f_K_CC_%.2f", 
                             self$SIM_NAME, TS, RSIENA_ITERATION, N, M, self$BI_PROB, avg_degree_social, avg_degree_component)
        ggsave(filename = sprintf('3plot_SAOM_NK_networks_%s_%s.png', keystring, TS), 
               plot = plt, height = 4.5, width = 9, units = 'in', dpi = 300)
      }
      
      if(return_plot)
        return(plt)
    },
  
  
  # Interactive visualization using visNetwork
  visNetwork_social = function() {
    ig_social <- graph_from_adjacency_matrix(as.matrix.network(self$social_network), mode = 'directed')
    vis_data <- toVisNetworkData(ig_social)
    
    visNetwork(nodes = vis_data$nodes, edges = vis_data$edges) %>%
      visNodes(shape = "dot", size = 10) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123)
  },
  
  # Evaluate interactions between social and search spaces using the bipartite matrix
  evaluate_interspace_interactions = function() {
    bipartite_matrix <- as.matrix.network(self$bipartite_igraph)
    actor_influence_on_components <- bipartite_matrix %*% as.matrix.network(self$search_landscape)
    total_interaction_score <- sum(actor_rounfluence_on_components)
    return(total_interaction_score)
  },


  plot_actor_utility = function(xints=c(), thin_factor=1, loess_span=0.5, return_plot=TRUE) {
    ## Individual actors
    actthin <- self$actor_util_df %>%  filter(chain_step_id %% thin_factor == 0)
    print(dim(actthin))
    tmpdf <-  actthin %>% mutate(actor_id=as.factor(actor_id))
    npoints <- nrow(self$chain_stats) * self$M
    point_size <- 6 / log10( npoints )
    point_alpha <- min( 1,  2.2/log( npoints ) )
    suppressMessages({
      plt.act <-  ggplot(aes(x=chain_step_id, y=utility, color=strategy), 
                         data=tmpdf) + # linetype=actor_id, color=strategy, point=actor_id, shape=actor_id
        geom_point(alpha=point_alpha, shape=1, size= point_size ) +
        geom_smooth(aes(fill=actor_id, linetype=actor_id), method='loess', span=loess_span, alpha=.05) +
        geom_smooth(aes(x=chain_step_id, y=mean), 
                    data=actthin %>% group_by(chain_step_id,actor_id) %>% dplyr::summarize(mean=mean(utility, na.rm=TRUE)),
                    method='loess', color='black', span=loess_span, alpha=.05, linewidth=1.1) +
        geom_hline(yintercept = 0, linetype=4, color='black') +
        theme_bw()
    })
    
    if (length(xints)) 
      plt.act <- plt.act + geom_vline(xintercept=xints, linetype=1, color='black') 
    
    if (return_plot)
      return(plt.act)
  },
  
  plot_strategy_utility = function(xints=c(), thin_factor=1, loess_span=0.5, return_plot=TRUE) {
    actthin <- self$actor_util_df %>%  filter(chain_step_id %% thin_factor == 0)
    print(dim(actthin))
    tmpdf <-  actthin %>% mutate(actor_id=actor_id)
    npoints <- nrow(self$chain_stats) * self$M
    point_size <- 4 / log10( npoints )
    point_alpha <- min( 1,  1/log( npoints ) )
    suppressMessages({
      plt.act <- tmpdf %>%       #ungroup() %>%
        ggplot(aes(x=chain_step_id, y=utility)) +  #
        geom_point(aes(shape=actor_id, color=strategy), alpha=point_size, shape=1, size= point_size) +
        geom_smooth(aes(fill=strategy, color=strategy), 
                    method='loess', span=loess_span, alpha=.1) +
        geom_smooth(aes(x=chain_step_id, y=mean, color=strategy), 
                    method='loess', color='black', span=loess_span, alpha=.05, linewidth=1.1,
                    data=actthin %>% group_by(chain_step_id) %>% dplyr::summarize(mean=mean(utility, na.rm=TRUE)) %>% 
                      mutate(strategy=NA)) +
        geom_hline(yintercept = 0, linetype=4, color='black') +
        ggtitle("Average Utility by Strategy") +
        theme_bw()
    })
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt.act)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks) %>%
        mutate(utility=0, chain_step_id=0, effect_name=NULL, effect=NULL)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
      plt.act <- plt.act + 
        geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='darkorange', color='orange',linetype=2, alpha=.08) + 
        geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                  vjust = 1, size = 3) #fontface = "bold"
    }
    
    
    if (return_plot)
      return(plt.act)
    
  },
  
  
  expand_names = function(named_list) {
    names_vec <- names(named_list)
    counts <- unlist(named_list)
    
    # Repeat names according to counts
    expanded <- rep(names_vec, counts)
    
    # Add suffixes where count > 1
    suffixes <- unlist(lapply(counts, function(n) if(n > 1) seq_len(n) else NA))
    needs_suffix <- !is.na(suffixes)
    
    expanded[needs_suffix] <- paste0(expanded[needs_suffix], "_", suffixes[needs_suffix])
    
    return(expanded)
  },
  
  get_structure_model_params = function(){
    structure_model <- self$config_structure_model
    dv_bipartite <- structure_model$dv_bipartite
    dv_name <- dv_bipartite$name    
    ## ADD reuse_interaction to coCovars and coDyadCovars (for effects like totInDist2 that interact with strategy covariate)
    if (length(dv_bipartite$coCovars)) {
      for (i in seq_along(dv_bipartite$coCovars)) {
        if (!is.null(dv_bipartite$coCovars[[i]]$reuse_interaction1)) {
          dv_bipartite$coCovars[[i]]$interaction1 <- dv_bipartite$coCovars[[i]]$reuse_interaction1
        }
      }
    }
    if (length(dv_bipartite$coDyadCovars)) {
      for (i in seq_along(dv_bipartite$coDyadCovars)) {
        if (!is.null(dv_bipartite$coDyadCovars[[i]]$reuse_interaction1)) {
          dv_bipartite$coDyadCovars[[i]]$interaction1 <- dv_bipartite$coDyadCovars[[i]]$reuse_interaction1
        }
      }
    }
    ##==================================================
    efflist <- c(
      dv_bipartite$effects,
      dv_bipartite$coCovars,
      dv_bipartite$varCovars,
      dv_bipartite$coDyadCovars,
      dv_bipartite$varDyadCovars,
      dv_bipartite$interactions
    )
    ## Guard: return empty params if no effects defined
    if (length(efflist) == 0) {
      return(list(
        structeffs = numeric(0),
        covs = numeric(0),
        component_type_ids = integer(0),
        strat_type_ids = integer(0),
        interact_type_ids = integer(0)
      ))
    }
    ## process effect level names for duplicates
    duplist <- list()
    for (i in seq_along(efflist)) {
      eff_name <- efflist[[i]]$effect
      if (is.null(eff_name) || !nzchar(as.character(eff_name))) {
        warning(sprintf("Effect at index %d has no name, skipping", i))
        next
      }
      duplist[[ eff_name ]] <- ifelse( is.null(duplist[[ eff_name ]]),
                                                  1,
                                                  duplist[[ eff_name ]] + 1  )
    }
    effnames <- self$expand_names(duplist)
    neffs <- length(efflist)
    ### empty matrix to hold actor network statistics
    effparams <- sapply(efflist, function(x) x$parameter, simplify = TRUE)
    names(effparams) <- effnames
    ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
    COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
    if (! any(c('coCovars','coDyadCovars') %in% names(dv_bipartite)) ) {
      stop('dv_bipartite has no coCovars or coDyadCovars. Check structure_model.')
    }
    structeffs <- sapply(dv_bipartite$effects, function(x)x$parameter)
    names(structeffs) <- sapply(dv_bipartite$effects, function(x)x$effect)
    coCovars     <- sapply(dv_bipartite$coCovars, function(x)x$effect)
    varCovars     <- sapply(dv_bipartite$varCovars, function(x)x$effect)
    coDyadCovars  <- sapply(dv_bipartite$coDyadCovars, function(x)x$effect)
    varDyadCovars <- sapply(dv_bipartite$varDyadCovars, function(x)x$effect)
    interactions <- sapply(dv_bipartite$interactions, function(x)x$effect)
    coCovars_param     <- sapply(dv_bipartite$coCovars, function(x)x$parameter)
    varCovars_param     <- sapply(dv_bipartite$varCovars, function(x)x$parameter)
    coDyadCovars_param  <- sapply(dv_bipartite$coDyadCovars, function(x)x$parameter)
    varDyadCovars_param <- sapply(dv_bipartite$varDyadCovars, function(x)x$parameter)
    interactions_param <- sapply(dv_bipartite$interactions, function(x)x$parameter)
    covs <- unlist(c(coCovars_param, varCovars_param, coDyadCovars_param,  varDyadCovars_param, interactions_param))
    names(covs) <- unlist(c(coCovars, varCovars, coDyadCovars, varDyadCovars, interactions))
    coCovarTypes     <- sapply(dv_bipartite$coCovars, function(x)x$interaction1)
    varCovarTypes     <- sapply(dv_bipartite$varCovars, function(x)x$interaction1)
    coDyadCovarTypes  <- sapply(dv_bipartite$coDyadCovars, function(x)x$interaction1)
    varDyadCovarTypes <- sapply(dv_bipartite$varDyadCovars, function(x)x$interaction1)
    vartypes <- unlist(c(coCovarTypes, varCovarTypes, coDyadCovarTypes, varDyadCovarTypes))
    component_type_ids <-  grep('self\\$component.+var', vartypes)
    strat_type_ids <-  grep('self\\$strat.+var', vartypes)
    exclude_ids <- unique(c(component_type_ids, strat_type_ids))
    interact_type_ids <- if (length(covs) == 0) {
      integer(0)
    } else if (length(exclude_ids) == 0) {
      seq_along(covs)
    } else {
      seq_along(covs)[ -exclude_ids ]
    }
    return(list(
      structeffs=structeffs,
      # Covariates
      covs = effparams[ which( ! names(effparams) %in% names(structeffs))],
      component_type_ids=component_type_ids,
      strat_type_ids=strat_type_ids,
      interact_type_ids=interact_type_ids
    ))
  },
  
  get_structure_model_param_str = function(params=NULL) {
    if(is.null(params)) {
      params <- self$get_structure_model_params()
    }
    structeffs <- params$structeffs
    covs <- params$covs
    component_type_ids <- params$component_type_ids
    strat_type_ids <- params$strat_type_ids
    interact_type_ids <- params$interact_type_ids
    structeffs_str <- paste( paste(paste(names(structeffs), structeffs, sep='= '), sep=''), collapse = ';  ')
    components_str <- paste( paste(paste(names(covs)[component_type_ids], covs[component_type_ids], sep='= '), sep=''), collapse = ';  ')
    strategies_str <- ifelse(
      length(strat_type_ids),
      paste( paste(paste(names(covs)[strat_type_ids], covs[strat_type_ids], sep='= '), sep='' ), collapse = ';  '),
      '   ( none )'
    )
    interactions_str <- ifelse(
      length(interact_type_ids),
      sprintf('\nInteractions: %s',paste( paste(paste(names(covs)[interact_type_ids], covs[interact_type_ids], sep='= '), sep='' ), collapse = ';  ')), 
      ''
    )
    shocks_str <- ''
    if (!is.null(self$theta_shocks)) {
      theta_shocks <- self$theta_shocks
      theta_shocks_on <- theta_shocks[which(sapply(theta_shocks, function(x)x$shock_on==1))]
      shocks_str <- ifelse(
        !is.null(theta_shocks),
        sprintf('\nShocks: %s', paste(
          paste(sapply(theta_shocks_on, function(x) ifelse(is.null(x$label), '', x$label ) ),
                sapply(theta_shocks_on, function(x) sprintf('{%s}',paste(paste(x$effect_level, x$parameter, sep='='), collapse = '; '))),
                sep=' '
          ), 
          collapse='\n              '##padding below "Shocks: "
        )),
        ''
      )
    }
    sim_title_str <- sprintf(
      'Environment: Actors (M) = %s, Components (N) = %s, Init.P. = %.2f\nStructure:  %s\nComponent Values: %s\nActor Strategies:  %s%s%s', 
      self$M, self$N, self$BI_PROB,
      structeffs_str,  components_str, strategies_str,  interactions_str,  shocks_str
    )
    return(sim_title_str)
  },
  
  
  find_constant_spans_in_vec = function(vec) {
    # Find where values change
    change_points <- c(TRUE, vec[-1] != vec[-length(vec)], TRUE)
    # Get start and end indices of each run
    starts <- which(change_points[-length(change_points)])
    ends <- which(change_points[-1]) + 1 #- 1
    is_shocked <- length(starts) > 1
    span_id <- if(is_shocked) { 1:length(starts) } else { NA }
    # Store results in a data frame
    result <- data.frame(value = vec[starts], 
                         start = starts,
                         end = ends, 
                         is_shocked = is_shocked,
                         span_id = span_id)
    return(result)
  },
  
  get_theta_shock_rects_df = function(theta_shocks, filter_shocks_on=TRUE) {
    if (is.null(self$rsiena_model) || is.null(self$rsiena_model$thetaUsed))
      stop('rsiena_model not set or thetaUsed missing from model. Check siena07 function call.')
    theta_matrix <- self$rsiena_model$thetaUsed
    if(is.null(theta_shocks[[1]]$chain_step_ids)){
      theta_shocks <- self$preprocess_theta_shocks(theta_shocks, nrow(theta_matrix))
    }
    row_ids <- unlist(sapply(theta_shocks, function(x)x$chain_step_ids))
    if ( ! length(intersect(row_ids, 1:nrow(theta_matrix))) == nrow(theta_matrix) ) {
      stop('chain_step_ids in theta_shocks list do not cover all rows of thetaUsed=theta_matrix.')
    }
    if(filter_shocks_on)
      theta_shocks <- theta_shocks[ sapply(theta_shocks,function(x)x$shock_on==1) ]
    theta_shock_df <- theta_shocks %>% ldply(.fun = function(x){
      first_step <-  min(x$chain_step_ids, na.rm=TRUE)
      last_step <-  max(x$chain_step_ids, na.rm=TRUE)
      shock_label <- ifelse( !is.null(x$label), 
                             x$label,  
                             paste(paste(x$effect, x$parameter, sep='='), collapse = '; ') )
      data.frame(
        effect_all = paste(x$effect,collapse = ';'),
        effect_level_all = paste(x$effect_level,collapse = ';'),
        parameter=paste(x$parameter,collapse = '|'), 
        portion=paste(x$portion,collapse = '|'), 
        shock_on=paste(x$shock_on,collapse = '|'), 
        start = first_step,
        end = ifelse(last_step==nrow(theta_matrix), last_step, last_step + 1),
        first_chain_step_id = first_step,
        last_chain_step_id = last_step,
        label = shock_label
      )
    }, .id = 'span_id')
    
    return( theta_shock_df )
  },
  
  
  plot_utility_contributions_basic = function(use_thetas=TRUE, loess_span=0.5,
                                              return_plot=TRUE, save_plot=FALSE,
                                              thin_factor=1) {
    
    theta_df_norates <- self$get_rsiena_effects_theta_df(no_rates=TRUE)
    theta_levels_norate <-  theta_df_norates$effect_level
    sim_title_str <- self$get_structure_model_param_str()
    efflvls <- c('UTILITY', theta_levels_norate )
    actor_stats_df <- self$actor_stats_df
    actor_util_df <- self$actor_util_df  %>% mutate(
      effect_id=NA, 
      value=utility, 
      effect_name='UTILITY', 
      effect_level='UTILITY', 
      utility=NULL
    )
    
    plt_title <- sprintf('Actor Utility Statistics Decomposition:\n%s', sim_title_str)
    
    ## use actor stats contributions to utility instead of original stats
    if (use_thetas) {
      actor_stats_df <- actor_stats_df %>% mutate(value=value_contributions)
      plt_title <- sprintf('Actor Utility Contributions (statistic * theta):\n%s', sim_title_str)
    } 
    
    ## Add utility as extra 'effect' 
    act_effs <- actor_stats_df %>% bind_rows( actor_util_df ) 
    act_effs$effect_level <- factor(act_effs$effect_level, levels=efflvls)
    
    #plot signals
    act_effs2 <- act_effs %>% filter(chain_step_id %% thin_factor == 0)
    
    npoints <- nrow(self$chain_stats) * self$M
    point_size <- 4 / log10( npoints )
    point_alpha <- min( 1,  15/sqrt( npoints ) )
    
    plt2 <- act_effs2  %>%  ggplot(aes(x=chain_step_id, y=value, linetype=actor_id)) 
    
    suppressMessages({
      plt2 <- plt2 + 
        geom_point(alpha=point_alpha, shape=1, size= point_size )  + 
        geom_smooth(method='loess', alpha=.1, span=loess_span) +
        geom_hline(yintercept = 0, linetype=2 ) +
        facet_grid(effect_level ~ ., scales='free_y') +
        theme_bw() + theme(legend.position = 'bottom') +
        ggtitle(plt_title)
    })
    
    # plt2  
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt2)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks)%>%mutate(value=0, chain_step_id=0)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
      plt2 <- plt2 + geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                               fill='darkorange', color='orange',linetype=2,  alpha=.05)
      plt2 <- plt2 + geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                               vjust = 1, size = 2.7) #fontface = "bold"
    }
    
    plotname2 <- sprintf('plot_actor_utility_components_thin%s_%s.png',thin_factor, round(as.numeric(Sys.time())*100) )
    
    if (save_plot) {
      plot_dir <- getwd()
      ggsave(filename=file.path(plot_dir, plotname2), plt2, 
             height = 12, width = 8, dpi = 600, units = 'in')
    }
    
    if (return_plot)
      return(plt2)
    
  },
  
  
  get_actor_utility_effects = function(use_thetas=TRUE, 
                                       thin_factor = 1,
                                       thin_pct = 1,
                                       experiment = '') {
    
    if(is.null(self$get_actor_strategies()))
      return(self$plot_utility_contributions_basic(use_thetas = use_thetas,  
                                                   loess_span = loess_span,
                                                   return_plot= return_plot,
                                                   save_plot = save_plot))
    theta_df_norates <- self$get_rsiena_effects_theta_df(no_rates=TRUE)
    theta_levels_norate <-  theta_df_norates$effect_level
    sim_title_str <- self$get_structure_model_param_str()
    efflvls <- c('UTILITY', theta_levels_norate )
    actor_strats <- self$get_actor_strategies() 
    actor_stats_df <- if ( !is.null(self$experiments) &&  experiment %in% names(self$experiments) ) {
      self$experiments[[experiment]]$stats_df
    } else {
      self$actor_stats_df
    }
    actor_stats_df$strategy <- NA
    if (length(actor_strats))
      actor_stats_df$strategy <- actor_strats[ actor_stats_df$actor_id ]
    
    util_df <- if ( !is.null(self$experiments) &&  experiment %in% names(self$experiments) ) {
      self$experiments[[experiment]]$util_df
    } else {
      self$actor_util_df
    }
    actor_util_df <- util_df  %>% mutate(
      effect_id=NA, 
      value=utility, 
      effect_name='UTILITY', 
      effect_level='UTILITY', 
      strategy= actor_strats[ actor_id ], 
      utility=NULL
    )
    
    plt_title <- sprintf('Actor Utility Statistics Decomposition:\n%s', sim_title_str)
    
    ## use actor stats contributions to utility instead of original stats
    if (use_thetas) {
      actor_stats_df <- actor_stats_df %>% mutate(value=value_contributions)
      plt_title <- sprintf('Actor Utility Contributions (statistic * theta):\n%s', sim_title_str)
    } 
    
    ## Add utility as extra 'effect' 
    act_effs <- actor_stats_df %>% bind_rows( actor_util_df ) 
    act_effs$effect_level <- factor(act_effs$effect_level, levels=efflvls)
    
    #plot signals
    act_effs2 <- act_effs %>% filter(chain_step_id %% thin_factor == 0)
    
    if (thin_pct < 1) {
      sample_rows <- sample(1:nrow(act_effs2), size = round(nrow(act_effs2)*thin_pct), replace = FALSE )
      act_effs2 <- act_effs2 %>% filter(row_number() %in% sample_rows )
    }
    
    
    return(act_effs2)
  },
  

  plot_utility_contributions = function(use_thetas=TRUE, 
                                        loess_span=0.5,
                                        plot_return=TRUE, 
                                        plot_save=FALSE,
                                        plot_file='',
                                        plot_dir=NA,
                                        hide_zeros=FALSE,
                                        thin_factor = 1,
                                        thin_pct = 1,
                                        point_alpha_dimmer=1,
                                        experiment = '') {
    
    theta_df_norates <- self$get_rsiena_effects_theta_df(no_rates=TRUE)
    theta_levels_norate <-  theta_df_norates$effect_level
    # #
    sim_title_str <- self$get_structure_model_param_str()
    # #
    efflvls <- c('UTILITY', theta_levels_norate )
    # #
    actor_strats <- self$get_actor_strategies()
    
    plt_title <- sprintf('Actor Utility Statistics Decomposition:\n%s', sim_title_str)
    # ## use actor stats contributions to utility instead of original stats
    if (use_thetas) {
      plt_title <- sprintf('Actor Utility Contributions (statistic * theta):\n%s', sim_title_str)
    }
    
    act_effs2 <- self$get_actor_utility_effects(use_thetas=use_thetas, 
                                                thin_factor = thin_factor,
                                                thin_pct = thin_pct,
                                                experiment = experiment)
    
    if (hide_zeros) {
      act_effs2 <- act_effs2 %>% group_by(effect_level) %>% filter(any(value != 0)) %>% ungroup() 
      act_effs2$effect_level <- droplevels(act_effs2$effect_level)
    }
    
    npoints <- nrow(act_effs2) 
    point_size <- 4 / log10( npoints )
    point_alpha <- min( 1,  15/sqrt( npoints ) ) * point_alpha_dimmer
    
    plt2 <- act_effs2  %>%  ggplot(aes(x=chain_step_id, y=value)) 
    
    suppressMessages({
      plt2 <- plt2 + 
          geom_point(aes(color=strategy,fill=strategy), alpha=point_alpha, shape=1, size= point_size )  + 
          geom_smooth(aes(color=strategy,fill=strategy, linetype=strategy), method='loess', alpha=.1, span=loess_span) +
          geom_hline(yintercept = 0, linetype=2, ) +
          facet_grid(effect_level ~ ., scales='free_y') +
          theme_bw() + theme(legend.position = 'bottom') +
          ggtitle(plt_title)
    })
    
    # plt2  
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt2)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks)%>%mutate(value=0, chain_step_id=0)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
      plt2 <- plt2 + geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                               fill='darkorange', color='orange',linetype=2,  alpha=.05)
      plt2 <- plt2 + geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                              vjust = 1, size = 2.7) #fontface = "bold"
    }
  
    
    if(plot_save) {
      nfacets <- length(efflvls)
      plot_file <- paste0('util_contribs_',plot_file, round(as.numeric(Sys.time())*10))
      ggsave(filename = file.path(ifelse(is.na(plot_dir)||plot_dir=='',getwd(),plot_dir), 
                              sprintf("%s_%s.jpeg", self$config_environ_params$name, plot_file)),
             plt2,
             width = 8, height = 2 + 1.2*nfacets, units = 'in', dpi = 400)
    }
    
    if (plot_return)
      return(plt2)
  },
  
  
  get_component_strategy_by_actors = function(actor_strats, tiebreak='rand') {
    cnts <- plyr::count(actor_strats)
    cntsmax <- cnts[which(cnts$freq == max(cnts$freq)), ]
    strategy <- NA
    if (tiebreak == 'rand') {
      
      set.seed(self$rsiena_env_seed)
      sample_id <- sample(1:nrow(cntsmax), 1)
      strategy <- cntsmax$x[sample_id] 
      
    } else {
      stop('tiebreak not supported')
    }
    return(strategy)
  },
  
  
  search_rsiena_model_summary = function(digits=3, single.row = TRUE) {
    if (is.null(self$rsiena_model))
      stop('No rsiena_model to summarize')
    
    regtab <- screenreg(self$rsiena_model, digits = digits, single.row = single.row )
    
    return(regtab)  
  },
  
  
  plot_degree_4panel = function(loess_span=0.5, 
                                plot_return=TRUE, 
                                plot_save=FALSE,
                                plot_file='',
                                plot_dir=NA,
                                thin_factor=1,
                                thin_pct = 1,
                                point_alpha_dimmer = 1,
                                experiment='') {
    sim_title_str <- self$get_structure_model_param_str()
    
    actor_strats <- self$get_actor_strategies()
    avg_mat <- apply(self$bi_env_arr, c(1,2), mean)
    component_actor_strats <- actor_strats[ apply(avg_mat, 2, which.max) ]
   
    Kdf <- if ( !is.null(self$experiments) &&  experiment %in% names(self$experiments) ) {
      self$experiments[[experiment]]$K4_df
    } else {
      self$get_K4_df()
    }
    
    Kdf <- Kdf %>% filter(chain_step_id %% thin_factor == 0)

    
    suppressMessages({
      Klabels_df <- Kdf %>% group_by(panel_label, panel_label_text, node_type, dyad_type) %>% dplyr::summarize(n=n())
    })
    Klabels_df$panel_label[which(Klabels_df$node_type == 'Actor'      & Klabels_df$dyad_type == '2-mode  (bipartite)' )]  <- 'K_AC'
    Klabels_df$panel_label[which(Klabels_df$node_type == 'Component'  & Klabels_df$dyad_type == '2-mode  (bipartite)' )]  <- 'K_CA'
    
    if (thin_pct < 1) {
      sample_rows <- sample(1:nrow(Kdf), size = round(nrow(Kdf)*thin_pct), replace = FALSE )
      Kdf <- Kdf %>% filter(row_number() %in% sample_rows )
    }
    
    npoints <- nrow(Kdf)
    point_size <- 6 / log10( npoints )
    point_alpha <- min( 1,  15/sqrt( npoints ) ) * point_alpha_dimmer
    
    suppressMessages({
      plt <- Kdf %>% 
        ggplot(aes(x=chain_step_id, y=value)) +
        geom_point(aes(fill=node_group, color=node_group), 
                   pch=1, alpha=point_alpha, size=point_size) + 
        geom_smooth(aes(group=node_group, color=node_group, fill=node_group), ##**node_group** to color actors by strategy but components by node
                    method='loess', alpha=.05, span=loess_span) + 
        geom_smooth(aes(x=chain_step_id, y=mean), span=loess_span, 
                    data=Kdf %>% group_by(chain_step_id, node_type, dyad_type) %>% dplyr::summarize(mean=mean(value)),
                    method='loess', se=FALSE, color='black', linewidth=1) +
        geom_text(data=Klabels_df, aes(label=panel_label, x=Inf, y=-Inf), hjust=1.15, vjust=-.5, size=7, color='black', fontface='bold') +
        geom_hline(yintercept = 0, linetype=2) +
        scale_y_continuous(position = 'right') +
        facet_grid(  dyad_type ~ node_type,  switch = 'y') +
        theme_bw() + theme(strip.placement = 'inside', legend.position = 'bottom') +
        labs(group='Strategy', fill='Strategy',color='Strategy') +
        ylab('Node Degree') +
        ggtitle(sprintf('Actor and Component Degrees: K_AA, K_AC, K_CA, K_CC\n%s', sim_title_str))
    })
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt)$layout
        shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks) %>%
          mutate(value=0, chain_step_id=0, effect_name=NULL, effect=NULL)
        y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
        plt <- plt + 
          geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='darkorange', color='orange',linetype=2,  alpha=.05) + 
          geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                    vjust = 1, size = 3) #fontface = "bold"
      })
    }
    
    if (is.null(actor_strats))
      plt <- plt + theme(legend.position = 'none') 
    
    
    if(plot_save) {
      plot_file <- paste0('K4panel_',plot_file, round(as.numeric(Sys.time())*10))
      ggsave(filename = file.path(ifelse(is.na(plot_dir)||plot_dir=='',getwd(),plot_dir), 
                              sprintf("%s_%s.jpeg", self$config_environ_params$name, plot_file)),
             plt,
             width = 8, height = 8, units = 'in', dpi = 400)
    }
    
    
    if (plot_return)
      return(plt)
    
  }, 
  
  
  plot_component_degrees = function(loess_span=0.5, return_plot=TRUE) {
    
    Kdf2 <- self$K_CA_df %>% mutate(effect='K_CA') %>% 
      bind_rows(
        self$K_CC_df %>% mutate(effect='K_CC')
      ) 
    
    npoints <- nrow(self$chain_stats) * self$M
    point_size <- 6 / log10( npoints )
    point_alpha <- min( 1,  2.2/log( npoints ) )
    
    suppressMessages({
      plt <- Kdf2 %>% 
        ggplot(aes(x=chain_step_id, y=value)) +
        geom_point(aes(fill=component_id, color=component_id), 
                   pch=1, alpha=point_alpha, size=point_size) + 
        geom_smooth(aes(fill=component_id, color=component_id, linetype=component_id), 
                    method='loess', alpha=.1, span=loess_span) + 
        geom_smooth(aes(x=chain_step_id, y=mean), span=loess_span, 
                    data=Kdf2 %>% group_by(chain_step_id, effect) %>% dplyr::summarize(mean=mean(value)),
                    method='loess', se=FALSE, color='black', linewidth=1) +
        geom_hline(yintercept = 0, linetype=2) +
        facet_grid( effect ~ . ) +
        theme_bw() +
        ggtitle('Component Degrees: K_CA, K_CC')
    })
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks) %>%
        mutate(value=0, chain_step_id=0, effect_name=NULL, effect=NULL)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
      plt <- plt + 
        geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='darkorange', color='orange',linetype=2,  alpha=.05) + 
        geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                  vjust = 1, size = 3) #fontface = "bold"
    }
    
    if (return_plot)
      return(plt)
    
  }, 
  
  plot_actor_degrees = function(loess_span=0.5, return_plot=TRUE) {
    
    Kdf1 <- self$K_AA_df %>% mutate(effect='K_AA', node_id=actor_id) %>% 
      bind_rows(
        self$K_AC_df %>% mutate(effect='K_AC', node_id=actor_id)
      )
    
    npoints <- nrow(self$chain_stats) * self$M
    point_size <- 6 / log10( npoints )
    point_alpha <- min( 1,  2.2/log( npoints ) )
    
    suppressMessages({
      plt <- Kdf1 %>% 
        ggplot(aes(x=chain_step_id, y=value)) +
        # stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", size = 1) +
        geom_point(aes(color=strategy, fill=strategy), 
                   pch=1, alpha=point_alpha, size=point_size) + 
        geom_smooth(aes(color=strategy, fill=strategy, linetype=actor_id),
                    method='loess', alpha=.1, span=loess_span) + 
        geom_smooth(aes(x=chain_step_id, y=mean), span=loess_span, 
                    data=Kdf1 %>% group_by(chain_step_id, effect) %>% dplyr::summarize(mean=mean(value)),
                    method='loess', se=FALSE, color='black', linewidth=1) +
        geom_hline(yintercept = 0, linetype=2) +
        facet_grid( effect ~ . ) +
        theme_bw() + 
        ggtitle('Actor Degrees: K_AA, K_AC')
    })
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks) %>%
        mutate(value=0, chain_step_id=0, effect_name=NULL, effect=NULL)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
      plt <- plt + 
        geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='darkorange', color='orange',linetype=2,  alpha=.05) + 
        geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                  vjust = 1, size = 3) #fontface = "bold"
    }

    if (return_plot)
      return(plt)
  }, 
  
  
  plot_actor_utility_strategy_summary = function(actor_ids=c(), 
                                                 thin_factor=1, 
                                                 thin_pct=1,
                                                 thin_wave_factor=1,
                                                 smooth_method='loess',  ##"lm", "glm", "gam", "loess","auto"
                                                 show_utility_points=TRUE,
                                                 scale_utility=TRUE,
                                                 plot_return = TRUE,
                                                 plot_save = FALSE,
                                                 plot_file='', 
                                                 plot_dir=NA,
                                                 loess_span=0.5,
                                                 point_alpha_dimmer = 1,
                                                 ylim=NULL,
                                                 experiment=''
  ) {
    structure_model <- self$config_structure_model
    
    ## actor strategy
    if ( !identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
      stop("Actor Strategy self$strat_1_coCovar not set.")
    actor_strat <- self$get_actor_strategies() 
    efflist <- c(
      structure_model$dv_bipartite$effects,
      structure_model$dv_bipartite$coCovars,
      structure_model$dv_bipartite$varCovars,
      structure_model$dv_bipartite$coDyadCovars,
      structure_model$dv_bipartite$varDyadCovars,
      structure_model$dv_bipartite$interactions
    )
    
    
    dv_bipartite <- self$config_structure_model$dv_bipartite
    dv_name <- dv_bipartite$name    
    
    ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
    COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
    if (! 'coCovars' %in% names(dv_bipartite) ) {
      stop('dv_bipartite has no coCovars. Check structure_model.')
    }
    
    
    params <- self$get_structure_model_params()
    structeffs <- params$structeffs
    covs <- params$covs
    component_type_ids <- params$component_type_ids
    strat_type_ids <- params$strat_type_ids
    interact_type_ids <- params$interact_type_ids
    inputeffs <- c(structeffs, covs)
    ## Theta-storage convention (2026-08-23): coefficients live in
    ## `initialValue`, never in `parm` (RSiena's internal '#' parameter).
    modeleffs <- self$rsiena_effects$initialValue[self$rsiena_effects$include]
    names(modeleffs) <-  self$rsiena_effects$shortName[self$rsiena_effects$include]
    sim_title_str <- self$get_structure_model_param_str(params)
    efflvls <- c('utility', 
                 names(structeffs),
                 names(covs)[component_type_ids],
                 names(covs)[strat_type_ids], 
                 names(covs)[interact_type_ids]
    )
    nstep <- sum(!self$chain_stats$stability)
    ##----------------------------------
    
    
    ## use experiment MC samples if experiment %in% c('market_entry', 'market_exit') 
    util_df <- if ( !is.null(self$experiments) &&  experiment %in% names(self$experiments) ) {
      
      self$experiments[[experiment]]$util_df
      
    } else {
      
      self$actor_util_df
      
    }
    
    ## Compare 2 actors utilty
    dat <- util_df %>% 
      filter(chain_step_id %% thin_factor == 0) %>% 
      mutate(
        strategy = actor_strat[ actor_id ],
        chain_below_med =  chain_step_id < median(chain_step_id)
      )
    dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
    util_lab <- 'Actor Utility'
    if(scale_utility) {
      util_sc <- scale(dat$utility)
      util_lab <- sprintf('Actor Utility\n(Standardized Center = %.2f; Scale = %.2f)',
                          attr(util_sc, 'scaled:center'), 
                          attr(util_sc, 'scaled:scale'))
      if (!all(dat$utility == 0))
        dat <- dat %>% mutate(utility = c(scale(utility)))
    }
    if (thin_pct < 1) {
      sample_rows <- sample(1:nrow(dat), size = round(nrow(dat)*thin_pct), replace = FALSE )
      dat <- dat %>% filter(row_number() %in% sample_rows )
    }
    npoints <- nrow(dat)
    density_rng <- range(dat$utility, na.rm=TRUE)
    density_absdiff_scale <- abs(diff(density_rng)) * 0.15
    util_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
    ylim <- if(!is.null(ylim)){ ylim } else { util_lim }
    point_size <- 6 / log10( npoints )
    point_alpha <- min( 1,  2.2/log( npoints ) ) * point_alpha_dimmer
    if(length(actor_ids))
      dat <- dat %>% filter(actor_id %in% actor_ids)
    plt <- ggplot(dat, aes(x=chain_step_id, y=utility)) + 
      geom_hline(yintercept = dat$utility%>%mean(), linetype=3, col='black' ) 
    if(show_utility_points)
      plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)  # geom_line(alpha=.2) +#geom_smooth(method='loess', alpha=.1) + 
    if(self$exists(smooth_method))
      plt <- plt + geom_smooth(aes(color=strategy, fill=strategy, linetype=actor_id), 
                               method = smooth_method, span=loess_span, linewidth=1, alpha=.09)
    # Population Mean in black
    plt <- plt + geom_smooth(aes(x=chain_step_id, y=utility_mean), method = smooth_method, span=loess_span, 
                             data=dat %>% group_by(chain_step_id)%>%dplyr::summarize(utility_mean=mean(utility)),
                             color='black', linetype=1, alpha=.1) 
    
    
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks) %>%
        mutate(utility=0, chain_step_id=0, effect_name=NULL, effect=NULL)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(  max(x$y.range),  nrow(shock_rects)) ))
      plt <- plt + 
        geom_rect(data=shock_rects, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='darkorange', color='orange',linetype=2,  alpha=.05) + 
        geom_text(data = shock_rects, aes(x = (start + end) / 2, y = y_maxs, label = label),
                  vjust = 0, size = 3) #fontface = "bold"
    }
      
    plt <- plt + theme_bw() + 
      ylim(ylim) + 
      ylab(util_lab) +
      xlab('Actor Decision Chain Ministep') +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.key.width = unit(0.8, "cm"),  # Adjust legend key width
        legend.spacing.x = unit(0.3, "cm")   # Adjust spacing between keys
      )
    
    ACTORS     <- sienaNodeSet(self$M, nodeSetName="ACTORS")
    COMPONENTS <- sienaNodeSet(self$N, nodeSetName="COMPONENTS")
    if ( ! 'coCovars' %in% names(self$config_structure_model$dv_bipartite) ) {
      rsiena_data <- sienaDataCreate(list(self$bipartite_rsienaDV), nodeSets = list(ACTORS, COMPONENTS))
      return(rsiena_data)
    }
    
    plt <- plt + ggtitle(sim_title_str)
    plt <- plt +  guides(color = guide_legend(nrow = 1))
    
    #### Density -------------------------------------------------------
    nrows_title <- stringr::str_count(sim_title_str, "\\\n")
    stratmeans <- dat %>% group_by(strategy) %>% 
      dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
    ## Actor density fact plots comparing H1 to H2 utility distribution
    plt2 <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) + ##linetype=chain_half
      geom_density(alpha=.1, linewidth=1)  +
      geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
      labs(y='', x='') +
      xlim( ylim ) + ## coordflip ylim for xlim
      coord_flip() +
      # facet_grid(wave_id ~ .) +
      ylab('Actor Utility Density') +
      theme_bw() + theme(
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'none', 
        plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank()#,
      ) + ggtitle(paste(rep('\n', nrows_title), collapse = '')) ## add empty lines (\n) to plot heights below title
    
    
    suppressMessages({
      combined_plot <- ggarrange(
        plt, plt2, 
        ncol = 2, 
        widths = c(4.1,0.9), # Adjust column widths
        common.legend = TRUE, # Share a common legend if needed
        legend = "bottom"#,     # Place legend at the bottom
      )
    })
 
    
    if(plot_save) {
      plot_file <- paste0('actr_utl_strt_smry_',plot_file, round(as.numeric(Sys.time())*10))
      ggsave(filename = file.path(ifelse(is.na(plot_dir)||plot_dir=='',getwd(),plot_dir), 
                              sprintf("%s_%s.jpeg", self$config_environ_params$name, plot_file)),
             combined_plot,
             width = 8, height = 7, units = 'in', dpi = 400)
    }
    
    if(plot_return)
      return(combined_plot)
  },
  

  plot_bipartite_ring_markets = function(step_ids = c(),
                                         component_groups = NULL,
                                         influence_matrix = NULL,
                                         actor_strategies = NULL,
                                         component_labels = NULL,
                                         actor_labels = NULL,
                                         actor_size = 5,
                                         component_size = 12,
                                         edge_alpha = 0.2,
                                         path_arrow_size = 0.15,
                                         path_linewidth = 0.8,
                                         ring_radius = 11,
                                         center_radius = 4,
                                         market_alpha = 0.2,
                                         epistatic_int_mat = NULL) {

    affiliation_array <- self$bi_env_arr

    # Check if the input is a 3D array
    if (!is.array(affiliation_array) || length(dim(affiliation_array)) != 3) {
      stop("Input must be a 3D array with dimensions [M actors, N components, S steps]")
    }

    if (length(step_ids)==0)
      step_ids <- 1:dim(affiliation_array)[3]

    affiliation_array <- affiliation_array[,,step_ids]

    # Get dimensions
    dims <- dim(affiliation_array)
    M <- dims[1]  # Number of actors
    N <- dims[2]  # Number of components
    S <- dims[3]  # Number of steps in decision chain


    if (!is.null(epistatic_int_mat)) {
      warning("`epistatic_int_mat` is deprecated as of searchnet 0.8.2; use ",
              "`influence_matrix` instead. W is the influence matrix, the model ",
              "INPUT; K_CC reports the realized inter-component structure it drives, ",
              "and epistatic fitness is the XWX effect it carries.",
              call. = FALSE)
      if (is.null(influence_matrix)) influence_matrix <- epistatic_int_mat
    }
    # Set default influence matrix if not provided
    if (is.null(influence_matrix) && is.null(component_groups) && 
        (is.null(self$markets) || is.null(self$markets$component_groups) ) 
        ) {
      stop("Either influence_matrix or component_groups must be provided")
    }
    
    ## use markets$component_groups if set during environment init
    if(is.null(component_groups) & !is.null(self$markets)) {
      component_groups <- self$markets[['component_groups']]
    }
      
    # If influence_matrix is provided but component_groups is not, generate groups
    if (is.null(component_groups) && !is.null(influence_matrix)) {
      # Convert influence matrix to graph
      g <- graph_from_adjacency_matrix(
        influence_matrix,
        mode = "undirected",
        weighted = TRUE,
        diag = FALSE
      )

      # Set threshold for meaningful interactions
      E(g)$weight[E(g)$weight < 0.3] <- 0
      g <- igraph::delete_edges(g, which(E(g)$weight == 0))

      # Community detection to find component groups
      communities <- igraph::cluster_louvain(g)

      # Convert membership to list format for many-to-many mapping
      component_groups <- list()
      for (i in 1:max(communities$membership)) {
        component_groups[[i]] <- which(communities$membership == i)
      }
    }

    # Ensure component_groups is in proper format
    if (!is.list(component_groups)) {
      if (is.matrix(component_groups) || is.data.frame(component_groups)) {
        # Assuming binary matrix format [component, group]
        component_groups_list <- list()
        for (g in 1:ncol(component_groups)) {
          component_groups_list[[g]] <- which(component_groups[, g] > 0)
        }
        component_groups <- component_groups_list
      } else {
        # Assuming vector format with one group per component
        component_groups_list <- list()
        for (g in sort(unique(component_groups))) {
          component_groups_list[[length(component_groups_list) + 1]] <- which(component_groups == g)
        }
        component_groups <- component_groups_list
      }
    }

    # Fix component indices to ensure they're within range 1:N
    for (g in 1:length(component_groups)) {
      component_groups[[g]] <- component_groups[[g]][component_groups[[g]] > 0 & component_groups[[g]] <= N]
    }

    # Get the number of groups
    num_groups <- length(component_groups)

    # Create component-to-groups mapping (initializing with empty lists)
    component_to_groups <- vector("list", N)
    for (i in 1:N) {
      component_to_groups[[i]] <- integer(0)
    }

    # Map groups to components (using safer variable names)
    for (group_idx in 1:num_groups) {
      for (comp_idx in component_groups[[group_idx]]) {
        if (comp_idx > 0 && comp_idx <= N) {  # Extra safety check
          component_to_groups[[comp_idx]] <- unique(append(component_to_groups[[comp_idx]], group_idx))
        }
      }
    }

    # Set default labels if not provided
    if (is.null(component_labels)) {
      component_labels <- paste0("C", 1:N)
    }
    if (is.null(actor_labels)) {
      actor_labels <- paste0("A", 1:M)
    }

    # Set default strategies if not provided
    if (is.null(actor_strategies)) {
      actor_strategies <- rep("Default", M)
    }

    # Create positions for components in a ring
    component_angles <- seq(0, 2*pi, length.out = N+1)[1:N]
    component_data <- data.frame(
      id = component_labels,
      x = ring_radius * cos(component_angles),
      y = ring_radius * sin(component_angles),
      angle = component_angles,
      type = "component"
    )

    # Add group memberships to component data (for visualization)
    component_data$primary_group  <- sapply(component_to_groups, function(x) ifelse(length(x) == 1, x[1],  NA ))

    groups_all <- unique(c(unlist(component_to_groups)))
    groups_not_unique <- groups_all[which( ! groups_all %in% unique(component_data$primary_group))]
    
    
    group_colors <- RColorBrewer::brewer.pal(num_groups, 'Accent')
    market_regions <- list()

    # For each group, create a convex hull around its components
    for (g in 1:num_groups) {
      # Skip if group is empty
      if (length(component_groups[[g]]) == 0) {
        next
      }

      # Get indices of components in this group (with safety checks)
      group_component_indices <- component_groups[[g]]
      group_component_indices <- group_component_indices[group_component_indices > 0 &
                                                           group_component_indices <= N]

      # Skip if no valid components in this group
      if (length(group_component_indices) == 0) {
        next
      }

      if (length(group_component_indices) < 3) {
        # Need at least 3 points for a polygon
        # Add extra points around each component to create a small region
        extra_points <- data.frame()
        for (idx in group_component_indices) {
          angle <- component_data$angle[idx]
          base_x <- component_data$x[idx]
          base_y <- component_data$y[idx]

          # Add points in a small arc around the component
          for (offset in seq(-pi/3, pi/3, length.out = 5)) {
            extra_points <- rbind(extra_points, data.frame(
              x = base_x + 1.0 * cos(angle + offset),
              y = base_y + 1.0 * sin(angle + offset)
            ))
          }
        }
        points <- rbind(component_data[group_component_indices, c("x", "y")], extra_points)
      } else {
        # Use actual component positions for convex hull
        points <- component_data[group_component_indices, c("x", "y")]
      }

      # Create a buffer around points to make the region larger
      buffer_points <- data.frame()
      for (i in 1:nrow(points)) {
        angle <- atan2(points$y[i], points$x[i])
        buffer_points <- rbind(buffer_points, data.frame(
          x = points$x[i] + 1.5 * cos(angle),
          y = points$y[i] + 1.5 * sin(angle)
        ))
      }

      all_points <- rbind(points, buffer_points)

      # Create a convex hull
      if (nrow(all_points) >= 3) {
        ch <- grDevices::chull(all_points$x, all_points$y)
        hull <- all_points[c(ch, ch[1]), ]

        market_regions[[g]] <- data.frame(
          x = hull$x,
          y = hull$y,
          group = g
        )
      } else if (nrow(all_points) > 0) {
        # If not enough points for a hull, create a circle
        angles <- seq(0, 2*pi, length.out = 30)
        center_x <- mean(all_points$x)
        center_y <- mean(all_points$y)
        radius <- 1.5

        market_regions[[g]] <- data.frame(
          x = center_x + radius * cos(angles),
          y = center_y + radius * sin(angles),
          group = g
        )
      }
    }

    # Combine market regions into one data frame
    if (length(market_regions) > 0) {
      market_regions_df <- do.call(rbind, market_regions)
      market_regions_df$group <- as.factor(market_regions_df$group)
    } else {
      market_regions_df <- data.frame(
        x = numeric(0),
        y = numeric(0),
        group = factor()
      )
    }

    # Calculate actor positions at each step
    actor_positions <- array(0, dim = c(M, 2, S))

    for (s in 1:S) {
      for (i in 1:M) {
        # Get weights (connections) for this actor at step s
        weights <- affiliation_array[i, , s]

        if (sum(weights) > 0) {
          # Normalize weights
          weights <- weights / sum(weights)

          # Calculate weighted position based on components' circular positions
          weighted_x <- sum(weights * component_data$x)
          weighted_y <- sum(weights * component_data$y)

          # Scale position to be inside the ring but not at the exact center
          dist_from_center <- sqrt(weighted_x^2 + weighted_y^2)

          if (dist_from_center > 0) {
            # Scale to be within center_radius
            scale_factor <- min(1, center_radius / dist_from_center)
            actor_positions[i, 1, s] <- weighted_x * scale_factor
            actor_positions[i, 2, s] <- weighted_y * scale_factor
          } else {
            # If exactly at center, place slightly off-center
            actor_positions[i, 1, s] <- rnorm(1, 0, 0.1)
            actor_positions[i, 2, s] <- rnorm(1, 0, 0.1)
          }
        } else {
          # If no connections, maintain previous position or place near center
          if (s > 1) {
            actor_positions[i, , s] <- actor_positions[i, , s-1]
          } else {
            # Start at a random position near center
            angle <- runif(1, 0, 2*pi)
            actor_positions[i, 1, s] <- center_radius * 0.5 * cos(angle)
            actor_positions[i, 2, s] <- center_radius * 0.5 * sin(angle)
          }
        }
      }
    }

    # Create actor data for final positions (last step)
    actor_data <- data.frame(
      id = actor_labels,
      x = actor_positions[, 1, S],
      y = actor_positions[, 2, S],
      strategy = actor_strategies,
      type = "actor"
    )

    # Combine data
    all_nodes <- rbind(
      component_data %>% select(id, x, y, type),
      actor_data %>% select(id, x, y, type)
    )

    # Create edge data for final state (actor to component)
    edges_list <- list()
    edge_counter <- 1

    for (i in 1:M) {
      for (j in 1:N) {
        if (affiliation_array[i, j, S] > 0) {
          edges_list[[edge_counter]] <- data.frame(
            from = actor_labels[i],
            to = component_labels[j],
            weight = affiliation_array[i, j, S]
          )
          edge_counter <- edge_counter + 1
        }
      }
    }

    if (length(edges_list) > 0) {
      edges <- do.call(rbind, edges_list)

      # Join with node positions
      edges <- edges %>%
        left_join(actor_data %>% select(id, x_from = x, y_from = y),
                  by = c("from" = "id")) %>%
        left_join(component_data %>% select(id, x_to = x, y_to = y),
                  by = c("to" = "id"))
    } else {
      edges <- data.frame(
        from = character(0),
        to = character(0),
        weight = numeric(0),
        x_from = numeric(0),
        y_from = numeric(0),
        x_to = numeric(0),
        y_to = numeric(0)
      )
    }

    # Create path data
    paths_list <- list()
    path_counter <- 1

    for (i in 1:M) {
      for (s in 1:(S-1)) {
        # Only create path segments if there's actual movement
        if (actor_positions[i, 1, s] != actor_positions[i, 1, s+1] ||
            actor_positions[i, 2, s] != actor_positions[i, 2, s+1]) {

          paths_list[[path_counter]] <- data.frame(
            actor = actor_labels[i],
            strategy = actor_strategies[i],
            x = actor_positions[i, 1, s],
            y = actor_positions[i, 2, s],
            xend = actor_positions[i, 1, s+1],
            yend = actor_positions[i, 2, s+1],
            step = s
          )
          path_counter <- path_counter + 1
        }
      }
    }

    if (length(paths_list) > 0) {
      paths <- do.call(rbind, paths_list)
    } else {
      paths <- data.frame(
        actor = character(0),
        strategy = character(0),
        x = numeric(0),
        y = numeric(0),
        xend = numeric(0),
        yend = numeric(0),
        step = numeric(0)
      )
    }

    # Generate a palette for strategies
    strategy_colors <- scales::hue_pal()(length(unique(actor_strategies)))
    names(strategy_colors) <- rev(unique(actor_strategies))

    # Create multi-membership visualization for components
    # For components that belong to multiple groups
    pie_data <- data.frame(
      id = character(0),
      x = numeric(0),
      y = numeric(0),
      group = numeric(0),
      start = numeric(0),
      end = numeric(0)
    )

    # Try to create pie chart segments for components with multiple groups
    for (i in 1:N) {
      groups <- component_to_groups[[i]]
      if (length(groups) > 1) {  # Only for components with multiple groups
        # Create pie chart segments
        segment_size <- 2 * pi / length(groups)
        for (j in 1:length(groups)) {
          start_angle <- (j - 1) * segment_size
          end_angle <- j * segment_size

          pie_data <- rbind(pie_data, data.frame(
            id = component_labels[i],
            x = component_data$x[i],
            y = component_data$y[i],
            group = groups[j],
            start = start_angle,
            end = end_angle
          ))
        }
      }
    }

    # Create the plot
    p <- ggplot() +
      # Draw market regions with translucency to show overlaps
      geom_polygon(data = market_regions_df,
                   aes(x = x, y = y, group = group, fill = group),
                   alpha = market_alpha) +
      # Draw edges for final state (actor to component)
      geom_segment(data = edges,
                   aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                       alpha = weight),
                   color = "gray70") +
      # Draw actor paths with arrows
      geom_segment(data = paths,
                   aes(x = x, y = y, xend = xend, yend = yend,
                       color = strategy),
                   arrow = arrow(type = "closed",
                                 length = unit(path_arrow_size, "inches")),
                   linewidth = path_linewidth) +
      # Draw component nodes
      geom_point(data = component_data,
                 aes(x = x, y = y, fill = as.factor(primary_group)),
                 size = component_size,
                 shape = 22,
                 alpha = .35,
                 color = "black") +
      # Draw actor nodes
      geom_point(data = actor_data,
                 aes(x = x, y = y, color = strategy),
                 shape = 1,
                 size = actor_size * 3) +
      # Add labels
      geom_text(data = component_data,
                aes(x = x * 1.1, y = y * 1.1, label = id),
                size = 3) +
      geom_text(data = actor_data,
                aes(x = x, y = y, label = id),
                vjust = -1.5, size = 3) +
      # Set colors, sizes, and scales
      scale_color_manual(values = strategy_colors, name = "Strategy") +
      scale_fill_manual(values = group_colors, name = "Market", na.value = NA, na.translate = FALSE) +
      scale_alpha_continuous(range = c(0.1, 0.8), name = "Weight") +
      # Set proper axis labels and scales
      coord_equal() +
      xlim(-ring_radius * 1.2, ring_radius * 1.2) +
      ylim(-ring_radius * 1.2, ring_radius * 1.2) +
      labs(title = "Market Entry and Repositioning: Actors and Components",
           subtitle = paste0(M, " actors and ", N, " components with decision paths (", S, " steps)")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    # If ggforce is available, add pie charts for multi-group components
    if (requireNamespace("ggforce", quietly = TRUE) && nrow(pie_data) > 0) {
      p <- p + ggforce::geom_arc_bar(data = pie_data,
                                     aes(x0 = x, y0 = y, r0 = 0, r = component_size / 24,
                                         start = start, end = end, fill = as.factor(group)),
                                     color = NA, size = 0.25)
    } else if (nrow(pie_data) > 0) {
      message("Package 'ggforce' not available. Component pie charts will not be displayed.")
    }

    return(p)
  },
  
  plot_bipartite_ring_markets_animation = function(step_ids = c(),
                                                   component_groups = NULL,
                                                   influence_matrix = NULL,
                                                   actor_strategies = NULL,
                                                   component_labels = NULL,
                                                   actor_labels = NULL,
                                                   actor_size = 5,
                                                   component_size = 12,
                                                   edge_alpha = 0.2,
                                                   path_arrow_size = 0.15,
                                                   path_linewidth = 0.8,
                                                   ring_radius = 11,
                                                   center_radius = 4,
                                                   market_alpha = 0.2,
                                                   animation_fps = 10,
                                                   animation_duration = 10,
                                                   epistatic_int_mat = NULL) {
    
    # Load required libraries if not already loaded
    if (!requireNamespace("gganimate", quietly = TRUE)) {
      stop("Package 'gganimate' is required for animation. Please install it.")
    }
    
    affiliation_array <- self$bi_env_arr
    
    # Check if the input is a 3D array
    if (!is.array(affiliation_array) || length(dim(affiliation_array)) != 3) {
      stop("Input must be a 3D array with dimensions [M actors, N components, S steps]")
    }
    
    if (length(step_ids)==0)
      step_ids <- 1:dim(affiliation_array)[3]
    
    affiliation_array <- affiliation_array[,,step_ids]
    
    # Get dimensions
    dims <- dim(affiliation_array)
    M <- dims[1]  # Number of actors
    N <- dims[2]  # Number of components
    S <- dims[3]  # Number of steps in decision chain
    
    
    if (!is.null(epistatic_int_mat)) {
      warning("`epistatic_int_mat` is deprecated as of searchnet 0.8.2; use ",
              "`influence_matrix` instead. W is the influence matrix, the model ",
              "INPUT; K_CC reports the realized inter-component structure it drives, ",
              "and epistatic fitness is the XWX effect it carries.",
              call. = FALSE)
      if (is.null(influence_matrix)) influence_matrix <- epistatic_int_mat
    }
    # Set default influence matrix if not provided
    if (is.null(influence_matrix) && is.null(component_groups) && !is.null(self$markets)) {
      component_groups <- self$markets$component_groups
    }
    
    # If influence_matrix is provided but component_groups is not, generate groups
    if (is.null(component_groups) && !is.null(influence_matrix)) {
      # Convert influence matrix to graph
      g <- graph_from_adjacency_matrix(
        influence_matrix,
        mode = "undirected",
        weighted = TRUE,
        diag = FALSE
      )
      
      # Set threshold for meaningful interactions
      E(g)$weight[E(g)$weight < 0.4] <- 0
      g <- igraph::delete_edges(g, which(E(g)$weight == 0))
      
      # Community detection to find component groups
      communities <- igraph::cluster_louvain(g)
      
      # Convert membership to list format for many-to-many mapping
      component_groups <- list()
      for (i in 1:max(communities$membership)) {
        component_groups[[i]] <- which(communities$membership == i)
      }
    }
    
    # Ensure component_groups is in proper format
    if (!is.list(component_groups)) {
      if (is.matrix(component_groups) || is.data.frame(component_groups)) {
        # Assuming binary matrix format [component, group]
        component_groups_list <- list()
        for (g in 1:ncol(component_groups)) {
          component_groups_list[[g]] <- which(component_groups[, g] > 0)
        }
        component_groups <- component_groups_list
      } else {
        # Assuming vector format with one group per component
        component_groups_list <- list()
        for (g in sort(unique(component_groups))) {
          component_groups_list[[length(component_groups_list) + 1]] <- which(component_groups == g)
        }
        component_groups <- component_groups_list
      }
    }
    
    # Fix component indices to ensure they're within range 1:N
    for (g in 1:length(component_groups)) {
      component_groups[[g]] <- component_groups[[g]][component_groups[[g]] > 0 & component_groups[[g]] <= N]
    }
    
    # Get the number of groups
    num_groups <- length(component_groups)
    
    
    # Create component-to-groups mapping (initializing with empty lists)
    component_to_groups <- vector("list", N)
    for (i in 1:N) {
      component_to_groups[[i]] <- integer(0)
    }
    
    # Map groups to components (using safer variable names)
    for (group_idx in 1:num_groups) {
      for (comp_idx in component_groups[[group_idx]]) {
        if (comp_idx > 0 && comp_idx <= N) {  # Extra safety check
          component_to_groups[[comp_idx]] <- unique(append(component_to_groups[[comp_idx]], group_idx))
        }
      }
    }
    
    # Set default labels if not provided
    if (is.null(component_labels)) {
      component_labels <- paste0("C", 1:N)
    }
    if (is.null(actor_labels)) {
      actor_labels <- paste0("A", 1:M)
    }
    
    # Set default strategies if not provided
    if (is.null(actor_strategies)) {
      actor_strategies <- rep("Default", M)
    }
    
    # Create positions for components in a ring
    component_angles <- seq(0, 2*pi, length.out = N+1)[1:N]
    component_data <- data.frame(
      id = component_labels,
      x = ring_radius * cos(component_angles),
      y = ring_radius * sin(component_angles),
      angle = component_angles,
      type = "component"
    )
    
    # Add group memberships to component data (for visualization)
    component_data$primary_group <- sapply(component_to_groups, function(x) ifelse(length(x) == 1, x[1], NA))
    
    # Generate unique colors for each group
    group_colors <- RColorBrewer::brewer.pal(num_groups, 'Accent')
    
    # Create market region data (one region per group)
    market_regions <- list()
    
    # For each group, create a convex hull around its components
    for (g in 1:num_groups) {
      # Skip if group is empty
      if (length(component_groups[[g]]) == 0) {
        next
      }
      
      # Get indices of components in this group (with safety checks)
      group_component_indices <- component_groups[[g]]
      group_component_indices <- group_component_indices[group_component_indices > 0 &
                                                           group_component_indices <= N]
      
      # Skip if no valid components in this group
      if (length(group_component_indices) == 0) {
        next
      }
      
      if (length(group_component_indices) < 3) {
        # Need at least 3 points for a polygon
        # Add extra points around each component to create a small region
        extra_points <- data.frame()
        for (idx in group_component_indices) {
          angle <- component_data$angle[idx]
          base_x <- component_data$x[idx]
          base_y <- component_data$y[idx]
          
          # Add points in a small arc around the component
          for (offset in seq(-pi/3, pi/3, length.out = 5)) {
            extra_points <- rbind(extra_points, data.frame(
              x = base_x + 1.0 * cos(angle + offset),
              y = base_y + 1.0 * sin(angle + offset)
            ))
          }
        }
        points <- rbind(component_data[group_component_indices, c("x", "y")], extra_points)
      } else {
        # Use actual component positions for convex hull
        points <- component_data[group_component_indices, c("x", "y")]
      }
      
      # Create a buffer around points to make the region larger
      buffer_points <- data.frame()
      for (i in 1:nrow(points)) {
        angle <- atan2(points$y[i], points$x[i])
        buffer_points <- rbind(buffer_points, data.frame(
          x = points$x[i] + 1.5 * cos(angle),
          y = points$y[i] + 1.5 * sin(angle)
        ))
      }
      
      all_points <- rbind(points, buffer_points)
      
      # Create a convex hull
      if (nrow(all_points) >= 3) {
        ch <- grDevices::chull(all_points$x, all_points$y)
        hull <- all_points[c(ch, ch[1]), ]
        
        market_regions[[g]] <- data.frame(
          x = hull$x,
          y = hull$y,
          group = g
        )
      } else if (nrow(all_points) > 0) {
        # If not enough points for a hull, create a circle
        angles <- seq(0, 2*pi, length.out = 30)
        center_x <- mean(all_points$x)
        center_y <- mean(all_points$y)
        radius <- 1.5
        
        market_regions[[g]] <- data.frame(
          x = center_x + radius * cos(angles),
          y = center_y + radius * sin(angles),
          group = g
        )
      }
    }
    
    # Combine market regions into one data frame
    if (length(market_regions) > 0) {
      market_regions_df <- do.call(rbind, market_regions)
      market_regions_df$group <- as.factor(market_regions_df$group)
    } else {
      market_regions_df <- data.frame(
        x = numeric(0),
        y = numeric(0),
        group = factor()
      )
    }
    
    # ANIMATION-SPECIFIC CODE STARTS HERE
    
    # Calculate actor positions at each step, BUT NOW FOR ANIMATION
    # We will create a data frame to store actor positions for each step
    
    all_actor_positions <- data.frame()
    all_edges <- data.frame()
    
    # Create a helper data frame for the step counter
    step_counter <- data.frame(
      x = -ring_radius * 1.1,
      y = ring_radius * 1.1,
      step = 1:S
    )
    
    for (s in 1:S) {
      # Calculate actor positions for this step as before
      for (i in 1:M) {
        # Get weights (connections) for this actor at step s
        weights <- affiliation_array[i, , s]
        
        x_pos <- 0
        y_pos <- 0
        
        if (sum(weights) > 0) {
          # Normalize weights
          weights <- weights / sum(weights)
          
          # Calculate weighted position based on components' circular positions
          weighted_x <- sum(weights * component_data$x)
          weighted_y <- sum(weights * component_data$y)
          
          # Scale position to be inside the ring but not at the exact center
          dist_from_center <- sqrt(weighted_x^2 + weighted_y^2)
          
          if (dist_from_center > 0) {
            # Scale to be within center_radius
            scale_factor <- min(1, center_radius / dist_from_center)
            x_pos <- weighted_x * scale_factor
            y_pos <- weighted_y * scale_factor
          } else {
            # If exactly at center, place slightly off-center
            x_pos <- rnorm(1, 0, 0.1)
            y_pos <- rnorm(1, 0, 0.1)
          }
        } else {
          # If no connections, maintain previous position or place near center
          if (s > 1) {
            prev_pos <- all_actor_positions %>% 
              filter(actor_id == actor_labels[i], step == s-1)
            if (nrow(prev_pos) > 0) {
              x_pos <- prev_pos$x
              y_pos <- prev_pos$y
            } else {
              angle <- runif(1, 0, 2*pi)
              x_pos <- center_radius * 0.5 * cos(angle)
              y_pos <- center_radius * 0.5 * sin(angle)
            }
          } else {
            # Start at a random position near center
            angle <- runif(1, 0, 2*pi)
            x_pos <- center_radius * 0.5 * cos(angle)
            y_pos <- center_radius * 0.5 * sin(angle)
          }
        }
        
        # Add to the data frame
        all_actor_positions <- rbind(all_actor_positions, data.frame(
          actor_id = actor_labels[i],
          strategy = actor_strategies[i],
          x = x_pos,
          y = y_pos,
          step = s
        ))
        
        # Create edges for this step
        for (j in 1:N) {
          if (affiliation_array[i, j, s] > 0) {
            all_edges <- rbind(all_edges, data.frame(
              from = actor_labels[i],
              to = component_labels[j],
              weight = affiliation_array[i, j, s],
              step = s
            ))
          }
        }
      }
    }
    
    # Join edges with positions
    all_edges <- all_edges %>%
      left_join(all_actor_positions %>% select(actor_id, x_from = x, y_from = y, step), 
                by = c("from" = "actor_id", "step" = "step")) %>%
      left_join(component_data %>% select(id, x_to = x, y_to = y), 
                by = c("to" = "id"))
    
    # Create path data for animation
    # For each step, we'll show the path up to that step
    all_paths <- data.frame()
    
    for (i in 1:M) {
      for (s in 1:(S-1)) {
        actor_start <- all_actor_positions %>% 
          filter(actor_id == actor_labels[i], step == s)
        
        actor_end <- all_actor_positions %>% 
          filter(actor_id == actor_labels[i], step == s+1)
        
        if (nrow(actor_start) > 0 && nrow(actor_end) > 0) {
          # Only create path segments if there's actual movement
          if (actor_start$x != actor_end$x || actor_start$y != actor_end$y) {
            all_paths <- rbind(all_paths, data.frame(
              actor = actor_labels[i],
              strategy = actor_strategies[i],
              x = actor_start$x,
              y = actor_start$y,
              xend = actor_end$x,
              yend = actor_end$y,
              step = s,
              show_at_step = s+1  # This will be used for the animation
            ))
          }
        }
      }
    }
    
    # Prepare data for gradual path visualization
    path_animation_data <- data.frame()
    
    for (s in 1:S) {
      # Get all paths that should be visible at step s
      visible_paths <- all_paths %>% filter(show_at_step <= s)
      
      if (nrow(visible_paths) > 0) {
        visible_paths$current_step <- s
        path_animation_data <- rbind(path_animation_data, visible_paths)
      }
    }
    
    
    strategy_colors <- scales::hue_pal()(length(unique(actor_strategies)))
    names(strategy_colors) <- unique(actor_strategies)
    
    
    # Create data for multi-group component visualization
    
    # Create multi-membership visualization for components
    # For components that belong to multiple groups
    pie_data <- data.frame(
      id = character(0),
      x = numeric(0),
      y = numeric(0),
      group = numeric(0),
      start = numeric(0),
      end = numeric(0)
    )
    
    # Try to create pie chart segments for components with multiple groups
    for (i in 1:N) {
      groups <- component_to_groups[[i]]
      if (length(groups) > 1) {  # Only for components with multiple groups
        # Create pie chart segments
        segment_size <- 2 * pi / length(groups)
        for (j in 1:length(groups)) {
          start_angle <- (j - 1) * segment_size
          end_angle <- j * segment_size
          
          pie_data <- rbind(pie_data, data.frame(
            id = component_labels[i],
            x = component_data$x[i],
            y = component_data$y[i],
            group = groups[j],
            start = start_angle,
            end = end_angle
          ))
        }
      }
    }
    
    # Create the plot with animation
    p <- ggplot() +
      # Draw market regions with translucency to show overlaps
      geom_polygon(data = market_regions_df,
                   aes(x = x, y = y, group = group, fill = group),
                   alpha = market_alpha) +
      # Draw component nodes
      geom_point(data = component_data,
                 aes(x = x, y = y, fill = as.factor(primary_group)),
                 size = component_size,
                 shape = 22,
                 alpha = .35,
                 color = "black") +
      # Add component labels
      geom_text(data = component_data,
                aes(x = x * 1.1, y = y * 1.1, label = id),
                size = 3) +
      # Draw edges for each step
      geom_segment(data = all_edges,
                   aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                       alpha = weight, group = paste(from, to)),
                   color = "gray70") +
      # Draw actor paths with arrows that appear based on step
      geom_segment(data = all_paths,
                   aes(x = x, y = y, xend = xend, yend = yend, color = strategy,
                       group = paste(actor, step)),
                   arrow = arrow(type = "closed", length = unit(path_arrow_size, "inches")),
                   linewidth = path_linewidth) +
      # Draw actor nodes for each step
      geom_point(data = all_actor_positions,
                 aes(x = x, y = y, color = strategy, group = actor_id),
                 shape = 1,
                 size = actor_size * 3) +
      # Add actor labels
      geom_text(data = all_actor_positions,
                aes(x = x, y = y, label = actor_id, group = actor_id),
                vjust = -1.5, size = 3) +
      scale_color_manual(values = strategy_colors, name = "Strategy") +
      scale_fill_manual(values = group_colors, name = "Market", na.value = NA, na.translate = FALSE) +
      scale_alpha_continuous(range = c(0.1, 0.8), name = "Weight") +
      # Set proper axis labels and scales
      coord_equal() +
      xlim(-ring_radius * 1.2, ring_radius * 1.2) +
      ylim(-ring_radius * 1.2, ring_radius * 1.2) +
      labs(title = "Market Entry and Repositioning: Actors and Components",
           subtitle = "Step: {closest_state}") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    # If ggforce is available, add pie charts for multi-group components
    if (requireNamespace("ggforce", quietly = TRUE) && nrow(pie_data) > 0) {
      p <- p + ggforce::geom_arc_bar(data = pie_data,
                                     aes(x0 = x, y0 = y, r0 = 0, r = component_size / 24,
                                         start = start, end = end, fill = as.factor(group)),
                                     color = NA, size = 0.25)
    } else if (nrow(pie_data) > 0) {
      message("Package 'ggforce' not available. Component pie charts will not be displayed.")
    }
    
    
    # Add animation elements
    anim <- p + 
      # Add actor nodes that move
      geom_point(data = all_actor_positions,
                 aes(x = x, y = y, color = strategy, group = actor_id),
                 shape = 1,
                 size = actor_size * 3) +
      # Add actor labels that move with the actors
      geom_text(data = all_actor_positions,
                aes(x = x, y = y, label = actor_id, group = actor_id),
                vjust = -1.5, size = 3) +
      # Add edges that change at each step
      geom_segment(data = all_edges,
                   aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                       alpha = weight, group = paste(from, to, step)),
                   color = rgb(.5,.5,.5, .4) ) + #scale_size_continuous(range = c(0.5, 2)) +
      # Add path segments that appear over time
      geom_segment(data = path_animation_data,
                   aes(x = x, y = y, xend = xend, yend = yend, 
                       color = strategy, group = paste(actor, step)),
                   arrow = arrow(type = "closed", length = unit(path_arrow_size, "inches")),
                   linewidth = path_linewidth) +
      # Define the transition
      gganimate::transition_states(
        states = step,
        transition_length = 2,
        state_length = 3
      ) +
      # Add view_follow to keep focus on the moving actors
      # gganimate::view_follow(fixed_y = TRUE) +
      gganimate::ease_aes('linear') ##+ gganimate::shadow_trail(past = FALSE, future = FALSE)  # Show the path traveled
    
    # Render the animation
    animated_plot <- gganimate::animate(
      anim,
      nframes = S * 3,  # 5 frames per step
      fps = animation_fps,
      duration = animation_duration,
      width = 800,
      height = 800,
      renderer = gganimate::gifski_renderer()
    )
    
    return(animated_plot)
  
  },
  
  
  get_component_groups_list = function(component_groups=NULL, influence_matrix=NULL, epistatic_int_mat = NULL) {
    
    if (!is.null(epistatic_int_mat)) {
      warning("`epistatic_int_mat` is deprecated as of searchnet 0.8.2; use ",
              "`influence_matrix` instead. W is the influence matrix, the model ",
              "INPUT; K_CC reports the realized inter-component structure it drives, ",
              "and epistatic fitness is the XWX effect it carries.",
              call. = FALSE)
      if (is.null(influence_matrix)) influence_matrix <- epistatic_int_mat
    }
    # If influence_matrix is provided but component_groups is not, generate groups
    if (is.null(component_groups) && !is.null(influence_matrix)) {
      # Convert influence matrix to graph
      g <- graph_from_adjacency_matrix(
        influence_matrix,
        mode = "undirected",
        weighted = TRUE,
        diag = FALSE
      )
      
      # Set threshold for meaningful interactions
      E(g)$weight[E(g)$weight < 0.4] <- 0
      g <- igraph::delete_edges(g, which(E(g)$weight == 0))
      
      # Community detection to find component groups
      communities <- igraph::cluster_louvain(g)
      
      # Convert membership to list format for many-to-many mapping
      component_groups <- list()
      for (i in 1:max(communities$membership)) {
        component_groups[[i]] <- which(communities$membership == i)
      }
    }
    
    if (is.null(component_groups) && !is.null(self$markets))
      component_groups <- self$markets[['component_groups']]
    
    if (is.null(component_groups)) {
      return(list(
        component_groups = list(),
        component_to_groups = list(),
        num_groups = 0
      ))
    }

    
    # Ensure component_groups is in proper format
    if (!is.list(component_groups)) {
      if (is.matrix(component_groups) || is.data.frame(component_groups)) {
        # Assuming binary matrix format [component, group]
        component_groups_list <- list()
        for (g in 1:ncol(component_groups)) {
          component_groups_list[[g]] <- which(component_groups[, g] > 0)
        }
        component_groups <- component_groups_list
      } else {
        # Assuming vector format with one group per component
        component_groups_list <- list()
        for (g in sort(unique(component_groups))) {
          component_groups_list[[length(component_groups_list) + 1]] <- which(component_groups == g)
        }
        component_groups <- component_groups_list
      }
    }
    
    # Fix component indices to ensure they're within range 1:N
    for (g in 1:length(component_groups)) {
      component_groups[[g]] <- component_groups[[g]][component_groups[[g]] > 0 & component_groups[[g]] <= self$N]
    }
    
    # Get the number of groups
    num_groups <- length(component_groups)
    
    
    # Create component-to-groups mapping (initializing with empty lists)
    component_to_groups <- vector("list", self$N)
    for (i in 1:self$N) {
      component_to_groups[[i]] <- integer(0)
    }
    
    # Map groups to components (using safer variable names)
    for (group_idx in 1:num_groups) {
      for (comp_idx in component_groups[[group_idx]]) {
        if (comp_idx > 0 && comp_idx <= self$N) {  # Extra safety check
          component_to_groups[[comp_idx]] <- unique(append(component_to_groups[[comp_idx]], group_idx))
        }
      }
    }
    
    return(list(
      component_groups = component_groups,
      component_to_groups = component_to_groups,
      num_groups = num_groups
    ))
    
  },
  
  
  search_rsiena_mc_market_exit = function(experiment_name = 'market_exit',
                                          n=50, 
                                          environ_params=NULL, 
                                          structure_model=NULL, 
                                          iterations_per_actor=NULL, 
                                          theta_shocks=NULL,
                                          conf_level = 0.95,
                                          experiment_seed = 54321,
                                          verbose=FALSE) {
      
    environ_params <- if (!is.null(self$config_environ_params)){
      self$config_environ_params
    } else if (!is.null(environ_params)) {
      environ_params
    } else {
      stop('missing environ_params and self$config_environ_params')
    }
    
    structure_model <- if (!is.null(self$config_structure_model)){
      self$config_structure_model
    } else if (!is.null(structure_model)) {
      structure_model
    } else {
      stop('missing structure_model and self$config_structure_model')
    }
    
    if (is.null(theta_shocks))
      theta_shocks <- self$theta_shocks
    
    if (is.null(iterations_per_actor) && !is.null(self$theta_matrix))
      iterations_per_actor <- round(nrow(self$theta_matrix) / self$M)
    
    actor_strats <- as.character( self$get_actor_strategies() )
    
    # Initialize lists
    util_list <- list()
    stats_list <- list()
    K4_list <- list()
    exit_list <- list()
    
    set.seed(experiment_seed)
    batch_seeds <- sample(1:9999999, size = n, replace = FALSE)
    
    # Run simulations with multiple seeds
    for (i in 1:length(batch_seeds)) {
      run_seed_i <- batch_seeds[i]
      
      cat(sprintf('\n run %s, seed = %s \n', i, run_seed_i))
      
      if (i > 1) {
        env_i <- NULL;
        graphics.off();
        gc();
      }
      
      # Run RSiena search using variable parameters in theta_shocks
      env_i <- super$clone(deep = TRUE)
      
      env_i$search_rsiena(
        structure_model = structure_model,
        iterations_per_actor = iterations_per_actor,
        theta_shocks = theta_shocks,
        run_seed = run_seed_i,
        verbose = verbose
      )
      
      # graphics.off() 
      
      util_list[[as.character(run_seed_i)]]  <- env_i$actor_util_df
      stats_list[[as.character(run_seed_i)]] <- env_i$actor_stats_df
      K4_list[[as.character(run_seed_i)]]    <- env_i$get_K4_df() 
      
      # Process network data to find exits
      arr <- env_i$bi_env_arr[,,]
      
      # Find ties that existed initially but are broken later
      initial_ties <- which(self$bipartite_matrix_init == 1, arr.ind = TRUE)
      colnames(initial_ties) <- c('from', 'to')
      
      # Get component groups to identify markets
      component_grps_list <- self$get_component_groups_list()
      
      if (is.null(component_grps_list$component_groups)) {
        exit_list[[as.character(run_seed_i)]] <- data.frame()
        next
      }
      
      # Find actors who were initially in markets (had ties to at least one component in each market)
      actor_initial_markets <- list()
      
      for (actor_id in 1:self$M) {
        actor_ties <- initial_ties[initial_ties[,1] == actor_id, 2]
        actor_markets <- c()
        
        for (mkt_id in 1:length(component_grps_list$component_groups)) {
          components_in_market <- component_grps_list$component_groups[[mkt_id]]
          if (any(actor_ties %in% components_in_market)) {
            actor_markets <- c(actor_markets, mkt_id)
          }
        }
        
        if (length(actor_markets) > 0) {
          actor_initial_markets[[actor_id]] <- actor_markets
        }
      }
      
      # Track market exits for each step
      exit_events <- data.frame()
      
      for (step in 1:dim(arr)[3]) {
        current_ties <- which(arr[,,step] == 1, arr.ind = TRUE)
        colnames(current_ties) <- c('from', 'to')
        
        # Check each actor who was initially in markets
        for (actor_id in names(actor_initial_markets)) {
          actor_id_num <- as.numeric(actor_id)
          initial_markets <- actor_initial_markets[[actor_id]]
          
          # Check if actor has exited from any of their initial markets
          actor_current_ties <- current_ties[current_ties[,1] == actor_id_num, 2]
          
          for (mkt_id in initial_markets) {
            components_in_market <- component_grps_list$component_groups[[mkt_id]]
            
            # Check if actor still has ties to this market
            still_in_market <- any(actor_current_ties %in% components_in_market)
            
            # Check if this is the first step where the actor exited this market
            if (!still_in_market) {
              # Verify the actor was in the market in the previous step (or initially if step 1)
              was_in_market <- FALSE
              
              if (step == 1) {
                # Check initial state
                initial_actor_ties <- initial_ties[initial_ties[,1] == actor_id_num, 2]
                was_in_market <- any(initial_actor_ties %in% components_in_market)
              } else {
                # Check previous step
                prev_ties <- which(arr[,,step-1] == 1, arr.ind = TRUE)
                colnames(prev_ties) <- c('from', 'to')
                prev_actor_ties <- prev_ties[prev_ties[,1] == actor_id_num, 2]
                was_in_market <- any(prev_actor_ties %in% components_in_market)
              }
              
              # If was in market before but not now, record exit
              if (was_in_market) {
                # Find if we already recorded this exit event
                existing_exit <- any(exit_events$from == actor_id_num & 
                                       exit_events$market_id == mkt_id)
                
                if (!existing_exit) {
                  exit_events <- rbind(exit_events, data.frame(
                    from = actor_id_num,
                    market_id = mkt_id,
                    exit_step = step,
                    strategy = ifelse(length(actor_strats), 
                                      as.character(actor_strats)[actor_id_num], NA),
                    sim_seed = env_i$rsiena_env_seed
                  ))
                }
              }
            }
          }
        }
      }
      
      # Store exit data
      exit_list[[as.character(run_seed_i)]] <- exit_events
    }
    
    # Count total simulations
    total_sims <- length(batch_seeds)
    
    # Combine all results
    util_df  <- data.table::rbindlist(util_list, idcol = 'run_seed')
    stats_df <- data.table::rbindlist(stats_list, idcol = 'run_seed')
    K4_df    <- data.table::rbindlist(K4_list, idcol = 'run_seed')
    exit_df <- data.table::rbindlist(exit_list, idcol = 'run_seed')
    
    # Initialize 
    first_exits <- data.frame()
    survival_data <- data.frame()
    
    if (length(exit_df)) {
      
      # Process data for survival analysis
      # Get all unique actors and maximum step
      all_actors <- unique(exit_df$from)
      max_step <- max(exit_df$exit_step)
      
      # Find first exit time for each actor in each simulation
      first_exits <- exit_df %>%
        dplyr::group_by(run_seed, from) %>%
        dplyr::summarize(
          first_exit_step = min(exit_step),
          .groups = "drop"
        )
      
      if (length(actor_strats)) {
        first_exits$strategy <- NA
        for (actor_strat_id in 1:length(actor_strats)) {
          .ids <- which(first_exits$from == actor_strat_id )
          first_exits$strategy[.ids] <- as.character( actor_strats[actor_strat_id] )
        }
      }
      
      ##--------------------------------------------
        
      # Calculate z-value for confidence intervals
      z_value <- qnorm(1 - (1 - conf_level) / 2)
      
      # Process each actor separately
      for (actor_id in all_actors) {
        # Get exits for this actor
        actor_exits <- subset(first_exits, from == actor_id)
        
        # Count actor's simulations
        actor_sim_count <- length(unique(actor_exits$run_seed))
        
        # For each step, calculate survival and exit rates
        for (s in 0:max_step) {
          
            # For step 0, all simulations have survived (no exits yet)
            if (s == 0) {
              surviving_sims <- total_sims
            } else {
              # Count simulations where first exit is after this step or actor never exits
              exited_after_s <- sum(actor_exits$first_exit_step > s)
              never_exited <- total_sims - actor_sim_count
              surviving_sims <- exited_after_s + never_exited
            }
          
            # Calculate rates
            survival_rate <- surviving_sims / total_sims
            exit_rate <- 1 - survival_rate
            
            # Calculate standard errors
            se_exit <- sqrt(exit_rate * (1 - exit_rate) / total_sims)
            
            # Calculate confidence intervals
            ci_lower_exit <- max(0, exit_rate - z_value * se_exit)
            ci_upper_exit <- min(1, exit_rate + z_value * se_exit)
            
            # Create a row for this step and actor
            row_data <- data.frame(
              step = s,
              actor = actor_id,
              strategy = ifelse(length(actor_strats), as.character(actor_strats[actor_id]), NA),
              total_sims = total_sims,
              surviving_sims = surviving_sims,
              survival_rate = survival_rate,
              exit_rate = exit_rate,
              ci_lower_exit = ci_lower_exit,
              ci_upper_exit = ci_upper_exit
            )
          
            # Add to the combined data frame
            survival_data <- rbind(survival_data, row_data)
          }
        }
          
        ## add missing actors if they never exited any market
        # First identify actors who were initially in markets
        actors_in_initial_markets <- c()
        initial_ties <- which(self$bipartite_matrix_init == 1, arr.ind = TRUE)
        component_grps_list <- self$get_component_groups_list()
        
        if (!is.null(component_grps_list$component_groups)) {
          for (actor_id in 1:self$M) {
            actor_ties <- initial_ties[initial_ties[,1] == actor_id, 2]
            for (mkt_id in 1:length(component_grps_list$component_groups)) {
              components_in_market <- component_grps_list$component_groups[[mkt_id]]
              if (any(actor_ties %in% components_in_market)) {
                actors_in_initial_markets <- c(actors_in_initial_markets, actor_id)
                break
              }
            }
          }
        }
        
        missing_actors <- which( ! actors_in_initial_markets %in% all_actors )
        if (length(missing_actors)) {
          for (i in 1:length(missing_actors)){
            actor_id <- actors_in_initial_markets[missing_actors[i]]
            row_data <- data.frame(
                step = max_step+1,  ##**TODO**Check what is best index step for plotting not-yet-exited
                actor = actor_id,
                strategy = ifelse(length(actor_strats), as.character(actor_strats[actor_id]), NA),
                total_sims = total_sims,
                surviving_sims = total_sims,
                survival_rate = 100,
                exit_rate = 0,
                ci_lower_exit = 0,
                ci_upper_exit = 0
            )
              
              # Add to the combined data frame
            survival_data <- rbind(survival_data, row_data)
            }
        }
        
        # Ensure actor is a factor for proper plotting
        survival_data$actor <- factor(survival_data$actor)
        
      }
      
      
      # Set the simulation results for experiment
      self$experiments[[ experiment_name ]] <- list(
        survival_data = survival_data,
        exit_df = exit_df,
        util_df = util_df,
        stats_df = stats_df,
        K4_df = K4_df,
        first_exits = first_exits,
        all_actors = unique(exit_df$from),
        max_step = if(length(exit_df)) max(exit_df$exit_step) else 0,
        total_sims = length(batch_seeds),
        batch_seeds = batch_seeds
      )
      
      cat(sprintf('\n\n%s simulation runs completed.\n\n', n))
    },
    
    
    #   ##**TODO**
    #   ##*##**TODO**
    #   ##*##**TODO**
  
  
  search_rsiena_mc_market_entry = function(experiment_name = 'market_entry',
                                           n=50, 
                                           environ_params=NULL, 
                                           structure_model=NULL, 
                                           iterations_per_actor=NULL, 
                                           theta_shocks=NULL,
                                           conf_level = 0.95,
                                           experiment_seed = 54321,
                                           verbose=FALSE) {
   
    environ_params <- if (!is.null(self$config_environ_params)){
      self$config_environ_params
    } else if (!is.null(environ_params)) {
      environ_params
    } else {
      stop('missing environ_params and self$config_environ_params')
    }
    
    structure_model <- if (!is.null(self$config_structure_model)){
      self$config_structure_model
    } else if (!is.null(structure_model)) {
      structure_model
    } else {
      stop('missing structure_model and self$config_structure_model')
    }
    
    if (is.null(theta_shocks))
      theta_shocks <- self$theta_shocks
    
    if (is.null(iterations_per_actor) && !is.null(self$theta_matrix))
      iterations_per_actor <- round(nrow(self$theta_matrix) / self$M)
    
    actor_strats <- as.character( self$get_actor_strategies() )
    
    # Initialize lists
    util_list <- list()
    stats_list <- list()
    K4_list <- list()
    entry_list <- list()
    
    set.seed(experiment_seed)
    batch_seeds <- sample(1:9999999, size = n, replace = FALSE)
    
    # Run simulations with multiple seeds
    for (i in 1:length(batch_seeds)) {
      run_seed_i <- batch_seeds[i]
      
      cat(sprintf('\n run %s, seed = %s \n', i, run_seed_i))
      
      
      if (i > 1) {
        env_i <- NULL;
        graphics.off();
        gc();
      }
      
      # Run RSiena search using variable parameters in theta_shocks
      env_i <- super$clone(deep = TRUE)
      
      env_i$search_rsiena(
        structure_model = structure_model,
        iterations_per_actor = iterations_per_actor,
        theta_shocks = theta_shocks,
        run_seed = run_seed_i,
        verbose = verbose
      )
      
      util_list[[as.character(run_seed_i)]]  <- env_i$actor_util_df
      stats_list[[as.character(run_seed_i)]] <- env_i$actor_stats_df
      K4_list[[as.character(run_seed_i)]]    <- env_i$get_K4_df() 
      
      # Process network data
      arr <- env_i$bi_env_arr[,,]
      
      # Convert the 3D array to indices where value is 1
      indices1 <- which(arr == 1, arr.ind = TRUE)
      colnames(indices1) <- c('from', 'to', 'step')
      
      # Find ties in new markets (entries)
      empty_component_ids <- apply(self$bipartite_matrix_init, 2, function(x)all(x==0))
      component_grps_list <- self$get_component_groups_list()
      if ( is.null(component_grps_list$component_groups)) {
        entry_list[[as.character(run_seed_i)]] <- data.frame()
        next
      }
      markets_new <- sapply(component_grps_list$component_groups, function(x) all(empty_component_ids[x]) )
      ids_new_mkts <- which( markets_new )
      if ( !length(ids_new_mkts)) {
        entry_list[[as.character(run_seed_i)]] <- data.frame()
        next
      }
      new_mkt_component_ids <- component_grps_list$component_groups[[ ids_new_mkts ]]
      idx_mkt_new <- which(indices1[,2] %in% new_mkt_component_ids)
      
      # Store entry data
      if(length(idx_mkt_new) > 0) {
        entry_list[[as.character(run_seed_i)]] <- data.frame(
          from = indices1[idx_mkt_new, 1],
          to = indices1[idx_mkt_new, 2],
          chain_step_id = indices1[idx_mkt_new, 3],
          strategy = ifelse(length(actor_strats), as.character(actor_strats)[ indices1[idx_mkt_new, 1] ], NA),
          sim_seed = env_i$rsiena_env_seed  # Renamed to avoid column name conflict
        )
      }
    }
    
    # Count total simulations
    total_sims <- length(batch_seeds)
    
    # Combine all results
    util_df  <- data.table::rbindlist(util_list, idcol = 'run_seed')
    stats_df <- data.table::rbindlist(stats_list, idcol = 'run_seed')
    K4_df    <- data.table::rbindlist(K4_list, idcol = 'run_seed')
    entry_df <- data.table::rbindlist(entry_list, idcol = 'run_seed')
    
    # Initialize 
    first_entries <- data.frame()
    survival_data <- data.frame()
    
    if (length(entry_df)) {
      
      # Process data for survival analysis
      # Get all unique actors and maximum step
      all_actors <- unique(entry_df$from)
      max_step <- max(entry_df$chain_step_id)
      
      
      # Find first entry time for each actor in each simulation
      first_entries <- entry_df %>%
        dplyr::group_by(run_seed, from) %>%
        dplyr::summarize(
          first_entry_step = min(chain_step_id),
          .groups = "drop"
        )
      
      if (length(actor_strats)) {
        first_entries$strategy <- NA
        for (actor_strat_id in 1:length(actor_strats)) {
          .ids <- which(first_entries$from == actor_strat_id )
          first_entries$strategy[.ids] <- as.character( actor_strats[actor_strat_id] )
        }
      }
      
      ##--------------------------------------------
      
      # Calculate z-value for confidence intervals
      z_value <- qnorm(1 - (1 - conf_level) / 2)
      
      # Process each actor separately
      for (actor_id in all_actors) {
        # Get entries for this actor
        actor_entries <- subset(first_entries, from == actor_id)
        
        # Count actor's simulations
        actor_sim_count <- length(unique(actor_entries$run_seed))
        
        # For each step, calculate survival and entry rates
        for (s in 0:max_step) {
          # For step 0, all simulations have survived
          if (s == 0) {
            surviving_sims <- total_sims
          } else {
            # Count simulations where first entry is after this step or actor never enters
            entered_after_s <- sum(actor_entries$first_entry_step > s)
            never_entered <- total_sims - actor_sim_count
            surviving_sims <- entered_after_s + never_entered
          }
          
          # Calculate rates
          survival_rate <- surviving_sims / total_sims
          entry_rate <- 1 - survival_rate
          
          # Calculate standard errors
          se_entry <- sqrt(entry_rate * (1 - entry_rate) / total_sims)
          
          # Calculate confidence intervals
          ci_lower_entry <- max(0, entry_rate - z_value * se_entry)
          ci_upper_entry <- min(1, entry_rate + z_value * se_entry)
          
          # Create a row for this step and actor
          row_data <- data.frame(
            step = s,
            actor = actor_id,
            strategy = ifelse(length(actor_strats), as.character(actor_strats[actor_id]), NA),
            total_sims = total_sims,
            surviving_sims = surviving_sims,
            survival_rate = survival_rate,
            entry_rate = entry_rate,
            ci_lower_entry = ci_lower_entry,
            ci_upper_entry = ci_upper_entry
          )
          
          # Add to the combined data frame
          survival_data <- rbind(survival_data, row_data)
        }
      }
      
      ## add missing actors if they never entered the market
      missing_actors <- which( ! 1:self$M %in% all_actors )
      if (length(missing_actors)) {
        for (i in 1:length(missing_actors)){
          actor_id <- missing_actors[i]
          row_data <- data.frame(
            step = max_step+1,  ##**TODO**Check what is best index step for plotting not-yet-entered
            actor = actor_id,
            strategy = ifelse(length(actor_strats), as.character(actor_strats[actor_id]), NA),
            total_sims = total_sims,
            surviving_sims = total_sims,
            survival_rate = 100,
            entry_rate = 0,
            ci_lower_entry = 0,
            ci_upper_entry = 0
          )
          
          # Add to the combined data frame
          survival_data <- rbind(survival_data, row_data)
        }
      }
      
      # Ensure actor is a factor for proper plotting
      survival_data$actor <- factor(survival_data$actor)
      
    }
    
   
    # Set the simulation results for experiment
    self$experiments[[ experiment_name ]] <- list(
      survival_data = survival_data,
      entry_df = entry_df,
      util_df = util_df,
      stats_df = stats_df,
      K4_df = K4_df,
      first_entries = first_entries,
      all_actors = unique(entry_df$from),
      max_step = max(entry_df$chain_step_id),
      total_sims = length(batch_seeds),
      batch_seeds = batch_seeds
    )
    
    cat(sprintf('\n\n%s simulation runs completed.\n\n', n))
  },
  
  
  # Second function: Create market entry survival plot from simulation data
  plot_market_entry_survival = function(experiment_name = 'market_entry', 
                                        cumulative = FALSE, # switch survival (non-entry) rate to cumulative entry_rate 
                                        conf_level = 0.95,
                                        return_data=FALSE) {
    if (is.null(self$experiments) || is.null(self$experiments[[experiment_name]]))
      stop('market_entry experiment not available. First call mcsim_market_entry()')
    
    # sim results 
    sim_results <- self$experiments[[experiment_name]]
  
    # Extract data from simulation results
    survival_data <- sim_results$survival_data
    entry_df <- sim_results$entry_df
    first_entries <- sim_results$first_entries
    all_actors <- sim_results$all_actors
    max_step <- sim_results$max_step
    total_sims <- sim_results$total_sims
    
    if (!length(survival_data) || !nrow(survival_data)) {
      cat('\nNo Entries to plot.\n')
      return(sim_results)
    }
    
    if ( ! cumulative ) {
     
     survival_data$survival_rate <-   1 - survival_data$entry_rate
     survival_data$ci_lower_entry <-  1 - survival_data$ci_lower_entry
     survival_data$ci_upper_entry <-  1 - survival_data$ci_upper_entry
     # Create the cumulative entry plot
     plot <- ggplot2::ggplot(survival_data, 
                             ggplot2::aes(x = step, y = survival_rate, 
                                          color = actor, group = actor)) +
       ggplot2::geom_step(size = 1) +
       ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry, 
                                         fill = actor), alpha = 0.2, color = NA) +
       ggplot2::labs(
         x = "Chain Step ID",
         y = "Proportion Not Yet Entered Market (Survival Rate)",
         title = "Market Entry Survival Curves by Actor",
         subtitle = paste0(conf_level * 100, "% Confidence Intervals")
       ) +
       ggplot2::scale_y_continuous(
         labels = scales::percent_format(),
         limits = c(0, 1),
         breaks = seq(0, 1, by = 0.25)
       ) +
       ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
       ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
       ggplot2::theme_minimal() +
       ggplot2::theme(
         legend.position = "right",
         panel.grid.minor = ggplot2::element_blank(),
         plot.title = ggplot2::element_text(face = "bold")
       )
     
   } else {
     
     # Create the cumulative entry plot
     plot <- ggplot2::ggplot(survival_data, 
                             ggplot2::aes(x = step, y = entry_rate, 
                                          color = actor, group = actor)) +
       ggplot2::geom_step(size = 1) +
       ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry, 
                                         fill = actor), alpha = 0.2, color = NA) +
       ggplot2::labs(
         x = "Chain Step ID",
         y = "Proportion Entered Market",
         title = "Cumulative Market Entry Curves by Actor",
         subtitle = paste0(conf_level * 100, "% Confidence Intervals")
       ) +
       ggplot2::scale_y_continuous(
         labels = scales::percent_format(),
         limits = c(0, 1),
         breaks = seq(0, 1, by = 0.25)
       ) +
       ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
       ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
       ggplot2::theme_minimal() +
       ggplot2::theme(
         legend.position = "right",
         panel.grid.minor = ggplot2::element_blank(),
         plot.title = ggplot2::element_text(face = "bold")
       )
     
     
   }
    
    print(plot)
  
    # Return list with data and plot
    if(return_data) 
      return(list(
        survival_data = survival_data,
        plot = plot
      ))
  },
  
  
  plot_market_entry_survival_strategy = function(experiment_name = 'market_entry', 
                                                 actor_curves = FALSE,
                                                 cumulative = FALSE, # switch survival (non-entry) rate to cumulative entry_rate 
                                                 conf_level = 0.95,
                                                 return_data=FALSE) {
    if (is.null(self$experiments) || is.null(self$experiments[[experiment_name]]))
      stop('market_entry experiment not available. First call mcsim_market_entry()')
    
    # sim results 
    sim_results <- self$experiments[[experiment_name]]
    
    # Extract data from simulation results
    survival_data <- sim_results$survival_data
    entry_df <- sim_results$entry_df
    first_entries <- sim_results$first_entries
    all_actors <- sim_results$all_actors
    max_step <- sim_results$max_step
    total_sims <- sim_results$total_sims
    
    
    if (!length(survival_data) || !nrow(survival_data)) {
      cat('\nNo Entries to plot.\n')
      return(sim_results)
    }
    
    
    if ( ! cumulative ) {
      
      survival_data$survival_rate <-   1 - survival_data$entry_rate
      survival_data$ci_lower_entry <-  1 - survival_data$ci_lower_entry
      survival_data$ci_upper_entry <-  1 - survival_data$ci_upper_entry
      
      # Create the cumulative entry plot
      if(actor_curves) {
        plot <- ggplot2::ggplot(survival_data, 
                                ggplot2::aes(x = step, y = survival_rate, 
                                             color = strategy, linetype = actor)) 
      } else {
        survival_data <- survival_data %>% group_by(step, strategy) %>% 
          dplyr::summarize(
            survival_rate = mean(survival_rate, na.rm=TRUE),
            entry_rate = mean(entry_rate, na.rm=TRUE),
            ci_lower_entry = mean(ci_lower_entry, na.rm=TRUE),
            ci_upper_entry = mean(ci_upper_entry, na.rm=TRUE)
          )
        plot <- ggplot2::ggplot(survival_data, 
                                ggplot2::aes(x = step, y = survival_rate, 
                                             color = strategy)) 
      }
     
      plot <- plot +  
        ggplot2::geom_step(size = 1) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry, 
                                          fill = strategy), alpha = 0.2, color = NA) +
        ggplot2::labs(
          x = "Chain Step ID",
          y = "Proportion Not Yet Entered Market (Survival Rate)",
          title = sprintf("Market Entry Survival Curves by Strategy%s", ifelse(actor_curves,'',' (Averaged)') ),
          subtitle = paste0(conf_level * 100, "% Confidence Intervals")
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(),
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.25)
        ) +
        # ggplot2::scale_color_brewer(palette = "Set1", name = "St ID") +
        # ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "right",
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold")
        )
      
    } else {
      
      if(actor_curves) {
        plot <- ggplot2::ggplot(survival_data, 
                                ggplot2::aes(x = step, y = entry_rate, 
                                             color = strategy, linetype = actor)) 
      } else {
        survival_data <- survival_data %>% group_by(step, strategy) %>% 
          dplyr::summarize(
            survival_rate = mean(survival_rate, na.rm=TRUE),
            entry_rate = mean(entry_rate, na.rm=TRUE),
            ci_lower_entry = mean(ci_lower_entry, na.rm=TRUE),
            ci_upper_entry = mean(ci_upper_entry, na.rm=TRUE)
          )
        plot <- ggplot2::ggplot(survival_data, 
                                ggplot2::aes(x = step, y = entry_rate, 
                                             color = strategy)) 
      }
      
      # Create the cumulative entry plot
      plot <- plot + 
        ggplot2::geom_step(size = 1) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry, 
                                          fill = strategy), alpha = 0.2, color = NA) +
        ggplot2::labs(
          x = "Chain Step ID",
          y = "Proportion Entered Market",
          title = sprintf("Cumulative Entry Curves by Strategy%s", ifelse(actor_curves,'',' (Averaged)') ),
          subtitle = paste0(conf_level * 100, "% Confidence Intervals")
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(),
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.25)
        ) +
        ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
        ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "right",
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold")
        )
      
    }
    
    print(plot)
    
    # Return list with data and plot
    if(return_data) 
      return(list(
        survival_data = survival_data,
        plot = plot
      ))
  },
  
  
  ####
  plot_market_entry_survival_v0 = function(n=50, 
                                        environ_params=NULL, 
                                        structure_model=NULL, 
                                        steps_per_actor=NULL, 
                                        theta_shocks=NULL, 
                                        conf_level = 0.95,
                                        verbose=FALSE) {
      environ_params <- if (!is.null(self$config_environ_params)){
        self$config_environ_params
      } else if (!is.null(environ_params)) {
        environ_params
      } else {
        stop('missing environ_params and self$config_environ_params')
      }

      structure_model <- if (!is.null(self$config_structure_model)){
        self$config_structure_model
      } else if (!is.null(structure_model)) {
        structure_model
      } else {
        stop('missing structure_model and self$config_structure_model')
      }


    # Initialize lists
    util_list <- list()
    entry_list <- list()
    
    batch_seeds <- sample(1:9999999, size = n, replace = FALSE)
    

    # Run simulations with multiple seeds
    for (i in 1:length(batch_seeds)) {
      run_seed_i <- batch_seeds[i]

      cat(sprintf('\n run %s, seed=%s \n', i, run_seed_i))

      # Run RSiena search using variable parameters in theta_shocks
      env_i <- super$clone(deep = TRUE)

      env_i$search_rsiena(
        structure_model = structure_model,
        iterations_per_actor = steps_per_actor,
        theta_shocks = theta_shocks,
        run_seed = run_seed_i,
        verbose = verbose
      )

      util_list[[as.character(run_seed_i)]] <- env_i$actor_util_df

      # Process network data
      arr <- env_i$bi_env_arr[,,]

      # Convert the 3D array to indices where value is 1
      indices1 <- which(arr == 1, arr.ind = TRUE)
      colnames(indices1) <- c('from', 'to', 'step')

      # Find ties in new markets (entries)
      idx_new <- which(indices1[,2] %in% c(10:12))

      # Store entry data
      if(length(idx_new) > 0) {
        entry_list[[as.character(run_seed_i)]] <- data.frame(
          from = indices1[idx_new, 1],
          to = indices1[idx_new, 2],
          chain_step_id = indices1[idx_new, 3],
          sim_seed = env_i$rsiena_env_seed  # Renamed to avoid column name conflict
        )
      }
    }

  
    # Combine all results
    util_df <- data.table::rbindlist(util_list, idcol = 'run_seed')
    entry_df <- data.table::rbindlist(entry_list, idcol = 'run_seed')
    
    # Get all unique actors and maximum step
    all_actors <- unique(entry_df$from)
    max_step <- max(entry_df$chain_step_id)
    
    # Count total simulations
    total_sims <- length(batch_seeds)
    
    # Find first entry time for each actor in each simulation
    first_entries <- entry_df %>%
      dplyr::group_by(run_seed, from) %>%
      dplyr::summarize(
        first_entry_step = min(chain_step_id),
        .groups = "drop"
      )
    
    # Initialize empty data frame for survival data
    survival_data <- data.frame()
    
    # Calculate z-value outside all loops
    z_value <- qnorm(1 - (1 - conf_level) / 2)
    
    # Process each actor separately
    for (actor_id in all_actors) {
      # Get entries for this actor
      actor_entries <- subset(first_entries, from == actor_id)
      
      # Count actor's simulations
      actor_sim_count <- length(unique(actor_entries$run_seed))
      
      # For each step, calculate survival and entry rates
      for (s in 0:max_step) {
        # For step 0, all simulations have survived
        if (s == 0) {
          surviving_sims <- total_sims
        } else {
          # Count simulations where first entry is after this step or actor never enters
          entered_after_s <- sum(actor_entries$first_entry_step > s)
          never_entered <- total_sims - actor_sim_count
          surviving_sims <- entered_after_s + never_entered
        }
        
        # Calculate rates
        survival_rate <- surviving_sims / total_sims
        entry_rate <- 1 - survival_rate
        
        # Calculate standard errors
        se_entry <- sqrt(entry_rate * (1 - entry_rate) / total_sims)
        
        # Calculate confidence intervals
        ci_lower_entry <- max(0, entry_rate - z_value * se_entry)
        ci_upper_entry <- min(1, entry_rate + z_value * se_entry)
        
        # Create a row for this step and actor
        row_data <- data.frame(
          step = s,
          actor = actor_id,
          total_sims = total_sims,
          surviving_sims = surviving_sims,
          survival_rate = survival_rate,
          entry_rate = entry_rate,
          ci_lower_entry = ci_lower_entry,
          ci_upper_entry = ci_upper_entry
        )
        
        # Add to the combined data frame
        survival_data <- rbind(survival_data, row_data)
      }
    }
    
    # Ensure actor is a factor for proper plotting
    survival_data$actor <- factor(survival_data$actor)
    
    # Create the cumulative entry plot
    plot <- ggplot2::ggplot(survival_data, 
                            ggplot2::aes(x = step, y = entry_rate, 
                                         color = actor, group = actor)) +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry, 
                                        fill = actor), alpha = 0.2, color = NA) +
      ggplot2::labs(
        x = "Chain Step ID",
        y = "Proportion Entered Market",
        title = "Cumulative Market Entry Curves by Actor",
        subtitle = paste0(conf_level * 100, "% Confidence Intervals")
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.25)
      ) +
      ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
      ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold")
      )
    
    # Return list with data and plot
    return(list(
      entry_df = entry_df,
      util_df = util_df,
      survival_data = survival_data,
      plot = plot,
      first_entries = first_entries
    ))
  },
  
  
  compute_FMA_FMB = function(actor_ids=1:2, trim_upto_step=1, max_step=Inf, actor_type_label='Firm') {
    
    if (is.null(self$experiments) || 
        is.null(self$experiments$market_entry) || 
        is.null(self$experiments$market_entry$first_entries))
      stop('self$experiments$market_entry$first_entries is missing')
    
    if (length(actor_ids) < 2)
      stop('less than 2 actor_ids provided.')
    
    actor_id_1 <- actor_ids[1]
    actor_id_2 <- actor_ids[2]
    
    first_entries <- self$experiments$market_entry$first_entries %>% 
      mutate(actor_id = from) %>%  filter(actor_id %in% actor_ids)
    
    util_df <- self$experiments$market_entry$util_df %>%  filter(actor_id %in% actor_ids)
    
    mean_ent_times <- first_entries %>% group_by(from) %>% summarise(mean=mean(first_entry_step))
    # first_entries %>% ggplot(aes(x=first_entry_step, color=factor(from),fill=factor(from))) + 
    
    
    first_mover <- first_entries %>% 
      group_by(run_seed) %>% 
      slice_min(first_entry_step, n = 1) %>% ## min entry timing row (first mover) for each run_seed
      ungroup()

    # FMA (advantage) = [observed comparison; bad] performance of firm i first-mover vs. firm j second-mover
    # FMB (benefit)   = [counterfactual scenarios] performance of firm i as first-mover vs. follower; 
    scenario_a_12 <- first_mover %>% filter(from== actor_id_1 ) %>% pull(run_seed)
    scenario_b_21 <- first_mover %>% filter(from== actor_id_2 ) %>% pull(run_seed)
    
    if (!length(scenario_a_12) || !length(scenario_b_21)) {
      cat(sprintf('\nNo Comparison due to zero market entries: %s %s\n Try running more iterations or respecifying structure model\n', 
                  ifelse(!length(scenario_a_12),'scenario_a_12',''),
                  ifelse(!length(scenario_b_21),'scenario_b_21','')))
      return()
    }
    ##---------------------------
    
    u_a_12 <- util_df %>% filter(run_seed %in% scenario_a_12)
    u_b_21 <- util_df %>% filter(run_seed %in% scenario_b_21)

    # utility dataframe trimmed
    udf_trim <- util_df %>% 
      filter(chain_step_id > trim_upto_step) %>%
      filter(chain_step_id < max_step) %>%
      mutate(
        actor_id_label = paste(actor_type_label, actor_id),
        entry_scenario = case_when(
          run_seed %in% scenario_a_12 ~ "Scenario_A_1_2",
          run_seed %in% scenario_b_21 ~ "Scenario_B_2_1",
          TRUE ~ "Other"  # Default value for any run_seed not in the specified vectors
        )
      )
    
    ##----- Split density PLots comparison ---------
    plt_fma <- udf_trim %>% 
      ggplot(aes(x=utility, fill=actor_id_label, color=actor_id_label, linetype = entry_scenario)) + 
      geom_density(alpha=.12, size=1) + 
      facet_wrap(~entry_scenario) + theme_bw() +
      ggtitle('First Mover Advantage (FMA) by Entry Order Scenario')
    
    plt_fmb <- udf_trim %>% 
      ggplot(aes(x=utility, fill=actor_id_label, color=actor_id_label, linetype = entry_scenario)) + 
      geom_density(alpha=.12, size=1) + 
      facet_wrap(~actor_id_label) + theme_bw() +
      ggtitle('First Mover Benefit (FMB) by Firm [Counterfactual]')
    
    
    ##========================== FMB =====================================
    
    udf_trim_1 <- udf_trim %>% filter(actor_id == actor_id_1) %>% 
      mutate(treat = as.integer(entry_scenario == 'Scenario_A_1_2'))
    
    udf_trim_2 <- udf_trim %>% filter(actor_id == actor_id_2) %>% 
      mutate(treat = as.integer(entry_scenario == 'Scenario_B_2_1'))
    
    m.out1 <- matchit(treat ~ entry_scenario + utility,
                      data = udf_trim_1,
                      distance = "glm",
                      link = "probit",
                      replace = FALSE)
    m.out2 <- matchit(treat ~ entry_scenario + utility,
                      data = udf_trim_2,
                      distance = "glm",
                      link = "probit",
                      replace = FALSE)

    ###------- Actor 1 -----------------
    match_dat_1 <- match_data(m.out1, data=udf_trim_1) 
    match_dat_1$pair_id <- match_dat_1$subclass
  
    diffs_1 <- match_dat_1 %>%
      group_by(pair_id) %>%
      dplyr::summarize(utility_diff = utility[treat==1] - utility[treat==0])

    diff_test_1 <- t.test(diffs_1$utility_diff)
    diff_test_1_pval_str <- ifelse(diff_test_1$p.value < 0.001, '< 0.001', sprintf('= %.3f', diff_test_1$p.value))
    
    
    plt_diff_fmb_1 <- ggplot(diffs_1, aes(x = utility_diff)) +
      geom_histogram(bins=25, fill='steelblue', color='white', alpha=0.7) +
      geom_vline(xintercept=mean(diffs_1$utility_diff), color="red", linetype="dashed", size=1) +
      geom_vline(xintercept=diff_test_1$conf.int, color="red", linetype=3, size=1) +
      geom_vline(xintercept=0, color='black', linetype=1, size=.5) +
      theme_minimal() +
      labs(title = sprintf("Distribution of First Mover Benefit: Actor %s \nMatched Pairs t-Test, n=%s, est=%.2f, p %s\n ", 
                           'i', length(unique(diffs_1$pair_id)), diff_test_1$estimate, diff_test_1_pval_str ),
           x = "First Mover Benefit (vs. Counterfactual)",
           y = "Frequency")
    
    
    ###------- Actor 2 -----------------
    match_dat_2 <- match_data(m.out2, data=udf_trim_2) 
    match_dat_2$pair_id <- match_dat_2$subclass
    
    diffs_2 <- match_dat_2 %>%
      group_by(pair_id) %>%
      dplyr::summarize(utility_diff = utility[treat==1] - utility[treat==0])
    
    diff_test_2 <- t.test(diffs_2$utility_diff)
    diff_test_2_pval_str <- ifelse(diff_test_2$p.value < 0.001, '< 0.001', sprintf('= %.3f', diff_test_2$p.value))
    
    
    plt_diff_fmb_2 <- ggplot(diffs_2, aes(x = utility_diff)) +
      geom_histogram(bins=25, fill='steelblue', color='white', alpha=0.7) +
      geom_vline(xintercept=mean(diffs_2$utility_diff), color="red", linetype="dashed", size=1) +
      geom_vline(xintercept=diff_test_2$conf.int, color="red", linetype=3, size=1) +
      geom_vline(xintercept=0, color='black', linetype=1, size=.5) +
      theme_minimal() +
      labs(title = sprintf("Distribution of First Mover Benefit: Actor %s \nMatched Pairs t-Test, n=%s, est=%.2f, p %s\n ", 
                           'j', length(unique(diffs_2$pair_id)), diff_test_2$estimate, diff_test_2_pval_str ),
           x = "First Mover Benefit (vs. Counterfactual)",
           y = "Frequency")
    
    
    ##========================== FMA =====================================
    
    
    udf_trim_A <- udf_trim %>% filter(entry_scenario == 'Scenario_A_1_2') %>% 
      mutate(treat = as.integer(actor_id == actor_id_1))
    
    udf_trim_B <- udf_trim %>% filter(entry_scenario == 'Scenario_B_2_1') %>% 
      mutate(treat = as.integer(actor_id == actor_id_2))
    
    m.outA <- matchit(treat ~ actor_id + utility,
                      data = udf_trim_A,
                      distance = "glm",
                      link = "probit",
                      replace = FALSE)
    m.outB <- matchit(treat ~ actor_id + utility,
                      data = udf_trim_B,
                      distance = "glm",
                      link = "probit",
                      replace = FALSE)
    
    ###------- Scenario A (1 first, 2 second) -----------------
    match_dat_A <- match_data(m.outA, data=udf_trim_A) 
    match_dat_A$pair_id <- match_dat_A$subclass
    
    diffs_A <- match_dat_A %>%
      group_by(pair_id) %>%
      dplyr::summarize(utility_diff = utility[treat==1] - utility[treat==0])
    
    diff_test_A <- t.test(diffs_A$utility_diff)
    diff_test_A_pval_str <- ifelse(diff_test_A$p.value < 0.001, '< 0.001', sprintf('= %.3f', diff_test_A$p.value))
    
    
    plt_diff_fma_A <- ggplot(diffs_A, aes(x = utility_diff)) +
      geom_histogram(bins=25, fill='steelblue', color='white', alpha=0.7) +
      geom_vline(xintercept=mean(diffs_A$utility_diff), color="red", linetype="dashed", size=1) +
      geom_vline(xintercept=diff_test_A$conf.int, color="red", linetype=3, size=1) +
      geom_vline(xintercept=0, color='black', linetype=1, size=.5) +
      theme_minimal() +
      labs(title = sprintf("Distribution of First Mover Advantage: Scenario A \nMatched Pairs t-Test, n=%s, est=%.2f, p %s\n ", 
                             length(unique(diffs_A$pair_id)), diff_test_A$estimate, diff_test_A_pval_str ),
           x = "First Mover Advantage",
           y = "Frequency")
    
    ###------- Scenario B (2 first, 1 second) -----------------
    match_dat_B <- match_data(m.outB, data=udf_trim_B) 
    match_dat_B$pair_id <- match_dat_B$subclass
    
    diffs_B <- match_dat_B %>%
      group_by(pair_id) %>%
      dplyr::summarize(utility_diff = utility[treat==1] - utility[treat==0])
    
    diff_test_B <- t.test(diffs_B$utility_diff)
    diff_test_B_pval_str <- ifelse(diff_test_B$p.value < 0.001, '< 0.001', sprintf('= %.3f', diff_test_B$p.value))
    
    
    plt_diff_fma_B <- ggplot(diffs_B, aes(x = utility_diff)) +
      geom_histogram(bins=25, fill='steelblue', color='white', alpha=0.7) +
      geom_vline(xintercept=mean(diffs_B$utility_diff), color="red", linetype="dashed", size=1) +
      geom_vline(xintercept=diff_test_B$conf.int, color="red", linetype=3, size=1) +
      geom_vline(xintercept=0, color='black', linetype=1, size=.5) +
      theme_minimal() +
      labs(title = sprintf("Distribution of First Mover Advantage: Scenario B \nMatched Pairs t-Test, n=%s, est=%.2f, p %s\n ", 
                           length(unique(diffs_B$pair_id)), diff_test_B$estimate, diff_test_B_pval_str ),
           x = "First Mover Advantage",
           y = "Frequency")
    
    
    ##------------------- Combined Plots ------------------------------------
    
    
    plt_diff_combined <- ggarrange(plotlist=list(plt_diff_fmb_1, 
                                                 plt_diff_fmb_2, 
                                                 plt_diff_fma_A, 
                                                 plt_diff_fma_B), nrow=2, ncol=2)
    # Add common title
    combined_title_str <- "Deviation of First Mover Advantage from First Mover Benefit"
    plt_diff_combined <- ggpubr::annotate_figure(plt_diff_combined, 
                                     top = ggpubr::text_grob(combined_title_str, face = "bold", size = 14)) ##face = "bold"
    
    
    ####
    plt_fma_fmb_combined <- ggarrange(plotlist=list(plt_fmb,
                                                    plt_fma), nrow=2, ncol=1)
    # Add common title
    combined_title_str2 <- "First Mover Benefit vs. First Mover Advantage"
    plt_fma_fmb_combined <- ggpubr::annotate_figure(plt_fma_fmb_combined, 
                                         top = ggpubr::text_grob(combined_title_str2, face = "bold", size = 14)) ##face = "bold"
    
    
    ##=====================
      
    return(list(
      plot_diff_combined = plt_diff_combined,
      plot_fma_fmb_combined = plt_fma_fmb_combined,
      plot_comparison = list(
        plt_fma = plt_fma,
        plt_fmb = plt_fmb
      ),
      plot_diffs = list(
        plt_diff_fmb_1 = plt_diff_fmb_1,
        plt_diff_fmb_2 = plt_diff_fmb_2,
        plt_diff_fma_A = plt_diff_fma_A,
        plt_diff_fma_B = plt_diff_fma_B
      ),
      data_trim = list(
        udf_trim_1 = udf_trim_1,
        udf_trim_2 = udf_trim_2,
        udf_trim_A = udf_trim_A,
        udf_trim_B = udf_trim_B
      ),
      match_data = list(
        match_dat_1 = match_dat_1,
        match_dat_2 = match_dat_2,
        match_dat_A = match_dat_A,
        match_dat_B = match_dat_B
      ),
      diffs = list(
        diffs_1 = diffs_1,
        diffs_2 = diffs_2,
        diffs_A = diffs_A,
        diffs_B = diffs_B
      ),
      tests = list(
        diff_test_1 = diff_test_1,
        diff_test_2 = diff_test_2,
        diff_test_A = diff_test_A,
        diff_test_B = diff_test_B
      )
    ))
    
  }, 
  
  
  # Exploration/Exploitation plot with strategy logic consistent with plot_actor_utility_strategy_summary
  plot_exploration_exploitation_consistent = function(
    actor_ids = c(),
    thin_factor = 1,
    thin_pct = 1,
    smooth_method = 'loess',
    show_points = TRUE,
    show_individuals = FALSE,
    show_group_means = TRUE,
    loess_span = 0.3,
    point_alpha_dimmer = 1,
    line_alpha = 0.5,
    group_line_size = 2,
    se_ribbon = TRUE,
    ylim = c(0, 1),
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
  ) {

    # Get actor strategies - matching the utility plot logic
    if (!identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS'))
      stop("Actor Strategy self$strat_1_coCovar not set.")
    
    actor_strat <- self$get_actor_strategies()
    
    # Get bipartite array data
    bi_env_arr <- self$bi_env_arr
    n_steps <- dim(bi_env_arr)[3]
    n_actors <- self$M
    n_components <- self$N
    
    # Define old (exploitation) and new (exploration) components
    old_components <- 1:8
    new_components <- 9:16
    
    # Get unique chain steps
    unique_steps <- sort(unique(self$chain_stats$chain_step_id))
    
    # Calculate exploration/exploitation metrics
    metrics_list <- list()
    
    for (t_idx in 1:min(n_steps, length(unique_steps))) {
      incidence_t <- bi_env_arr[, , t_idx]
      
      for (i in 1:n_actors) {
        actor_activities <- which(incidence_t[i, ] > 0)
        
        if (length(actor_activities) > 0) {
          n_old <- sum(actor_activities %in% old_components)
          n_new <- sum(actor_activities %in% new_components)
          n_total <- length(actor_activities)
          
          metrics_list[[length(metrics_list) + 1]] <- data.frame(
            chain_step_id = unique_steps[t_idx],
            actor_id = i,
            n_total_activities = n_total,
            n_old_activities = n_old,
            n_new_activities = n_new,
            prop_exploration = n_new / n_total,
            prop_exploitation = n_old / n_total
          )
        }
      }
    }
    
    # Combine metrics and add strategy - matching utility plot logic
    metrics_df <- bind_rows(metrics_list) %>%
      mutate(
        strategy = actor_strat[actor_id],  # Same indexing as utility plot
        strategy = factor(strategy)
      )
    
    # Apply thinning - matching utility plot
    if (thin_factor > 1) {
      metrics_df <- metrics_df %>% 
        filter(chain_step_id %% thin_factor == 0)
    }
    
    if (thin_pct < 1) {
      sample_rows <- sample(1:nrow(metrics_df), size = round(nrow(metrics_df) * thin_pct), replace = FALSE)
      metrics_df <- metrics_df[sample_rows, ]
    }
    
    # Filter actors if specified
    if (length(actor_ids) > 0) {
      metrics_df <- metrics_df %>% 
        filter(actor_id %in% actor_ids)
    }
    
    # Calculate plot parameters matching utility plot
    npoints <- nrow(metrics_df)
    point_size <- 6 / log10(npoints)
    point_alpha <- min(1, 2.2/log(npoints)) * point_alpha_dimmer
    
    # Reshape to long format
    plot_data <- metrics_df %>%
      pivot_longer(
        cols = c(prop_exploration, prop_exploitation),
        names_to = "activity_type",
        values_to = "proportion"
      ) %>%
      mutate(
        activity_type = case_when(
          activity_type == "prop_exploration" ~ "Exploration",
          activity_type == "prop_exploitation" ~ "Exploitation"
        ),
        activity_label = factor(activity_type, levels = c("Exploitation", "Exploration")),
        actor_id_factor = factor(actor_id)
      )
    
    # Calculate group means by strategy and activity type
    group_means <- plot_data %>%
      group_by(chain_step_id, strategy, activity_type, activity_label) %>%
      summarise(
        mean_proportion = mean(proportion, na.rm = TRUE),
        se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
        n_obs = n(),
        .groups = "drop"
      )
    
    # Create main plot
    plt <- ggplot()
    
    # Add individual points if requested - color by strategy (matching utility plot)
    if (show_points) {
      plt <- plt + geom_point(
        data = plot_data,
        aes(x = chain_step_id, 
            y = proportion, 
            color = strategy),
        alpha = point_alpha,
        shape = 1,  # Open circle like utility plot
        size = point_size
      )
    }
    
    # Add individual lines if requested
    if (show_individuals) {
      plt <- plt + geom_line(
        data = plot_data,
        aes(x = chain_step_id, 
            y = proportion,
            color = strategy,
            linetype = activity_label,
            group = interaction(actor_id, activity_type)),
        alpha = line_alpha * 0.3,
        size = 0.5
      )
    }
    
    # Add group mean lines
    if (show_group_means) {
      # Add confidence ribbons
      if (se_ribbon) {
        plt <- plt + geom_ribbon(
          data = group_means,
          aes(x = chain_step_id,
              ymin = pmax(0, mean_proportion - se_proportion),
              ymax = pmin(1, mean_proportion + se_proportion),
              fill = strategy,
              group = interaction(strategy, activity_label)),
          alpha = 0.15
        )
      }
      
      # Add mean lines - color by strategy, linetype by activity
      plt <- plt + geom_line(
        data = group_means,
        aes(x = chain_step_id, 
            y = mean_proportion,
            color = strategy,
            linetype = activity_label,
            group = interaction(strategy, activity_label)),
        size = group_line_size
      )
      
      # Add smoothed trends
      if (!is.null(smooth_method) && self$exists(smooth_method)) {
        plt <- plt + geom_smooth(
          data = group_means,
          aes(x = chain_step_id, 
              y = mean_proportion,
              color = strategy,
              linetype = activity_label,
              group = interaction(strategy, activity_label)),
          method = smooth_method,
          span = loess_span,
          se = FALSE,
          size = 0.8,
          alpha = 0.6
        )
      }
    }
    
    # Add population mean (black line) - similar to utility plot
    pop_mean <- plot_data %>%
      group_by(chain_step_id, activity_type) %>%
      summarise(
        pop_mean_prop = mean(proportion, na.rm = TRUE),
        .groups = "drop"
      )
    
    plt <- plt + geom_smooth(
      data = pop_mean,
      aes(x = chain_step_id, y = pop_mean_prop, linetype = activity_type),
      method = smooth_method,
      span = loess_span,
      color = 'black',
      alpha = 0.1,
      size = 1
    )
    
    # Add shock rectangles if they exist (matching utility plot)
    if (!is.null(self$theta_shocks)) {
      suppressMessages({
        layout <- ggplot_build(plt)$layout
      })
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks) %>%
        mutate(proportion = 0, chain_step_id = 0)
      
      y_maxs <- rep(ylim[2] * 0.95, nrow(shock_rects))
      
      plt <- plt + 
        geom_rect(
          data = shock_rects, 
          aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
          fill = 'darkorange', 
          color = 'orange',
          linetype = 2,
          alpha = 0.05
        ) +
        geom_text(
          data = shock_rects, 
          aes(x = (start + end) / 2, y = y_maxs, label = label),
          vjust = 0, 
          size = 3
        )
    }
    
    # Get parameter string for title (matching utility plot)
    params <- self$get_structure_model_params()
    sim_title_str <- self$get_structure_model_param_str(params)
    
    # Customize appearance
    plt <- plt +
      scale_linetype_manual(
        name = "Activity Type",
        values = c("Exploitation" = "solid",
                   "Exploration" = "dashed"),
        drop = FALSE
      ) +
      labs(
        title = sim_title_str,
        subtitle = sprintf("Exploration (New Activities: C%d-C%d) vs Exploitation (Old Activities: C%d-C%d)",
                           min(new_components), max(new_components),
                           min(old_components), max(old_components)),
        x = "Simulation Step",
        y = "Proportion of Activities"
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.key.width = unit(0.8, "cm"),
        legend.spacing.x = unit(0.3, "cm")
      ) +
      coord_cartesian(ylim = ylim) +
      scale_y_continuous(labels = scales::percent) +
      guides(color = guide_legend(nrow = 1))  # Single row legend like utility plot
    
    # For side density plot (optional, matching utility plot style)
    if (plot_return) {
      # Calculate strategy means
      stratmeans <- plot_data %>% 
        group_by(strategy, activity_type) %>% 
        summarise(
          n = n(), 
          mean = mean(proportion, na.rm = TRUE), 
          sd = sd(proportion, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Create density plot
      nrows_title <- stringr::str_count(sim_title_str, "\\\n")
      
      plt2 <- ggplot(plot_data, aes(x = proportion, color = strategy, fill = strategy)) +
        geom_density(alpha = 0.1, linewidth = 1) +
        geom_vline(
          data = stratmeans, 
          aes(xintercept = mean, color = strategy), 
          linetype = 2, 
          linewidth = 0.9
        ) +
        facet_wrap(~activity_type, ncol = 1) +
        labs(y = '', x = '') +
        xlim(ylim) +
        coord_flip() +
        ylab('Proportion Density') +
        theme_bw() + 
        theme(
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'none',
          plot.margin = unit(c(5.5, 5.5, 5.5, -23), 'pt'),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) + 
        ggtitle(paste(rep('\n', nrows_title), collapse = ''))
      
      # Combine plots
      suppressMessages({
        combined_plot <- ggarrange(
          plt, plt2,
          ncol = 2,
          widths = c(4.1, 0.9),
          common.legend = TRUE,
          legend = "bottom"
        )
      })
      
      # Save if requested
      if (plot_save) {
        plot_file <- paste0('explore_exploit_consistent_', plot_file, round(as.numeric(Sys.time()) * 10))
        ggsave(
          filename = file.path(
            ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
            sprintf("%s_%s.jpeg", self$config_environ_params$name, plot_file)
          ),
          combined_plot,
          width = 10,
          height = 7,
          units = 'in',
          dpi = 400
        )
      }
      
      return(combined_plot)
    }
    
    return(plt)
  },
  
  
  # R6 method for plotting exploration vs exploitation in SAOM NK simulation
  # Add this method to your SaoMNK R6 class
  
  plot_exploration_exploitation = function(
    actor_ids = c(),
    thin_factor = 1,
    thin_pct = 1,
    smooth_method = 'loess',
    show_points = TRUE,
    show_individuals = TRUE,
    show_group_means = TRUE,
    loess_span = 0.3,
    point_alpha_dimmer = 1,
    line_alpha = 0.5,
    group_line_size = 2,
    ylim = NULL,
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
  ) {
    
    # Get bipartite network data from chain
    bi_env_arr <- self$bi_env_arr
    n_steps <- dim(bi_env_arr)[3]
    n_actors <- self$M
    n_components <- self$N
    
    # Define old (initial) and new components
    # Based on your setup: C1-C8 are initial (old), C9-C16 are new
    old_components <- 1:8
    new_components <- 9:16
    
    # Get unique chain steps
    unique_steps <- sort(unique(self$chain_stats$chain_step_id))
    
    # Calculate exploration/exploitation metrics for each actor at each step
    metrics_list <- list()
    
    for (t_idx in 1:min(n_steps, length(unique_steps))) {
      if (t_idx > dim(bi_env_arr)[3]) break
      
      incidence_t <- bi_env_arr[, , t_idx]
      
      for (i in 1:n_actors) {
        # Get activities for this actor at this time
        actor_activities <- which(incidence_t[i, ] > 0)
        
        if (length(actor_activities) > 0) {
          n_old <- sum(actor_activities %in% old_components)
          n_new <- sum(actor_activities %in% new_components)
          n_total <- length(actor_activities)
          
          metrics_list[[length(metrics_list) + 1]] <- data.frame(
            chain_step_id = unique_steps[t_idx],
            actor_id = as.character(i),
            n_total_activities = n_total,
            n_old_activities = n_old,
            n_new_activities = n_new,
            prop_exploration = n_new / n_total,
            prop_exploitation = n_old / n_total,
            exploration_score = n_new / n_total,  # Simple proportion
            exploitation_score = n_old / n_total
          )
        }
      }
    }
    
    # Combine metrics
    metrics_df <- bind_rows(metrics_list)
    
    # Add actor strategies
    if (exists('strat_1_coCovar', where = self)) {
      actor_strat <- self$get_actor_strategies()
      metrics_df$strategy <- actor_strat[as.numeric(metrics_df$actor_id)]
    } else {
      metrics_df$strategy <- "0"  # Default if no strategy
    }
    
    # Filter actors if specified
    if (length(actor_ids) > 0) {
      metrics_df <- metrics_df %>% filter(actor_id %in% actor_ids)
    }
    
    # Apply thinning
    metrics_df <- metrics_df %>% 
      filter(chain_step_id %% thin_factor == 0)
    
    if (thin_pct < 1) {
      sample_rows <- sample(1:nrow(metrics_df), size = round(nrow(metrics_df) * thin_pct), replace = FALSE)
      metrics_df <- metrics_df[sample_rows, ]
    }
    
    # Reshape to long format for plotting
    plot_data <- metrics_df %>%
      pivot_longer(
        cols = c(exploration_score, exploitation_score),
        names_to = "behavior_type",
        values_to = "score"
      ) %>%
      mutate(
        behavior_type = case_when(
          behavior_type == "exploration_score" ~ "Exploration",
          behavior_type == "exploitation_score" ~ "Exploitation"
        ),
        strategy_label = paste("Strategy", strategy)
      )
    
    # Calculate group means
    group_means <- plot_data %>%
      group_by(chain_step_id, strategy, strategy_label, behavior_type) %>%
      summarise(
        mean_score = mean(score, na.rm = TRUE),
        se_score = sd(score, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Create plot
    p <- ggplot()
    
    # Add individual points if requested
    if (show_points) {
      p <- p + geom_point(
        data = plot_data,
        aes(x = chain_step_id, y = score, color = behavior_type),
        alpha = point_alpha_dimmer * 0.3,
        size = 1
      )
    }
    
    # Add individual lines if requested
    if (show_individuals) {
      p <- p + geom_line(
        data = plot_data,
        aes(x = chain_step_id, y = score, 
            color = behavior_type,
            group = interaction(actor_id, behavior_type)),
        alpha = line_alpha * 0.3,
        size = 0.5
      )
    }
    
    # Add group mean lines
    if (show_group_means) {
      # Add confidence ribbons
      p <- p + geom_ribbon(
        data = group_means,
        aes(x = chain_step_id,
            ymin = mean_score - se_score,
            ymax = mean_score + se_score,
            fill = behavior_type,
            group = interaction(strategy_label, behavior_type)),
        alpha = 0.15
      )
      
      # Add mean lines with different linetypes for strategies
      p <- p + geom_line(
        data = group_means,
        aes(x = chain_step_id, 
            y = mean_score,
            color = behavior_type,
            linetype = strategy_label,
            group = interaction(strategy_label, behavior_type)),
        size = group_line_size
      )
      
      # Add smoothed trends if requested
      if (!is.null(smooth_method)) {
        p <- p + geom_smooth(
          data = group_means,
          aes(x = chain_step_id, 
              y = mean_score,
              color = behavior_type,
              linetype = strategy_label,
              group = interaction(strategy_label, behavior_type)),
          method = smooth_method,
          span = loess_span,
          se = FALSE,
          size = 0.8,
          alpha = 0.6
        )
      }
    }
    
    # Add shock rectangles if they exist
    if (!is.null(self$theta_shocks)) {
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks)
      p <- p + 
        geom_rect(
          data = shock_rects, 
          aes(xmin = start, xmax = end), 
          ymin = -Inf, ymax = Inf,
          fill = 'darkorange', 
          alpha = 0.05
        ) +
        geom_text(
          data = shock_rects, 
          aes(x = (start + end) / 2, y = ifelse(is.null(ylim), 0.95, ylim[2] * 0.95), 
              label = label),
          vjust = 0, 
          size = 3
        )
    }
    
    # Customize appearance
    p <- p +
      scale_color_manual(
        name = "Activity Type",
        values = c("Exploration" = "#E74C3C", 
                   "Exploitation" = "#3498DB")
      ) +
      scale_fill_manual(
        name = "Activity Type",
        values = c("Exploration" = "#E74C3C", 
                   "Exploitation" = "#3498DB"),
        guide = "none"
      ) +
      scale_linetype_manual(
        name = "Strategy Group",
        values = c("Strategy 0" = "solid",
                   "Strategy 100" = "dashed")
      ) +
      labs(
        title = "Exploration vs Exploitation Dynamics",
        subtitle = sprintf("M=%d actors, N=%d components (Old: C1-C8, New: C9-C16)", 
                           self$M, self$N),
        x = "Simulation Step",
        y = "Proportion of Activities"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.minor = element_blank()
      ) +
      coord_cartesian(ylim = ylim) +
      scale_y_continuous(labels = scales::percent)
    
    # Add parameter information to title
    params <- self$get_structure_model_params()
    sim_title_str <- self$get_structure_model_param_str(params)
    p <- p + ggtitle(sim_title_str, 
                     subtitle = sprintf("Exploration (New Activities: C%d-C%d) vs Exploitation (Old Activities: C%d-C%d)",
                                        min(new_components), max(new_components),
                                        min(old_components), max(old_components)))
    
    # Save if requested
    if (plot_save) {
      plot_file <- paste0('explore_exploit_', plot_file, round(as.numeric(Sys.time()) * 10))
      ggsave(
        filename = file.path(
          ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
          sprintf("%s_%s.jpeg", self$config_environ_params$name, plot_file)
        ),
        p,
        width = 10, 
        height = 6, 
        units = 'in', 
        dpi = 400
      )
    }
    
    if (plot_return) {
      return(p)
    }
  },
  
  # Alternative function for phase space plot (exploration vs exploitation scatter)
  plot_exploration_exploitation_phase = function(
    time_window = 50,
    show_trajectories = TRUE,
    plot_return = TRUE
  ) {
    
    # Get the metrics using the same calculation as above
    bi_env_arr <- self$bi_env_arr
    old_components <- 1:8
    new_components <- 9:16
    
    # Calculate metrics (same as above but simplified)
    metrics_list <- list()
    unique_steps <- sort(unique(self$chain_stats$chain_step_id))
    
    for (t_idx in 1:min(dim(bi_env_arr)[3], length(unique_steps))) {
      incidence_t <- bi_env_arr[, , t_idx]
      
      for (i in 1:self$M) {
        actor_activities <- which(incidence_t[i, ] > 0)
        if (length(actor_activities) > 0) {
          metrics_list[[length(metrics_list) + 1]] <- data.frame(
            chain_step_id = unique_steps[t_idx],
            actor_id = as.character(i),
            exploration = sum(actor_activities %in% new_components) / length(actor_activities),
            exploitation = sum(actor_activities %in% old_components) / length(actor_activities)
          )
        }
      }
    }
    
    metrics_df <- bind_rows(metrics_list)
    
    # Add strategy
    if (exists('strat_1_coCovar', where = self)) {
      actor_strat <- self$get_actor_strategies()
      metrics_df$strategy <- factor(actor_strat[as.numeric(metrics_df$actor_id)])
    }
    
    # Create time periods
    metrics_df <- metrics_df %>%
      mutate(
        time_period = cut(chain_step_id, 
                          breaks = seq(0, max(chain_step_id), by = time_window),
                          include.lowest = TRUE,
                          labels = FALSE)
      )
    
    # Create phase space plot
    p <- ggplot(metrics_df, aes(x = exploitation, y = exploration)) +
      geom_abline(intercept = 1, slope = -1, linetype = "dashed", color = "gray50") +
      geom_point(aes(color = factor(time_period)), alpha = 0.6, size = 2) +
      scale_color_viridis_d(name = "Time Period") +
      coord_fixed() +
      xlim(0, 1) + ylim(0, 1) +
      labs(
        title = "Actor Positions in Exploration-Exploitation Space",
        x = "Exploitation (proportion old activities)",
        y = "Exploration (proportion new activities)"
      ) +
      theme_minimal()
    
    # Add trajectories if requested
    if (show_trajectories) {
      p <- p + geom_path(
        aes(group = actor_id, color = factor(time_period)),
        alpha = 0.3, 
        size = 0.5
      )
    }
    
    # Add strategy facets if exists
    if ("strategy" %in% names(metrics_df)) {
      p <- p + facet_wrap(~paste("Strategy", strategy))
    }
    
    # Add region labels
    p <- p +
      annotate("text", x = 0.8, y = 0.15, label = "Exploitation\nFocus", 
               hjust = 0.5, color = "gray50", fontface = "italic") +
      annotate("text", x = 0.15, y = 0.8, label = "Exploration\nFocus", 
               hjust = 0.5, color = "gray50", fontface = "italic") +
      annotate("text", x = 0.5, y = 0.5, label = "Balanced", 
               hjust = 0.5, color = "gray50", fontface = "italic", angle = -45)
    
    if (plot_return) {
      return(p)
    }
  }, 
  
  
  # Improved exploration/exploitation plot with better visual design
  plot_exploration_exploitation_improved = function(
    actor_ids = c(),
    thin_factor = 1,
    show_points = FALSE,  # Default to FALSE for cleaner look
    show_individuals = FALSE,
    show_group_means = TRUE,
    show_difference = TRUE,  # New: show difference between strategies
    loess_span = 0.2,  # Smaller span for more responsive smoothing
    point_alpha = 0.2,
    line_alpha = 0.5,
    group_line_size = 2.5,  # Thicker lines
    se_ribbon = TRUE,
    ylim = c(0, 1),
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
  ) {

    # Get actor strategies
    if (!identical(attr(self$strat_1_coCovar, 'nodeSet'), 'ACTORS'))
      stop("Actor Strategy self$strat_1_coCovar not set.")
    
    actor_strat <- self$get_actor_strategies()
    
    # Get bipartite array data
    bi_env_arr <- self$bi_env_arr
    n_steps <- dim(bi_env_arr)[3]
    n_actors <- self$M
    n_components <- self$N
    
    # Define components
    old_components <- 1:8
    new_components <- 9:16
    
    # Get unique chain steps
    unique_steps <- sort(unique(self$chain_stats$chain_step_id))
    
    # Calculate metrics (same as before)
    metrics_list <- list()
    
    for (t_idx in 1:min(n_steps, length(unique_steps))) {
      incidence_t <- bi_env_arr[, , t_idx]
      
      for (i in 1:n_actors) {
        actor_activities <- which(incidence_t[i, ] > 0)
        
        if (length(actor_activities) > 0) {
          n_old <- sum(actor_activities %in% old_components)
          n_new <- sum(actor_activities %in% new_components)
          n_total <- length(actor_activities)
          
          metrics_list[[length(metrics_list) + 1]] <- data.frame(
            chain_step_id = unique_steps[t_idx],
            actor_id = i,
            n_total_activities = n_total,
            n_old_activities = n_old,
            n_new_activities = n_new,
            prop_exploration = n_new / n_total,
            prop_exploitation = n_old / n_total
          )
        }
      }
    }
    
    # Combine metrics
    metrics_df <- bind_rows(metrics_list) %>%
      mutate(
        strategy = factor(actor_strat[actor_id], levels = c("0", "100")),
        strategy_label = ifelse(strategy == "0", "Control (0)", "Subsidized (100)")
      )
    
    # Apply thinning
    if (thin_factor > 1) {
      metrics_df <- metrics_df %>% 
        filter(chain_step_id %% thin_factor == 0)
    }
    
    # Filter actors if specified
    if (length(actor_ids) > 0) {
      metrics_df <- metrics_df %>% 
        filter(actor_id %in% actor_ids)
    }
    
    # Reshape to long format
    plot_data <- metrics_df %>%
      pivot_longer(
        cols = c(prop_exploration, prop_exploitation),
        names_to = "activity_type",
        values_to = "proportion"
      ) %>%
      mutate(
        activity_type = case_when(
          activity_type == "prop_exploration" ~ "Exploration",
          activity_type == "prop_exploitation" ~ "Exploitation"
        )
      )
    
    # Calculate group means
    group_means <- plot_data %>%
      group_by(chain_step_id, strategy, strategy_label, activity_type) %>%
      summarise(
        mean_proportion = mean(proportion, na.rm = TRUE),
        se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
        n_obs = n(),
        .groups = "drop"
      )
    
    # Get parameter string
    params <- self$get_structure_model_params()
    sim_title_str <- self$get_structure_model_param_str(params)
    
    # Create improved main plot
    p_main <- ggplot()
    
    # Add shock rectangles first (background)
    if (!is.null(self$theta_shocks)) {
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks)
      p_main <- p_main + 
        geom_rect(
          data = shock_rects, 
          aes(xmin = start, xmax = end), 
          ymin = -Inf, ymax = Inf,
          fill = '#FFA500', 
          alpha = 0.1
        ) +
        geom_vline(
          data = shock_rects,
          aes(xintercept = start),
          linetype = "dotted",
          color = "darkorange",
          size = 0.8
        ) +
        annotate(
          "text",
          x = shock_rects$start[1] + (shock_rects$end[1] - shock_rects$start[1])/2,
          y = ylim[2] * 0.98,
          label = shock_rects$label[1],
          size = 3.5,
          fontface = "bold",
          color = "darkorange"
        )
    }
    
    # Add individual points if requested
    if (show_points) {
      p_main <- p_main + geom_point(
        data = plot_data,
        aes(x = chain_step_id, y = proportion, color = strategy_label),
        alpha = point_alpha,
        size = 0.8
      )
    }
    
    # Add confidence ribbons
    if (se_ribbon) {
      p_main <- p_main + geom_ribbon(
        data = group_means,
        aes(x = chain_step_id,
            ymin = pmax(0, mean_proportion - se_proportion),
            ymax = pmin(1, mean_proportion + se_proportion),
            fill = strategy_label,
            group = interaction(strategy_label, activity_type)),
        alpha = 0.2
      )
    }
    
    # Add mean lines with improved styling
    p_main <- p_main + geom_line(
      data = group_means,
      aes(x = chain_step_id, 
          y = mean_proportion,
          color = strategy_label,
          linetype = activity_type,
          group = interaction(strategy_label, activity_type)),
      size = group_line_size
    )
    
    # Add smoothed trends with better visibility
    if (!is.null(loess_span)) {
      p_main <- p_main + geom_smooth(
        data = group_means,
        aes(x = chain_step_id, 
            y = mean_proportion,
            color = strategy_label,
            linetype = activity_type,
            group = interaction(strategy_label, activity_type)),
        method = "loess",
        span = loess_span,
        se = FALSE,
        size = 1,
        alpha = 0.8
      )
    }
    
    # Improved color scheme and styling
    p_main <- p_main +
      scale_color_manual(
        name = "Strategy Group",
        values = c("Control (0)" = "#2C3E50",      # Dark blue-gray
                   "Subsidized (100)" = "#E74C3C")  # Bright red
      ) +
      scale_fill_manual(
        name = "Strategy Group",
        values = c("Control (0)" = "#2C3E50",
                   "Subsidized (100)" = "#E74C3C"),
        guide = "none"
      ) +
      scale_linetype_manual(
        name = "Activity Type",
        values = c("Exploitation" = "solid",
                   "Exploration" = "longdash")  # More distinct dashing
      ) +
      labs(
        title = sim_title_str,
        subtitle = "Activity Portfolio Evolution: Exploitation (Old: C1-C8) vs Exploration (New: C9-C16)",
        x = "Simulation Step",
        y = "Proportion of Activities"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9)
      ) +
      scale_y_continuous(
        labels = scales::percent,
        limits = ylim,
        breaks = seq(0, 1, 0.25)
      ) +
      guides(
        color = guide_legend(order = 1, nrow = 1),
        linetype = guide_legend(order = 2, nrow = 1)
      )
    
    # Create difference plot if requested
    if (show_difference) {
      # Calculate differences between strategies
      diff_data <- group_means %>%
        select(chain_step_id, strategy, activity_type, mean_proportion) %>%
        pivot_wider(
          names_from = strategy,
          values_from = mean_proportion,
          names_prefix = "strategy_"
        ) %>%
        mutate(
          difference = strategy_100 - strategy_0,
          activity_type = factor(activity_type, levels = c("Exploitation", "Exploration"))
        )
      
      p_diff <- ggplot(diff_data, aes(x = chain_step_id, y = difference)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.5) +
        geom_line(aes(color = activity_type), size = 1.5) +
        geom_smooth(
          aes(color = activity_type),
          method = "loess",
          span = loess_span * 1.5,
          se = TRUE,
          alpha = 0.2,
          size = 0.8
        )
      
      # Add shock period
      if (!is.null(self$theta_shocks)) {
        p_diff <- p_diff + 
          geom_rect(
            data = shock_rects, 
            aes(xmin = start, xmax = end), 
            ymin = -Inf, ymax = Inf,
            fill = '#FFA500', 
            alpha = 0.1
          )
      }
      
      p_diff <- p_diff +
        scale_color_manual(
          name = "Activity Type",
          values = c("Exploitation" = "#3498DB",
                     "Exploration" = "#E74C3C")
        ) +
        labs(
          title = "Strategy Differential (Subsidized - Control)",
          subtitle = "Positive values indicate subsidized actors have higher proportion",
          x = "Simulation Step",
          y = "Difference in Proportion"
        ) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, color = "gray40")
        ) +
        scale_y_continuous(labels = scales::percent)
      
      # Combine plots
      combined_plot <- p_main / p_diff + 
        patchwork::plot_layout(heights = c(3, 1))
      
      return(combined_plot)
    }
    
    return(p_main)
  },
  
  # Additional function for a cleaner faceted view
  plot_exploration_exploitation_faceted = function(
    metrics_df,
    loess_span = 0.3,
    show_points = FALSE
  ) {
    
    # Prepare data
    plot_data <- metrics_df %>%
      mutate(
        strategy_label = ifelse(strategy == "0", "Control (0)", "Subsidized (100)")
      ) %>%
      select(chain_step_id, actor_id, strategy_label, 
             Exploitation = prop_exploitation, 
             Exploration = prop_exploration) %>%
      pivot_longer(
        cols = c(Exploitation, Exploration),
        names_to = "Activity_Type",
        values_to = "Proportion"
      )
    
    # Calculate means
    group_means <- plot_data %>%
      group_by(chain_step_id, strategy_label, Activity_Type) %>%
      summarise(
        mean_prop = mean(Proportion, na.rm = TRUE),
        se_prop = sd(Proportion, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Create faceted plot
    p <- ggplot(group_means, aes(x = chain_step_id, y = mean_prop)) +
      facet_grid(Activity_Type ~ strategy_label, scales = "free_y") +
      geom_ribbon(
        aes(ymin = mean_prop - se_prop, 
            ymax = mean_prop + se_prop),
        alpha = 0.2
      ) +
      geom_line(size = 1.5, color = "#2C3E50") +
      geom_smooth(
        method = "loess",
        span = loess_span,
        se = FALSE,
        color = "#E74C3C",
        size = 1,
        linetype = "dashed"
      )
    
    # Add points if requested
    if (show_points) {
      p <- p + geom_point(
        data = plot_data,
        aes(x = chain_step_id, y = Proportion),
        alpha = 0.1,
        size = 0.5
      )
    }
    
    p <- p +
      labs(
        title = "Activity Type Evolution by Strategy Group",
        x = "Simulation Step",
        y = "Proportion"
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "gray95"),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(labels = scales::percent)
    
    return(p)
  },
  
  
  # R6 method for plotting with strategies as colors and exploration/exploitation as linetypes
  plot_exploration_exploitation_by_strategy = function(
    actor_ids = c(),
    thin_factor = 1,
    show_points = TRUE,
    show_individuals = FALSE,
    show_group_means = TRUE,
    loess_span = 0.3,
    point_alpha = 0.3,
    line_alpha = 0.5,
    group_line_size = 2,
    se_ribbon = TRUE,
    ylim = c(0, 1),
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
  ) {

    # Get actor strategies using the R6 method
    actor_strategies <- self$get_actor_strategies()
    
    # Get bipartite array data (using the working method)
    bi_env_arr <- self$bi_env_arr  # Assuming this is passed or available
    n_steps <- dim(bi_env_arr)[3]
    n_actors <- self$M
    n_components <- self$N
    
    # Define old (exploitation) and new (exploration) components
    old_components <- 1:8
    new_components <- 9:16
    
    # Get unique chain steps
    unique_steps <- sort(unique(self$chain_stats$chain_step_id))
    
    # Calculate exploration/exploitation metrics
    metrics_list <- list()
    
    for (t_idx in 1:min(n_steps, length(unique_steps))) {
      incidence_t <- bi_env_arr[, , t_idx]
      
      for (i in 1:n_actors) {
        actor_activities <- which(incidence_t[i, ] > 0)
        
        if (length(actor_activities) > 0) {
          n_old <- sum(actor_activities %in% old_components)
          n_new <- sum(actor_activities %in% new_components)
          n_total <- length(actor_activities)
          
          metrics_list[[length(metrics_list) + 1]] <- data.frame(
            chain_step_id = unique_steps[t_idx],
            actor_id = as.character(i),
            strategy = as.character(actor_strategies[i]),  # Get strategy from R6 method
            n_total_activities = n_total,
            n_old_activities = n_old,
            n_new_activities = n_new,
            prop_exploration = n_new / n_total,
            prop_exploitation = n_old / n_total
          )
        }
      }
    }
    
    # Combine metrics
    metrics_df <- bind_rows(metrics_list) %>%
      mutate(
        strategy = factor(strategy),
        strategy_label = paste("Strategy", strategy)
      )
    
    # Apply thinning
    if (thin_factor > 1) {
      metrics_df <- metrics_df %>% 
        filter(chain_step_id %% thin_factor == 0)
    }
    
    # Filter actors if specified
    if (length(actor_ids) > 0) {
      metrics_df <- metrics_df %>% 
        filter(actor_id %in% actor_ids)
    }
    
    # Reshape to long format
    plot_data <- metrics_df %>%
      pivot_longer(
        cols = c(prop_exploration, prop_exploitation),
        names_to = "activity_type",
        values_to = "proportion"
      ) %>%
      mutate(
        activity_type = case_when(
          activity_type == "prop_exploration" ~ "Exploration",
          activity_type == "prop_exploitation" ~ "Exploitation"
        ),
        activity_label = factor(activity_type, levels = c("Exploitation", "Exploration"))
      )
    
    # Calculate group means by strategy and activity type
    group_means <- plot_data %>%
      group_by(chain_step_id, strategy, strategy_label, activity_type, activity_label) %>%
      summarise(
        mean_proportion = mean(proportion, na.rm = TRUE),
        se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
        n_obs = n(),
        .groups = "drop"
      )
    
    # Create plot
    p <- ggplot()
    
    # Add individual points if requested
    if (show_points) {
      p <- p + geom_point(
        data = plot_data,
        aes(x = chain_step_id, 
            y = proportion, 
            color = strategy_label),
        alpha = point_alpha,
        size = 0.8
      )
    }
    
    # Add individual lines if requested
    if (show_individuals) {
      p <- p + geom_line(
        data = plot_data,
        aes(x = chain_step_id, 
            y = proportion,
            color = strategy_label,
            linetype = activity_label,
            group = interaction(actor_id, activity_type)),
        alpha = line_alpha * 0.3,
        size = 0.4
      )
    }
    
    # Add group mean lines
    if (show_group_means) {
      # Add confidence ribbons if requested
      if (se_ribbon) {
        p <- p + geom_ribbon(
          data = group_means,
          aes(x = chain_step_id,
              ymin = mean_proportion - se_proportion,
              ymax = mean_proportion + se_proportion,
              fill = strategy_label,
              group = interaction(strategy_label, activity_label)),
          alpha = 0.15
        )
      }
      
      # Add mean lines with strategy as color and activity type as linetype
      p <- p + geom_line(
        data = group_means,
        aes(x = chain_step_id, 
            y = mean_proportion,
            color = strategy_label,
            linetype = activity_label,
            group = interaction(strategy_label, activity_label)),
        size = group_line_size
      )
      
      # Add smoothed trends
      if (!is.null(loess_span)) {
        p <- p + geom_smooth(
          data = group_means,
          aes(x = chain_step_id, 
              y = mean_proportion,
              color = strategy_label,
              linetype = activity_label,
              group = interaction(strategy_label, activity_label)),
          method = "loess",
          span = loess_span,
          se = FALSE,
          size = 0.8,
          alpha = 0.7
        )
      }
    }
    
    # Add shock rectangles if they exist
    if (!is.null(self$theta_shocks)) {
      shock_rects <- self$get_theta_shock_rects_df(self$theta_shocks)
      p <- p + 
        geom_rect(
          data = shock_rects, 
          aes(xmin = start, xmax = end), 
          ymin = -Inf, ymax = Inf,
          fill = 'darkorange', 
          alpha = 0.05
        ) +
        geom_text(
          data = shock_rects, 
          aes(x = (start + end) / 2, 
              y = ylim[2] * 0.95, 
              label = label),
          vjust = 0, 
          size = 3
        )
    }
    
    # Customize appearance with strategy colors and activity linetypes
    p <- p +
      scale_color_manual(
        name = "Strategy Group",
        values = c("Strategy 0" = "#3498DB",      # Blue for Strategy 0
                   "Strategy 100" = "#E74C3C")    # Red for Strategy 100
      ) +
      scale_fill_manual(
        name = "Strategy Group",
        values = c("Strategy 0" = "#3498DB",
                   "Strategy 100" = "#E74C3C"),
        guide = "none"  # Hide fill legend as it duplicates color
      ) +
      scale_linetype_manual(
        name = "Activity Type",
        values = c("Exploitation" = "solid",      # Solid for exploitation
                   "Exploration" = "dashed")      # Dashed for exploration
      ) +
      labs(
        title = "Exploration vs Exploitation by Strategy Groups",
        subtitle = sprintf("M=%d actors, N=%d components (Old Activities: C1-C8, New Activities: C9-C16)", 
                           self$M, self$N),
        x = "Simulation Step",
        y = "Proportion of Activities"
      ) +
      theme_bw() +
      theme(
        legend.position = "right",
        legend.box = "vertical",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray50")
      ) +
      coord_cartesian(ylim = ylim) +
      scale_y_continuous(labels = scales::percent)
    
    # Add parameter information if available
    if (!is.null(self$get_structure_model_params)) {
      params <- self$get_structure_model_params()
      sim_title_str <- self$get_structure_model_param_str(params)
      p <- p + labs(caption = sim_title_str)
    }
    
    # Save if requested
    if (plot_save) {
      plot_file <- paste0('strategy_explore_exploit_', plot_file, round(as.numeric(Sys.time()) * 10))
      ggsave(
        filename = file.path(
          ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
          sprintf("%s_%s.jpeg", self$config_environ_params$name, plot_file)
        ),
        p,
        width = 10, 
        height = 6, 
        units = 'in', 
        dpi = 400
      )
    }
    
    if (plot_return) {
      return(p)
    }
  },
  
  # Standalone version that works with the result object
  plot_strategy_exploration_exploitation = function(result, 
                                                     show_points = TRUE,
                                                     show_group_means = TRUE,
                                                     group_line_size = 2) {
    
    # Use the metrics from the result
    metrics_df <- result$metrics
    
    # Reshape to long format
    plot_data <- metrics_df %>%
      select(chain_step_id, actor_id, strategy, exploration, exploitation) %>%
      pivot_longer(
        cols = c(exploration, exploitation),
        names_to = "activity_type",
        values_to = "proportion"
      ) %>%
      mutate(
        activity_type = stringr::str_to_title(activity_type),
        strategy_label = paste("Strategy", strategy)
      )
    
    # Calculate group means
    group_means <- plot_data %>%
      group_by(chain_step_id, strategy, strategy_label, activity_type) %>%
      summarise(
        mean_proportion = mean(proportion, na.rm = TRUE),
        se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Create plot
    p <- ggplot()
    
    # Add points
    if (show_points) {
      p <- p + geom_point(
        data = plot_data,
        aes(x = chain_step_id, y = proportion, color = strategy_label),
        alpha = 0.2,
        size = 0.8
      )
    }
    
    # Add group means
    if (show_group_means) {
      # Confidence ribbons
      p <- p + geom_ribbon(
        data = group_means,
        aes(x = chain_step_id,
            ymin = mean_proportion - se_proportion,
            ymax = mean_proportion + se_proportion,
            fill = strategy_label,
            group = interaction(strategy_label, activity_type)),
        alpha = 0.15
      )
      
      # Mean lines
      p <- p + geom_line(
        data = group_means,
        aes(x = chain_step_id, 
            y = mean_proportion,
            color = strategy_label,
            linetype = activity_type,
            group = interaction(strategy_label, activity_type)),
        size = group_line_size
      )
    }
    
    # Customize
    p <- p +
      scale_color_manual(
        name = "Strategy Group",
        values = c("Strategy 0" = "#3498DB",
                   "Strategy 100" = "#E74C3C")
      ) +
      scale_fill_manual(
        name = "Strategy Group",
        values = c("Strategy 0" = "#3498DB",
                   "Strategy 100" = "#E74C3C"),
        guide = "none"
      ) +
      scale_linetype_manual(
        name = "Activity Type",
        values = c("Exploitation" = "solid",
                   "Exploration" = "dashed")
      ) +
      labs(
        title = "Exploration vs Exploitation by Strategy Groups",
        subtitle = "Colors: Strategy groups | Line types: Activity types",
        x = "Simulation Step",
        y = "Proportion"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.box = "vertical"
      ) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    
    return(p)
  }, 
  
  calculate_explore_exploit_risk_adjusted = function(new_components = 9:16,
                                                     old_components = 1:8,
                                                     component_payoffs = NULL,
                                                     component_variance = NULL) {
    
    # Get data from class properties
    df <- self$get_K4_df()
    bi_env_arr <- self$bi_env_arr
    
    # Use class component payoffs if not provided
    if (is.null(component_payoffs)) {
      # Check if class has component_payoffs
      if (!is.null(self$component_payoffs)) {
        component_payoffs <- self$component_payoffs
      } else {
        # Create default payoffs if not available
        component_payoffs <- c(rep(10, 8), rep(c(0, 50, 50, 50, 0, 50, 50, 50), 1))
      }
    }
    
    # Default variance if not provided
    if (is.null(component_variance)) {
      component_variance <- c(rep(0.1, 8), rep(0.5, 8))
    }
    
    # Ensure payoffs and variance have the right length
    n_components_expected <- length(old_components) + length(new_components)
    if (length(component_payoffs) < n_components_expected) {
      warning("component_payoffs length is less than expected. Extending with zeros.")
      component_payoffs <- c(component_payoffs, rep(0, n_components_expected - length(component_payoffs)))
    }
    if (length(component_variance) < n_components_expected) {
      warning("component_variance length is less than expected. Extending with 0.1.")
      component_variance <- c(component_variance, rep(0.1, n_components_expected - length(component_variance)))
    }
    
    # Extract edges from incidence matrix
    n_actors <- dim(bi_env_arr)[1]
    n_components <- dim(bi_env_arr)[2]
    n_steps <- dim(bi_env_arr)[3]
    unique_steps <- sort(unique(df$chain_step_id))
    
    edges_list <- list()
    
    for (t_idx in 1:min(n_steps, length(unique_steps))) {
      incidence_t <- bi_env_arr[, , t_idx]
      
      for (i in 1:n_actors) {
        for (j in 1:n_components) {
          if (!is.na(incidence_t[i, j]) && incidence_t[i, j] > 0) {
            edges_list[[length(edges_list) + 1]] <- data.frame(
              chain_step_id = unique_steps[t_idx],
              actor_id = as.character(i),
              component_id = as.character(j),
              tie_strength = incidence_t[i, j],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    
    edges_df <- dplyr::bind_rows(edges_list)
    
    # Add payoff and risk information to edges BEFORE grouping
    edges_df <- edges_df %>%
      dplyr::mutate(
        component_id_num = as.numeric(component_id),
        is_new = component_id_num %in% new_components,
        is_old = component_id_num %in% old_components,
        payoff = component_payoffs[pmin(component_id_num, length(component_payoffs))],
        risk = component_variance[pmin(component_id_num, length(component_variance))]
      )
    
    # Now calculate metrics with the payoff and risk already in the data
    metrics <- edges_df %>%
      dplyr::group_by(chain_step_id, actor_id) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_new = sum(is_new),
        n_old = sum(is_old),
        prop_new = n_new / n_total,
        prop_old = n_old / n_total,
        
        total_risk_exposure = sum(risk * tie_strength, na.rm = TRUE),
        avg_activity_risk = weighted.mean(risk, tie_strength, na.rm = TRUE),
        
        expected_payoff = sum(payoff * tie_strength, na.rm = TRUE),
        risk_adjusted_return = expected_payoff / (1 + total_risk_exposure),
        
        portfolio_concentration = sum((tie_strength / sum(tie_strength))^2),
        
        exploration_risk = prop_new * avg_activity_risk,
        exploitation_safety = prop_old * (1 - avg_activity_risk),
        
        entropy = {
          if (dplyr::n() > 1) {
            comp_table <- table(component_id)
            probs <- comp_table / sum(comp_table)
            -sum(probs * log(probs))
          } else {
            0
          }
        },
        
        hhi = {
          comp_table <- table(component_id)
          probs <- comp_table / sum(comp_table)
          sum(probs^2)
        },
        
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        entropy_norm = ifelse(n_total > 1, entropy / log(n_total), 0),
        exploration = (prop_new + (1 - hhi) + entropy_norm) / 3,
        exploitation = (prop_old + hhi + (1 - entropy_norm)) / 3,
        risk_taking_score = (exploration_risk + (1 - portfolio_concentration)) / 2,
        risk_aversion_score = (exploitation_safety + portfolio_concentration) / 2
      )
    
    # Add strategy information
    actor_strategies <- df %>%
      dplyr::filter(effect == "K_AC", !is.na(actor_id)) %>%
      dplyr::select(actor_id, strategy) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        actor_id = as.character(actor_id),
        strategy = as.character(strategy)
      )
    
    metrics <- metrics %>%
      dplyr::left_join(actor_strategies, by = "actor_id")
    
    return(metrics)
  },
  
  
  calculate_social_logic_influence = function(metrics_df, component_social_logic = NULL) {
    
    if (is.null(component_social_logic)) {
      component_social_logic <- self$component_social_logic
    }
    
    metrics_enhanced <- metrics_df %>%
      mutate(
        social_pressure = case_when(
          strategy == "100" ~ 0.3,
          TRUE ~ 0.1
        ),
        
        socially_constrained_exploration = exploration * (1 - social_pressure),
        
        moral_licensing = ifelse(strategy == "100" & exploitation > 0.5, 0.1, 0),
        
        exploration_adjusted = socially_constrained_exploration + moral_licensing,
        exploitation_adjusted = exploitation + (social_pressure * 0.5)
      )
    
    return(metrics_enhanced)
  },
  
  plot_subsidized_risk_taking = function(metrics_df = NULL, 
                                         shock_time = NULL, 
                                         show_se_ribbon = TRUE,
                                         line_size = 2,
                                         point_alpha = 0.3) {
    
    if (is.null(metrics_df)) {
      metrics_df <- self$calculate_explore_exploit_risk_adjusted()
    }
    
    plot_data <- metrics_df %>%
      mutate(
        post_subsidy = ifelse(!is.null(shock_time), 
                              chain_step_id >= shock_time, 
                              FALSE),
        strategy_label = case_when(
          strategy == "0" ~ "Control (No Subsidy)",
          strategy == "100" ~ "Treatment (Subsidized)",
          TRUE ~ as.character(strategy)
        )
      )
    
    p <- ggplot(plot_data) +
      geom_line(
        aes(x = chain_step_id, 
            y = risk_taking_score,
            color = strategy_label,
            group = interaction(actor_id, strategy_label)),
        alpha = point_alpha, size = 0.5
      ) +
      stat_summary(
        aes(x = chain_step_id, 
            y = risk_taking_score,
            color = strategy_label),
        fun = mean,
        geom = "line",
        size = line_size
      )
    
    if (show_se_ribbon) {
      p <- p + stat_summary(
        aes(x = chain_step_id, 
            y = risk_taking_score,
            fill = strategy_label),
        fun.data = mean_se,
        geom = "ribbon",
        alpha = 0.2
      )
    }
    
    if (!is.null(shock_time)) {
      p <- p + 
        geom_vline(xintercept = shock_time, 
                   linetype = "dashed", 
                   color = "gray50") +
        annotate("text", x = shock_time, y = 0.9, 
                 label = "Subsidy\nShock", 
                 hjust = -0.1, size = 3)
    }
    
    p <- p +
      scale_color_manual(
        values = c("Control (No Subsidy)" = "#2C3E50",
                   "Treatment (Subsidized)" = "#E74C3C")
      ) +
      scale_fill_manual(
        values = c("Control (No Subsidy)" = "#2C3E50",
                   "Treatment (Subsidized)" = "#E74C3C"),
        guide = "none"
      ) +
      labs(
        title = "Firm Risk-Taking Behavior: Subsidized vs Control",
        subtitle = "Higher scores indicate more exploration of risky new activities",
        x = "Simulation Step",
        y = "Risk-Taking Score",
        color = "Treatment Group"
      ) +
      theme_minimal() +
      ylim(0, 1)
    
    return(p)
  },
  
  plot_did_exploration = function(metrics_df = NULL, treatment_time = 50) {
    
    if (is.null(metrics_df)) {
      metrics_df <- self$calculate_explore_exploit_risk_adjusted()
      metrics_df <- self$calculate_social_logic_influence(metrics_df)
    }
    
    # Calculate summaries in two steps to avoid n() issues
    did_summary <- metrics_df %>%
      dplyr::mutate(
        period = factor(ifelse(chain_step_id < treatment_time, "Pre", "Post"), levels = c("Pre", "Post")),
        treatment = ifelse(strategy == "100", "Treated", "Control")
      ) %>%
      dplyr::group_by(period, treatment) %>%
      dplyr::summarise(
        mean_exploration = mean(exploration_adjusted, na.rm = TRUE),
        mean_risk_taking = mean(risk_taking_score, na.rm = TRUE),
        sd_risk_taking = sd(risk_taking_score, na.rm = TRUE),
        n_obs = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        se_risk_taking = sd_risk_taking / sqrt(n_obs)
      )
    
    p <- ggplot(did_summary, aes(x = period, y = mean_risk_taking, 
                                 color = treatment, group = treatment)) +
      geom_line(size = 2) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = mean_risk_taking - se_risk_taking,
                        ymax = mean_risk_taking + se_risk_taking),
                    width = 0.1) +
      scale_color_manual(values = c("Control" = "#2C3E50", 
                                    "Treated" = "#E74C3C")) +
      labs(
        title = "Difference-in-Differences: Subsidy Effect on Risk-Taking",
        subtitle = "Comparing subsidized vs non-subsidized firms",
        x = "Period",
        y = "Mean Risk-Taking Score"
      ) +
      theme_minimal()
    
    return(p)
  },
  
  analyze_subsidies_risk_taking = function(shock_time = 50, 
                                           plot_save = FALSE,
                                           plot_dir = NULL) {
    
    # Calculate risk-adjusted metrics
    metrics <- self$calculate_explore_exploit_risk_adjusted(
      component_variance = c(rep(0.1, 8), rep(0.5, 8))
    )
    
    # Apply social logic constraints
    metrics_adjusted <- self$calculate_social_logic_influence(metrics)
    
    # Check if strategy column exists
    if (!"strategy" %in% names(metrics_adjusted)) {
      stop("Strategy column not found in metrics. Check calculate_explore_exploit_risk_adjusted function.")
    }
    
    # Create visualizations
    p1 <- self$plot_subsidized_risk_taking(metrics_adjusted, shock_time)
    p2 <- self$plot_did_exploration(metrics_adjusted, shock_time)
    
    # Summary statistics - use dplyr explicitly
    summary_stats <- metrics_adjusted %>%
      dplyr::group_by(strategy) %>%
      dplyr::summarise(
        n_actors = dplyr::n_distinct(actor_id),
        pre_shock_risk = mean(risk_taking_score[chain_step_id < shock_time], na.rm = TRUE),
        post_shock_risk = mean(risk_taking_score[chain_step_id >= shock_time], na.rm = TRUE),
        risk_change = post_shock_risk - pre_shock_risk,
        final_exploration = mean(exploration_adjusted[chain_step_id == max(chain_step_id)], na.rm = TRUE),
        avg_portfolio_concentration = mean(portfolio_concentration, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate DiD estimate
    did_estimate <- summary_stats %>%
      dplyr::summarise(
        did = (risk_change[strategy == "100"] - risk_change[strategy == "0"])
      ) %>%
      dplyr::pull(did)
    
    cat("\n=== SUBSIDIES AND RISK-TAKING ANALYSIS ===\n")
    cat("\nSummary Statistics by Strategy:\n")
    print(summary_stats)
    cat("\nDifference-in-Differences Estimate:", round(did_estimate, 4), "\n")
    
    # Save plots if requested
    if (plot_save && !is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) {
        dir.create(plot_dir)
      }
      ggsave(file.path(plot_dir, "subsidies_risk_taking_trajectories.png"), 
             p1, width = 10, height = 6, dpi = 300)
      ggsave(file.path(plot_dir, "subsidies_did_analysis.png"), 
             p2, width = 8, height = 6, dpi = 300)
      cat("\nPlots saved to:", plot_dir, "\n")
    }
    
    return(list(
      metrics = metrics_adjusted,
      plots = list(risk_taking = p1, did = p2),
      summary = summary_stats,
      did_estimate = did_estimate
    ))
  },
  
  plot_exploration_exploitation_subsidies = function(metrics_df = NULL,
                                                     show_points = FALSE,
                                                     show_se_ribbon = TRUE,
                                                     loess_span = 0.3,
                                                     shock_time = NULL) {
    
    if (is.null(metrics_df)) {
      metrics_df <- self$calculate_explore_exploit_risk_adjusted()
      metrics_df <- self$calculate_social_logic_influence(metrics_df)
    }
    
    plot_data <- metrics_df %>%
      tidyr::pivot_longer(
        cols = c(exploration_adjusted, exploitation_adjusted),
        names_to = "behavior_type",
        values_to = "intensity"
      ) %>%
      dplyr::mutate(
        behavior_type = dplyr::case_when(
          behavior_type == "exploration_adjusted" ~ "Exploration",
          behavior_type == "exploitation_adjusted" ~ "Exploitation"
        ),
        strategy_label = dplyr::case_when(
          strategy == "0" ~ "Control (0)",
          strategy == "100" ~ "Subsidized (100)",
          TRUE ~ paste("Strategy", strategy)
        ),
        activity_type = behavior_type
      )
    
    # Calculate group means with explicit dplyr namespace
    group_means <- plot_data %>%
      dplyr::group_by(chain_step_id, strategy, strategy_label, activity_type) %>%
      dplyr::summarise(
        mean_proportion = mean(intensity, na.rm = TRUE),
        sd_proportion = sd(intensity, na.rm = TRUE),
        n_obs = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        se_proportion = sd_proportion / sqrt(n_obs)
      )
    
    p <- ggplot()
    
    if (show_points) {
      p <- p + geom_point(
        data = plot_data,
        aes(x = chain_step_id, y = intensity, color = strategy_label),
        alpha = 0.1, size = 0.5
      )
    }
    
    if (show_se_ribbon) {
      p <- p + geom_ribbon(
        data = group_means,
        aes(x = chain_step_id,
            ymin = mean_proportion - se_proportion,
            ymax = mean_proportion + se_proportion,
            fill = strategy_label,
            group = interaction(strategy_label, activity_type)),
        alpha = 0.15
      )
    }
    
    p <- p + geom_line(
      data = group_means,
      aes(x = chain_step_id,
          y = mean_proportion,
          color = strategy_label,
          linetype = activity_type),
      size = 1.5
    )
    
    if (!is.null(shock_time)) {
      p <- p + 
        geom_vline(xintercept = shock_time, linetype = "dashed", 
                   color = "gray50", alpha = 0.7) +
        annotate("text", x = shock_time, y = 0.95, 
                 label = "Subsidy", hjust = -0.1, size = 3, 
                 color = "gray40")
    }
    
    p <- p +
      scale_color_manual(
        name = "Strategy Group",
        values = c("Control (0)" = "#2C3E50",
                   "Subsidized (100)" = "#E74C3C")
      ) +
      scale_fill_manual(
        name = "Strategy Group",
        values = c("Control (0)" = "#2C3E50",
                   "Subsidized (100)" = "#E74C3C"),
        guide = "none"
      ) +
      scale_linetype_manual(
        name = "Activity Type",
        values = c("Exploitation" = "solid",
                   "Exploration" = "longdash")
      ) +
      labs(
        title = "Risk-Adjusted Exploration vs Exploitation: Subsidies Impact",
        subtitle = "Social logic constraints applied; moral licensing effect included",
        x = "Simulation Step",
        y = "Activity Intensity (Risk-Adjusted)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      ) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    
    return(p)
  }, 
  
  
  check_exploration_data_availability = function() {
    # Get K4 data
    k4_df <- self$get_K4_df()
    cat("\n=== DATA AVAILABILITY CHECK ===\n")
    cat("K4 data frame:\n")
    cat("  Total rows:", nrow(k4_df), "\n")
    cat("  Chain steps range:", min(k4_df$chain_step_id), "to", max(k4_df$chain_step_id), "\n")
    cat("  Unique chain steps:", length(unique(k4_df$chain_step_id)), "\n")
    
    # Check shock timing
    if (!is.null(self$theta_shocks)) {
      # Find the first element where shock_on = 1
      shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
      if (!is.na(shock_idx)) {
        shock_start <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
        cat("\nShock timing:\n")
        cat("  Shock starts at step:", shock_start, "\n")
        cat("  Pre-shock steps in K4 data:", sum(k4_df$chain_step_id < shock_start), "\n")
        cat("  Post-shock steps in K4 data:", sum(k4_df$chain_step_id >= shock_start), "\n")
      } else {
        cat("\nNo shock found with shock_on = 1\n")
      }
    }
    
    ## Functions added/updated in v39:
    # 1. check_exploration_data_availability - Diagnostic tool with correct shock detection
    # 2. plot_exploration_risk_multiperiod - Time series with automatic shock detection
    # 3. plot_exploration_risk_did - Simple before/after DiD plot
    # 4. test_multiperiod_exploration_risk - Event study analysis with proper shock finding
    # 5. analyze_exploration_risk_shocks - Combined analysis with shock_on detection
    # Key fix: All methods now correctly detect shock timing by finding the first
    # theta_shocks element where shock_on = 1, not just using the first element.
    
    # Try to get metrics
    tryCatch({
      metrics <- self$calculate_explore_exploit_risk_adjusted()
      cat("\nExploration metrics:\n")
      cat("  Total rows:", nrow(metrics), "\n")
      cat("  Chain steps range:", min(metrics$chain_step_id), "to", max(metrics$chain_step_id), "\n")
      cat("  Actors:", length(unique(metrics$actor_id)), "\n")
      
      # Check for missing steps
      expected_steps <- seq(min(k4_df$chain_step_id), max(k4_df$chain_step_id))
      metric_steps <- sort(unique(metrics$chain_step_id))
      missing_steps <- setdiff(expected_steps, metric_steps)
      if (length(missing_steps) > 0) {
        cat("  WARNING: Missing steps in metrics:", head(missing_steps, 10), 
            ifelse(length(missing_steps) > 10, "...", ""), "\n")
      }
    }, error = function(e) {
      cat("\nERROR calculating metrics:", e$message, "\n")
    })
    
    invisible(list(k4_df = k4_df))
  },
  
  # Method to plot exploration risk with automatic shock detection and formatting
  plot_exploration_risk_multiperiod = function(
    metrics_df = NULL,
    show_points = FALSE,
    show_se_ribbon = TRUE,
    line_size = 1.5,
    point_alpha = 0.1,
    loess_span = 0.3
  ) {
    
    if (is.null(metrics_df)) {
      metrics_df <- self$calculate_explore_exploit_risk_adjusted()
    }
    
    # Automatically detect shock start period
    shock_time <- NULL
    shock_label <- NULL
    if (!is.null(self$theta_shocks) && length(self$theta_shocks) > 0) {
      # Find the first element where shock_on = 1
      shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
      if (!is.na(shock_idx)) {
        shock_time <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
        shock_label <- ifelse(!is.null(self$theta_shocks[[shock_idx]]$label), 
                              self$theta_shocks[[shock_idx]]$label, 
                              "Subsidy\nShock")
      }
    }
    
    # Prepare data
    plot_data <- metrics_df %>%
      dplyr::mutate(
        strategy_label = dplyr::case_when(
          strategy == "0" ~ "Control (No Subsidy)",
          strategy == "100" ~ "Treatment (Subsidized)",
          TRUE ~ as.character(strategy)
        )
      )
    
    # Create the plot
    p <- ggplot(plot_data)
    
    # Add individual trajectories if requested
    if (show_points) {
      p <- p + geom_line(
        aes(x = chain_step_id, 
            y = risk_taking_score,
            color = strategy_label,
            group = interaction(actor_id, strategy_label)),
        alpha = point_alpha, 
        size = 0.3
      )
    }
    
    # Add mean lines
    p <- p + stat_summary(
      aes(x = chain_step_id, 
          y = risk_taking_score,
          color = strategy_label),
      fun = mean,
      geom = "line",
      size = line_size
    )
    
    # Add SE ribbons if requested
    if (show_se_ribbon) {
      p <- p + stat_summary(
        aes(x = chain_step_id, 
            y = risk_taking_score,
            fill = strategy_label),
        fun.data = mean_se,
        geom = "ribbon",
        alpha = 0.2
      )
    }
    
    # Add shock period shading
    if (!is.null(shock_time)) {
      shock_end <- max(plot_data$chain_step_id)
      p <- p + 
        annotate("rect", 
                 xmin = shock_time, 
                 xmax = shock_end,
                 ymin = -Inf, 
                 ymax = Inf,
                 fill = "orange", 
                 alpha = 0.1) +
        annotate("text", 
                 x = shock_time + (shock_end - shock_time) / 2, 
                 y = max(plot_data$risk_taking_score) * 0.95,
                 label = shock_label,
                 size = 3.5,
                 color = "black")
    }
    
    # Format the plot
    p <- p +
      scale_color_manual(
        values = c("Control (No Subsidy)" = "#2C3E50",
                   "Treatment (Subsidized)" = "#E74C3C")
      ) +
      scale_fill_manual(
        values = c("Control (No Subsidy)" = "#2C3E50",
                   "Treatment (Subsidized)" = "#E74C3C"),
        guide = "none"
      ) +
      labs(
        title = "Firm Risk-Taking Behavior: Subsidized vs Control",
        subtitle = "Higher scores indicate more exploration of risky new activities",
        x = "Simulation Step",
        y = "Risk-Taking Score",
        color = "Treatment Group"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      ) +
      ylim(0, 1)
    
    return(p)
  },
  
  # Method for regular before/after diff-in-diff plot
  plot_exploration_risk_did = function(
    metrics_df = NULL,
    pre_periods = NULL,
    post_periods = NULL
  ) {
    
    if (is.null(metrics_df)) {
      metrics_df <- self$calculate_explore_exploit_risk_adjusted()
    }
    
    # Automatically detect shock period
    shock_time <- NULL
    if (!is.null(self$theta_shocks) && length(self$theta_shocks) > 0) {
      shock_time <- min(self$theta_shocks[[1]]$chain_step_ids)
    }
    
    # Define pre/post periods if not provided
    if (is.null(shock_time)) {
      # If no shock, use midpoint
      midpoint <- median(unique(metrics_df$chain_step_id))
      pre_periods <- metrics_df$chain_step_id < midpoint
      post_periods <- metrics_df$chain_step_id >= midpoint
    } else {
      pre_periods <- metrics_df$chain_step_id < shock_time
      post_periods <- metrics_df$chain_step_id >= shock_time
    }
    
    # Calculate summaries
    did_summary <- metrics_df %>%
      mutate(
        period =       dplyr::case_when(
          chain_step_id %in% metrics_df$chain_step_id[pre_periods] ~ "Pre",
          chain_step_id %in% metrics_df$chain_step_id[post_periods] ~ "Post",
          TRUE ~ NA_character_
        ),
        treatment = ifelse(strategy == "100", "Treated", "Control")
      ) %>%
      filter(!is.na(period)) %>%
      dplyr::group_by(period, treatment) %>%
      dplyr::summarise(
        mean_risk_taking = mean(risk_taking_score, na.rm = TRUE),
        se_risk_taking = sd(risk_taking_score, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
    
    # Create the plot
    p <- ggplot(did_summary, aes(x = period, y = mean_risk_taking, 
                                 color = treatment, group = treatment)) +
      geom_line(size = 2) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = mean_risk_taking - se_risk_taking,
                        ymax = mean_risk_taking + se_risk_taking),
                    width = 0.1) +
      scale_color_manual(values = c("Control" = "#2C3E50", 
                                    "Treated" = "#E74C3C")) +
      labs(
        title = "Difference-in-Differences: Subsidy Effect on Risk-Taking",
        subtitle = "Comparing subsidized vs non-subsidized firms",
        x = "Period",
        y = "Mean Risk-Taking Score"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      )
    
    return(p)
  },
  
  # Method for multiperiod diff-in-diff tests (following the pattern from test_multiperiod_diff_actor_utility)
  test_multiperiod_exploration_risk = function(
    metrics_df = NULL,
    pre_periods_analyze = NULL,
    post_periods_analyze = NULL,
    verbose = FALSE
  ) {

    if (is.null(metrics_df)) {
      metrics_df <- self$calculate_explore_exploit_risk_adjusted()
    }
    
    # Detect shock information
    shock_info <- NULL
    if (!is.null(self$theta_shocks) && length(self$theta_shocks) > 0) {
      # Find the first element where shock_on = 1
      shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
      if (!is.na(shock_idx)) {
        shock_info <- list(
          start = min(self$theta_shocks[[shock_idx]]$chain_step_ids),
          label = ifelse(!is.null(self$theta_shocks[[shock_idx]]$label), 
                         self$theta_shocks[[shock_idx]]$label, 
                         "subsidy")
        )
      }
    }
    
    if (is.null(shock_info)) {
      stop("No shock information found in theta_shocks with shock_on = 1")
    }
    
    # Check if shock is too early for DiD analysis
    min_step <- min(metrics_df$chain_step_id)
    if (shock_info$start <= min_step) {
      warning(sprintf("Shock occurs at step %d, which is at or before the first step (%d). No pre-shock periods available for diff-in-diff analysis.", 
                      shock_info$start, min_step))
      return(list(
        error = "No pre-shock periods available",
        shock_time = shock_info$start,
        message = "Difference-in-differences analysis requires pre-treatment periods. Consider running the simulation with the shock occurring later."
      ))
    }
    
    # Prepare data for multiperiod analysis
    analysis_data <- metrics_df %>%
      dplyr::mutate(
        treated = as.integer(strategy == "100"),
        time_to_treat = chain_step_id - shock_info$start,
        post = as.integer(chain_step_id >= shock_info$start)
      )
    
    # Determine periods to analyze if not specified
    if (is.null(pre_periods_analyze)) {
      pre_periods_analyze <- max(abs(analysis_data$time_to_treat[analysis_data$time_to_treat < 0]), na.rm = TRUE)
    }
    if (is.null(post_periods_analyze)) {
      post_periods_analyze <- max(analysis_data$time_to_treat[analysis_data$time_to_treat > 0], na.rm = TRUE)
    }
    
    # Ensure we have valid periods
    if (is.infinite(pre_periods_analyze) || is.na(pre_periods_analyze) || pre_periods_analyze < 0) {
      warning("No pre-shock periods available in the data. This likely means:")
      warning("  - The metrics data starts at or after the shock")
      warning("  - Early simulation steps were filtered out")
      warning("Run check_exploration_data_availability() to diagnose")
      pre_periods_analyze <- 0
    }
    if (is.infinite(post_periods_analyze) || is.na(post_periods_analyze) || post_periods_analyze < 0) {
      warning("No post-shock periods available. Setting post_periods_analyze to 0.")
      post_periods_analyze <- 0
    }
    
    # Filter data based on periods
    analysis_data <- analysis_data %>%
      dplyr::filter(
        time_to_treat >= -pre_periods_analyze,
        time_to_treat <= post_periods_analyze
      )
    
    # Run fixest multiperiod regression
    # Check what reference periods are available
    available_periods <- sort(unique(analysis_data$time_to_treat))
    pre_periods <- available_periods[available_periods < 0]
    
    if (length(pre_periods) == 0) {
      warning("No pre-treatment periods available for event study design")
      return(list(
        error = "No pre-treatment periods",
        available_periods = available_periods
      ))
    }
    
    # Use the last pre-period as reference (typically -1, but could be different)
    ref_period <- max(pre_periods)
    
    model_risk <- feols(
      risk_taking_score ~ i(time_to_treat, treated, ref = ref_period) | actor_id + chain_step_id,
      data = analysis_data,
      cluster = ~actor_id
    )
    
    model_exploration <- feols(
      exploration ~ i(time_to_treat, treated, ref = ref_period) | actor_id + chain_step_id,
      data = analysis_data,
      cluster = ~actor_id
    )
    
    # Extract coefficients for plotting
    extract_coefs <- function(model, outcome_name, ref_period = -1) {
      coefs <- tidy(model, conf.int = TRUE) %>%
        dplyr::filter(grepl("time_to_treat", term)) %>%
        dplyr::mutate(
          event.time = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", term)),
          outcome = outcome_name
        ) %>%
        dplyr::select(event.time, estimate, std.error, conf.low, conf.high, p.value, outcome)
      
      # Add reference period
      ref_row <- data.frame(
        event.time = ref_period,
        estimate = 0,
        std.error = 0,
        conf.low = 0,
        conf.high = 0,
        p.value = NA,
        outcome = outcome_name
      )
      
      dplyr::bind_rows(ref_row, coefs)
    }
    
    coefs_risk <- extract_coefs(model_risk, "Risk-Taking Score", ref_period)
    coefs_exploration <- extract_coefs(model_exploration, "Exploration", ref_period)
    
    # Combine coefficients
    all_coefs <- dplyr::bind_rows(coefs_risk, coefs_exploration)
    
    # Create the multiperiod plot
    p1 <- ggplot(all_coefs, aes(x = event.time, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
      facet_wrap(~ outcome, scales = "free_y", nrow = 2) +
      labs(
        x = "Periods Relative to Subsidy Shock",
        y = "Average Treatment Effect on Treated (ATT)"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank()
      )
    
    # Add shock period shading
    max_post_period <- max(all_coefs$event.time[all_coefs$event.time > 0], na.rm = TRUE)
    shock_rect <- data.frame(
      xmin = 0,
      xmax = max_post_period,
      ymin = -Inf,
      ymax = Inf
    )
    
    p1 <- p1 + 
      geom_rect(data = shock_rect, 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "orange", alpha = 0.05, inherit.aes = FALSE)
    
    # Create summary statistics plot
    summary_stats <- analysis_data %>%
      dplyr::mutate(
        period = factor(ifelse(time_to_treat < 0, "Pre", "Post"), levels = c("Pre", "Post")),
        treatment_group = ifelse(treated == 1, "Subsidized", "Control")
      ) %>%
      dplyr::group_by(period, treatment_group) %>%
      dplyr::summarise(
        mean_risk = mean(risk_taking_score, na.rm = TRUE),
        mean_exploration = mean(exploration, na.rm = TRUE),
        .groups = "drop"
      )
    
    p2 <- summary_stats %>%
      tidyr::pivot_longer(cols = starts_with("mean_"), 
                          names_to = "metric", 
                          values_to = "value") %>%
      dplyr::mutate(
        metric = dplyr::case_when(
          metric == "mean_risk" ~ "Risk-Taking Score",
          metric == "mean_exploration" ~ "Exploration"
        )
      ) %>%
      ggplot(aes(x = period, y = value, color = treatment_group, group = treatment_group)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      facet_wrap(~ metric, scales = "free_y") +
      scale_color_manual(values = c("Control" = "#2C3E50", "Subsidized" = "#E74C3C")) +
      labs(
        title = "Average Outcomes by Period and Treatment",
        x = "Period",
        y = "Average Value",
        color = "Group"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold")
      )
    
    # Combine plots
    combined_plot <- ggarrange(p1, p2, nrow = 2, heights = c(2, 1))
    
    # Add title with simulation parameters
    if (pre_periods_analyze == 0) {
      title_str <- sprintf(
        "Multiperiod Diff-in-Diff Tests of Exploration Risk\n(WARNING: No pre-treatment periods - shock at start)\nEnvironment: Actors (M) = %d, Components (N) = %d",
        self$M, self$N
      )
    } else {
      title_str <- sprintf(
        "Multiperiod Diff-in-Diff Tests of Exploration Risk (%d pre, %d post periods)\nEnvironment: Actors (M) = %d, Components (N) = %d",
        pre_periods_analyze, post_periods_analyze, self$M, self$N
      )
    }
    
    if (!is.null(self$config_structure_model)) {
      # Add more parameters to title if available
      effects <- sapply(self$config_structure_model$dv_bipartite$effects, 
                        function(x) sprintf("%s=%s", x$effect, round(x$parameter, 2)))
      title_str <- paste0(title_str, "\nEffects: ", paste(effects[1:min(3, length(effects))], collapse = ", "))
    }
    
    combined_plot <- ggpubr::annotate_figure(
      combined_plot,
      top = ggpubr::text_grob(title_str, face = "bold", size = 14)
    )
    
    if (verbose) {
      cat("\n=== MULTIPERIOD DIFF-IN-DIFF RESULTS ===\n")
      if (pre_periods_analyze == 0) {
        cat("\nWARNING: No pre-treatment periods available for analysis\n")
        cat("The shock occurs at the beginning of the simulation\n")
      } else {
        cat(sprintf("\nAnalyzing %d pre-periods and %d post-periods\n", 
                    pre_periods_analyze, post_periods_analyze))
      }
      cat("\nRisk-Taking Score Model:\n")
      print(summary(model_risk))
      cat("\nExploration Model:\n")
      print(summary(model_exploration))
    }
    
    return(list(
      plot = combined_plot,
      models = list(
        risk_taking = model_risk,
        exploration = model_exploration
      ),
      coefficients = all_coefs,
      summary_stats = summary_stats
    ))
  },
  
  # Combined analysis method that runs both types of tests
  analyze_exploration_risk_shocks = function(
    shock_time = NULL,
    pre_periods_analyze = NULL,
    post_periods_analyze = NULL,
    plot_save = FALSE,
    plot_dir = NULL,
    verbose = FALSE,
    debug = FALSE
  ) {
    
    if (debug) {
      cat("\n=== DEBUG MODE: analyze_exploration_risk_shocks ===\n")
      cat("Starting analysis...\n")
    }
    
    # Calculate risk-adjusted metrics
    if (debug) cat("Step 1: Calculating risk-adjusted metrics...\n")
    metrics <- self$calculate_explore_exploit_risk_adjusted()
    
    # Check if metrics were calculated successfully
    if (is.null(metrics) || nrow(metrics) == 0) {
      stop("Failed to calculate exploration/exploitation metrics. Check that calculate_explore_exploit_risk_adjusted() is working properly.")
    }
    
    if (debug) {
      cat("  - Metrics calculated: ", nrow(metrics), " rows\n")
      cat("  - Chain steps range: ", min(metrics$chain_step_id), " to ", max(metrics$chain_step_id), "\n")
      cat("  - Unique chain steps: ", length(unique(metrics$chain_step_id)), "\n")
      
      # Check if early steps are missing
      expected_first_step <- 1
      actual_first_step <- min(metrics$chain_step_id)
      if (actual_first_step > expected_first_step) {
        cat("  - WARNING: Data starts at step", actual_first_step, "instead of step", expected_first_step, "\n")
        cat("  - Missing steps 1 to", actual_first_step - 1, "may affect DiD analysis\n")
      }
    }
    
    # Check if strategy column exists
    if (!"strategy" %in% names(metrics)) {
      if (debug) cat("  - Strategy column missing, attempting to add...\n")
      # Try to add strategy information
      if (exists("self") && !is.null(self$get_actor_strategies)) {
        actor_strategies <- self$get_actor_strategies()
        metrics$strategy <- as.character(actor_strategies[metrics$actor_id])
      } else {
        stop("Strategy column not found in metrics and unable to retrieve actor strategies.")
      }
    }
    if (debug) cat("  - Strategy column present\n")
    
    # If social logic influence method exists, apply it
    if ("calculate_social_logic_influence" %in% names(self)) {
      if (debug) cat("Step 2: Applying social logic influence...\n")
      tryCatch({
        metrics <- self$calculate_social_logic_influence(metrics)
        if (debug) cat("  - Social logic applied successfully\n")
      }, error = function(e) {
        warning("calculate_social_logic_influence failed: ", e$message, "\nContinuing without social logic adjustments.")
        if (debug) cat("  - Social logic failed, continuing without it\n")
      })
    }
    
    # Ensure we have the necessary columns
    if (!"exploration" %in% names(metrics)) {
      if ("exploration_adjusted" %in% names(metrics)) {
        metrics$exploration <- metrics$exploration_adjusted
      } else {
        stop("Neither 'exploration' nor 'exploration_adjusted' column found in metrics.")
      }
    }
    
    if (!"risk_taking_score" %in% names(metrics)) {
      # Calculate a simple risk-taking score if not present
      if (all(c("exploration_risk", "portfolio_concentration") %in% names(metrics))) {
        metrics$risk_taking_score <- (metrics$exploration_risk + (1 - metrics$portfolio_concentration)) / 2
      } else {
        stop("Unable to calculate risk_taking_score. Required columns missing.")
      }
    }
    
    if (debug) {
      cat("  - Available columns: ", paste(names(metrics), collapse = ", "), "\n")
      cat("  - Unique strategies: ", paste(unique(metrics$strategy), collapse = ", "), "\n")
    }
    
    # Automatically detect shock if not provided
    if (is.null(shock_time) && !is.null(self$theta_shocks)) {
      # Find the first theta_shocks element where shock_on = 1
      shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
      if (!is.na(shock_idx)) {
        shock_time <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
        if (debug) cat("Step 3: Detected shock time from theta_shocks (shock_on=1):", shock_time, "\n")
      } else {
        stop("No shock found with shock_on = 1 in theta_shocks")
      }
    }
    
    if (is.null(shock_time)) {
      stop("No shock time detected. Please provide shock_time or ensure theta_shocks is properly set.")
    }
    
    # Check available data range
    min_step <- min(metrics$chain_step_id)
    max_step <- max(metrics$chain_step_id)
    
    if (debug) {
      cat("  - Data range: steps", min_step, "to", max_step, "\n")
      cat("  - Shock occurs at step:", shock_time, "\n")
    }
    
    # Count actual pre/post periods in the data
    pre_shock_steps <- metrics$chain_step_id[metrics$chain_step_id < shock_time]
    post_shock_steps <- metrics$chain_step_id[metrics$chain_step_id >= shock_time]
    n_pre <- length(unique(pre_shock_steps))
    n_post <- length(unique(post_shock_steps))
    
    if (debug) {
      cat("  - Pre-shock periods in data:", n_pre, "\n")
      cat("  - Post-shock periods in data:", n_post, "\n")
    }
    
    # Check if we have enough data for DiD
    if (n_pre == 0) {
      cat("\n=== EXPLORATION RISK DIFF-IN-DIFF ANALYSIS ===\n")
      cat("\nERROR: No pre-shock data available\n")
      cat("Shock occurs at step", shock_time, "but metrics data starts at step", min_step, "\n")
      cat("\nThis typically means:\n")
      cat("1. The metrics calculation is filtering out early steps\n")
      cat("2. Check calculate_explore_exploit_risk_adjusted() for any filtering\n")
      cat("3. The shock timing is correct (step", shock_time, "), but pre-shock data is missing\n")
      
      # Still try to create a basic trajectory plot
      p1 <- tryCatch({
        self$plot_exploration_risk_multiperiod(metrics)
      }, error = function(e) NULL)
      
      return(list(
        error = "No pre-shock data available in metrics",
        shock_time = shock_time,
        data_range = c(min_step, max_step),
        metrics = metrics,
        plots = list(trajectories = p1, did = NULL, multiperiod = NULL),
        message = "Check calculate_explore_exploit_risk_adjusted() for data filtering"
      ))
    }
    
    # Create visualizations
    if (debug) cat("Step 4: Creating visualizations...\n")
    
    tryCatch({
      p1 <- self$plot_exploration_risk_multiperiod(metrics)
      if (debug) cat("  - Multiperiod plot created\n")
    }, error = function(e) {
      warning("Failed to create multiperiod plot: ", e$message)
      p1 <- NULL
    })
    
    tryCatch({
      p2 <- self$plot_exploration_risk_did(metrics)
      if (debug) cat("  - DiD plot created\n")
    }, error = function(e) {
      warning("Failed to create DiD plot: ", e$message)
      p2 <- NULL
    })
    
    # Run multiperiod analysis
    if (debug) cat("Step 5: Running multiperiod analysis...\n")
    
    multiperiod_results <- tryCatch({
      self$test_multiperiod_exploration_risk(
        metrics_df = metrics,
        pre_periods_analyze = pre_periods_analyze,
        post_periods_analyze = post_periods_analyze,
        verbose = verbose
      )
    }, error = function(e) {
      warning("Multiperiod analysis failed: ", e$message)
      if (debug) cat("  - Error in multiperiod analysis: ", e$message, "\n")
      NULL
    })
    
    # Calculate DiD estimates
    if (debug) cat("Step 6: Calculating DiD estimates...\n")
    
    did_stats <- tryCatch({
      metrics %>%
        dplyr::mutate(
          period = factor(ifelse(chain_step_id < shock_time, "Pre", "Post"), levels = c("Pre", "Post")),
          treatment = ifelse(strategy == "100", "Treated", "Control")
        ) %>%
        dplyr::group_by(period, treatment) %>%
        dplyr::summarise(
          mean_risk = mean(risk_taking_score, na.rm = TRUE),
          mean_exploration = mean(exploration, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
          names_from = period,
          values_from = c(mean_risk, mean_exploration)
        ) %>%
        dplyr::mutate(
          risk_change = mean_risk_Post - mean_risk_Pre,
          exploration_change = mean_exploration_Post - mean_exploration_Pre
        )
    }, error = function(e) {
      warning("Failed to calculate DiD statistics: ", e$message)
      if (debug) cat("  - Error in DiD calculation: ", e$message, "\n")
      NULL
    })
    
    # Calculate DiD estimates
    did_estimate_risk <- NA
    did_estimate_exploration <- NA
    
    if (!is.null(did_stats) && nrow(did_stats) > 0) {
      # Check if we have both Pre and Post values
      if (all(c("mean_risk_Pre", "mean_risk_Post") %in% names(did_stats))) {
        treated_idx <- which(did_stats$treatment == "Treated")
        control_idx <- which(did_stats$treatment == "Control")
        
        if (length(treated_idx) > 0 && length(control_idx) > 0) {
          did_estimate_risk <- did_stats$risk_change[treated_idx] - 
            did_stats$risk_change[control_idx]
          
          if (all(c("mean_exploration_Pre", "mean_exploration_Post") %in% names(did_stats))) {
            did_estimate_exploration <- did_stats$exploration_change[treated_idx] - 
              did_stats$exploration_change[control_idx]
          }
        }
      }
      if (debug) cat("  - DiD estimates calculated successfully\n")
    }
    
    cat("\n=== EXPLORATION RISK DIFF-IN-DIFF ANALYSIS ===\n")
    cat("\nShock Period:", shock_time, "\n")
    if (verbose || debug) {
      cat(sprintf("Data available: steps %d to %d\n", 
                  min(metrics$chain_step_id), max(metrics$chain_step_id)))
      actual_pre <- length(unique(metrics$chain_step_id[metrics$chain_step_id < shock_time]))
      actual_post <- length(unique(metrics$chain_step_id[metrics$chain_step_id >= shock_time]))
      cat(sprintf("Periods for analysis: %d pre-shock, %d post-shock\n", actual_pre, actual_post))
    }
    cat("\nDifference-in-Differences Estimates:\n")
    if (!is.na(did_estimate_risk)) {
      cat("  Risk-Taking Score DiD:", round(did_estimate_risk, 4), "\n")
      cat("  Exploration DiD:", round(did_estimate_exploration, 4), "\n")
    } else {
      cat("  Unable to calculate DiD estimates\n")
      if (verbose || debug) {
        cat("\n  Possible reasons:\n")
        cat("  - Insufficient pre-treatment data\n")
        cat("  - Data collection started after shock\n")
        cat("  - Check calculate_explore_exploit_risk_adjusted() for data filtering\n")
      }
    }
    
    if (debug) cat("\nAnalysis completed successfully!\n")
    
    # Save plots if requested
    if (plot_save && !is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) {
        dir.create(plot_dir, recursive = TRUE)
      }
      
      tryCatch({
        if (!is.null(p1)) {
          ggsave(file.path(plot_dir, "exploration_risk_trajectories.png"), 
                 p1, width = 10, height = 6, dpi = 300)
        }
        if (!is.null(p2)) {
          ggsave(file.path(plot_dir, "exploration_risk_did.png"), 
                 p2, width = 8, height = 6, dpi = 300)
        }
        if (!is.null(multiperiod_results) && !is.null(multiperiod_results$plot)) {
          ggsave(file.path(plot_dir, "exploration_risk_multiperiod.png"), 
                 multiperiod_results$plot, width = 12, height = 10, dpi = 300)
        }
        cat("\nPlots saved to:", plot_dir, "\n")
      }, error = function(e) {
        warning("Failed to save some plots: ", e$message)
      })
    }
    
    return(list(
      metrics = metrics,
      plots = list(
        trajectories = p1,
        did = p2,
        multiperiod = if (!is.null(multiperiod_results)) multiperiod_results$plot else NULL
      ),
      multiperiod_results = multiperiod_results,
      did_estimates = list(
        risk_taking = did_estimate_risk,
        exploration = did_estimate_exploration
      ),
      summary_stats = did_stats,
      shock_time = shock_time
    ))
  }, 
  
  # Clean implementation of simple exploration measures for SAOM analysis
  
  # Exploration analysis methods that work with bi_env_arr directly
  
  # Extract edges from bipartite array
  get_bipartite_edges = function(debug = FALSE) {
    
    if (is.null(self$bi_env_arr)) {
      stop("bi_env_arr not found in environment")
    }
    
    arr <- self$bi_env_arr
    dims <- dim(arr)
    n_actors <- dims[1]
    n_components <- dims[2]
    n_time <- dims[3]
    
    if (debug) {
      cat("\n=== Extracting edges from bi_env_arr ===\n")
      cat("  Array dimensions:", n_actors, "actors x", n_components, "components x", n_time, "time steps\n")
    }
    
    # Get chain_step_ids from chain_stats
    chain_step_ids <- self$chain_stats$chain_step_id
    if (length(chain_step_ids) != n_time) {
      warning("Mismatch between chain_stats and bi_env_arr time dimension")
      chain_step_ids <- 1:n_time
    }
    
    # Convert array to edge list
    edges_list <- list()
    
    for (t in 1:n_time) {
      time_matrix <- arr[, , t]
      
      # Find non-zero entries
      active_edges <- which(time_matrix > 0, arr.ind = TRUE)
      
      if (nrow(active_edges) > 0) {
        for (i in 1:nrow(active_edges)) {
          a <- active_edges[i, 1]  # actor
          c <- active_edges[i, 2]  # component
          
          edges_list[[length(edges_list) + 1]] <- data.frame(
            chain_step_id = chain_step_ids[t],
            actor_id = as.character(a),
            component_id = as.character(c),
            value = time_matrix[a, c],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    edges_df <- dplyr::bind_rows(edges_list)
    
    if (debug && nrow(edges_df) > 0) {
      cat("  Total edges found:", nrow(edges_df), "\n")
      cat("  Unique actors:", length(unique(edges_df$actor_id)), "\n")
      cat("  Unique components:", sort(unique(as.numeric(edges_df$component_id))), "\n")
      cat("  Time range:", min(edges_df$chain_step_id), "-", max(edges_df$chain_step_id), "\n")
      
      # Show activity by component
      comp_activity <- edges_df %>%
        dplyr::group_by(component_id) %>%
        dplyr::summarise(n_ties = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(as.numeric(component_id))
      cat("\n  Activity by component:\n")
      print(comp_activity)
    }
    
    return(edges_df)
  },
  
  # Calculate simple exploration metrics from array
  calculate_simple_exploration_metrics = function(debug = FALSE) {
    
    # Get edges from array
    edges_df <- self$get_bipartite_edges(debug = debug)
    
    if (nrow(edges_df) == 0) {
      warning("No active edges found in bi_env_arr")
      return(data.frame())
    }
    
    # Define new vs old activities
    new_activities <- as.character(9:16)
    old_activities <- as.character(1:8)
    
    # Calculate metrics
    metrics <- edges_df %>%
      dplyr::mutate(
        is_new = component_id %in% new_activities,
        is_old = component_id %in% old_activities
      ) %>%
      dplyr::group_by(chain_step_id, actor_id) %>%
      dplyr::summarise(
        n_total_activities = dplyr::n(),
        n_new_activities = sum(is_new),
        n_old_activities = sum(is_old),
        any_new_activity = as.numeric(n_new_activities > 0),
        prop_new_simple = n_new_activities / n_total_activities,
        .groups = "drop"
      )
    
    # Add strategy
    actor_strategies <- self$get_actor_strategies()
    actor_indices <- as.numeric(metrics$actor_id)
    metrics$strategy <- as.character(actor_strategies[actor_indices])
    
    if (debug) {
      cat("\n=== Exploration Metrics Summary ===\n")
      cat("  Total observations:", nrow(metrics), "\n")
      cat("  Mean new activities:", mean(metrics$n_new_activities), "\n")
      cat("  Max new activities:", max(metrics$n_new_activities), "\n")
      cat("  Actors ever exploring:", sum(metrics$any_new_activity > 0) / n_distinct(metrics$actor_id), "\n")
      
      # Check by strategy
      strat_summary <- metrics %>%
        dplyr::group_by(strategy) %>%
        dplyr::summarise(
          avg_new = mean(n_new_activities),
          max_new = max(n_new_activities),
          ever_explored = mean(any_new_activity),
          .groups = "drop"
        )
      cat("\n  By strategy:\n")
      print(strat_summary)
    }
    
    return(metrics)
  },
  
  # Analyze simple exploration shocks
  analyze_simple_exploration_shocks = function(
    shock_time = NULL,
    metric = "n_new_activities",
    plot_save = FALSE,
    plot_dir = NULL,
    debug = FALSE
  ) {
    
    # Get metrics
    metrics <- self$calculate_simple_exploration_metrics(debug = debug)
    
    if (nrow(metrics) == 0) {
      stop("No metrics calculated. Check bi_env_arr data.")
    }
    
    # Check metric exists
    if (!metric %in% names(metrics)) {
      stop(paste("Metric", metric, "not found. Available:", 
                 paste(names(metrics), collapse = ", ")))
    }
    
    # Detect shock time if not provided
    if (is.null(shock_time) && !is.null(self$theta_shocks)) {
      shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
      if (!is.na(shock_idx)) {
        shock_time <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
      }
    }
    
    if (is.null(shock_time)) {
      stop("No shock time detected")
    }
    
    if (debug) {
      cat("\n=== Shock Analysis ===\n")
      cat("  Shock time:", shock_time, "\n")
      cat("  Pre-shock steps:", sum(unique(metrics$chain_step_id) < shock_time), "\n")
      cat("  Post-shock steps:", sum(unique(metrics$chain_step_id) >= shock_time), "\n")
    }
    
    # Calculate DiD
    did_summary <- metrics %>%
      dplyr::mutate(
        period = factor(ifelse(chain_step_id < shock_time, "Pre", "Post"), levels = c("Pre", "Post")),
        treatment = ifelse(strategy == "100", "Treated", "Control")
      ) %>%
      dplyr::group_by(period, treatment) %>%
      dplyr::summarise(
        mean_value = mean(!!sym(metric), na.rm = TRUE),
        n_obs = dplyr::n(),
        .groups = "drop"
      )
    
    # Pivot to calculate changes
    did_data <- did_summary %>%
      tidyr::pivot_wider(
        names_from = period,
        values_from = mean_value
      ) %>%
      dplyr::mutate(
        change = Post - Pre
      )
    
    # Calculate DiD estimate
    did_estimate <- NA
    if (all(c("Treated", "Control") %in% did_data$treatment)) {
      did_estimate <- did_data$change[did_data$treatment == "Treated"] - 
        did_data$change[did_data$treatment == "Control"]
    }
    
    # Create plot
    p <- ggplot(metrics, aes(x = chain_step_id, y = !!sym(metric))) +
      stat_summary(
        aes(color = strategy),
        fun = mean,
        geom = "line",
        size = 1.5
      ) +
      stat_summary(
        aes(fill = strategy),
        fun.data = mean_se,
        geom = "ribbon",
        alpha = 0.2
      ) +
      geom_vline(xintercept = shock_time, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("0" = "#2C3E50", "100" = "#E74C3C")) +
      scale_fill_manual(values = c("0" = "#2C3E50", "100" = "#E74C3C")) +
      labs(
        title = paste("Simple Exploration Measure:", metric),
        subtitle = sprintf("DiD = %.4f", ifelse(is.na(did_estimate), 0, did_estimate)),
        x = "Simulation Step",
        y = metric,
        color = "Strategy",
        fill = "Strategy"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Print results
    cat("\n=== SIMPLE EXPLORATION ANALYSIS ===\n")
    cat("Metric:", metric, "\n")
    cat("Shock time:", shock_time, "\n")
    cat("\nMeans by period and treatment:\n")
    print(did_summary)
    cat("\nChanges by treatment:\n")
    print(did_data)
    cat("\nDifference-in-Differences:", sprintf("%.4f", did_estimate), "\n")
    
    if (!is.na(did_estimate)) {
      if (did_estimate < -0.001) {
        cat("Interpretation: Subsidies REDUCE", metric, "by", sprintf("%.4f", abs(did_estimate)), "\n")
      } else if (did_estimate > 0.001) {
        cat("Interpretation: Subsidies INCREASE", metric, "by", sprintf("%.4f", did_estimate), "\n")
      } else {
        cat("Interpretation: No meaningful effect of subsidies on", metric, "\n")
      }
    }
    
    # Save plot if requested
    if (plot_save && !is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      filename <- file.path(plot_dir, paste0("simple_exploration_", metric, ".png"))
      ggsave(filename, p, width = 10, height = 6, dpi = 300)
      cat("\nPlot saved to:", filename, "\n")
    }
    
    return(list(
      metrics = metrics,
      plot = p,
      did_summary = did_summary,
      did_data = did_data,
      did_estimate = did_estimate
    ))
  },
  
  # Quick diagnostic for exploration patterns
  diagnose_exploration_patterns = function() {
    
    cat("\n=== EXPLORATION PATTERNS DIAGNOSTIC ===\n")
    
    # Get edges
    edges <- self$get_bipartite_edges(debug = FALSE)
    
    if (nrow(edges) == 0) {
      cat("No edges found in bi_env_arr\n")
      return(invisible(NULL))
    }
    
    # Summarize by component over time
    comp_time <- edges %>%
      dplyr::mutate(
        component_group = ifelse(as.numeric(component_id) <= 8, "Old (1-8)", "New (9-16)")
      ) %>%
      dplyr::group_by(chain_step_id, component_group) %>%
      dplyr::summarise(n_ties = dplyr::n(), .groups = "drop")
    
    # Plot
    p <- ggplot(comp_time, aes(x = chain_step_id, y = n_ties, color = component_group)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Old (1-8)" = "#2C3E50", "New (9-16)" = "#E74C3C")) +
      labs(
        title = "Activity in Old vs New Components Over Time",
        x = "Simulation Step",
        y = "Number of Active Ties",
        color = "Component Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    print(p)
    
    # Summary stats
    cat("\nTotal ties by component type:\n")
    comp_summary <- edges %>%
      dplyr::mutate(
        component_group = ifelse(as.numeric(component_id) <= 8, "Old (1-8)", "New (9-16)")
      ) %>%
      dplyr::group_by(component_group) %>%
      dplyr::summarise(
        total_ties = dplyr::n(),
        unique_actors = dplyr::n_distinct(actor_id),
        .groups = "drop"
      )
    print(comp_summary)
    
    invisible(list(edges = edges, plot = p))
  }, 
  
  
  # Fix for exploration analysis to match K_AC DiD structure
  
  # The issue is that the exploration analysis needs to add a treatment_group column
  # similar to how compute_K_shocks does it. Here's a modified version:
  
  analyze_simple_exploration_shocks_fixed = function(
    metric = "n_new_activities",
    shock_time = NULL,
    plot_save = FALSE,
    plot_dir = NULL,
    debug = FALSE
  ) {
    
    if (debug) {
      cat("\n=== DEBUG: analyze_simple_exploration_shocks ===\n")
    }
    
    # Get metrics
    metrics <- self$calculate_explore_exploit_risk_adjusted()
    if (nrow(metrics) == 0) {
      stop("No exploration metrics found. Check bi_env_arr data.")
    }
    
    # Check metric exists
    if (!metric %in% names(metrics)) {
      stop(paste("Metric", metric, "not found. Available:", 
                 paste(names(metrics), collapse = ", ")))
    }
    
    # Detect shock time if not provided
    if (is.null(shock_time) && !is.null(self$theta_shocks)) {
      shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
      if (!is.na(shock_idx)) {
        shock_time <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
      }
    }
    
    if (is.null(shock_time)) {
      stop("No shock time detected")
    }
    
    # Add treatment_group column similar to compute_K_shocks
    # This is the key fix - we need to indicate WHEN treatment starts
    metrics <- metrics %>%
      mutate(
        # Treatment group indicates when treatment starts
        # 0 means never treated (control group)
        # shock_time means treated starting at shock_time
        treatment_group = case_when(
          strategy == "0" ~ 0,  # Control never gets treatment
          strategy == "100" ~ shock_time,  # Treatment starts at shock_time
          TRUE ~ 0
        )
      )
    
    if (debug) {
      cat("\n=== Shock Analysis ===\n")
      cat("  Shock time:", shock_time, "\n")
      cat("  Treatment groups:", unique(metrics$treatment_group), "\n")
      cat("  Pre-shock steps:", sum(unique(metrics$chain_step_id) < shock_time), "\n")
      cat("  Post-shock steps:", sum(unique(metrics$chain_step_id) >= shock_time), "\n")
    }
    
    # Method 1: Manual DiD calculation (original approach but fixed)
    did_summary <- metrics %>%
      mutate(
        period = factor(ifelse(chain_step_id < shock_time, "Pre", "Post"), levels = c("Pre", "Post")),
        treatment = ifelse(strategy == "100", "Treated", "Control")
      ) %>%
      group_by(period, treatment) %>%
      summarise(
        mean_value = mean(!!sym(metric), na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      )
    
    # Check that we have all required groups
    required_groups <- expand.grid(
      period = c("Pre", "Post"),
      treatment = c("Treated", "Control"),
      stringsAsFactors = FALSE
    )
    
    # Merge to ensure all groups exist (fill missing with NA)
    did_summary_complete <- required_groups %>%
      left_join(did_summary, by = c("period", "treatment")) %>%
      replace_na(list(mean_value = NA, n_obs = 0))
    
    # Pivot to calculate changes
    did_data <- did_summary_complete %>%
      pivot_wider(
        names_from = period,
        values_from = mean_value
      ) %>%
      mutate(
        change = Post - Pre
      )
    
    # Calculate DiD estimate - now guaranteed to be single value
    treated_idx <- which(did_data$treatment == "Treated")
    control_idx <- which(did_data$treatment == "Control")
    
    if (length(treated_idx) == 1 && length(control_idx) == 1 && 
        !is.na(did_data$change[treated_idx]) && !is.na(did_data$change[control_idx])) {
      did_estimate <- did_data$change[treated_idx] - did_data$change[control_idx]
    } else {
      did_estimate <- NA
    }
    
    # Method 2: Use the did package (like K_AC analysis does)
    # This is more robust and provides standard errors
    if (!requireNamespace("did", quietly = TRUE)) {
      cat("Note: Install the 'did' package for more robust DiD analysis\n")
    } else {
      # Prepare data for did package
      did_dat <- metrics %>%
        group_by(chain_step_id, actor_id) %>%
        summarize(
          value_mean = mean(!!sym(metric), na.rm = TRUE),
          strategy = first(strategy),
          treatment_group = first(treatment_group),
          .groups = "drop"
        ) %>%
        mutate(actor_id = as.numeric(as.character(actor_id)))
      
      # Run att_gt estimation
      tryCatch({
        did_attgt <- did::att_gt(
          yname = 'value_mean',
          tname = 'chain_step_id',
          idname = 'actor_id',
          gname = 'treatment_group',
          data = did_dat,
          panel = TRUE,
          allow_unbalanced_panel = TRUE,
          control_group = 'notyettreated',
          anticipation = 0,
          alp = 0.05,
          bstrap = TRUE,
          cband = TRUE,
          biters = 1000,
          est_method = "dr",
          base_period = 'universal',
          print_details = debug,
          pl = FALSE,
          cores = 1
        )
        
        # Get dynamic effects
        did_dyna <- did::aggte(did_attgt, type = 'dynamic')
        
        # Get overall ATT
        did_overall <- did::aggte(did_attgt, type = 'simple')
        
        if (debug) {
          cat("\n=== DiD Package Results ===\n")
          print(summary(did_overall))
        }
        
      }, error = function(e) {
        cat("Error using did package:", e$message, "\n")
        did_attgt <- NULL
        did_dyna <- NULL
        did_overall <- NULL
      })
    }
    
    # Create plot
    p <- ggplot(metrics, aes(x = chain_step_id, y = !!sym(metric))) +
      stat_summary(
        aes(color = strategy),
        fun = mean,
        geom = "line",
        size = 1.5
      ) +
      stat_summary(
        aes(fill = strategy),
        fun.data = mean_se,
        geom = "ribbon",
        alpha = 0.2
      ) +
      geom_vline(xintercept = shock_time, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("0" = "#2C3E50", "100" = "#E74C3C")) +
      scale_fill_manual(values = c("0" = "#2C3E50", "100" = "#E74C3C")) +
      labs(
        title = paste("Simple Exploration Measure:", metric),
        subtitle = sprintf("DiD = %.4f", ifelse(is.na(did_estimate), 0, did_estimate)),
        x = "Simulation Step",
        y = metric,
        color = "Strategy",
        fill = "Strategy"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Print results
    cat("\n=== SIMPLE EXPLORATION ANALYSIS ===\n")
    cat("Metric:", metric, "\n")
    cat("Shock time:", shock_time, "\n")
    cat("\nMeans by period and treatment:\n")
    print(did_summary_complete)
    cat("\nChanges by treatment:\n")
    print(did_data)
    cat("\nDifference-in-Differences:", 
        ifelse(is.na(did_estimate), "NA", sprintf("%.4f", did_estimate)), "\n")
    
    if (!is.na(did_estimate)) {
      if (did_estimate < -0.001) {
        cat("Interpretation: Subsidies REDUCE", metric, "by", sprintf("%.4f", abs(did_estimate)), "\n")
      } else if (did_estimate > 0.001) {
        cat("Interpretation: Subsidies INCREASE", metric, "by", sprintf("%.4f", did_estimate), "\n")
      } else {
        cat("Interpretation: No meaningful effect of subsidies on", metric, "\n")
      }
    }
    
    # Save plot if requested
    if (plot_save && !is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      filename <- file.path(plot_dir, paste0("simple_exploration_", metric, ".png"))
      ggsave(filename, p, width = 10, height = 6, dpi = 300)
      cat("\nPlot saved to:", filename, "\n")
    }
    
    return(list(
      metrics = metrics,
      plot = p,
      did_summary = did_summary_complete,
      did_data = did_data,
      did_estimate = did_estimate,
      did_results = if (exists("did_overall")) did_overall else NULL
    ))
  },
  
  
  compute_exploration_shocks = function(metric = "exploration", verbose = FALSE) {
    
    if(is.null(self$theta_shocks))
      stop('self$theta_shocks is missing.')
    
    # CRITICAL FIX: Start with K4 data structure like K_AC does
    Kdf <- self$get_K4_df()
    
    # Get base structure from K_AC data (ensures complete panel)
    base_df <- Kdf %>%
      filter(effect == "K_AC") %>%
      select(chain_step_id, actor_id, strategy) %>%
      distinct()
    
    # Get exploration metrics
    exp_metrics <- self$calculate_explore_exploit_risk_adjusted()
    
    # Join exploration data to base structure (ensures no missing actor-time combinations)
    metrics_df <- base_df %>%
      left_join(
        exp_metrics,
        by = c("chain_step_id", "actor_id", "strategy")
      )
    
    # Select the metric we want and rename to 'value' for consistency
    metrics_df <- metrics_df %>%
      mutate(value = !!sym(metric)) %>%
      select(chain_step_id, actor_id, strategy, value)
    
    # Initialize columns exactly like K_AC
    metrics_df <- metrics_df %>%
      mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
    
    # Process shocks exactly like compute_K_shocks
    theta_shocks <- self$theta_shocks
    
    for (i in 1:length(theta_shocks)) {
      shock <- theta_shocks[[i]]
      
      idx <- which(metrics_df$chain_step_id %in% shock$chain_step_ids)
      
      metrics_df$shock_id[idx] <- i
      metrics_df$shock_on[idx] <- shock$shock_on
      metrics_df$shock_label[idx] <- ifelse(is.null(shock$label), as.character(i), shock$label)
      
      if (shock$shock_on == 1) {
        # Set treatment_group for strategy 100 actors
        # This assigns the treatment timing to ALL rows for treated actors
        treated_actors <- unique(metrics_df$actor_id[metrics_df$strategy == "100"])
        for (actor in treated_actors) {
          metrics_df$treatment_group[metrics_df$actor_id == actor] <- min(shock$chain_step_ids)
        }
      }
    }
    
    # Rest of the function continues as before...
    actor_strats <- as.factor(self$get_actor_strategies())
    actor_strat_lvls <- levels(actor_strats)
    
    trt_lvl <- "100"
    ctrl_lvl <- "0"
    
    test_list <- list()
    
    # Prepare data EXACTLY like K_AC
    did_dat <- metrics_df %>%
      filter(strategy %in% c(trt_lvl, ctrl_lvl)) %>%
      group_by(chain_step_id, actor_id) %>%
      summarize(
        value_mean = mean(value, na.rm = TRUE),
        strategy = as.factor(first(strategy)),
        treatment_group = first(treatment_group),
        .groups = "drop"
      ) %>%
      mutate(actor_id = as.numeric(as.character(actor_id)))
    
    did_dat$treatment_group <- as.numeric(did_dat$treatment_group)
    
    if (verbose) {
      cat("\n=== Processing:", trt_lvl, "vs", ctrl_lvl, "===\n")
      cat("Data dimensions:", nrow(did_dat), "x", ncol(did_dat), "\n")
      cat("Unique actors:", n_distinct(did_dat$actor_id), "\n")
      cat("Unique time periods:", n_distinct(did_dat$chain_step_id), "\n")
      cat("Treatment groups:", sort(unique(did_dat$treatment_group)), "\n")
    }
    
    # Run att_gt with same parameters as K_AC
    did_attgt <- did::att_gt(
      yname = 'value_mean',
      tname = 'chain_step_id',
      idname = 'actor_id',
      gname = 'treatment_group',
      data = did_dat,
      panel = TRUE,
      allow_unbalanced_panel = TRUE,
      control_group = 'notyettreated',
      anticipation = 0,
      weightsname = NULL,
      alp = 0.05,
      bstrap = TRUE,
      cband = TRUE,
      biters = 2000,
      clustervars = NULL,
      est_method = "dr",
      base_period = 'universal',
      print_details = verbose,
      pl = TRUE,
      cores = 4
    )
    
    # Aggregate results
    did_group <- did::aggte(did_attgt, type = 'group', cband = FALSE)
    did_dyna <- did::aggte(did_attgt, type = 'dynamic')
    
    first_treated_step <- min(did_dat$treatment_group[did_dat$treatment_group > 0])
    
    test_key <- sprintf('treatment_%s__control_%s', trt_lvl, ctrl_lvl)
    
    test_list[[test_key]] <- list(
      treatment_strategy = trt_lvl,
      control_strategy = ctrl_lvl,
      test_key = test_key,
      first_treated_step = first_treated_step,
      stat_type = metric,
      did = list(
        group = did_group,
        dynamic = did_dyna
      )
    )
    
    return(test_list)
  },
  
  # Alternative diagnostic function to check what's happening
  diagnose_exploration_treatment_assignment = function() {
    
    # Get metrics
    metrics <- self$calculate_explore_exploit_risk_adjusted()
    
    # Check theta_shocks structure
    cat("\n=== THETA SHOCKS STRUCTURE ===\n")
    for (i in 1:length(self$theta_shocks)) {
      shock <- self$theta_shocks[[i]]
      cat("\nShock", i, ":\n")
      cat("- shock_on:", shock$shock_on, "\n")
      cat("- chain_step_ids range:", min(shock$chain_step_ids), "-", max(shock$chain_step_ids), "\n")
      cat("- label:", ifelse(is.null(shock$label), "NULL", shock$label), "\n")
      if (!is.null(shock$effectName)) {
        cat("- effectName:", shock$effectName, "\n")
      }
    }
    
    # Check shockable effects
    cat("\n=== SHOCKABLE EFFECTS ===\n")
    shock_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% 
      filter(grepl('(self\\$)?shockable[|]', effect_key, ignore.case = TRUE))
    
    if (nrow(shock_effs) > 0) {
      print(shock_effs %>% select(effectName, effect_key, theta))
    } else {
      cat("No shockable effects found!\n")
    }
    
    # Check strategies
    cat("\n=== ACTOR STRATEGIES ===\n")
    strat_table <- table(self$get_actor_strategies())
    print(strat_table)
    
    # Test the treatment assignment
    cat("\n=== TESTING TREATMENT ASSIGNMENT ===\n")
    
    # Run compute_exploration_shocks with verbose
    test_results <- self$compute_exploration_shocks(metric = "exploration", verbose = TRUE)
    
    return(invisible(NULL))
  },
  
  test_exploration_shocks_did = function(test_type = 'dynamic', metric = 'exploration') {
    
    # Get the test results using compute_exploration_shocks
    test_list <- self$compute_exploration_shocks(metric = metric, verbose = FALSE)
    
    # Extract results exactly like test_shocks_did
    test_key <- switch(test_type, 
                       group = 'group',
                       dynamic = 'dynamic')
    
    if (is.null(test_key))
      stop(sprintf('test_type=`%s` not supported.', test_type))
    
    # Process results
    reg_table_list <- lapply(test_list, function(test) {
      model_test <- test$did[[test_key]]
      reg_df <- broom::tidy(model_test) %>%
        mutate(`(sig.)` = ifelse((point.conf.low * point.conf.high) < 0 | is.na(point.conf.low), '', ' * ')) %>%
        select(!c('type')) %>%
        mutate(first_treated_step = test$first_treated_step)
      
      reg_df
    })
    
    return(reg_table_list)
  },
  
  plot_exploration_event_study = function(metric = "exploration", save_plot = FALSE, plot_dir = NULL) {
    
    # Get dynamic results
    ed <- data.table::rbindlist(
      self$test_exploration_shocks_did(test_type = 'dynamic', metric = metric), 
      idcol = 'test_id'
    )
    
    # Add metadata
    ed <- ed %>%
      mutate(
        test_type = 'dynamic',
        stat_type = toupper(metric),
        combined_comparison = 'Common Treatment Scale'
      ) %>%
      filter(event.time >= -1)
    
    # Get simulation parameters
    sim_title_str <- self$get_structure_model_param_str()
    
    # Find shock timing
    shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
    shock_start <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
    shock_label <- ifelse(!is.null(self$theta_shocks[[shock_idx]]$label), 
                          self$theta_shocks[[shock_idx]]$label, "subsidy")
    
    # Create the plot matching K_AC style
    p <- ggplot(ed, aes(x = event.time, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
      
      # Add shock region
      annotate("rect", xmin = -0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
               fill = 'darkorange', alpha = 0.05) +
      
      # Add confidence bands and estimates
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                  alpha = 0.2, fill = "cyan3") +
      geom_line(color = "cyan3", size = 1.2) +
      geom_point(color = "cyan3", size = 2) +
      
      # Add shock label
      annotate("text", x = max(ed$event.time) * 0.5, 
               y = max(ed$conf.high) * 0.9,
               label = shock_label, vjust = 0, size = 2.7, color = 'black') +
      
      facet_grid(stat_type ~ test_id) +
      
      labs(
        x = sprintf("Event Time\n(Shock Starts at Simulated Decision Chain Step %d)", shock_start),
        y = "Avg Treatment Effect on Treated (ATT)",
        title = paste("Multiperiod Diff-in-Diff Test of", toupper(metric)),
        subtitle = sim_title_str
      ) +
      
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "gray90", color = "gray50"),
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      )
    
    # Save plot if requested
    if (save_plot && !is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      filename <- file.path(plot_dir, paste0("exploration_event_study_", metric, ".png"))
      ggsave(filename, p, width = 10, height = 8, dpi = 300)
      cat("Plot saved to:", filename, "\n")
    }
    
    return(p)
  },
  
  plot_exploration_did_combined = function(metrics = c("exploration", "n_new", "prop_new"), 
                                           save_plot = FALSE, plot_dir = NULL) {

    # Create data for all metrics
    all_data <- list()
    
    for (metric in metrics) {
      ed <- data.table::rbindlist(
        self$test_exploration_shocks_did(test_type = 'dynamic', metric = metric), 
        idcol = 'test_id'
      )
      
      ed <- ed %>%
        mutate(
          test_type = 'dynamic',
          stat_type = toupper(metric),
          combined_comparison = 'Common Treatment Scale'
        ) %>%
        filter(event.time >= -1)
      
      all_data[[metric]] <- ed
    }
    
    # Combine all data
    combined_data <- bind_rows(all_data)
    
    # Get simulation parameters
    sim_title_str <- self$get_structure_model_param_str()
    
    # Find shock info
    shock_idx <- which(sapply(self$theta_shocks, function(x) x$shock_on == 1))[1]
    shock_start <- min(self$theta_shocks[[shock_idx]]$chain_step_ids)
    
    # Create separate plots for each metric (matching K_AC multi-panel style)
    plot_list <- list()
    
    for (metric in unique(combined_data$stat_type)) {
      metric_data <- combined_data %>% filter(stat_type == metric)
      
      p <- ggplot(metric_data, aes(x = event.time, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
        
        # Add shock region
        annotate("rect", xmin = -0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
                 fill = 'darkorange', alpha = 0.05) +
        
        # Add estimates
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                    alpha = 0.2, fill = "cyan3") +
        geom_line(color = "cyan3", size = 1.2) +
        geom_point(color = "cyan3", size = 2) +
        
        # Add shock label
        annotate("text", x = max(metric_data$event.time) * 0.5, 
                 y = max(metric_data$conf.high) * 0.9,
                 label = "subsidy", vjust = 0, size = 2.7, color = 'black') +
        
        facet_wrap(~ stat_type) +
        
        labs(x = NULL, y = "Avg Treatment Effect on Treated (ATT)",
             title = metric) +
        
        theme_minimal() +
        theme(
          strip.background = element_rect(fill = "gray90"),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      
      plot_list[[metric]] <- p
    }
    
    # Combine plots
    combined_plot <- ggarrange(plotlist = plot_list, nrow = length(plot_list), 
                               common.legend = FALSE)
    
    # Add common title and x-label
    combined_plot <- ggpubr::annotate_figure(
      combined_plot,
      top = ggpubr::text_grob(paste("Multiperiod Diff-in-Diff Tests of Exploration Metrics\n", sim_title_str), 
                      face = "bold", size = 14),
      bottom = ggpubr::text_grob(sprintf("Event Time\n(Shock Starts at Simulated Decision Chain Step %d)", shock_start))
    )
    
    # Save if requested
    if (save_plot && !is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      filename <- file.path(plot_dir, "exploration_did_combined.png")
      ggsave(filename, combined_plot, width = 10, height = 12, dpi = 300)
      cat("Plot saved to:", filename, "\n")
    }
    
    return(combined_plot)
  }, 
  
  
  # Diagnostic function to compare K_AC and exploration data structures
  compare_data_structures = function() {
    
    cat("\n=== COMPARING K_AC AND EXPLORATION DATA STRUCTURES ===\n")
    
    # Get K_AC data using compute_K_shocks
    k_ac_results <- self$compute_K_shocks(K_type = "K_AC", verbose = FALSE)
    
    # Extract the data that would be passed to att_gt for K_AC
    k_ac_test <- k_ac_results[[1]]  # First test
    
    # Now let's manually prepare K_AC data like compute_K_shocks does
    Kdf <- self$get_K4_df()
    
    # Process K_AC data exactly like compute_K_shocks
    Kdf_KAC <- Kdf %>% 
      filter(effect == "K_AC") %>%
      mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
    
    # Apply shock information
    theta_shocks <- self$theta_shocks
    for (i in 1:length(theta_shocks)) {
      shock <- theta_shocks[[i]]
      Kdf_idx <- which(Kdf_KAC$chain_step_id %in% shock$chain_step_ids)
      
      Kdf_KAC$shock_id[Kdf_idx] <- i
      Kdf_KAC$shock_on[Kdf_idx] <- shock$shock_on
      Kdf_KAC$shock_label[Kdf_idx] <- ifelse(is.null(shock$label), as.character(i), shock$label)
      
      if (shock$shock_on == 1) {
        # This is the key part - how K_AC sets treatment_group
        shock_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% 
          filter(grepl('(self\\$)?shockable[|]', effect_key, ignore.case = TRUE))
        
        if (nrow(shock_effs) > 0) {
          for (j in 1:nrow(shock_effs)) {
            eff <- shock_effs[j,]
            
            if (grepl('strat_100', eff$effectName) || grepl('strat_100', eff$effect_key)) {
              strat_actor_idx <- which(Kdf_KAC$strategy == '100')
              Kdf_KAC$treatment_group[strat_actor_idx] <- min(shock$chain_step_ids)
            }
            if (grepl('strat_0', eff$effectName) || grepl('strat_0', eff$effect_key)) {
              strat_actor_idx <- which(Kdf_KAC$strategy == '0')
              Kdf_KAC$treatment_group[strat_actor_idx] <- min(shock$chain_step_ids)
            }
          }
        } else {
          # Fallback for K_AC when no shockable effects
          treated_actors <- unique(Kdf_KAC$actor_id[Kdf_KAC$strategy == "100"])
          for (actor in treated_actors) {
            Kdf_KAC$treatment_group[Kdf_KAC$actor_id == actor] <- min(shock$chain_step_ids)
          }
        }
      }
    }
    
    # Prepare K_AC data for did
    did_Kdf_dat <- Kdf_KAC %>%
      filter(strategy %in% c("100", "0")) %>%
      group_by(chain_step_id, actor_id) %>%
      summarize(
        value_mean = mean(value, na.rm = TRUE),
        strategy = as.factor(first(strategy)),
        treatment_group = first(treatment_group),
        .groups = "drop"
      ) %>%
      mutate(actor_id = as.numeric(as.character(actor_id)))
    
    did_Kdf_dat$treatment_group <- as.numeric(did_Kdf_dat$treatment_group)
    
    # Now get exploration data
    exp_metrics <- self$calculate_explore_exploit_risk_adjusted()
    
    # Process exploration data with same logic
    exp_metrics_processed <- exp_metrics %>%
      mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
    
    # Apply shocks to exploration data  
    for (i in 1:length(theta_shocks)) {
      shock <- theta_shocks[[i]]
      exp_idx <- which(exp_metrics_processed$chain_step_id %in% shock$chain_step_ids)
      
      exp_metrics_processed$shock_id[exp_idx] <- i
      exp_metrics_processed$shock_on[exp_idx] <- shock$shock_on
      exp_metrics_processed$shock_label[exp_idx] <- ifelse(is.null(shock$label), as.character(i), shock$label)
      
      if (shock$shock_on == 1) {
        # Set treatment_group for strategy 100
        treated_actors <- unique(exp_metrics_processed$actor_id[exp_metrics_processed$strategy == "100"])
        for (actor in treated_actors) {
          exp_metrics_processed$treatment_group[exp_metrics_processed$actor_id == actor] <- min(shock$chain_step_ids)
        }
      }
    }
    
    # Prepare exploration data for did
    did_exp_dat <- exp_metrics_processed %>%
      filter(strategy %in% c("100", "0")) %>%
      group_by(chain_step_id, actor_id) %>%
      summarize(
        value_mean = mean(exploration, na.rm = TRUE),
        strategy = as.factor(first(strategy)),
        treatment_group = first(treatment_group),
        .groups = "drop"
      ) %>%
      mutate(actor_id = as.numeric(as.character(actor_id)))
    
    did_exp_dat$treatment_group <- as.numeric(did_exp_dat$treatment_group)
    
    # Compare structures
    cat("\n=== K_AC DID DATA ===\n")
    cat("Dimensions:", nrow(did_Kdf_dat), "x", ncol(did_Kdf_dat), "\n")
    cat("Unique actors:", n_distinct(did_Kdf_dat$actor_id), "\n")
    cat("Unique time periods:", n_distinct(did_Kdf_dat$chain_step_id), "\n")
    cat("Treatment groups:", sort(unique(did_Kdf_dat$treatment_group)), "\n")
    cat("\nFirst few rows:\n")
    print(head(did_Kdf_dat, 10))
    
    cat("\n\n=== EXPLORATION DID DATA ===\n")
    cat("Dimensions:", nrow(did_exp_dat), "x", ncol(did_exp_dat), "\n")
    cat("Unique actors:", n_distinct(did_exp_dat$actor_id), "\n")
    cat("Unique time periods:", n_distinct(did_exp_dat$chain_step_id), "\n")
    cat("Treatment groups:", sort(unique(did_exp_dat$treatment_group)), "\n")
    cat("\nFirst few rows:\n")
    print(head(did_exp_dat, 10))
    
    # Check panel balance
    cat("\n\n=== PANEL BALANCE CHECK ===\n")
    
    k_ac_balance <- did_Kdf_dat %>%
      group_by(actor_id, treatment_group) %>%
      summarise(n_periods = n(), .groups = "drop")
    
    exp_balance <- did_exp_dat %>%
      group_by(actor_id, treatment_group) %>%
      summarise(n_periods = n(), .groups = "drop")
    
    cat("\nK_AC panel balance:\n")
    print(table(k_ac_balance$n_periods, k_ac_balance$treatment_group))
    
    cat("\nExploration panel balance:\n")
    print(table(exp_balance$n_periods, exp_balance$treatment_group))
    
    # Check if actors have observations in both pre and post periods
    cat("\n\n=== PRE/POST PERIOD COVERAGE ===\n")
    
    shock_time <- 39
    
    k_ac_coverage <- did_Kdf_dat %>%
      mutate(period = ifelse(chain_step_id < shock_time, "pre", "post")) %>%
      group_by(actor_id, strategy) %>%
      summarise(
        has_pre = any(period == "pre"),
        has_post = any(period == "post"),
        n_pre = sum(period == "pre"),
        n_post = sum(period == "post"),
        .groups = "drop"
      )
    
    exp_coverage <- did_exp_dat %>%
      mutate(period = ifelse(chain_step_id < shock_time, "pre", "post")) %>%
      group_by(actor_id, strategy) %>%
      summarise(
        has_pre = any(period == "pre"),
        has_post = any(period == "post"),
        n_pre = sum(period == "pre"),
        n_post = sum(period == "post"),
        .groups = "drop"
      )
    
    cat("\nK_AC coverage summary:\n")
    print(k_ac_coverage)
    
    cat("\nExploration coverage summary:\n")
    print(exp_coverage)
    
    return(list(
      k_ac_data = did_Kdf_dat,
      exp_data = did_exp_dat,
      k_ac_balance = k_ac_balance,
      exp_balance = exp_balance
    ))
  },
  
  # Simplified fix for compute_exploration_shocks
  compute_exploration_shocks_fixed = function(metric = "exploration", verbose = FALSE) {
    
    if(is.null(self$theta_shocks))
      stop('self$theta_shocks is missing.')
    
    # Get K4 data frame instead of just exploration metrics
    # This ensures we have the same data structure as K_AC
    full_df <- self$get_K4_df()
    
    # Get exploration metrics
    exp_metrics <- self$calculate_explore_exploit_risk_adjusted()
    
    # Create a combined dataframe that matches K4 structure
    # We need to ensure each actor-time combination exists
    base_structure <- full_df %>%
      filter(effect == "K_AC") %>%  # Use K_AC as template for structure
      select(chain_step_id, actor_id, strategy) %>%
      distinct()
    
    # Join exploration metrics to base structure
    combined_df <- base_structure %>%
      left_join(
        exp_metrics %>% select(chain_step_id, actor_id, all_of(metric)),
        by = c("chain_step_id", "actor_id")
      ) %>%
      rename(value = all_of(metric))
    
    # Now apply shock processing exactly like K_AC
    combined_df <- combined_df %>%
      mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
    
    theta_shocks <- self$theta_shocks
    
    for (i in 1:length(theta_shocks)) {
      shock <- theta_shocks[[i]]
      
      idx <- which(combined_df$chain_step_id %in% shock$chain_step_ids)
      
      combined_df$shock_id[idx] <- i
      combined_df$shock_on[idx] <- shock$shock_on
      combined_df$shock_label[idx] <- ifelse(is.null(shock$label), as.character(i), shock$label)
      
      if (shock$shock_on == 1) {
        # Check for shockable effects
        shock_effs <- self$get_rsiena_effects_theta_df(no_rates = TRUE) %>% 
          filter(grepl('(self\\$)?shockable[|]', effect_key, ignore.case = TRUE))
        
        if (nrow(shock_effs) > 0) {
          # Process based on shockable effects
          for (j in 1:nrow(shock_effs)) {
            eff <- shock_effs[j,]
            
            if (grepl('strat_100', eff$effectName) || grepl('strat_100', eff$effect_key)) {
              strat_actor_idx <- which(combined_df$strategy == '100')
              combined_df$treatment_group[strat_actor_idx] <- min(shock$chain_step_ids)
            }
          }
        } else {
          # Fallback: treat all strategy 100 actors
          if (verbose) cat("No shockable effects found, using default strategy '100' as treated\n")
          strat_actor_idx <- which(combined_df$strategy == '100')
          combined_df$treatment_group[strat_actor_idx] <- min(shock$chain_step_ids)
        }
      }
    }
    
    # Continue with rest of compute_K_shocks logic...
    # [Rest of the function remains the same]
    
    # Get strategy levels
    actor_strats <- as.factor(self$get_actor_strategies())
    actor_strat_lvls <- levels(actor_strats)
    
    trt_lvl <- "100"
    ctrl_lvl <- "0"
    
    test_list <- list()
    
    # Prepare data for did package
    did_dat <- combined_df %>%
      filter(strategy %in% c(trt_lvl, ctrl_lvl)) %>%
      group_by(chain_step_id, actor_id) %>%
      summarize(
        value_mean = mean(value, na.rm = TRUE),
        strategy = as.factor(first(strategy)),
        treatment_group = first(treatment_group),
        .groups = "drop"
      ) %>%
      mutate(actor_id = as.numeric(as.character(actor_id)))
    
    did_dat$treatment_group <- as.numeric(did_dat$treatment_group)
    
    if (verbose) {
      cat("\nProcessing treatment: 100 vs control: 0\n")
      cat("Data rows:", nrow(did_dat), "\n")
      cat("Treatment groups:", unique(did_dat$treatment_group), "\n")
    }
    
    # Run att_gt
    did_attgt <- did::att_gt(
      yname = 'value_mean',
      tname = 'chain_step_id',
      idname = 'actor_id',
      gname = 'treatment_group',
      data = did_dat,
      panel = TRUE,
      allow_unbalanced_panel = TRUE,
      control_group = 'notyettreated',
      anticipation = 0,
      weightsname = NULL,
      alp = 0.05,
      bstrap = TRUE,
      cband = TRUE,
      biters = 2000,
      clustervars = NULL,
      est_method = "dr",
      base_period = 'universal',
      print_details = verbose,
      pl = TRUE,
      cores = 4
    )
    
    # Aggregate results
    did_group <- did::aggte(did_attgt, type = 'group', cband = FALSE)
    did_dyna <- did::aggte(did_attgt, type = 'dynamic')
    
    first_treated_step <- min(did_dat$treatment_group[did_dat$treatment_group > 0])
    
    test_key <- sprintf('treatment_%s__control_%s', trt_lvl, ctrl_lvl)
    
    test_list[[test_key]] <- list(
      treatment_strategy = trt_lvl,
      control_strategy = ctrl_lvl,
      test_key = test_key,
      first_treated_step = first_treated_step,
      stat_type = metric,
      did = list(
        group = did_group,
        dynamic = did_dyna
      )
    )
    
    return(test_list)
  }, 
  
  
  # Detailed diagnostic to understand why did package is failing
  
  diagnose_did_detailed = function(metric = "exploration") {
    
    cat("\n=== DETAILED DID DIAGNOSTIC ===\n")
    
    # Get K_AC results (which work)
    k_ac_results <- self$compute_K_shocks(K_type = "K_AC", verbose = FALSE)
    
    # Get exploration results (which don't work) 
    exp_results <- self$compute_exploration_shocks(metric = metric, verbose = FALSE)
    
    # Extract the data passed to att_gt
    # For K_AC
    Kdf <- self$get_K4_df()
    Kdf_processed <- Kdf %>% 
      filter(effect == "K_AC") %>%
      mutate(
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
    
    # Apply shocks
    theta_shocks <- self$theta_shocks
    for (i in 1:length(theta_shocks)) {
      shock <- theta_shocks[[i]]
      idx <- which(Kdf_processed$chain_step_id %in% shock$chain_step_ids)
      Kdf_processed$shock_id[idx] <- i
      Kdf_processed$shock_on[idx] <- shock$shock_on
      
      if (shock$shock_on == 1) {
        # Set treatment for strategy 100
        strat_idx <- which(Kdf_processed$strategy == '100')
        Kdf_processed$treatment_group[strat_idx] <- min(shock$chain_step_ids)
      }
    }
    
    # Prepare K_AC data
    k_ac_did <- Kdf_processed %>%
      filter(strategy %in% c("100", "0")) %>%
      group_by(chain_step_id, actor_id) %>%
      summarize(
        value_mean = mean(value, na.rm = TRUE),
        strategy = as.factor(first(strategy)),
        treatment_group = first(treatment_group),
        .groups = "drop"
      ) %>%
      mutate(actor_id = as.numeric(as.character(actor_id)))
    
    k_ac_did$treatment_group <- as.numeric(k_ac_did$treatment_group)
    
    # For exploration
    base_df <- Kdf %>%
      filter(effect == "K_AC") %>%
      select(chain_step_id, actor_id, strategy) %>%
      distinct()
    
    exp_metrics <- self$calculate_explore_exploit_risk_adjusted()
    
    exp_df <- base_df %>%
      left_join(exp_metrics, by = c("chain_step_id", "actor_id", "strategy")) %>%
      mutate(
        value = !!sym(metric),
        shock_id = NA,
        shock_label = NA,
        shock_on = NA,
        treatment_group = 0
      )
    
    # Apply shocks to exploration
    for (i in 1:length(theta_shocks)) {
      shock <- theta_shocks[[i]]
      idx <- which(exp_df$chain_step_id %in% shock$chain_step_ids)
      exp_df$shock_id[idx] <- i
      exp_df$shock_on[idx] <- shock$shock_on
      
      if (shock$shock_on == 1) {
        strat_idx <- which(exp_df$strategy == '100')
        exp_df$treatment_group[strat_idx] <- min(shock$chain_step_ids)
      }
    }
    
    # Prepare exploration data
    exp_did <- exp_df %>%
      filter(strategy %in% c("100", "0")) %>%
      group_by(chain_step_id, actor_id) %>%
      summarize(
        value_mean = mean(value, na.rm = TRUE),
        strategy = as.factor(first(strategy)),
        treatment_group = first(treatment_group),
        .groups = "drop"
      ) %>%
      mutate(actor_id = as.numeric(as.character(actor_id)))
    
    exp_did$treatment_group <- as.numeric(exp_did$treatment_group)
    
    # Compare outcome variables
    cat("\n=== OUTCOME VARIABLE COMPARISON ===\n")
    
    cat("\nK_AC value_mean summary:\n")
    print(summary(k_ac_did$value_mean))
    cat("Missing values:", sum(is.na(k_ac_did$value_mean)), "\n")
    cat("Variance:", var(k_ac_did$value_mean, na.rm = TRUE), "\n")
    
    cat("\nExploration value_mean summary:\n")
    print(summary(exp_did$value_mean))
    cat("Missing values:", sum(is.na(exp_did$value_mean)), "\n")
    cat("Variance:", var(exp_did$value_mean, na.rm = TRUE), "\n")
    
    # Check variation by treatment group and time
    cat("\n=== VARIATION BY GROUP AND TIME ===\n")
    
    k_ac_var <- k_ac_did %>%
      group_by(treatment_group, chain_step_id < 39) %>%
      summarise(
        mean_val = mean(value_mean, na.rm = TRUE),
        sd_val = sd(value_mean, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    exp_var <- exp_did %>%
      group_by(treatment_group, chain_step_id < 39) %>%
      summarise(
        mean_val = mean(value_mean, na.rm = TRUE),
        sd_val = sd(value_mean, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    cat("\nK_AC variation by group/period:\n")
    print(k_ac_var)
    
    cat("\nExploration variation by group/period:\n")
    print(exp_var)
    
    # Check specific actors
    cat("\n=== SAMPLE ACTOR TRAJECTORIES ===\n")
    
    # Pick one treated and one control actor
    treated_actor <- k_ac_did %>% filter(treatment_group == 39) %>% pull(actor_id) %>% unique() %>% first()
    control_actor <- k_ac_did %>% filter(treatment_group == 0) %>% pull(actor_id) %>% unique() %>% first()
    
    cat("\nK_AC - Actor", treated_actor, "(treated) around shock:\n")
    k_ac_sample <- k_ac_did %>%
      filter(actor_id == treated_actor, chain_step_id %in% 35:45) %>%
      select(chain_step_id, value_mean, treatment_group)
    print(k_ac_sample)
    
    cat("\nExploration - Actor", treated_actor, "(treated) around shock:\n")
    exp_sample <- exp_did %>%
      filter(actor_id == treated_actor, chain_step_id %in% 35:45) %>%
      select(chain_step_id, value_mean, treatment_group)
    print(exp_sample)
    
    # Check if data is sorted
    cat("\n=== DATA SORTING CHECK ===\n")
    cat("K_AC sorted by time and id?", 
        all(k_ac_did == k_ac_did %>% arrange(chain_step_id, actor_id)), "\n")
    cat("Exploration sorted by time and id?", 
        all(exp_did == exp_did %>% arrange(chain_step_id, actor_id)), "\n")
    
    # Try running att_gt with different options
    cat("\n=== TESTING ATT_GT WITH DIFFERENT OPTIONS ===\n")
    
    # Test 1: Try with sorted data
    exp_did_sorted <- exp_did %>% arrange(actor_id, chain_step_id)
    
    cat("\nTest 1: With explicitly sorted data\n")
    tryCatch({
      test1 <- did::att_gt(
        yname = 'value_mean',
        tname = 'chain_step_id',
        idname = 'actor_id',
        gname = 'treatment_group',
        data = exp_did_sorted,
        panel = TRUE,
        control_group = 'notyettreated',
        est_method = "dr"
      )
      cat("Success!\n")
    }, error = function(e) {
      cat("Error:", e$message, "\n")
    })
    
    # Test 2: Try with reg method instead of dr
    cat("\nTest 2: With 'reg' estimation method\n")
    tryCatch({
      test2 <- did::att_gt(
        yname = 'value_mean',
        tname = 'chain_step_id',
        idname = 'actor_id',
        gname = 'treatment_group',
        data = exp_did,
        panel = TRUE,
        control_group = 'notyettreated',
        est_method = "reg"  # Changed from "dr"
      )
      cat("Success!\n")
    }, error = function(e) {
      cat("Error:", e$message, "\n")
    })
    
    # Test 3: Check if it's a data type issue
    cat("\n=== DATA TYPE CHECK ===\n")
    cat("\nK_AC data types:\n")
    print(sapply(k_ac_did, class))
    
    cat("\nExploration data types:\n")
    print(sapply(exp_did, class))
    
    return(list(
      k_ac_data = k_ac_did,
      exp_data = exp_did,
      k_ac_var = k_ac_var,
      exp_var = exp_var
    ))
  }
 
  
  # Add these as methods to your R6 class
  
  # Add this at the end of your R6 class definition (don't forget the comma before it)
 
  
  # Usage:
  
  # Usage:
  
  # Usage examples:
  # First run diagnostics to understand your data
  
  # Debug first to understand the data
  
  # Then run analyses
  
  # Usage examples:
  # First run diagnostics to understand your data
  
  # Debug first to understand the data
  
  # Then run analyses

  
  ,

  # =========================================================================
  # merge_nodes()
  # Implements Hernandez & Menon (2018) "node collapse" mechanism.
  # When two firms merge via M&A, a node is removed from the competitive

  # network and K-system dimensions reconfigure for all remaining firms.
  # See also: Feldman & Hernandez (2022) synergy typology.
  # =========================================================================
  merge_nodes = function(node_type = 'actor', node_ids = c(1, 2),
                         merge_method = 'union', integration_friction = 0.0,
                         verbose = FALSE) {
    stopifnot(node_type %in% c('actor', 'component'))
    stopifnot(length(node_ids) >= 2)
    stopifnot(merge_method %in% c('union', 'intersection'))
    stopifnot(integration_friction >= 0 && integration_friction <= 1)

    mat <- self$bipartite_matrix
    old_M <- nrow(mat)
    old_N <- ncol(mat)

    if (node_type == 'actor') {
      stopifnot(all(node_ids >= 1 & node_ids <= old_M))
      # Extract rows for merging firms
      rows_to_merge <- mat[node_ids, , drop = FALSE]
      if (merge_method == 'union') {
        merged_row <- apply(rows_to_merge, 2, max)
      } else {
        merged_row <- apply(rows_to_merge, 2, min)
      }
      # Apply integration friction (Feldman & Hernandez 2022)
      if (integration_friction > 0) {
        active <- which(merged_row == 1)
        drop_mask <- runif(length(active)) < integration_friction
        merged_row[active[drop_mask]] <- 0
      }
      # Replace first node, remove others
      mat[node_ids[1], ] <- merged_row
      remove_ids <- node_ids[-1]
      mat <- mat[-remove_ids, , drop = FALSE]
      if (verbose) {
        cat(sprintf("Node collapse: merged actors %s into actor %d\n",
                    paste(node_ids, collapse = ","), node_ids[1]))
        cat(sprintf("  M: %d -> %d\n", old_M, nrow(mat)))
        cat(sprintf("  Activities in merged entity: %d\n", sum(merged_row)))
      }
    } else {
      stopifnot(all(node_ids >= 1 & node_ids <= old_N))
      cols_to_merge <- mat[, node_ids, drop = FALSE]
      if (merge_method == 'union') {
        merged_col <- apply(cols_to_merge, 1, max)
      } else {
        merged_col <- apply(cols_to_merge, 1, min)
      }
      if (integration_friction > 0) {
        active <- which(merged_col == 1)
        drop_mask <- runif(length(active)) < integration_friction
        merged_col[active[drop_mask]] <- 0
      }
      mat[, node_ids[1]] <- merged_col
      remove_ids <- node_ids[-1]
      mat <- mat[, -remove_ids, drop = FALSE]
      if (verbose) {
        cat(sprintf("Component collapse: merged components %s into component %d\n",
                    paste(node_ids, collapse = ","), node_ids[1]))
        cat(sprintf("  N: %d -> %d\n", old_N, ncol(mat)))
      }
    }

    # Recalculate all projections
    self$set_system_from_bipartite_matrix(mat)
    self$M <- nrow(mat)
    self$N <- ncol(mat)

    # Log the merge event
    event <- list(
      timestamp = Sys.time(),
      node_type = node_type,
      node_ids = node_ids,
      surviving_id = node_ids[1],
      merge_method = merge_method,
      integration_friction = integration_friction,
      old_dim = c(old_M, old_N),
      new_dim = c(self$M, self$N)
    )
    self$merge_log <- c(self$merge_log, list(event))

    invisible(self)
  },

  # =========================================================================
  # strategic_alliance_formation()
  # Implements alliance formation logic from Lavie (2006), Hernandez &
  # Shaver (2019) network synergy, and Hernandez & Menon (2021) endogenous
  # network reconfiguration. K_CC moderates absorptive capacity.
  # =========================================================================
  strategic_alliance_formation = function(actor_i, actor_j,
                                          alliance_type = 'knowledge_transfer',
                                          transfer_rate = 0.3,
                                          complementarity_threshold = 0.0,
                                          verbose = FALSE) {
    stopifnot(alliance_type %in% c('knowledge_transfer', 'one_way', 'merger'))
    stopifnot(actor_i >= 1 && actor_i <= self$M)
    stopifnot(actor_j >= 1 && actor_j <= self$M)
    stopifnot(actor_i != actor_j)
    stopifnot(transfer_rate >= 0 && transfer_rate <= 1)

    mat <- self$bipartite_matrix
    row_i <- mat[actor_i, ]
    row_j <- mat[actor_j, ]

    # Compute complementarity (1 - Jaccard similarity)
    intersection <- sum(row_i == 1 & row_j == 1)
    union_size <- sum(row_i == 1 | row_j == 1)
    jaccard <- if (union_size > 0) intersection / union_size else 0
    complementarity <- 1 - jaccard

    if (verbose) {
      cat(sprintf("Alliance: actor %d <-> actor %d\n", actor_i, actor_j))
      cat(sprintf("  Jaccard overlap: %.3f, Complementarity: %.3f\n",
                  jaccard, complementarity))
    }

    if (complementarity < complementarity_threshold) {
      if (verbose) cat("  WARNING: Low complementarity - alliance may provide limited value\n")
    }

    if (alliance_type == 'merger') {
      self$merge_nodes(node_type = 'actor', node_ids = c(actor_i, actor_j),
                       merge_method = 'union', verbose = verbose)
      event <- list(
        timestamp = Sys.time(), actor_i = actor_i, actor_j = actor_j,
        alliance_type = 'merger', complementarity = complementarity,
        activities_transferred = NA
      )
      self$alliance_log <- c(self$alliance_log, list(event))
      return(invisible(self))
    }

    # Compute K_CC proxy for each actor (coupling intensity)
    # Higher coupling = harder to absorb new activities
    search_mat <- self$search_matrix
    activities_i <- which(row_i == 1)
    activities_j <- which(row_j == 1)
    coupling_i <- if (length(activities_i) > 1 && !is.null(search_mat)) {
      mean(search_mat[activities_i, activities_i])
    } else 0
    coupling_j <- if (length(activities_j) > 1 && !is.null(search_mat)) {
      mean(search_mat[activities_j, activities_j])
    } else 0

    # Normalize coupling to [0,1] penalty
    max_coupling <- max(c(coupling_i, coupling_j, 1))
    penalty_i <- coupling_i / max_coupling  # higher coupling = more penalty
    penalty_j <- coupling_j / max_coupling

    transferred_to_i <- 0
    transferred_to_j <- 0

    # Transfer from j to i
    unique_j <- which(row_j == 1 & row_i == 0)
    if (length(unique_j) > 0) {
      effective_rate_i <- transfer_rate * (1 - penalty_i)
      adopt_mask <- runif(length(unique_j)) < effective_rate_i
      mat[actor_i, unique_j[adopt_mask]] <- 1
      transferred_to_i <- sum(adopt_mask)
    }

    # Transfer from i to j (if bidirectional)
    if (alliance_type == 'knowledge_transfer') {
      unique_i <- which(row_i == 1 & row_j == 0)
      if (length(unique_i) > 0) {
        effective_rate_j <- transfer_rate * (1 - penalty_j)
        adopt_mask <- runif(length(unique_i)) < effective_rate_j
        mat[actor_j, unique_i[adopt_mask]] <- 1
        transferred_to_j <- sum(adopt_mask)
      }
    }

    if (verbose) {
      cat(sprintf("  Coupling penalty: actor_%d=%.3f, actor_%d=%.3f\n",
                  actor_i, penalty_i, actor_j, penalty_j))
      cat(sprintf("  Activities transferred to actor_%d: %d\n", actor_i, transferred_to_i))
      if (alliance_type == 'knowledge_transfer')
        cat(sprintf("  Activities transferred to actor_%d: %d\n", actor_j, transferred_to_j))
    }

    # Update system
    self$set_system_from_bipartite_matrix(mat)

    # Log alliance
    event <- list(
      timestamp = Sys.time(), actor_i = actor_i, actor_j = actor_j,
      alliance_type = alliance_type, complementarity = complementarity,
      coupling_i = coupling_i, coupling_j = coupling_j,
      transferred_to_i = transferred_to_i,
      transferred_to_j = transferred_to_j
    )
    self$alliance_log <- c(self$alliance_log, list(event))

    invisible(self)
  },

  # =========================================================================
  # did_shock_analysis()
  # Implements causal identification approach from Hernandez, Lee & Shaver
  # (2025). Comprehensive difference-in-differences analysis for network
  # shock experiments with parallel trends testing.
  # =========================================================================
  did_shock_analysis = function(treatment_step = NULL,
                                outcome_var = 'utility',
                                treatment_ids = NULL,
                                control_ids = NULL,
                                covariates = NULL,
                                parallel_trends_test = TRUE,
                                plot = TRUE,
                                verbose = FALSE) {
    stopifnot(!is.null(treatment_step))
    stopifnot(outcome_var %in% c('utility', 'K_AC', 'K_AA', 'K_CC', 'K_CA', 'degree'))

    # Extract outcome data
    if (outcome_var == 'utility') {
      stopifnot(!is.null(self$actor_util_df))
      panel <- self$actor_util_df
      if (!'actor_id' %in% names(panel)) {
        if ('actor' %in% names(panel)) names(panel)[names(panel) == 'actor'] <- 'actor_id'
      }
      if (!'chain_step_id' %in% names(panel)) {
        if ('step' %in% names(panel)) names(panel)[names(panel) == 'step'] <- 'chain_step_id'
      }
      outcome_col <- 'utility'
      if (!outcome_col %in% names(panel)) {
        # Try common alternatives
        candidates <- c('util', 'value', 'fitness', 'actor_utility')
        for (c_name in candidates) {
          if (c_name %in% names(panel)) { outcome_col <- c_name; break }
        }
      }
    } else if (outcome_var == 'degree') {
      # Compute degree from bipartite array
      stopifnot(!is.null(self$bi_env_arr))
      arr <- self$bi_env_arr
      T_steps <- dim(arr)[3]
      rows_list <- lapply(1:T_steps, function(t) {
        degrees <- rowSums(arr[, , t])
        data.frame(actor_id = 1:length(degrees),
                   chain_step_id = t,
                   outcome = degrees)
      })
      panel <- do.call(rbind, rows_list)
      outcome_col <- 'outcome'
    } else {
      # K statistics
      k_df_name <- paste0(outcome_var, '_df')
      k_df <- self[[k_df_name]]
      stopifnot(!is.null(k_df))
      panel <- k_df
      outcome_col <- 'value'
      if (!outcome_col %in% names(panel)) {
        # Try to find the outcome column
        num_cols <- names(panel)[sapply(panel, is.numeric)]
        outcome_col <- num_cols[length(num_cols)]
      }
    }

    # Determine treatment and control groups
    all_actors <- sort(unique(panel$actor_id))
    if (is.null(treatment_ids)) {
      # Default: first half treated, second half control
      n_actors <- length(all_actors)
      treatment_ids <- all_actors[1:floor(n_actors / 2)]
      if (verbose) cat(sprintf("No treatment_ids specified; using first %d actors\n",
                               length(treatment_ids)))
    }
    if (is.null(control_ids)) {
      control_ids <- setdiff(all_actors, treatment_ids)
    }
    stopifnot(length(intersect(treatment_ids, control_ids)) == 0)

    # Build DID panel
    panel$treated <- as.integer(panel$actor_id %in% treatment_ids)
    panel$post <- as.integer(panel$chain_step_id >= treatment_step)
    panel$outcome_val <- panel[[outcome_col]]

    # Filter to treatment + control actors only
    panel <- panel[panel$actor_id %in% c(treatment_ids, control_ids), ]

    # Parallel trends test
    parallel_trends_p <- NA
    if (parallel_trends_test) {
      pre_data <- panel[panel$post == 0, ]
      if (nrow(pre_data) > 10) {
        pt_model <- tryCatch(
          lm(outcome_val ~ chain_step_id * treated, data = pre_data),
          error = function(e) NULL
        )
        if (!is.null(pt_model)) {
          coefs <- summary(pt_model)$coefficients
          interaction_name <- 'chain_step_id:treated'
          if (interaction_name %in% rownames(coefs)) {
            parallel_trends_p <- coefs[interaction_name, 'Pr(>|t|)']
          }
        }
        if (verbose) {
          cat(sprintf("Parallel trends test p-value: %.4f %s\n",
                      ifelse(is.na(parallel_trends_p), -1, parallel_trends_p),
                      ifelse(!is.na(parallel_trends_p) && parallel_trends_p > 0.05,
                             "(PASS)", "(FAIL - trends not parallel)")))
        }
      }
    }

    # DID regression
    formula_str <- 'outcome_val ~ treated + post + treated:post'
    if (!is.null(covariates)) {
      cov_str <- paste(covariates, collapse = ' + ')
      formula_str <- paste(formula_str, '+', cov_str)
    }
    did_model <- lm(stats::as.formula(formula_str), data = panel)
    did_summary <- summary(did_model)

    # Extract DID estimate
    interaction_term <- 'treated:post'
    coefs <- did_summary$coefficients
    did_estimate <- coefs[interaction_term, 'Estimate']
    did_se <- coefs[interaction_term, 'Std. Error']
    did_p <- coefs[interaction_term, 'Pr(>|t|)']
    did_ci <- confint(did_model)[interaction_term, ]

    if (verbose) {
      cat(sprintf("\nDID Estimate: %.4f (SE=%.4f, p=%.4f)\n",
                  did_estimate, did_se, did_p))
      cat(sprintf("95%% CI: [%.4f, %.4f]\n", did_ci[1], did_ci[2]))
    }

    # Build results
    results <- list(
      did_estimate = did_estimate,
      se = did_se,
      p_value = did_p,
      ci_lower = did_ci[1],
      ci_upper = did_ci[2],
      parallel_trends_p = parallel_trends_p,
      treatment_step = treatment_step,
      outcome_var = outcome_var,
      n_treated = length(treatment_ids),
      n_control = length(control_ids),
      treatment_ids = treatment_ids,
      control_ids = control_ids,
      model = did_model,
      model_summary = did_summary,
      panel_data = panel
    )

    # Plot
    if (plot) {
      # Compute group means by time
      group_means <- aggregate(outcome_val ~ chain_step_id + treated,
                               data = panel, FUN = mean)
      group_means$group <- ifelse(group_means$treated == 1, 'Treatment', 'Control')

      p <- ggplot2::ggplot(group_means,
                           ggplot2::aes(x = chain_step_id, y = outcome_val,
                                        color = group, group = group)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 1.5) +
        ggplot2::geom_vline(xintercept = treatment_step,
                            linetype = 'dashed', color = 'gray40') +
        ggplot2::annotate('text', x = treatment_step, y = max(group_means$outcome_val),
                          label = sprintf('DID = %.3f (p=%.3f)', did_estimate, did_p),
                          hjust = -0.1, vjust = 1, size = 3.5) +
        ggplot2::labs(title = sprintf('DID Analysis: %s', outcome_var),
                      subtitle = sprintf('Treatment step: %d | n_treated=%d, n_control=%d',
                                         treatment_step, length(treatment_ids),
                                         length(control_ids)),
                      x = 'Chain Step', y = outcome_var, color = 'Group') +
        ggplot2::theme_bw() +
        ggplot2::scale_color_manual(values = c('Control' = '#4C72B0',
                                               'Treatment' = '#DD8452'))

      results$plot <- p
      if (verbose) print(p)
    }

    self$did_analysis_results <- results
    return(results)
  },

  # =========================================================================
  # Mean-field equilibrium diagnostic (Theorem 4: Brock-Durlauf reduction)
  # =========================================================================
  #
  # Compares the analytical mean-field fixed point m* of the SaoMNK ministep
  # against the empirical population mean of the realized bipartite matrix.
  # Wraps `solve_mean_field()` (R/mean_field_solver.R) and reads theta_inPop
  # from the structure model used in the most recent run.  Discrepancy is the
  # gap |m_emp - m_star_closest| in spin form, with adoption-form analogues
  # provided for comparison to the SaoMNK simulator's native [0,1] DV.
  #
  # @param T Numeric Gibbs temperature (default 1).
  # @param theta_inPop_override Numeric (length 1) or NULL.  If non-NULL,
  #   override the value read from the structure model (useful for what-if
  #   diagnostics).
  # @param verbose Logical.  If TRUE, print a one-line summary.
  # @return A list with components:
  #   m_star_spin, m_emp_spin, discrepancy_spin (in [-1, 1] units)
  #   m_star_adopt, m_emp_adopt, discrepancy_adopt (in [0, 1] units)
  #   theta_inPop, M, beta_eff, above_critical, regime
  diagnose_mean_field_fit = function(T = 1,
                                     theta_inPop_override = NULL,
                                     verbose = FALSE) {

    ## ---- 1. Extract theta_inPop from current structure model -------------
    ## The effects list is needed even when theta_inPop is overridden,
    ## because the density coefficient (the field term) is always read from
    ## the structure model.
    sm_eff <- tryCatch(
      self$config_structure_model$dv_bipartite$effects,
      error = function(e) NULL
    )
    if (is.null(sm_eff) && !is.null(self$structure_model)) {
      sm_eff <- tryCatch(
        self$structure_model$dv_bipartite$effects,
        error = function(e) NULL
      )
    }

    theta_inPop <- 0
    if (!is.null(theta_inPop_override)) {
      stopifnot(is.numeric(theta_inPop_override),
                length(theta_inPop_override) == 1L,
                is.finite(theta_inPop_override))
      theta_inPop <- as.numeric(theta_inPop_override)
    } else {
      if (!is.null(sm_eff)) {
        for (eff in sm_eff) {
          enm <- eff$effect
          if (!is.null(enm) && identical(enm, "inPop")) {
            theta_inPop <- as.numeric(eff$parameter)
            break
          }
        }
      }
    }

    ## The density coefficient is the external field of the process. It was
    ## previously ignored here, so the analytical side assumed h = 0 while the
    ## simulation ran under (in the failing test) h_b = -1.0 -- one of the three
    ## defects behind the 0.38 discrepancy diagnosed on 2026-08-14.
    h_b <- 0
    if (!is.null(sm_eff)) {
      for (eff in sm_eff) {
        enm <- eff$effect
        if (!is.null(enm) && identical(enm, "density")) {
          h_b <- as.numeric(eff$parameter)
          break
        }
      }
    }

    ## ---- 2. Empirical population mean from current bipartite_matrix ------
    bi <- self$bipartite_matrix
    if (is.null(bi) || !is.matrix(bi)) {
      stop("diagnose_mean_field_fit: env$bipartite_matrix is unavailable; ",
           "run a simulation first (e.g., via search_rsiena() / saomnk_run())")
    }
    p_emp        <- mean(bi)               ## fraction of active ties in [0, 1]
    m_emp_spin   <- 2 * p_emp - 1          ## spin-form magnetisation in [-1, 1]

    ## ---- 3. Analytical comparison objects --------------------------------
    ##
    ## TWO analytics, per PROOF_TABLE.md L16, which designates them Option B
    ## and Option C. They are different objects and only one of them is the
    ## law of the simulated process:
    ##
    ##   BINDING (Option B): the fixed point of the map the simulation
    ##   actually obeys. RSiena's inPop evaluation delta is sqrt-form, so the
    ##   equilibrium solves p = sigmoid(beta*(h_b + theta*sqrt(M*p + 1))).
    ##   Identified empirically on 2026-08-14: against exact per-column Gibbs
    ##   laws, the sqrt family matched the simulated stationary state within
    ##   2 SD while linear and squared readings were rejected at |z| > 80.
    ##
    ##   REFERENCE (Option C): the zero-field linear Curie-Weiss roots from
    ##   solve_mean_field(). Valid as a comparison only in L16's linearised
    ##   regime (near p = 1/2, sub-threshold). Reported for orientation, and
    ##   because the bifurcation structure is stated in these terms.
    ##
    ## The previous version compared the simulation against the REFERENCE
    ## object only, with no field term, and selected the nearest root even
    ## when that root was the unstable m = 0 -- which made discrepancy_spin
    ## non-monotone in the actual model error (49/76 seeds at M = 12).
    p_binding <- saomnk_inpop_self_consistency(beta        = 1 / T,
                                               theta_inPop = theta_inPop,
                                               h_b         = h_b,
                                               M           = self$M)
    m_binding_spin          <- 2 * p_binding - 1
    discrepancy_adopt       <- p_emp - p_binding
    discrepancy_spin        <- m_emp_spin - m_binding_spin

    fp <- solve_mean_field(theta_inPop = theta_inPop,
                           M           = self$M,
                           T           = T)

    ## Reference-root selection: STABLE roots only. Above critical the CW map
    ## has roots (-m*, 0, +m*) and m = 0 is unstable; matching it makes the
    ## reported gap shrink exactly when the simulation is furthest from any
    ## attainable equilibrium.
    stable_roots <- if (fp$above_critical && length(fp$m_star) > 1L) {
      fp$m_star[abs(fp$m_star) > 1e-8]
    } else {
      fp$m_star
    }
    closest_idx     <- which.min(abs(stable_roots - m_emp_spin))
    m_star_spin     <- stable_roots[closest_idx]
    discrepancy_bd  <- m_emp_spin - m_star_spin

    p_star_adopt    <- (m_star_spin + 1) / 2

    regime <- if (fp$above_critical) "supercritical" else "subcritical"

    ## L16's applicability flag for the linear-CW reference: the linearisation
    ## is locally accurate near p = 1/2 (Option C regime).
    in_BD_regime <- abs(p_binding - 0.5) < 0.15

    if (verbose) {
      cat(sprintf(
        "[diagnose_mean_field_fit] M=%d, theta_inPop=%.4f, T=%.3f, ",
        self$M, theta_inPop, T))
      cat(sprintf("beta_eff=%.4f (%s)\n", fp$beta_eff, regime))
      cat(sprintf(
        "  m_star = %s\n  m_emp  = %.4f  =>  spin-form gap = %.4f\n",
        paste(sprintf("%.4f", fp$m_star), collapse = ", "),
        m_emp_spin, discrepancy_spin))
    }

    list(
      ## Binding comparison (Option B): the law of the simulated process.
      ## discrepancy_* now measure against THIS object.
      p_binding         = p_binding,
      m_binding_spin    = m_binding_spin,
      m_emp_spin        = m_emp_spin,
      discrepancy_spin  = discrepancy_spin,
      m_emp_adopt       = p_emp,
      discrepancy_adopt = discrepancy_adopt,
      ## Linear-CW reference (Option C), stable-root gap; valid near p = 1/2.
      m_star_spin       = m_star_spin,
      m_star_adopt      = p_star_adopt,
      discrepancy_bd    = discrepancy_bd,
      m_star_all        = fp$m_star,
      in_BD_regime      = in_BD_regime,
      ## Parameters and regime.
      theta_inPop       = theta_inPop,
      h_b               = h_b,
      M                 = self$M,
      T                 = T,
      beta_eff          = fp$beta_eff,
      above_critical    = fp$above_critical,
      regime            = regime
    )
  },

  # =========================================================================
  # Ising hysteresis temperature sweep
  # =========================================================================
  #
  # Forward annealing pass T_min -> T_max (n_steps), reverse pass back, with
  # n_reps_per_step independent simulation replicates per temperature.  At
  # each (T, direction) we record the population mean adoption m (in [0, 1])
  # and the spin-form magnetisation in [-1, 1].  The hysteresis loop area
  # (in spin form) is computed via the trapezoidal rule and used to classify
  # the regime as "reversible" (no path-dependence) or "hysteretic"
  # (strategic irreversibility).
  #
  # Inspired by D:/industrial_policy_AMR/amr_ising_hysteresis.R; ported into
  # the SaoMNK API so that the underlying simulator is the package's own
  # SAOM ministep (and thus inherits all RSiena effects, not just NK + 10
  # hand-coded social terms).
  #
  # @param T_min,T_max Numeric (>0).  Temperature range; T_min < T_max.
  # @param n_steps Integer (>=2).  Number of temperature steps per direction.
  # @param n_reps_per_step Integer (>=1).  Number of independent replicate
  #   simulations averaged at each T.
  # @param structure_model A SaomNkRSienaBiEnv structure_model (output of
  #   `saomnk_model()` or compatible list).  Must include an `inPop` effect
  #   for the hysteresis curve to be meaningful.
  # @param iterations_per_actor Integer (>=1).  Steps per actor per simulation
  #   replicate (default 30).
  # @param hysteresis_threshold Numeric (>=0).  Loop-area threshold for the
  #   reversible/hysteretic classification (default 0.02; in spin*T units).
  # @param run_seed Integer (or NULL).  Base seed; replicate r at temperature
  #   step k uses run_seed + 1000*k + r.
  # @param verbose Logical.
  # @return A list with components:
  #   forward_path, reverse_path: data.frames (T, m_adopt, m_spin, sd_m)
  #   loop_area: trapezoidal hysteresis area in (spin * T) units
  #   regime_classification: "reversible" or "hysteretic"
  #   T_min, T_max, n_steps, n_reps_per_step
  ising_hysteresis_sweep = function(T_min = 0.1,
                                    T_max = 2.0,
                                    n_steps = 10L,
                                    n_reps_per_step = 3L,
                                    structure_model = NULL,
                                    iterations_per_actor = 30L,
                                    hysteresis_threshold = 0.02,
                                    run_seed = 12345L,
                                    verbose = FALSE) {

    ## ---- Validation ------------------------------------------------------
    stopifnot(is.numeric(T_min), length(T_min) == 1L, T_min > 0)
    stopifnot(is.numeric(T_max), length(T_max) == 1L, T_max > T_min)
    stopifnot(is.numeric(n_steps), length(n_steps) == 1L, n_steps >= 2L)
    stopifnot(is.numeric(n_reps_per_step), length(n_reps_per_step) == 1L,
              n_reps_per_step >= 1L)
    stopifnot(is.numeric(iterations_per_actor),
              length(iterations_per_actor) == 1L,
              iterations_per_actor >= 1L)
    stopifnot(is.numeric(hysteresis_threshold),
              length(hysteresis_threshold) == 1L,
              hysteresis_threshold >= 0)

    if (is.null(structure_model)) {
      structure_model <- self$config_structure_model
    }
    if (is.null(structure_model) ||
        is.null(structure_model$dv_bipartite)) {
      stop("ising_hysteresis_sweep: no structure_model supplied and ",
           "self$config_structure_model is empty; provide a structure ",
           "model (e.g., from saomnk_model(popularity = 0.5)).")
    }

    ## ---- Helper: scale all theta parameters by 1/T ------------------------
    ## In the SAOM logit ministep, temperature acts as the inverse-beta on
    ## the linear utility.  We rescale every theta in the structure model by
    ## 1/T to emulate annealing without modifying the simulator core.
    scale_sm_by_inv_T <- function(sm, T_val) {
      effs <- sm$dv_bipartite$effects
      for (i in seq_along(effs)) {
        if (!is.null(effs[[i]]$parameter)) {
          effs[[i]]$parameter <- effs[[i]]$parameter / T_val
        }
      }
      sm$dv_bipartite$effects <- effs
      sm
    }

    ## ---- Helper: run one simulation, return adoption fraction ------------
    run_one_T <- function(T_val, init_matrix, seed) {
      sm_T <- scale_sm_by_inv_T(structure_model, T_val)

      params <- list(
        M         = self$M,
        N         = self$N,
        BI_PROB   = if (!is.null(self$BI_PROB)) self$BI_PROB else 0.3,
        rand_seed = as.integer(seed),
        name      = "_hysteresis_",
        dir_output = tempdir(),
        init_matrix = init_matrix
      )

      env_loc <- tryCatch(
        SaomNkRSienaBiEnv$new(params),
        error = function(e) NULL
      )
      if (is.null(env_loc)) return(list(p = NA_real_, bi = init_matrix))

      ok <- tryCatch({
        env_loc$search_rsiena(
          structure_model      = sm_T,
          iterations_per_actor = as.integer(iterations_per_actor),
          run_seed             = as.integer(seed),
          verbose              = FALSE
        )
        TRUE
      }, error = function(e) {
        if (verbose) {
          message("ising_hysteresis_sweep: replicate failed at T = ",
                  T_val, ": ", conditionMessage(e))
        }
        FALSE
      })

      bi_out <- if (ok) env_loc$bipartite_matrix else init_matrix
      if (!is.matrix(bi_out)) bi_out <- init_matrix
      list(p = mean(bi_out), bi = bi_out)
    }

    ## ---- Forward sweep:  T_min -> T_max ---------------------------------
    T_seq_fwd <- seq(T_min, T_max, length.out = as.integer(n_steps))
    state     <- self$bipartite_matrix
    if (is.null(state) || !is.matrix(state)) {
      state <- self$bipartite_matrix_init
    }
    if (is.null(state) || !is.matrix(state)) {
      state <- matrix(0, nrow = self$M, ncol = self$N)
    }

    fwd <- data.frame(T = T_seq_fwd,
                      m_adopt = NA_real_,
                      sd_m    = NA_real_,
                      m_spin  = NA_real_)

    for (k in seq_along(T_seq_fwd)) {
      reps_p <- numeric(n_reps_per_step)
      last_bi <- state
      for (r in seq_len(n_reps_per_step)) {
        seed_kr <- as.integer(run_seed + 1000L * k + r)
        out <- run_one_T(T_seq_fwd[k], state, seed_kr)
        reps_p[r] <- out$p
        last_bi   <- out$bi
      }
      fwd$m_adopt[k] <- mean(reps_p, na.rm = TRUE)
      fwd$sd_m[k]    <- if (n_reps_per_step > 1L) {
        stats::sd(reps_p, na.rm = TRUE)
      } else 0
      fwd$m_spin[k]  <- 2 * fwd$m_adopt[k] - 1
      ## Carry the last replicate's frozen state forward.
      state <- last_bi
      if (verbose) {
        cat(sprintf("[fwd %2d/%d] T=%.4f m_adopt=%.4f (+-%.4f)\n",
                    k, length(T_seq_fwd), T_seq_fwd[k],
                    fwd$m_adopt[k], fwd$sd_m[k]))
      }
    }

    ## ---- Reverse sweep:  T_max -> T_min, starting from forward-final ----
    T_seq_rev <- rev(T_seq_fwd)
    rev_df <- data.frame(T = T_seq_rev,
                         m_adopt = NA_real_,
                         sd_m    = NA_real_,
                         m_spin  = NA_real_)

    for (k in seq_along(T_seq_rev)) {
      reps_p <- numeric(n_reps_per_step)
      last_bi <- state
      for (r in seq_len(n_reps_per_step)) {
        seed_kr <- as.integer(run_seed + 1000L * (length(T_seq_fwd) + k) + r)
        out <- run_one_T(T_seq_rev[k], state, seed_kr)
        reps_p[r] <- out$p
        last_bi   <- out$bi
      }
      rev_df$m_adopt[k] <- mean(reps_p, na.rm = TRUE)
      rev_df$sd_m[k]    <- if (n_reps_per_step > 1L) {
        stats::sd(reps_p, na.rm = TRUE)
      } else 0
      rev_df$m_spin[k]  <- 2 * rev_df$m_adopt[k] - 1
      state <- last_bi
      if (verbose) {
        cat(sprintf("[rev %2d/%d] T=%.4f m_adopt=%.4f (+-%.4f)\n",
                    k, length(T_seq_rev), T_seq_rev[k],
                    rev_df$m_adopt[k], rev_df$sd_m[k]))
      }
    }

    ## ---- Hysteresis loop area (trapezoidal in (m_spin, T) plane) --------
    ## Sort forward (ascending T) and reverse (interpolate to fwd T grid).
    fwd_sorted <- fwd[order(fwd$T), ]
    rev_sorted <- rev_df[order(rev_df$T), ]
    diff_m <- fwd_sorted$m_spin - rev_sorted$m_spin
    Tgrid  <- fwd_sorted$T

    if (length(Tgrid) >= 2L && all(is.finite(diff_m))) {
      dT <- diff(Tgrid)
      avg_diff <- 0.5 * (head(diff_m, -1) + tail(diff_m, -1))
      loop_area <- sum(abs(avg_diff * dT))
    } else {
      loop_area <- NA_real_
    }

    regime_classification <- if (is.finite(loop_area) &&
                                 loop_area >= hysteresis_threshold) {
      "hysteretic"
    } else {
      "reversible"
    }

    list(
      forward_path          = fwd,
      reverse_path          = rev_df,
      loop_area             = loop_area,
      regime_classification = regime_classification,
      T_min                 = T_min,
      T_max                 = T_max,
      n_steps               = as.integer(n_steps),
      n_reps_per_step       = as.integer(n_reps_per_step),
      hysteresis_threshold  = hysteresis_threshold
    )
  }

  )  ##/end pubic list
  
)  ##/end class


# ##-----------------


