#' @title SaoMNK Monte Carlo Experiment Runner
#' @description R6 class for running batches of SaoMNK simulations,
#'   aggregating results, and analyzing market entry survival curves.
#'
#' @import R6
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon labs scale_y_continuous
#'   scale_color_brewer scale_fill_brewer theme_minimal theme element_blank
#'   element_text
#' @importFrom scales percent_format
#' @importFrom data.table rbindlist data.table
#' @importFrom stats qnorm
#' @importFrom utils txtProgressBar setTxtProgressBar
NULL

# --- Define the Experiments Class ---

#' @export
SaoMNKexperiments <- R6Class("SaoMNKexperiments",
                             public = list(
                               # --- Public Fields ---
                               experiment_name = NULL,
                               n_simulations = NULL,
                               base_environ_params = NULL,
                               base_structure_model = NULL,
                               base_steps_per_actor = NULL,
                               base_theta_shocks = NULL, # Allows for baseline shocks if needed
                               conf_level = 0.95,
                               verbose_run = FALSE,
                               batch_seeds = NULL,
                               target_markets = 10:12, # Define target markets for entry analysis (PARAMETERIZE THIS!)

                               # --- Results Storage ---
                               simulation_results = list(), # Stores minimal data by default
                               aggregated_util_df = NULL,
                               aggregated_entry_df = NULL,
                               aggregated_first_entries = NULL,
                               survival_analysis_data = NULL,
                               market_entry_plot = NULL,

                               # --- Initialization Method ---

                               #' Initialize the Experiment Setup
                               #'
                               #' @param name A name for the experiment.
                               #' @param n The number of Monte Carlo simulations to run.
                               #' @param environ_params Base environmental parameters for SaoMNK.
                               #' @param structure_model Base structural model for SaoMNK search_rsiena.
                               #' @param steps_per_actor Base steps per actor for search_rsiena.
                               #' @param theta_shocks Base theta shocks list (optional).
                               #' @param target_markets Vector of component IDs representing the target market(s).
                               #' @param conf_level Confidence level for plotting CI.
                               #' @param verbose_run Verbosity level during simulation runs.
                               #' @param rand_seed Seed for generating batch seeds (for reproducibility of the batch).
                               initialize = function(name = "SaoMNK_Experiment",
                                                     n = 50,
                                                     environ_params,
                                                     structure_model,
                                                     steps_per_actor,
                                                     theta_shocks = NULL,
                                                     target_markets = 10:12,
                                                     conf_level = 0.95,
                                                     verbose_run = FALSE,
                                                     rand_seed = NULL) {

                                 # Input validation
                                 stopifnot(!is.null(environ_params), is.list(environ_params))
                                 stopifnot(!is.null(structure_model), is.list(structure_model))
                                 stopifnot(!is.null(steps_per_actor), is.numeric(steps_per_actor))
                                 stopifnot(is.numeric(n), n > 0)
                                 stopifnot(is.numeric(conf_level), conf_level > 0, conf_level < 1)
                                 stopifnot(is.vector(target_markets), is.numeric(target_markets))

                                 self$experiment_name <- name
                                 self$n_simulations <- as.integer(n)
                                 self$base_environ_params <- environ_params
                                 self$base_structure_model <- structure_model
                                 self$base_steps_per_actor <- steps_per_actor
                                 self$base_theta_shocks <- theta_shocks
                                 self$target_markets <- target_markets
                                 self$conf_level <- conf_level
                                 self$verbose_run <- verbose_run

                                 # Generate batch seeds for reproducibility of the simulation set
                                 if (!is.null(rand_seed)) set.seed(rand_seed)
                                 self$batch_seeds <- sample(1:9999999, size = self$n_simulations, replace = FALSE)

                                 invisible(self)
                               },

                               # --- Core Methods ---

                               #' Run the Monte Carlo Simulations
                               #'
                               #' Executes 'n' simulations based on the initialized parameters.
                               #'
                               #' @param theta_shocks Optional list to override base_theta_shocks for this specific run set.
                               #' @param store_full_object If TRUE, stores the entire SaoMNK object for each run. FALSE (default) stores minimal data.
                               #' @param SaoMNK_class The R6 class generator for the simulation (e.g., SaomNkRSienaBiEnv). Needed if it's not automatically found.
                               run_simulations = function(theta_shocks = NULL, store_full_object = FALSE, SaoMNK_class = NULL) {

                                 # Determine the SaoMNK class generator to use
                                 if (is.null(SaoMNK_class)) {
                                   # Attempt to find the class generator (assuming it's named SaomNkRSienaBiEnv)
                                   if (exists("SaomNkRSienaBiEnv") && inherits(SaomNkRSienaBiEnv, "R6ClassGenerator")) {
                                     SaoMNK_class <- SaomNkRSienaBiEnv
                                   } else {
                                     stop("SaoMNK class generator 'SaomNkRSienaBiEnv' not found or is not an R6ClassGenerator. Load it or pass it via the SaoMNK_class argument.")
                                   }
                                 } else {
                                   # Validate the passed class generator
                                   if (!inherits(SaoMNK_class, "R6ClassGenerator")) {
                                     stop("The provided SaoMNK_class is not a valid R6ClassGenerator.")
                                   }
                                 }


                                 # Use provided theta_shocks or the base ones
                                 run_theta_shocks <- if (!is.null(theta_shocks)) theta_shocks else self$base_theta_shocks

                                 # --- Simulation Loop ---
                                 temp_results <- list()
                                 if(!self$verbose_run) pb <- utils::txtProgressBar(min = 0, max = self$n_simulations, style = 3) # Progress bar only if not verbose

                                 for (i in 1:self$n_simulations) {
                                   run_seed_i <- self$batch_seeds[i]
                                   if(self$verbose_run) cat(sprintf('\n--- Starting simulation %s/%s, seed=%s ---\n', i, self$n_simulations, run_seed_i))

                                   # Create a *new* SaoMNK instance for each run to ensure independence
                                   env_i <- tryCatch({
                                     # Use the determined class generator
                                     SaoMNK_class$new(self$base_environ_params)
                                   }, error = function(e) {
                                     warning(paste("Failed to initialize SaoMNK object for run", i, "seed", run_seed_i, ":", e$message))
                                     return(NULL) # Skip this run if initialization fails
                                   })

                                   # Proceed only if initialization was successful
                                   if (!is.null(env_i)) {
                                     # Run the search simulation within a tryCatch
                                     sim_success <- tryCatch({
                                       env_i$search_rsiena(
                                         structure_model = self$base_structure_model,
                                         iterations_per_actor = self$base_steps_per_actor,
                                         theta_shocks = run_theta_shocks,
                                         run_seed = run_seed_i,
                                         verbose = self$verbose_run
                                       )
                                       TRUE # Indicate success
                                     }, error = function(e) {
                                       warning(paste("Simulation run", i, "seed", run_seed_i, "failed during search_rsiena:", e$message))
                                       FALSE # Indicate failure
                                     })

                                     # Store results only if simulation succeeded
                                     if (sim_success) {
                                       if (store_full_object) {
                                         temp_results[[as.character(run_seed_i)]] <- env_i
                                       } else {
                                         # Extract only necessary data safely
                                         util_df_i <- tryCatch(env_i$actor_util_df, error = function(e) NULL)
                                         bi_env_arr_i <- tryCatch(env_i$bi_env_arr, error = function(e) NULL)
                                         seed_used <- tryCatch(env_i$rsiena_run_seed, error = function(e) run_seed_i) # Fallback to intended seed

                                         temp_results[[as.character(run_seed_i)]] <- list(
                                           util_df = util_df_i,
                                           bi_env_arr = bi_env_arr_i,
                                           sim_seed = seed_used # Store the actual seed used
                                         )
                                       }
                                     }# end if sim_success
                                   } # end if !is.null(env_i)

                                   if(!self$verbose_run) utils::setTxtProgressBar(pb, i) # Update progress bar
                                 } # End simulation loop
                                 if(!self$verbose_run) close(pb) # Close progress bar

                                 self$simulation_results <- temp_results
                                 cat(sprintf("\nSimulations complete. %d results stored.\n", length(self$simulation_results)))
                                 invisible(self)
                               },

                               #' Process Simulation Results
                               #'
                               #' Aggregates data (utility, market entries) from the stored simulation results.
                               process_results = function() {
                                 if (length(self$simulation_results) == 0) {
                                   stop("No simulation results to process. Run run_simulations() first.")
                                 }

                                 util_list <- list()
                                 entry_list <- list()

                                 # Determine if full objects or minimal data were stored
                                 # Check the class of the first non-null element
                                 first_valid_result <- NULL
                                 for(key in names(self$simulation_results)) {
                                   if(!is.null(self$simulation_results[[key]])) {
                                     first_valid_result <- self$simulation_results[[key]]
                                     break
                                   }
                                 }
                                 if(is.null(first_valid_result)) {
                                   warning("No valid simulation results found to process.")
                                   self$aggregated_util_df <- data.table::data.table()
                                   self$aggregated_entry_df <- data.table::data.table()
                                   self$aggregated_first_entries <- data.table::data.table()
                                   return(invisible(self))
                                 }
                                 is_full_object <- "R6" %in% class(first_valid_result)


                                 cat("Processing results...\n")
                                 pb <- utils::txtProgressBar(min = 0, max = length(self$simulation_results), style = 3)

                                 i <- 0
                                 processed_count <- 0
                                 for (run_seed_key in names(self$simulation_results)) {
                                   i <- i + 1
                                   result_i <- self$simulation_results[[run_seed_key]]

                                   # Skip NULL results (from failed runs)
                                   if (is.null(result_i)) {
                                     utils::setTxtProgressBar(pb, i)
                                     next
                                   }

                                   # Extract data based on storage type
                                   util_df_i <- NULL
                                   arr_i <- NULL
                                   sim_seed_i <- run_seed_key # Default to key

                                   if (is_full_object) {
                                     env_i <- result_i # It's the full object
                                     util_df_i <- tryCatch(env_i$actor_util_df, error = function(e) NULL)
                                     arr_i <- tryCatch(env_i$bi_env_arr, error = function(e) NULL)
                                     sim_seed_i <- tryCatch(env_i$rsiena_run_seed, error = function(e) run_seed_key)
                                   } else {
                                     util_df_i <- result_i$util_df
                                     arr_i <- result_i$bi_env_arr
                                     sim_seed_i <- result_i$sim_seed # Seed stored during run
                                   }

                                   # Add to utility list (if data exists)
                                   if (!is.null(util_df_i) && nrow(util_df_i) > 0) {
                                     util_list[[run_seed_key]] <- util_df_i
                                   }


                                   # Process entry data from the array
                                   if (!is.null(arr_i) && length(dim(arr_i)) == 3) {
                                     indices1 <- tryCatch(which(arr_i == 1, arr.ind = TRUE), error = function(e) matrix(nrow=0, ncol=3)) # Handle potential errors
                                     if (nrow(indices1) > 0) {
                                       colnames(indices1) <- c('from', 'to', 'step')

                                       # Find ties in new markets based on target_markets field
                                       idx_new <- which(indices1[, 'to'] %in% self$target_markets) #

                                       if (length(idx_new) > 0) {
                                         entry_list[[run_seed_key]] <- data.frame(
                                           from = indices1[idx_new, 'from'],
                                           to = indices1[idx_new, 'to'],
                                           chain_step_id = indices1[idx_new, 'step'],
                                           sim_seed = sim_seed_i # Use the seed associated with this run
                                         )
                                       }
                                     }
                                   }
                                   processed_count <- processed_count + 1
                                   utils::setTxtProgressBar(pb, i)
                                 } # End loop through results
                                 close(pb)

                                 # Combine lists into data tables
                                 if (length(util_list) > 0) {
                                   self$aggregated_util_df <- data.table::rbindlist(util_list, idcol = 'run_seed', fill = TRUE)
                                 } else {
                                   self$aggregated_util_df <- data.table::data.table() # Empty data.table
                                   warning("No utility data found in simulation results.")
                                 }

                                 if (length(entry_list) > 0) {
                                   self$aggregated_entry_df <- data.table::rbindlist(entry_list, idcol = 'run_seed', fill = TRUE)

                                   # Calculate first entries after aggregation
                                   if (nrow(self$aggregated_entry_df) > 0) {
                                     self$aggregated_first_entries <- self$aggregated_entry_df %>%
                                       # Ensure correct grouping variable if run_seed is just the list key
                                       # Use sim_seed if it reliably identifies the run
                                       dplyr::group_by(run_seed, from) %>% # Or group_by(sim_seed, from)
                                       dplyr::summarize(
                                         first_entry_step = min(chain_step_id, na.rm = TRUE),
                                         .groups = "drop"
                                       )
                                   } else {
                                     self$aggregated_first_entries <- data.table::data.table() # Empty dt
                                   }

                                 } else {
                                   # Handle case where no entries occurred
                                   self$aggregated_entry_df <- data.table::data.table() # Use data.table for consistency
                                   self$aggregated_first_entries <- data.table::data.table() # Use data.table
                                   warning("No market entries detected in any simulation.")
                                 }

                                 cat(sprintf("Processing complete. Processed %d valid simulation results.\n", processed_count))
                                 invisible(self)
                               },

                               #' Analyze Market Entry Survival
                               #'
                               #' Calculates survival/entry rates over simulation steps. Logic adapted from original function.
                               analyze_market_entry = function() {
                                 if (is.null(self$aggregated_entry_df) || is.null(self$aggregated_first_entries)) {
                                   stop("Aggregated entry data not available. Run process_results() first.")
                                 }
                                 # Check if entry data is empty
                                 if (nrow(self$aggregated_entry_df) == 0 || nrow(self$aggregated_first_entries) == 0) {
                                   warning("No entry data to analyze for market entry survival.")
                                   # Define empty structure for consistency
                                   self$survival_analysis_data <- data.table::data.table(step = integer(), actor = factor(), total_sims = integer(), surviving_sims = integer(), survival_rate = numeric(), entry_rate = numeric(), ci_lower_entry = numeric(), ci_upper_entry = numeric())
                                   return(invisible(self))
                                 }


                                 all_actors <- unique(self$aggregated_entry_df$from)
                                 # Ensure max_step considers all steps, even if no entry happened at the last step
                                 max_step <- max(0, self$aggregated_entry_df$chain_step_id, na.rm = TRUE)
                                 total_sims <- self$n_simulations # Use the total number of runs intended

                                 survival_data_list <- list()
                                 z_value <- stats::qnorm(1 - (1 - self$conf_level) / 2)

                                 cat("Analyzing market entry...\n")
                                 pb <- utils::txtProgressBar(min = 0, max = length(all_actors), style = 3)

                                 i <- 0
                                 for (actor_id in all_actors) {
                                   i <- i + 1
                                   # Get first entry data for this specific actor
                                   actor_first_entries <- subset(self$aggregated_first_entries, from == actor_id)

                                   # Seeds where this specific actor made at least one entry
                                   entered_run_seeds <- unique(actor_first_entries$run_seed) # Or use sim_seed if that's the unique run ID
                                   actor_entry_sim_count <- length(entered_run_seeds) # Number of sims where this actor entered

                                   actor_survival_steps <- list()
                                   for (s in 0:max_step) {
                                     if (s == 0) {
                                       surviving_sims <- total_sims # At step 0, no one has entered yet
                                     } else {
                                       # Count sims where first entry > s
                                       entered_after_s <- sum(actor_first_entries$first_entry_step > s, na.rm = TRUE)

                                       # Count sims where this actor *never* entered
                                       never_entered_count <- total_sims - actor_entry_sim_count

                                       # Sims surviving (not entered the target market yet) = those entering later + those never entering
                                       surviving_sims <- entered_after_s + never_entered_count
                                     }

                                     # Calculate rates
                                     survival_rate <- surviving_sims / total_sims
                                     entry_rate <- 1 - survival_rate

                                     # Calculate standard errors and CIs for entry rate
                                     se_entry <- sqrt(max(0, entry_rate * (1 - entry_rate)) / total_sims) # Ensure non-negative variance
                                     ci_lower_entry <- max(0, entry_rate - z_value * se_entry)
                                     ci_upper_entry <- min(1, entry_rate + z_value * se_entry)

                                     # Store row data
                                     actor_survival_steps[[s + 1]] <- data.frame(
                                       step = s,
                                       actor = actor_id,
                                       total_sims = total_sims,
                                       surviving_sims = surviving_sims,
                                       survival_rate = survival_rate,
                                       entry_rate = entry_rate,
                                       ci_lower_entry = ci_lower_entry,
                                       ci_upper_entry = ci_upper_entry
                                     )
                                   } # End step loop
                                   survival_data_list[[as.character(actor_id)]] <- dplyr::bind_rows(actor_survival_steps)
                                   utils::setTxtProgressBar(pb, i)
                                 } # End actor loop
                                 close(pb)

                                 # Combine results for all actors
                                 self$survival_analysis_data <- data.table::rbindlist(survival_data_list)
                                 if (nrow(self$survival_analysis_data) > 0) {
                                   self$survival_analysis_data$actor <- factor(self$survival_analysis_data$actor) # Ensure factor
                                 }

                                 cat("Analysis complete.\n")
                                 invisible(self)
                               },

                               #' Plot Market Entry Survival Curves
                               #'
                               #' Generates the ggplot object for market entry using aggregated data.
                               plot_market_entry_survival = function() {
                                 if (is.null(self$survival_analysis_data)) {
                                   stop("Survival analysis data not available. Run analyze_market_entry() first.")
                                 }
                                 # Handle case with no data
                                 if (nrow(self$survival_analysis_data) == 0) {
                                   warning("No survival data to plot.")
                                   # Return an empty plot or a message plot
                                   self$market_entry_plot <- ggplot2::ggplot() +
                                     ggplot2::theme_void() +
                                     ggplot2::labs(title = "No Market Entry Data to Plot",
                                          subtitle = paste("Experiment:", self$experiment_name))
                                   return(invisible(self))
                                 }

                                 # --- Plotting Code (adapted from original function) ---
                                 plot <- ggplot2::ggplot(self$survival_analysis_data,
                                                         ggplot2::aes(x = step, y = entry_rate,
                                                                      color = actor, group = actor)) +
                                   ggplot2::geom_step(linewidth = 1) + # Use linewidth
                                   ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry,
                                                                     fill = actor), alpha = 0.2, color = NA) +
                                   ggplot2::labs(
                                     x = "Chain Step ID",
                                     y = "Proportion Entered Market",
                                     title = paste("Cumulative Market Entry Curves by Actor\nExperiment:", self$experiment_name),
                                     subtitle = paste0(self$conf_level * 100, "% CIs across ", self$n_simulations, " simulations. Target Markets: ", paste(self$target_markets, collapse=", "))
                                   ) +
                                   ggplot2::scale_y_continuous(
                                     labels = scales::percent_format(),
                                     limits = c(0, 1),
                                     breaks = seq(0, 1, by = 0.25)
                                   ) +
                                   ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") + # Or another suitable palette
                                   ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +  # Match fill palette
                                   ggplot2::theme_minimal() +
                                   ggplot2::theme(
                                     legend.position = "right",
                                     panel.grid.minor = ggplot2::element_blank(),
                                     plot.title = ggplot2::element_text(face = "bold", size = 14),
                                     plot.subtitle = ggplot2::element_text(size = 10)
                                   )

                                 self$market_entry_plot <- plot
                                 invisible(self)
                               },

                               # --- Getter Methods ---
                               get_aggregated_util_df = function() { return(self$aggregated_util_df) },
                               get_aggregated_entry_df = function() { return(self$aggregated_entry_df) },
                               get_survival_analysis_data = function() { return(self$survival_analysis_data) },
                               get_market_entry_plot = function() {
                                 if(is.null(self$market_entry_plot)) {
                                   warning("Plot not generated yet. Run plot_market_entry_survival() first.")
                                   # Optionally generate it on the fly if data exists
                                   if (!is.null(self$survival_analysis_data) && nrow(self$survival_analysis_data) > 0) {
                                     self$plot_market_entry_survival()
                                   } else {
                                     return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Plot not available"))
                                   }
                                 }
                                 return(self$market_entry_plot)
                               },
                               get_simulation_results = function(run_seed = NULL) {
                                 if (is.null(run_seed)) {
                                   return(self$simulation_results)
                                 } else {
                                   # Ensure the key exists before accessing
                                   key <- as.character(run_seed)
                                   if (key %in% names(self$simulation_results)) {
                                     return(self$simulation_results[[key]])
                                   } else {
                                     warning(paste("No results found for run_seed:", run_seed))
                                     return(NULL)
                                   }
                                 }
                               },
                               get_batch_seeds = function() { return(self$batch_seeds)}

                             ), # End public list

                             private = list(
                               # Add private fields or methods if needed for internal logic encapsulation
                             ) # End private list
) # End R6Class definition
