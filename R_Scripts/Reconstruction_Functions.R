# Customized Radius and Combination of Edge-Correction and no Edge-Correction for reconstruct_pattern-function.
reconstruct_pattern_custom <- function (pattern, n_random = 1, e_threshold = 0.01, max_runs = 1000, noEdge_runs = 1000,  # noEdge_runs = Number of Runs without Edge Correction (Runs with EC are executed after no EC)
                                        no_change = Inf, annealing = 0.01, n_points = NULL, window = NULL, 
                                        comp_fast = 1000, weights = c(0.5, 0.5), r_length = 250, 
                                        return_input = TRUE, simplify = FALSE, verbose = TRUE, plot = FALSE,
                                        r = 12.5){  # Fixed Radius for pcf() and gest()
  if (n_random < 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }
  if (is.null(n_points)) {
    message("> Using number of points 'pattern'.")
    n_points <- pattern$n
  }
  if (is.null(window)) {
    message("> Using window of 'pattern'.")
    window <- pattern$window
  }
  intensity <- n_points/spatstat.geom::area(window)
  if (n_points > comp_fast) {
    if (verbose) {
      message("> Using fast compuation of summary functions.")
    }
    comp_fast <- TRUE
  }
  else {
    comp_fast <- FALSE
  }
  names_randomization <- paste0("randomized_", seq_len(n_random))
  energy_list <- vector("list", length = n_random)
  iterations_list <- vector("list", length = n_random)
  stop_criterion_list <- as.list(rep("max_runs", times = n_random))
  result_list <- vector("list", length = n_random)
  names(energy_list) <- names_randomization
  names(iterations_list) <- names_randomization
  names(stop_criterion_list) <- names_randomization
  names(result_list) <- names_randomization
  if (sum(weights) > 1 || sum(weights) == 0) {
    stop("The sum of 'weights' must be 0 < sum(weights) <= 1.", 
         call. = FALSE)
  }
  if (spatstat.geom::is.marked(pattern)) {
    pattern <- spatstat.geom::unmark(pattern)
    if (verbose) {
      warning("Unmarked provided input pattern. For marked pattern, see reconstruct_pattern_marks().", 
              call. = FALSE)
    }
  }
  ##############################################################################################################################################
  #### Changed r to be fixed 
  ##############################################################################################################################################
  r <- seq(from = 0, to = r, length.out = r_length)
  # Before:
  # r <- seq(from = 0, to = spatstat.core::rmax.rule(W = window, 
  #          lambda = intensity), length.out = r_length)
  simulated <- spatstat.random::runifpoint(n = n_points, nsim = 1, 
                                           drop = TRUE, win = window, warn = FALSE)
  if (comp_fast) {
    gest_observed <- spatstat.core::Gest(pattern, correction = "none", 
                                         r = r)
    gest_simulated <- spatstat.core::Gest(simulated, correction = "none", 
                                          r = r)
    pcf_observed <- estimate_pcf_fast(pattern, correction = "none", 
                                      method = "c", spar = 0.5, r = r)
    pcf_simulated <- estimate_pcf_fast(simulated, correction = "none", 
                                       method = "c", spar = 0.5, r = r)
  }
  else {
    gest_observed <- spatstat.core::Gest(X = pattern, correction = "han", 
                                         r = r)
    gest_simulated <- spatstat.core::Gest(X = simulated, 
                                          correction = "han", r = r)
    pcf_observed <- spatstat.core::pcf.ppp(X = pattern, correction = "best", 
                                           divisor = "d", r = r)
    pcf_simulated <- spatstat.core::pcf.ppp(X = simulated, 
                                            correction = "best", divisor = "d", r = r)
  }
  energy <- (mean(abs(gest_observed[[3]] - gest_simulated[[3]]), 
                  na.rm = TRUE) * weights[[1]]) + (mean(abs(pcf_observed[[3]] - 
                                                              pcf_simulated[[3]]), na.rm = TRUE) * weights[[2]])
  for (current_pattern in seq_len(n_random)) {
    
    simulated_current <- simulated
    energy_current <- energy
    iterations <- 0
    energy_counter <- 0
    energy_df <- data.frame(i = seq(from = 1, to = max_runs, 
                                    by = 1), energy = NA)
    rp_id <- sample(x = seq_len(simulated_current$n), size = max_runs, 
                    replace = TRUE)
    rp_coords <- spatstat.random::runifpoint(n = max_runs, 
                                             nsim = 1, drop = TRUE, win = simulated_current$window, 
                                             warn = FALSE)
    if (annealing != 0) {
      random_annealing <- stats::runif(n = max_runs, min = 0, 
                                       max = 1)
    }
    else {
      random_annealing <- rep(0, max_runs)
    }
    for (i in seq_len(max_runs)) {
      ##############################################################################################################################################
      #### Make Runs without Edge-Correction before runs with Edge-Correction
      ##############################################################################################################################################
      if(i > noEdge_runs){
        comp_fast <- FALSE
        gest_observed <- spatstat.core::Gest(X = pattern, correction = "han", 
                                             r = r)
        pcf_observed <- spatstat.core::pcf.ppp(X = pattern, correction = "best", 
                                               divisor = "d", r = r)
      }else{
        comp_fast <- TRUE
      }
      relocated <- simulated_current
      rp_id_current <- rp_id[[i]]
      relocated$x[[rp_id_current]] <- rp_coords$x[[i]]
      relocated$y[[rp_id_current]] <- rp_coords$y[[i]]
      if (comp_fast) {
        gest_relocated <- spatstat.core::Gest(relocated, 
                                              correction = "none", r = r)
        pcf_relocated <- estimate_pcf_fast(relocated, 
                                           correction = "none", method = "c", 
                                           spar = 0.5, r = r)
      }
      else {
        gest_relocated <- spatstat.core::Gest(X = relocated, 
                                              correction = "han", r = r)
        pcf_relocated <- spatstat.core::pcf.ppp(X = relocated, 
                                                correction = "best", divisor = "d", 
                                                r = r)
      }
      energy_relocated <- (mean(abs(gest_observed[[3]] - 
                                      gest_relocated[[3]]), na.rm = TRUE) * weights[[1]]) + 
        (mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), 
              na.rm = TRUE) * weights[[2]])
      if (energy_relocated < energy_current || random_annealing[i] < 
          annealing) {
        simulated_current <- relocated
        energy_current <- energy_relocated
        energy_counter <- 0
        if (plot) {
          Sys.sleep(0.01)
          graphics::plot(x = pcf_observed[[1]], y = pcf_observed[[3]], 
                         type = "l", col = "black", xlab = "r", 
                         ylab = "g(r)")
          graphics::abline(h = 1, lty = 2, col = "grey")
          graphics::lines(x = pcf_relocated[[1]], y = pcf_relocated[[3]], 
                          col = "red")
          graphics::legend("topright", legend = c("observed", 
                                                  "reconstructed"), col = c("black", 
                                                                            "red"), lty = 1, inset = 0.025)
        }
      }
      else {
        energy_counter <- energy_counter + 1
      }
      iterations <- iterations + 1
      energy_df[iterations, 2] <- energy_current
      if (verbose) {
        if (!plot) {
          Sys.sleep(0.01)
        }
        message("\r> Progress: n_random: ", current_pattern, 
                "/", n_random, " || max_runs: ", 
                floor(i/max_runs * 100), "%", " || energy = ", 
                round(energy_current, 5), "\t\t", appendLF = FALSE)
      }
      if (energy_current <= e_threshold || energy_counter > 
          no_change) {
        stop_criterion_list[[current_pattern]] <- "e_threshold/no_change"
        break
      }
    }
    if (plot) {
      grDevices::dev.off()
    }
    if (stop_criterion_list[[current_pattern]] == "e_threshold/no_change") {
      energy_df <- energy_df[1:iterations, ]
    }
    energy_list[[current_pattern]] <- energy_df
    iterations_list[[current_pattern]] <- iterations
    result_list[[current_pattern]] <- simulated_current
  }
  reconstruction <- list(randomized = result_list, observed = pattern, 
                         method = "reconstruct_pattern_homo()", energy_df = energy_list, 
                         stop_criterion = stop_criterion_list, iterations = iterations_list)
  class(reconstruction) <- "rd_pat"
  if (!return_input) {
    reconstruction$observed <- "NA"
    if (simplify) {
      if (n_random > 1 && verbose) {
        warning("'simplify = TRUE' not possible for 'n_random > 1'.", 
                call. = FALSE)
      }
      else if (n_random == 1) {
        reconstruction <- reconstruction$randomized[[1]]
      }
    }
  }
  else {
    if (simplify && verbose) {
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", 
              call. = FALSE)
    }
  }
  if (verbose) {
    message("\r")
  }
  return(reconstruction)
}






reconstruct_pattern_rcustom <- function (pattern, n_random = 1, e_threshold = 0.01, max_runs = 1000, 
                                         no_change = Inf, annealing = 0.01, n_points = NULL, window = NULL, 
                                         comp_fast = 1000, weights = c(0.5, 0.5), r_length = 250, 
                                         return_input = TRUE, simplify = FALSE, verbose = TRUE, plot = FALSE,
                                         r = 12.5){
  if (n_random < 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }
  if (is.null(n_points)) {
    message("> Using number of points 'pattern'.")
    n_points <- pattern$n
  }
  if (is.null(window)) {
    message("> Using window of 'pattern'.")
    window <- pattern$window
  }
  intensity <- n_points/spatstat.geom::area(window)
  if (n_points > comp_fast) {
    if (verbose) {
      message("> Using fast compuation of summary functions.")
    }
    comp_fast <- TRUE
  }
  else {
    comp_fast <- FALSE
  }
  names_randomization <- paste0("randomized_", seq_len(n_random))
  energy_list <- vector("list", length = n_random)
  iterations_list <- vector("list", length = n_random)
  stop_criterion_list <- as.list(rep("max_runs", times = n_random))
  result_list <- vector("list", length = n_random)
  names(energy_list) <- names_randomization
  names(iterations_list) <- names_randomization
  names(stop_criterion_list) <- names_randomization
  names(result_list) <- names_randomization
  if (sum(weights) > 1 || sum(weights) == 0) {
    stop("The sum of 'weights' must be 0 < sum(weights) <= 1.", 
         call. = FALSE)
  }
  if (spatstat.geom::is.marked(pattern)) {
    pattern <- spatstat.geom::unmark(pattern)
    if (verbose) {
      warning("Unmarked provided input pattern. For marked pattern, see reconstruct_pattern_marks().", 
              call. = FALSE)
    }
  }
  ##############################################################################################################################################
  #### Changed r to be fixed 
  ##############################################################################################################################################
  r <- seq(from = 0, to = r, length.out = r_length)
  simulated <- spatstat.random::runifpoint(n = n_points, nsim = 1, 
                                           drop = TRUE, win = window, warn = FALSE)
  if (comp_fast) {
    gest_observed <- spatstat.core::Gest(pattern, correction = "none", 
                                         r = r)
    gest_simulated <- spatstat.core::Gest(simulated, correction = "none", 
                                          r = r)
    pcf_observed <- estimate_pcf_fast(pattern, correction = "none", 
                                      method = "c", spar = 0.5, r = r)
    pcf_simulated <- estimate_pcf_fast(simulated, correction = "none", 
                                       method = "c", spar = 0.5, r = r)
  }
  else {
    gest_observed <- spatstat.core::Gest(X = pattern, correction = "han", 
                                         r = r)
    gest_simulated <- spatstat.core::Gest(X = simulated, 
                                          correction = "han", r = r)
    pcf_observed <- spatstat.core::pcf.ppp(X = pattern, correction = "best", 
                                           divisor = "d", r = r)
    pcf_simulated <- spatstat.core::pcf.ppp(X = simulated, 
                                            correction = "best", divisor = "d", r = r)
  }
  energy <- (mean(abs(gest_observed[[3]] - gest_simulated[[3]]), 
                  na.rm = TRUE) * weights[[1]]) + (mean(abs(pcf_observed[[3]] - 
                                                              pcf_simulated[[3]]), na.rm = TRUE) * weights[[2]])
  for (current_pattern in seq_len(n_random)) {
    simulated_current <- simulated
    energy_current <- energy
    iterations <- 0
    energy_counter <- 0
    energy_df <- data.frame(i = seq(from = 1, to = max_runs, 
                                    by = 1), energy = NA)
    rp_id <- sample(x = seq_len(simulated_current$n), size = max_runs, 
                    replace = TRUE)
    rp_coords <- spatstat.random::runifpoint(n = max_runs, 
                                             nsim = 1, drop = TRUE, win = simulated_current$window, 
                                             warn = FALSE)
    if (annealing != 0) {
      random_annealing <- stats::runif(n = max_runs, min = 0, 
                                       max = 1)
    }
    else {
      random_annealing <- rep(0, max_runs)
    }
    for (i in seq_len(max_runs)) {
      relocated <- simulated_current
      rp_id_current <- rp_id[[i]]
      relocated$x[[rp_id_current]] <- rp_coords$x[[i]]
      relocated$y[[rp_id_current]] <- rp_coords$y[[i]]
      if (comp_fast) {
        gest_relocated <- spatstat.core::Gest(relocated, 
                                              correction = "none", r = r)
        pcf_relocated <- estimate_pcf_fast(relocated, 
                                           correction = "none", method = "c", 
                                           spar = 0.5, r = r)
      }
      else {
        gest_relocated <- spatstat.core::Gest(X = relocated, 
                                              correction = "han", r = r)
        pcf_relocated <- spatstat.core::pcf.ppp(X = relocated, 
                                                correction = "best", divisor = "d", 
                                                r = r)
      }
      energy_relocated <- (mean(abs(gest_observed[[3]] - 
                                      gest_relocated[[3]]), na.rm = TRUE) * weights[[1]]) + 
        (mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), 
              na.rm = TRUE) * weights[[2]])
      if (energy_relocated < energy_current || random_annealing[i] < 
          annealing) {
        simulated_current <- relocated
        energy_current <- energy_relocated
        energy_counter <- 0
        if (plot) {
          Sys.sleep(0.01)
          graphics::plot(x = pcf_observed[[1]], y = pcf_observed[[3]], 
                         type = "l", col = "black", xlab = "r", 
                         ylab = "g(r)")
          graphics::abline(h = 1, lty = 2, col = "grey")
          graphics::lines(x = pcf_relocated[[1]], y = pcf_relocated[[3]], 
                          col = "red")
          graphics::legend("topright", legend = c("observed", 
                                                  "reconstructed"), col = c("black", 
                                                                            "red"), lty = 1, inset = 0.025)
        }
      }
      else {
        energy_counter <- energy_counter + 1
      }
      iterations <- iterations + 1
      energy_df[iterations, 2] <- energy_current
      if (verbose) {
        if (!plot) {
          Sys.sleep(0.01)
        }
        message("\r> Progress: n_random: ", current_pattern, 
                "/", n_random, " || max_runs: ", 
                floor(i/max_runs * 100), "%", " || energy = ", 
                round(energy_current, 5), "\t\t", appendLF = FALSE)
      }
      if (energy_current <= e_threshold || energy_counter > 
          no_change) {
        stop_criterion_list[[current_pattern]] <- "e_threshold/no_change"
        break
      }
    }
    if (plot) {
      grDevices::dev.off()
    }
    if (stop_criterion_list[[current_pattern]] == "e_threshold/no_change") {
      energy_df <- energy_df[1:iterations, ]
    }
    energy_list[[current_pattern]] <- energy_df
    iterations_list[[current_pattern]] <- iterations
    result_list[[current_pattern]] <- simulated_current
  }
  reconstruction <- list(randomized = result_list, observed = pattern, 
                         method = "reconstruct_pattern_homo()", energy_df = energy_list, 
                         stop_criterion = stop_criterion_list, iterations = iterations_list)
  class(reconstruction) <- "rd_pat"
  if (!return_input) {
    reconstruction$observed <- "NA"
    if (simplify) {
      if (n_random > 1 && verbose) {
        warning("'simplify = TRUE' not possible for 'n_random > 1'.", 
                call. = FALSE)
      }
      else if (n_random == 1) {
        reconstruction <- reconstruction$randomized[[1]]
      }
    }
  }
  else {
    if (simplify && verbose) {
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", 
              call. = FALSE)
    }
  }
  if (verbose) {
    message("\r")
  }
  return(reconstruction)
}





reconstruct_pattern_marks_custom <- function (pattern, marked_pattern, n_random = 1, e_threshold = 0.01, r = 12.5,
                                              max_runs = 10000, no_change = Inf, annealing = 0.01, r_length = 250, 
                                              return_input = TRUE, simplify = FALSE, verbose = TRUE, plot = FALSE) 
{
  if (!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }
  if (spatstat.geom::is.marked(pattern) || !spatstat.geom::is.marked(marked_pattern)) {
    stop("'pattern' must be unmarked and 'marked_pattern' marked", 
         call. = FALSE)
  }
  if (!inherits(x = marked_pattern$marks, what = "numeric")) {
    stop("marks must be 'numeric'", call. = FALSE)
  }
  names_randomization <- paste0("randomized_", seq_len(n_random))
  energy_list <- vector("list", length = n_random)
  iterations_list <- vector("list", length = n_random)
  stop_criterion <- as.list(rep("max_runs", times = n_random))
  result_list <- vector("list", length = n_random)
  names(energy_list) <- names_randomization
  names(result_list) <- names_randomization
  names(iterations_list) <- names_randomization
  names(stop_criterion) <- names_randomization
  r <- seq(from = 0, to = r, length.out = r_length)
  simulated <- pattern
  spatstat.geom::marks(simulated) <- sample(x = marked_pattern$marks, 
                                            size = simulated$n, replace = TRUE)
  kmmr_observed <- spatstat.core::markcorr(marked_pattern, 
                                           correction = "Ripley", r = r)
  kmmr_simulated <- spatstat.core::markcorr(simulated, correction = "Ripley", 
                                            r = r)
  energy <- mean(abs(kmmr_observed[[3]] - kmmr_simulated[[3]]), 
                 na.rm = TRUE)
  for (current_pattern in seq_len(n_random)) {
    simulated_current <- simulated
    energy_current <- energy
    iterations <- 0
    energy_counter <- 0
    energy_df <- data.frame(i = seq(from = 1, to = max_runs, 
                                    by = 1), energy = NA)
    rp_a <- sample(x = seq_len(simulated_current$n), size = max_runs, 
                   replace = TRUE)
    rp_b <- sample(x = seq_len(simulated_current$n), size = max_runs, 
                   replace = TRUE)
    if (annealing != 0) {
      random_annealing <- stats::runif(n = max_runs, min = 0, 
                                       max = 1)
    }
    else {
      random_annealing <- rep(0, max_runs)
    }
    for (i in seq_len(max_runs)) {
      relocated <- simulated_current
      rp_a_current <- rp_a[[i]]
      rp_b_current <- rp_b[[i]]
      mark_a <- relocated$marks[[rp_a_current]]
      mark_b <- relocated$marks[[rp_b_current]]
      relocated$marks[[rp_a_current]] <- mark_b
      relocated$marks[[rp_b_current]] <- mark_a
      kmmr_relocated <- spatstat.core::markcorr(relocated, 
                                                correction = "Ripley", r = r)
      energy_relocated <- mean(abs(kmmr_observed[[3]] - 
                                     kmmr_relocated[[3]]), na.rm = TRUE)
      if (energy_relocated < energy_current || random_annealing[i] < 
          annealing) {
        simulated_current <- relocated
        energy_current <- energy_relocated
        if (plot) {
          Sys.sleep(0.01)
          graphics::plot(x = kmmr_observed[[1]], y = kmmr_observed[[3]], 
                         type = "l", col = "black", xlab = "r", 
                         ylab = "kmm(r)")
          graphics::abline(h = 1, lty = 2, col = "grey")
          graphics::lines(x = kmmr_relocated[[1]], y = kmmr_relocated[[3]], 
                          col = "red")
          graphics::legend("topright", legend = c("observed", 
                                                  "reconstructed"), col = c("black", 
                                                                            "red"), lty = 1, inset = 0.025)
        }
      }
      else {
        energy_counter <- energy_counter + 1
      }
      iterations <- iterations + 1
      energy_df[iterations, 2] <- energy_current
      if (verbose) {
        if (!plot) {
          Sys.sleep(0.01)
        }
        message("\r> Progress: n_random: ", current_pattern, 
                "/", n_random, " || max_runs: ", 
                floor(i/max_runs * 100), "%", " || energy = ", 
                round(energy_current, 5), "\t\t", appendLF = FALSE)
      }
      if (energy_current <= e_threshold || energy_counter > 
          no_change) {
        stop_criterion[[current_pattern]] <- "e_threshold/no_change"
        break
      }
    }
    if (plot) {
      grDevices::dev.off()
    }
    if (stop_criterion[[current_pattern]] == "e_threshold/no_change") {
      energy_df <- energy_df[1:iterations, ]
    }
    energy_list[[current_pattern]] <- energy_df
    iterations_list[[current_pattern]] <- iterations
    result_list[[current_pattern]] <- simulated_current
  }
  reconstruction <- list(randomized = result_list, observed = marked_pattern, 
                         method = "reconstruct_pattern_marks()", energy_df = energy_list, 
                         stop_criterion = stop_criterion, iterations = iterations_list)
  class(reconstruction) <- "rd_mar"
  if (!return_input) {
    reconstruction$observed <- "NA"
    if (simplify) {
      if (n_random > 1 && verbose) {
        warning("'simplify = TRUE' not possible for 'n_random > 1'.", 
                call. = FALSE)
      }
      else if (n_random == 1) {
        reconstruction <- reconstruction$randomized[[1]]
      }
    }
  }
  else {
    if (simplify && verbose) {
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", 
              call. = FALSE)
    }
  }
  if (verbose) {
    message("\r")
  }
  return(reconstruction)
}




