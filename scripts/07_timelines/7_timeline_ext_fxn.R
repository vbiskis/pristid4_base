# Details ----
#' 7_timeline_ext_fxn.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-6-14
#' Content: all functions for ext tl
#' -----------

# Estimates of extirpation----

## Method 1: Uniform----
calc_ext_sp <- function(t_n, T_obs, n_sightings, alpha = 0.05) {
  # Use exact formulas from Solow (1993a)
  p_value <- (t_n / T_obs)^n_sightings
  is_extinct <- p_value < alpha
  
  # T̂E = (n+1)/n * tn (exact formula from table)
  T_hat <- ((n_sightings + 1) / n_sightings) * t_n
  
  # TuE = tn / α^(1/n) (exact formula from table)
  T_upper <- t_n / (alpha^(1/n_sightings))
  
  return(list(
    p_value = p_value,
    is_extinct = is_extinct,
    extinction_year = T_hat,
    upper_bound_CI = T_upper,
    method = "Uniform"
  ))
}

## Method 2: Truncated Exponential----
calc_ext_ns <- function(sighting_years, T_obs, alpha = 0.05) {
  sighting_years <- sort(sighting_years[!is.na(sighting_years)])
  n <- length(sighting_years)
  
  if(n == 0) return(list(p_value = NA, is_extinct = NA,
                         method = "Truncated Exponential"))
  
  t_n <- max(sighting_years)
  s <- sum(sighting_years)
  
  # F(x) = 1 - Σ(i=1 to [s/x]) (-1)^(i-1) * (n choose i) * (1 - ix/s)^(n-1)
  calculate_F <- function(x) {
    if(s == 0 || x == 0) return(0)
    
    upper_limit <- floor(s/x)
    if(upper_limit < 1) return(0)
    
    # Limit to prevent overflow but be less restrictive
    upper_limit <- min(upper_limit, n, 100)
    
    summation <- 0
    for(i in 1:upper_limit) {
      if(i > n) break
      
      # Calculate (1 - ix/s)
      inner_term <- 1 - (i * x / s)
      if(inner_term <= 0) break  # Can't take negative to a power
      
      # Calculate binomial coefficient
      if(i <= n) {
        binom_coeff <- choose(n, i)
        if(is.infinite(binom_coeff)) break
        
        # Calculate the term: (-1)^(i-1) * (n choose i) * (1 - ix/s)^(n-1)
        sign <- (-1)^(i-1)
        term <- sign * binom_coeff * (inner_term^(n-1))
        
        if(is.finite(term)) {
          summation <- summation + term
        } else {
          break
        }
      }
    }
    
    F_x <- 1 - summation
    return(max(0, min(1, F_x)))  # Ensure between 0 and 1
  }
  
  # Calculate p-value = F(tn) / F(T)
  F_tn <- calculate_F(t_n)
  F_T <- calculate_F(T_obs)
  
  p_value <- if(F_T == 0 || is.na(F_T) || !is.finite(F_T)) NA else F_tn / F_T
  is_extinct <- !is.na(p_value) && p_value < alpha
  
  # Calculate T̂E using the complex formula from table
  T_hat <- NA
  T_hat <- NA
  if(n > 1) {
    # Calculate numerator: Σ(i=0 to [s/tn]) (-1)^i * (n choose i) * (s - i*tn)^(n-1)
    upper_limit <- floor(s/t_n)
    numerator <- 0
    for(i in 0:upper_limit) {
      if(i > n) break
      inner_term <- s - i * t_n
      if(inner_term < 0) break
      
      if(i <= n) {
        binom_coeff <- choose(n, i)
        sign <- (-1)^i
        term <- sign * binom_coeff * (inner_term^(n-1))
        if(is.finite(term)) numerator <- numerator + term
      }
    }
    
    # Calculate denominator: n*(n-1) * Σ(i=0 to [s/tn]-1) (-1)^i * (n-1 choose i) * (s - (i+1)*tn)^(n-2)
    denominator <- 0
    if(upper_limit > 0) {
      for(i in 0:(upper_limit-1)) {
        if(i > (n-1)) break
        inner_term <- s - (i + 1) * t_n
        if(inner_term < 0) break
        
        if(i <= (n-1)) {
          binom_coeff <- choose(n-1, i)
          sign <- (-1)^i
          term <- sign * binom_coeff * (inner_term^(n-2))
          if(is.finite(term)) denominator <- denominator + term
        }
      }
      denominator <- n * (n - 1) * denominator
    }
    
    # T̂E = tn + numerator/denominator
    if(denominator != 0 && is.finite(denominator)) {
      T_hat <- t_n + (numerator / denominator)
    }
  }
  
  return(list(
    p_value = p_value,
    is_extinct = is_extinct,
    extinction_year = T_hat,
    upper_bound_CI = NA,
    F_tn = F_tn,
    F_T = F_T,
    s = s,
    method = "Truncated Exponential"
  ))
}

## Method 3: End Point Estimation----
calc_ext_ep <- function(sighting_years, T_obs, alpha = 0.05) {
  sighting_years <- sort(sighting_years[!is.na(sighting_years)])
  n <- length(sighting_years)
  
  if(n < 2) return(list(p_value = NA, is_extinct = NA, 
                        method = "End Point (insufficient data)"))
  
  t_n <- max(sighting_years)
  t_n_minus_1 <- sighting_years[n-1]
  
  # p = (tn - tn-1) / (T - tn-1)
  p_value <- (t_n - t_n_minus_1) / (T_obs - t_n_minus_1)
  is_extinct <- p_value < alpha
  
  # T̂E = tn + (tn - tn-1)
  T_hat <- t_n + (t_n - t_n_minus_1)
  
  # TuE = tn + (1-α)/α * (tn - tn-1)
  T_upper <- t_n + ((1 - alpha) / alpha) * (t_n - t_n_minus_1)
  
  return(list(
    p_value = p_value,
    is_extinct = is_extinct,
    extinction_year = T_hat,
    upper_bound_CI = T_upper,
    method = "End Point"
  ))
}

## Method 4: Adapted End Point----
calc_ext_aep <- function(sighting_years, T_obs, alpha = 0.05) {
  sighting_years <- sort(sighting_years[!is.na(sighting_years)])
  n <- length(sighting_years)
  
  if(n < 3) return(list(p_value = NA, is_extinct = NA, 
                        method = "Adapted End Point"))
  
  t_n <- max(sighting_years)
  t_n_minus_1 <- sighting_years[n-1]
  
  # γ = Σ(i=2 to n-1) [(ti+1 - ti) - (ti - ti-1)] / (n - 2)
  gamma <- 0
  if(n >= 3) {
    for(i in 2:(n-1)) {
      t_i_plus_1 <- sighting_years[i+1]
      t_i <- sighting_years[i]
      t_i_minus_1 <- sighting_years[i-1]
      gamma <- gamma + (t_i_plus_1 - t_i) - (t_i - t_i_minus_1)
    }
    gamma <- gamma / (n - 2)
  }
  
  # Exact formula from paper
  # p = (tn-1)/(n-1+γ) / [(tn-1)/(n-1+γ) + (T-tn)]
  numerator <- t_n_minus_1 / (n - 1 + gamma)
  denominator <- numerator + (T_obs - t_n)
  
  if(denominator == 0) {
    p_value <- NA
  } else {
    p_value <- numerator / denominator
  }
  
  is_extinct <- !is.na(p_value) && p_value < alpha
  
  # T̂E calculation (not given in table, but following pattern)
  T_upper <- t_n + (t_n_minus_1 / (n - 1 + gamma)) * ((1 - alpha) / alpha)
  
  return(list(
    p_value = p_value,
    is_extinct = is_extinct,
    extinction_year = T_upper,
    upper_bound_CI = T_upper,
    gamma = gamma,
    method = "Adapted End Point"
  ))
}

## Method 5: Weibull----
calc_ext_np <- function(sighting_years, k = NULL, T_obs, alpha = 0.05) {
  sighting_years <- sort(sighting_years[!is.na(sighting_years)])
  n <- length(sighting_years)
  
  if(n < 3) return(list(p_value = NA, is_extinct = NA, 
                        method = "Weibull (insufficient data)"))
  
  if(is.null(k)) k <- n  # Use all sightings by default
  k <- min(k, n)
  if(k < 3) return(list(p_value = NA, is_extinct = NA, 
                        method = "Weibull (k too small)"))
  
  recent_sightings <- tail(sighting_years, k)
  t_n <- max(recent_sightings)
  t_k_plus_1 <- min(recent_sightings)  # tn-k+1
  
  # ν̂ = (1/(k-1)) * Σ(i=1 to k-2) log[(tn - tn-k+1)/(tn - tn-i+1)]
  if(k <= 2) {
    nu_hat <- 1
  } else {
    log_sum <- 0
    valid_terms <- 0
    for(i in 1:(k-2)) {
      t_i_plus_1 <- recent_sightings[k-i+1]  # tn-i+1
      denominator <- t_n - t_i_plus_1
      numerator <- t_n - t_k_plus_1
      
      if(denominator > 0 && numerator > 0) {
        log_sum <- log_sum + log(numerator / denominator)
        valid_terms <- valid_terms + 1
      }
    }
    
    if(valid_terms > 0) {
      nu_hat <- log_sum / (k - 1)
      if(nu_hat <= 0 || !is.finite(nu_hat)) nu_hat <- 1
    } else {
      nu_hat <- 1
    }
  }
  
  # p = exp[-k * ((T-tn)/(T-tn-k+1))^(1/ν̂)]
  if((T_obs - t_k_plus_1) > 0 && (T_obs - t_n) >= 0) {
    exponent <- -k * ((T_obs - t_n) / (T_obs - t_k_plus_1))^(1/nu_hat)
    p_value <- exp(exponent)
  } else {
    p_value <- NA
  }
  
  is_extinct <- !is.na(p_value) && p_value < alpha
  
  # c(α) = [(-log(α))/k]^ν̂
  c_alpha <- ((-log(alpha)) / k)^nu_hat
  
  if(c_alpha < 1 && c_alpha > 0) {
    # T̂E = (tn - c(α)*tn-k+1) / (1 - c(α))
    T_upper <- (t_n - c_alpha * t_k_plus_1) / (1 - c_alpha)
  } else {
    T_upper <- NA
  }
  
  return(list(
    p_value = p_value,
    is_extinct = is_extinct,
    extinction_year = T_upper,
    upper_bound_CI = T_upper,
    nu_hat = nu_hat,
    k_used = k,
    c_alpha = c_alpha,
    method = "Weibull"
  ))
}

### Model Additions: Bayesian Uncertainty----
calc_bay_unc <- function(certain, unconfirmed, T_obs, alpha = 0.05) {
  certain <- certain[!is.na(certain)]
  unconfirmed <- unconfirmed[!is.na(unconfirmed)]
  
  nc <- length(certain)
  nu <- length(unconfirmed)
  
  if(nc == 0 && nu == 0) return(list(error = "No sighting data"))
  
  all_years <- sort(c(certain, unconfirmed))
  t_last_certain <- if(nc > 0) max(certain) else min(all_years)
  
  # Grid search over possible extinction times
  possible_TE <- seq(max(t_last_certain, min(all_years)), T_obs + 10, by = 1)
  log_likelihoods <- numeric(length(possible_TE))
  
  for(i in seq_along(possible_TE)) {
    tau_E <- possible_TE[i]
    
    # Count sightings before and after tau_E
    certain_before <- sum(certain <= tau_E)
    certain_after <- sum(certain > tau_E)
    unconfirmed_before <- sum(unconfirmed <= tau_E)
    unconfirmed_after <- sum(unconfirmed > tau_E)
    
    # No certain sightings allowed after extinction
    if(certain_after > 0) {
      log_likelihoods[i] <- -Inf
      next
    }
    
    # Calculate likelihood components (Bayesian Model 2)
    # p(tc|τE) = (nc-1)!/τE^nc
    if(nc > 0) {
      log_p_tc <- log(factorial(nc-1)) - nc * log(tau_E)
    } else {
      log_p_tc <- 0
    }
    
    # p(tu|τE) - simplified version of equation 11
    if(nu > 0) {
      # Quality parameter ω = 0.5 (50% of unconfirmed sightings are valid)
      omega <- 0.5 #can adjust... idk to what?
      time_span <- max(T_obs - min(all_years), 1)
      
      valid_unconfirmed <- unconfirmed_before * omega
      invalid_all <- unconfirmed_after + unconfirmed_before * (1 - omega)
      
      log_p_tu <- valid_unconfirmed * log(max(tau_E, 1)) - valid_unconfirmed * tau_E +
        invalid_all * log(max(time_span, 1)) - invalid_all * time_span
    } else {
      log_p_tu <- 0
    }
    
    log_likelihoods[i] <- log_p_tc + log_p_tu
  }
  
  # Normalize probabilities
  max_ll <- max(log_likelihoods[is.finite(log_likelihoods)])
  log_likelihoods[!is.finite(log_likelihoods)] <- max_ll - 100
  
  likelihoods <- exp(log_likelihoods - max_ll)
  posterior_probs <- likelihoods / sum(likelihoods)
  
  # Results
  best_idx <- which.max(posterior_probs)
  T_hat <- possible_TE[best_idx]
  prob_extinct_now <- sum(posterior_probs[possible_TE <= T_obs])
  
  # Credible interval
  cumsum_probs <- cumsum(posterior_probs)
  lower_idx <- which(cumsum_probs >= alpha/2)[1]
  upper_idx <- which(cumsum_probs >= 1 - alpha/2)[1]
  
  CI_lower <- possible_TE[max(1, lower_idx)]
  CI_upper <- possible_TE[min(length(possible_TE), upper_idx)]
  
  # Bayes factor
  prob_not_extinct <- 1 - prob_extinct_now
  bayes_factor <- if(prob_not_extinct > 0) prob_extinct_now / prob_not_extinct else Inf
  
  return(list(
    bayes_factor = bayes_factor,
    prob_extinct = prob_extinct_now,
    is_extinct = prob_extinct_now > 0.5,
    extinction_year = T_hat,
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    certain_sightings = nc,
    unconfirmed_sightings = nu,
    method = "Bayesian Model 2"
  ))
}

# Lets go all at once!----
run_em_all <- function(sighting_years, confidence = NULL, 
                       T_obs = 2025, alpha = 0.05, k_w = NULL, recenter = TRUE) {
  # Clean data
  valid_idx <- !is.na(sighting_years)
  original_sighting_years <- sighting_years[valid_idx]
  
  # Store original values for back-translation
  min_year_original <- min(original_sighting_years)
  
  # RE-CENTER (match Cheale et al. 2024)
  if(recenter && length(original_sighting_years) > 0) {
    min_year <- min(original_sighting_years)
    max_year <- max(original_sighting_years)
    
    # Re-center: earliest sighting becomes 0
    sighting_years <- original_sighting_years - min_year
    
    # T_obs becomes the observation window length
    T_obs_centered <- T_obs - min_year
    
    cat("RE-CENTERING DATA:\n")
    cat("Original range:", min_year, "-", max_year, "\n")
    cat("Centered range: 0 -", max_year - min_year, "\n")
    cat("T_obs changed from", T_obs, "to", T_obs_centered, "\n\n")
    
    # Use centered data for calculations
    T_obs <- T_obs_centered
  } else {
    sighting_years <- original_sighting_years
    min_year_original <- 0  # No translation needed
  }
  
  if(!is.null(confidence)) {
    confidence <- confidence[valid_idx]
    
    # Your confidence scheme:
    # Certain: 1 (photo) + 2 (expert description)
    # Confirmed: 3 (to family level)  
    # Unconfirmed: 0 (not guaranteed sawfish)
    certain_idx <- confidence %in% c(1, 2)
    confirmed_idx <- confidence == 3
    all_confirmed_idx <- confidence %in% c(1, 2, 3)  # All except 0
    
    certain_years <- sighting_years[certain_idx]
    confirmed_years <- sighting_years[confirmed_idx]
    all_confirmed_years <- sighting_years[all_confirmed_idx]
    all_years <- sighting_years
  } else {
    certain_years <- sighting_years
    confirmed_years <- numeric(0)
    all_confirmed_years <- sighting_years
    all_years <- sighting_years
  }
  
  translate_back <- function(result) {
    if(!is.null(result) && recenter) {
      if(!is.null(result$extinction_year) && !is.na(result$extinction_year)) {
        result$extinction_year <- result$extinction_year + min_year_original
      }
      if(!is.null(result$upper_bound_CI) && !is.na(result$upper_bound_CI)) {
        result$upper_bound_CI <- result$upper_bound_CI + min_year_original
      }
      # Add Bayesian CI handling with safety checks
      if(!is.null(result$CI_lower) && !is.na(result$CI_lower)) {
        result$CI_lower <- result$CI_lower + min_year_original
      }
      if(!is.null(result$CI_upper) && !is.na(result$CI_upper)) {
        result$CI_upper <- result$CI_upper + min_year_original
      }
    }
    return(result)
  }
  
  results <- list()
  
  # Analysis 1: Certain sightings only (1+2)
  if(length(certain_years) > 0) {
    results$certain_only <- list()
    t_n_certain <- max(certain_years)
    n_certain <- length(certain_years)
    
    results$certain_only$stationary <- translate_back(calc_ext_sp(t_n_certain, T_obs, n_certain, alpha))
    results$certain_only$declining <- translate_back(calc_ext_ns(certain_years, T_obs, alpha))
    results$certain_only$endpoint <- translate_back(calc_ext_ep(certain_years, T_obs, alpha))
    results$certain_only$adapted_endpoint <- translate_back(calc_ext_aep(certain_years, T_obs, alpha))
    results$certain_only$weibull <- translate_back(calc_ext_np(certain_years, k_w, T_obs, alpha))
  }
  
  # Analysis 2: All confirmed sightings (1+2+3)
  if(length(all_confirmed_years) > 0) {
    results$all_confirmed <- list()
    t_n_confirmed <- max(all_confirmed_years)
    n_confirmed <- length(all_confirmed_years)
    
    results$all_confirmed$stationary <- translate_back(calc_ext_sp(t_n_confirmed, T_obs, n_confirmed, alpha))
    results$all_confirmed$declining <- translate_back(calc_ext_ns(all_confirmed_years, T_obs, alpha))
    results$all_confirmed$endpoint <- translate_back(calc_ext_ep(all_confirmed_years, T_obs, alpha))
    results$all_confirmed$adapted_endpoint <- translate_back(calc_ext_aep(all_confirmed_years, T_obs, alpha))
    results$all_confirmed$weibull <- translate_back(calc_ext_np(all_confirmed_years, k_w, T_obs, alpha))
  }
  
  # Analysis 3: All sightings including unconfirmed (1+2+3+0)
  if(length(all_years) > 0) {
    results$all_sightings <- list()
    t_n_all <- max(all_years)
    n_all <- length(all_years)
    
    results$all_sightings$stationary <- translate_back(calc_ext_sp(t_n_all, T_obs, n_all, alpha))
    results$all_sightings$declining <- translate_back(calc_ext_ns(all_years, T_obs, alpha))
    results$all_sightings$endpoint <- translate_back(calc_ext_ep(all_years, T_obs, alpha))
    results$all_sightings$adapted_endpoint <- translate_back(calc_ext_aep(all_years, T_obs, alpha))
    results$all_sightings$weibull <- translate_back(calc_ext_np(all_years, k_w, T_obs, alpha))
  }
  
  # Analysis 4a: Bayesian Model 2 - Species-level uncertainty (1+2 vs 3)
  if(!is.null(confidence) && length(certain_years) > 0 && length(confirmed_years) > 0) {
    results$bayesian_species <- translate_back(calc_bay_unc(certain_years, confirmed_years, T_obs, alpha))
  }
  
  # Analysis 4b: Bayesian Model 2 - Identification uncertainty (1+2+3 vs 0)  
  if(!is.null(confidence) && length(all_confirmed_years) > 0) {
    unconfirmed_idx <- confidence == 0
    unconfirmed_years <- sighting_years[unconfirmed_idx]
    
    if(length(unconfirmed_years) > 0) {
      results$bayesian_identification <- translate_back(calc_bay_unc(all_confirmed_years, unconfirmed_years, T_obs, alpha))
    }
  }
  
  # Summary
  results$summary <- list(
    certain_sightings = length(certain_years),
    confirmed_sightings = length(confirmed_years),
    all_confirmed_sightings = length(all_confirmed_years),
    unconfirmed_sightings = length(all_years) - length(all_confirmed_years),
    total_sightings = length(all_years),
    last_certain = if(length(certain_years) > 0) max(certain_years) + min_year_original else NA,
    last_confirmed = if(length(confirmed_years) > 0) max(confirmed_years) + min_year_original else NA,
    last_all_confirmed = if(length(all_confirmed_years) > 0) max(all_confirmed_years) + min_year_original else NA,
    last_overall = if(length(all_years) > 0) max(all_years) + min_year_original else NA,
    recentered = recenter,
    T_obs_used = T_obs,
    min_year_original = min_year_original
  )
  return(results)
}

## Call em!----
get_ext_timelines <- function(df, var_name, spec_group, confidence_col = "Spec_Acc", 
                              yr_col_name = "Year", timefrom = 2025, recenter = TRUE) {
  
  year_vect <- df %>% 
    filter(!!sym(var_name) == spec_group) %>% 
    pull(!!sym(yr_col_name))
  
  confidence_vect <- df %>% 
    filter(!!sym(var_name) == spec_group) %>% 
    pull(!!sym(confidence_col))
  
  results <- run_em_all(year_vect, confidence_vect, T_obs = timefrom, recenter = recenter)
  
  cat(paste(rep("=", 50), collapse=""), "\n")
  cat("EXTINCTION ANALYSIS:", spec_group, "\n")
  cat(paste(rep("=", 50), collapse=""), "\n\n")
  
  if(results$summary$recentered) {
    cat("DATA RE-CENTERED (T_obs =", results$summary$T_obs_used, ")\n\n")
  }
  
  cat("SAMPLE SIZES:\n")
  cat("Certain (1+2):", results$summary$certain_sightings, "\n")
  cat("Confirmed (3):", results$summary$confirmed_sightings, "\n") 
  cat("All confirmed (1+2+3):", results$summary$all_confirmed_sightings, "\n")
  cat("Total (1+2+3+0):", results$summary$total_sightings, "\n\n")
  
  analyses <- c("certain_only", "all_confirmed", "all_sightings")
  analysis_names <- c("CERTAIN ONLY (1+2)", "ALL CONFIRMED (1+2+3)", "ALL SIGHTINGS (1+2+3+0)")
  
  for(i in seq_along(analyses)) {
    analysis <- analyses[i]
    if(!is.null(results[[analysis]])) {
      cat(analysis_names[i], "\n")
      
      methods <- c("stationary", "declining", "endpoint", "adapted_endpoint", "weibull")
      method_names <- c("Method 1 (Stationary)", "Method 2 (Truncated Exp)", 
                        "Method 3 (End Point)", "Method 4 (Adapted End Point)", "Method 5 (Weibull)")
      
      for(j in seq_along(methods)) {
        method <- methods[j]
        if(!is.null(results[[analysis]][[method]]) && !is.na(results[[analysis]][[method]]$p_value)) {
          cat(method_names[j], ":\n")
          cat("P-value:", round(results[[analysis]][[method]]$p_value, 6), "\n")
          cat("Extinct:", results[[analysis]][[method]]$is_extinct, "\n")
          if(!is.na(results[[analysis]][[method]]$extinction_year)) {
            cat("Predicted extinction:", round(results[[analysis]][[method]]$extinction_year, 1), "\n")
          }
          if(!is.na(results[[analysis]][[method]]$upper_bound_CI)) {
            cat("Upper confidence bound:", round(results[[analysis]][[method]]$upper_bound_CI, 1), "\n")
          }
          cat("\n")
        }
      }
      cat("\n")
    }
  }
  
  # Bayesian results
  if(!is.null(results$bayesian_species)) {
    cat("BAYESIAN MODEL 2a (Species uncertainty - 1+2 vs 3):\n")
    cat("Probability extinct:", round(results$bayesian_species$prob_extinct, 3), "\n")
    cat("Bayes factor:", round(results$bayesian_species$bayes_factor, 2), "\n")
    cat("Predicted extinction:", round(results$bayesian_species$extinction_year, 1), "\n\n")
  }
  
  if(!is.null(results$bayesian_identification)) {
    cat("BAYESIAN MODEL 2b (ID uncertainty - 1+2+3 vs 0):\n")
    cat("Probability extinct:", round(results$bayesian_identification$prob_extinct, 3), "\n")
    cat("Bayes factor:", round(results$bayesian_identification$bayes_factor, 2), "\n")
    cat("Predicted extinction:", round(results$bayesian_identification$extinction_year, 1), "\n\n")
  }
  
  return(results)
}

## Return----
#we need a helper function here lol
ext_stbl <- function(results_list, group_names) {
  require(dplyr)
  
  summary_rows <- list()
  
  for(i in seq_along(results_list)) {
    result <- results_list[[i]]
    group <- group_names[i]
    
    # Helper function to safely extract values with defaults
    safe_extract <- function(x, default = NA) {
      if(is.null(x) || length(x) == 0) return(default)
      return(x)
    }
    
    # Extract data for certain-only analysis
    if(!is.null(result$certain_only)) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Group = group,
        Analysis = "Certain Only (1+2)",
        Total_Sightings = safe_extract(result$summary$certain_sightings, 0),
        Last_Sighting = safe_extract(result$summary$last_certain, NA),
        
        Method1_P = round(safe_extract(result$certain_only$stationary$p_value, NA), 6),
        Method1_Year = round(safe_extract(result$certain_only$stationary$extinction_year, NA), 1),
        Method1_Up = round(safe_extract(result$certain_only$stationary$upper_bound_CI, NA), 1),
        
        Method2_P = round(safe_extract(result$certain_only$declining$p_value, NA), 6),
        Method2_Year = round(safe_extract(result$certain_only$declining$extinction_year, NA), 1),
        Method2_Up = round(safe_extract(result$certain_only$declining$upper_bound_CI, NA), 1),
        
        Method3_P = round(safe_extract(result$certain_only$endpoint$p_value, NA), 6),
        Method3_Year = round(safe_extract(result$certain_only$endpoint$extinction_year, NA), 1),
        Method3_Up = round(safe_extract(result$certain_only$endpoint$upper_bound_CI, NA), 1),
        
        Method4_P = round(safe_extract(result$certain_only$adapted_endpoint$p_value, NA), 6),
        Method4_Year = round(safe_extract(result$certain_only$adapted_endpoint$extinction_year, NA), 1),
        Method4_Up = round(safe_extract(result$certain_only$adapted_endpoint$upper_bound_CI, NA), 1),
        
        Method5_P = round(safe_extract(result$certain_only$weibull$p_value, NA), 6),
        Method5_Year = round(safe_extract(result$certain_only$weibull$extinction_year, NA), 1),
        Method5_Up = round(safe_extract(result$certain_only$weibull$upper_bound_CI, NA), 1),
        
        stringsAsFactors = FALSE
      )
    }
    
    # Extract data for all confirmed analysis
    if(!is.null(result$all_confirmed)) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Group = group,
        Analysis = "All Confirmed (1+2+3)",
        Total_Sightings = safe_extract(result$summary$all_confirmed_sightings, 0),
        Last_Sighting = safe_extract(result$summary$last_all_confirmed, NA),
        
        Method1_P = round(safe_extract(result$all_confirmed$stationary$p_value, NA), 6),
        Method1_Year = round(safe_extract(result$all_confirmed$stationary$extinction_year, NA), 1),
        Method1_Up = round(safe_extract(result$all_confirmed$stationary$upper_bound_CI, NA), 1),
        
        Method2_P = round(safe_extract(result$all_confirmed$declining$p_value, NA), 6),
        Method2_Year = round(safe_extract(result$all_confirmed$declining$extinction_year, NA), 1),
        Method2_Up = round(safe_extract(result$all_confirmed$declining$upper_bound_CI, NA), 1),
        
        Method3_P = round(safe_extract(result$all_confirmed$endpoint$p_value, NA), 6),
        Method3_Year = round(safe_extract(result$all_confirmed$endpoint$extinction_year, NA), 1),
        Method3_Up = round(safe_extract(result$all_confirmed$endpoint$upper_bound_CI, NA), 1),
        
        Method4_P = round(safe_extract(result$all_confirmed$adapted_endpoint$p_value, NA), 6),
        Method4_Year = round(safe_extract(result$all_confirmed$adapted_endpoint$extinction_year, NA), 1),
        Method4_Up = round(safe_extract(result$all_confirmed$adapted_endpoint$upper_bound_CI, NA), 1),
        
        Method5_P = round(safe_extract(result$all_confirmed$weibull$p_value, NA), 6),
        Method5_Year = round(safe_extract(result$all_confirmed$weibull$extinction_year, NA), 1),
        Method5_Up = round(safe_extract(result$all_confirmed$weibull$upper_bound_CI, NA), 1),
        
        stringsAsFactors = FALSE
      )
    }
    
    # Extract data for all sightings analysis
    if(!is.null(result$all_sightings)) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Group = group,
        Analysis = "All Sightings (1+2+3+0)",
        Total_Sightings = safe_extract(result$summary$total_sightings, 0),
        Last_Sighting = safe_extract(result$summary$last_overall, NA),
        
        Method1_P = round(safe_extract(result$all_sightings$stationary$p_value, NA), 6),
        Method1_Year = round(safe_extract(result$all_sightings$stationary$extinction_year, NA), 1),
        Method1_Up = round(safe_extract(result$all_sightings$stationary$upper_bound_CI, NA), 1),
        
        Method2_P = round(safe_extract(result$all_sightings$declining$p_value, NA), 6),
        Method2_Year = round(safe_extract(result$all_sightings$declining$extinction_year, NA), 1),
        Method2_Up = round(safe_extract(result$all_sightings$declining$upper_bound_CI, NA), 1),
        
        Method3_P = round(safe_extract(result$all_sightings$endpoint$p_value, NA), 6),
        Method3_Year = round(safe_extract(result$all_sightings$endpoint$extinction_year, NA), 1),
        Method3_Up = round(safe_extract(result$all_sightings$endpoint$upper_bound_CI, NA), 1),
        
        Method4_P = round(safe_extract(result$all_sightings$adapted_endpoint$p_value, NA), 6),
        Method4_Year = round(safe_extract(result$all_sightings$adapted_endpoint$extinction_year, NA), 1),
        Method4_Up = round(safe_extract(result$all_sightings$adapted_endpoint$upper_bound_CI, NA), 1),
        
        Method5_P = round(safe_extract(result$all_sightings$weibull$p_value, NA), 6),
        Method5_Year = round(safe_extract(result$all_sightings$weibull$extinction_year, NA), 1),
        Method5_Up = round(safe_extract(result$all_sightings$weibull$upper_bound_CI, NA), 1),
        
        stringsAsFactors = FALSE
      )
    }
    
    # Add Bayesian species uncertainty results (1+2 vs 3)
    if(!is.null(result$bayesian_species)) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Group = group,
        Analysis = "Bayesian Species (1+2 vs 3)",
        Total_Sightings = paste0(
          safe_extract(result$summary$certain_sightings, 0), 
          " vs ", 
          safe_extract(result$summary$confirmed_sightings, 0)
        ),
        Last_Sighting = safe_extract(result$summary$last_certain, NA),
        
        Method1_P = round(safe_extract(result$bayesian_species$prob_extinct, NA), 3),
        Method1_Year = round(safe_extract(result$bayesian_species$extinction_year, NA), 1),
        Method1_Up = round(safe_extract(result$bayesian_species$CI_upper, NA), 1),
        
        Method2_P = round(safe_extract(result$bayesian_species$bayes_factor, NA), 2),
        Method2_Year = round(safe_extract(result$bayesian_species$CI_lower, NA), 1),
        Method2_Up = round(safe_extract(result$bayesian_species$CI_upper, NA), 1),
        
        Method3_P = NA,
        Method3_Year = NA,
        Method3_Up = NA,
        
        Method4_P = NA,
        Method4_Year = NA,
        Method4_Up = NA,
        
        Method5_P = NA,
        Method5_Year = NA,
        Method5_Up = NA,
        
        stringsAsFactors = FALSE
      )
    }
    
    # Add Bayesian identification uncertainty results (1+2+3 vs 0)
    if(!is.null(result$bayesian_identification)) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Group = group,
        Analysis = "Bayesian ID (1+2+3 vs 0)",
        Total_Sightings = paste0(
          safe_extract(result$summary$all_confirmed_sightings, 0), 
          " vs ", 
          safe_extract(result$summary$unconfirmed_sightings, 0)
        ),
        Last_Sighting = safe_extract(result$summary$last_overall, NA),
        
        Method1_P = round(safe_extract(result$bayesian_identification$prob_extinct, NA), 3),
        Method1_Year = round(safe_extract(result$bayesian_identification$extinction_year, NA), 1),
        Method1_Up = round(safe_extract(result$bayesian_identification$CI_upper, NA), 1),
        
        Method2_P = round(safe_extract(result$bayesian_identification$bayes_factor, NA), 2),
        Method2_Year = round(safe_extract(result$bayesian_identification$CI_lower, NA), 1),
        Method2_Up = round(safe_extract(result$bayesian_identification$CI_upper, NA), 1),
        
        Method3_P = NA,
        Method3_Year = NA,
        Method3_Up = NA,
        
        Method4_P = NA,
        Method4_Year = NA,
        Method4_Up = NA,
        
        Method5_P = NA,
        Method5_Year = NA,
        Method5_Up = NA,
        
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all rows
  summary_table <- do.call(rbind, summary_rows)
  
  # Add row names
  rownames(summary_table) <- NULL
  
  # Add column explanations
  attr(summary_table, "method_names") <- c(
    "Method1" = "Uniform (Stationary)",
    "Method2" = "Truncated Exponential", 
    "Method3" = "End Point Estimation",
    "Method4" = "Adapted End Point",
    "Method5" = "Weibull Extreme Value"
  )
  
  attr(summary_table, "bayesian_note") <- "For Bayesian analyses: Method1=P(extinct), Method2=Bayes Factor, Method3=CI bounds"
  
  return(summary_table)
}

## And plot----
plot_timeline <- function(results_list, group_names, title,
                          methods = c("stationary", "declining", "weibull"),
                          analysis = "all_confirmed", current_year = 2025,
                          include_bayesian = TRUE) {
  
  # Extract all data
  plot_data <- data.frame()
  
  for(i in seq_along(results_list)) {
    result <- results_list[[i]]
    group <- group_names[i]
    last_sighting <- result$summary$last_all_confirmed
    
    # Regular methods
    for(method in methods) {
      if(!is.null(result[[analysis]][[method]])) {
        method_result <- result[[analysis]][[method]]
        
        extinction_year <- method_result$extinction_year
        upper_ci <- method_result$upper_bound_CI
        
        if(!is.null(extinction_year) && extinction_year > 2050) { #to remove crazy values
          extinction_year <- NA
          upper_ci <- NA
        }
        
        plot_data <- rbind(plot_data, data.frame(
          Group = group,
          Method = str_to_title(method),
          Method_Type = "Frequentist",
          Last_Sighting = last_sighting,
          Extinction_Year = extinction_year,  
          Upper_CI = upper_ci,  
          P_Value = method_result$p_value,
          Prob_Extinct = NA,
          Bayes_Factor = NA,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Add Bayesian ID uncertainty (1+2+3 vs 0)
    if(include_bayesian && !is.null(result$bayesian_identification)) {
      bay_result <- result$bayesian_identification
      
      plot_data <- rbind(plot_data, data.frame(
        Group = group,
        Method = "Bayesian ID",
        Method_Type = "Bayesian",
        Last_Sighting = result$summary$last_overall,  # Uses all sightings including unconfirmed
        Extinction_Year = bay_result$extinction_year,
        Upper_CI = bay_result$CI_upper,
        P_Value = NA,  # Bayesian doesn't have p-values
        Prob_Extinct = bay_result$prob_extinct,
        Bayes_Factor = bay_result$bayes_factor,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Clean data
  plot_data <- plot_data %>%
    mutate(
      Group = factor(Group, levels = rev(group_names)),
      Method = factor(Method, levels = c(str_to_title(methods), "Bayesian ID")),
      
      # Define significance levels for plotting
      Significance = case_when(
        Method_Type == "Frequentist" & P_Value < 0.001 ~ "p < 0.001",
        Method_Type == "Frequentist" & P_Value < 0.01 ~ "p < 0.01", 
        Method_Type == "Frequentist" & P_Value < 0.05 ~ "p < 0.05",
        Method_Type == "Frequentist" & P_Value < 0.1 ~ "p < 0.1",
        Method_Type == "Frequentist" & P_Value >= 0.1 ~ "p ≥ 0.1",
        Method_Type == "Bayesian" ~ "Bayesian",
        TRUE ~ "No Result"
      ),
      
      # Order significance levels
      Significance = factor(Significance, levels = c("p < 0.001", "p < 0.01", "p < 0.05", 
                                                     "p < 0.1", "p ≥ 0.1", "Bayesian", "No Result"))
    )
  
  # Create the plot  
  p <- ggplot(plot_data, aes(y = Group, color = Significance, shape = Method)) +
    
    # CI lines (use position_dodge to separate methods)
    geom_segment(aes(x = Extinction_Year, xend = Upper_CI),
                 size = 2, alpha = 0.6,
                 position = position_dodge(width = 0.5)) +
    
    # Last sightings reference (centered on group)
    geom_point(aes(x = Last_Sighting), 
               shape = "|", size = 8, color = "gray30", alpha = 0.7) +
    
    # Extinction estimates (dodged by method)
    geom_point(aes(x = Extinction_Year), 
               size = 3, alpha = 0.8,
               position = position_dodge(width = 0.5)) +
    
    # Current year line
    geom_vline(xintercept = current_year, linetype = "dashed", color = "grey", size = 1) +
    
    # Scales
    scale_shape_manual(values = c("Stationary" = 16, "Declining" = 17, 
                                  "Weibull" = 15, "Bayesian ID" = 18),
                       name = "Method") +
    
    scale_color_manual(values = c("p < 0.001" ="darkgreen", "p < 0.01" = "#2ca02c", 
                                  "p < 0.05" = "gold", "p < 0.1" =  "#1f77b4",
                                  "p ≥ 0.1" = "#7f7f7f", "Bayesian" = "#9467bd", 
                                  "No Result" = "#d62728"),
                       name = "Significance") +
    
    labs(title = title,
         x = "Year", y = "Region") +
    
    theme_few() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal", 
      panel.grid.minor = element_blank()
    ) +
    
    # Fix y-axis to show region names
    scale_y_discrete() +
    
    guides(
      color = guide_legend(override.aes = list(size = 3), ncol = 4),
      shape = guide_legend(override.aes = list(size = 3), ncol = 2)
    )
  
  return(p)
}
