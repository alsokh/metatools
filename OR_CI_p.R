calculate_p_from_or_ci <- function(or, lower_ci, upper_ci, conf_level = 0.95) {

  # Input validation
  if (!is.numeric(or) || !is.numeric(lower_ci) || !is.numeric(upper_ci) || !is.numeric(conf_level)) {
    stop("All inputs (or, lower_ci, upper_ci, conf_level) must be numeric.")
  }
  if (or <= 0 || lower_ci <= 0 || upper_ci <= 0) {
    warning("Odds Ratio and CI limits must be positive. Returning NA for p-value.")
    return(list(log_or = NA, se_log_or = NA, z_stat = NA, p_value = NA))
  }
  if (lower_ci >= upper_ci) {
    stop("Lower CI limit must be less than upper CI limit.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("Confidence level must be between 0 and 1 (exclusive).")
  }
  if (or < lower_ci || or > upper_ci) {
    warning("The Odds Ratio is outside the provided confidence interval. Results may be unreliable.")
  }

  # 1. Convert OR and CI to Log Scale
  log_or <- log(or)
  log_lower_ci <- log(lower_ci)
  log_upper_ci <- log(upper_ci)

  # 2. Estimate the Standard Error (SE) of the Log Odds Ratio
  # The Z-score for the given confidence level (e.g., 1.96 for 95% CI)
  # qnorm(1 - (1 - conf_level) / 2) gives the Z-score for a two-tailed test
  z_score_ci <- qnorm(1 - (1 - conf_level) / 2)
  
  se_log_or <- (log_upper_ci - log_lower_ci) / (2 * z_score_ci)

  # 3. Calculate the Z-statistic for the hypothesis that OR = 1 (log(OR) = 0)
  # Z = (log(OR) - log(1)) / SE(log(OR))
  # Since log(1) = 0, this simplifies to:
  z_stat <- log_or / se_log_or

  # 4. Calculate the two-tailed P-value
  # pnorm gives the area to the left of the z-statistic.
  # For a two-tailed test, p_value = 2 * (1 - pnorm(abs(z_stat)))
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  # Alternative calculation for p-value using pnorm's lower.tail argument:
  # p_value <- 2 * pnorm(-abs(z_stat)) 

  return(list(
    log_odds_ratio = log_or,
    std_error_log_or = se_log_or,
    z_statistic = z_stat,
    p_value = p_value
  ))
}
