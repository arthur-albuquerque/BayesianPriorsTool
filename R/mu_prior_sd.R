# File: R/mu_prior_sd.R

#' Compute Standard Deviation for a Normal Distribution Prior
#'
#' Computes the standard deviation of a normal distribution for use as a prior,
#' adapted from <https://hbiostat.org/r/examples/rmsbgraphicsvignette/graphics>.
#'
#' This function is designed for Bayesian random-effects meta-analysis models to facilitate prior
#' elicitation for the overall effect parameter (mu). It calculates the standard deviation based
#' on a specified upper limit, assuming a symmetric normal distribution around the mean.
#' Its main use is to create weakly informative priors.
#'
#' This function accounts for the scale of the distribution. For example, effect sizes such as
#' odds ratios, risk ratios, and hazard ratios are analyzed on the log scale (termed "relative" scale).
#' In contrast, effect sizes like risk differences, mean differences, and standardized mean
#' differences are on the "absolute" scale.
#'
#' @param extreme_value Numeric value representing the upper limit of the interval (must be positive if `scale = "relative"`).
#' @param scale Character string specifying the scale: `"absolute"` (default) or `"relative"`.
#' @param mean Numeric value for the mean of the distribution. Defaults to `0` if `scale = "absolute"`, and `1` if `scale = "relative"`. Must be positive if `scale = "relative"`.
#' @param probability Numeric value between 0 and 1 representing the desired probability density between the lower and upper tails (default is 0.95).
#' @return Numeric value of the standard deviation.
#' @importFrom stats qnorm
#' @export
#' @examples
#' # Absolute scale with default mean (0)
#' mu_prior_sd(extreme_value = 10, scale = "absolute")
#'
#' # Relative scale with default mean (1)
#' mu_prior_sd(extreme_value = 2, scale = "relative")
#'
#' # Custom mean and probability
#' mu_prior_sd(extreme_value = 5, scale = "absolute", mean = 2, probability = 0.9)
mu_prior_sd <- function(extreme_value,
                        scale = c("absolute", "relative"),
                        mean = NULL,
                        probability = 0.95) {
  # Validate inputs
  if (!is.numeric(extreme_value) || !is.numeric(probability)) {
    stop("`extreme_value` and `probability` must be numeric.")
  }
  if (probability <= 0 || probability >= 1) {
    stop("`probability` must be between 0 and 1.")
  }
  scale <- match.arg(scale)

  # Set default mean based on scale if not provided
  if (is.null(mean)) {
    mean <- ifelse(scale == "absolute", 0, 1)
  } else if (!is.numeric(mean)) {
    stop("`mean` must be numeric.")
  }

  # Additional validation for relative scale
  if (scale == "relative" && (mean <= 0 || extreme_value <= 0)) {
    stop("For `scale = 'relative'`, `mean` and `extreme_value` must be positive.")
  }

  # Calculate tail probability
  tail_probability <- (1 - probability) / 2

  # Compute standard deviation
  sd <- switch(scale,
               "absolute" = (extreme_value - mean) / qnorm(1 - tail_probability),
               "relative" = (log(extreme_value) - log(mean)) / qnorm(1 - tail_probability))

  return(sd)
}
