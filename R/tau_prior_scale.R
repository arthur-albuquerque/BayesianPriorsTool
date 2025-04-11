# File: R/tau_prior_scale.R

#' Compute Scale Parameter for a Weakly Informative Half-Normal Prior for Tau
#'
#' This function calculates the scale parameter (σ) of a half-normal distribution to be used as a
#' weakly informative prior for the heterogeneity parameter (τ) in Bayesian random-effects meta-analysis.
#' The prior is designed such that the 95th percentile of the half-normal distribution serves as the
#' standard deviation of a normal distribution, and the user-specified `extreme_value` represents the
#' 97.5th percentile of that normal distribution. This approach ensures that the prior for τ allows for
#' plausible heterogeneity while linking to a broader context of effect size variation, making extreme
#' heterogeneity values unlikely.
#'
#' In a meta-analysis, τ represents the standard deviation of true effect sizes across studies. By
#' setting the `extreme_value` as the 97.5th percentile of a normal distribution with standard deviation
#' equal to τ’s 95th percentile, the prior indirectly controls the plausible range of heterogeneity,
#' ensuring that 95% of τ values are below `extreme_value / z_0.975` (where z_0.975 ≈ 1.96 is the
#' standard normal’s 97.5th percentile).
#'
#' @param extreme_value Numeric value (must be positive) representing the 97.5th percentile of a normal
#'   distribution whose standard deviation is the 95th percentile of the half-normal prior for τ. This
#'   value is considered a "surprisingly large" benchmark, indirectly informing the prior’s scale.
#' @return The scale parameter (`scale`) of the half-normal distribution.
#' @importFrom stats qnorm
#' @export
#' @examples
#' # Example: Set an upper limit of 10, implying τ’s 95th percentile is ~5.1
#' tau_prior_scale(extreme_value = 10)
#'
#' # Example: Set an upper limit of 5, implying τ’s 95th percentile is ~2.55
#' tau_prior_scale(extreme_value = 5)
tau_prior_scale <- function(extreme_value) {
  # Input validation: Ensure the extreme_value is numeric and positive
  # τ represents between-study heterogeneity in meta-analysis, which must be non-negative.
  # A non-numeric or non-positive value would be invalid in this context.
  if (!is.numeric(extreme_value)) stop("`extreme_value` must be numeric.")
  if (extreme_value <= 0) stop("`extreme_value` must be positive.")

  # Compute the z-score for the 97.5th percentile of a standard normal distribution
  # The half-normal distribution is defined as τ = |Z|, where Z ~ N(0, σ).
  # The 95th percentile of τ satisfies P(τ ≤ x) = 2 * Φ(x/σ) - 1 = 0.95,
  # leading to Φ(x/σ) = 0.975, so x_0.95 = σ * z_0.975, where z_0.975 ≈ 1.96.
  z <- qnorm(0.975)  # Approximately 1.959964

  # Calculate the scale parameter (σ) of the half-normal distribution
  # The prior is set such that x_0.95 (the 95th percentile of τ) is the standard
  # deviation of a normal distribution whose 97.5th percentile is extreme_value.
  # Mathematically:
  # extreme_value = z_0.975 * x_0.95
  # x_0.95 = σ * z_0.975
  # Thus, extreme_value = z_0.975 * (σ * z_0.975) = σ * z_0.975^2
  # Solving for σ: σ = extreme_value / z_0.975^2
  # This ensures that 95% of τ values are below extreme_value / z_0.975,
  # and extreme_value reflects an extreme value in a related normal distribution.
  scale <- extreme_value / (z^2)

  # Return the scale parameter
  return(scale)
}
