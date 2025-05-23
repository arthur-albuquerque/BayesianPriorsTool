% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mu_prior_sd.R
\name{mu_prior_sd}
\alias{mu_prior_sd}
\title{Compute Standard Deviation for a Normal Distribution Prior}
\usage{
mu_prior_sd(
  extreme_value,
  scale = c("absolute", "relative"),
  mean = NULL,
  probability = 0.95
)
}
\arguments{
\item{extreme_value}{Numeric value representing the upper limit of the interval (must be positive if `scale = "relative"`).}

\item{scale}{Character string specifying the scale: `"absolute"` (default) or `"relative"`.}

\item{mean}{Numeric value for the mean of the distribution. Defaults to `0` if `scale = "absolute"`, and `1` if `scale = "relative"`. Must be positive if `scale = "relative"`.}

\item{probability}{Numeric value between 0 and 1 representing the desired probability density between the lower and upper tails (default is 0.95).}
}
\value{
Numeric value of the standard deviation.
}
\description{
Computes the standard deviation of a normal distribution for use as a prior,
adapted from <https://hbiostat.org/r/examples/rmsbgraphicsvignette/graphics>.
}
\details{
This function is designed for Bayesian random-effects meta-analysis models to facilitate prior
elicitation for the overall effect parameter (mu). It calculates the standard deviation based
on a specified upper limit, assuming a symmetric normal distribution around the mean.
Its main use is to create weakly informative priors.

This function accounts for the scale of the distribution. For example, effect sizes such as
odds ratios, risk ratios, and hazard ratios are analyzed on the log scale (termed "relative" scale).
In contrast, effect sizes like risk differences, mean differences, and standardized mean
differences are on the "absolute" scale.
}
\examples{
# Absolute scale with default mean (0)
mu_prior_sd(extreme_value = 10, scale = "absolute")

# Relative scale with default mean (1)
mu_prior_sd(extreme_value = 2, scale = "relative")

# Custom mean and probability
mu_prior_sd(extreme_value = 5, scale = "absolute", mean = 2, probability = 0.9)
}
