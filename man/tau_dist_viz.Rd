% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tau_dist_viz.R
\name{tau_dist_viz}
\alias{tau_dist_viz}
\title{Visualization of Distributions for Tau (Between-Study Heterogeneity)}
\usage{
tau_dist_viz(
  dist = c("half-normal", "log-normal", "log-t"),
  scale = NULL,
  meanlog = NULL,
  sdlog = NULL,
  degrees_f = NULL,
  fill_color = "#D1A14F"
)
}
\arguments{
\item{dist}{Character string specifying the distribution: "half-normal", "log-normal", or
"log-t".}

\item{scale}{Numeric value for the scale parameter of the half-normal or log-Student's t
distribution (must be positive). Ignored for log-normal.}

\item{meanlog}{Numeric value for the mean on the log scale for log-normal or location (mu)
for log-Student's t (ignored for half-normal).}

\item{sdlog}{Numeric value for the standard deviation on the log scale for log-normal
(must be positive; ignored for half-normal and log-Student's t).}

\item{degrees_f}{Numeric value for the degrees of freedom for log-Student's t (must be positive;
ignored for half-normal and log-normal).}

\item{fill_color}{Character string specifying the fill color for the 95% probability area
(default: "#D1A14F", a light orange).}
}
\value{
A `ggplot2` object displaying the distribution with annotated statistics.
}
\description{
This function generates a plot of a prior distribution for the between-study
heterogeneity parameter in Bayesian random-effects meta-analysis.
Supported distributions include half-normal, log-normal, and log-Student's t,
with user-specified parameters. The plot highlights the 95% probability
density area and marks the 0, median, and 95th percentiles.
}
\details{
The half-normal distribution requires a scale parameter, the log-normal
requires a mean and standard deviation on the log scale, and the log-Student's
t requires a location, scale, and degrees of freedom. All distributions are
constrained to positive values, suitable for tau as a standard deviation.
}
\examples{
# Half-normal distribution
tau_dist_viz(dist = "half-normal", scale = 2)
# Log-normal distribution
tau_dist_viz(dist = "log-normal", meanlog = 0, sdlog = 1)
# Log-Student's t distribution
tau_dist_viz(dist = "log-t", meanlog = 0, scale = 0.5, degrees_f = 5)
}
