# File: R/tau_dist_viz.R

#' Visualization of Distributions for Tau (Between-Study Heterogeneity)
#'
#' This function generates a plot of a prior distribution for the between-study
#' heterogeneity parameter in Bayesian random-effects meta-analysis.
#' Supported distributions include half-normal, log-normal, and log-Student's t,
#' with user-specified parameters. The plot highlights the 95% probability
#' density area and marks the 0, median, and 95th percentiles.
#'
#' The half-normal distribution requires a scale parameter, the log-normal
#' requires a mean and standard deviation on the log scale, and the log-Student's
#' t requires a location, scale, and degrees of freedom. All distributions are
#' constrained to positive values, suitable for tau as a standard deviation.
#'
#' @param dist Character string specifying the distribution: "half-normal", "log-normal", or
#'   "log-t".
#' @param scale Numeric value for the scale parameter of the half-normal or log-Student's t
#'   distribution (must be positive). Ignored for log-normal.
#' @param meanlog Numeric value for the mean on the log scale for log-normal or location (mu)
#'   for log-Student's t (ignored for half-normal).
#' @param sdlog Numeric value for the standard deviation on the log scale for log-normal
#'   (must be positive; ignored for half-normal and log-Student's t).
#' @param degrees_f Numeric value for the degrees of freedom for log-Student's t (must be positive;
#'   ignored for half-normal and log-normal).
#' @param fill_color Character string specifying the fill color for the 95% probability area
#'   (default: "#D1A14F", a light orange).
#' @param text_color Character string specifying the color of the text annotation for the 95%
#'   probability (default: "black").
#' @return A `ggplot2` object displaying the distribution with annotated statistics.
#' @importFrom stats qlnorm qnorm qt dlnorm dt
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_vline annotate labs scale_x_continuous coord_cartesian theme element_text element_blank
#' @importFrom extraDistr dhnorm phnorm qhnorm
#' @export
#' @examples
#' # Half-normal distribution
#' tau_dist_viz(dist = "half-normal", scale = 2)
#' # Log-normal distribution
#' tau_dist_viz(dist = "log-normal", meanlog = 0, sdlog = 1)
#' # Log-Student's t distribution
#' tau_dist_viz(dist = "log-t", meanlog = 0, scale = 0.5, degrees_f = 5)
tau_dist_viz <- function(dist = c("half-normal", "log-normal", "log-t"),
                         scale = NULL,
                         meanlog = NULL,
                         sdlog = NULL,
                         degrees_f = NULL,
                         fill_color = "#D1A14F",
                         text_color = "black") {
  # Match distribution argument
  dist <- match.arg(dist)

  # Input validation
  if (!is.character(fill_color) || !is.character(text_color)) {
    stop("`fill_color` and `text_color` must be character strings.")
  }

  # Distribution-specific validation and density functions
  if (dist == "half-normal") {
    if (is.null(scale)) stop("`scale` is required for half-normal distribution.")
    if (!is.numeric(scale) || scale <= 0) stop("`scale` must be a positive numeric value.")
    dens_func <- function(x) extraDistr::dhnorm(x, sigma = scale)
    prob_func <- function(x) extraDistr::phnorm(x, sigma = scale)
    quant_func <- function(p) extraDistr::qhnorm(p, sigma = scale)
    dist_label <- paste0("Half-Normal(", round(scale, 2), ")")
  } else if (dist == "log-normal") {
    if (is.null(meanlog) || is.null(sdlog)) stop("`meanlog` and `sdlog` are required for log-normal distribution.")
    if (!is.numeric(meanlog)) stop("`meanlog` must be numeric.")
    if (!is.numeric(sdlog) || sdlog <= 0) stop("`sdlog` must be a positive numeric value.")
    dens_func <- function(x) stats::dlnorm(x, meanlog = meanlog, sdlog = sdlog)
    prob_func <- function(x) stats::plnorm(x, meanlog = meanlog, sdlog = sdlog)
    quant_func <- function(p) stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog)
    dist_label <- paste0("Log-Normal(", round(meanlog, 2), ", ", round(sdlog, 2), ")")
  } else if (dist == "log-t") {
    if (is.null(meanlog) || is.null(scale) || is.null(degrees_f)) {
      stop("`meanlog`, `scale`, and `degrees_f` are required for log-Student's t distribution.")
    }
    if (!is.numeric(meanlog)) stop("`meanlog` must be numeric.")
    if (!is.numeric(scale) || scale <= 0) stop("`scale` must be a positive numeric value.")
    if (!is.numeric(degrees_f) || degrees_f <= 0) stop("`degrees_f` must be a positive numeric value.")
    dens_func <- function(x) stats::dt((log(x) - meanlog) / scale, df = degrees_f) / (x * scale)
    prob_func <- function(x) stats::pt((log(x) - meanlog) / scale, df = degrees_f)
    quant_func <- function(p) exp(meanlog + scale * stats::qt(p, df = degrees_f))
    dist_label <- paste0("Log-t(", round(meanlog, 2), ", ", round(scale, 2), ", ", round(degrees_f, 2), ")")
  }

  # Generate x-axis and density
  tau_axis <- seq(0, max(quant_func(0.999), 10), length.out = 10000)
  density_vals <- dens_func(tau_axis)

  # Create data frame
  df <- data.frame(
    tau = tau_axis,
    density = density_vals,
    Distribution = dist_label
  )

  # Calculate percentiles
  percentile95 <- quant_func(0.95)
  median_value <- quant_func(0.5)
  prob <- prob_func(percentile95) * 100
  breaks_x <- c(0, median_value, percentile95)

  # Suppress no visible binding note
  tau <- density <- NULL  # Dummy assignments for codetools

  # Create plot
  ggplot2::ggplot(df, ggplot2::aes(x = tau, y = density)) +
    ggplot2::geom_area(stat = "function", fun = dens_func,
                       fill = fill_color, xlim = c(0, percentile95), alpha = 0.9) +
    ggplot2::geom_line(linewidth = 1.5, color = "black") +
    ggplot2::geom_vline(xintercept = median_value, linetype = 2) +
    ggplot2::labs(
      x = " ",
      y = "Density",
      title = df$Distribution[1],
      caption = "X-axis breaks show 0, the median, and the 95th percentiles.\nThe shaded area represents 95% of the probability density."
    ) +
    ggplot2::scale_x_continuous(breaks = breaks_x, labels = round(breaks_x, 2)) +
    ggplot2::coord_cartesian(x = c(0, percentile95 * 1.5)) +
    ggplot2::theme(
      legend.position = "none",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = 20),
      plot.caption = ggplot2::element_text(size = 13),
      plot.caption.position = "plot",
      axis.title.y = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 22),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.3),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
}
