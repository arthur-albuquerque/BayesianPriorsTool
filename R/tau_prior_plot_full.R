# File: R/tau_prior_plot_full.R

#' Plot a Weakly Informative Half-Normal Prior for Tau and Corresponding Normal Distribution
#'
#' This function generates two plots for a Bayesian random-effects meta-analysis:
#' 1. A plot of a half-normal distribution used as a weakly informative prior for the heterogeneity
#'    parameter (tau), which represents the standard deviation of true effect sizes across studies.
#' 2. A plot of a normal distribution whose standard deviation is the 95th percentile of the
#'    half-normal prior, with the user-specified `extreme_value` set as its 97.5th percentile.
#'
#' The half-normal prior is defined such that its 95th percentile is `extreme_value / z_0.975`, where
#' z_0.975 ≈ 1.96 is the standard normal’s 97.5th percentile. This ensures that 95% of the prior
#' probability for tau lies below `extreme_value / z_0.975`, making extreme heterogeneity unlikely while
#' allowing plausible variation. The `extreme_value` represents a "surprisingly large" value in the
#' context of a normal distribution (e.g., effect sizes), where the standard deviation equals tau’s
#' 95th percentile, and `extreme_value` is the 97.5th percentile of that normal distribution. This links
#' the prior for tau to a broader context of effect size variation.
#'
#' The half-normal plot highlights key statistics (median, 95th percentile, and 99th percentile).
#' The normal distribution plot shows the distribution of effect sizes with mean 0 and standard
#' deviation equal to tau’s 95th percentile, with the 95% probability region shaded and `extreme_value`
#' marking the 97.5th percentile.
#'
#' @param extreme_value Numeric value (must be positive) representing the 97.5th percentile of a normal
#'   distribution whose standard deviation is the 95th percentile of the half-normal prior for tau.
#'   This is a "surprisingly large" benchmark that indirectly informs the prior’s scale.
#' @param fill_color Character string specifying the fill color for the area under the half-normal
#'   curve up to its 95th percentile (default: "#D1A14F", a light orange).
#' @param text_color Character string specifying the color of the text annotation for the probability
#'   in the half-normal plot (default: "white").
#' @param xlabel Character string specifying the x-axis label for the normal distribution plot.
#' @param inner_color Character string specifying the fill color for the 95% probability region in
#'   the normal distribution plot (default: "orange").
#' @param tail_color Character string specifying the fill color for the tails outside the 95%
#'   probability region in the normal distribution plot (default: "gray90").
#' @return A patchwork object combining a ggplot2 plot of the half-normal distribution and a ggplot2
#'   plot of the corresponding normal distribution.
#' @importFrom stats qnorm
#' @import patchwork
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_vline annotate labs scale_x_continuous coord_cartesian theme element_text element_blank
#' @importFrom extraDistr dhnorm phnorm qhnorm
#' @importFrom ggdist stat_halfeye
#' @importFrom distributional dist_normal
#' @export
#' @examples
#' # Plot the prior and normal distribution with extreme_value = 10
#' tau_prior_plot_full(extreme_value = 10, xlabel = "Effect Size")
#' # Use different colors
#' tau_prior_plot_full(extreme_value = 5, fill_color = "lightblue", text_color = "black",
#'                     xlabel = "Effect Size", inner_color = "lightgreen", tail_color = "gray70")
tau_prior_plot_full <- function(extreme_value,
                                fill_color = "#D1A14F",
                                text_color = "white",
                                xlabel,
                                inner_color = "orange",
                                tail_color = "gray90") {
  if (!is.numeric(extreme_value)) stop("`extreme_value` must be numeric.")
  if (extreme_value <= 0) stop("`extreme_value` must be positive.")
  if (!is.character(fill_color) || !is.character(text_color) || !is.character(xlabel) ||
      !is.character(inner_color) || !is.character(tail_color)) {
    stop("`fill_color`, `text_color`, `xlabel`, `inner_color`, and `tail_color` must be character strings.")
  }
  if (missing(xlabel)) stop("`xlabel` is required and must be provided.")

  sd_upper <- tau_prior_scale(extreme_value)
  tau_est <- round(sd_upper, 2)
  tau_axis <- seq(0, 1000, length.out = 10000)
  half_normal <- extraDistr::dhnorm(tau_axis, sigma = sd_upper)

  df <- data.frame(
    tau = tau_axis,
    density = half_normal,
    Distribution = paste0("Half-Normal(", tau_est, ")")
  )

  percentile95 <- extraDistr::qhnorm(0.95, sigma = sd_upper)
  median_value <- extraDistr::qhnorm(0.5, sigma = sd_upper)
  percentile99 <- extraDistr::qhnorm(0.99, sigma = sd_upper)
  prob <- extraDistr::phnorm(percentile95, sigma = sd_upper) * 100
  breaks_x <- c(0, median_value, percentile95, percentile99)

  # Suppress no visible binding note for half-normal plot
  tau <- density <- NULL  # Dummy assignments for codetools
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = tau, y = density)) +
    ggplot2::geom_area(stat = "function", fun = extraDistr::dhnorm,
                       args = list(sigma = sd_upper),
                       fill = fill_color, xlim = c(0, percentile95), alpha = 0.9) +
    ggplot2::geom_line(linewidth = 1.5, color = "black") +
    ggplot2::geom_vline(xintercept = median_value, linetype = 2) +
    ggplot2::annotate("text", x = percentile95 / 2, y = max(df$density) / 3,
                      label = paste0(prob, "%"),
                      colour = text_color, size = 9, fontface = "bold") +
    ggplot2::labs(
      x = " ",
      y = "Density",
      title = bquote("Weakly Informative Prior for Between-Study Heterogeneity ("*tau*")"),
      subtitle = df$Distribution[1],
      caption = "X-axis breaks show 0, the median, the 95th and 99th percentiles"
    ) +
    ggplot2::scale_x_continuous(breaks = breaks_x, labels = round(breaks_x, 1)) +
    ggplot2::coord_cartesian(x = c(0, percentile95 * 1.5)) +
    ggplot2::theme(
      legend.position = "none",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = 16),
      plot.subtitle = ggplot2::element_text(size = 14),
      plot.caption = ggplot2::element_text(size = 13),
      plot.caption.position = "plot",
      axis.title.y = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(size = 20),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 21),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.3),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )

  z_score <- qnorm(1 - (1 - 0.95) / 2)
  center <- 0
  breaks <- c(center, center - z_score * percentile95, center + z_score * percentile95)
  label_func <- function(x) round(x, 2)

  # Suppress no visible binding note for normal plot
  x <- NULL  # Dummy assignment for codetools
  df2 <- data.frame(x = center)
  p2 <- ggplot2::ggplot(df2) +
    ggplot2::aes(xdist = distributional::dist_normal(x, percentile95),
                 fill = ggplot2::after_stat(x > center - z_score * percentile95 &
                                              x < center + z_score * percentile95)) +
    ggdist::stat_halfeye(.width = 0.95) +
    ggplot2::scale_fill_manual(values = c(tail_color, inner_color)) +
    ggplot2::annotate("text", x = center, y = 0.5, label = paste0(0.95 * 100, "%"),
                      colour = "black", size = 10) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = label_func) +
    ggplot2::labs(
      x = xlabel,
      title = "Normal Distribution with SD as 95th Percentile of Half-Normal above",
      subtitle = bquote("Normal("*.(round(center, 2))*", "*.(round(percentile95, 2))^2*")")
    ) +
    ggplot2::theme(
      legend.position = "none",
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 15),
      plot.subtitle = ggplot2::element_text(size = 14),
      plot.title.position = "plot",
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 21),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 19),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.3)
    )

  p1 / p2
}
