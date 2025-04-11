# File: R/tau_prior_plot.R

#' Plot a Weakly Informative Half-Normal Prior for Tau
#'
#' This function generates a plot of a half-normal distribution to be used as a weakly informative
#' prior for the heterogeneity parameter (τ) in Bayesian random-effects meta-analysis. The prior is
#' defined such that 95% of the prior probability for τ lies below a user-specified upper limit,
#' leaving a 5% chance that τ exceeds this limit. The function computes the scale parameter (σ) of
#' the half-normal distribution and visualizes the distribution with key statistics (median, 95th
#' percentile, and 99th percentile) highlighted.
#'
#' @param extreme_value Numeric value representing the upper limit for the heterogeneity parameter τ
#'   (must be positive). This is the value considered "surprisingly large" for τ, corresponding to
#'   the 97.5th percentile of the half-normal prior distribution.
#' @param fill_color Character string specifying the fill color for the area under the curve up to
#'   the 95th percentile (default: "#D1A14F", a light orange).
#' @param text_color Character string specifying the color of the text annotation for the probability
#'   (default: "white").
#' @return A ggplot2 object displaying the half-normal distribution with annotated statistics.
#' @importFrom stats qnorm
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_vline annotate labs scale_x_continuous coord_cartesian theme element_text element_blank
#' @importFrom extraDistr dhnorm phnorm qhnorm
#' @export
#' @examples
#' # Plot a half-normal prior with an upper limit of 5 for τ
#' tau_prior_plot(extreme_value = 5)
#' # Use a different fill color
#' tau_prior_plot(extreme_value = 10, fill_color = "lightblue", text_color = "black")
tau_prior_plot <- function(extreme_value,
                           fill_color = "#D1A14F",
                           text_color = "white") {
  if (!is.numeric(extreme_value)) stop("`extreme_value` must be numeric.")
  if (extreme_value <= 0) stop("`extreme_value` must be positive.")
  if (!is.character(fill_color) || !is.character(text_color)) stop("`fill_color` and `text_color` must be character strings.")

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

  # Suppress no visible binding note
  tau <- density <- NULL  # Dummy assignments for codetools
  ggplot2::ggplot(df, ggplot2::aes(x = tau, y = density)) +
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
      title = df$Distribution[1],
      caption = "X-axis breaks show 0, the median, the 95th and 99th percentiles"
    ) +
    ggplot2::scale_x_continuous(breaks = breaks_x, labels = round(breaks_x, 1)) +
    ggplot2::coord_cartesian(x = c(0, percentile95 * 1.5)) +
    ggplot2::theme(
      legend.position = "none",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = 24),
      plot.caption = ggplot2::element_text(size = 15),
      plot.caption.position = "plot",
      axis.title.y = ggplot2::element_text(size = 20),
      axis.text.x = ggplot2::element_text(size = 20),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 22),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.3),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
}
