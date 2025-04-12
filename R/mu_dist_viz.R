# File: R/mu_dist_viz.R

#' Plot a Normal Distribution Prior for Mu with User-Specified Mean and SD
#'
#' Creates a density plot of a normal distribution for the overall effect parameter (mu) in Bayesian
#' random-effects meta-analysis, using a user-specified mean and standard deviation. The plot highlights
#' a quantile interval (default 95%) and supports both absolute and relative (log) scales.
#'
#' This function is useful when users have prior knowledge of the mean and standard deviation for mu
#' and want to visualize the resulting prior distribution. For example, effect sizes like odds ratios
#' or risk ratios are analyzed on the log scale (termed "relative" scale), while mean differences or
#' standardized mean differences are on the "absolute" scale.
#'
#' @param mean Numeric value for the mean of the distribution (must be positive if `scale = "relative"`).
#' @param sd Numeric value for the standard deviation of the distribution (must be positive).
#' @param prob Numeric value between 0 and 1 specifying the quantile interval probability (default is 0.95).
#' @param scale Character string specifying the scale: `"absolute"` (default) or `"relative"`.
#' @param xlabel Character string for the x-axis label; must be provided by the user (e.g., "Odds Ratio").
#' @param inner_color Character string specifying the fill color for the inner quantile interval (default is "orange").
#' @param tail_color Character string specifying the fill color for the tails outside the quantile interval (default is "gray90").
#' @return A `ggplot2` object representing the normal distribution plot.
#' @importFrom stats qnorm
#' @import ggplot2
#' @import ggdist
#' @import distributional
#' @export
#' @examples
#' # Absolute scale plot
#' mu_dist_viz(mean = 0, sd = 1, xlabel = "Mean Difference")
#' # Relative scale plot (log scale)
#' mu_dist_viz(mean = 1, sd = 0.5, scale = "relative", xlabel = "Odds Ratio")
#' # Custom probability
#' mu_dist_viz(mean = 2, sd = 1.5, prob = 0.9, xlabel = "Effect Size")
mu_dist_viz <- function(mean,
                            sd,
                            prob = 0.95,
                            scale = c("absolute", "relative"),
                            xlabel,
                            inner_color = "orange",
                            tail_color = "gray90") {
  # Input validation
  if (!is.numeric(mean)) stop("`mean` must be numeric.")
  if (!is.numeric(sd)) stop("`sd` must be numeric.")
  if (sd <= 0) stop("`sd` must be positive.")
  if (!is.numeric(prob) || prob <= 0 || prob >= 1) stop("`prob` must be a numeric value between 0 and 1.")
  if (missing(xlabel)) stop("`xlabel` is required and must be provided.")
  if (!is.character(xlabel)) stop("`xlabel` must be a character string.")
  if (!is.character(inner_color) || !is.character(tail_color)) stop("`inner_color` and `tail_color` must be character strings.")
  scale <- match.arg(scale)

  # Calculate z-score for quantile interval
  z_score <- qnorm(1 - (1 - prob) / 2)

  # Define center and breaks based on scale
  if (scale == "relative") {
    center <- mean
    breaks <- c(center, center - z_score * sd, center + z_score * sd)
    label_func <- function(x) round(exp(x), 2)  # Back-transform for labels
    xlab <- paste0(xlabel, " (log scale)")
  } else {  # absolute scale
    center <- mean
    breaks <- c(center, center - z_score * sd, center + z_score * sd)
    label_func <- function(x) round(x, 2)
    xlab <- xlabel
  }

  # Suppress no visible binding note for 'x'
  x <- NULL  # Dummy assignment for codetools (consistent with mu_prior_plot)

  # Create plot
  df <- data.frame(x = center)
  p <- ggplot2::ggplot(df) +
    ggplot2::aes(xdist = distributional::dist_normal(x, sd),
                 fill = ggplot2::after_stat(x > center - z_score * sd &
                                              x < center + z_score * sd)) +
    ggdist::stat_halfeye(.width = prob) +
    ggplot2::scale_fill_manual(values = c(tail_color, inner_color)) +
    ggplot2::annotate("text", x = center, y = 0.5, label = paste0(prob * 100, "%"),
                      colour = "black", size = 10) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = label_func) +
    ggplot2::labs(x = xlab,
                  title = paste0("Mean = ", round(center, 2), "\nStandard Deviation = ", round(sd, 2))) +
    ggplot2::theme(
      legend.position = "none",
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 18),
      plot.title.position = "plot",
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 21),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 19),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.3)
    )

  return(p)
}
