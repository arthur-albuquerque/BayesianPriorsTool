# File: R/mu_prior_plot.R

#' Plot a Normal Distribution Prior with Quantile Interval
#'
#' Creates a density plot of a normal distribution based on an upper limit, with a highlighted
#' quantile interval (default 95%). Designed for visualizing priors of the overall effect
#' parameter (mu) in Bayesian random-effects meta-analysis models.
#'
#' This function accounts for the scale of the distribution. For example, effect sizes such as
#' odds ratios, risk ratios, and hazard ratios are analyzed on the log scale (termed "relative" scale).
#' In contrast, effect sizes like risk differences, mean differences, and standardized mean
#' differences are on the "absolute" scale.
#'
#' @param extreme_value Numeric value representing the upper limit of the interval (must be positive if `scale = "relative"`).
#' @param scale Character string specifying the scale: `"absolute"` (default) or `"relative"`.
#' @param mean Numeric value for the mean of the distribution. Defaults to `0` if `scale = "absolute"`, and `1` if `scale = "relative"`. Must be positive if `scale = "relative"`.
#' @param prob Numeric value between 0 and 1 specifying the quantile interval probability (default is 0.95).
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
#' mu_prior_plot(extreme_value = 5, scale = "absolute", xlabel = "Mean Difference")
#' # Relative scale plot
#' mu_prior_plot(extreme_value = 2, scale = "relative", xlabel = "Odds Ratio")
#' # Custom mean and prob
#' mu_prior_plot(extreme_value = 10, scale = "absolute", mean = 2, prob = 0.9,
#'               xlabel = "Effect Size")
mu_prior_plot <- function(extreme_value,
                          scale = c("absolute", "relative"),
                          mean = NULL,
                          prob = 0.95,
                          xlabel,
                          inner_color = "orange",
                          tail_color = "gray90") {
  # Validate inputs
  if (!is.numeric(extreme_value)) stop("`extreme_value` must be numeric.")
  if (missing(xlabel)) stop("`xlabel` is required and must be provided.")
  if (!is.character(xlabel)) stop("`xlabel` must be a character string.")
  if (!is.numeric(prob) || prob <= 0 || prob >= 1) stop("`prob` must be a numeric value between 0 and 1.")
  if (!is.character(inner_color) || !is.character(tail_color)) stop("`inner_color` and `tail_color` must be character strings.")
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

  # Calculate SD using mu_prior_sd
  sd <- mu_prior_sd(extreme_value = extreme_value, scale = scale, mean = mean, probability = prob)

  # Calculate z-score for the specified probability
  z_score <- qnorm(1 - (1 - prob) / 2)

  # Define center and breaks based on scale
  if (scale == "relative") {
    center <- log(mean)
    breaks <- c(center, center - z_score * sd, center + z_score * sd)
    label_func <- function(x) round(exp(x), 2)
    xlab <- paste0(xlabel, " (log scale)")
  } else {  # absolute scale
    center <- mean
    breaks <- c(center, center - z_score * sd, center + z_score * sd)
    label_func <- function(x) round(x, 2)
    xlab <- xlabel
  }

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
      plot.title = ggplot2::element_text(size = 20),
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
