# BayesianPriorsTool

BayesianPriorsTool is an R package designed to facilitate
prior elicitation and visualization for Bayesian random-effects meta-analysis.

It provides functions to compute and plot prior distributions for the overall
effect parameter (μ) and between-study heterogeneity parameter (τ), supporting
both absolute and relative (log) scales.

The package is ideal for researchers aiming to specify weakly informative priors
and explore their implications.

## Installation

Install the package from GitHub using `pacman`:

```R 
if (!require("pacman")) install.packages("pacman")
pacman::p_install_gh("arthur-albuquerque/BayesianPriorsTool")
```

## Features
- **μ Priors**: Compute and visualize normal priors for the overall effect with
flexible scale options.

- **τ Priors**: Compute and plot weakly informative priors for heterogeneity
using half-normal, log-normal, or log-Student's t distributions.

- **Customizable Plots**: Adjust colors, labels, and scales via `ggplot2`-based
outputs.

- **Dependencies**: Requires `ggplot2`, `ggdist`, `distributional`, `extraDistr`,
`patchwork`, and `stats`.

## Functions

### `mu_prior_sd`

Computes the standard deviation of a normal prior for μ based on an upper limit
and probability, supporting absolute or relative scales.

```R 
mu_prior_sd(extreme_value = 10, scale = "absolute")
mu_prior_sd(extreme_value = 2, scale = "relative", mean = 1, probability = 0.9)
``` 

### `mu_prior_plot`

Plots a normal prior for μ based on an upper limit, with a highlighted quantile
interval, supporting absolute or relative scales.

```R
mu_prior_plot(extreme_value = 5, scale = "absolute", xlabel = "Mean Difference")
mu_prior_plot(extreme_value = 2, scale = "relative", xlabel = "Odds Ratio")
```

### `mu_dist_viz`

Plots a normal prior for μ with user-specified mean and standard deviation,
highlighting a quantile interval (default 95%).

```R
mu_dist_viz(mean = 0, sd = 1, xlabel = "Mean Difference")
mu_dist_viz(mean = 1, sd = 0.5, scale = "relative", xlabel = "Odds Ratio")
```

### `tau_prior_scale` 

Calculates the scale parameter for a half-normal prior for τ, where `extreme_value`
is the 97.5th percentile of a normal distribution with standard deviation equal
to τ's 95th percentile.

```R
tau_prior_scale(extreme_value = 10)
```

### `tau_prior_plot`

Plots a half-normal prior for τ, showing the median, 95th, and 99th percentiles,
where `extreme_value` is the 97.5th percentile of a normal distribution with
standard deviation equal to τ's 95th percentile.

```R
tau_prior_plot(extreme_value = 5, fill_color = "lightblue", text_color = "black")
```

### `tau_prior_plot_full`
Combines two plots: a half-normal prior for τ and a normal distribution with
standard deviation as τ's 95th percentile, where `extreme_value` is the 97.5th
percentile.

```R
tau_prior_plot_full(extreme_value = 10, xlabel = "Effect Size")
```

### `tau_dist_viz`
Plots a prior for τ using half-normal, log-normal, or log-Student's t distributions,
marking 0, median, and 95th percentiles.

```R
tau_dist_viz(dist = "half-normal", scale = 2)
tau_dist_viz(dist = "log-normal", logmean = -1.07, sdlog = 0.87)
tau_dist_viz(dist = "log-t", meanlog = -2.2, scale = 1.155, degrees_f = 5)
```

## Usage Example
```R
library(BayesianPriorsTool)

# Compute and plot a normal prior for mu
sd_mu <- mu_prior_sd(extreme_value = 5, scale = "absolute")
mu_dist_viz(mean = 0, sd = sd_mu, prob = 0.9, xlabel = "Mean Difference", inner_color = "purple")

# Plot a log-Student's t prior for tau
tau_dist_viz(dist = "log-t", meanlog = -2.2, scale = 1.155, degrees_f = 5, fill_color = "blue", text_color = "white" )

# Visualize tau prior and corresponding normal distribution
tau_prior_plot_full(extreme_value = 10, fill_color = "lightgreen", text_color = "black", xlabel = "Effect Size", inner_color = "pink" )
```

## Output
- **μ Plots**: Density curves with shaded quantile areas and customizable labels.

- **τ Plots**: Density curves with shaded 95% areas, median lines, and percentile breaks. 

- **Combined Plot**: `tau_prior_plot_full` stacks a τ prior plot and a related
normal distribution plot.

## Development

This package is under active development. Contributions and feedback are welcome
via GitHub issues or pull requests.

## License

MIT License. See `LICENSE` file for details.

## Contact 

For questions, open an issue on GitHub or contact arthur_albuquerque1@yahoo.com




