% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_nact.R
\name{plot_nact}
\alias{plot_nact}
\title{Plot scores from neighborhood based contingency tables
based on Stein and Stoop article, 2019 Jan
Neighborhood-Based Contingency Tables Includig Errors Compensation}
\usage{
plot_nact(
  plot_data,
  score_name = "NACT",
  point_size = 1.2,
  extend_y_to_zero = TRUE,
  line_width = 1,
  y_label = "",
  x_label = "",
  flip_axes = FALSE,
  nact_scores = list("all"),
  colour_by = "scale",
  ...
)
}
\arguments{
\item{score_name}{In this context, table name}

\item{nact_scores}{Actual score to plot. Must be included in plot_opts()}
}
\description{
\code{plot_spatial_verif} is used to plot verification scores computed by
functions from the harpSpatial package.
}
