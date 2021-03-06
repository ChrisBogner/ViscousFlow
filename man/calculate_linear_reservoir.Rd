% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_models.R
\name{calculate_linear_reservoir}
\alias{calculate_linear_reservoir}
\title{Calculate the resending limb of the linear reservoir}
\usage{
calculate_linear_reservoir(drainage_data, q0, t0, lambda)
}
\arguments{
\item{drainage_data}{tibble. First column time, second column flow rate}

\item{q0}{numeric. Flow rate at the end of irrigation}

\item{t0}{numeric. End of irrigation}

\item{lambda}{numeric. Reservoir parameter.}
}
\value{
tibble. First two column are original drainage_data. Third column is the drainage according to the linear reservoir model.
}
\description{
Calculate the resending limb of the linear reservoir
}
