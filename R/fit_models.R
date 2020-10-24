#' Calculate the breakthrough time of a tracer
#'
#' The breakthrough time of a tracer in the drainage of a soil column is calculated as the maximum of curvature of its concentration in a suitable time interval.
#'
#'
#' @param tracer_data tibble or data.frame. Tracer breakthrough data. The first column must be time, the second the tracer concentration.
#' @param time_interval numeric vector of length 2. The time interval to search for the breakthrough, typically early in the experiment.
#' @param xlab_tag string or expression for the x label. Default is Time (sec).
#' @param ylab_tag string or expression for the y label. Default is c/c0.
#' @param do_plot logical. Should a plot be produced. Default is TRUE.
#'
#' @return numeric. The estimated time of the tracer breakthrough.
#' @export
#'
#' @examples
#' data(tracer)
#' find_tracer_breakthrough(tracer_data = tracer, time_interval = c(30000, 40000))
find_tracer_breakthrough <- function(tracer_data, time_interval,
                                     xlab_tag = 'Time (sec)', ylab_tag = expression(c/c[0]),
                                     do_plot = TRUE) {

  d1 <- sfsmisc::D1ss(unlist(tracer_data[,1]), unlist(tracer_data[,2]))
  d2 <- sfsmisc::D2ss(unlist(tracer_data[,1]), unlist(tracer_data[,2]))$y

  # formula for curvature from Wikipedia (https://en.wikipedia.org/wiki/Curvature)
  curvature <- abs(d2)/((1+d1^2)^(3/2))
  # Change of curvature
  d1_curve <- sfsmisc::D1ss(unlist(tracer_data[,1]), curvature)
  ind_search <- which(tracer_data[,1] > time_interval[1] & tracer_data[,1] < time_interval[2])
  # Pick the max of curvature as tracer breakthrough
  breakthrough_time <- unlist(tracer_data[(which.max(curvature[ind_search]) + ind_search[1] - 1), 1])

  tracer_data %>%
    dplyr::mutate(curvature, d1_curve)

  if(do_plot) {
    g_all <- list(
      ggplot2::geom_line(), ggplot2::xlab(xlab_tag)
    )

    g1 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                   y = colnames(tracer_data)[2])) +
      ggplot2::ylab(ylab_tag) +
      g_all +
      ggplot2::geom_vline(ggplot2::aes(xintercept = breakthrough_time,
                              colour = 'breakthrough'), lty = 2) +
      ggplot2::scale_colour_manual(name = ' ', values = (time = 'blue')) +
      ggplot2::theme(legend.position=c(0.2, 0.9),
            legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
            legend.key = ggplot2::element_rect(fill = "white")) +
      ggplot2::ggtitle('Tracer breakthrough')


    g2 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                           y = 'curvature')) +
      g_all +
      ggplot2::geom_vline(xintercept = breakthrough_time,
                          colour = 'blue', lty = 2) +
      ggplot2::ylab('curvature') +
      ggplot2::ggtitle('Curvature in time')

    g3 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                           y = 'd1_curve')) +
      g_all +
      ggplot2::geom_vline(xintercept = breakthrough_time,
                          colour = 'blue', lty = 2) +
      ggplot2::ylab(expression(paste(Delta, ' curvature'))) +
      ggplot2::ggtitle('Change of curvature (1st derivative)')


    g <- gridExtra::grid.arrange(g1, g2, g3, ncol = 1)

    breakthrough_time
  } else breakthrough_time
}


#' Calculate stationary flow rate in drainage
#'
#' @param drainage_data tibble or data.frame. Drainage data from a column experiment. First column must be time, second the drainage data.
#' @param time_interval numeric vector of length 2. The time interval where the flow is assumed to be stationary. Typically one would select the shortly before the irrigation is switched off, i.e. 0.9 * end_of_irrigation until end_or_irrigation.
#'
#' @return numeric. The stationary flow rate
#' @export
#'
#' @examples
#' data(drainage)
#' fit_stationary_flow_rate(drainage_data = drainage, time_interval = c(0.9 * 64404 , 64404))
fit_stationary_flow_rate <- function(drainage_data, time_interval) {
  ind1 <- max(which(drainage_data[,1] <= time_interval[2]))
  ind2 <- min(which(drainage_data[,1] >= time_interval[1]))

  drainage_data[ind1:ind2, ] %>%
    dplyr::pull(var = 2) %>%
    mean(na.rm = T)
}
