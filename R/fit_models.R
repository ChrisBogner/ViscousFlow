#' Calculate the breakthrough time of a tracer
#'
#' @param tracer_data tibble or data.frame. Tracer breakthrough data.
#' @param time_interval vector of length 2. The time interval to search for the breakthrough.
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
